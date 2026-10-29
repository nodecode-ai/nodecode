;;;; provider.lisp --- the provider SDK layer: stream parts, lanes, policy.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One waist between the turn loop and every provider dialect, mirroring the
;;;; Zig SDK split (src/ai/core.zig StreamPart + per-dialect lanes). The turn
;;;; loop and the gateway consume STREAM-PARTs and the assembled chat-shaped
;;;; assistant message; only this file and the dialect files under engine/providers/, one
;;;; file per dialect, read provider wire bytes.
;;;;
;;;; Contracts owned here:
;;;;   - the STREAM-PART event alphabet and the *TURN-PART-FN* observer seam
;;;;   - PROVIDER-USAGE with the disjoint invariant: INPUT-TOKENS is non-cached
;;;;     input only, and NIL means unknown — never zero (core.zig:34-48)
;;;;   - provider identity and request-control globals, frozen per turn into
;;;;     EFFECTIVE-PROVIDER-CONFIG by turn.lisp
;;;;   - stream integrity: usage is requested and captured, in-stream error
;;;;     frames signal, a stream that ends without its done marker signals
;;;;     PROVIDER-STREAM-INCOMPLETE instead of passing truncation off as success
;;;;   - retry classification and delay policy (Retry-After first, capped)
;;;;   - the lane registry the files under engine/providers/ register into
;;;;
;;;; The retry loop itself, context compilation, and overflow eviction stay in
;;;; turn.lisp: they are turn policy folded over this layer, not part of it.
;;;;
;;;; The :USE-CONNECTION-POOL NIL constraint on every dexador call is not
;;;; optional; see the TLS-in-the-image note at the head of turn.lisp.

(in-package #:nodecode.evolved)

;;; --- provider configuration ---------------------------------------------
;;; Defaults point at OpenCode Zen's free tier, whose public key is genuinely
;;; public (it is documented as the literal string "public"). That keeps the
;;; offline gates runnable with no secret in the environment. Override per
;;; process, or per running image through the gateway model control op. Every
;;; control read from the environment is a startup parameter: a dumped binary
;;; reads it again when it starts, whereas a DEFPARAMETER would keep what the
;;; build's environment held.

(defun provider-env-number (name &optional (type 'real))
  "A number of TYPE parsed from environment variable NAME, or NIL."
  (let ((parsed (ignore-errors (let ((*read-eval* nil)) (read-from-string (uiop:getenv name))))))
    (and (typep parsed type) parsed)))

(define-startup-parameter *provider*
  (or (uiop:getenv "NODECODE_PROVIDER") "openai-completions")
  "Lane name selecting the provider dialect. See *PROVIDER-LANES*.")

(define-startup-parameter *endpoint* (uiop:getenv "NODECODE_ENDPOINT")
  "Explicit endpoint override, or NIL to use the selected lane's default.")

(define-startup-parameter *model*
  (or (uiop:getenv "NODECODE_MODEL")
      "deepseek-v4-flash-free")
  "Model id passed to the provider.")

;;; Seeded from general.auxiliary_model at every store open
;;; (SEED-MODEL-CONFIG-FROM-SHARED-CONFIG), moved by SELECT-AUXILIARY-MODEL —
;;; a pick on /models' Auxiliary tab — which persists it there too. :EFFORT NIL
;;; means the weakest declared rung.
(defparameter *auxiliary-model* nil
  "The auxiliary target — the model side work runs on (an agent's naming
call, and whatever else a layer or a cell routes through
WITH-AUXILIARY-MODEL): a (:provider :model :effort) plist, or NIL when
none is configured, in which case side work runs on the session's own
target at the weakest reasoning rung its model declares (AUXILIARY-MODEL).")

;;; A leading ! is the command indirection, re-executed at every resolution,
;;; never cached. Never logged: failures carry response bodies, not headers.
(define-startup-parameter *api-key*
  (uiop:getenv "NODECODE_API_KEY")
  "The operator-config credential tier, or NIL to resolve through the chain
(auth file, then environment — RESOLVE-PROVIDER-CREDENTIAL).")

;;; The pi-mono bodyTimeout rule. Kept by the attempt's own idle deadline
;;; (WITH-IDLE-CUT), armed only while a read is outstanding: dexador's
;;; :READ-TIMEOUT carries the same number but never reaches a TLS socket.
(defparameter *request-timeout* 120
  "Seconds without a single byte arriving before the attempt fails: the
stream's idle watchdog, and the ONLY clock a streaming attempt runs under —
a stream that keeps delivering bytes is never cut by wall time, however
long the generation takes (a local model at ~100 tok/s spends 10+ minutes
on a long answer, and a ceiling on a lively stream only ever kills
productive work and then retries the identical generation into the same
wall).")

;;; The Zig retry_policy.default_stream_max_retries.
(defparameter *max-stream-retries* 5
  "Maximum stream-scope retries: attempts whose failure arrived after a 200
head (truncation, in-stream errors, empty responses, idle-deadline expiry).")

;;; Refilled by every stream retry
;;; (the attempt.zig reset rule), so the two budgets multiply, never add. The Zig
;;; retry_policy.default_request_max_retries.
(defparameter *max-request-retries* 4
  "Maximum request-scope retries PER STREAM ATTEMPT: failures before any
stream bytes (connect, DNS, non-200 head).")

(defparameter *retry-base-delay* 0.2
  "Base exponential back-off in seconds (the Zig base_delay_ms 200).")

;;; A stream retry is a fresh request: no lane can resume a generation, so a
;;; re-run repeats the whole thing from zero. Under this ceiling that is a
;;; blip on a short answer, cheap to re-run with a clean prompt. Past it the
;;; re-run costs at least what was already sunk, while the continuation costs
;;; the sealed output as prompt tokens and the model resumes from its last
;;; sentence. NIL never continues: every stream failure re-runs.
(defparameter *stream-rerun-ceiling* 120
  "Seconds of streamed output past which a failed stream attempt is NOT
re-run but CONTINUED: the next attempt carries what streamed back to the
model as its own cut-off reply and asks it to go on from there (the
turn.lisp continuation context), and the round's message is the sealed
prefix merged with what the continuation streams.")

;;; Request controls. NIL means "not configured": the key is omitted from the
;;; wire entirely, because unknown is not zero and this layer never substitutes
;;; a default the operator did not set (the Zig model_controls rule).

(define-startup-parameter *temperature* (provider-env-number "NODECODE_TEMPERATURE")
  "Sampling temperature, or NIL to omit.")

(define-startup-parameter *top-p* (provider-env-number "NODECODE_TOP_P")
  "Nucleus sampling bound, or NIL to omit.")

;;; The Anthropic lane, whose wire requires max_tokens, substitutes its
;;; documented lane default when NIL.
(define-startup-parameter *max-completion-tokens*
  (provider-env-number "NODECODE_MAX_TOKENS" 'integer)
  "Completion token ceiling, or NIL to omit.")

(define-startup-parameter *tool-choice*
  (uiop:getenv "NODECODE_TOOL_CHOICE")
  "Tool choice policy string (\"auto\", \"none\", \"required\"), or NIL to omit.")

;;; Chat and Responses send it verbatim; Google clamps it to "high"; the
;;; Anthropic lane spends it as an adaptive effort or a thinking budget
;;; depending on what the model declares; "off" disables thinking in every
;;; lane's own spelling. The global is the default effort, as *MODEL* is the
;;; default model: a session that picked none runs on it, a session's own
;;; pick outranks it (EFFECTIVE-EFFORT), and every pick moves it
;;; (SELECT-DEFAULT-EFFORT).
(define-startup-parameter *reasoning-effort*
  (uiop:getenv "NODECODE_REASONING_EFFORT")
  "Reasoning effort — any +EFFORT-LADDER+ rung (\"off\" \"minimal\" \"low\"
\"medium\" \"high\" \"xhigh\" \"max\") — or NIL to omit.")

;;; --- memoized JSON files ----------------------------------------------------
;;; One protocol for every JSON file this layer reads off disk: parse once per
;;; (name, size, write-date) identity and answer from the memo until the file
;;; moves. The models.dev catalog, the shared operator config and the /models
;;; listing cache all ride it. Nothing here fetches, writes or watches a file:
;;; a caller that changed one drops its memo by setting the variable to NIL.

;;; Without this, first readers that miss together would each parse the
;;; same file, and each parse would promote its table through every
;;; collection the others force.
(defvar *json-file-memo-locks* (make-hash-table :test 'eq :synchronized t)
  "A lock per memo variable, held from its identity check through its parse,
so readers that miss the same file at once parse it once, and a reader of
another file never waits on it.")

(defun read-json-file-memo (memo path &key max-bytes probe (probe-seconds 0)
                                           (decoder #'nlk:decode-json))
  "(values VALUE ERROR-TEXT) for the JSON file at PATH, reparsed through
DECODER — a function of the file's UTF-8 octets — only when the file's (namestring,
size, write-date) identity moved since the parse MEMO holds."
  ;; MEMO and PROBE
  ;; are SYMBOLS naming special variables, not their values: setting one to NIL is
  ;; how any caller invalidates a memo, and the write-back lands on the innermost
  ;; dynamic binding, so a LET around it isolates a test. A NIL or empty PATH and
  ;; an absent file both answer (values NIL NIL) — not configured is not an error;
  ;; MAX-BYTES refuses a larger file without memoizing it. PROBE holds
  ;; (path . internal-real-time) of the last identity check and PROBE-SECONDS how
  ;; long the memo is trusted without a stat at all — the status line resolves the
  ;; model every animation frame, and that stat+open+stat cluster was a measured
  ;; per-frame syscall hotspot. A failed parse memoizes its error text, so a
  ;; broken file is re-read when it moves and not once per turn.
  (let ((memoed (symbol-value memo))
        (now (get-internal-real-time)))
    (when (and memoed probe (symbol-value probe)
               (equal (car (symbol-value probe)) path)
               (< (- now (cdr (symbol-value probe)))
                  (* probe-seconds internal-time-units-per-second)))
      (return-from read-json-file-memo (values (fourth memoed) (fifth memoed))))
    (let ((truename (and path (not (equal path "")) (probe-file path))))
      (unless truename
        (return-from read-json-file-memo (values nil nil)))
      (when probe (setf (symbol-value probe) (cons path now)))
      (bt2:with-lock-held ((sb-ext:with-locked-hash-table (*json-file-memo-locks*)
                             (alexandria:ensure-gethash memo *json-file-memo-locks*
                                                        (bt2:make-lock :name (symbol-name memo)))))
        (let ((memoed (symbol-value memo))
              (name (namestring truename))
              (size (nlk:file-bytes truename))
              (mtime (ignore-errors (file-write-date truename))))
          (when (and max-bytes (not (and (integerp size) (<= size max-bytes))))
            (return-from read-json-file-memo (values nil nil)))
          (unless (and memoed (equal name (first memoed))
                       (eql size (second memoed)) (eql mtime (third memoed)))
            ;; (NAME SIZE MTIME VALUE ERROR-TEXT)
            (setf memoed (list* name size mtime
                                (handler-case
                                    ;; Read as the bytes it is: a 4 MB catalog
                                    ;; widened to characters is 17 MB of heap.
                                    (let ((parsed (funcall decoder (alexandria:read-file-into-byte-vector
                                                                    truename))))
                                      (if (hash-table-p parsed)
                                          (list parsed nil)
                                          (list nil "JSON root is not an object")))
                                  (error (condition) (list nil (princ-to-string condition)))))
                  (symbol-value memo) memoed))
          (values (fourth memoed) (fifth memoed)))))))

(deftype provider-id () "A provider's id: the providers.<id> config key." 'string)
(deftype model-id () "A model's catalog id, as its provider names it." 'string)

;;; --- frozen per-turn provider policy --------------------------------------

(defstruct (effective-provider-config (:copier copy-effective-provider-config))
  "Immutable provider and loop policy captured once for an admitted turn."
  ;; CONTEXT-TOKENS and OUTPUT-LIMIT are the model capability resolved at
  ;; admission (the Zig admitted_model_context_limit/output_limit freeze):
  ;; request shaping and eviction budgets read the frozen numbers, never the
  ;; live catalog.
  (provider "openai-completions" :type provider-id :read-only t)
  ;; The wire dialect this turn rides — a registered lane name, resolved per
  ;; (provider, model) at the freeze (RESOLVE-MODEL-LANE), never re-derived
  ;; from the provider id mid-turn. ENDPOINT is composed for this lane. They
  ;; are the only slots here that are not read-only: a lane the endpoint
  ;; refused the model on is the one sanctioned mid-turn drift, and WITH-LANE
  ;; moves both on a fresh copy, never on the config a turn already rides.
  (lane (error "lane required") :type string)
  (endpoint (error "endpoint required") :type string)
  (model (error "model required") :type model-id :read-only t)
  (api-key (error "api key required") :type string :read-only t)
  ;; providers.<id>.proxy — a dexador proxy URI (http:// or socks5://) for
  ;; endpoints with no direct route, e.g. a tailnet peer behind tailscaled's
  ;; userspace-networking SOCKS5 server. NIL defers to dex:*default-proxy*.
  (proxy nil :type (or null string) :read-only t)
  (request-timeout 120 :type real :read-only t)
  (max-stream-retries 0 :type integer :read-only t)
  (max-request-retries 0 :type integer :read-only t)
  (retry-base-delay 1 :type real :read-only t)
  (stream-rerun-ceiling nil :type (or null real) :read-only t)
  (temperature nil :type (or null real) :read-only t)
  (top-p nil :type (or null real) :read-only t)
  (max-completion-tokens nil :type (or null integer) :read-only t)
  (tool-choice nil :type (or null string) :read-only t)
  (reasoning-effort nil :type (or null string) :read-only t)
  ;; What the Responses lane asks the API to say about its reasoning:
  ;; "auto", "concise" or "detailed" ride as reasoning.summary, NIL asks for
  ;; none. providers.<id>.reasoning_summary sets it, "auto" when the entry
  ;; says nothing, "off" meaning none.
  ;;
  ;; The default stays "auto": it is what the OpenAI reference agents send,
  ;; and turning it off is UNTESTED rather than free. A summary looks like
  ;; billed output nobody reads on a headless run, but it also replays in
  ;; the assistant history beside the encrypted reasoning item, so cutting
  ;; it cuts something the next round reads. An ARC-AGI-3 attempt to measure
  ;; that on gpt-6-astra failed: summary-off runs reasoned at 234
  ;; and 161 tokens per call against summary-on runs at 106, 59 and 290 —
  ;; the same-configuration spread swallowed the effect. Only differences
  ;; far larger than 3x are readable on that benchmark.
  (reasoning-summary "auto" :type (or null string) :read-only t)
  (context-tokens nil :type (or null integer) :read-only t)
  ;; True when CONTEXT-TOKENS came from explicit operator configuration (the
  ;; env override or a capability override), not from the catalog. An
  ;; operator number is a contract EFFECTIVE-CONTEXT-BUDGET obeys verbatim; a
  ;; derived number is a prior that learned window evidence may move — and a
  ;; NIL prior is an unknown window that stays unknown until a rejection
  ;; teaches a ceiling.
  (context-tokens-operator-p nil :type boolean :read-only t)
  (output-limit nil :type (or null integer) :read-only t)
  ;; Credential provenance is the key alone (the providers/root.zig
  ;; LocalTurnConfig collapse: key and token ride one string; the auth *style*
  ;; is the lane's business). What a credential CARRIES rides in a table keyed
  ;; by the config, never in a slot here, so a config this image already handed
  ;; out keeps its shape across the change (CREDENTIAL-ATTRIBUTE).
  ;;
  ;; The rungs the admitted model declares, frozen with the rest of the
  ;; capability: a lane asks what its target admits without ever re-reading
  ;; the live catalog mid-turn.
  (effort-levels '() :type list :read-only t))

(nlk:access (config effective-provider-config))

;;; One memo for the process. An encoding is remembered by the message it
;;; encodes, weakly (json.lisp JSON-MEMO), so it lives as long as some session
;;; still holds that message -- and the hydration cache keeps a warm session's
;;; closed turns EQ from one turn to the next. Without this, a memo per turn
;;; would encode the whole history twice a turn, once measured as text for the
;;; estimate and once as the body. An item's encoding is the message's under a
;;; dialect that names everything else it depends on (:CHAT-THINKING,
;;; :RESPONSES), so a later turn's lookup is the same answer.
(defvar *history-memo* (make-json-memo)
  "The encodings of history messages, remembered by message identity.")

(defstruct (context-request (:copier nil))
  "One round's frozen engine input — everything COMPILE-TURN-CONTEXT reads."
  ;; The caller freezes policy here (turn.lisp SNAPSHOT-CONTEXT-REQUEST is the
  ;; one seam that reads the ambient configuration); the engine is a pure
  ;; function of this struct. MESSAGES is the storeless fallback projection
  ;; used when HISTORY-TURNS is empty (heap-only sessions, isolated loop
  ;; tests).
  (session-id nil :type (or null string) :read-only t)
  (provider-config (error "provider config required")
                   :type effective-provider-config :read-only t)
  (base-system-prompt "" :type string :read-only t)
  (harness-sections '() :type list :read-only t)
  (tools nil :type (or null vector) :read-only t)
  (tool-bindings '() :type list :read-only t)
  (history-turns '() :type list :read-only t)
  (history-boundary 0 :type (integer 0) :read-only t)
  ;; The floors the session's chain recorded (NLK:RETAINED-HISTORY-FLOORS):
  ;; where the eviction stub splits the evicted history into blocks.
  (history-floors '() :type list :read-only t)
  (messages '() :type (or list vector) :read-only t)
  ;; The live turn's attachments, grouped by the message they arrived
  ;; with: (ANCHOR . IMAGES) entries, ANCHOR NIL for the turn's inputs and
  ;; a tool call id for what that call attached (turn.lisp *TURN-IMAGES*).
  (visible-images '() :type list :read-only t))

(defstruct (compiled-turn-context (:copier nil)
                                  (:include context-request
                                   (messages #() :type vector :read-only t)))
  "One turn's provider input and exact admitted tool bindings."
  ;; SYSTEM-PROMPT is what the request carries — the base plus the system
  ;; sections; BASE-SYSTEM-PROMPT is the base alone and HARNESS-SECTIONS the
  ;; (NAME . TEXT) sections it was composed with, all of them the
  ;; conversation's pinned prompt beside the tools (turn.lisp TURN-PROMPT):
  ;; the block every request of the conversation repeats byte for byte, so
  ;; the provider's prefix cache holds across its turns. The base stays
  ;; beside the overlay so a compile never double-appends. HEAD-MESSAGE is
  ;; the sections the system message does not carry — the instructions file,
  ;; a pinned index, a room's contract — as one user-role message, or NIL;
  ;; every lane joins it ahead of the first user message's own text, inside
  ;; the cached prefix (MESSAGES-WITH-HEAD). What changes after rides the
  ;; history, as the reminders the fold places (context.lisp
  ;; WIRE-MESSAGES-FROM-EVENTS).
  (system-prompt (error "system prompt required") :type string :read-only t)
  (head-message nil :type (or null hash-table) :read-only t)
  ;; The floors the eviction stub listed the evicted blocks by, kept beside
  ;; the boundary so a retarget re-renders the same stub.
  ;; The live turn's attachments as the frozen (ANCHOR . IMAGES) groups the
  ;; messages were compiled with (context.lisp ATTACH-VISIBLE-IMAGES), kept
  ;; beside them so a retarget — an eviction re-aiming the window, a
  ;; withholding after a provider refused them — places them again.
  ;; What the history has encoded, by message identity: *HISTORY-MEMO*,
  ;; read by the request builders, the serialize seams and the estimate.
  (memo *history-memo* :type json-memo :read-only t))

(nlk:access (context compiled-turn-context))

;;; --- the reasoning-effort vocabulary ---------------------------------------
;;; One ladder every surface and every lane speaks (the lisp mirror of
;;; protocol/thinking.zig): a user's "think harder" means the same thing
;;; everywhere and can be ranked against whatever subset a model declares.
;;; Pure vocabulary — no capability ingestion (that is the catalog seam below)
;;; and no wire spelling (that is each lane's request builder).

;;; List order IS rank order
;;; (protocol/thinking.zig all_levels). models.dev spells the disabled rung
;;; "none"; it is normalized to "off" at ingestion, and each lane re-emits its
;;; own spelling on the way out.
(defparameter +effort-ladder+
  '("off" "minimal" "low" "medium" "high" "xhigh" "max")
  "Every reasoning rung, weakest to strongest.")

(defun effort-rank (value)
  "VALUE's rank in +EFFORT-LADDER+, or NIL when it names no rung."
  (and (stringp value) (position value +effort-ladder+ :test #'string-equal)))

(defun clamp-effort (effort ceiling &aux (rank (effort-rank effort))
                                         (limit (effort-rank ceiling)))
  "EFFORT when it ranks at or below CEILING, else CEILING."
  ;; A string this vocabulary does not know passes through untouched: an
  ;; operator spelling the ladder never claimed is the provider's business to
  ;; accept or refuse, not ours to rewrite.
  (if (and rank limit (> rank limit)) ceiling effort))

;;; --- model capability: catalog, overrides, and request shaping -------------
;;; One resolution seam (the lisp mirror of src/providers/model_metadata.zig +
;;; src/ai/model_controls.zig): the organism's own learned overrides outrank
;;; the models.dev disk cache; for the window, the operator's own number in the
;;; shared config (providers.<id>.models[].context_window) outranks both, and
;;; the NODECODE_CONTEXT_TOKENS env override outranks the config. A model no
;;; source knows keeps a NIL window — unknown, never a guess.
;;; Everything that shapes a request or an eviction budget reads
;;; RESOLVE-MODEL-CAPABILITY — no other authority.

;;; Wins over catalog and overrides: explicit operator configuration outranks
;;; every discovered value.
(define-startup-parameter *context-tokens-env-override*
  (provider-env-number "NODECODE_CONTEXT_TOKENS" 'integer)
  "Operator-configured context ceiling.")

;;; Mutable material: the organism
;;; records what it learns about its models here, and a recorded fact outranks
;;; the checked models.dev cache it cannot verify. :EFFORT-LEVELS is a list of
;;; +EFFORT-LADDER+ rungs — the escape for an endpoint models.dev does not carry
;;; (a local server, a private lane) whose reasoning ladder the operator knows.
(defparameter *model-capability-overrides* '()
  "Alist of (model-substring . plist) with keys :CONTEXT-TOKENS
:OUTPUT-LIMIT :REASONING-P :EFFORT-LEVELS.")

;;; MODELS-CATALOG-TABLE reads it and REFRESH-MODELS-CATALOG keeps it current.
;;; NIL disables catalog lookups entirely.
(define-startup-parameter *models-catalog-path*
  (or (uiop:getenv "NODECODE_MODELS_CATALOG")
      (namestring (nlk:cache-path "nodecode/models.dev/api.json")))
  "Where the models.dev api.json disk cache lives.")

;;; NIL fetches nothing and the disk cache, whatever its age, is all there is:
;;; the hermetic test posture, SETF so a gateway worker thread's read cannot
;;; reach the network or write the operator's cache either.
(defparameter *models-catalog-url* "https://models.dev/api.json"
  "Where REFRESH-MODELS-CATALOG fetches the catalog from.")

;;; Without a TTL, a disk cache would keep serving the catalog already on
;;; disk and never start a refetch: a model added upstream would be absent and
;;; a model whose shape changed would be read the old way. An hour keeps a
;;; model models.dev adds within reach without a multi-megabyte download per
;;; read, per process.
(defparameter *models-catalog-ttl-seconds* 3600
  "How old the disk cache may grow before a read starts a refetch behind
itself.")

;;; A caller that asks REFRESH-MODELS-CATALOG directly is never held back.
(defparameter *models-catalog-retry-seconds* 300
  "How long after a fetch started before a read may start another, so a box
without the network does not dial on every status-line frame.")

(defvar *models-catalog-fetch* nil
  "(THREAD STARTED-AT . FAILED-BEFORE) of the last models.dev fetch this
process started, STARTED-AT a universal time and FAILED-BEFORE why the fetch
before it failed (MODELS-CATALOG-FETCH-FAILURE), or NIL before the first.")

(defvar *models-catalog-fetch-lock* (bt2:make-lock :name "models-catalog-fetch")
  "Held while REFRESH-MODELS-CATALOG decides, so two threads reading a stale
cache at once start one fetch.")

(defvar *models-catalog-memo* nil
  "The READ-JSON-FILE-MEMO record of the last catalog read, so the
multi-megabyte parse happens once per file identity, not once per turn.")

;;; The status line resolves the model every animation frame, and the probe's
;;; stat+open+stat cluster was a measured per-frame syscall hotspot — between
;;; probes the memoized parse is trusted for *MODELS-CATALOG-PROBE-SECONDS*.
(defvar *models-catalog-probe* nil
  "(configured-path . probed-at) of the last on-disk identity probe.")

;;; A refresh, this process's or another's, lands here within this window.
(defparameter *models-catalog-probe-seconds* 2
  "How long MODELS-CATALOG-TABLE trusts the memoized parse before re-checking
the file identity on disk.")

;;; The catalog kept as decoded is a large live object: every model's cost,
;;; modalities and limit are an EQUAL table of two or three keys. What any
;;; reader asks of a model is these facts, so a model is kept as them, and a
;;; provider as the fields its readers ask for.
(defstruct (catalog-model (:constructor make-catalog-model
                              (name context output inputs outputs efforts reasoning-p
                               release-date tool-call-p price))
                          (:copier nil) (:predicate nil))
  "What the models.dev catalog says of one model, as its readers ask it."
  (name nil :read-only t)
  ;; limit (else limits) context, else input, else context_input: CATALOG-LIMIT
  (context nil :read-only t)
  (output nil :read-only t)
  ;; modalities.input and modalities.output as published, or NIL where not
  (inputs nil :read-only t)
  (outputs nil :read-only t)
  ;; CATALOG-EFFORT-LEVELS
  (efforts nil :read-only t)
  (reasoning-p nil :read-only t)
  (release-date nil :read-only t)
  (tool-call-p nil :read-only t)
  ;; CATALOG-PRICE
  (price nil :read-only t))

(defun catalog-model-turn-p (model)
  "Whether a turn can run on MODEL, as the catalog tells it: it takes tool
calls and answers in text alone."
  ;; A turn always offers tools and reads text back. Setup listed speech, live
  ;; and translation models beside the chat ones.
  (and (catalog-model-tool-call-p model)
       (every (lambda (kind) (equal kind "text")) (catalog-model-outputs model))))

(defparameter *catalog-provider-fields* '("name" "display_name" "npm" "api" "env" "models")
  "The fields of a catalog provider any reader asks for; the rest are dropped.")

(defparameter *catalog-model-fields*
  '("name" "limit" "limits" "modalities" "reasoning" "reasoning_options" "release_date" "tool_call"
    "cost")
  "The fields of a models.dev model that DECODE-MODELS-CATALOG reads.")

(defun catalog-member-p (path)
  "Whether the models.dev member at PATH, innermost key first, is one the
catalog keeps: a provider's *CATALOG-PROVIDER-FIELDS*, a model's
*CATALOG-MODEL-FIELDS*, and everything inside them."
  (case (length path)
    (2 (member (first path) *catalog-provider-fields* :test #'string=))
    (4 (or (string/= (third path) "models")
           (member (first path) *catalog-model-fields* :test #'string=)))
    (t t)))

(defun decode-models-catalog (octets &aux (shared (make-hash-table :test 'equal)))
  "The models.dev catalog in OCTETS, each provider kept to
*CATALOG-PROVIDER-FIELDS* and each of its models a CATALOG-MODEL."
  ;; Decoded through KEEP and REVIVE (DECODE-JSON): the fields no reader asks
  ;; for are passed over, and each model is its CATALOG-MODEL the moment its
  ;; object closes. If decoded whole and compacted after, the document would
  ;; hold many tables alive and promoted until the first full walk. SHARED gives
  ;; every model that publishes the same modalities or ladder one copy of it.
  (flet ((shared (value)
           (and value (alexandria:ensure-gethash (coerce value 'list) shared value))))
    ;; Prices read as doubles on whichever thread refreshes the catalog: only
    ;; the launch thread binds the format, and a single-float 0.003 is off in
    ;; its eighth digit.
    (let ((*read-default-float-format* 'double-float))
     (nlk:decode-json
     octets
     :keep #'catalog-member-p
     :revive (lambda (path entry)
               (if (and (= 3 (length path)) (string= (second path) "models") (hash-table-p entry))
                   (make-catalog-model
                    (json-value entry :text "name")
                    (catalog-limit entry "context" "input" "context_input")
                    (catalog-limit entry "output")
                    (shared (json-value entry :array "modalities" "input"))
                    (shared (json-value entry :array "modalities" "output"))
                    (shared (catalog-effort-levels entry))
                    (json-value entry :boolean "reasoning")
                    (json-value entry :text "release_date")
                    (json-value entry :boolean "tool_call")
                    (shared (catalog-price entry)))
                   entry))))))

(defun models-catalog-table ()
  "The parsed models.dev root object, memoized on (path, size, mtime); NIL
when the cache file is absent, oversized, or unparseable. Each provider's
models are CATALOG-MODELs (DECODE-MODELS-CATALOG)."
  ;; A successful read is trusted without touching the filesystem for
  ;; *MODELS-CATALOG-PROBE-SECONDS*.
  ;;
  ;; A read that finds no parse, or a file older than
  ;; *MODELS-CATALOG-TTL-SECONDS*, answers what is there and starts
  ;; REFRESH-MODELS-CATALOG behind itself, at most once per
  ;; *MODELS-CATALOG-RETRY-SECONDS*. The age is the memo's mtime, so a fresh
  ;; cache costs no syscall beyond the probe's.
  (let ((root (read-json-file-memo '*models-catalog-memo* *models-catalog-path*
                                   :max-bytes (* 8 1024 1024) ; the Zig read cap
                                   :probe '*models-catalog-probe*
                                   :probe-seconds *models-catalog-probe-seconds*
                                   :decoder #'decode-models-catalog))
        (mtime (third *models-catalog-memo*))
        (now (get-universal-time)))
    (when (and (or (null root)
                   (null mtime)
                   (> (- now mtime) *models-catalog-ttl-seconds*))
               (let ((started (second *models-catalog-fetch*)))
                 (or (null started)
                     (>= (- now started) *models-catalog-retry-seconds*))))
      (refresh-models-catalog))
    root))

(defun refresh-models-catalog (&aux (path *models-catalog-path*)
                                    (url *models-catalog-url*))
  "Fetch the models.dev catalog into *MODELS-CATALOG-PATH* on a background
thread, unless one is already running."
  ;; => that thread, started here or in
  ;; flight, or NIL when there is no path or no *MODELS-CATALOG-URL*. Joining it
  ;; answers NIL, or why the fetch failed; the cache moves only by an atomic
  ;; rename, so a failed fetch leaves the old file serving. MODELS-CATALOG-TABLE
  ;; calls this when its cache is absent or stale; a caller that needs the rows
  ;; now, /models or an import on a box with no cache, calls it directly and
  ;; waits on the thread.
  (when (and path url)
    (bt2:with-lock-held (*models-catalog-fetch-lock*)
      (let ((running (car *models-catalog-fetch*)))
        (if (and running (bt2:thread-alive-p running))
            running
            (car (setf *models-catalog-fetch*
                       (list* (spawn "nodecode-models-catalog"
                                (handler-case
                                    (progn (fetch-models-catalog :path path :url url)
                                           nil)
                                  ;; DEADLINE-TIMEOUT is serious, not an ERROR;
                                  ;; a failure below HTTP is its transport
                                  ;; fact, FETCH-MODELS-CATALOG's refusals are
                                  ;; words already, and any other keeps its
                                  ;; class, never its printed self
                                  (serious-condition (condition)
                                    (cond ((transport-failure-row condition)
                                           (transport-failure-label condition url))
                                          ((typep condition 'file-error)
                                           "its copy could not be written here")
                                          ((typep condition 'simple-error)
                                           (princ-to-string condition))
                                          (t (format nil "the fetch failed (~(~a~))"
                                                     (type-of condition)))))))
                              (get-universal-time)
                              (models-catalog-fetch-failure)))))))))

(defun models-catalog-fetch-failure (&aux (thread (car *models-catalog-fetch*)))
  "Why the last models.dev fetch this process finished failed, or NIL: it
landed, or none has finished."
  ;; A retry in flight answers the failure before it: offline, the first turn
  ;; after each retry window said `try again in a moment' (xm-204).
  (if (and thread (not (bt2:thread-alive-p thread)))
      (let ((reason (bt2:join-thread thread)))
        (and (stringp reason) reason))
      (cddr *models-catalog-fetch*)))

(defun fetch-models-catalog (&key (path *models-catalog-path*) (url *models-catalog-url*))
  "Fetch the models.dev catalog into PATH, the file MODELS-CATALOG-TABLE
reads, atomically, and forget the reader's memo so the next read sees it."
  ;; => PATH. Signals on any failure, and nothing is written then: a body that
  ;; does not open a JSON object (a captive portal's page) never replaces a
  ;; catalog. REFRESH-MODELS-CATALOG is the one caller.
  (check-type path string)
  (check-type url string)
  ;; the deadline over the whole fetch, TLS handshake included: api.json is a few megabytes
  (bind ((timeout 30)
         ((body status) (sb-sys:with-deadline (:seconds timeout)
                          (nlk:http :get url :headers (with-user-agent '()) :binary t
                                             :timeout timeout :connect-timeout timeout))))
    (unless (eql status 200)
      (error "models.dev answered HTTP ~a" status))
    (unless (and (vectorp body) (plusp (length body)))
      (error "models.dev answered an empty body"))
    (unless (eql (char-code #\{)
                 (find-if-not (lambda (octet) (member octet '(9 10 13 32))) body))
      (error "models.dev answered a body that is not a JSON object"))
    (nlk:write-file-atomically path body)
    (setf *models-catalog-memo* nil
          *models-catalog-probe* nil)
    path))

(defvar *models-catalog-rows-index* nil
  "(CATALOG . TABLE): per model id, every (provider-id . catalog-model)
namespace of the parsed models.dev CATALOG carrying it, in provider-id
order; rebuilt when the catalog is reparsed.")

(defun models-catalog-rows (model-id &aux (root (models-catalog-table)))
  "Every (provider-id . catalog-model) namespace carrying MODEL-ID, in
provider-id order: exact key match first, then the reseller-prefix-stripped
id (the model_metadata.zig retry for ids like \"kr/claude-opus-5\")."
  ;; A model id is not unique in models.dev. Every reseller that serves a
  ;; model republishes its row, and the rows disagree — `claude-opus-5`
  ;; appears in fourteen namespaces, some of which never heard of its top two
  ;; reasoning rungs. Returning the whole list, ordered, is what lets the
  ;; readers below choose on evidence instead of on hash-bucket order.
  ;;
  ;; One pass over the catalog's ~9,000 rows files every namespace under its
  ;; model id, once per parsed catalog: asking all ~230 namespaces per id cost
  ;; the /models picker 70 ms of names alone. The answer is a fresh list, so
  ;; no caller's sort reaches the index.
  (when (hash-table-p root)
    (let ((index *models-catalog-rows-index*))
      (unless (and index (eq (car index) root))
        (let ((table (make-hash-table :test #'equal)))
          (loop for provider-id being the hash-keys of root using (hash-value provider)
                for models = (json-value provider :object "models")
                when models
                  do (loop for id being the hash-keys of models using (hash-value entry)
                           when entry do (push (cons provider-id entry) (gethash id table))))
          (loop for id being the hash-keys of table using (hash-value rows)
                do (setf (gethash id table) (sort rows #'string< :key #'car)))
          (setf index (setf *models-catalog-rows-index* (cons root table)))))
      (flet ((collect (id) (copy-list (gethash id (cdr index)))))
        (or (collect model-id)
            (let ((slash (position #\/ model-id)))
              (and slash (< (1+ slash) (length model-id)) (collect (subseq model-id (1+ slash))))))))))

(defun models-catalog-entry (model-id &optional provider)
  "The models.dev CATALOG-MODEL for MODEL-ID, PROVIDER's own row when that
namespace carries one, else the first namespace in provider-id order."
  ;; An operator who names a real models.dev provider gets that provider's
  ;; numbers; an aggregator lands on a stable row rather than a different one
  ;; per boot.
  (let ((rows (models-catalog-rows (or model-id ""))))
    (or (cdr (find provider rows :key #'car :test #'equal))
        (cdr (first rows)))))

(defun catalog-limit (entry &rest keys)
  "The first of KEYS ENTRY carries as an integer, within limit then limits —
the models_dev.zig precedence (context reads context, input, context_input
in that order; output reads output)."
  (loop for table in '("limit" "limits")
        thereis (loop for key in keys
                      thereis (json-value entry :integer table key))))

(defun catalog-effort-levels (entry &aux (levels '()))
  "ENTRY's declared reasoning rungs, weakest to strongest, or NIL when the
model declares no effort option at all."
  ;; models.dev publishes reasoning_options as a union — a toggle, an effort
  ;; ladder, a budget_tokens range — and an effort ladder outranks the other
  ;; two whenever a model offers both, because it is the provider's own
  ;; vocabulary and needs no synthesized numbers (the model_controls.zig
  ;; preference). "none" is the disabled rung under another name and
  ;; normalizes to "off"; "default" and JSON null name no rung and are
  ;; dropped.
  (loop for option across (json-array entry "reasoning_options")
        when (equal "effort" (json-value option :string "type"))
          do (loop for value across (json-array option "values")
                   for rung = (and (stringp value)
                                   (if (string-equal value "none") "off" value))
                   when (effort-rank rung)
                     do (pushnew rung levels :test #'string-equal)))
  (sort levels #'< :key #'effort-rank))

(defun model-reads-images-p (model-id &optional (provider *provider*))
  "Whether MODEL-ID takes image input, by the models.dev row PROVIDER lands
on: T when its modalities.input names image, NIL when the row says text
only, :UNKNOWN when no row or no modalities are published — an unknown
stays unknown, and the provider's own answer decides: the request carrying
the image goes out, and one the provider refuses is re-sent without it with
a note in its place (turn.lisp CALL-PROVIDER)."
  ;; The codex view_image gate in front, refusing a model without the image
  ;; modality before any bytes are staged; a landing for the provider's answer
  ;; behind it.
  (let ((inputs (let ((entry (models-catalog-entry model-id provider)))
                  (and entry (catalog-model-inputs entry)))))
    (if inputs (and (find "image" inputs :test #'equal) t) :unknown)))

(defun declared-effort-levels (model-id provider)
  "The reasoning ladder MODEL-ID declares for PROVIDER, or NIL when no
namespace names one."
  ;; PROVIDER's own row is the whole answer whenever models.dev carries one:
  ;; the vendor publishes the vocabulary its own API accepts, and a row that
  ;; names no effort option is telling us this model spends thinking some
  ;; other way — that silence is a fact, not a gap to fill from a reseller.
  ;;
  ;; An aggregator models.dev has never heard of — a private router, a local
  ;; gateway — has no row of its own, and the resellers who republish the model
  ;; disagree; there the ladder the most namespaces agree on wins. Consensus is
  ;; not truth, but it beats the row that happened to hash first, and
  ;; *MODEL-CAPABILITY-OVERRIDES* is where an operator settles it outright.
  (let* ((rows (models-catalog-rows (or model-id "")))
         (own (find provider rows :key #'car :test #'equal)))
    (if own
        (catalog-model-efforts (cdr own))
        (let ((ladders (remove nil (mapcar (lambda (row) (catalog-model-efforts (cdr row))) rows))))
          ;; Rows arrive in provider-id order and ties break on the wider
          ;; ladder, so the same catalog always answers the same way.
          (first (stable-sort (reverse (remove-duplicates ladders :test #'equal :from-end t)) #'>
                              :key (lambda (ladder)
                                     (+ (* 10 (count ladder ladders :test #'equal))
                                        (length ladder)))))))))

(defun catalog-price (entry)
  "ENTRY's published price as (INPUT OUTPUT CACHE-READ CACHE-WRITE . TIERS),
US dollars per million tokens, the cache prices NIL where unpublished; NIL
when no input and output price is, or both are zero -- a plan's row, not a
price. Each of TIERS is (SIZE INPUT OUTPUT CACHE-READ CACHE-WRITE): what a
round whose prompt is over SIZE tokens pays instead, smallest SIZE first."
  ;; The tiers are cost.tiers, each naming its own size. The older
  ;; context_over_200k is not read: it carries no size, and on the rows that
  ;; have both its prices start at the tier's, which is 272k for gpt-5.x.
  (flet ((prices (object)
           (flet ((price (key &aux (value (json-value object :number key)))
                    (and value (>= value 0) value)))
             (let ((input (price "input")) (output (price "output")))
               (and input output (or (plusp input) (plusp output))
                    (list input output (price "cache_read") (price "cache_write")))))))
    (when-let (base (prices (json-value entry :object "cost")))
      (append base
              (sort (loop for tier across (or (json-value entry :array "cost" "tiers") #())
                          for size = (json-value tier :number "tier" "size")
                          for price = (and (equal (json-value tier :string "tier" "type") "context")
                                           (realp size) (prices tier))
                          when price collect (cons size price))
                    #'< :key #'first)))))

(defun model-price (model-id provider)
  "What a million tokens of MODEL-ID cost, as CATALOG-PRICE: PROVIDER's own
priced row, else the price the most priced rows agree on, else NIL."
  ;; The DECLARED-EFFORT-LEVELS rule: a relay models.dev has never heard of
  ;; gets the resellers' consensus, which also passes over the row that
  ;; forgot its cache price (302ai, first in id order for claude-sonnet-5,
  ;; would bill every cached token at the full input price). Ties go to the
  ;; row that publishes more prices, then to provider-id order.
  (let* ((rows (models-catalog-rows (or model-id "")))
         (own (find provider rows :key #'car :test #'equal))
         (prices (loop for (nil . entry) in rows
                       when (catalog-model-price entry) collect it)))
    (or (and own (catalog-model-price (cdr own)))
        (first (stable-sort (remove-duplicates prices :test #'equal :from-end t) #'>
                            :key (lambda (price)
                                   (+ (* 10 (count price prices :test #'equal))
                                      (count-if-not #'null price))))))))

(defun round-cost (price &key input-tokens output-tokens cached-input-tokens
                              cache-write-tokens &allow-other-keys)
  "The US dollars PRICE (MODEL-PRICE) makes of one round's disjoint counts,
a double-float, or NIL when there is no price or no count at all."
  ;; A prompt over a tier's size bills every token of the round at that
  ;; tier, as the providers bill it: the prompt is what was sent, fresh,
  ;; read from cache and written to it. An unpublished cache price bills at
  ;; the input price of the tier the round lands in: an over-count the
  ;; reader can see, never an invented discount.
  (when (and price (or input-tokens output-tokens cached-input-tokens cache-write-tokens))
    (destructuring-bind (input output cache-read cache-write)
        (price-tier price (+ (or input-tokens 0) (or cached-input-tokens 0) (or cache-write-tokens 0)))
      (float (/ (+ (* (or input-tokens 0) input)
                   (* (or cached-input-tokens 0) (or cache-read input))
                   (* (or cache-write-tokens 0) (or cache-write input))
                   (* (or output-tokens 0) output))
                1000000)
             1d0))))

(defun price-tier (price prompt)
  "PRICE's four rates -- input, output, cache read, cache write -- for a round
whose prompt is PROMPT tokens: the last of its tiers the prompt is over, else
its base. A price of those four alone bills every round alike, however many
rounds' counts it is handed at once."
  (let ((tier (find-if (lambda (tier) (> prompt (first tier))) (nthcdr 4 price) :from-end t)))
    (subseq (or (rest tier) price) 0 4)))

(defun price-tier-sizes (price)
  "The prompt sizes PRICE's tiers begin over, in its order."
  (mapcar #'first (nthcdr 4 price)))

(defun round-cache-saving (price &key input-tokens cached-input-tokens cache-write-tokens
                                 &allow-other-keys)
  "What the prompt cache saved one round at PRICE, in US dollars: its prompt
billed whole at the input price, less what it was billed -- the discount on
what was read net of the premium on what was written, so a round that only
wrote the cache saved less than nothing. NIL when there is no price."
  (when-let (billed (round-cost price :input-tokens (or input-tokens 0)
                                      :cached-input-tokens cached-input-tokens
                                      :cache-write-tokens cache-write-tokens))
    (- (round-cost price :input-tokens (+ (or input-tokens 0) (or cached-input-tokens 0)
                                          (or cache-write-tokens 0)))
       billed)))

(defvar *shared-config-memo* nil
  "The READ-JSON-FILE-MEMO record of the last shared-config read, so per-turn
provider resolution does not reparse an unchanged file.")

(defstruct (model-capability (:copier nil))
  "What the capability seam knows about one model."
  ;; KNOWN-P false means no source recognized the model — unknown is not no,
  ;; so reasoning controls stay available (the model_controls rule).
  (context-tokens nil :type (or null integer) :read-only t)
  ;; Operator provenance for CONTEXT-TOKENS: env override, the config's
  ;; context_window, or capability override, as opposed to catalog
  ;; derivation. Rides into the
  ;; frozen turn config so the learned-budget seam knows which numbers are
  ;; contracts and which are priors.
  (context-tokens-operator-p nil :type boolean :read-only t)
  (output-limit nil :type (or null integer) :read-only t)
  (reasoning-p nil :type boolean :read-only t)
  ;; The rungs this model declares, weakest to strongest, or NIL when no
  ;; source names a ladder. NIL is not "none": an undeclared model falls back
  ;; to the whole +EFFORT-LADDER+ at the descriptor seam.
  (effort-levels '() :type list :read-only t)
  (known-p nil :type boolean :read-only t))

(nlk:access (capability model-capability))

;;; The status line resolves the model every animation frame; without this the
;;; resolve was a measured per-frame catalog scan.
(defvar *model-capability-memo* nil
  "(KEY . ID-TABLE): resolved capabilities per (provider, model id), valid
for one KEY, the identities of every input the resolve reads: the catalog
root, the overrides list, the env override, the shared config's path and its
last read.")

(defun resolve-model-capability (model-id &optional (provider *provider*))
  "Resolve MODEL-ID against the env override, the shared config, overrides,
and the models.dev cache, in that authority order per field — NIL where no
source answers."
  ;; PROVIDER selects which namespace of the catalog answers for a model id
  ;; many of them republish, and which providers.<PROVIDER> entry of the
  ;; config: only the window is read there (its models[].context_window).
  ;; Memoized per (provider, id) until one of the inputs changes.
  ;; the KEY: every input's identity, compared by EQL; none of them touches a file
  (let ((key (list (models-catalog-table) *model-capability-overrides*
                   *context-tokens-env-override* *shared-config-path* *shared-config-memo*))
        (memo *model-capability-memo*))
    (unless (and memo (every #'eql key (car memo)))
      ;; Turn threads and the render thread both resolve capabilities, so
      ;; the id table is synchronized; the memo spine swap is a plain store.
      (setf memo (cons key (make-hash-table :test #'equal :synchronized t))
            *model-capability-memo* memo))
    (values (alexandria:ensure-gethash (cons (or provider "") (or model-id "")) (cdr memo)
                                       (%resolve-model-capability model-id provider)))))

(defun %resolve-model-capability (model-id provider)
  (let* ((id (string-downcase (or model-id "")))
         (override (cdr (find-if (lambda (entry) (search (car entry) id))
                                 *model-capability-overrides*)))
         ;; The operator's own window from providers.<PROVIDER>.models: a
         ;; relay no catalog knows has no other source, and an unknown window
         ;; never arms the eviction trigger. One not a positive integer warns:
         ;; the bound did not take.
         (configured
           (when-let (model-entry (and (stringp model-id)
                                       (configured-model-entry provider model-id)))
             (or (config-context-window model-entry)
                 (multiple-value-bind (value present-p)
                     (gethash "context_window" model-entry)
                   (when present-p
                     (warn "providers.~a.models ~a: context_window ~s is not a positive integer, so no window is set"
                           provider model-id value))
                   nil))))
         (entry (models-catalog-entry (or model-id "") provider))
         (operator (or *context-tokens-env-override* configured (getf override :context-tokens)))
         (levels (or (getf override :effort-levels)
                     (declared-effort-levels model-id provider))))
    (make-model-capability
     ;; NIL when no source knows: an unknown window is unknown, not a flat
     ;; guess. Without this, an uncatalogued model would get a fixed default
     ;; window it never had, because the learned floor can only ratchet to
     ;; what the eviction trigger sends. NIL disables the trigger; the
     ;; provider's own rejection teaches the ceiling (CONTEXT-WINDOW-BELIEF).
     :context-tokens (or operator
                         (and entry (catalog-model-context entry)))
     :context-tokens-operator-p (and operator t)
     :output-limit (or (getf override :output-limit)
                       (and entry (catalog-model-output entry)))
     ;; A declared ladder is itself a reasoning declaration: naming the rungs
     ;; a model spends says more than the catalog's boolean, so an operator
     ;; who records only :EFFORT-LEVELS does not have to say `reasoning` too.
     ;; An explicit :REASONING-P still outranks both.
     :reasoning-p (let ((recorded (getf override :reasoning-p :unset)))
                    (cond ((not (eq recorded :unset)) (and recorded t))
                          (entry (catalog-model-reasoning-p entry))
                          (t (and levels t))))
     :effort-levels levels
     :known-p (not (null (or override entry))))))

(defun model-context-tokens (model-id &optional provider)
  "The context budget for MODEL-ID, as PROVIDER (by default *PROVIDER*)
serves it, through the one capability seam."
  (model-capability-context-tokens (resolve-model-capability model-id (or provider *provider*))))

(defun model-context-tokens-operator-p (model-id)
  "True when MODEL-ID's context budget is explicit operator configuration."
  (model-capability-context-tokens-operator-p
   (resolve-model-capability model-id)))

;;; --- the learned context budget --------------------------------------------
;;; The window as evidence, not configuration: successful rounds prove floors
;;; (usage totals the server actually processed), hard context rejections
;;; bound ceilings (estimate-grade — our count of the refused request), and
;;; the durable rows live in state.lisp's context_window_observations. The
;;; belief below is THE ONE SANCTIONED MID-TURN DRIFT in the frozen provider
;;; config: the budget is a measurement, and a window learned from this
;;; round's context-edge cut must reach the retry and every later round, or
;;; the recovery recovers nothing (adversarial review F3). Everything else in
;;; EFFECTIVE-PROVIDER-CONFIG stays frozen policy.

(defun context-window-belief (provider model prior operator-p)
  "The effective context budget for (PROVIDER, MODEL): OPERATOR-P returns
the operator's explicit PRIOR verbatim; otherwise the derived PRIOR
corrected by durable evidence — a proven floor raises it, a rejection
ceiling caps it, and a floor outranks a ceiling it contradicts (floors are
measured, ceilings are estimates)."
  ;; A NIL PRIOR is an unknown window, and a floor alone never fills it in: a
  ;; floor proves the window is AT LEAST that large, and reading it as the
  ;; budget would ratchet the belief to the largest prompt the trigger ever
  ;; dared. Unknown stays NIL (no eviction trigger) until a rejection
  ;; contributes a ceiling; from then on the ceiling is the belief, still
  ;; outranked by any floor that disproves it.
  (if (or operator-p
          (not (stringp provider)) (not (stringp model))
          (not (nlk:store-open-p)))
      prior
      (multiple-value-bind (floor ceiling)
          (nlk:context-window-observation provider model)
        (cond ((null prior) (and ceiling (max (or floor 0) ceiling)))
              (ceiling (max (or floor 0) (min (max prior (or floor 0)) ceiling)))
              (t (max prior (or floor 0)))))))

(defun effective-context-budget (config)
  "CONFIG's context budget through the learned-window belief."
  (context-window-belief
   config.provider
   config.model
   config.context-tokens
   config.context-tokens-operator-p))

;;; --- the measured prompt -----------------------------------------------------
;;; ESTIMATE-JSON-TOKENS is chars/4, and characters are not what the server
;;; counts. Every round the server processes reports the prompt as it counted
;;; it (the usage on the stream's completion: input + cached), and that count
;;; is what the eviction trigger and the output cap ride: the newest round's
;;; measured prompt over the chars/4 estimate of that same request is the
;;; ratio every estimate of the next request is scaled by, so a request that
;;; has not changed estimates at exactly what the server counted, and only
;;; what was added since is sized from characters. The newest measurement
;;; replaces the last, unsmoothed and unbounded: a reasoning item replayed by
;;; reference is invisible to characters and grows the true ratio every round.
;;; DeepSeek's encrypted_content is a ~38-character handle the server expands
;;; to the reasoning it stands for; the EWMA clamped at 3.0 that stood here
;;; held the trigger at three times the characters. In-image only,
;;; deliberately: the ratio is measured again every round, and durable state
;;; should carry the window (a fact), not the estimator's error (a parameter).

;;; Every touch below is one GETHASH or one SETF of one entry -- never a
;;; read-modify-write -- so the table's own synchronization is the whole mutual
;;; exclusion this needs, and the turn threads that measure share it with the
;;; render thread that reads without a lock of their own.
(defvar *context-estimate-calibration*
  (make-hash-table :test #'equal :synchronized t)
  "\"provider/model\" -> the newest round's measured prompt tokens over its
estimated tokens.")

(defun context-estimate-ratio (provider model)
  "The measured/estimated ratio for (PROVIDER, MODEL), 1.0 until observed."
  (or (and (stringp provider) (stringp model)
           (gethash (format nil "~a/~a" provider model) *context-estimate-calibration*))
      1.0d0))

(defun observe-context-estimate (provider model estimated measured)
  "Take one round's MEASURED prompt tokens, the server's own count, over its
ESTIMATED count as the (PROVIDER, MODEL) ratio, replacing the last."
  (when (and (stringp provider) (stringp model)
             (integerp estimated) (plusp estimated)
             (integerp measured) (plusp measured))
    (setf (gethash (format nil "~a/~a" provider model) *context-estimate-calibration*)
          (/ (float measured 1.0d0) estimated))))

;;; Under it the request cannot answer — every token of the window is prompt —
;;; and the only reply it can produce is a finish_reason=length with nothing
;;; in it, which the turn loop then has to read as the model's final word.
;;; CALL-PROVIDER refuses the round instead (PROVIDER-NO-OUTPUT-ROOM); a cap
;;; the operator configured is never measured against this, only the
;;; shrink-to-fit.
(defparameter +min-usable-output-tokens+ 512
  "The smallest window-derived output cap worth sending.")

(defun agent-max-output-tokens (output-limit context-tokens estimated-prompt-tokens)
  "Shrink the model's output limit to what the context window can still hold
(turn_engine.zig agentMaxOutputTokens): NIL when no output limit is known;
the full limit when no window is known or it fits; else what remains above
the safety margin."
  ;; => (values CAP SHRUNK-P), SHRUNK-P true when the window
  ;; and not the model's own limit is what set CAP.
  ;;
  ;; Deliberately NOT floored at 1: a cap of one token is not a smaller
  ;; answer, it is a request that cannot answer, and a floor would make a
  ;; one-token reply look usable. What is unusable is the caller's to
  ;; refuse (+MIN-USABLE-OUTPUT-TOKENS+), not this function's to round up
  ;; into looking fine.
  ;; 4096 tokens of headroom (the Zig agent_output_cap_context_safety_tokens)
  (let ((available (and output-limit context-tokens
                        (max 0 (- context-tokens estimated-prompt-tokens 4096)))))
    (if (and available (< available output-limit))
        (values available t)
        (values output-limit nil))))

(defun estimate-json-tokens (value &optional memo)
  "Cheap stable token estimate used before a provider request: a token per
four characters of VALUE's JSON text."
  ;; Through MEMO the measure is remembered by identity (JSON-MEMO-CHARS), so
  ;; a value frozen for the turn — the prompt, the tools, every history
  ;; message — is walked once and summed after.
  (ceiling (if memo
               (json-memo-chars memo value)
               (length (encode-json-object value)))
           4))

;;; An image rides the wire as a base64 data: URI, and chars/4 reads those
;;; characters as if they were prompt text — which they are not: every lane
;;; downscales an attachment into its own tile grid and charges a bounded
;;; count. Without a flat per-image charge, the base64 characters would be
;;; counted as prompt text, overcount the context, evict the session's whole
;;; history and shrink the output cap (AGENT-MAX-OUTPUT-TOKENS) to one
;;; token, so the turn would end on a one-token finish_reason=length with
;;; nothing said.
;;;
;;; 1600 is the loosest of the lanes' own per-image ceilings — Anthropic caps
;;; an image at 1568px, ~1600 tokens; OpenAI's high detail at 2048px, ~1105 +
;;; 85 — deliberately, because an upper bound keeps the budget conservative
;;; and the observed ratio (OBSERVE-CONTEXT-ESTIMATE) corrects the rest from
;;; the server's own count next round.
(defparameter +image-estimated-tokens+ 1600
  "What one attached image costs the context budget, whatever its bytes.")

(defun compiled-context-estimated-tokens (context)
  "The request-shaped token estimate for CONTEXT: frozen system prompt,
the head message, messages and tool schemas — the same terms
the eviction estimate uses, measured through the turn's memo, with every image part
charged as an image (+IMAGE-ESTIMATED-TOKENS+) rather than as the base64
text it serializes to."
  ;; The request fact, the usage fact, the calibration observation and the
  ;; output ceiling each ask for this every round; a 100k-token window walked
  ;; once a turn per message answers them all from the sum.
  ;; An image part costs the flat ceiling in place of the characters already counted for its
  ;; base64; every message of durable history carries none (attachments live on the visible
  ;; turn only, context.lisp ATTACH-VISIBLE-IMAGES).
  (let ((memo context.memo) (messages context.messages))
    (+ (estimate-json-tokens context.system-prompt memo)
       (if-let (head context.head-message) (estimate-json-tokens head) 0)
       (estimate-json-tokens messages memo)
       (loop for message across messages
             sum (loop for part in (message-content-parts (gethash "content" message))
                       when (equal "image_url" (content-part-type part))
                         sum (- +image-estimated-tokens+ (estimate-json-tokens part memo))))
       (estimate-json-tokens (or context.tools #()) memo))))

(defun join-user-message (messages message)
  "MESSAGES with MESSAGE's text joined to their first user-role message,
ahead of its own content — one more text part when that content is typed, two
newlines when it is a string — or standing first as a user message of its own
when there is none. The message it touches is a copy, so a compiled history is
never observed changed. => a fresh vector, MESSAGES itself when MESSAGE is NIL."
  (if (null message)
      messages
      (if-let (position (position-if #'user-role-message-p messages))
        (let* ((copy (copy-json-object (aref messages position)))
               (content (gethash "content" copy))
               (text (content-text (gethash "content" message))))
          (setf (gethash "content" copy)
                (cond ((and (vectorp content) (not (stringp content)))
                       (concatenate 'vector (vector (make-text-content-part text)) content))
                      ((and (stringp content) (plusp (length content)))
                       (format nil "~a~%~%~a" text content))
                      (t text)))
          (replace (copy-seq messages) (list copy) :start1 position))
        (concatenate 'vector (vector message) messages))))

(defun messages-with-head (context)
  "CONTEXT's history with its head message joined ahead of the first user
message's own text: the standing block the system message does not carry,
at the head of the conversation, where every round's prefix repeats it."
  ;; Where Claude Code, Codex and DSH put the instructions file, the memory
  ;; and the skills: a user message at the head of the conversation, so the
  ;; system message is the harness's own voice and the text a repository or
  ;; the model itself wrote never ranks with it. The first user
  ;; message moves only when the window does — an eviction rebuilds the
  ;; prefix anyway — so the head is as cache-stable as the system message.
  (join-user-message context.messages context.head-message))

(defun request-messages (context)
  "CONTEXT's history as the chat, Responses and GenAI lanes send it: the head
message joined ahead of the first user message's text."
  ;; Nothing joins behind: a tail that moved with the newest user message —
  ;; the notices, a lane's `where' — re-read the turn before it at every new
  ;; turn. What changes after the pin stands in the history where it was
  ;; noticed (turn.lisp TURN-PROMPT), never after the round's results, which
  ;; the Responses wire re-renders from the first user message on every round.
  (messages-with-head context))

(defun effective-max-output-tokens (context &aux (config context.provider-config))
  "The max_tokens a request should carry: the operator's configured ceiling
outranks the capability-derived shrink-to-fit; NIL when neither source knows."
  ;; The window term rides the learned budget and the estimate rides the
  ;; calibration ratio — the same corrected numbers the eviction policy uses.
  ;; => (values CAP SHRUNK-P), SHRUNK-P true only when the window shrank CAP
  ;; below what the model itself allows: the one case where a length finish is
  ;; our own arithmetic talking and not the model's output limit.
  (if-let (configured config.max-completion-tokens)
    (values configured nil)
    (agent-max-output-tokens
     config.output-limit
     (effective-context-budget config)
     (round (* (compiled-context-estimated-tokens context)
               (context-estimate-ratio config.provider config.model))))))

;;; --- conditions ---------------------------------------------------------

;;; A bound on what is kept, never on what is parsed: an SSE event larger than
;;; this folds like any other. DeepSeek's response.completed echoes a whole
;;; think, 250-310 KB on a 60K-token round, and a cap applied to the line
;;; threw the finished round away and re-ran the think.
(defparameter +max-provider-evidence-bytes+ (* 256 1024)
  "Bound on retained failure evidence — the partial stream body and the
outbound request body — so a huge stream cannot pin unbounded memory on a
path that is already failing (transport.zig max_partial_stream_body_bytes,
256KiB).")

(defun bounded-evidence (text)
  "TEXT clipped to the evidence bound, or NIL when TEXT is neither a string
nor octets."
  ;; Octets — a request body — read as their UTF-8 text, a sequence the bound
  ;; cuts through as ?.
  (typecase text
    (string (nlk:clip text +max-provider-evidence-bytes+ :ellipsis ""))
    (nlk:octets (sb-ext:octets-to-string
                 text :end (min (length text) +max-provider-evidence-bytes+)
                 :external-format '(:utf-8 :replacement #\?)))))

(defun json-first (object type &rest paths)
  "The first value OBJECT carries as TYPE (JSON-VALUE) at one of PATHS, each a list of keys."
  (loop for path in paths thereis (apply #'json-value object type path)))

(defun provider-said (detail &aux (parsed (ignore-errors (nlk:decode-json detail))))
  "The provider's own sentence in DETAIL, the head of an error body:
error.message, message, a string error or detail, read from the JSON -- or,
from a body the 400-character cut left unparseable, the first message
field -- else DETAIL itself."
  ;; A mistyped key would otherwise print the provider's raw JSON error body
  ;; as the first thing the operator reads. The cut can land inside the
  ;; message itself, and the row would print the raw JSON: the field reads to
  ;; its end or the cut's, and says it was cut.
  (or (json-first parsed :text '("error" "message") '("message") '("error") '("detail"))
      (multiple-value-bind (match groups)
          (ppcre:scan-to-strings "\"message\"\\s*:\\s*\"((?:[^\"\\\\]|\\\\.)*)(\")?" detail)
        (and match (format nil "~a~:[…~;~]"
                           (ppcre:regex-replace-all "\\\\(.)" (aref groups 0)
                                                    (lambda (match c) (declare (ignore match)) (if (find (char c 0) "nrt") " " c))
                                                    :simple-calls t)
                           (aref groups 1))))
      detail))

(define-error provider-error (error)
    ((status :initform nil)
     (detail :initform "")
     ;; The retry policy's own words on why the failure stands -- a spent
     ;; stream budget that kept what streamed, a failover chain walked to its
     ;; end -- so the turn's terminal fact explains itself. Apart from DETAIL:
     ;; appended there, it would be carried with the rest of a JSON body when
     ;; the report reads the provider's sentence out of it.
     (note :initform nil :writable t)
     ;; Every model a walked failover chain tried before the failure stood,
     ;; in the order they ran, the selection first: (:provider P :model M
     ;; :status S :said TEXT), TEXT the provider's own words. Set, the report
     ;; is that list, one model a line.
     (chain :initform nil :writable t)
     (retry-after :initform nil)
     ;; Which retry budget this failure spends — the providers/root.zig phase →
     ;; scope routing, captured at the signal site rather than re-derived:
     ;; :REQUEST for failures before any stream byte (connect, DNS, non-200
     ;; head), :STREAM for failures after a 200 head, :CONTRACT for clean
     ;; streams that said nothing. Scope picks the budget, never the verdict
     ;; (retryableError discards it); the default is :STREAM, the Zig
     ;; null-phase arm.
     (scope :initform :stream)
     ;; Failure evidence (the core.zig ProviderErrorDetail retention rule):
     ;; DETAIL stays a 400-char UI string, while these carry what a diagnosis
     ;; actually needs — the bounded response/partial-stream body and the exact
     ;; request that provoked it. Never logged wholesale; read at the debugger
     ;; or by an evidence-persisting caller.
     (evidence-body :initform nil :writable t)
     (evidence-request-body :initform nil :writable t)
     ;; The response's declared Content-Type, when a response arrived: the
     ;; one header a body-shape diagnosis turns on (a CloudFront error page
     ;; answering application/octet-stream to a JSON request).
     (evidence-content-type :initform nil :writable t)
     (stream-complete-p :initform t :writable t)
     ;; Provenance: which (lane, model) the failing request targeted. RUN
     ;; binds the specials to the turn-effective identity for the turn's
     ;; whole dynamic extent, so capturing them at signal time names the
     ;; identity the request was built from — no signal site restates it.
     (provider :initform *provider*)
     (model :initform *model*))
  ;; The detail stays the body's head -- the overflow needles read it -- and
  ;; the report says the provider's sentence and the next step: a refused key
  ;; is /models's, any other 4xx the model's. Google refuses a key with a 400
  ;; too; without the refused-key branch first, a refused Google key would read
  ;; as the model's 4xx. A walked chain's report is every model it tried, one a
  ;; line, each with what its provider said: the last model's words alone hid
  ;; the selection's failure that sent the round down the chain, and a label
  ;; (`http_400') hid what each candidate answered.
  (:report (lambda (condition stream)
             (let* ((status (provider-error-status condition))
                    (hint (cond ((refused-key-answer-p
                                  status (error-body-code (provider-error-evidence-body condition)))
                                 "/models saves a new key")
                                ((typep status '(integer 400 499)) "/models picks another model"))))
               (if (provider-error-chain condition)
                   (format stream "every model failed~@[ ~a~]~@[ · ~a~]~:{~%~d. ~a/~a~@[ · HTTP ~d~]: ~a~}"
                           (provider-error-note condition) hint
                           (loop for entry in (provider-error-chain condition)
                                 for n from 1
                                 collect (list n (getf entry :provider) (getf entry :model)
                                               (getf entry :status) (getf entry :said))))
                   (format stream "provider request failed [~a ~a]~@[ with status ~a~]: ~a~@[ ~a~]~@[ · ~a~]"
                           (provider-error-provider condition) (provider-error-model condition)
                           status (provider-said (provider-error-detail condition))
                           (provider-error-note condition) hint)))))
  (:documentation "A provider round trip that did not yield a usable message."))

(define-error provider-connection-refused (provider-error) ()
  (:default-initargs :scope :request)
  ;; The host answered that nothing listens on the port -- a stopped Ollama
  ;; or llama.cpp server, a base_url with the wrong port -- and answers it
  ;; again to the next attempt: the round spends the request budget at once
  ;; and goes to the failover chain, or stands.
  (:documentation "The endpoint refused the connection: nothing listens at
its address."))

(define-error provider-slow-start (provider-error) ()
  ;; No status: with nowhere to go it is retried like any stall. With the
  ;; failover candidate that stood behind the attempt, the round leaves for
  ;; it at once (CALL-PROVIDER), and the cold start is never waited out
  ;; twice.
  (:documentation "A watched attempt streamed no token by its deadline, or
was skipped while it waited for one (ATTEMPT-WATCH)."))

(define-error provider-stream-incomplete (provider-error) ()
  (:default-initargs :stream-complete-p nil)
  (:documentation "A 200 stream ended before its done marker: truncation is a
failure of the attempt, never a shorter answer. Nil status makes it retryable."))

(define-error provider-empty-response (provider-error) ()
  (:default-initargs :scope :contract)
  ;; Contract error, fed back into the retry loop as transient; spends the
  ;; stream budget (the attempt.zig provider_contract arm).
  (:documentation "A clean response carrying no content, no tool calls, and no
reasoning."))

(define-error provider-refused (provider-error) ()
  (:default-initargs :scope :contract)
  ;; A provider can answer with stop_reason "refusal" and no content blocks.
  ;; Read as an empty response, the same request would be sent again and
  ;; refused again.
  (:documentation "The provider declined to answer: a refusal or content-filter
finish with nothing said. The same request meets the same verdict, so it is
never resent."))

(define-error provider-malformed-tool-call (provider-error) ()
  (:default-initargs :scope :contract)
  ;; No wire delivers one by accident: an aggregator converting a streamed
  ;; call between dialects one chunk at a time may emit one functionCall part
  ;; per argument fragment, each named "", and nothing is registered under
  ;; "" to dispatch it to — so the message is the attempt's failure, never a
  ;; round. Contract scope: it spends the stream budget, like an empty message.
  (:documentation "A clean stream whose assembled message carries a tool call
with no name."))

(define-error provider-config-error (provider-error) ()
  (:report "~a" detail)
  ;; Refused wherever a selection is made or frozen, and printed as its detail
  ;; alone — the detail names the key to set, and the round-trip prefix of a
  ;; PROVIDER-ERROR would attribute it to whatever pair happened to be
  ;; selected.
  (:documentation "The operator's configuration cannot name a wire: an sdk no
lane recognizes, a provider entry naming no sdk, a provider nothing knows."))

(define-error provider-no-output-room (provider-error) ()
  (:default-initargs :scope :request)
  ;; Signalled before dispatch, so no round trip is spent and — unlike the
  ;; server's own overflow verdict — no context-window ceiling is observed
  ;; from it: the number that produced it is our estimate, not the server's
  ;; count, and recording an estimate as a ceiling would durably poison the
  ;; budget. Images are withheld first where the round carries any; otherwise
  ;; it stands, and the turn fails saying so.
  (:documentation "The window-derived output cap collapsed below
+MIN-USABLE-OUTPUT-TOKENS+: the prompt fills the model's context, so the
request could only come back on finish_reason=length with nothing in it."))

(define-error provider-non-sse-response (provider-error) ()
  ;; Feeding it to the SSE walk can only produce misleading framing errors, so
  ;; the body itself is surfaced as the verdict (transport.zig
  ;; ProviderNonSseResponse). Status 200 makes it non-retryable: the peer
  ;; answered decisively.
  (:documentation "A 200 answer to a stream request whose Content-Type is not
an event stream (a proxy answering stream:true with a plain JSON body)."))

(defun attach-provider-evidence (condition body request-body)
  "Fill CONDITION's empty failure-evidence slots and mark it stream-partial."
  ;; Runs from a HANDLER-BIND around a live stream walk, before unwinding, so
  ;; the evidence accumulator is still intact; slots already set at signal
  ;; time win.
  (when (typep condition 'provider-error)
    (unless (provider-error-evidence-body condition)
      (let ((bounded (bounded-evidence body)))
        (when (and bounded (plusp (length bounded)))
          (setf (provider-error-evidence-body condition) bounded
                (provider-error-stream-complete-p condition) nil))))
    (unless (provider-error-evidence-request-body condition)
      (setf (provider-error-evidence-request-body condition)
            (bounded-evidence request-body))))
  condition)

(defun provider-failure-evidence (condition)
  "What a turn's terminal failure fact keeps of the provider failure that
ended it, as a payload plist: the HTTP status, the response's Content-Type,
the sha256 of the request that provoked it (when the whole request was
kept) and the head of the response body."
  ;; Debug-only retention: nothing here folds, renders or replays as
  ;; transcript — it is what a diagnosis of a dead session reads when the
  ;; process and its debugger are long gone. Without this, a failed
  ;; provider turn would leave no request identity in the evidence. NIL for
  ;; a failure that is not a provider's.
  (when (typep condition 'provider-error)
    (let ((status (provider-error-status condition))
          (content-type (provider-error-evidence-content-type condition))
          (body (provider-error-evidence-body condition))
          (request (provider-error-evidence-request-body condition)))
      (nconc (when (integerp status) (list :status status))
             ;; Which lane and model failed, as a retry fact names them.
             (when (stringp (provider-error-model condition))
               (list :provider (provider-error-provider condition)
                     :model (provider-error-model condition)))
             (when (stringp content-type) (list :content-type content-type))
             ;; A request the bound clipped has no honest digest.
             (when (and (stringp request)
                        (< (length request) +max-provider-evidence-bytes+))
               (list :request-sha256 (nlk::sha256-text request)))
             ;; the head only: the fact replays to every client of the session
             (when (and (stringp body) (plusp (length body)))
               (list :response-body (subseq body 0 (min (length body) 4096))))))))

;;; --- stream parts: the one event alphabet consumers see -------------------

(defstruct (stream-part (:copier nil))
  "One normalized provider stream event (the lisp StreamPart waist,
mirroring core.zig:924 StreamPart)."
  ;;
  ;; KIND is one of:
  ;;   :stream-start        an attempt opened (fires per attempt, including each
  ;;                        tool round's request)
  ;;   :stream-retry        the previous attempt failed after possibly emitting
  ;;                        parts and will be retried: live consumers must void
  ;;                        what that attempt buffered. Emitted by the retry
  ;;                        policy in turn.lisp, never by a lane.
  ;;   :stream-fallback     the retry budget is spent and the round re-aims at
  ;;                        another configured model: live consumers must void
  ;;                        what the failed attempt buffered and may say the
  ;;                        turn moved (PAYLOAD: from/to provider and model,
  ;;                        reason, status). Emitted by the failover policy in
  ;;                        turn.lisp, never by a lane.
  ;;   :response-metadata   ID/MODEL identify the provider response
  ;;                        and REASONING-EFFORT is the effort it reports actually
  ;;                        running under (the responses dialect's
  ;;                        response.reasoning.effort); fires once per attempt,
  ;;                        from the first frame that carries any of them. This is
  ;;                        what the provider ANSWERED with, which a relay is free
  ;;                        to make differ from what the request asked for
  ;;   :text-start          a text span opened; ID names the span
  ;;   :text-delta          TEXT holds one assistant content fragment; ID names
  ;;                        the open span
  ;;   :text-end            the text span ID closed
  ;;   :reasoning-start     a thinking span opened; ID names the span
  ;;   :reasoning-delta     TEXT holds one thinking/reasoning fragment
  ;;   :reasoning-end       the thinking span ID closed
  ;;   :reasoning-signature TEXT holds a thinking-signature fragment (replay
  ;;                        material licensing the thinking block's next round)
  ;;   :tool-input-start    tool call ID/INDEX/TOOL-NAME became known; fires once
  ;;                        both the call id and the tool name have arrived
  ;;   :tool-input-delta    TEXT holds this fragment; ARGUMENTS holds the
  ;;                        accumulated state of the in-flight call after it (the
  ;;                        accumulated view is a lisp projection convenience on
  ;;                        top of the Zig fragment-only contract)
  ;;   :tool-input-end      the tool call ID's input is complete
  ;;   :tool-call           one complete assembled tool call (ID/TOOL-NAME/ARGUMENTS)
  ;;   :error               PAYLOAD holds a provider-reported in-stream error
  ;;                        value; the lane signals right after emitting it, so
  ;;                        today an error part is always round-fatal
  ;;   :finish              the attempt finished; its usage and its finish reason
  ;;                        are the lane's own second and third values, not slots
  ;;
  ;; Zig kinds deliberately absent until provider-executed tools exist (no
  ;; lisp producer, and a dead kind is worse than a missing one): tool_result,
  ;; tool_approval_request, file, source_url.
  (kind (error "kind required") :type keyword :read-only t)
  (text nil :read-only t)
  (index nil :read-only t)
  (id nil :read-only t)
  (tool-name nil :read-only t)
  (arguments nil :read-only t)
  (model nil :read-only t)
  (reasoning-effort nil :read-only t)
  (payload nil :read-only t))

(nlk:access (part stream-part))

(defstruct (provider-usage (:copier nil))
  "Token usage with the disjoint invariant: INPUT-TOKENS counts non-cached
input only, CACHED-INPUT-TOKENS counts cache reads, and NIL means the provider
did not say — never zero."
  ;; Mirrors the 8-slot core.zig:34 Usage: context use is always input +
  ;; cached-input; cache writes carry the 5m/1h split when the provider
  ;; reports one, the undifferentiated total when it does not.
  (input-tokens nil :type (or null integer))
  (output-tokens nil :type (or null integer))
  (cached-input-tokens nil :type (or null integer))
  (total-tokens nil :type (or null integer))
  (reasoning-tokens nil :type (or null integer))
  (cache-write-tokens nil :type (or null integer))
  (cache-write-5m-tokens nil :type (or null integer))
  (cache-write-1h-tokens nil :type (or null integer)))

;;; Bound by the gateway (to project item_delta frames) and the local TUI (to
;;; update live rows); lanes call it for every part in arrival order. Bound
;;; via LET, never global mutation, so concurrent turns on different sessions
;;; do not interfere.
(defvar *turn-part-fn* nil
  "The one stream observer seam: a function of one STREAM-PART, or NIL.")

(defvar *current-durable-turn* nil
  "Kernel lifecycle record for the currently executing evolved turn.")

(defun emit-stream-part (on-part kind &rest initargs)
  (when on-part
    (funcall on-part (apply #'make-stream-part :kind kind initargs)))
  nil)

;;; --- usage accumulation ----------------------------------------------------
;;; One mutable accumulator per attempt; APPLY-* fold each wire usage object
;;; into it field-by-field (Zig applyUsage semantics: a field is set when the
;;; provider reports it, and absent stays NIL — "reported zero" must remain
;;; distinguishable from "never reported").

(nlk:access (accum provider-usage))

(defun reported-count (&rest values)
  "The count a provider actually reported, given the same number spelled in
several dialects: the first positive value, else the first reported zero,
else NIL."
  ;; A relay that translates between the Responses and chat dialects emits
  ;; BOTH spellings and zeroes the one it did not serve — a6api answers a chat
  ;; request with `prompt_tokens: 4393` beside `input_tokens: 0`. A plain OR
  ;; takes the zero (0 is true in Lisp) and the round's real prompt count
  ;; vanishes from the divider, so a positive report outranks a zero one.
  ;; "Reported zero" still survives when it is the only report: the disjoint
  ;; contract's NIL-is-unknown rule is untouched.
  (or (find-if (lambda (value) (and (integerp value) (plusp value))) values)
      (find-if #'integerp values)))

(defun apply-chat-usage (accum table)
  "Fold one chat-completions usage object into ACCUM (chat.zig applyUsage)."
  ;; The disjoint rule has two wire shapes: a provider reporting
  ;; cache_read_tokens already excludes the cached share from its prompt count
  ;; (Anthropic-style), so no adjustment; one reporting
  ;; prompt_tokens_details.cached_tokens (chat) or
  ;; input_tokens_details.cached_tokens (responses) includes it
  ;; (OpenAI-style), so the cached share is subtracted out of INPUT.
  ;;
  ;; Both dialects' spellings are read through REPORTED-COUNT, because a relay
  ;; converting between them emits both and zeroes the unused one.
  (when (hash-table-p table)
    (flet ((int (&rest path) (apply #'json-value table :integer path)))
      (let* ((input (reported-count (int "input_tokens") (int "prompt_tokens")))
             (output (reported-count (int "output_tokens")
                                     (int "completion_tokens")))
             (total (int "total_tokens"))
             (cache-read (int "cache_read_tokens"))
             (prompt-details (or (json-value table :object "prompt_tokens_details")
                                 (json-value table :object "input_tokens_details")))
             (cached (json-value prompt-details :integer "cached_tokens"))
             ;; The responses dialect nests its cache-write count inside the same
             ;; details object that carries cached_tokens; the chat dialect keeps
             ;; it at the top level under either spelling.
             (write (reported-count (int "cache_write_tokens")
                                    (int "cache_creation_input_tokens")
                                    (json-value prompt-details :integer "cache_write_tokens")))
             (five (or (int "cache_write_5m_tokens") (int "cache_write_5_minute_tokens")))
             (one (or (int "cache_write_1h_tokens") (int "cache_write_1_hour_tokens")))
             (reasoning (or (int "completion_tokens_details" "reasoning_tokens")
                            (int "output_tokens_details" "reasoning_tokens"))))
        (loop for (slot count) in `((input-tokens ,input) (output-tokens ,output)
                                    (cache-write-tokens ,write) (cache-write-5m-tokens ,five)
                                    (cache-write-1h-tokens ,one) (reasoning-tokens ,reasoning))
              when count do (setf (slot-value accum slot) count))
        (cond
          (cache-read (setf accum.cached-input-tokens cache-read))
          (cached
           (setf accum.cached-input-tokens cached)
           (when accum.input-tokens
             (setf accum.input-tokens (max 0 (- accum.input-tokens cached))))))
        (cond (total (setf accum.total-tokens total))
              ((and accum.input-tokens accum.output-tokens)
               (setf accum.total-tokens (+ accum.input-tokens accum.output-tokens))))
        (when (json-value table :object "cache_creation")
          (loop for (slot key) in '((cache-write-5m-tokens "ephemeral_5m_input_tokens")
                                    (cache-write-1h-tokens "ephemeral_1h_input_tokens"))
                do (setf (slot-value accum slot)
                         (+ (or (slot-value accum slot) 0) (or (int "cache_creation" key) 0)))))))))

;;; --- lane assembly: the bracket discipline and the one accumulator ---------
;;; The lisp mirror of core.zig StreamNormalizationState: spans open lazily on
;;; their first delta, text/reasoning close before :finish, and every buffered
;;; tool call drains to :tool-input-end + :tool-call exactly once. Every lane
;;; folds its dialect back into the SAME chat-shaped assistant message, because
;;; the event log stores exactly one dialect — so the bracket state and the
;;; message it assembles are one struct per attempt, not four hand-rolled LET
;;; groups, and consumers see one bracket contract rather than four. What
;;; differs per lane stays an argument at the call site, never a flag folded in
;;; here: which span a delta closes, whether an empty reasoning chunk counts as
;;; seen, the close order, and the lane's own replay fields.

(defstruct (lane-assembly (:copier nil))
  ;; Bracket state: ON-PART is the attempt's one observer, the two span ids
  ;; name what is open, and the emitted-p flag keeps :response-metadata
  ;; singular however many frames offer it (:finish fires once, at the lane's end).
  (on-part nil :read-only t)
  (text-id nil)
  (reasoning-id nil)
  (metadata-emitted-p nil)
  ;; Durable text: what the message replays to the provider next round,
  ;; independent of what the live stream showed.
  (content (make-string-output-stream) :read-only t)
  (reasoning (make-string-output-stream) :read-only t)
  ;; Seen-p, not "non-empty": a thinking-mode endpoint checks the key's
  ;; PRESENCE on replay, so an empty reasoning chunk still sets it.
  (reasoning-seen-p nil)
  (signature-seen-p nil)
  ;; How much reasoning the wire has streamed, in characters: the
  ;; thinking-tag route below engages only while this is zero.
  (reasoning-length 0)
  ;; The thinking-tag route (ROUTE-THINKING-TAG): :UNDECIDED until the
  ;; first non-blank text says whether the content opens with <thinking>,
  ;; :INSIDE while it does, :TEXT once content is content. PENDING holds
  ;; what cannot be routed yet — a prefix that may still become the tag,
  ;; or the tail that may still become its close.
  (tag-state :undecided)
  (tag-pending "")
  ;; tool-buffers: alist of (index . plist).
  ;; Each plist: :id :type :name :arguments :started-p :drained-p.
  (tool-buffers '())
  (usage (make-provider-usage) :read-only t))

(nlk:access (asm lane-assembly))

(defmacro define-lifecycle-span (name slot start-kind delta-kind end-kind)
  "The open/delta/close trio of one streamed span kind on the assembly's
SLOT: open once emitting START-KIND, delta (opening if needed) emitting
DELTA-KIND on the open id, close clearing the slot and emitting END-KIND."
  (let ((open (intern (format nil "LIFECYCLE-OPEN-~a" name)))
        (delta (intern (format nil "LIFECYCLE-~a-DELTA" name)))
        (close (intern (format nil "LIFECYCLE-CLOSE-~a" name)))
        (accessor (intern (format nil "LANE-ASSEMBLY-~a" slot))))
    `(progn
       (defun ,open (asm id)
         (unless (,accessor asm)
           (setf (,accessor asm) id)
           (emit-stream-part (lane-assembly-on-part asm) ,start-kind :id id)))
       (defun ,delta (asm id text)
         (,open asm id)
         (emit-stream-part (lane-assembly-on-part asm) ,delta-kind
                           :id (,accessor asm) :text text))
       (defun ,close (asm)
         (when-let (id (,accessor asm))
           (setf (,accessor asm) nil)
           (emit-stream-part (lane-assembly-on-part asm) ,end-kind :id id))))))

(define-lifecycle-span text text-id :text-start :text-delta :text-end)
(define-lifecycle-span reasoning reasoning-id
  :reasoning-start :reasoning-delta :reasoning-end)

(defun accumulate-tool-arguments (accumulated fragment)
  "Append streamed tool-arguments FRAGMENT to ACCUMULATED, superseding a
closed value."
  ;; OpenAI-compatible bridges over Anthropic models serialize the tool_use
  ;; block's empty input as a leading "{}" arguments chunk (some send it more
  ;; than once, some resend whole snapshots) before the real JSON deltas.
  ;; Blind concatenation yields {}{"form":...}, which decodes as an empty
  ;; object and silently drops the call's input. When ACCUMULATED already
  ;; holds a complete value and FRAGMENT opens a new one, the fragment
  ;; replaces the buffer; a genuinely incremental stream never closes early,
  ;; so real deltas always take the append path.
  (let ((start (ignore-errors (nlk::jsonc-trivia-end accumulated 0)))
        (end (length accumulated)))
    (if (and (find (find-if-not #'nlk::jsonc-space-p fragment) "{[")
             ;; ACCUMULATED closed: one top-level object or array, by structure, never a
             ;; parse -- the JSONC scanner's own value end, a string stepped over whole
             start (< start end)
             (find (char accumulated start) "{[")
             (eql end (ignore-errors (nlk::jsonc-trivia-end
                                      accumulated (nlk::jsonc-value-end accumulated start)))))
        fragment
        (concatenate 'string accumulated fragment))))

(defun lifecycle-tool-progress (asm index buf fragment)
  "Bracket one accumulated tool-call buffer after FRAGMENT was appended to it."
  ;; :tool-input-start fires once both the call id and the tool name are known
  ;; (the core.zig updateIndexedToolCall rule); input accumulated before that
  ;; flushes as one delta right after the start, so no fragment is lost to the
  ;; bracket.
  (destructuring-bind (&key id name started-p arguments &allow-other-keys) buf
    ;; after the start, the delta is the fragment; at it, all that accumulated
    (let ((text (if started-p fragment arguments)))
      (when (and (not started-p) (stringp id) (stringp name) (plusp (length name)))
        (setf (getf buf :started-p) t)
        (emit-stream-part asm.on-part :tool-input-start :id id :index index :tool-name name))
      (when (and (getf buf :started-p) (stringp text) (plusp (length text)))
        (emit-stream-part asm.on-part :tool-input-delta :id id :index index :tool-name name
                          :text text :arguments arguments)))))

(defparameter +thinking-open+ "<thinking>")
(defparameter +thinking-close+ "</thinking>")

(defun assembly-text-delta (asm id text &key close-reasoning)
  "Accumulate TEXT durably and stream it on span ID."
  ;; An empty chunk opens no span. CLOSE-REASONING first closes an open
  ;; thinking span — the chat.zig rule that text and reasoning never
  ;; interleave within one span.
  ;;
  ;; Content that opens with <thinking> on a wire that streamed no reasoning
  ;; is reasoning. Without this, an upstream that renders its thinking inline
  ;; instead of as reasoning_content would land that thinking as assistant
  ;; text — rows the operator reads as the answer, a transcript that says
  ;; nothing. The tag and what it encloses take the reasoning route
  ;; (ROUTE-THINKING-TAG); the text after its close, if any, is the
  ;; content.
  (if (eq asm.tag-state :text)
      (assembly-plain-text asm id text close-reasoning)
      (route-thinking-tag asm id text close-reasoning)))

(defun assembly-plain-text (asm id text close-reasoning)
  (write-string text asm.content)
  (when (plusp (length text))
    (when close-reasoning (lifecycle-close-reasoning asm))
    (lifecycle-text-delta asm id text)))

(defun thinking-span-id (asm)
  (or asm.reasoning-id "reasoning-thinking"))

(defun route-thinking-tag (asm id text close-reasoning)
  "TEXT through the thinking-tag state of ASM: held while it may still be
the tag or its close, reasoning while inside it, content otherwise."
  (let ((pending (concatenate 'string asm.tag-pending text)))
    (setf asm.tag-pending "")
    (ecase asm.tag-state
      (:undecided
       (let ((body (string-left-trim '(#\Space #\Newline #\Tab #\Return) pending)))
         (cond
           ;; The wire streams reasoning of its own: content is content.
           ((plusp (lane-assembly-reasoning-length asm))
            (setf asm.tag-state :text)
            (assembly-plain-text asm id pending close-reasoning))
           ((uiop:string-prefix-p +thinking-open+ body)
            (setf asm.tag-state :inside)
            (route-thinking-tag asm id (subseq body (length +thinking-open+)) close-reasoning))
           ;; Too short to tell, and still a prefix of the tag: hold.
           ((uiop:string-prefix-p body +thinking-open+) (setf asm.tag-pending pending))
           (t (setf asm.tag-state :text) (assembly-plain-text asm id pending close-reasoning)))))
      (:inside
       (if-let (close (search +thinking-close+ pending))
         (let ((rest (string-left-trim '(#\Newline #\Return)
                                       (subseq pending (+ close (length +thinking-close+))))))
           (assembly-reasoning-delta asm (thinking-span-id asm)
                                     (subseq pending 0 close) :close-text t)
           (setf asm.tag-state :text)
           (when (plusp (length rest))
             (assembly-plain-text asm id rest t)))
         ;; Stream all but a tail the size of the close tag less one:
         ;; what could still be its first characters.
         (let ((cut (max 0 (- (length pending) (1- (length +thinking-close+))))))
           (when (plusp cut)
             (assembly-reasoning-delta asm (thinking-span-id asm)
                                       (subseq pending 0 cut) :close-text t))
           (setf asm.tag-pending (subseq pending cut)))))
      (:text (assembly-plain-text asm id pending close-reasoning)))))

(defun flush-thinking-tag (asm &aux (pending asm.tag-pending))
  "Whatever ROUTE-THINKING-TAG still holds, routed at the end of the
attempt: an unclosed thinking span is reasoning to its last character, a
held prefix that never became the tag is text."
  (setf asm.tag-pending "")
  (when (plusp (length pending))
    (ecase asm.tag-state
      (:inside (assembly-reasoning-delta asm (thinking-span-id asm) pending))
      ((:undecided :text) (assembly-plain-text asm "txt-0" pending nil))))
  (setf asm.tag-state :text))

(defun assembly-reasoning-delta (asm id text &key close-text)
  "Accumulate TEXT durably, mark reasoning seen, and stream it on span ID."
  ;; CLOSE-TEXT first closes an open text span (the gen_ai.zig rule).
  (setf asm.reasoning-seen-p t)
  (incf asm.reasoning-length (length text))
  (write-string text asm.reasoning)
  (when (plusp (length text))
    (when close-text (lifecycle-close-text asm))
    (lifecycle-reasoning-delta asm id text)))

(defun assembly-signature-delta (asm text)
  "Note a reasoning signature and emit it as its own part: it licenses
replaying the thinking block next round, so it is replay material consumers
may persist, and never display text. The lane that replays signatures keeps
them itself, one per block (the Anthropic lane's reasoning_blocks)."
  (setf asm.signature-seen-p t)
  (when (plusp (length text)) (emit-stream-part asm.on-part :reasoning-signature :text text)))

(defun wire-call-id (id)
  "ID when the provider sent a non-empty one, else an id minted here."
  ;; Some OpenAI-compatible routes stream tool_call frames with no id, for
  ;; every call of a batch or now and then (GH #32). Kept as "", the call and
  ;; its result replay with empty ids, and a strict endpoint (OpenRouter's
  ;; validator) refuses the whole history with a 400 on every later turn.
  ;; The id only pairs a call with its result, so ours serves as well as
  ;; theirs; minted once here, it is stored with the call and replays as is.
  (if (and (stringp id) (plusp (length id)))
      id
      (nlk:make-durable-id "call")))

(defun open-tool-buffer (asm index &key id name arguments)
  "Buffer a whole tool call announced at INDEX and bracket it immediately."
  ;; A fresh buffer is never :started-p, so LIFECYCLE-TOOL-PROGRESS can only
  ;; take its start arm — which flushes any ARGUMENTS already known as one
  ;; delta right after the start, so nothing is lost to the bracket.
  (let ((buf (list :id id :type "function" :name name :arguments arguments
                   :started-p nil :drained-p nil)))
    (push (cons index buf) asm.tool-buffers)
    (lifecycle-tool-progress asm index buf nil)
    buf))

(defun assembly-tool-fragment (asm index fragment &aux (buf (cdr (assoc index asm.tool-buffers))))
  "Append FRAGMENT to the accumulated arguments of the call ASM buffers at
INDEX and re-bracket the call; nothing unless FRAGMENT is a string and a call
is buffered there."
  (when (and (stringp fragment) buf)
    (setf (getf buf :arguments)
          (accumulate-tool-arguments (getf buf :arguments) fragment))
    (lifecycle-tool-progress asm index buf fragment)))

(defun assembly-close-spans (asm &key (order '(:tools :reasoning :text)))
  "Close every span ASM holds open, in ORDER."
  ;; The order is a lane contract, not a detail: the chat and gen_ai lanes
  ;; drain tools first, the anthropic and responses lanes close text first.
  ;; Every step is idempotent.
  (dolist (step order)
    (ecase step
      ;; every buffered call brackets once, in index order, with whatever id and name arrived
      (:tools
       (loop for (index . buf) in (sort (copy-list (lane-assembly-tool-buffers asm)) #'< :key #'car)
             for id = (or (getf buf :id) "")
             for name = (or (getf buf :name) "")
             unless (getf buf :drained-p)
               do (setf (getf buf :drained-p) t)
                  (unless (getf buf :started-p)
                    (setf (getf buf :started-p) t)
                    (emit-stream-part asm.on-part :tool-input-start
                                      :id id :index index :tool-name name))
                  (emit-stream-part asm.on-part :tool-input-end :id id :index index)
                  (emit-stream-part asm.on-part :tool-call
                                    :id id :index index :tool-name name
                                    :arguments (getf buf :arguments))))
      (:reasoning (lifecycle-close-reasoning asm))
      (:text (lifecycle-close-text asm)))))

;;; --- lane registry --------------------------------------------------------

(defstruct (provider-lane (:copier nil))
  "One provider dialect: symbols, not functions, so a test or a mutation that
rebinds the fdefinition is honored at the next call."
  ;; One fold per lane. STREAM-SYMBOL is the lane's whole entry: a round with
  ;; no observer is what "blocking" means, spelled :ON-PART NIL, so a dialect
  ;; never carries a second transport for the caller who is not watching.
  (name (error "lane name required") :type string :read-only t)
  (stream-symbol (error "stream symbol required") :type symbol :read-only t)
  (default-endpoint (error "default endpoint required") :type string :read-only t)
  ;; The SDK family (the Provider enum in providers/root.zig): drives the
  ;; env-ladder default key name, and is the fact a :CREDENTIAL hook reads to
  ;; decide whether its own auth style applies to this lane.
  (family :openai-completions :type keyword :read-only t)
  ;; How a continuation hands a cut attempt's reasoning back to the model:
  ;; :REASONING as the assistant message's reasoning_content — the chat wire
  ;; replays it verbatim and the model resumes from it; :TEXT as assistant
  ;; text, for wires whose thinking blocks must be signed or id-bearing to
  ;; replay at all (Anthropic signatures, Responses reasoning item ids,
  ;; Google thought signatures) — the model still sees its own thought, as
  ;; prose rather than as thinking.
  (reasoning-carry :reasoning :type (member :reasoning :text) :read-only t)
  ;; What a provider's base_url composes with to reach this lane (LANE-ENDPOINT;
  ;; google appends its model segments per request), and the models.dev npm
  ;; package -- or packages -- whose catalog providers ride it
  ;; (CATALOG-NPM-LANE-NAME).
  (path "" :type string :read-only t)
  (npm nil :type (or null string list) :read-only t))

(nlk:access (lane provider-lane))

(defparameter *provider-lanes* '()
  "Registered provider lanes, newest first.")

(defun register-provider-lane (lane)
  (check-type lane provider-lane)
  (setf *provider-lanes*
        (cons lane (remove lane.name *provider-lanes* :key #'provider-lane-name :test #'string=)))
  lane.name)

(defun shared-config-where ()
  "The shared config as a refusal names it: its path with the operator's
home written `~', else `the shared config' when this process reads none."
  (if *shared-config-path*
      (home-abbreviated (uiop:native-namestring *shared-config-path*))
      "the shared config"))

(defun find-provider-lane (name)
  "The lane the provider NAME rides — CONFIGURED-PROVIDER-LANE's answer —
or PROVIDER-ERROR saying why there is none: an sdk no lane recognizes, a
config entry naming no sdk at all, or no provider by that name."
  (if-let (lane (configured-provider-lane name))
    (find-lane-by-name lane)
    (let* ((entry (configured-provider-entry name))
           (sdk (and entry (trimmed-config-string entry "sdk")))
           (where (shared-config-where)))
      (error 'provider-config-error
             :detail (cond
                       (sdk (format nil "provider ~s names an unknown sdk ~s ~
                                             (one of ~{~a~^, ~})"
                                    name sdk +sdk-names+))
                       (entry (format nil "provider ~s names no sdk: set ~
                                               providers.~a.sdk in ~a to one ~
                                               of ~{~a~^, ~}"
                                      name name where +sdk-names+))
                       ;; No lane, catalog entry or config entry: nothing here
                       ;; may stand in for it (vr-101, a typo that went to Zen).
                       ((models-catalog-table)
                        (format nil "provider ~a is not one this box knows: it is ~
                                     not a lane, not a models.dev provider and not ~
                                     under providers in ~a; pick a model in /setup"
                                name where))
                       ;; Offline the list never loads; without fetch-failure,
                       ;; `in a moment' would be said every turn.
                       ((when-let (why (models-catalog-fetch-failure))
                          (format nil "provider ~a is not a lane or under providers in ~a, ~
                                       and the models.dev list that would name it could not ~
                                       be fetched (~a); try again once models.dev answers, or ~
                                       add providers.~a with an sdk and a base_url"
                                  name where why name)))
                       (t (format nil "provider ~a is not a lane or under providers ~
                                       in ~a, and the models.dev list that could ~
                                       name it has not loaded yet; try again in a ~
                                       moment, or pick a model in /setup"
                                  name where)))))))

(defun selection-refusal (provider)
  "Why PROVIDER cannot serve a turn — the config refusal's detail (an sdk no
lane recognizes, an entry naming no sdk, no provider at all, no endpoint of
its own) — or NIL when it resolves a lane and an endpoint."
  ;; The one check every shell shows the moment a selection lands: an operator
  ;; learns at boot what the first turn would otherwise teach, in the words
  ;; that name the key to set. The explicit *ENDPOINT* override, while set, is
  ;; the address every turn rides (the freeze weighs it first), so only the
  ;; lane is asked then.
  (handler-case (progn (if *endpoint* (find-provider-lane provider) (provider-endpoint provider)) nil)
    (provider-config-error (condition) (provider-error-detail condition))))

(defun provider-base-url (provider)
  "The base PROVIDER's endpoint composes from and its listing is fetched
against: the configured entry's base_url, else the models.dev catalog's api
base — the tier that makes an auth.json-credentialed catalog provider
reachable without a config entry. NIL when neither names one."
  ;; One rule for both readers: the endpoint a turn dials and the endpoint
  ;; /models asks for a listing are the same endpoint, so a provider a turn
  ;; can reach is a provider the picker can list. Without the catalog's api
  ;; fallback, the picker would list only providers whose config entry carries
  ;; a base_url.
  (or (trimmed-config-string (configured-provider-entry provider) "base_url")
      (trimmed-config-string (and (stringp provider)
                                  (json-value (models-catalog-table) :object provider))
                             "api")))

(defun local-endpoint-p (url)
  "Whether URL's host is a machine of the operator's own: localhost, a
.local, .localhost or tailnet (.ts.net) name, or a loopback, private
(RFC 1918, fc00::/7), shared tailnet (100.64/10) or link-local address."
  ;; The host alone decides: a server on this box, the LAN or the tailnet is
  ;; the operator's, whatever port or path it answers on.
  (let ((host (string-downcase (string-trim "[]" (or (ignore-errors (quri:uri-host (quri:uri url))) ""))))
        (octets nil))
    (and (plusp (length host))
         (or (member host '("localhost" "::1") :test #'string=)
             (some (lambda (suffix) (uiop:string-suffix-p host suffix)) '(".localhost" ".local" ".ts.net"))
             (and (find #\: host) (member (subseq host 0 (min 2 (length host))) '("fc" "fd") :test #'string=))
             (progn
               (setf octets (mapcar (lambda (part) (ignore-errors (parse-integer part)))
                                    (uiop:split-string host :separator ".")))
               (and (= 4 (length octets)) (every #'integerp octets)
                    (destructuring-bind (a b &rest _) octets
                      (declare (ignore _))
                      (or (= a 127) (= a 10) (and (= a 172) (<= 16 b 31)) (and (= a 192) (= b 168))
                          (and (= a 100) (<= 64 b 127)) (and (= a 169) (= b 254)))))))
         t)))

(defun lane-default-endpoint (provider lane)
  "LANE's default endpoint, for PROVIDER when that address is its own:
PROVIDER is the lane's name (`anthropic', `google', the build's keyless
`openai-completions') or its family's (`openai' on openai-responses). Any
other provider with no base_url has nowhere to send a request, and is
refused by name."
  ;; The rule reads no catalog, so an unloaded list moves nothing:
  ;; models.dev's only providers on a lane's package with no api base are
  ;; those lanes' own. It can supply a base, though, so its absence is said.
  (if (or (equal provider lane.name) (string-equal provider (symbol-name lane.family)))
      lane.default-endpoint
      (let ((unloaded (null (models-catalog-table))))
        (error 'provider-config-error
               :detail (format nil "providers.~a has no base_url~:[, so its requests have nowhere to go~;, ~
                                    and the models.dev list that could name its address has not loaded yet~]: ~
                                    set providers.~a.base_url in ~a to the address that serves it~:[~;, ~
                                    or try again in a moment~]"
                               provider unloaded provider (shared-config-where) unloaded)))))

(defun lane-endpoint (provider lane &aux (base (provider-base-url provider))
                                         (resolved (or lane (resolve-model-lane provider *model*))))
  "The endpoint PROVIDER's LANE rides: PROVIDER-BASE-URL — the configured
provider's base_url, then the models.dev catalog's api base for an
auth-credentialed catalog provider — composed with the lane path, then the
lane's default when that is PROVIDER's own (LANE-DEFAULT-ENDPOINT)."
  ;; The explicit
  ;; *ENDPOINT* override is deliberately not a rung here: it names one lane's
  ;; path, so only EFFECTIVE-ENDPOINT weighs it, against the lane it resolves.
  ;; A base composes with LANE, else the lane RESOLVE-MODEL-LANE picks for the
  ;; selected model, the kit.buildRequestUrl way: right-trimmed of slashes, then
  ;; the lane's path.
  (if (and base resolved)
      (uiop:strcat (string-right-trim "/" base) (provider-lane-path (find-lane-by-name resolved)))
      (lane-default-endpoint
       provider (if lane (find-lane-by-name lane) (find-provider-lane provider)))))

(defun provider-endpoint (provider)
  "The endpoint PROVIDER's own lane rides (LANE-ENDPOINT), or
PROVIDER-CONFIG-ERROR saying why there is none: no lane, or no address of
its own."
  (lane-endpoint provider (provider-lane-name (find-provider-lane provider))))

(defun effective-endpoint ()
  "The explicit *ENDPOINT* override, then the configured provider's
base_url composed with the lane path, then the models.dev catalog's api
base for an auth-credentialed catalog provider, then the lane's default."
  ;; The lane is the one RESOLVE-MODEL-LANE picks for the selection.
  (or *endpoint* (lane-endpoint *provider* (resolve-model-lane *provider* *model*))))

;;; --- reasoning effort: the rungs a target admits ------------------------------
;;; A session's effort pick rides its model selection fact, the `effort' beside
;;; its provider and model (the shape general.default_model spells it), and
;;; binds a turn only while that turn's (provider, model) target admits it. The
;;; ladder exists unless the catalog positively says the model does not
;;; reason: a model no source recognizes offers the whole ladder, because
;;; unknown is not "no" and a provider refuses a rung it does not know loudly.

(defun effort-ladder (provider model &aux (capability (resolve-model-capability model provider)))
  "The reasoning-effort rungs the (PROVIDER, MODEL) target admits, weakest
first, or NIL when the catalog says the model does not reason."
  (when (or (not capability.known-p) capability.reasoning-p)
    ;; Disabling is a toggle, not an enabled effort tier: catalogs can
    ;; list only the latter. Keep their rungs intact and offer Off once,
    ;; as Pi's supported-thinking-levels surface does.
    (adjoin "off" (or capability.effort-levels +effort-ladder+) :test #'equal)))

(define-refusal effort-refusal refuse-effort "~a"
  "CHECK-EFFORT refused a pick before anything moved.")

(defun check-effort (provider model effort &aux (ladder (effort-ladder provider model)))
  "Refuse EFFORT unless it is NIL, which clears a pick, or a rung of the
(PROVIDER, MODEL) target's own ladder. => EFFORT."
  ;; Every /think pick, the HTTP effort route and the model-facing
  ;; SELECT-EFFORT are checked here before anything is written, so a refusal
  ;; leaves no half-moved state (SET-SESSION-EFFORT).
  (cond ((null effort))
        ((null ladder)
         (refuse-effort "~a/~a does not reason: it has no effort to pick" provider model))
        ((not (member effort ladder :test #'equal))
         (refuse-effort "~s is not a reasoning effort ~a/~a offers; the ladder: ~{~a~^ ~}"
                        effort provider model ladder)))
  effort)

(defun admitted-effort (effort provider model)
  "EFFORT, a session's pick, while the (PROVIDER, MODEL) target admits it,
else NIL."
  ;; A pick made on one target outlives a change of target: a session that
  ;; follows the default keeps its reasoning when the default moves. A rung
  ;; the new target lacks reads as no pick, so the default effort binds
  ;; (EFFECTIVE-EFFORT), instead of a request the endpoint refuses.
  (and effort (member effort (effort-ladder provider model) :test #'equal) effort))

(defun effective-effort (pick provider model)
  "The effort a turn on the (PROVIDER, MODEL) target binds: PICK, a session's
own, while that target admits it, else the default (*REASONING-EFFORT*)."
  ;; The effort half of RUN's selection overlay: a session's pick over the
  ;; default the way its model pin is over the default model. The default
  ;; passes as it is, a rung the vocabulary does not know included
  ;; (CLAMP-EFFORT's rule for what the operator configured).
  (or (admitted-effort pick provider model) *reasoning-effort*))

;;; --- retry policy ---------------------------------------------------------

(defun retryable-provider-error-p (condition &aux (status (provider-error-status condition)))
  "True when CONDITION represents a transient provider failure worth retrying."
  ;; Retryable: nil status (timeout, network error, truncation, empty
  ;; response, in-stream error without a code), 408, 409, 425, 429, and every
  ;; 5xx. Not retryable: 400, 401, 403, 404, 413, 422 — a second identical
  ;; request cannot succeed where the first was rejected for content or auth
  ;; reasons.
  (and (not (typep condition 'provider-refused))
       (or (null status)
           (member status '(408 409 425 429))
           (and (integerp status) (>= status 500)))))

(defparameter +transport-failures+
  '((sb-sys:deadline-timeout "timeout") (usocket:timeout-error "timeout")
    (usocket:connection-refused-error "connection refused")
    (usocket:connection-reset-error "connection reset" t)
    (usocket:host-unreachable-error "host unreachable") (usocket:ns-error "host not found")
    (usocket:network-unreachable-error "network unreachable")
    (cl+ssl::ssl-error-verify "tls certificate rejected")
    (cl+ssl::ssl-error-syscall "connection reset" t) (cl+ssl::ssl-error "tls failure" t))
  "(CLASS LABEL TRANSIENT-P) for each failure below HTTP a request can meet, a
class before its superclass: what TRANSPORT-FAILURE-LABEL calls it and whether
TRANSIENT-TRANSPORT-FAILURE-P expects one more attempt to succeed.")

(defun transport-failure-row (condition)
  "CONDITION's row of +TRANSPORT-FAILURES+, or NIL when no class names it."
  (find-if (lambda (row) (typep condition (first row))) +transport-failures+))

(defun url-address (url &aux (uri (and url (ignore-errors (quri:uri url)))))
  "URL's `host[:port]', the address a failure to reach it names, or NIL when it names no host."
  (and uri (quri:uri-host uri) (format nil "~a~@[:~d~]" (quri:uri-host uri) (quri:uri-port uri))))

(defun transport-failure-label (condition &optional url &aux (where (url-address url)))
  "One short reason for a request that failed below HTTP — no status to
show — named from CONDITION's class, the transport fact, never from its
message text; a refusal of URL, the address dialed, names where nothing
listens, and a host that is not found or not reached names that address."
  ;; cl+ssl reports a peer's TCP reset as SSL_ERROR_SYSCALL and prints it as
  ;; `undocumented reason (SSL_get_error: 5)`. A condition no clause names
  ;; keeps its class, never its printed self, so an unknown failure stays
  ;; diagnosable: a socket closed before the head printed the stream it read,
  ;; `#<SB-SYS:FD-STREAM for "socket ...">', or dexador's `SIMPLE-ERROR: maybe
  ;; invalid header'. The listing GET and the turn's request POST both read
  ;; failures through this. Without the host-error clause, a bare `host not
  ;; found' would name no host.
  (cond ((and where (typep condition 'usocket:connection-refused-error))
         (format nil "nothing listens at ~a" where))
        ((and where (typep condition '(or usocket:ns-error usocket:host-unreachable-error)))
         (format nil "~a at ~a" (second (transport-failure-row condition)) where))
        ((second (transport-failure-row condition)))
        ((typep condition 'end-of-file)
         (format nil "the connection closed before an answer~@[ from ~a~]" where))
        (t (format nil "the request~@[ to ~a~] failed (~(~a~))" where (type-of condition)))))

(defun stream-cut-detail (condition)
  "What an answer's stream that failed mid-read says of it: the transport
fact in words, never CONDITION's printed self."
  ;; Without this code, a read error would print the stream it read from,
  ;; and the web page's cut-off note would show that. An unnamed class
  ;; keeps its name, so the failure stays diagnosable.
  (if (typep condition 'end-of-file)
      "the provider closed the connection mid-answer"
      (format nil "the connection broke mid-answer (~a)"
              (or (second (transport-failure-row condition))
                  (string-downcase (symbol-name (type-of condition)))))))

(defun provider-failure-reason (condition &aux (status (provider-error-status condition)))
  "Why the provider failure CONDITION failed: a label from the vocabulary
the failover fact and the terminal copy speak (the failover-reason set the
recon in issue #18 found in openclaw) when a wire status or the failure's
class names one, else its detail -- below HTTP, the transport fact the
signal site wrote in words (`host not found at api.anthropic.com:443') --
or `unknown' when that is empty."
  ;; `auth' is a refused key, one rule with /models's (REFUSED-KEY-ANSWER-P):
  ;; the import's check read a 403 as refused and Google's 400 as not
  ;; Below HTTP the signal site already named the transport fact, and it is the
  ;; detail: labelled again, the condition's own class and report came back,
  ;; `PROVIDER-ERROR: provider request failed [...]: host not found'.
  (cond
    ((refused-key-answer-p status (error-body-code (provider-error-evidence-body condition))) "auth")
    ((cdr (assoc status '((429 . "rate_limit") (402 . "billing") (403 . "forbidden")
                          (404 . "not_found") (408 . "timeout") (503 . "overloaded")))))
    ((and (integerp status) (>= status 500)) "server_error")
    ((integerp status) (format nil "http_~d" status))
    ((typep condition 'provider-connection-refused) "connection_refused")
    ((typep condition 'provider-slow-start) "slow_start")
    ((typep condition 'provider-stream-incomplete) "stream_incomplete")
    ((typep condition 'provider-empty-response) "empty_response")
    ((typep condition 'provider-refused) "refused")
    ((typep condition 'provider-malformed-tool-call) "malformed_tool_call")
    ((plusp (length (provider-error-detail condition))) (provider-error-detail condition))
    (t "unknown")))

(defun transient-transport-failure-p (condition)
  "True when one more attempt can plausibly succeed where CONDITION's
failed: a reset, or a TLS session that died under the request."
  ;; A refusal and an unknown or unreachable host answer the same way twice; a
  ;; rejected certificate is a fact about the endpoint; a timeout has already
  ;; spent the budget a retry would spend again.
  (third (transport-failure-row condition)))

(defun retry-scope-name (scope)
  "The wire name of a retry scope keyword (the retry_classifier.zig
scopeName strings)."
  (format nil "provider_~(~a~)" (if (member scope '(:request :contract)) scope :stream)))

(defun provider-retry-delay (attempt base-delay &key retry-after status)
  "Seconds to sleep before per-scope retry number ATTEMPT (1-based)."
  ;; A provider-declared Retry-After wins outright, capped at 60 seconds with
  ;; no jitter and no curve (the Zig retryDelayMs rule: the provider knows its
  ;; own load better than our curve). Otherwise exponential back-off from
  ;; BASE-DELAY doubling per retry, capped at 8 seconds, jittered ±10%. A 503
  ;; is the one status with its own floor — base at least 5 seconds, cap
  ;; raised to 60 — the service_unavailable override; 429 and the other 5xx
  ;; ride the plain curve.
  (if (and (realp retry-after) (plusp retry-after))
      (min retry-after 60)
      (let* ((unavailable-p (eql status 503))
             (base (max base-delay 1/1000 (if unavailable-p 5 0))))
        (* (min (* base (expt 2 (1- attempt))) (max 8 base (if unavailable-p 60 0)))
           (+ 1 (/ (- (random 2001) 1000) 10000.0))))))

;;; --- shared operator config -------------------------------------------
;;; One reader for ~/.nodecode/config.jsonc — the SAME operator config the
;;; Zig gateway loads (config.zig buildLocalPaths, JSONC-normalized). The
;;; configured-provider tier below is the lisp mirror of the
;;; providers/root.zig config collapse: providers.<id>.sdk names the lane,
;;; base_url the endpoint base, api_key the config credential, models the
;;; /models merge input, and the stream_* fields the per-provider retry
;;; tuning. Every consumer (/models, the credential chain, cells)
;;; reads through this seam; no second parser exists.

;;; NIL disables shared-config reads entirely — the hermetic test posture.
;;; Resolved at process start: a release binary under another $HOME reads THAT
;;; home's config, never the builder's.
(define-startup-parameter *shared-config-path*
  (home "config.jsonc")
  "The shared operator config (config.zig buildLocalPaths).")

(defun read-shared-config ()
  "(values CONFIG-OBJECT ERROR-TEXT): the parsed shared config, or NIL and
why."
  ;; An absent file (or a NIL path) is (values NIL NIL) — not configured is
  ;; not an error. Memoized on (path, size, mtime), the models-catalog
  ;; posture.
  (read-json-file-memo '*shared-config-memo* *shared-config-path*
                       :decoder (lambda (octets)
                                  (nlk:decode-json
                                   (nlk::normalize-jsonc
                                    (sb-ext:octets-to-string octets :external-format :utf-8))))))

(defparameter *shared-config-write-lock*
  (bt2:make-lock :name "shared-config-write"))

;;; --- the secret vocabulary ---------------------------------------------
;;; One place says which members of this document hold a credential, and one
;;; string stands in for a credential's value. The reader below redacts to
;;; that string and the writer refuses to write it: without both halves
;;; naming the same constant they drift, and a later write would replace
;;; live keys with the sentinel.

(defparameter *redacted-secret* "[set]"
  "What CONFIG-GET answers in place of a secret's value, and the one string
PERSIST-SHARED-CONFIG will not write where a secret belongs.")

;;; The names that hold a REFERENCE to a secret -- bot_token_env, token_file
;;; -- end otherwise and read as they are. Data: a layer can widen it.
(defparameter *secret-key-suffixes*
  '("api_key" "token" "secret" "password" "passwd" "passphrase"
    "access_key" "private_key" "authorization" "cookie")
  "Member names whose string values CONFIG-GET answers as *REDACTED-SECRET*
rather than as the value, matched with case, `_' and `-' set aside: api_key,
apiKey and X-Api-Key alike, an AWS secret_access_key, a header's
Authorization or Cookie. A config path that names a secret is read for its
presence, never echoed into a transcript.")

(defun secret-key-p (key)
  (flet ((bare (name) (remove-if (lambda (char) (find char "_-")) name)))
    (let ((name (bare key)))
      (some (lambda (suffix) (alexandria:ends-with-subseq (bare suffix) name :test #'char-equal))
            *secret-key-suffixes*))))

(defun secret-path-p (keys &aux (name (find-if #'stringp keys)))
  "Whether the leaf KEYS reach -- innermost first, a member under its name and
an element under its index -- holds a secret: its own name is a secret's
(SECRET-KEY-P), or for an element the name of the list it sits in, so a key
list under api_key reads as secrets too. A name further up never counts: a
provider's or a server's id is the operator's word (sub-cookie)."
  (and name (secret-key-p name)))

(defun redact-url-credentials (text &aux (scheme (search "://" text)))
  "TEXT with what a URL in it carries as a credential read as
*REDACTED-SECRET*: the user:password@ before its host, and the value of a
query parameter named key or named as a secret (SECRET-KEY-P); text that is
no URL as it is."
  (if (null scheme)
      text
      (let* ((host (+ scheme 3))
             (path (or (position-if (lambda (char) (find char "/?#")) text :start host)
                       (length text)))
             (at (position #\@ text :start host :end path :from-end t))
             (query (position #\? text :start path))
             (fragment (and query (position #\# text :start query))))
        (flet ((parameter (pair &aux (is (position #\= pair)))
                 (if (and is (let ((name (subseq pair 0 is)))
                               (or (string-equal name "key") (secret-key-p name))))
                     (concatenate 'string (subseq pair 0 (1+ is)) *redacted-secret*)
                     pair)))
          (concatenate 'string
                       (subseq text 0 host)
                       (if at *redacted-secret* "")
                       (subseq text (or at host) (or query (length text)))
                       (if query
                           (format nil "?~{~a~^&~}"
                                   (mapcar #'parameter
                                           (uiop:split-string (subseq text (1+ query)
                                                                      (or fragment (length text)))
                                                              :separator "&")))
                           "")
                       (if fragment (subseq text fragment) ""))))))

;;; --- one walk over a JSON tree ---------------------------------------------
;;; Three readers take a JSON value apart the same way: the deep copy a caller
;;; takes before it writes into a value it does not own (COPY-JSON-VALUE), the
;;; secret redaction below, and the refusal that finds a sentinel about to be
;;; written back over a live key. One walk rebuilds the tree and hands every
;;; leaf to FN with the key path that reaches it.

(defun map-json-leaves (value fn &optional path)
  "VALUE rebuilt with FN over each leaf, FN taking the leaf and the key path
that reaches it -- innermost first, a member under its name (a symbol key
downcased), an element under its index. Objects, arrays, lists and keyword
plists are walked; a string is copied before FN sees it, so a value FN hands
back unchanged still shares nothing with VALUE."
  (flet ((down (item key)
           (map-json-leaves item fn (cons (if (symbolp key) (string-downcase key) key) path))))
    (cond ((hash-table-p value)
           (let ((copy (make-hash-table :test #'equal)))
             (maphash (lambda (key item) (setf (gethash key copy) (down item key))) value)
             copy))
          ((stringp value) (funcall fn (copy-seq value) path))
          ((vectorp value)
           (let ((index -1)) (map 'vector (lambda (item) (down item (incf index))) value)))
          ((and (consp value) (keywordp (first value)) (evenp (length value)))
           (loop for (key item) on value by #'cddr collect key collect (down item key)))
          ((consp value)
           (let ((index -1)) (mapcar (lambda (item) (down item (incf index))) value)))
          (t (funcall fn value path)))))

(defun persist-shared-config (path value &key (file *shared-config-path*))
  "Write VALUE at PATH -- a list of member keys from the root -- into the
shared operator config: one atomic write, every comment and every other
byte kept (NLK:SET-JSONC-PATH: an object value merges its members in,
anything else replaces the member, and :REMOVE takes the member out)."
  ;;
  ;; => (values OUTCOME DETAIL). Exactly three outcomes, and no condition escapes:
  ;;
  ;;   :written   the document says what you asked. DETAIL is the diff as one
  ;;              line -- `channels.discord: set enabled, bot_token_env (2 added,
  ;;              0 replaced)', `providers.quiai: removed', or
  ;;              `general.theme: unchanged'; a change touching secret-named
  ;;              members ends `; N secrets'. Bytes may not have moved: a
  ;;              document that already said it is left alone, so its mode, its
  ;;              mtime and READ-SHARED-CONFIG's memo over both stay valid. The
  ;;              caller's question is whether the config says this now, not
  ;;              whether a write happened.
  ;;   :disabled  FILE is NIL -- the hermetic test posture, and not a failure.
  ;;              DETAIL NIL.
  ;;   :failed    nothing was written. DETAIL says why, fit to log or put on the
  ;;              wire: a value that would write *REDACTED-SECRET* where a secret
  ;;              belongs, a document the splice refuses to guess at, a value that
  ;;              is not JSON, a path through a member that is not an object.
  ;;
  ;; The redaction sentinel is refused before the lock is taken. CONFIG-GET
  ;; answers a secret as that string, so without this refusal, reading a section,
  ;; changing one member, and writing the section back would launder every
  ;; credential in it, replacing each with *REDACTED-SECRET*. The reader and
  ;; this refusal name the same constant, so they cannot drift apart.
  ;;
  ;; The file is the operator's alone (:PRIVATE): this document holds
  ;; providers.<id>.api_key and gateway.operator_token, so the write that
  ;; creates it makes it 0600 and a file left readable by others is narrowed
  ;; back to its owner.
  (let ((redacted
          ;; the dotted paths where VALUE would write *REDACTED-SECRET* over a secret
          (let ((found '()))
            (map-json-leaves
             value
             (lambda (leaf keys)
               ;; a secret's own sentinel, or a URL carrying it where
               ;; (KEYS run innermost first, PATH reversed below)
               ;; REDACT-URL-CREDENTIALS read a credential out
               (when (and (stringp leaf)
                          (if (secret-path-p keys)
                              (string= leaf *redacted-secret*)
                              (and (search "://" leaf) (search *redacted-secret* leaf))))
                 (push (format nil "~{~a~^.~}" (reverse keys)) found))
               leaf)
             (reverse path))
            (nreverse found))))
   (cond
    ((null file) (values :disabled nil))
    (redacted
     (values :failed
             (format nil "~{~a~^, ~} would be written carrying ~s, the string a ~
                          secret reads as -- write only the members you mean ~
                          to change, or :remove to take one out"
                     redacted *redacted-secret*)))
    (t
     (bt2:with-lock-held (*shared-config-write-lock*)
       (with-handlers ((error (condition) (values :failed (princ-to-string condition))))
         (bind ((existing (and (probe-file file) (uiop:read-file-string file)))
                ((text added replaced removed) (nlk:set-jsonc-path (or existing "") path value)))
           (when (or added replaced removed)
             (nlk:write-file-atomically file text :mode :private)
             ;; FILE-WRITE-DATE is second-granular and the memo keys on it
             ;; together with the size, so two writes inside one second at
             ;; equal length would otherwise read back stale.
             (setf *shared-config-memo* nil))
           ;; the diff as one line, ending with a count of the secret-named members moved
           (let* ((dotted (format nil "~{~a~^.~}" (if (listp path) path (list path))))
                  (prefix (concatenate 'string dotted ".")))
             (flet ((relative (leaf)
                      (cond ((string= leaf dotted) nil)
                            ((uiop:string-prefix-p prefix leaf) (subseq leaf (length prefix)))
                            (t leaf))))
               ;; every answer is `PATH: what moved', and none moved a secret but the count says so
               (values :written
                       (format nil "~a: ~a~[~:;; ~:*~d secret~:p~]" dotted
                               (cond
                                 (removed "removed")
                                 ((and (null added) (null replaced)) "unchanged")
                                 ((and (= 1 (+ (length added) (length replaced)))
                                       (null (relative (first (or added replaced)))))
                                  (if added "added" "replaced"))
                                 (t
                                  (format nil "set ~{~a~^, ~} (~d added, ~d replaced)"
                                          (mapcar (lambda (leaf) (or (relative leaf) "value"))
                                                  (append added replaced))
                                          (length added) (length replaced))))
                               (count-if #'secret-key-p (append added replaced removed)))))))))))))

(defun persist-shared-model-pin (key provider model)
  "general.KEY — default_model or auxiliary_model — as the pin object
{provider, model}, both members in one write."
  ;; The object merges member by member (NLK:SET-JSONC-PATH), so an effort the
  ;; operator wrote stays.
  ;;
  ;; Both ids are required (the rule the Zig saveDefaultModelSelection kept
  ;; for its flat pair): a half-written pin names a model under whatever
  ;; provider the file happened to carry, which is exactly how the shared
  ;; config came to hold a model one lane had never served.
  (if (and (stringp provider) (plusp (length (string-trim '(#\Space #\Tab) provider)))
           (stringp model) (plusp (length (string-trim '(#\Space #\Tab) model))))
      (multiple-value-prog1
          (persist-shared-config (list "general" key)
                                 (list :provider provider :model model))
        ;; The retired flat partner goes out with the same write that
        ;; supersedes it. SEED-MODEL-CONFIG-FROM-SHARED-CONFIG refuses the whole general
        ;; section while a `default_provider' STRING is present, and writing
        ;; the pin object beside it does not take it away — so a file carrying
        ;; the old spelling stayed refused no matter how many times the pin was
        ;; written, and the only way out was to hand-edit JSON. An operator
        ;; arriving from a build before e33aad963 hit exactly that, through the
        ;; Hermes import, which had done its job correctly. This is the
        ;; no-legacy position, not a shim: the retired key is DELETED at the
        ;; moment its replacement lands, never read and never honoured.
        ;; Removing a member that is not there rewrites nothing.
        (when (equal key "default_model")
          (persist-shared-config (list "general" "default_provider") :remove)))
      (values :failed "provider and model are both required")))

(defun check-model-id (provider model)
  "MODEL unchanged, or PROVIDER-CONFIG-ERROR for an empty id or a known
listing/catalog display name. Exact ids win; unlisted private ids stay legal."
  ;; Never translate a name into an id: resellers can use different namespaces,
  ;; and one display name can describe several ids. Tell the caller what to pick.
  (unless (and (stringp model) (plusp (length (nlk:trimmed model))))
    (error 'provider-config-error :provider provider :model model
           :detail "the model id must be a non-empty string"))
  (when (configured-model-entry provider model)
    (return-from check-model-id model))
  (let* ((rows (getf (provider-models-cache-entry provider) :models))
         (catalog (models-catalog-table))
         (candidates (loop for row in rows
                           when (equal model (getf row :display)) collect (getf row :id))))
    (when (or (find model rows :key (lambda (row) (getf row :id)) :test #'equal)
              (and catalog
                   (loop for entry being the hash-values of catalog
                         thereis (json-value entry :any "models" model))))
      (return-from check-model-id model))
    (flet ((names (entry)
             (when-let (models (json-value entry :object "models"))
               (maphash (lambda (id row)
                          (when (equal model (catalog-model-name row))
                            (pushnew id candidates :test #'equal)))
                        models))))
      (names (and catalog (gethash provider catalog)))
      ;; A configured aggregator is not a models.dev provider. Its labels still
      ;; have useful id hints in the catalog, not an implicit provider switch.
      (when (and catalog (null candidates))
        (maphash (lambda (id entry) (declare (ignore id)) (names entry)) catalog)))
    (when candidates
      (error 'provider-config-error :provider provider :model model
             :detail (format nil "~s is a display name, not a model id for ~a; select an explicit id: ~{~a~^, ~}"
                             model provider (sort candidates #'string<)))))
  model)

(defun select-auxiliary-model (model &key provider (effort nil effort-p))
  "Make (PROVIDER MODEL) this image's auxiliary target and the shared
config's general.auxiliary_model, in that order — the one place an
auxiliary pick is made, for /models' Auxiliary tab, `/models aux', the
gateway's auxiliary-model op and the model-facing form alike. A NIL MODEL
takes the pin out of both: side work runs on each session's own target again.
With EFFORT, the pin's reasoning effort is that rung of the model's ladder,
or with NIL the lightest it offers (AUXILIARY-MODEL)."
  ;; PROVIDER defaults to the default provider, the way a model-only /models
  ;; pick keeps its lane; without EFFORT, an effort already configured for the
  ;; target stays. The lane, its endpoint and the rung are checked first
  ;; (PROVIDER-ENDPOINT, CHECK-EFFORT refuse by name), so a rejected pick
  ;; moves nothing. => (values PROVIDER MODEL OUTCOME DETAIL), the pin and
  ;; PERSIST-SHARED-CONFIG's answer.
  (check-type model (or null model-id))
  (check-type provider (or null provider-id))
  (if (null model)
      (progn (setf *auxiliary-model* nil)
             (multiple-value-call #'values nil nil
               (persist-shared-config '("general" "auxiliary_model") :remove)))
      (let ((next (or provider *provider*)))
        (check-model-id next model)
        (provider-endpoint next)
        (when effort-p (check-effort next model effort))
        (setf *auxiliary-model*
              (list :provider next :model model
                    :effort (if effort-p effort (getf *auxiliary-model* :effort))))
        (multiple-value-bind (outcome detail) (persist-shared-model-pin "auxiliary_model" next model)
          (if (and effort-p (not (eq outcome :failed)))
              (multiple-value-call #'values next model
                (persist-shared-config '("general" "auxiliary_model" "effort") (or effort :remove)))
              (values next model outcome detail))))))

;;; --- the decider ------------------------------------------------------------
;;; A decider is not a chat model: it answers a map of named questions about a
;;; state as probabilities (POST <base>/v1/systemone), the call a guard, a
;;; triage or a router asks. general.decider_model is the third pin, of the same
;;; {provider, model} shape, naming one of the deciders below; /models' Decider
;;; tab and `/models decider' write it. providers.<id>.base_url and .proxy move
;;; a decider's address the way they move a chat provider's; a decider id is
;;; never a lane, so nothing lists it as one (LISTABLE-PROVIDER-IDS).

(defparameter +deciders+
  '(("djev" :name "DJev" :model "dgemma" :base "http://127.0.0.1:8011"))
  "The deciders /models offers: (ID :NAME :MODEL :BASE), BASE the address
its own server answers on when providers.<ID>.base_url names none.")

(defun find-decider (id)
  "ID's +DECIDERS+ entry, or NIL."
  (and (stringp id) (assoc id +deciders+ :test #'string-equal)))

(defun decider-choices ()
  "Each decider as (:ID :NAME :MODEL :BASE :PROXY), its address the
configured entry's base_url over its own default, PROXY that entry's proxy."
  (loop for (id . facts) in +deciders+
        for entry = (configured-provider-entry id)
        collect (list :id id :name (getf facts :name) :model (getf facts :model)
                      :base (or (trimmed-config-string entry "base_url") (getf facts :base))
                      :proxy (trimmed-config-string entry "proxy"))))

(defun decider-model ()
  "The decider general.decider_model names, as (values ID MODEL BASE PROXY),
or NIL when none is picked or the pin names no decider."
  (multiple-value-bind (id model) (shared-model-pin "decider_model")
    (when-let (decider (find id (decider-choices) :key (lambda (row) (getf row :id))
                                                  :test #'equalp))
      (values (getf decider :id) model (getf decider :base) (getf decider :proxy)))))

(defun select-decider-model (id &key model)
  "Pin decider ID -- MODEL, else its own -- as general.decider_model; a NIL ID
takes the pin out. An ID no decider has is refused before anything moves.
=> (values ID MODEL OUTCOME DETAIL), PERSIST-SHARED-CONFIG's answer."
  (if (null id)
      (multiple-value-call #'values nil nil
        (persist-shared-config '("general" "decider_model") :remove))
      (let ((decider (or (find-decider id)
                         (error 'provider-config-error
                                :provider id :model model
                                :detail (format nil "~a is no decider; the deciders are ~{~a~^, ~}"
                                                id (mapcar #'first +deciders+))))))
        (multiple-value-call #'values (first decider) (or model (getf (rest decider) :model))
          (persist-shared-model-pin "decider_model" (first decider)
                                    (or model (getf (rest decider) :model)))))))

;;; --- the model-facing pair ---------------------------------------------
;;; Two forms the model reaches through eval, over the one reader and
;;; the one writer above. Refusals are conditions here, since a refused
;;; form is what the eval snippet renders as an error the model reads and
;;; corrects; the outcome vocabulary stays with the writer.

(defun config-path (path)
  "PATH as a list of key strings: a list as it is, a string as one key."
  (cond ((and (consp path) (every #'stringp path)) path)
        ((stringp path) (list path))
        ((null path) '())
        (t (error "a config path is a list of member keys, not ~s" path))))

(defun redact-secrets (value key &aux (above (if (listp key) (reverse key) (list key))))
  "VALUE with every string member that holds a secret (SECRET-PATH-P: its own
name, or for a list's element the list's) replaced by *REDACTED-SECRET*, and
what a URL carries as a credential read the same way (REDACT-URL-CREDENTIALS),
recursively; KEY is the name VALUE itself sits under, or the names from the
root that reach it."
  (map-json-leaves value
                   (lambda (leaf keys)
                     (cond ((not (and (stringp leaf) (plusp (length leaf)))) leaf)
                           ((secret-path-p (append keys above)) *redacted-secret*)
                           (t (redact-url-credentials leaf))))))

(defun config-get (&optional path)
  "The shared operator config (~/.nodecode/config.jsonc) at PATH -- a list
of member keys from the root, or a single key -- as the eval snippet prints
it: an object is a keyword plist, an array a vector, null :NULL; NIL when
nothing is there."
  ;; No path is the whole document. A member named like a secret (*api_key,
  ;; *token, *secret, *password) reads as "[set]": the config is read here for
  ;; what it says, and a token is never echoed into a transcript. What follows
  ;; from that: a section read here cannot be written back whole -- CONFIG-SET
  ;; refuses the sentinel rather than spend the keys -- so change one member
  ;; at a time, or :remove one. A config that does not parse signals with the
  ;; parser's own text.
  (multiple-value-bind (config error-text) (read-shared-config)
    (when error-text
      (error "the shared config does not read: ~a" error-text))
    (let ((steps (config-path path)))
      (nlk:json-plist (redact-secrets (apply #'json-value config :any steps) steps)))))

(defun config-set (path value)
  "Write VALUE into the shared operator config at PATH and answer the diff
it made, as one line: `channels.discord: set enabled, bot_token_env (2
added, 0 replaced)'."
  ;; VALUE is a string, a number, T, :false, :null, a keyword plist (an object
  ;; -- MERGED into what is there, member by member, never removing one) or a
  ;; list or vector (an array); an object value merges, anything else replaces
  ;; the member at PATH. :REMOVE takes the member at PATH out of the document
  ;; -- the only way one leaves, since an object value never removes. Comments
  ;; and every other byte of the document are kept, and the write is atomic. A
  ;; document the writer cannot understand, a value that is not JSON, a path
  ;; through a member that is not an object, or a value that would write a
  ;; secret's `[set]' back over the secret signals instead of guessing, and
  ;; nothing is written.
  (multiple-value-bind (outcome detail)
      (persist-shared-config (config-path path) value)
    (ecase outcome
      (:written detail)
      (:disabled (error "the shared config is not writable in this process (no config path)"))
      (:failed (error "config-set ~{~a~^.~}: ~a" (config-path path) detail)))))

(defun select-default-model (&key provider model endpoint &aux (next (or provider *provider*)))
  "Make (PROVIDER MODEL) this image's default selection and the shared config's,
in that order; ENDPOINT, when given, is the address it rides."
  ;; => (values PROVIDER MODEL OUTCOME DETAIL), the two effective ids
  ;; and PERSIST-SHARED-MODEL-PIN's answer.
  ;;
  ;; The one place a selection is made. Every surface that lets an operator
  ;; pick -- the owner shell's /models, the gateway model op, the per-session
  ;; model route -- comes through here, because each of them used to SETF
  ;; these globals itself and they had drifted into three different opinions
  ;; about what a pick means.
  ;;
  ;; Every pick resolves its provider's lane and endpoint first —
  ;; PROVIDER-ENDPOINT refuses with the exact reason (an sdk no lane
  ;; recognizes, an entry naming no sdk, no provider at all, no endpoint of
  ;; its own) — so a rejected pick leaves no half-moved state behind, and a
  ;; model picked under a provider that cannot serve it is refused by that
  ;; provider's name here rather than at the freeze. Turns already admitted
  ;; keep their frozen config; the next compile sees the change. An explicit
  ;; ENDPOINT -- the gateway op's -- or the override the pick keeps is the
  ;; address, so only the lane is asked then. It is process-lifetime only:
  ;; the durable per-lane endpoint is providers.<id>.base_url.
  (check-model-id next (or model *model*))
  (if (or endpoint (and *endpoint* (equal next *provider*)))
      (find-provider-lane next)
      (provider-endpoint next))
  (unless (equal next *provider*)
    (setf *provider* next
          ;; A lane switch invalidates an override that named the old
          ;; lane's endpoint. Staying on the lane leaves the override alone.
          *endpoint* nil))
  (when endpoint (setf *endpoint* endpoint))
  (when model (setf *model* model))
  (multiple-value-call #'values *provider* *model*
    (persist-shared-model-pin "default_model" *provider* *model*)))

(defun select-default-effort (effort)
  "Make EFFORT — a rung, or NIL for the model's own reasoning — the effort
every session opened from now on starts on: this image's default and the
shared config's general.default_model.effort, in that order."
  ;; The effort half of SELECT-DEFAULT-MODEL, for every surface that picks an
  ;; effort (SET-SESSION-EFFORT). The member is written only into a
  ;; default_model pin the config already carries: an object holding an effort
  ;; alone is a pin missing its provider, which the seed refuses. => (values
  ;; OUTCOME DETAIL), PERSIST-SHARED-CONFIG's answer, NIL with no pin to write
  ;; into.
  (setf *reasoning-effort* effort)
  (when (json-value (read-shared-config) :object "general" "default_model")
    (persist-shared-config (list "general" "default_model" "effort") (or effort :remove))))

(defun trimmed-config-string (object key)
  "OBJECT's KEY as a trimmed non-empty string, or NIL (the Zig
trimmedStringField rule)."
  (let ((trimmed (nlk:trimmed (or (json-value object :string key) ""))))
    (and (plusp (length trimmed)) trimmed)))

(defun configured-provider-entry (name)
  "The providers.<NAME> object, or NIL — exact key match, the Zig
providerObjectForId rule."
  (when (stringp name)
    (json-value (read-shared-config) :object "providers" name)))

(defun configured-model-entry (provider model)
  "The providers.<PROVIDER>.models[] row whose id is MODEL, or NIL."
  (find model (json-array (configured-provider-entry provider) "models")
        :test #'equal :key (lambda (row) (trimmed-config-string row "id"))))

(defun config-context-window (model-entry)
  "MODEL-ENTRY's context_window when it is a positive integer — the model's
window in the provider's own tokens — else NIL."
  ;; The one parse of the key, for the capability seam's budget and the
  ;; configured-model inventory alike.
  (let ((value (json-value model-entry :integer "context_window")))
    (and value (plusp value) value)))

(defparameter +sdk-names+ '("openai-completions" "openai-responses" "anthropic" "google")
  "The config sdk alphabet (pi's api ids, one spelling per wire), in the
order a refusal lists them.")

(defun catalog-npm-lane-name (npm)
  "The lane a models.dev npm package rides -- the one registered with it --
the catalog counterpart of the +SDK-NAMES+ alphabet."
  ;; NIL for every other package — the provider is then unusable without a
  ;; config entry naming an sdk.
  (when-let (lane (and npm (find-if (lambda (lane) (member npm (uiop:ensure-list lane.npm) :test #'equal))
                                    *provider-lanes*)))
    lane.name))

;;; --- lane resolution: the operator's declaration -----------------------------
;;; A provider id names an endpoint — where plus credential — and the wire
;;; dialect it speaks: providers.<id>.sdk. The lane a model rides is that
;;; declaration, or a per-model models[].sdk pin; nothing here asks the
;;; endpoint. Probing the endpoint to learn the lane would be unreliable: an
;;; endpoint may cut a stream or split one tool call into nameless parts.
;;; Which wire an endpoint translates faithfully is the operator's
;;; knowledge, declared once.

(defun find-lane-by-name (name &optional (errorp t))
  "The registered lane NAME names, else PROVIDER-ERROR -- NIL when ERRORP is false."
  (or (find name *provider-lanes* :key #'provider-lane-name :test #'equal)
      (and errorp
           (error 'provider-config-error
                  :detail (format nil "no provider lane named ~s is registered" name)))))

(defun configured-provider-lane (name &aux (entry (configured-provider-entry name))
                                           (sdk (and entry (trimmed-config-string entry "sdk"))))
  "The lane name the configured provider NAME rides, or NIL when none
resolves:

  sdk present       its lane; an sdk no lane recognizes yields NIL — the
                    provider is unusable and FIND-PROVIDER-LANE says why
  id names a lane   that lane (`\"anthropic\": {}`), the providers/root.zig
                    Provider.parse(provider_id) fallback
  no config entry   a lane registered under NAME itself, or the models.dev
                    catalog entry's npm package — the tier that makes an
                    auth.json-credentialed catalog provider usable without
                    a config entry

A config entry naming no sdk resolves nothing: the wire is the operator's
declaration, never learned from the endpoint."
  ;; A model that needs another
  ;; wire at the same endpoint is a second provider entry, or a models[].sdk pin
  ;; (RESOLVE-MODEL-LANE).
  (flet ((registered (lane-name) (and (find-lane-by-name lane-name nil) lane-name)))
    (cond
      ;; an sdk names its lane in +SDK-NAMES+; one it does not name leaves the provider unusable
      (sdk (registered (find sdk +sdk-names+ :test #'equal)))
      (entry (registered name))
      (t (or (registered name)
             (when-let (catalog (and (stringp name)
                                     (json-value (models-catalog-table) :object name)))
               (registered (catalog-npm-lane-name
                            (trimmed-config-string catalog "npm")))))))))

(defun resolve-model-lane (provider model)
  "The lane name (PROVIDER, MODEL) rides, or NIL when the provider resolves
none: providers.<id>.models[].sdk pins one for this model, else the
provider's own lane (CONFIGURED-PROVIDER-LANE)."
  ;; Both are the operator's declarations; nothing here asks the endpoint.
  ;; an operator's pin is the one source a verdict never outranks
  (or (find (trimmed-config-string (configured-model-entry provider model) "sdk") +sdk-names+
            :test #'equal)
      (configured-provider-lane provider)))

;;; --- the two model pins of the general section ------------------------------
;;; general.default_model and general.auxiliary_model are two objects of one
;;; shape, {"provider": id, "model": id, "effort": rung} — the
;;; session.model_selected payload with an optional effort — so the config,
;;; the durable pin and a side session's pin are one value spelled one way.
;;; Both members are required in both: a model under an unnamed lane is how
;;; the shared config would hold a model no lane had served, and
;;; the write rule PERSIST-SHARED-MODEL-PIN keeps is the read rule here.
;;; These are lisp-only keys, read through the refusing waist accessors: a
;;; malformed object names its key, where the Zig-parity lenient reader
;;; would coerce in silence. The flat general.default_provider +
;;; general.default_model strings of the Zig schema are retired, and refused
;;; by name rather than read: a boot that would quietly run the image's baked
;;; default while the file named another would be the same class of silence.

;;; Carried on the target (GET /api/gateway/target) as the shell's `refusal` (beside a
;;; lane's own), so the operator learns at boot, in the words that name the
;;; key, that the file's pin was not applied.
(defparameter *model-config-refusal* nil
  "Why the general section's model pins could not be read at the last seed
— the CONFIG-REFUSAL's text — or NIL when they read clean.")

(defun config-model-pin (general key)
  "general.KEY as (values PROVIDER MODEL EFFORT), or NIL when the key is
absent."
  ;; Refuses (CONFIG-REFUSAL) a value that is not an object, an object missing
  ;; provider or model, or a member that is not a string; EFFORT passes
  ;; through as written — a rung this vocabulary does not know is the
  ;; provider's to accept or refuse (CLAMP-EFFORT's rule).
  (multiple-value-bind (object present) (section-value general key)
    (when present
      (unless (hash-table-p object)
        (config-error "general.~a must be an object {\"provider\": ..., \"model\": ...}, got ~s"
                      key object))
      (let ((provider (config-string object "provider"))
            (model (config-string object "model"))
            (effort (config-string object "effort")))
        (unless (and provider model)
          (config-error "general.~a needs both provider and model~@[ (~a is missing)~]"
                        key (cond ((null provider) "provider") (t "model"))))
        (handler-case (check-model-id provider model)
          (provider-config-error (condition)
            (config-error "general.~a: ~a" key (provider-error-detail condition))))
        (values provider model effort)))))

(defun shared-model-pin (&optional (key "default_model"))
  "The shared config's general.KEY -- default_model or auxiliary_model --
as CONFIG-MODEL-PIN answers it, or NIL when there is none or it cannot be
read."
  ;; The config is what a shell reads: *AUXILIARY-MODEL* is seeded at a store
  ;; open, and an attached shell opens none.
  (ignore-errors (config-model-pin (json-value (read-shared-config) :object "general")
                                   key)))

(defun seed-model-config-from-shared-config (&aux (general (json-value (read-shared-config) :object "general"))
                                                  (changed nil))
  "general.default_model seeds the process selection when the operator
supplied no NODECODE_* override (the providers/root.zig selection
ladder: explicit override > general default), and general.auxiliary_model
seeds *AUXILIARY-MODEL*."
  ;; The default provider is seeded whether or not it resolves: the footer
  ;; then names it, and the first pick or freeze refuses by its name
  ;; (FIND-PROVIDER-LANE). Without seeding it, the image's baked default would
  ;; remain in place under the pin's model: a configured entry naming no
  ;; sdk would fail 401 against a lane nobody selected, and an id nothing
  ;; knows would be sent to OpenCode Zen as "public".
  ;; An effort on the default pin seeds the default effort under
  ;; NODECODE_REASONING_EFFORT, as its model does under NODECODE_MODEL; a
  ;; session's own /think pick outranks it. A section that refuses to read — the retired flat keys, a pin
  ;; missing its provider — seeds nothing, warns once and leaves its text in
  ;; *MODEL-CONFIG-REFUSAL* for the shells. => T when either default global
  ;; changed.
  (setf *model-config-refusal* nil)
  (when general
    (with-handlers ((config-refusal (condition)
                      (setf *model-config-refusal* (princ-to-string condition))
                      (warn "~a" *model-config-refusal*)))
      (when (or (stringp (gethash "default_provider" general))
                (stringp (gethash "default_model" general)))
        (config-error "general.default_provider and general.default_model as strings are retired; write general.default_model as {\"provider\": \"...\", \"model\": \"...\"}"))
      (multiple-value-bind (provider model effort)
          (config-model-pin general "default_model")
        (when provider
          (when (and (not (credential-env "NODECODE_PROVIDER"))
                     (not (equal provider *provider*)))
            (setf *provider* provider
                  changed t)
            ;; A seeded lane switch drops a stale endpoint
            ;; override the same way /models does — unless the
            ;; operator pinned one.
            (unless (credential-env "NODECODE_ENDPOINT")
              (setf *endpoint* nil)))
          (unless (or (credential-env "NODECODE_MODEL") (equal model *model*))
            (setf *model* model
                  changed t))
          (when (and effort
                     (not (credential-env "NODECODE_REASONING_EFFORT")))
            (setf *reasoning-effort* effort))))
      (multiple-value-bind (provider model effort)
          (config-model-pin general "auxiliary_model")
        (setf *auxiliary-model*
              (and provider
                   (list :provider provider :model model :effort effort))))))
  changed)

(defun default-model-configured-p ()
  "Whether this process's default target is one the operator configured: a
NODECODE_PROVIDER / NODECODE_MODEL override, or the general.default_model pin
the shared config carries — the pin every pick writes (SELECT-DEFAULT-MODEL)
and every store open seeds from."
  ;; Anything else is the model the image was built with, which nobody chose
  ;; and no provider was connected for: a surface that names it names a model
  ;; that cannot answer. A process with no shared config path (the hermetic
  ;; posture) has no file to be unconfigured against, so its globals are its
  ;; configuration.
  (or (null *shared-config-path*)
      (and (or (credential-env "NODECODE_PROVIDER")
               (credential-env "NODECODE_MODEL"))
           t)
      (and (shared-model-pin) t)))

;;; --- the auxiliary target ---------------------------------------------------
;;; Side work — a naming call, a recap, a summary: anything a session did not
;;; ask for in so many words — runs on the auxiliary target when one is
;;; configured, and on the session's own target at its weakest reasoning rung
;;; when none is. One resolver and one binder over the same three specials
;;; RUN binds, so the credential chain, the lane, the retry policy and the
;;; test stub all come for free; a side session pins the same triple through
;;; NLK:RECORD-SESSION-MODEL-SELECTION itself.

(defun auxiliary-model (&optional session-id &aux (pin *auxiliary-model*))
  "=> (values PROVIDER MODEL EFFORT CONFIGURED-P): the auxiliary target
side work runs on — *AUXILIARY-MODEL* when one is configured, its effort
the configured rung or else the weakest the model declares; otherwise
SESSION-ID's own target (its durable pin over the process default, the RUN
overlay) at the weakest rung its model declares. CONFIGURED-P says which."
  (bind (((session-provider session-model) (and (null pin) session-id (nlk:store-open-p)
                                                (nlk:session-model-selection session-id)))
         (provider (if pin (getf pin :provider) (or session-provider *provider*)))
         (model (if pin (getf pin :model) (or session-model *model*))))
    ;; a model declaring no rung leaves the key off the wire, never a forced "off"
    (values provider model
            (or (and pin (getf pin :effort))
                (first (model-capability-effort-levels
                        (resolve-model-capability model provider))))
            (and pin t))))

(defmacro with-auxiliary-model ((&key session-id) &body body)
  "Run BODY with *PROVIDER*, *MODEL* and *REASONING-EFFORT* bound to the
auxiliary target (AUXILIARY-MODEL), so the usual freeze and CALL-PROVIDER
run it — SNAPSHOT-EFFECTIVE-PROVIDER-CONFIG reads these three."
  ;; A configured target drops the process endpoint override, which names the
  ;; default lane's path; the fallback keeps it, being that lane.
  (alexandria:with-gensyms (provider model effort configured)
    `(multiple-value-bind (,provider ,model ,effort ,configured)
         (auxiliary-model ,session-id)
       (let ((*provider* ,provider)
             (*model* ,model)
             (*reasoning-effort* ,effort)
             (*endpoint* (if ,configured nil *endpoint*)))
         ,@body))))

(defun complete (system user &key session-id max-tokens (fallback-p t))
  "One side call: SYSTEM as the whole instruction, USER as the single
message, no tools and no history — the answer's own text."
  ;;
  ;; It runs on the auxiliary target (WITH-AUXILIARY-MODEL), so a title, a
  ;; classification, a recap and every other piece of side work share one
  ;; selection, one credential chain and one retry policy. SESSION-ID, when
  ;; given, is the session whose target stands in for the auxiliary one;
  ;; MAX-TOKENS is this call's completion ceiling, else the ambient
  ;; *MAX-COMPLETION-TOKENS*. A provider failure signals, the way the same call
  ;; inside a turn would: whether a nicety may pass is the caller's decision.
  (labels ((ask ()
             (with-auxiliary-model (:session-id session-id)
               (let ((*max-completion-tokens* (or max-tokens *max-completion-tokens*)))
                 (message-content
                  (call-provider (compile-turn-context
                                  (make-context-request
                                   :session-id session-id
                                   :provider-config (snapshot-effective-provider-config)
                                   :base-system-prompt (or system "")
                                   :messages (list (json-object "role" "user"
                                                                "content" user))))
                                 :on-part nil))))))
    (handler-case (ask)
      (error (condition)
        ;; A side call is not the work, and a target that is configured but
        ;; dead (an auth refusal, a model the endpoint does not serve) would
        ;; otherwise take every naming call, classification and recap down
        ;; with it. One retry on the target the session itself runs on, then
        ;; the failure stands: the caller decides what a missing answer means.
        (unless fallback-p (error condition))
        (format *error-output*
                "~&;; side call: auxiliary target refused (~a); using the session's own target~%"
                condition)
        (let ((*auxiliary-model* nil))
          (ask))))))

;;; --- credential chain -------------------------------------------------
;;; One resolution seam (the providers/root.zig localTurnCredential ladder):
;;; config tier -> auth-file api key -> auth-file OAuth token -> environment.
;;; Strict first-hit-wins; the hard failures (a failing !command, a corrupt
;;; or ambiguous auth file) never fall through to the next tier — that is a
;;; tested Zig behavior, not an accident. The config tier is *API-KEY*
;;; (process configuration) and then the configured provider's
;;; providers.<id>.api_key from the shared config. One lisp deviation stays
;;; deliberate: an empty chain resolves to "public" instead of Zig's
;;; ProviderNotConfigured, preserving the keyless default-endpoint boot.

(define-error credential-error (error) ((detail :initform ""))
  (:report "credential resolution failed: ~a" detail) (:signal credential-fail)
  ;; Aborts resolution with no fallback (the providers/root.zig hard-error
  ;; rule).
  (:documentation "A decisive credential failure: the !command indirection
failed, or the auth file is corrupt or ambiguous."))

(define-startup-parameter *auth-file-path*
  (home "auth.json")
  "The credential store shared with the Zig gateway (auth/root.zig).")

(defparameter +credential-command-timeout-seconds+ 10
  "Wall clock granted to a !command (config.zig command timeout).")

(defun credential-env (name &aux (value (nlk:trimmed (or (uiop:getenv name) ""))))
  "Trimmed environment variable NAME, or NIL when unset or blank (the
trimmedEnvValue rule: empty-after-trim counts as absent)."
  (and (plusp (length value)) value))

(defun read-auth-file (path)
  "The parsed auth.json object, or NIL when the file is absent."
  ;; A file that
  ;; exists but is oversized, unreadable, or not a JSON object is
  ;; CREDENTIAL-ERROR: a present credential store that cannot be trusted must
  ;; stop resolution, not silently yield the environment tier.
  (when-let (truename (probe-file path))
    (let ((text (handler-case (uiop:read-file-string truename)
                  (error (e) (credential-fail "auth file unreadable: ~a" e)))))
      (when (> (length text) (* 1024 1024)) ; providers/root.zig max_auth_file_bytes
        (credential-fail "auth file exceeds the 1MiB bound"))
      (let ((parsed (handler-case (nlk:decode-json text)
                      (error () (credential-fail "auth file is not valid JSON")))))
        (unless (hash-table-p parsed)
          (credential-fail "auth file is not a JSON object"))
        parsed))))

(defun auth-api-key (auth provider)
  "auth.json api_keys.<PROVIDER>.key — exact id match only, no aliases."
  (json-value auth :text "api_keys" provider "key"))

;;; --- what a credential is -------------------------------------------------
;;; A credential is a PLIST — (:key SECRET :source PROVENANCE :attributes
;;; PLIST) — and not a structure, because it is what a :CREDENTIAL hook
;;; answers: a cell builds one without a class this image would have had to
;;; install at boot (the file-edit path holds a defstruct; cells.lisp
;;; LOAD-SOURCE). SOURCE is provenance for the operator-facing auth state.
;;; ATTRIBUTES is the request side of the credential: a plist whose keys the
;;; request path reads where a lane leaves room — :endpoint, the address this
;;; credential is served at; :headers, an alist added to the request;
;;; :cache-key, the discriminator a wire request's cache routing key folds in.
;;; An attribute the kernel does not read is carried and ignored.

(defun make-credential (key source &optional attributes)
  "The credential that sends secret KEY, comes from SOURCE, and carries the
request ATTRIBUTES."
  (list :key key :source source :attributes attributes))

;;; Its readers, one per member: CREDENTIAL-KEY, CREDENTIAL-SOURCE and
;;; CREDENTIAL-ATTRIBUTES.
(macrolet ((reader (key documentation)
             `(defun ,(alexandria:symbolicate 'credential- key) (credential) ,documentation
                (getf credential ,key))))
  (reader :key "The secret the credential sends.")
  (reader :source "Where the credential came from: :config, :api-key, :env and :public for
this file's own rungs; whatever a cell names for its own.")
  (reader :attributes "The request attributes the credential carries, or NIL."))

;;; The frozen config carries the attributes BESIDE it, in a table keyed by the
;;; config rather than in a slot of it: a config this image already handed to a
;;; running turn keeps its shape across the change (DEFINE-SIDE-TABLE).
(nlk:define-side-table config-credential-attributes (:weakness :key)
  "The request attributes the credential frozen into a provider config
carried.")

(defun credential-attribute (config key)
  "The request attribute KEY CONFIG's frozen credential carries, or NIL."
  (getf (config-credential-attributes config) key))

(defun provider-env-key (provider)
  "PROVIDER's own <ID>_API_KEY from the environment, or NIL."
  (credential-env (format nil "~a_API_KEY" (substitute #\_ #\- (string-upcase provider)))))

(defun env-credential (provider family endpoint)
  "The environment ladder: derived name first; the SDK-family default second,
only when ENDPOINT is its vendor's own API; ANTHROPIC_AUTH_TOKEN third for
anthropic only (still sent as x-api-key)."
  ;; The SDK default is one vendor's key: OPENAI_API_KEY is OpenAI's, though
  ;; every compatible provider rides the chat-completions wire, the build's
  ;; keyless OpenCode Zen lane among them. So it goes only where its vendor's
  ;; own lane goes by default -- the same scheme, host and port -- and never
  ;; to whichever host a keyless provider of that family dials. A probe
  ;; composes no ENDPOINT, so it never counts the default; the vendors' own
  ;; providers name it as their derived variable anyway.
  (or (provider-env-key provider)
      (destructuring-bind (name vendor-lane)
          (ecase family
            ((:openai :openai-completions) '("OPENAI_API_KEY" "openai-responses"))
            (:anthropic '("ANTHROPIC_API_KEY" "anthropic"))
            (:google '("GOOGLE_API_KEY" "google")))
        (flet ((origin (url &aux (uri (and url (ignore-errors (quri:uri url)))))
                 (and uri (quri:uri-host uri)
                      (list (string-downcase (or (quri:uri-scheme uri) ""))
                            (string-downcase (quri:uri-host uri))
                            (quri:uri-port uri)))))
          (let ((lane (find-lane-by-name vendor-lane nil)))
            (and lane (origin endpoint)
                 (equal (origin endpoint) (origin (provider-lane-default-endpoint lane)))
                 (credential-env name)))))
      (and (string-equal provider "anthropic")
           (credential-env "ANTHROPIC_AUTH_TOKEN"))))

(defun resolve-provider-credential (provider &key (auth-path *auth-file-path*) endpoint probe)
  "The CREDENTIAL PROVIDER resolves to."
  ;;
  ;; Chain: config (*API-KEY*, ! indirection) -> auth api_keys -> the
  ;; :CREDENTIAL point -> env ladder -> "public". ENDPOINT is the address the
  ;; provider would dial; the point carries it, and a hook may answer a
  ;; credential of its own for another address (a backend a saved login is
  ;; served at). PROBE walks the same chain without its two side effects, for
  ;; a question of where the credential comes from: a !command never runs (the
  ;; config tier answers), and a corrupt auth file reads as none. It composes
  ;; no endpoint either, so the question never reads the catalog, and the
  ;; environment's SDK-family default, which rides only to its vendor's own
  ;; endpoint (ENV-CREDENTIAL), never answers it.
  (let* ((lane (find-provider-lane provider))
         (family lane.family)
         (endpoint (unless probe
                     (or endpoint *endpoint* (lane-endpoint provider lane.name))))
         (value (or (and (stringp *api-key*) (plusp (length *api-key*))
                         *api-key*)
                    (and provider
                         (trimmed-config-string
                          (configured-provider-entry provider) "api_key")))))
    (when (and value (or probe (char/= (char value 0) #\!)))
      (return-from resolve-provider-credential (make-credential value :config)))
    (when value
      ;; the !command: sh -lc under a bound at every resolution, never memoized
      (let ((trimmed (nlk:trimmed (subseq value 1))))
        (when (zerop (length trimmed))
          (credential-fail "empty credential command"))
        (bind (((output _ status)
                (handler-case
                    (run-bounded (shell-argv trimmed :login t)
                                 :seconds +credential-command-timeout-seconds+)
                  (error (e) (credential-fail "credential command failed to run: ~a" e)))))
          (unless (and (integerp status) (zerop status))
            (if (eq status :timeout)
                (credential-fail "credential command ran past ~a seconds"
                                 +credential-command-timeout-seconds+)
                (credential-fail "credential command exited with status ~a" status)))
          (let ((result (nlk:trimmed (or output ""))))
            (when (zerop (length result))
              (credential-fail "credential command produced no output"))
            (return-from resolve-provider-credential
              (make-credential result :config))))))
    ;; The store tier: AUTH is the parsed auth.json (NIL when there is none),
    ;; whose api_keys entry is read here; then the whole parsed object is
    ;; offered to the :CREDENTIAL hooks — a cell reads the store this process
    ;; already read, never the file again — and behind them the ladder's
    ;; remainder: the environment, then the keyless default.
    (let ((auth (handler-case (read-auth-file auth-path)
                  (credential-error (condition) (if probe nil (error condition))))))
      (or (when-let (key (and auth (auth-api-key auth provider)))
            (make-credential key :api-key))
          (call-hooked :credential
                       (list :provider provider :family family :auth auth
                             :auth-path auth-path :endpoint endpoint)
                       (lambda-ignoring (op &aux (env (env-credential provider family endpoint)))
                         (if env
                             (make-credential env :env)
                             (make-credential "public" :public))))))))

(defun snapshot-effective-provider-config (&optional (provider *provider*)
                                                    (model *model*))
  "Freeze (PROVIDER MODEL) — the AMBIENT selection when neither is named —
into an immutable EFFECTIVE-PROVIDER-CONFIG, the round-zero materialization
the compiled context carries for the whole turn."
  ;; This is the one reader of the configuration specials on the turn path;
  ;; per-provider retry tuning from the shared config outranks the process
  ;; defaults at this freeze (the LocalTurnConfig tuning fields), and the
  ;; capability freeze means request shaping and the eviction budget read
  ;; these numbers, never the live catalog -- on a box with none yet, the
  ;; rows its first fetch lands.
  (let* ((capability (progn
                       ;; On a box with no catalog yet, the one this process's first fetch lands is
                       ;; waited for while it is in flight; a fetch after a failed one holds
                       ;; nothing back. A box that cannot reach models.dev waits once, the fetch's
                       ;; own deadline at most.
                       (when-let (fetch (and (null (models-catalog-table))
                                             (car *models-catalog-fetch*)))
                         (when (and (null (cddr *models-catalog-fetch*)) (bt2:thread-alive-p fetch))
                           (ignore-errors (bt2:join-thread fetch))))
                       (check-model-id provider model)
                       (resolve-model-capability model provider)))
         (entry (configured-provider-entry provider))
         ;; providers.<id>'s providerRetryTuning fields; idle milliseconds become read seconds
         (idle (and entry (gethash "stream_idle_timeout_ms" entry)))
         (rerun-ceiling (and entry (gethash "stream_rerun_ceiling_seconds" entry)))
         (lane (or (resolve-model-lane provider model)
                   (provider-lane-name (find-provider-lane provider)))))
    (flet ((copied (value) (and (stringp value) (copy-seq value)))
           (retries (key default &aux (value (and entry (gethash key entry))))
             (if (and (integerp value) (>= value 0)) value default)))
      (let* ((endpoint (or *endpoint* (lane-endpoint provider lane)))
             (credential (resolve-provider-credential provider :endpoint endpoint))
             (config
               (make-effective-provider-config
                :provider (copy-seq provider)
                :lane (copy-seq lane)
                ;; The candidate's own endpoint: EFFECTIVE-ENDPOINT resolves the
                ;; lane through the AMBIENT selection, which a failover candidate must
                ;; not inherit. Without this, a failover candidate would re-dial the
                ;; primary's dead address.
                :endpoint (copy-seq endpoint)
                :model (copy-seq model)
                :api-key (copy-seq (credential-key credential))
                :proxy (copied (trimmed-config-string entry "proxy"))
                :request-timeout (if (and (integerp idle) (plusp idle)) (/ idle 1000) *request-timeout*)
                :max-stream-retries (retries "stream_max_retries" *max-stream-retries*)
                :max-request-retries (retries "request_max_retries" *max-request-retries*)
                :retry-base-delay *retry-base-delay*
                ;; a ceiling of 0 is configured too, and disables it: every stream failure re-runs
                :stream-rerun-ceiling (if (and (realp rerun-ceiling) (>= rerun-ceiling 0))
                                          (and (plusp rerun-ceiling) rerun-ceiling)
                                          *stream-rerun-ceiling*)
                :temperature *temperature*
                :top-p *top-p*
                :max-completion-tokens *max-completion-tokens*
                :tool-choice (copied *tool-choice*)
                :reasoning-effort (copied *reasoning-effort*)
                :reasoning-summary (let ((asked (trimmed-config-string entry "reasoning_summary")))
                                     (cond ((null asked) "auto")
                                           ((member asked '("off" "none" "false")
                                                    :test #'string-equal)
                                            nil)
                                           (t (copy-seq asked))))
                :context-tokens capability.context-tokens
                :context-tokens-operator-p capability.context-tokens-operator-p
                :output-limit capability.output-limit
                :effort-levels (copy-list capability.effort-levels))))
        ;; What the credential brings to the request rides BESIDE the config, in
        ;; a table keyed by it, never in a slot: a config this image already
        ;; handed to a running turn keeps its shape across the change.
        (setf (config-credential-attributes config)
              (credential-attributes credential))
        config))))

;;; --- model failover: the chain and its candidates ---------------------------
;;; One turn-scoped stage (issue #18): when the selection's retries are
;;; exhausted by a retryable failure and the operator configured a chain,
;;; the round re-runs on the next candidate — the session's selection is
;;; never touched, so the next turn starts from the primary again. The
;;; chain is configuration, never a heuristic: absent or empty is no
;;; failover, exactly the behavior before this seam existed.

(defun failover-chain ()
  "The operator's model-failover chain, in order: a list of
(:provider P :model M) plists from the shared config's top-level
`failover.chain' array — objects of the same shape as
general.default_model ({\"provider\": ..., \"model\": ...})."
  ;; NIL when the config sets none: an absent key and an empty array are both
  ;; the opt-out. An entry missing either member is skipped with a warning,
  ;; never read as a model nobody named.
  (when-let (failover (json-value (read-shared-config) :object "failover"))
    (let ((chain (gethash "chain" failover)))
      (when (and chain (not (vectorp chain)))
        (warn "config failover.chain must be an array; ignoring it"))
      (loop for entry across (if (vectorp chain) chain #())
            for provider = (trimmed-config-string entry "provider")
            for model = (trimmed-config-string entry "model")
            when (and provider model)
              collect (list :provider provider :model model)
            else
              do (warn "config failover.chain entry needs provider and ~
model; skipped: ~s" entry)))))

(defun failover-candidate-config (provider model &aux (*api-key* nil))
  "The immutable config a fallback candidate (PROVIDER MODEL) rides,
resolved through the same freeze as the selection — one credential chain,
one lane resolution, one capability resolution."
  ;; The candidate's own credential is resolved: a process-level *API-KEY*
  ;; names the selection's provider and is not lent to another provider's
  ;; entry.
  (snapshot-effective-provider-config provider model))

;;; The chain is walked off the selection only for a failure whose trigger the
;;; operator left on (failover.on, every trigger when it names none); a round
;;; already on a candidate walks on whatever failed it. A trigger is read off
;;; transport facts -- the failure's class and its HTTP status -- never its
;;; words.

(defparameter +failover-triggers+
  '(("rate_limit" . "rate limit") ("outage" . "outage") ("slow_start" . "slow start")
    ("broken_answer" . "broken answer") ("refusal" . "refusal"))
  "The failures that can move a round to the failover chain, (ID . LABEL) in
the order a picker lists them: a 429; a server or transport failure, a
timeout or a stall; an attempt that streamed no token by its deadline, or
was skipped while it waited (ATTEMPT-WATCH); a stream cut short, an empty
answer or a malformed tool call; the model's refusal.")

(defvar *failover* t
  "NIL holds a provider call to the model it was aimed at: a probe asks one
model, never the chain behind it.")

(defun failover-trigger (condition)
  "The +FAILOVER-TRIGGERS+ id CONDITION, a provider failure, falls under."
  (cond ((typep condition 'provider-refused) "refusal")
        ((typep condition 'provider-slow-start) "slow_start")
        ((eql 429 (provider-error-status condition)) "rate_limit")
        ((typep condition '(or provider-stream-incomplete provider-empty-response
                            provider-malformed-tool-call))
         "broken_answer")
        (t "outage")))

(defun failover-triggers ()
  "The trigger ids switched on, in +FAILOVER-TRIGGERS+ order: the shared
config's failover.on, or every one when it sets none."
  ;; An empty array is every trigger off: the chain stays configured and is
  ;; never walked. An id no trigger has is skipped with a warning.
  (let ((on (when-let (failover (json-value (read-shared-config) :object "failover"))
              (gethash "on" failover))))
    (if (vectorp on)
        (progn
          (loop for id across on
                unless (and (stringp id) (assoc id +failover-triggers+ :test #'string=))
                  do (warn "config failover.on names no trigger; skipped: ~s" id))
          (loop for (id) in +failover-triggers+
                when (find id on :test #'equal) collect id))
        (mapcar #'car +failover-triggers+))))

(defun failover-chain-for (failure walking)
  "The chain a round that failed with FAILURE -- a provider failure, or the
id of its trigger -- may walk: NIL under *FAILOVER* NIL, and, off the chain
(WALKING NIL), NIL when FAILURE's trigger is off."
  (and *failover*
       (or walking (member (if (stringp failure) failure (failover-trigger failure))
                           (failover-triggers) :test #'string=))
       (failover-chain)))

(defun set-failover-chain (entries)
  "Write ENTRIES, (PROVIDER MODEL) lists in order, as the shared config's
failover.chain: PERSIST-SHARED-CONFIG's (values OUTCOME DETAIL)."
  (persist-shared-config '("failover" "chain")
                         (map 'vector (lambda (entry)
                                        (json-object "provider" (first entry) "model" (second entry)))
                              entries)))

(defun set-failover-triggers (ids)
  "Write the trigger IDS switched on as the shared config's failover.on, in
+FAILOVER-TRIGGERS+ order: PERSIST-SHARED-CONFIG's (values OUTCOME DETAIL)."
  (persist-shared-config '("failover" "on")
                         (coerce (loop for (id) in +failover-triggers+
                                       when (member id ids :test #'string=) collect id)
                                 'vector)))

(defun failover-record (provider model)
  "What history says of PROVIDER MODEL as a backup, a plist: :ANSWERED-AT,
the ISO time of the newest round it answered in a retained turn, or NIL;
:TRIED, the rounds a chain moved to it; :RESCUED, how many of those it went
on to answer."
  (nlk::with-store-handle (handle)
    (destructuring-bind (&optional tried rescued)
        (first (nlk::sql-rows
                handle
                "select count(*),
                        coalesce(sum(exists (select 1 from events u
                                    where u.turn_id = f.turn_id and u.kind = ?
                                      and u.log_position > f.log_position
                                      and json_extract(u.payload, '$.provider') = ?
                                      and json_extract(u.payload, '$.model') = ?
                                      and coalesce(json_extract(u.payload, '$.voided'), 0) = 0)), 0)
                   from events f
                  where f.kind = ? and json_extract(f.payload, '$.to-provider') = ?
                    and json_extract(f.payload, '$.to-model') = ?"
                nlk::+kind-turn-usage+ provider model
                nlk::+kind-provider-fallback+ provider model))
      (list :answered-at (sqlite:execute-single
                          handle
                          "select occurred_at from events where log_position =
                             (select max(log_position) from model_history
                               where provider = ? and model = ?)"
                          provider model)
            :tried (or tried 0)
            :rescued (or rescued 0)))))

(defun probe-model (provider model)
  "(values OK REASON SECONDS): one short request to PROVIDER MODEL alone --
no retry, no chain, no turn's facts -- answered (OK T), or the failure's
PROVIDER-FAILURE-REASON; SECONDS, how long it took."
  (let ((start (get-internal-real-time)))
    (flet ((took ()
             (/ (round (* 10 (- (get-internal-real-time) start)) internal-time-units-per-second) 10.0)))
      (handler-case
          (let ((*failover* nil)
                (*current-durable-turn* nil)
                (*max-stream-retries* 0)
                (*max-request-retries* 0)
                (*max-completion-tokens* 1024))
            (call-provider (compile-turn-context
                            (make-context-request
                             :provider-config (failover-candidate-config provider model)
                             :base-system-prompt "Answer in one word."
                             :messages (list (json-object "role" "user" "content" "Say OK."))))
                           :on-part nil)
            (values t nil (took)))
        (provider-error (e) (values nil (provider-failure-reason e) (took)))
        (error (e) (values nil (princ-to-string e) (took)))))))

;;; --- provider auth state and credential save (the /models backing) --------
;;; The Zig connect dialog is client-side: it lists the models.dev providers
;;; the gateway catalog carries and saves an entered API key into the SHARED
;;; ~/.nodecode/auth.json (auth/root.zig writeApiKeyAuthFile: api_keys.<id>
;;; = {"provider": id, "key": key}). The catalog half is client.lisp's
;;; (CONNECT-PICKER-ROWS over MODELS-CATALOG-TABLE); the two credential
;;; halves live here because this file owns the credential chain and the
;;; auth-file shape.

(defun provider-auth-state (provider &key (auth-path *auth-file-path*))
  "The credential source PROVIDER would resolve from, WITHOUT running any
!command indirection or touching the network: :config, :api-key, :oauth,
:env, or :none."
  ;; The chain itself, probed (RESOLVE-PROVIDER-CREDENTIAL :PROBE): a
  ;; !command never runs, and a corrupt auth file reads as :none rather than
  ;; signalling (a listing must not crash the dialog). The :CREDENTIAL point
  ;; answers here exactly as it would on a round — it reads the parsed store,
  ;; so asking it costs no network either.
  (let ((source (credential-source
                 (resolve-provider-credential provider :auth-path auth-path :probe t))))
    (if (eq source :public) :none source)))

(defun provider-config-entries ()
  "The shared config's providers.<id> members as (TRIMMED-ID . ENTRY), a blank id dropped."
  (when-let (providers (json-value (read-shared-config) :object "providers"))
    (loop for id being the hash-keys of providers using (hash-value entry)
          for trimmed = (nlk:trimmed id)
          when (plusp (length trimmed)) collect (cons trimmed entry))))

(defun configured-provider-inventory (&key (auth-path *auth-file-path*))
  "(values PROVIDER-IDS MODELS): the credentialed provider set and the
configured model list (config.zig configuredProviderInventory)."
  ;; IDS = auth.json providers plus shared-config providers carrying config
  ;; credentials, case-insensitively deduplicated, sorted ascending. MODELS =
  ;; each credentialed config provider's models entries as plists
  ;; (:provider-id :provider-display :model-id :context-window); malformed
  ;; entries are skipped rather than refused — a dialog listing must not
  ;; crash.
  (let ((auth (handler-case (read-auth-file auth-path) (credential-error () nil)))
        (entries (provider-config-entries))
        (ids '()))
    ;; auth.json's ids (config.zig appendStoredAuthProviderIds + oauthProviderId): an
    ;; api_keys key is the id; an oauth_tokens key may carry a profile after a colon.
    (dolist (section '("api_keys" "oauth_tokens"))
      (when-let (table (json-value auth :object section))
        (loop for key being the hash-keys of table
              for trimmed = (nlk:trimmed key)
              for id = (subseq trimmed 0 (and (string= section "oauth_tokens")
                                              (position #\: trimmed)))
              when (plusp (length id)) do (pushnew id ids :test #'string-equal))))
    ;; config providers in two passes: the credentialed ids (config.zig
    ;; providerHasConfigCredentials), then the models of every id listed
    (loop for (id . entry) in entries
          when (or (trimmed-config-string entry "api_key")
                   (trimmed-config-string entry "session_token")
                   (and (trimmed-config-string entry "access_key_id")
                        (trimmed-config-string entry "secret_access_key")))
            do (pushnew id ids :test #'string-equal))
    (setf ids (sort ids #'string<))
    (values ids
            (loop for (id . entry) in entries
                  when (and (hash-table-p entry) (member id ids :test #'string-equal))
                    append (loop with display = (or (trimmed-config-string entry "display_name") id)
                                 for row across (json-array entry "models")
                                 for model-id = (trimmed-config-string row "id")
                                 when model-id
                                   collect (list :provider-id id :provider-display display
                                                 :model-id model-id
                                                 :context-window (config-context-window row)))))))

(defun connected-provider-ids (&aux (ids (loop for id in (configured-provider-inventory)
                                                when (configured-provider-lane id)
                                                  collect id))
                                    (catalog (models-catalog-table)))
  "The providers a turn can authenticate to: the credentialed ones a lane
drives (CONFIGURED-PROVIDER-INVENTORY, CONFIGURED-PROVIDER-LANE), and each
models.dev provider a lane drives whose own <ID>_API_KEY the environment
sets."
  ;; Sorted ascending, like the inventory. The environment rung's SDK-family
  ;; default connects nothing: it rides only to its vendor's own endpoint
  ;; (ENV-CREDENTIAL), and the vendors' providers name it as their own
  ;; <ID>_API_KEY; counted here, OPENAI_API_KEY would connect DeepSeek,
  ;; Moonshot and every other provider on the chat-completions wire. A saved key no lane
  ;; speaks connects nothing: auth.json keys for google-vertex and
  ;; google-vertex-anthropic, whose packages no lane serves, would make the
  ;; /models list ask where their credential comes from, which signals, and
  ;; /setup would open nothing at all.
  (when (hash-table-p catalog)
    (loop for id being the hash-keys of catalog using (hash-value provider)
          when (and (hash-table-p provider)
                    (catalog-npm-lane-name (json-value provider :text "npm"))
                    (provider-env-key id))
            do (pushnew id ids :test #'string-equal)))
  (sort ids #'string<))

(defun key-shaped-p (text)
  "Whether TEXT reads as a credential rather than a name: it holds a run of
twenty or more letters and digits with a digit in it."
  ;; No models.dev provider id holds one -- its longest run is sixteen
  ;; letters, `thinkingmachines'. Without this, the import would save a
  ;; provider named by its own key under itself, an id the lists would show.
  (and (stringp text)
       (some (lambda (run) (find-if #'digit-char-p run))
             (ppcre:all-matches-as-strings "[A-Za-z0-9]{20,}" text))
       t))

(defun save-provider-api-key (provider api-key &key (auth-path *auth-file-path*))
  "Persist API-KEY for PROVIDER into the shared auth.json, creating the file
and its directory (0700/0600) when absent; API-KEY NIL takes PROVIDER's key
out."
  ;; The entry shape is the Zig writeApiKeyAuthFile contract:
  ;; api_keys.<PROVIDER> = {"provider": id, "key": key}; every other field of
  ;; an existing file is preserved. The write is atomic (temp file + rename).
  ;; => the normalized provider id.
  (check-type provider provider-id)
  (check-type api-key (or null string))
  (let ((id (string-downcase (nlk:trimmed provider)))
        (key (and api-key (nlk:trimmed api-key))))
    (when (zerop (length id))
      (credential-fail "provider id must be non-empty"))
    (when (and key (zerop (length key)))
      (credential-fail "api key must be non-empty"))
    (update-auth-file
     (lambda (auth)
       (let ((api-keys (or (json-value auth :object "api_keys")
                           (setf (gethash "api_keys" auth)
                                 (make-hash-table :test #'equal)))))
         (if key
             (setf (gethash id api-keys) (json-object "provider" id "key" key))
             (remhash id api-keys))))
     :auth-path auth-path)
    id))

(defun update-auth-file (change &key (auth-path *auth-file-path*))
  "Call CHANGE on the shared auth.json object -- a fresh one when the file is absent -- and write
it back atomically (0700/0600), every field CHANGE leaves alone preserved."
  (let* ((path (merge-pathnames auth-path))
         (auth (or (read-auth-file path) (make-hash-table :test #'equal))))
    (funcall change auth)
    (nlk:write-file-atomically path (shasht:write-json auth nil)
                               :mode #o600 :directory-mode #o700)))

;;; --- provider model listings -----------------------------------------------
;;; A configured provider is {base_url, sdk, api_key}; which models it serves
;;; is the endpoint's own answer, not a hand-typed list. GET <base_url>/models
;;; is the one listing shape every lane publishes — openai and anthropic
;;; answer data[].id under their /v1 base, google models[].name under its
;;; version root — so the /models picker fills itself from the endpoint and a
;;; configured models list is an optional supplement (pinned aliases, or an
;;; endpoint that refuses to list). An auth.json-credentialed catalog provider
;;; has no config entry at all: its base is the catalog's api
;;; (PROVIDER-BASE-URL, the base its turns dial), and it lists from there the
;;; same way. The answers live in ONE cache file both
;;; shells and the gateway read; a fetch never runs on a UI thread and never
;;; signals — a failure is a recorded fact the picker can show.

;;; NIL disables the cache, the fetch, and the refresh thread entirely — the
;;; hermetic test posture, the same rule *MODELS-CATALOG-PATH* follows.
(define-startup-parameter *provider-models-cache-path*
  (namestring (nlk:cache-path "nodecode/provider-models.json"))
  "Where fetched provider model listings live: one object keyed by provider
id, each {fetched_at, base_url, error, models[{id, display, context_window}]}.")

(defparameter *provider-models-ttl-seconds* 600
  "How long a cached listing is trusted before the picker refetches it in
the background. /models refresh drops it outright.")

;;; The rows a failure kept (REFRESH-PROVIDER-MODELS) ride with it.
(defparameter *provider-models-failure-ttl-seconds* 60
  "How long a recorded listing failure is trusted before the picker re-asks:
a blip clears on the first open a minute later, while a good listing rides
the full TTL and a dead endpoint is still not re-dialled on every open.")

;;; Without the deadline, a server that accepts and stays silent keeps the
;;; GET waiting past Dexador's read timeout, which does not cover cl+ssl's
;;; handshake wait. The fetch runs off the UI thread, so this bounds how long
;;; the picker title says fetching, nothing else.
(defparameter *provider-models-fetch-timeout-seconds* 5
  "The deadline of one listing GET — dial, TLS handshake, headers and body
together, as an SB-SYS:WITH-DEADLINE.")

(defvar *provider-models-memo* nil
  "The READ-JSON-FILE-MEMO record of the last listing-cache read.")

(defvar *provider-models-write-lock* (bt2:make-lock :name "provider-models-write")
  "Serializes in-process read-modify-write of the cache file.")

;;; A second /models open while one runs is a no-op; the picker repaints from
;;; the one that lands.
(defvar *provider-models-refresh-thread* nil
  "The in-flight refresh, or NIL.")

(defun provider-models-cache ()
  "The parsed cache root, or NIL when the path is NIL, the file is absent,
or it does not parse. Memoized on (path, size, mtime) like the catalog."
  (values (read-json-file-memo '*provider-models-memo*
                               *provider-models-cache-path*)))

(defun listable-provider-ids (&aux (ids '()))
  "The providers a listing can be fetched for: config providers carrying a
base_url, whether or not they carry a key (a keyless local server lists
fine), plus every credentialed provider whose base the models.dev catalog
supplies (PROVIDER-BASE-URL) — a key /models saved against a catalog
provider lists from that provider's own endpoint, not from the catalog's
snapshot of it."
  ;; Sorted ascending, like the inventory. A decider's entry moves its
  ;; address only: it serves no chat listing.
  (loop for (id . entry) in (provider-config-entries)
        when (and (trimmed-config-string entry "base_url") (not (find-decider id)))
          do (pushnew id ids :test #'string-equal))
  ;; The inventory's ids are auth.json's plus the config entries carrying a
  ;; credential; the ones the catalog gives a base join.
  (dolist (id (configured-provider-inventory))
    (when (provider-base-url id)
      (pushnew id ids :test #'string-equal)))
  (sort ids #'string<))

;;; --- who is calling ---------------------------------------------------------
;;; Every request the kernel makes names its client, the way codex_cli_rs
;;; ("codex_cli_rs/0.28.0 (Ubuntu 24.04; x86_64) xterm-256color") and pi
;;; ("pi (linux 7.0.11; x64)") do: product/version (os release; arch) terminal.
;;; Without this, every provider would see dexador's own default and could
;;; not tell a nodecode session from any other Lisp program. The product is a
;;; token — no spaces — and the version carries the commit when the source
;;; tree can say it; nodecode.asd composes that version.

(defun compose-user-agent-version ()
  "The version this image reports: what the system definition declares — the
base plus the commit the build names, one rule living in nodecode.asd
(NODECODE-VERSION: the pack script's stamp outranks git, and a tree that can
name neither reports the bare base)."
  ;; The user-agent header, `nodecode
  ;; --version', and the line under the home logo all say this one thing.
  (or (ignore-errors (asdf:component-version (asdf:find-system "nodecode")))
      "0"))



;;; Read once, at load, so a dumped image carries the version it was built
;;; from.
(defvar *user-agent-version* (compose-user-agent-version)
  "The organism's version, the one it reports everywhere: the user-agent
header, `nodecode --version', and the line under the home screen's logo.")

(defun user-agent ()
  "The user-agent header value: product/version (os release; arch) terminal."
  ;; The terminal is read at call time, not at load, so a dumped image reports
  ;; the terminal it runs in rather than the one it was built in.
  (format nil "nodecode_cli/~a (~a ~a; ~a) ~a"
          *user-agent-version*
          (software-type) (software-version) (machine-type)
          (or (uiop:getenv "TERM") "unknown")))

(defun with-user-agent (headers)
  "HEADERS with the user-agent header in front, unless the caller set one."
  (if (assoc "user-agent" headers :test #'string-equal)
      headers
      (cons (cons "user-agent" (user-agent)) headers)))

(defun with-app-attribution (endpoint headers)
  "HEADERS plus the app-attribution headers ENDPOINT's host reads, each
unless the caller set it: OpenRouter credits a request's tokens to the app
its HTTP-Referer names on its public app rankings, under X-OpenRouter-Title
and X-OpenRouter-Categories (openrouter.ai/docs/app-attribution). Any other
host gets HEADERS unchanged."
  ;; Only the host that ranks by them hears them: another provider has no use
  ;; for the app's name. A credential's own headers (CREDENTIAL-ATTRIBUTE
  ;; :HEADERS) outrank these, so an operator can name a different app.
  (let ((host (string-downcase (or (ignore-errors (quri:uri-host (quri:uri endpoint))) ""))))
    (if (or (string= host "openrouter.ai") (uiop:string-suffix-p host ".openrouter.ai"))
        (append headers
                (remove-if (lambda (header) (assoc (car header) headers :test #'string-equal))
                           '(("HTTP-Referer" . "https://nodecode.ai")
                             ("X-OpenRouter-Title" . "NODECODE")
                             ("X-OpenRouter-Categories" . "cli-agent,personal-agent")))
                '())
        headers)))

(defun http-fetch (url &key content headers (timeout 30) proxy binary)
  "URL's answer to one exchange of at most TIMEOUT seconds -- a POST of
CONTENT when there is one, else a GET, the user-agent in front -- as (values
BODY STATUS), or (values NIL REASON TRANSIENT-P) when there is none: REASON
`HTTP <status>' for a refusal, and the code its body names when it names one
(ERROR-BODY-CODE), else the transport fact (TRANSPORT-FAILURE-LABEL),
TRANSIENT-P whether one more attempt can plausibly succeed."
  ;; TIMEOUT is one deadline over the whole exchange, the dial and the TLS
  ;; handshake included: dexador's own timeouts never reach a TLS read. A
  ;; one-shot exchange gains nothing from the stream path's pool and its
  ;; save-hook rules, so it takes none. DEADLINE-TIMEOUT is a serious
  ;; condition, not an ERROR: an ERROR clause alone would let it unwind the
  ;; caller.
  (handler-case
      (bind (((body status)
              (sb-sys:with-deadline (:seconds timeout)
                (nlk:http (if content :post :get) url :headers (with-user-agent headers)
                          :content content :proxy proxy :binary binary
                          :timeout timeout :connect-timeout timeout))))
        (if (< status 400)
            (values body status)
            (values nil (format nil "HTTP ~d~@[ ~a~]" status (error-body-code body)))))
    ((or error sb-sys:deadline-timeout) (condition)
     (values nil (transport-failure-label condition (or proxy dex:*default-proxy* url))
             (transient-transport-failure-p condition)))))

(defun error-body-code (body &aux (parsed (ignore-errors (nlk:decode-json (nlk:body-text body)))))
  "The machine-readable reason an error BODY names -- Google's
error.details[].reason (API_KEY_INVALID), else a string error.code or
error.type -- or NIL."
  (or (some (lambda (detail) (json-value detail :text "reason"))
            (json-array parsed "error" "details"))
      (json-first parsed :text '("error" "code") '("error" "type"))))

(defun refused-key-answer-p (status code)
  "Whether an HTTP answer of STATUS whose body names CODE (ERROR-BODY-CODE)
refuses the key it was sent: a 401, or the 400 Google answers a key it does
not know with (API_KEY_INVALID)."
  ;; A 403 is also what a key scoped away from listing gets: no verdict.
  (or (eql status 401)
      (and (eql status 400) (equal code "API_KEY_INVALID"))))

(defparameter +anthropic-version+ "2023-06-01"
  "The anthropic-version header value every request carries.")

(defun provider-auth-headers (family api-key)
  "The credential headers a request to a FAMILY lane carries, a streaming
round's and a listing GET's alike: anthropic sends x-api-key plus its version
header, google x-goog-api-key, the openai families a bearer token."
  ;; A round's request-only extras (anthropic's beta header) ride beside these.
  (case family
    (:anthropic `(("x-api-key" . ,api-key)
                  ("anthropic-version" . ,+anthropic-version+)))
    (:google `(("x-goog-api-key" . ,api-key)))
    (t `(("authorization" . ,(format nil "Bearer ~a" api-key))))))

(defun parse-provider-listing (text)
  "(values ROWS ERROR): the listing body TEXT as sorted, deduplicated row
plists (:id :display :context-window :owned-by :kind), or NIL and why."
  ;; data[] is the openai/anthropic shape, models[] the google one (ids wear a
  ;; models/ prefix there, stripped so the id is what a request names).
  (let* ((parsed (handler-case (nlk:decode-json text) (error () nil)))
         (entries (json-first parsed :array '("data") '("models")))
         (rows '()))
    (cond
      ((not (hash-table-p parsed)) (values nil "unparseable listing"))
      ((null entries) (values nil "listing carries neither data[] nor models[]"))
      (t
       (loop for entry across entries
             for raw = (json-first entry :text '("id") '("name"))
             for id = (and raw (nlk:trimmed (ppcre:regex-replace "(?s)^models/(?=.)" raw "")))
             ;; a window the listing happens to carry: vLLM's max_model_len,
             ;; OpenRouter's context_length, google's, llama.cpp's n_ctx_train
             for window = (json-first entry :integer '("max_model_len") '("context_length")
                                      '("inputTokenLimit") '("meta" "n_ctx_train"))
             when (and id (plusp (length id))
                       (not (find id rows :key (lambda (row) (getf row :id))
                                           :test #'string=)))
               do (push (list :id id
                              :display (json-first entry :text '("display_name") '("displayName"))
                              :context-window (and window (plusp window) window)
                              ;; The vendor the endpoint attributes the model
                              ;; to (openai-style listings) — kept on the row
                              ;; as a namespace hint, never read as a lane.
                              :owned-by (json-value entry :text "owned_by")
                              ;; What the model is, when the listing says:
                              ;; nodecode.ai's /v1/models answers chat or
                              ;; image, and only a chat model runs a turn.
                              :kind (json-value entry :text "kind"))
                        rows))
       (values (sort rows #'string-lessp :key (lambda (row) (getf row :id)))
               nil)))))

(defun listing-urls (base)
  "The URLs a listing GET tries, in order: <BASE>/models, and — when BASE
names no API version segment of its own — <BASE>/v1/models after it."
  ;; An operator's anthropic base is often the bare host (the lane path
  ;; supplies /messages), and a bare host answers /models with its web app,
  ;; not a listing.
  (let* ((base (string-right-trim "/" base))
         ;; its last path segment a v and a digit, then anything: v1, v1beta
         (versioned (ppcre:scan "(?i)(?:^|/)v\\d[^/]*$" base)))
    (cons (uiop:strcat base "/models") (unless versioned (list (uiop:strcat base "/v1/models"))))))

(defun list-provider-models (provider &key key (base (provider-base-url provider)))
  "(values ROWS ERROR): GET the configured PROVIDER's model listing from
BASE, with KEY in place of the credential PROVIDER resolves when given."
  ;; Never signals — every failure is a reason (HTTP 401, timeout, the config
  ;; refusal naming the sdk to set) the cache records and the picker shows.
  ;; Requires a base (PROVIDER-BASE-URL): a provider naming none, in config or
  ;; in the catalog, has no endpoint to ask.
  (unless base
    (return-from list-provider-models (values nil "no base_url configured")))
  (bind ((lane (handler-case (find-provider-lane provider)
                 (provider-error (condition)
                   ;; A listing nobody can fetch says why, verbatim: the
                   ;; refusal names the key to set (providers.<id>.sdk),
                   ;; and the picker's placeholder shows it.
                   (return-from list-provider-models
                     (values nil (provider-error-detail condition))))))
         ;; the key alone: the credential is a plist. Without this, a listing
         ;; that sends it whole as its bearer would get a 401 from every
         ;; provider that checks one.
         (api-key (or key (ignore-errors (credential-key (resolve-provider-credential provider)))))
         (proxy (trimmed-config-string (configured-provider-entry provider) "proxy"))
         (timeout *provider-models-fetch-timeout-seconds*)
         (headers (provider-auth-headers lane.family
                                         (or api-key "public"))) (last-error nil))
    ;; The deadline is the fetch timeout over the whole exchange, the TLS
    ;; handshake included -- see the parameter's note.
    (flet ((fetch (url &aux (args (list url :headers headers :timeout timeout :proxy proxy)))
             ;; A transient failure earns one immediate second attempt
             ;; before it is recorded: a reset on the one connection a
             ;; listing opens would be a `no listing`.
             (multiple-value-bind (body status transient) (apply #'http-fetch args)
               (if (and (null body) transient) (apply #'http-fetch args) (values body status)))))
      (dolist (url (listing-urls base) (values nil last-error))
        (multiple-value-bind (body status) (fetch url)
          (cond
            ((stringp status) (setf last-error (subseq status 0 (min 80 (length status)))))
            ((not (stringp body)) (setf last-error "empty listing"))
            (t (multiple-value-bind (rows error) (parse-provider-listing body)
                 (if error
                     (setf last-error error)
                     (return (values rows nil)))))))))))

(defun provider-key-check (provider key)
  "What PROVIDER's model listing, asked with KEY, says of it, as (values
VERDICT WORDS), or NIL when nothing was asked. VERDICT is :WORKS, the listing
answered; :REFUSED, it refused the key (REFUSED-KEY-ANSWER-P); :UNREACHABLE,
nothing listens where it would ask, a verdict on the address; or :UNCHECKED,
no verdict either way -- no answer within *PROVIDER-MODELS-FETCH-TIMEOUT-
SECONDS*, a 5xx, a 404, no base to ask. WORDS say what happened: a sentence
naming the provider for the two refusals, which stand in for a save, and only
the fact for :UNCHECKED (`no answer from 127.0.0.1:8080 in 5 s')."
  ;; /models asks before it saves: a mistyped key would otherwise read
  ;; `Credential setup is complete' and fail on the first turn instead. The
  ;; hermetic posture, which has no listing cache, dials nothing.
  ;;
  ;; The base is the one a turn dials: the catalog gives Anthropic, OpenAI and
  ;; Google no api base, and their turns ride the lane's default endpoint. A
  ;; refused connection is a verdict on the address: without this check, a key
  ;; saved for a port nothing listens on would read `saved' and connected, and
  ;; the first turn would fail. A check that heard nothing is no verdict:
  ;; without that rule it would say `connected' over a key nobody answered,
  ;; and the caller would save it and say it was not checked. The deadline is
  ;; over the whole check, a bare host's second URL included, so the seconds
  ;; the checking screen states are the seconds it waits.
  (when *provider-models-cache-path*
    (let* ((lane (ignore-errors (find-provider-lane provider)))
           (refusal nil)
           ;; a provider with no address of its own has none to borrow
           (default (and lane (handler-case (lane-default-endpoint provider lane)
                                (provider-config-error (condition)
                                  (setf refusal (provider-error-detail condition))
                                  nil))))
           (base (or (provider-base-url provider)
                     (and default (uiop:string-suffix-p default lane.path)
                          (subseq default 0 (- (length default) (length lane.path))))))
           (where (or (url-address base) base))
           (seconds *provider-models-fetch-timeout-seconds*)
           (reason (handler-case (sb-sys:with-deadline (:seconds seconds)
                                   (nth-value 1 (list-provider-models provider :key key :base base)))
                     (sb-sys:deadline-timeout () "timeout")))
           ;; (STATUS CODE) of an `HTTP 401 invalid_api_key' reason
           (answer (and reason (ppcre:register-groups-bind ((#'parse-integer status) code)
                                   ("^HTTP (\\d+)(?: (\\S+))?" reason)
                                 (list status code))))
           (name (or (json-value (models-catalog-table) :text provider "name") provider)))
      (cond ((null reason) (values :works nil))
            ((and answer (apply #'refused-key-answer-p answer))
             (values :refused (format nil "~a refused this key (~a)" name reason)))
            ((uiop:string-prefix-p "nothing listens at" reason)
             (values :unreachable (format nil "~a could not check the key: ~a" name reason)))
            ((equal reason "timeout")
             (values :unchecked (format nil "no answer from ~a in ~d s" where seconds)))
            (answer (values :unchecked (format nil "~a answered HTTP ~d" where (first answer))))
            ;; the rest name the address too: a bare `host not found' read as the key's
            ((null base) (values :unchecked (or refusal "no address to check it at")))
            ((find reason +transport-failures+ :key #'second :test #'equal)
             (values :unchecked (format nil "~a at ~a" reason where)))
            ;; PARSE-PROVIDER-LISTING's reasons, and an empty body
            ((search "listing" reason)
             (values :unchecked (format nil "~a answered with no model list" where)))
            ;; the config's refusal, which names the key to set
            (t (values :unchecked reason))))))

(defun listing-rows-from-json (models)
  "The row plists (:id :display :context-window :owned-by :kind) a cache
entry's models[] array carries; a row without an id is dropped."
  (loop for row across (or models #())
        for id = (json-value row :text "id")
        when id
          collect (list :id id :display (json-value row :text "display")
                        :context-window (json-value row :integer "context_window")
                        :owned-by (json-value row :text "owned_by")
                        :kind (json-value row :text "kind"))))

(defun provider-models-cache-entry (provider)
  "The cached listing for PROVIDER as a plist (:models :error :fetched-at
:fresh-p), or NIL when nothing is cached or the entry was fetched from a
different base_url than the one configured now."
  ;; MODELS are row plists
  ;; (:id :display :context-window); an entry can carry both rows and an error,
  ;; the rows a failed refresh kept. FRESH-P is NIL once the TTL has lapsed —
  ;; the listing TTL, or the shorter failure TTL when an error is recorded — and
  ;; the rows still show, the picker just refetches behind them.
  (let ((entry (json-value (provider-models-cache) :object provider)))
    (when (and entry
               (equal (json-value entry :text "base_url") (provider-base-url provider)))
      (let ((fetched-at (json-value entry :integer "fetched_at"))
            (error (json-value entry :text "error")))
        (list :models (listing-rows-from-json (json-value entry :array "models"))
              :error error
              :fetched-at fetched-at
              :fresh-p (and fetched-at
                            (< (- (get-universal-time) fetched-at)
                               (if error
                                   *provider-models-failure-ttl-seconds*
                                   *provider-models-ttl-seconds*))))))))

(defun stale-listable-provider-ids ()
  "The listable providers whose cache entry is absent or past the TTL — what
a /models open refreshes."
  (remove-if (lambda (id &aux (entry (provider-models-cache-entry id)))
               (and entry (getf entry :fresh-p)))
             (listable-provider-ids)))

(defun write-provider-models-cache (root)
  "Persist ROOT as the cache file, atomically (temp file + rename), and drop
this process's read memo of it: the file moved, so the next read parses
it anew."
  ;; The protocol every memoized JSON file follows (READ-JSON-FILE-MEMO): the
  ;; caller that changed a file sets its memo to NIL. Without this, the
  ;; /models picker's row memo rebuilds only when a listing file is read
  ;; anew; a caller that reads the file before writing it would leave a
  ;; record that describes the old file, and an operator's picker held open
  ;; while the listing lands would keep its old rows.
  (nlk:write-file-atomically (merge-pathnames *provider-models-cache-path*)
                             (shasht:write-json root nil))
  (setf *provider-models-memo* nil))

(defun refresh-provider-models (ids)
  "Fetch every provider in IDS — one worker thread each, joined — and merge
the answers into the cache file in one write."
  ;; A failure is recorded beside its stamp, and beside the rows the last
  ;; answer from the same base_url carried: the picker says why, keeps showing
  ;; the listing it had rather than emptying on a blip, and re-asks once the
  ;; failure TTL lapses. => the ids written. A NIL cache path fetches nothing.
  (when (and ids *provider-models-cache-path*)
    (let* ((ids (remove-duplicates ids :test #'string-equal))
           (workers (mapcar (lambda (id)
                              (spawn (format nil "nodecode-models-~a" id)
                                (cons id (multiple-value-list (list-provider-models id)))))
                            ids))
           (answers (mapcar (lambda (worker) (ignore-errors (bt2:join-thread worker)))
                            workers)))
      (bt2:with-lock-held (*provider-models-write-lock*)
        (let ((root (or (provider-models-cache)
                        (make-hash-table :test #'equal))))
          (loop for (id rows error) in (remove nil answers)
                do (let* ((base (provider-base-url id))
                          (prior (and error (json-value root :object id)))
                          (kept (and prior
                                     (equal (json-value prior :text "base_url") base)
                                     (listing-rows-from-json
                                      (json-value prior :array "models")))))
                     (setf (gethash id root)
                           (json-object "fetched_at" (get-universal-time)
                                        "base_url" base
                                        "error" (or error :null)
                                        "models"
                                        (map 'vector
                                             (lambda (row)
                                               (json-object
                                                (:getf row "id")
                                                (:getf row :opt "display" "context_window"
                                                       "owned_by" "kind")))
                                             (or rows kept))))))
          (write-provider-models-cache root)))
      (mapcar #'first (remove nil answers)))))

(defun drop-provider-models-cache (ids)
  "Forget the cached listings of IDS so the next open refetches them."
  (when (and ids *provider-models-cache-path*)
    (bt2:with-lock-held (*provider-models-write-lock*)
      (when-let (root (provider-models-cache))
        (dolist (id ids) (remhash id root))
        (write-provider-models-cache root)))))

;;; --- failure body rendering ----------------------------------------------

(defun octet-stream-p (stream &aux (type (ignore-errors (stream-element-type stream))))
  "True when STREAM yields octets, not characters — dexador's binary
KEEP-ALIVE-STREAM against its character DECODING-STREAM."
  (and type (not (eq type t)) (subtypep type 'integer)))

;;; --- shared stream mechanics ----------------------------------------------

(defun parse-sse-line (line)
  "Classify one SSE text line."
  ;; Returns (values kind value).
  ;;
  ;;   :data    — a data: line; value is the data string (never the sentinel)
  ;;   :done    — the data: [DONE] sentinel; value is NIL
  ;;   :comment — a :comment, event:, or unrecognised-field line; value is the raw string
  ;;   :empty   — a blank line (SSE field separator); value is NIL
  (cond
    ((string= line "") (values :empty nil))
    ((string= line "data: [DONE]") (values :done nil))
    ((uiop:string-prefix-p "data: " line) (values :data (subseq line 6)))
    ((uiop:string-prefix-p ":" line) (values :comment (subseq line 1)))
    (t (values :comment line))))

;;; --- provider phase timing (debug-only retention) -------------------------
;;; One append-only line per observation, keyed by an epoch-ms timestamp, so
;;; per-round transport cost (dial + TLS + request write + response headers)
;;; and tool execution can be read off a single file. Off unless the
;;; NODECODE_PHASE_LOG environment variable names a path at boot.


;;; --- provider connection reuse ---------------------------------------------
;;; Every provider lane runs with dexador's connection pool ON: each tool-call
;;; round is a fresh POST, and without reuse each round pays a full TCP + TLS
;;; handshake to the provider (measured at hundreds of ms against live relay
;;; endpoints — the dominant fixed cost of a tool roundtrip).
;;;
;;; Two hazards make pooling conditional, and both are owned HERE:
;;;
;;; 1. Heredity (the reason pooling was historically off, see turn.lisp):
;;;    a pooled keep-alive connection holds OpenSSL pointers in the Lisp
;;;    heap, and SAVE-LISP-AND-DIE with a populated pool dumps a core that
;;;    segfaults inside SSL_write after restore. The guard is by
;;;    construction, not call-site discipline: FLUSH-PROVIDER-CONNECTION-POOL
;;;    is registered on SB-EXT:*SAVE-HOOKS*, which SAVE-LISP-AND-DIE itself
;;;    runs before dumping, so no save path can capture a live socket.
;;;    RESET-PROVIDER-CONNECTION-POOL on SB-EXT:*INIT-HOOKS* rebuilds a
;;;    fresh empty pool on every restore as the belt to that suspender.
;;;
;;; 2. Dirty sockets: a stream closed before EOF (cancel, timeout, in-stream
;;;    error) still carries unread response bytes; dexador's close-action
;;;    would drain whatever is buffered and push the socket back anyway,
;;;    poisoning the pool. CLOSE-PROVIDER-STREAM-BODY hard-closes the raw
;;;    socket first on every non-EOF exit so the wrapper's close-action sees
;;;    a dead stream and cannot pool it. A stream read to EOF has already
;;;    returned its socket to the pool by the time it is closed here.
;;;
;;; A stale pooled socket (provider idled it out) degrades transparently:
;;; dexador retries the request once on a fresh connection when a reused
;;; socket fails at write or response-read time.

(defun flush-provider-connection-pool ()
  "Empty dexador's connection pool, closing every cached socket."
  (ignore-errors (dexador.connection-cache:clear-connection-pool))
  (values))

(defun reset-provider-connection-pool ()
  "Install a fresh empty pool object; restore-time state must own no FFI."
  (ignore-errors
    (setf dexador.connection-cache:*connection-pool*
          (dexador.connection-cache:make-connection-pool)))
  (values))

(pushnew 'flush-provider-connection-pool sb-ext:*save-hooks*)
(pushnew 'reset-provider-connection-pool sb-ext:*init-hooks*)

(defun provider-connection-key (endpoint &aux (start (search "://" endpoint)))
  "dexador's pool key for ENDPOINT: scheme://authority, the URL up to its
path (dexador request: (format nil \"~A://~A\" scheme authority))."
  (subseq endpoint 0 (and start (position #\/ endpoint :start (+ start 3)))))

;;; --- a body sent as a delta ------------------------------------------------
;;; A conversation's next request repeats the one before it but for what the
;;; round added. Without this, over a slow, distant link that repeat would be
;;; most of the first token's wait. A relay beside the server
;;; (src/tools/body-relay.py) keeps the bodies it forwarded by SHA-256 and
;;; says so on every response (X-Body-Delta); to an endpoint that said so, a
;;; body goes as a delta (NLK:ENCODE-DELTA, the request_bodies encoding)
;;; against the one its session sent before. Every body still arrives whole
;;; at the server, checked against its hash, so the prefix cache sees the
;;; same bytes. Live-only: after a restart the first body goes against
;;; nothing, which the relay rebuilds from the delta alone.

(defvar *delta-endpoints* (make-hash-table :test 'equal :synchronized t)
  "Connection keys (PROVIDER-CONNECTION-KEY) whose responses said they take a
body as a delta. Live-only.")

(defvar *sent-bodies* (make-hash-table :test 'equal :synchronized t)
  "Session id -> (SHA256-HEX OCTETS . STAMP): the body that session sent last
to an endpoint taking deltas, the base of its next. Live-only.")

(defparameter *sent-bodies-limit* 32
  "How many sessions' last bodies stay: past it the longest unused goes, and
that session's next body goes against nothing.")

(defun request-octets (request-json)
  "REQUEST-JSON as the octets that go on the wire."
  (if (stringp request-json)
      (sb-ext:string-to-octets request-json :external-format :utf-8)
      request-json))

(defun body-wire (endpoint request-json session &key whole)
  "How REQUEST-JSON goes to ENDPOINT for SESSION => (values CONTENT HEADERS):
the JSON as it is, or, to an endpoint that takes deltas, the delta against
SESSION's last body there -- against nothing when WHOLE or there is none --
with the headers naming its base and the hash of the body it rebuilds."
  (if (gethash (provider-connection-key endpoint) *delta-endpoints*)
      (let* ((octets (request-octets request-json))
             (base (and (not whole) session (gethash session *sent-bodies*))))
        (values (nlk:encode-delta (or (second base) (make-array 0 :element-type '(unsigned-byte 8)))
                                  octets)
                (list (cons "content-type" "application/octet-stream")
                      (cons "x-body-delta" (if base (car base) "empty"))
                      (cons "x-body-sha256" (subseq (nlk:sha256-text octets) 7)))))
      (values request-json (list (cons "content-type" "application/json")))))

(defun note-body-wire (endpoint request-json session response-headers)
  "After ENDPOINT answered: when RESPONSE-HEADERS say it takes deltas, mark
it so, and keep REQUEST-JSON as SESSION's base there -- the relay now holds
the body under its hash whichever way it went."
  (when (json-value response-headers :string "x-body-delta")
    (setf (gethash (provider-connection-key endpoint) *delta-endpoints*) t)
    (when session
      (let ((octets (request-octets request-json)))
        (sb-ext:with-locked-hash-table (*sent-bodies*)
          (setf (gethash session *sent-bodies*)
                (list* (subseq (nlk:sha256-text octets) 7) octets (get-internal-real-time)))
          (when (> (hash-table-count *sent-bodies*) *sent-bodies-limit*)
            (remhash (alexandria:extremum (alexandria:hash-table-keys *sent-bodies*) #'<
                                          :key (lambda (key) (cddr (gethash key *sent-bodies*))))
                     *sent-bodies*)))))))

(defun pooled-connection-count (endpoint &aux (pool dexador.connection-cache:*connection-pool*))
  "How many idle sockets the pool holds for ENDPOINT's host — 0 means the
next request dials and pays the TCP + TLS handshake (~130 ms to
api.deepseek.com, whose ELB keeps an idle socket at least 180 s)."
  (if (null pool)
      0
      (bt2:with-lock-held ((dexador.connection-cache::lru-pool-lock pool))
        (length (gethash (provider-connection-key endpoint)
                         (dexador.connection-cache::lru-pool-hash-table pool))))))

(defun provider-stream-socket (body)
  "The raw socket under dexador's streaming BODY, or NIL once the keep-alive
wrapper has let it go (after a clean EOF it is the pool's) or when BODY is
no dexador stream at all (a canned stream)."
  (and (typep body 'dexador.keep-alive-stream:keep-alive-stream)
       (dexador.keep-alive-stream:keep-alive-stream-stream body)))

(defun close-provider-stream-body (body &aux (raw (provider-stream-socket body)))
  "Close a streaming response BODY without ever pooling a dirty socket."
  ;; After a clean EOF the keep-alive wrapper has already returned the socket
  ;; to the pool and its slot is NIL, so the hard-close is a no-op and the
  ;; outer close just releases the wrapper. On every earlier exit the raw
  ;; socket is still mid-response: close it with :ABORT first so the wrapper's
  ;; close-action finds it dead and leaves the pool untouched.
  (when (and raw (open-stream-p raw))
    (ignore-errors (close raw :abort t)))
  (ignore-errors (close body))
  (values))

;;; --- the idle deadline: a clock armed only while a read is outstanding ----
;;; *REQUEST-TIMEOUT* promised an idle watchdog and dexador's :READ-TIMEOUT
;;; was taken to be it. It is not, on the sockets that matter: usocket makes
;;; :READ-TIMEOUT the fd-stream's own timeout (SB-IMPL::FD-STREAM-TIMEOUT,
;;; honoured by the fd-stream's refill and nothing else), and cl+ssl unwraps
;;; that fd-stream to its raw descriptor, sets O_NONBLOCK and hands OpenSSL a
;;; socket BIO (INSTALL-HANDLE-AND-BIO), so a TLS read never consults it:
;;; SSL_read answers WANT_READ and ENSURE-SSL-FUNCALL parks in
;;; SB-SYS:WAIT-UNTIL-FD-USABLE with no timeout. Every provider endpoint is
;;; https, so without the deadline below a TLS read would park with no timeout.
;;;
;;; What that park does consult is SBCL's own deadline: WAIT-UNTIL-FD-USABLE
;;; honours SB-SYS:WITH-DEADLINE, and cl+ssl's SBCL INPUT-WAIT and OUTPUT-WAIT
;;; are exactly that call ("*deadline* is handled by wait-until-fd-usable
;;; automatically", ssl-funcall.lisp). So the attempt binds a deadline around
;;; each park and nowhere else — the POST up to its headers, and every octet of
;;; the body, so the clock measures silence between octets, never the length
;;; of a line. A deadline that passes signals SB-SYS:DEADLINE-TIMEOUT, a
;;; SERIOUS-CONDITION and not an ERROR, so dexador's WITH-RETRYING (it retries
;;; ERRORs) never redials on it, and the catch site classifies by phase:
;;; before headers a request-scope PROVIDER-ERROR, after them a
;;; PROVIDER-STREAM-INCOMPLETE, so the budgets the retry policy already keeps
;;; apply unchanged. The socket unwinds closed on both paths (dexador's own
;;; cleanup before headers, CLOSE-PROVIDER-STREAM-BODY with :ABORT after) and
;;; is never pooled. Live-only: the cut is a failure detail and a phase-log
;;; line, never a fact.

(defun idle-seconds-label (seconds)
  "The timeout as a failure detail prints it: whole seconds plain, else one decimal."
  (if (integerp seconds) seconds (format nil "~,1f" seconds)))

;;; --- the first token's wait -----------------------------------------------
;;; The idle clock cuts silence, and a provider that has taken the request
;;; and is still reading it -- a cold prompt on a busy host -- stays silent
;;; inside it for minutes, or keeps the socket warm with comments, while
;;; every attempt of the ladder waits out the same cold start (2026-10-22,
;;; a test box: 100 s to the first token on mimo-v2.6-pro, the room's card saying
;;; only that it waited). With a failover candidate standing behind it, an
;;; attempt is WATCHED until its first token: past failover.first_token_seconds
;;; (*FIRST-TOKEN-SECONDS* unless set, 0 never) its wait is a slow start
;;; (PROVIDER-SLOW-START), which leaves for the candidate at once instead of
;;; retrying the cold start; SKIP-PROVIDER-ATTEMPT cuts the same wait when
;;; asked, a room's press among the askers. With no candidate nothing is
;;; watched, and an attempt waits as long as its idle clock lets it. A
;;; watched read looks at its watch once a second; an unwatched one runs as
;;; it always did.

(defparameter *first-token-seconds* 60
  "Seconds a watched attempt waits for its first token before it leaves for
its failover candidate, unless failover.first_token_seconds says otherwise.")

(defstruct (attempt-watch (:copier nil))
  "One provider attempt's wait for its first token, with a failover candidate
behind it."
  ;; DEADLINE is the internal real time the wait is cut at, NIL for a wait
  ;; only a skip cuts; NEXT is the candidate (:PROVIDER P :MODEL M); SKIP the
  ;; words a skip gave, once one asked; TOKEN-P set by the first token.
  (deadline nil)
  (seconds 0)
  (next nil)
  (skip nil)
  (token-p nil))

(defvar *attempt-watch* nil
  "The watch of the attempt this thread is walking, or NIL.")

(defvar *attempt-watches* (make-hash-table :test 'equal :synchronized t)
  "Turn id -> the ATTEMPT-WATCH of that turn's attempt in flight.")

(defun first-token-seconds ()
  "failover.first_token_seconds when the shared config sets it to a number
of 0 or more, else *FIRST-TOKEN-SECONDS*."
  (let ((value (when-let (failover (json-value (read-shared-config) :object "failover"))
                 (gethash "first_token_seconds" failover))))
    (if (and (realp value) (>= value 0)) value *first-token-seconds*)))

(defun call-with-attempt-watch (next thunk)
  "Call THUNK, one provider attempt, watched until its first token when NEXT,
the failover candidate (:PROVIDER P :MODEL M) a slow start would leave for,
stands behind it."
  (if (null next)
      (funcall thunk)
      (let* ((seconds (first-token-seconds))
             (watch (make-attempt-watch
                     :next next :seconds seconds
                     :deadline (and (plusp seconds)
                                    (+ (get-internal-real-time)
                                       (round (* seconds internal-time-units-per-second))))))
             (turn-id (and *current-durable-turn*
                           (nlk:durable-turn-turn-id *current-durable-turn*)))
             (*attempt-watch* watch))
        (when turn-id (setf (gethash turn-id *attempt-watches*) watch))
        (unwind-protect (funcall thunk)
          (when (and turn-id (eq watch (gethash turn-id *attempt-watches*)))
            (remhash turn-id *attempt-watches*))))))

(defun note-attempt-token ()
  "The attempt this thread walks streamed a token: its wait is over."
  (when *attempt-watch* (setf (attempt-watch-token-p *attempt-watch*) t)))

(defun waiting-attempt (turn-id)
  "The watch of TURN-ID's attempt while it waits for its first token, or NIL."
  (let ((watch (gethash turn-id *attempt-watches*)))
    (and watch (not (attempt-watch-token-p watch)) watch)))

(defun provider-attempt-next (turn-id)
  "The candidate (:PROVIDER P :MODEL M) TURN-ID's attempt would leave for,
while it waits for its first token unskipped; NIL when nothing waits so, or
no candidate stands behind it."
  (let ((watch (waiting-attempt turn-id)))
    (and watch (null (attempt-watch-skip watch)) (attempt-watch-next watch))))

(defun skip-provider-attempt (turn-id &optional (reason "skipped"))
  "Cut TURN-ID's provider attempt while it waits for its first token, so its
round leaves for the failover candidate within a second; REASON is the cut's
words. => that candidate, (:PROVIDER P :MODEL M), or NIL when no such wait
stood to cut."
  (let ((watch (waiting-attempt turn-id)))
    (when (and watch (null (attempt-watch-skip watch)))
      (setf (attempt-watch-skip watch) reason)
      (attempt-watch-next watch))))

(defun attempt-cut-p (watch)
  "True when WATCH's wait ended without a token: skipped, or past its deadline."
  (and watch
       (not (attempt-watch-token-p watch))
       (or (attempt-watch-skip watch)
           (and (attempt-watch-deadline watch)
                (>= (get-internal-real-time) (attempt-watch-deadline watch))))
       t))

(defun attempt-cut-condition (&rest initargs)
  "The PROVIDER-SLOW-START, made with INITARGS, that the cut of this thread's
watched wait stands for, or NIL when nothing cut it."
  (let ((watch *attempt-watch*))
    (when (attempt-cut-p watch)
      (apply #'make-condition 'provider-slow-start
             :detail (or (attempt-watch-skip watch)
                         (format nil "no first token in ~a s"
                                 (idle-seconds-label (attempt-watch-seconds watch))))
             initargs))))

(defun call-with-watched-deadline (seconds thunk &aux (watch *attempt-watch*))
  "Call THUNK under a deadline of SECONDS that looks at this thread's watch
once a second: a cut ends the wait at that look, as the deadline would."
  ;; The deadline is deferred a second at a time; one not deferred unwinds
  ;; to the WITH-IDLE-CUT around the read, which names the cut.
  (let ((until (+ (get-internal-real-time)
                  (round (* seconds internal-time-units-per-second)))))
    (handler-bind ((sb-sys:deadline-timeout
                     (lambda (condition)
                       (let ((left (/ (- until (get-internal-real-time))
                                      internal-time-units-per-second)))
                         (when (and (plusp left) (not (attempt-cut-p watch)))
                           (sb-sys:defer-deadline (float (min 1 left)) condition))))))
      (sb-sys:with-deadline (:seconds (min 1 seconds))
        (funcall thunk)))))

(defmacro with-read-deadline ((seconds) &body body)
  "BODY, one park of a provider attempt, under a deadline of SECONDS -- looked
at once a second (CALL-WITH-WATCHED-DEADLINE) while this thread's attempt
waits for its first token."
  (alexandria:with-gensyms (limit)
    `(let ((,limit ,seconds))
       (if (and *attempt-watch* (not (attempt-watch-token-p *attempt-watch*)))
           (call-with-watched-deadline ,limit (lambda () ,@body))
           (sb-sys:with-deadline (:seconds ,limit) ,@body)))))

(defmacro with-idle-cut ((seconds) form &body on-cut)
  "Evaluate FORM, each of whose parks runs under a deadline of SECONDS:
FORM's values when it completes, ON-CUT's when one of those deadlines passed."
  `(handler-case ,form
     (sb-sys:deadline-timeout ()
       (note-phase "idle-cut after=~as" (idle-seconds-label ,seconds))
       ,@on-cut)))

(defun read-octet-line (stream buffer seconds)
  "The next line of octet STREAM as a string, or NIL at EOF with nothing
read: octets up to a LF gathered into BUFFER (the attempt's reusable line
buffer), a CR before the LF dropped, and the line decoded as UTF-8 the
moment its newline arrives — SSE is UTF-8 by its spec, an invalid sequence
reads as ?."
  ;; The line is whole however long: the evidence bound is on what is kept,
  ;; never on what is parsed. Each octet is read under a deadline of SECONDS,
  ;; so a line arriving slowly is a lively stream, never a silent one.
  (setf (fill-pointer buffer) 0)
  (flet ((line (&aux (end (fill-pointer buffer)))
           (when (and (plusp end) (= 13 (aref buffer (1- end))))
             (decf end))
           (sb-ext:octets-to-string buffer :end end
                                           :external-format '(:utf-8 :replacement #\?))))
    (loop for octet = (with-read-deadline (seconds) (read-byte stream nil nil))
          while (and octet (/= octet 10))
          do (vector-push-extend octet buffer)
          finally (return (and (or octet (plusp (fill-pointer buffer))) (line))))))

(defmacro with-phase-note ((label &optional fields &rest arguments) &body body
                           &aux (t0 (gensym "T0")))
  "Run BODY, appending one LABEL ms=<wall> phase line when the log is on,
FIELDS -- a format control over ARGUMENTS -- after the timing."
  ;; Values pass through; the timing itself is debug-only retention. LABEL and
  ;; FIELDS are literal strings, joined into one control here.
  `(let ((,t0 (get-internal-real-time)))
     (multiple-value-prog1
         (progn ,@body)
       (note-phase
        ,(format nil "~a ms=~~,1f~@[ ~a~]" label fields)
        (/ (* 1000.0 (- (get-internal-real-time) ,t0))
           internal-time-units-per-second)
        ,@arguments))))

(defmacro cancellable-post ((request-json address) &body post-form)
  "One blocking provider POST (POST-FORM) to ADDRESS, the URL it dials, as a
cancellable wait: Esc interrupts the thread parked on connect/headers instead
of waiting for bytes off the socket."
  ;; The cancel unwind passes through untouched; a status failure carries
  ;; REQUEST-JSON as evidence, a refusal names ADDRESS, and every other
  ;; transport failure classifies by its class (a host's, by ADDRESS too).
  (alexandria:with-gensyms (c e body headers)
    `(handler-case
         (nlk:with-cancellable-wait (*current-durable-turn*)
           (with-phase-note ("post-headers" "req-bytes=~d" (length ,request-json))
             ,@post-form))
       (nlk:turn-cancelled-condition (,c) (error ,c))
       ;; with its Retry-After; the bounded body, its Content-Type and the request ride as evidence
       (dex:http-request-failed (,e)
         ;; The drain is here because :WANT-STREAM T changes what a DEX:HTTP-REQUEST-FAILED
         ;; condition carries: instead of a decoded string, dexador hands the
         ;; condition the still-open response stream, so PRINC-TO-STRING yields
         ;; "#<DECODING-STREAM {...}>" and the provider's own explanation is
         ;; discarded. A non-text content type — which an error page frequently has —
         ;; arrives as octets instead, and a failure raised before any body arrives
         ;; carries a string or nothing. Draining here keeps the diagnosis at the
         ;; seam that owns it, and every lane reads failures the same way.
         ;;
         ;; NLK:BODY-TEXT dispatches a stream on its element type. A text-ish
         ;; Content-Type arrives as dexador's character DECODING-STREAM; anything
         ;; else — a CloudFront error page answering application/octet-stream to a
         ;; JSON request — stays the binary KEEP-ALIVE-STREAM, which implements only
         ;; the byte readers, so READ-LINE on it has no method and the body would
         ;; render as `<response body unreadable>' while the provider's explanation
         ;; would remain unread in the socket. Octets drain through READ-SEQUENCE
         ;; and decode.
         ;;
         ;; Errors while draining keep what arrived: this runs on a path that is
         ;; already failing, and losing the original status to a secondary error
         ;; would be worse than a partial body.
         (let ((,body (nlk:body-text (dex:response-body ,e) +max-provider-evidence-bytes+))
               (,headers (ignore-errors (dex:response-headers ,e))))
           (error 'provider-error
                  :status (dex:response-status ,e)
                  :detail (subseq ,body 0 (min 400 (length ,body)))
                  ;; integral seconds only: back-off covers an HTTP-date form
                  :retry-after (let ((value (json-value ,headers :any "retry-after")))
                                 (typecase value
                                   (integer value)
                                   (string (ignore-errors (parse-integer value :junk-allowed t)))))
                  :scope :request
                  :evidence-body (bounded-evidence ,body)
                  :evidence-request-body (bounded-evidence ,request-json)
                  :evidence-content-type (json-value ,headers :string "content-type")
                  :stream-complete-p t)))
       (usocket:connection-refused-error (,e)
         (error 'provider-connection-refused
                :detail (format nil "~a — is the server running?"
                                (transport-failure-label ,e ,address))))
       (error (,e)
         (error 'provider-error :scope :request
                :detail (transport-failure-label ,e ,address))))))

(defun walk-provider-stream (fold &key config request-json headers on-part
                                       ;; the credential's address, else the one the config froze:
                                       ;; a lane that composed an endpoint of its own keeps it
                                       (endpoint (or (credential-attribute config :endpoint)
                                                     config.endpoint))
                                       finish-on-sentinel (require-done t)
                                       incomplete-detail signal-error-fn degraded-finish)
  "POST one streaming provider request and walk its SSE body, calling FOLD
once per decoded data frame (a hash table) with the frame and the two
functions below."
  ;; Non-object data frames, comments, and blank lines are skipped.
  ;;
  ;; This is the transport bracket the streaming lanes share; a lane owns only
  ;; its fold. The bracket owns, exactly once:
  ;;
  ;;   - the cancellable POST: ENDPOINT (CONFIG's own by default) and HEADERS
  ;;     are the lane's, behind the JSON content-type; CONFIG supplies the
  ;;     timeouts; the request streams with the connection pool on
  ;;   - the idle deadline: armed across the POST and each octet read
  ;;     (WITH-IDLE-CUT); a cut before headers is a
  ;;     request-scope PROVIDER-ERROR, after them a PROVIDER-STREAM-INCOMPLETE
  ;;   - the non-200 refusal (with REQUEST-JSON as evidence) and the buffered
  ;;     non-SSE 200 verdict
  ;;   - the :stream-start part on ON-PART
  ;;   - per-line discipline: lines split from the socket's octets by the walk
  ;;     itself and decoded as each newline arrives (READ-OCTET-LINE), the
  ;;     cancellation check, the attempt clock, and bounded evidence capture
  ;;     (every line folds whole; only what is kept as evidence is clipped)
  ;;   - completion: FINISH-ON-SENTINEL T takes data: [DONE] as the done marker
  ;;     (otherwise the sentinel is a silent no-op); a lane whose done marker is
  ;;     a frame calls FINISH-STREAM from FOLD; while REQUIRE-DONE is true (the
  ;;     default), EOF before either signals PROVIDER-STREAM-INCOMPLETE with
  ;;     INCOMPLETE-DETAIL — after preferring a recorded error frame through
  ;;     SIGNAL-ERROR-FN when one arrived
  ;;   - the degraded-error rule, when DEGRADED-FINISH is supplied: error
  ;;     frames were recorded but the provider still declared normal completion
  ;;     (DEGRADED-FINISH, called, answers a finish reason that is neither
  ;;     "error" nor "unknown") — degrade to a warning; otherwise the first
  ;;     recorded frame signals through SIGNAL-ERROR-FN (turn_engine.zig:4383)
  ;;   - evidence attachment on every PROVIDER-ERROR unwinding the walk, the
  ;;     drain of a finished body's tail so its socket pools, and the
  ;;     dirty-socket-safe close on every exit
  ;;
  ;; FOLD's two functions:
  ;;
  ;;   (FINISH-STREAM)                     mark the stream complete and stop
  ;;   (RECORD-STREAM-ERROR PAYLOAD FRAME) emit an :error part carrying PAYLOAD,
  ;;                                       record FRAME for stream-end fatality
  ;;
  ;; Returns the bounded evidence text captured during the walk, so a lane
  ;; whose terminal error signals after the bracket (the responses lane) can
  ;; re-attach it.
  ;; Debug-only: whether this round reuses a socket or dials (the pool count
  ;; for the host, before the POST steals one).
  (when (credential-env "NODECODE_PHASE_LOG")
    (note-phase "connect pooled=~d ~a"
                (pooled-connection-count endpoint) (provider-connection-key endpoint)))
  (let ((seconds config.request-timeout)
        (session (and *current-durable-turn*
                      (nlk:durable-turn-session-id *current-durable-turn*))))
    (multiple-value-bind (stream status response-headers)
        (with-idle-cut (seconds)
            (with-read-deadline (seconds)
              (flet ((post (whole)
                       (multiple-value-bind (content content-headers)
                           (body-wire endpoint request-json session :whole whole)
                         (cancellable-post (request-json (or config.proxy dex:*default-proxy* endpoint))
                           (dex:post endpoint
                                     ;; the lane's headers, then whatever the credential brings,
                                     ;; then the app's name for a host that ranks apps
                                     :headers (with-user-agent
                                               (with-app-attribution
                                                endpoint
                                                (append content-headers
                                                        headers
                                                        (credential-attribute config :headers))))
                                     :content content
                                     ;; usocket makes :READ-TIMEOUT the fd-stream's timeout,
                                     ;; which cuts a plain-http socket (a local model server)
                                     ;; and never a TLS one; at the configured number it
                                     ;; raced the idle clock to the verdict over plain http
                                     ;; with its own IO-TIMEOUT wording. Four times over it
                                     ;; is a backstop that fires only if the deadline itself
                                     ;; is gone. :CONNECT-TIMEOUT is a real deadline on the dial.
                                     :read-timeout (* 4 seconds)
                                     :connect-timeout seconds
                                     :proxy (or config.proxy dex:*default-proxy*)
                                     :want-stream t
                                     ;; Octets: dexador's own character decoding fills a
                                     ;; 128-byte buffer INSIDE the POST — a park where a
                                     ;; cut leaks the socket (its cleanup has already
                                     ;; nilled the stream it would close) — and every
                                     ;; refill after waits for 128 more, so a short line
                                     ;; sat until the next one or EOF. The walk splits
                                     ;; and decodes lines itself (READ-OCTET-LINE), under
                                     ;; its clock, with the socket in hand.
                                     :force-binary t
                                     :use-connection-pool t)))))
                ;; A relay that no longer holds the base (restarted, or past its
                ;; budget) says so with 412: the body goes again against nothing,
                ;; which rebuilds from the delta alone.
                (multiple-value-bind (stream status response-headers)
                    (handler-case (post nil)
                      (provider-error (e)
                        (if (and (eql 412 (provider-error-status e))
                                 (gethash (provider-connection-key endpoint) *delta-endpoints*))
                            (post t)
                            (error e))))
                  (note-body-wire endpoint request-json session response-headers)
                  (values stream status response-headers))))
          (error (or (attempt-cut-condition :scope :request
                                            :evidence-request-body (bounded-evidence request-json))
                     (make-condition 'provider-error :scope :request
                                     :detail (format nil "no response for ~a s" (idle-seconds-label seconds))
                                     :evidence-request-body (bounded-evidence request-json)))))
      (unless (and (integerp status) (= status 200))
        (error 'provider-error :status status :detail "non-200 response"
               :scope :request
               :evidence-request-body (bounded-evidence request-json)))
      ;; A declared non-SSE Content-Type on a 200 is a buffered verdict, not a stream.
      (let ((content-type (json-value response-headers :string "content-type")))
        (when (and content-type (not (ppcre:scan "(?i)^[ \\t]*text/event-stream" content-type)))
          (let ((body (nlk:body-text stream +max-provider-evidence-bytes+)))
            (close-provider-stream-body stream)
            (error 'provider-non-sse-response
                   :status 200
                   :detail (format nil "provider answered the stream request with ~
                                           a non-SSE ~a body: ~a"
                                   content-type
                                   (subseq body 0 (min 400 (length body))))
                   :evidence-body (bounded-evidence body)
                   :evidence-request-body (bounded-evidence request-json)
                   :evidence-content-type content-type
                   :stream-complete-p t))))
      (emit-stream-part on-part :stream-start)
      (let ((evidence (make-string-output-stream))
            (bytes 0)
            (done nil)
            (count 0)
            (first nil)
            ;; The attempt's line buffer when STREAM yields octets; a
            ;; stream that already yields characters (a canned stream)
            ;; is read with READ-LINE.
            (buffer (and (octet-stream-p stream)
                         (make-array 512 :element-type '(unsigned-byte 8)
                                         :adjustable t :fill-pointer 0))))
        (with-cleanup ((close-provider-stream-body stream))
          (handler-bind ((provider-error
                           (lambda (condition)
                             (attach-provider-evidence
                              condition (get-output-stream-string evidence) request-json))))
            (with-idle-cut (seconds)
                (block walk
                  (flet ((finish-stream ()
                           (setf done t)
                           (return-from walk))
                         (record-stream-error (payload frame)
                           (emit-stream-part on-part :error :payload payload)
                           (incf count)
                           (unless first (setf first frame))))
                    (loop
                      (when *current-durable-turn*
                        (nlk:ensure-turn-not-cancelled *current-durable-turn*))
                      ;; A watched wait cut between lines -- a keep-alive
                      ;; keeps every read short -- ends here.
                      (when-let (cut (attempt-cut-condition :scope :stream))
                        (error cut))
                      ;; A read error mid-body is truncation; a cancel unwinds as itself.
                      (let ((line (handler-case (nlk:with-cancellable-wait (*current-durable-turn*)
                                                  (if buffer
                                                      (read-octet-line stream buffer seconds)
                                                      (with-read-deadline (seconds)
                                                        (read-line stream nil nil))))
                                    (nlk:turn-cancelled-condition (c) (error c))
                                    (error (e)
                                      (error 'provider-stream-incomplete
                                             :detail (stream-cut-detail e))))))
                        (when (null line)
                          (when require-done
                            (when (and first signal-error-fn)
                              (funcall signal-error-fn first))
                            (error 'provider-stream-incomplete :detail incomplete-detail))
                          (return-from walk))
                        (let ((room (- +max-provider-evidence-bytes+ bytes)))
                          (when (plusp room)
                            (write-line line evidence :end (min room (length line)))
                            (incf bytes (1+ (length line)))))
                        (multiple-value-bind (kind value) (parse-sse-line line)
                          (case kind
                            (:done (when finish-on-sentinel (finish-stream)))
                            (:data
                             (let ((frame (ignore-errors (nlk:decode-json value))))
                               (when (hash-table-p frame)
                                 (funcall fold frame #'finish-stream #'record-stream-error))))))))))
              ;; Silence after headers is truncation of the stream — a
              ;; recorded error frame outranks it, as at EOF — so the
              ;; stream budget applies.
              (when (and first signal-error-fn)
                (funcall signal-error-fn first))
              (error (or (attempt-cut-condition :scope :stream)
                         (make-condition 'provider-stream-incomplete
                                         :detail (format nil "no byte for ~a s"
                                                         (idle-seconds-label seconds))))))
            ;; After the fold declared the stream complete, read what is already there of
            ;; the tail -- the event terminator and the terminal chunk -- so a body that ends
            ;; where the stream did reaches EOF and the keep-alive wrapper pools its socket
            ;; for the next round. Splitting lines ourselves delivers the done marker before
            ;; those bytes are read (dexador's decoder over-read them, which is the only
            ;; reason a clean stream ever pooled). Only what has arrived (LISTEN on the raw
            ;; socket, a provider writes the marker and the terminal chunk together) and at
            ;; most a few KiB, so a tail that is not there or does not end costs a missed
            ;; reuse, never a wait; the one park this can take -- a chunk header split
            ;; across records -- is under the idle deadline, and its cut abandons the
            ;; drain, never the completed stream.
            (when-let (raw (and done buffer (provider-stream-socket stream)))
              (with-idle-cut (seconds)
                  (sb-sys:with-deadline (:seconds seconds)
                    (loop repeat 4096
                          while (and (open-stream-p raw) (ignore-errors (listen raw)))
                          do (unless (read-byte stream nil nil) (return))))
                nil))
            (when (and degraded-finish (plusp count))
              (let ((finish (funcall degraded-finish)))
                (if (and done (stringp finish)
                         (not (member finish '("error" "unknown") :test #'string=)))
                    (warn "provider stream carried ~d in-stream ~
                                 error frame~:p but declared normal ~
                                 completion (~a); degrading to a warning"
                          count finish)
                    (funcall signal-error-fn first))))))
        (get-output-stream-string evidence)))))

;;; --- stream cadence: live-only rollup for the provider_request fact --------

(defstruct (stream-cadence (:copier nil))
  "What one round's live part stream looked like, rolled up to a handful of
numbers instead of one persisted event per chunk (the turn_engine.zig
LiveProviderPartProjection rollup carried on the turn.provider_request fact)."
  ;; Timing fields stay NIL on a round with no observer (:ON-PART NIL), where
  ;; no parts flow. SENT-MS is stamped by CALL-PROVIDER as the request goes
  ;; out; FIRST-TOKEN-MS is the first word of any kind -- text, thinking or a
  ;; tool call's input -- since a round that only calls tools streams no text,
  ;; and would otherwise have no first token time.
  (part-count 0 :type (integer 0))
  (text-delta-count 0 :type (integer 0))
  (sent-ms nil :type (or null integer))
  (first-part-ms nil :type (or null integer))
  (first-token-ms nil :type (or null integer))
  (last-part-ms nil :type (or null integer)))

(nlk:access (cadence stream-cadence))

(defun observe-stream-cadence (cadence part &aux (now (elapsed-ms 0)))
  (unless cadence.first-part-ms (setf cadence.first-part-ms now))
  (setf cadence.last-part-ms now)
  (incf cadence.part-count)
  (when (member part.kind '(:text-delta :reasoning-delta :tool-input-start
                            :tool-input-delta :tool-call))
    ;; A token ends the attempt's watched wait (ATTEMPT-WATCH).
    (note-attempt-token)
    (unless cadence.first-token-ms (setf cadence.first-token-ms now)))
  (when (eq :text-delta part.kind)
    (incf cadence.text-delta-count))
  part)

(defun stream-cadence-duration-ms (cadence &aux (first (or cadence.first-token-ms
                                                           cadence.first-part-ms))
                                                (last cadence.last-part-ms))
  "Milliseconds from the first token to the last part -- the time the answer
streamed, which TTFT does not overlap -- or NIL. A stream that carried no
token runs from its first part."
  ;; :STREAM-START is a part and fires on the response head, which vLLM sends
  ;; as it accepts a request it then queues: timed from the first part, the
  ;; queue counted in TTFT and again in the stream.
  (and first last (- last first)))

(defun stream-cadence-ttft-ms (cadence &aux (sent cadence.sent-ms)
                                            (first cadence.first-token-ms))
  "Milliseconds from the request going out to its first token, or NIL."
  (and sent first (- first sent)))

(defun chat-tool-call-object (id name arguments)
  "One chat-shaped tool_calls entry, in the key order the prompt-cache
prefix depends on."
  (json-object "id" id
               "type" "function"
               "function" (json-object "name" name "arguments" arguments)))

;;; --- shared request shaping ------------------------------------------------
;;; The walks every lane repeats on the way OUT: durable history's typed
;;; content parts into that lane's block vocabulary, a chat tool_call's
;;; serialized arguments back into an object, and a function tool wrapper
;;; into the lane's flat declaration.

(defmacro with-request-shape (context (&rest bindings) &body body)
  "Every request builder's preamble: CONTEXT is a COMPILED-TURN-CONTEXT, and
CONFIG, EFFORT, TOOLS and CHOICE name what all four wires read off it before
BINDINGS, which a lane may shadow with its own spelling of one."
  ;; The four differ in how they SPEND these — a budget, a thinkingLevel, an
  ;; effort name, an omitted key — never in where they come from, and the
  ;; check that the caller handed over a compiled context is the same check
  ;; five times over.
  `(progn
     (check-type ,context compiled-turn-context)
     (let* ((config (compiled-turn-context-provider-config ,context))
            (effort (effective-provider-config-reasoning-effort config))
            (tools (compiled-turn-context-tools ,context))
            (choice (effective-provider-config-tool-choice config))
            ,@bindings)
       (declare (ignorable config effort tools choice))
       ,@body)))

(defun map-content-parts (content &key text-fn image-fn drop-empty-text-p &aux (blocks '()))
  "CONTENT's typed parts as a list of lane blocks: text parts through TEXT-FN
(always a string — \"\" when the wire carried a non-string), image_url parts
through IMAGE-FN, and, when no part survived, (CONTENT-TEXT CONTENT) through
TEXT-FN as the one block, the same text rule applied."
  ;; A NIL from either leaf drops that part. DROP-EMPTY-TEXT-P skips empty
  ;; text parts (the gen_ai.zig addGeminiTextPart rule). The lanes'
  ;; empty-content policies differ in exactly that — one input_text part, or
  ;; no parts at all — so the same flag that drops an empty text part drops
  ;; the empty fallback, and no lane restates its text leaf as a fallback.
  (flet ((add (block) (when block (push block blocks)))
         (text-block (text)
           (unless (and drop-empty-text-p (zerop (length text))) (funcall text-fn text))))
    (dolist (part (message-content-parts content))
      (let ((type (content-part-type part)))
        (cond
          ((equal type "text") (add (text-block (or (json-value part :text "text") ""))))
          ((equal type "image_url")
           (when-let (url (json-value part :string "image_url" "url"))
             (add (funcall image-fn url)))))))
    (or (nreverse blocks)
        (when-let (block (text-block (content-text content))) (list block)))))

;; A system message in the history (the eviction stub), as the user text a lane without system
;; turns carries.
(defun system-stub-text (content) (concatenate 'string "[system] " (content-text content)))

(defun tool-call-function-input (call)
  "(values NAME INPUT) for one chat-shaped tool_call: NAME is the function
name (\"\" when absent) and INPUT its arguments decoded as a JSON object."
  ;; Arguments that do not parse as an object degrade to an empty input rather
  ;; than failing the request: the tool result already recorded what actually
  ;; ran.
  (let ((arguments (json-value call :string "function" "arguments")))
    (values (or (json-value call :string "function" "name") "")
            (or (and arguments
                     (json-value (ignore-errors (nlk:decode-json arguments))
                                 :object))
                (make-hash-table :test #'equal)))))

(defun tool-declarations (tools schema-key &key type (description ""))
  "TOOLS' chat function wrappers as a vector of a lane's flat declarations, each in key order:
TYPE when given, name, description (DESCRIPTION when absent), then the
parameters schema under SCHEMA-KEY."
  (map 'vector
       (lambda (wrapper &aux (fn (gethash "function" wrapper)))
         (let ((declaration (json-object :opt "type" type
                                         "name" (gethash "name" fn)
                                         "description" (or (gethash "description" fn)
                                                           description))))
           (setf (gethash schema-key declaration)
                 (or (gethash "parameters" fn) (json-object "type" "object")))
           declaration))
       tools))


;;; --- what the lanes share --------------------------------------------------
;;; Defined before the lane files under engine/providers/ load: the response
;;; envelope every lane emits, DEFINE-PROVIDER-LANE that each lane is written
;;; in, the data: URI reading the anthropic and google lanes share, and the
;;; two OpenAI wires' thinking-mode model test and cache-routing key.

(defun assembly-response-metadata (asm object &optional reasoning-effort)
  "Emit ASM's :response-metadata from the id and model wire OBJECT carries, or
nothing when OBJECT is NIL: every lane's response envelope names both."
  ;; once per attempt, from the first frame offered
  (when (and object (not asm.metadata-emitted-p))
    (setf asm.metadata-emitted-p t)
    (emit-stream-part asm.on-part :response-metadata
                      :id (json-value object :string "id")
                      :model (json-value object :string "model")
                      :reasoning-effort reasoning-effort)))

(defmacro define-provider-lane (name (&rest lane) docstring
                                (frame &key request state close-order message
                                            (finish-reason 'finish-reason) after-finish)
                                (&rest walk) &body body)
  "Define NAME (CONTEXT &key (ON-PART *TURN-PART-FN*)), one lane's streaming
entry, and register it as the stream symbol of the lane LANE's initargs
describe."
  ;; It binds CONFIG, the STATE bindings, REQUEST-JSON (the REQUEST form:
  ;; the body as UTF-8 octets, the form it goes out and is kept in) and ASM; runs WALK-PROVIDER-STREAM with WALK's keywords, BODY folding each
  ;; FRAME beside FINISH-STREAM, RECORD-STREAM-ERROR and CLOSE-SPANS (CLOSE-ORDER);
  ;; closes the spans; binds MESSAGE (plus the MESSAGE specs), USAGE and the
  ;; FINISH-REASON form (the lane's own variable by default) and emits :finish; runs
  ;; AFTER-FINISH, its provider errors carrying the walk's evidence; and answers
  ;; (values MESSAGE USAGE FINISH-REASON REQUEST-JSON). Where WALK names none, the
  ;; headers are the lane family's auth headers (PROVIDER-AUTH-HEADERS) and the
  ;; incomplete detail says the stream ended before the answer finished.
  (let ((finish (gensym "FINISH")) (record (gensym "RECORD")) (evidence (gensym "EVIDENCE")))
    `(progn
       (defun ,name (context &key (on-part *turn-part-fn*))
         ,docstring
         (let* ((config (compiled-turn-context-provider-config context))
                ,@state
                (request-json ,request)
                (asm (make-lane-assembly :on-part on-part))
                (,evidence
                  (walk-provider-stream
                   (lambda (,frame ,finish ,record)
                     (flet ((finish-stream () (funcall ,finish))
                            (record-stream-error (payload frame) (funcall ,record payload frame))
                            (close-spans () (assembly-close-spans asm :order ',close-order)))
                       (declare (ignorable #'finish-stream #'record-stream-error #'close-spans))
                       ,@body))
                   :config config :request-json request-json :on-part on-part
                   :degraded-finish (lambda () ,finish-reason) ,@walk
                   :headers (provider-auth-headers ,(getf lane :family) config.api-key)
                   :incomplete-detail "the stream ended before the answer finished")))
           (assembly-close-spans asm :order ',close-order)
           (flush-thinking-tag asm)
           ;; the round, chat-shaped: empty content is null, MESSAGE specs ride before tool_calls
           (let* ((message
                    (json-object
                     "role" "assistant"
                     "content" (let ((full (get-output-stream-string (lane-assembly-content asm))))
                                 (if (string= full "") :null full))
                     :when (lane-assembly-reasoning-seen-p asm) "reasoning_content"
                     (get-output-stream-string (lane-assembly-reasoning asm))
                     ,@message
                     :when (lane-assembly-tool-buffers asm) "tool_calls"
                     (map 'vector
                          (lambda (pair &aux (buf (cdr pair)))
                            (chat-tool-call-object (or (getf buf :id) "")
                                                   (getf buf :name) (getf buf :arguments)))
                          (sort (copy-list (lane-assembly-tool-buffers asm)) #'< :key #'car))))
                  ;; NIL, never a usage of nothing, when no field was ever reported
                  (usage (and (not (equalp asm.usage (make-provider-usage))) asm.usage))
                  (finish-reason ,finish-reason))
             (emit-stream-part on-part :finish)
             (handler-bind ((provider-error
                              (lambda (e) (attach-provider-evidence e ,evidence request-json))))
               ,@after-finish)
             (values message usage finish-reason request-json))))
       (register-provider-lane (make-provider-lane :stream-symbol ',name ,@lane)))))

(defun thinking-mode-model-p (model &aux (id (string-downcase (or model ""))))
  "Whether MODEL's family thinks as endpoint configuration, whatever the
request asks, and so wants reasoning on every assistant message it is shown."
  (or (search "deepseek" id)
      (and (search "kimi" id) (search "thinking" id))))

(defun parse-data-uri (url)
  "For a base64 data: URI, (values media-type data); NIL otherwise."
  ;; The media type runs to the first ; and the data from the first comma on.
  (when (stringp url)
    (ppcre:register-groups-bind (media-type data) ("(?s)^data:([^;,]*)[^,]*;base64[^,]*,(.*)" url)
      (values media-type data))))

(defun openai-prompt-cache-key (context)
  "Stable cache-shard routing key for OpenAI wire requests: one for every
conversation on the same static prompt."
  ;; The provider routes by the prompt's prefix and this key together, so a
  ;; key shared across sessions is what lets one session be served a prefix
  ;; another paid for: a fork whose history repeats its parent's, a new
  ;; conversation with the same static prompt. A per-session key would send
  ;; each fork to a shard that holds none of it, and its misses would come in
  ;; bursts across every key at once. History, session identity, the
  ;; credential's secret and endpoints stay absent; the credential's
  ;; discriminator rides as `credential'.
  (with-request-shape context
      ((material
        (json-object
         "version" "nodecode.agent-prompt-cache-routing.v1"
         "provider" (effective-provider-config-provider config)
         "model" (effective-provider-config-model config)
         "credential" (or (credential-attribute config :cache-key) :false)
         "system" (compiled-turn-context-base-system-prompt context)
         "tools" (coerce
                  (loop for wrapper across (or tools #())
                        for fn = (json-value wrapper :object "function")
                        when fn collect (or (gethash "name" fn) ""))
                  'vector)))
       (encoded (encode-json-object material)))
    (subseq (nlk::sha256-text encoded) 7)))
