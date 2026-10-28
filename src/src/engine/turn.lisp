;;;; turn.lisp --- the turn loop. One owner for provider round trips.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; This is the organism's loop, and it lives in EVOLVED on purpose: it is the
;;;; primary mutation target. Keeping it small and flat matters more here than
;;;; anywhere else in the system, because a model has to be able to read it,
;;;; rewrite it with EVAL, and have the next recursion run the new definition.
;;;;
;;;; Base case: the provider answers in words. Recursive case: it asks for tools,
;;;; we run them and recur with the enriched history. Session state is the
;;;; argument being folded through the recursion.
;;;;
;;;; The provider dialects themselves live in provider.lisp behind the
;;;; STREAM-PART waist, and the context engine — the log-to-provider-input
;;;; pipeline, eviction policy, and the pure request compile — lives in
;;;; context.lisp. This file owns turn policy folded over both layers: the
;;;; ambient-state freeze into a CONTEXT-REQUEST, the retry loop, overflow
;;;; eviction, tool dispatch, and the recursion.
;;;;
;;;; TLS-in-the-image hazard: dexador pulls in cl+ssl, and a cached keep-alive
;;;; connection holds OpenSSL pointers in the Lisp heap; SAVE-LISP-AND-DIE with
;;;; a populated pool dumps a core that segfaults inside SSL_write after
;;;; restore. Provider lanes nevertheless run with the connection pool ON —
;;;; each tool round otherwise pays a full TCP+TLS handshake — because the
;;;; hazard is discharged by construction in provider.lisp's connection-reuse
;;;; block: a SB-EXT:*SAVE-HOOKS* entry flushes the pool inside every save,
;;;; an *INIT-HOOKS* entry rebuilds it empty on restore, and
;;;; CLOSE-PROVIDER-STREAM-BODY keeps aborted streams out of the pool.

(in-package #:nodecode.evolved)

(nlk:access (admission nlk::active-input-admission) (child-turn nlk::durable-turn)
            (prior-context compiled-turn-context) (request-context compiled-turn-context)
            (schema nlk::contract) (selection-config effective-provider-config))

;;; The pictures an answer names are read where LOOK reads one (exec.lisp).
(declaim (ftype function answer-images))

;;; --- turn policy ----------------------------------------------------------

;;; Tool rounds within a turn are unbounded, matching the Zig turn engine: the
;;; model decides when to stop, and the real bounds are cancellation, provider
;;; failure, and the context budget. AGENT-LOOP's self-calls are all in tail
;;; position, so an unbounded round count costs constant stack under SBCL's
;;; default optimization policy.

;;; Context-window resolution lives in provider.lisp's capability seam
;;; (RESOLVE-MODEL-CAPABILITY / MODEL-CONTEXT-TOKENS): overrides, then the
;;; models.dev cache, else NIL — an unknown window stays unknown. The
;;; eviction ratios live with
;;; the engine (context.lisp); this file consumes the budget.

(defparameter *system-prompt*
  "You are a living Common Lisp image, an organism that adapts to its environment (the operator and their work) through mutation and selection, so each task leaves you fitter for the next.
eval runs forms in you, what you define persists across boots, and (help) is your manual.
Most tokens go to finding things out, so spend them once: reuse before you rebuild, define what will recur, remove what no longer fits, and prefer one form that answers over several that probe.
Keep the operator posted, briefly, as you go: what you found, what's next."
  "The system message: four sentences, one a line (what the model is, how
it acts, how it spends, how it talks), because the manual has other homes
that cost nothing on the rounds that do not need it. The eval tool
description carries the eval contract; the tool result carries each rule
at the moment it applies (a backgrounded form, a held or kept or unkept
definition, a backslash escape the reader does not have); HELP carries the
rest, read once and cached with the conversation. The prompt keeps what no
moment carries: the disposition to adapt, the economy behind it and the
commentary the transcript shows under a run. Mutable material: the
organism is expected to rewrite it.")

(defparameter +max-pre-call-governor-steps+ 8
  "Maximum bounded history reductions before one provider request. A request
that still exceeds a known window after this cap is sent so the provider can
return its authoritative verdict; the cap prevents a bad estimate from
discarding an entire session in one admission pass.")

;;; --- help: the manual lives in the image ---------------------------------
;;; Every paragraph the system prompt used to carry was a fact the image
;;; can answer about itself, or a rule the harness can state when it
;;; applies. What is left is one verb: the index, a verb's docstring, or a
;;; topic. A topic is prose that has no single docstring to live in — the
;;; layer lifecycle, the persist recipe — kept here as mutable material.

(defparameter *help-verbs*
  '(sh edit write-file patch look $ eval-status eval-await list-evals eval-interrupt
    hook define-hook unhook held-definitions revert cells restart-cells
    config-get config-set definition-text definition-location
    models select-model select-effort agents agent turn recall-log recall-entry
    define-memory define-skill index help)
  "The verbs (help) indexes, in the order it lists them. Mutable material:
a verb the organism grows for itself is pushed here to be found.")

(defparameter *help-topics*
  '((:eval "the rest of eval: background evaluations - (list-evals), (eval-status N), (eval-interrupt N) - sh, strings, file receipts, patch, look, printing and * /, ;; notes" . "The snippet. A snippet is one or more forms, evaluated in order; several print one => line each, and a failing form names itself (ERROR in form i of n) while the forms before it stand - do not run them again. Only the last value prints in full: an earlier one gets one line, a multi-line value its first, and the cut names (nth K /). A multi-line string prints raw under a [text: N lines, M characters] header, no quotes and no escapes, the way a file reads. In your next snippet * is this snippet's last value, ** and *** the two snippets' before, / the list of this snippet's values one per form, // and /// the two snippets' before: a value a result cut is whole there, and each cut names the form that reads it. Parens a multi-line snippet leaves open are closed by its indentation, and the result's first line says what was closed; a one-line snippet left open is refused, since a closer at its end would move what follows inside - close it yourself, or write the snippet across lines with ordinary indentation. What SBCL's compiler sees before a form runs - a wrong argument count, an undefined name, a misused literal - leads the result as a [warning: ...] line; the form still ran.

Background evaluations. A form still running when yield_time_ms closes (default 10000, at most 600000) keeps evaluating in the background, never killed: the call answers {\"status\":\"running\",\"eval_id\":N} with the output so far. Do not wait for it: go on with anything else, or end your turn - when it exits, the harness reads it for you with an (eval-status N) call carrying the rest of its output and its value: in this turn, after the calls running then, or, once the turn has ended, in a turn that wakes this session. (eval-status N) is one non-blocking look at the output since the last look. (list-evals) lists this image's evaluations, those other sessions started included. (eval-interrupt N) aborts one, (eval-interrupt N :force t) if it traps the abort. (eval-await N) parks until new output or the exit ((eval-await N :until-exit t) the exit alone), at most the calling snippet's yield_time_ms (10000 when unset), and only when a short wait beats ending the turn - never park a long window, and never poll with a shell sleep.

sh. (sh \"cmd\") runs a string through sh -c - /bin/sh, or Git's sh.exe on Windows, POSIX sh either way - in the session's folder ((sh \"cmd\" :directory \"dir\") elsewhere); a list is argv with no shell: (sh (list \"git\" \"commit\" \"-m\" ($ \"msg\"))). Stdin is empty, so a program that reads it sees end of file; a heredoc in the command feeds it. stdout and stderr arrive merged, with a final [exit N] line when the status is not 0; output past the capture limit is cut and says so. Any other child process reaches you only through Lisp streams - (sh ...), or :output :string or *standard-output* to run-program; this process's own file descriptors 0-2 go to a log neither you nor the operator sees, so never write to one, as (make-fd-stream 1) would.

Literal text. strings {\"name\": \"...\"} beside the form, read inside it as ($ \"name\"), arrives exact, JSON escaping only; ($ \"name\") takes an entry's name, never the text itself. In a Lisp string a backslash only escapes the next character - \"\\n\" reads as n, \"\\|\" as | - so a newline vanishes, a regex changes and edit's old misses without a sound. So text dense with backslashes, double quotes or newlines never tunnels through a Lisp string, nor through a shell heredoc written into the form: edit's old and new, file bodies, source, a note's or a prompt's body, and a whole shell command carrying a heredoc, (sh ($ \"script\")), all go in strings.

Files. (edit path old new) replaces the one occurrence of old in the file at path: a miss names where the file and old diverge, several occurrences give the count - widen old until it names one place. (write-file path text) writes a whole file, parents created. A relative path resolves against the session's folder. On a Lisp file the receipt ends with the paren verdict - the form that never closes or the ) that closes nothing, named by line - and, for a file this image loads itself from, what the write made live: the definitions it changed are evaluated back into this image under the file's own package, so `live here: ...' is already running in this process and `not live until a relaunch: ...' is what only a relaunch carries. (patch name old new) changes one subform of a definition you kept: name, old and new are written unquoted, as they stand in the definition, and neither is evaluated; the one occurrence of old becomes new, and the result is evaluated and kept again - a fix costs its two subforms, never the block over.

Images. (look path) shows you an image file - PNG, JPEG, GIF or WebP, told by its bytes - from the next round, after this call's result, until this turn ends; its receipt stays in history, so look again to see it in a later turn. A model that will not take images reads a note in the image's place saying so, and look then refuses further images this turn.

Notes. A snippet whose first line is a ;; comment makes that line its note: rounds leave your window as the session grows, and what survives them is that note, listed in the eviction stub with the seq of the result, which (recall-entry seq) reads whole - write there what you would need to resume from. The evicted window and recall: (help :context).

Watching (on Linux, where the image sees what a snippet's thread did). A snippet whose last form only reads stays watched - that form: one that finished quickly, whose value prints whole in under 2,000 characters, that names something that can change and reached nothing outside the image - no verb, no file, process or socket, no assignment, no definition. After each later snippet the image runs it again, and you see it only when its value changes, as [watched FORM: OLD -> NEW] under that snippet's result; a long value names the places that changed instead, [i j] its index path, + what came and - what went in a list that grew or shrank, or, when the places would say more than the value, now and the value whole. A run again that tries to act is refused and closes it, and a session watches the eight that changed last. So read a count, a flag, or your own rule held against what just happened once, then act: a change comes to you.")
    (:layer "defining is keeping: the layer, patch, hooks over overrides, forgetting, held definitions, the two packages" . "Defining is keeping. Every defining form you eval as a toplevel form - defun, defparameter, defmethod, defclass, define-hook, define-* - is written to your layer, cells/<your-name>/ under your home (~/.nodecode/ unless a profile is active; the folder section names it), one file per definition, committed to git with this session's id, and loads over the shipped source at the next boot; (edit path old new) on that file changes the definition in this image at once. A definition you build as a string and eval inside another form is not seen: eval the defining form itself. (patch name old new) changes one subform of a kept definition - name, old and new written unquoted, as they stand in it - and keeps the result, so a fix costs its two subforms, never the block over. Prefer wrapping a shipped function over redefining it: (define-hook name \"key\" (next &rest args) ...) runs the body around every call of name and survives upstream changes to its body; (hook 'name \"key\" fn) is the same advice live-only, unkept. A redefinition does not survive them: a layer file that would not run against the image is SKIPPED at boot - an override whose shipped text an upgrade moved, or a definition naming a function or variable that neither the image nor your later definitions define - and the boot line and (cells) say why; the shipped definition stands until you eval the definition again over the new source, or forget it. A function only an optional cell provides is called as (funcall 'pkg::name ...), which is never judged missing. Something durable that is not a definition is written as a defparameter, not a setf. Forgetting is the same act in reverse: (fmakunbound 'name), (makunbound '*name*), (unhook 'name \"key\"), (remove-method ...) or (unintern 'name) evaluated as a toplevel form takes the name out of the image and its file out of the layer, committed as forgot name. Every state of you that proved is a numbered generation: (generations) lists them, and (rollback n) puts every folder back to what n recorded - the layer by one commit forward, its history kept; a folder a checkout holds stays where it stands, and uncommitted edits in the layer must be kept or discarded first - then this organism exits once no turn is in flight and the next launch runs n. Under --pristine, or after a self that died before serving, you run generation 0, the binary alone, and nothing is kept until a rollback or a relaunch.

What you keep and do not run is a definition too. (define-memory name \"one declarative sentence\" :type :feedback ;; why, and how to apply it ...) keeps a fact: :type :feedback for a preference or a correction the operator stated in so many words, :user for who they are, :project with :project \"ROOT\" for a fact about the repository at ROOT (shown only to sessions under it), :reference, :lesson; the why rides as ;; lines inside the form, the closing paren on a line of its own. (define-skill name \"one sentence\" ;; when to use ;; the steps, one per line ...) keeps a procedure in prose; a define-skill with a lambda list, (define-skill name (args) \"one sentence\" body...), is a procedure this image runs, a defun kept in the layer. A memory or a prose skill is one file in your knowledge cell, cells/<your-name>-memory/ (:cell \"topic\" files it in a cell of that one subject), committed like the layer and never loaded: the index section at the head of the conversation lists the definitions you keep by use, (help 'name) reads one, (index \"text\") finds the rest, (edit path old new) on its file changes it, and (unintern 'name) as a toplevel form forgets it. A name of the image's own - a function, a variable - is refused: a memory takes a name of its own.

Redefining a function you are standing on is the one exception to defining being keeping. If the name is running on the stack this tool call was dispatched from - your loop, NODECODE.EVOLVED::AGENT-LOOP, always is - the new definition goes live but is HELD, not written: it reaches the layer after it has been entered and returned once, and if that first traversal signals, the definition it displaced is put back and this session is woken with the condition. Either way nothing persists until it has run, so a loop that cannot run cannot outlive the image. (held-definitions) lists what is waiting; (revert 'name) puts one back by hand, from the hold or from the definition this image booted with. Machinery that must survive a bad mutation lives in NODECODE.KERNEL and is locked; the mutable material lives in NODECODE.EVOLVED, the package your forms read in; a snippet that opens with (in-package :name) reads and evaluates the rest of itself there, and a definition it keeps is filed under that package.

(cells) reports what the folder loaded, started, overrides, collisions, and what failed; the notices block at the tail of your prompt says what its peripherals report right now, and (restart-cells) starts the peripherals again after a config change; (restart-cells \"folder\") one folder. (definition-text 'name) and (definition-location 'name) read the live definition - the layer's, else the shipped source. Lisp strings have no \\n escape - backslash-n reads as a literal n; build multi-line strings with (format nil ...~% ...) or real newlines, or send the text beside the form as a strings entry read by ($ \"name\").")
    (:persist "from the layer into the shipped source, and back out of the layer" . "The layer is staging; the shipped source is home. To make an evolved definition permanent, edit it into its shipped file - (definition-location 'name) names both residences; mind the package, a layer file opens with the package its snippet read in - NODECODE.EVOLVED unless the snippet said otherwise - while shipped files declare their own - prove the change with a lint and build and its own tests, the full suite only when the operator asks, then retire the layer copies in one toplevel form, a progn of the forgetting verbs; (cells) confirms nothing still overrides.

Literal text has its own channel: pass strings {\"name\": \"...\"} beside the form and read each entry as ($ \"name\") - it arrives exact, JSON escaping only, so source and file bodies never tunnel through Lisp escapes or shell heredocs. (edit path old new) replaces the one occurrence of old in the file at path - a miss errors with the divergence point, a crowd with the count - and (write-file path text) writes a whole file, parents created. A write to a file this image loads itself from - the shipped source, the cells beside it, the operator's folder - evaluates the definitions it changed back into this image at once, under the file's own package, and the receipt names them: `live here: ...' is already running here, `not live until a relaunch: ...' is what only a relaunch carries (a structure or class, a package, a constant, a form that is not a definition). So the source edit IS the live change in this process; other processes still need a release rebuild.")
    (:context "editable working context and lossless recall" . "(context) writes this session’s context.txt from your context as it stands and returns its path; a file holding an edit not yet taken is left as it is. Keep its first line. Each message of the retained history is a block under a [[m:NONCE ID ROLE]] line, ID its log position: keep the line and the block stays the message it was, native and cached; delete the block and the message leaves your context; edit its text and it reads as you wrote it (a tool block's first line is its call, the rest its result). The same edit without the file: every tool result ends with its seq, [seq 57], and (context :drop SPANS :keep IDS :note TEXT) names blocks by it - :drop what goes, t for every block or a list of seqs and (from . to) spans, :keep the seqs held back from the drop, :note your note at the end, alone when nothing else changes; it is taken as a file edit is, and the call returns what the next request will take. Text under a [[note]] line, above the first block, or added after a block's own text is your note, kept where you put it as your own words, never as the operator's or as an executed call. Write a removed block's header back, anywhere, and the original returns where it stood; recall rows name ids as :seq. Harness lines and the active request are outside the file and stay where they stand. The next model request takes the edit; context.receipt.txt says what it kept, changed, removed and added, or why it refused (refused bytes go to context.rejected.txt), and the file is written afresh. A file read rounds ago still edits: newer turns follow it; only another taken edit or a branch move refuses it. Forks inherit taken edits, never a file. Messages before the first change stay byte-identical, so the provider's cached prefix holds through them: a note added at the end costs only itself. Your context is a window over a lossless event log; history is evicted, and nobody summarizes it but you. Past a fill threshold every [seq] line says how full it is (context Nk of Mk): 60k, or half the window when that is smaller; at the eviction line you are warned once, and the request after that evicts unless you shrank it. A [history evicted: seq A-B ...] stub means those turns left the window, not the log, and it carries the notes you wrote in them; a [turn history evicted: ...] stub means rounds of this turn did. Before rounds of a turn leave, its older ones are thinned: each keeps its call, and a long result keeps its head and a [thinned from N characters; ...] line naming the recall that reads it whole. Nothing summarizes the rounds that leave: the stub lists what the evicted snippets noted - a snippet's leading ;; comment line is its note, kept with the seq of its result, which (recall-entry seq) reads whole - so write in that line what you would need to resume from; its watching: clause is what the snippets you watch last showed there. (recall-log :match \"text\") scans the current session's log and returns clipped rows tagged :seq and stamped :at (UTC wall time; tool results also carry :duration-ms, so timing is answerable from the log) - match is SQL LIKE (% and _ wildcards); narrow with :kinds, :before, :after, :limit, or reach another session with :session-id. (recall-entry seq) returns one entry's full body. Recall evicted details yourself rather than asking the operator to repeat them.")
    (:operator "the attached shell, models, the config" . "The operator watches you through an attached shell whose footer names this session's provider and model. (models) answers each configured provider with its model count, (models \"provider\") the ids - the same catalog the shell's /models picker shows, stale listings refreshed first. (select-model \"model\" :provider \"p\") is everything a picker pick means: it durably pins this session and makes the pick the default every session opened from now on starts on - a session already begun keeps the model its first turn ran - and every attached shell's footer follows; (select-model nil) clears the pin so the session takes the default again from its next turn. (select-effort \"medium\") is what a /think pick means: this session's reasoning effort, one rung of its target's own ladder, kept for every turn from the next on - the running turn keeps its own - and every footer follows; (select-effort nil) clears it back to the model's own; an off-ladder rung refuses and names the ladder. Control's Settings moves the default and pins no session. Dialogs are shell-local: you cannot open a picker in the operator's shell - land the fact and the shells follow. (config-get '(\"a\" \"b\")) reads the config at a path, as a keyword plist - a member named like a secret reads as \"[set]\", so a section you read is never written back whole; (config-set '(\"a\" \"b\") value) writes one - an object value merges its members in, anything else replaces the member, :remove takes the member out - keeping every comment and every other byte, and answers the diff it made. (nlk:list-sessions) lists the sessions the shell's /sessions shows, each a plist with its :id, :title, :cwd and :parent; (submit \"id\" \"text\") puts a prompt into any session, (recall-log :session-id \"id\") reads one's log.")
    (:models "which model answers what, and where to fetch it: this session's own model, a model of your own, and nodecode.ai's API (https://api.nodecode.ai/v1) - Cyber v1 for security work, Image v1 for an image, Decide v1 for a decision - with its key and the call" . "Models, and where each one is fetched.

This session's own model. The footer names it; (models), (select-model ...) and (select-effort ...) are in (help :operator). (complete SYSTEM USER) is one side call - SYSTEM the whole instruction, USER the one message, no tools, no history - on the auxiliary model (general.auxiliary_model, else this session's own at its weakest rung); (complete SYSTEM USER :session-id ID) runs on session ID's own model. Bind *auxiliary-model* to send that one call to any configured provider and model: (let ((*auxiliary-model* (list :provider \"nodecode\" :model \"foresight-cyber-v1\"))) (complete SYSTEM USER :fallback-p nil)). Without :fallback-p nil a refused call retries on the session's own model and the answer comes from the wrong one. Binding *provider* or *model* does not move complete.

A model of your own. A provider whose base_url is a machine you run - localhost, a private or tailnet address, a .local or .ts.net name - is listed under Local in /models' Main tab: providers.<id> = {\"sdk\": \"openai-completions\", \"base_url\": \"http://HOST:PORT/v1\"}, its models from GET <base_url>/models.

nodecode.ai's API: Nodecode's own models, at https://api.nodecode.ai/v1, OpenAI's wire, provider id nodecode. `nodecode login` or /login (the operator's to run; signs in at nodecode.ai in a browser) saves the key at auth.json api_keys.nodecode.key and writes providers.nodecode {\"sdk\": \"openai-completions\", \"base_url\": \"https://api.nodecode.ai/v1\"}; a key made at https://nodecode.ai/account/keys works the same. (getf (resolve-provider-credential \"nodecode\") :key) reads it: never print it or put it in a shell command or a file. Keys start with sk-nc-; any other key answers 401 invalid_api_key, and the fix is a new `nodecode login`. Routes: POST /v1/chat/completions, POST /v1/images/generations, GET /v1/models (what the key may call), GET /v1/models/ID, GET /v1/usage (the plan's requests left, its end and quota reset, seats, the credit; the operator's footer and /usage draw it); there is no /v1/messages or /v1/responses. A call is taken from the account's plan month first, then its topped-up balance (https://nodecode.ai/pricing); with neither it answers 402.
- Cyber v1, foresight-cyber-v1: the security model, a chat model (DeepSeek V4.1 Flash 1017 with Foresight Labs finetunes), 1M context, 16K output. A breach, a CVE, a patch or a server or website to check starts at (help :security), the cyber-security cell's manual, whatever model runs the session. Ask Cyber v1 one security question with the complete call above, or make it this session's model: (select-model \"foresight-cyber-v1\" :provider \"nodecode\"), `/models nodecode foresight-cyber-v1` in a shell.
- Image v1, foresight-image-v1: the image model (Qwen-Image 2.1 Turbo with Foresight Labs finetunes). POST /v1/images/generations with {\"model\", \"prompt\", \"n\" 1-4, \"size\" \"WxH\" up to 2048x2048 or \"auto\", \"response_format\" \"b64_json\", \"seed\" optional} answers {\"data\": [{\"b64_json\": ...}], \"size\", \"usage\"}: the images come back in the body, never as URLs. nlk:http answers (values BODY STATUS HEADERS TRUNCATED); check STATUS is 200 before decoding BODY:
  (multiple-value-bind (body status)
      (nlk:http :post \"https://api.nodecode.ai/v1/images/generations\" :timeout 600
                :headers (list (cons \"authorization\" (format nil \"Bearer ~a\" (getf (resolve-provider-credential \"nodecode\") :key)))
                               (cons \"content-type\" \"application/json\"))
                :content (shasht:write-json (json-object \"model\" \"foresight-image-v1\" \"prompt\" PROMPT \"size\" \"1024x1024\" \"response_format\" \"b64_json\") nil))
    (if (/= status 200) body
        (alexandria:write-byte-vector-into-file
         (cl-base64:base64-string-to-usb8-array (gethash \"b64_json\" (aref (gethash \"data\" (nlk:decode-json body)) 0)))
         PATH :if-exists :supersede)))
  then (look PATH), and hand it to a room's answer with nck:answer-file. An image is 30.3 plan requests, or $0.04 from the balance.
- Decide v1, foresight-decide-v1: the classifier (Perplexity Decider Flash 1.2 with Foresight Labs finetunes). It answers named questions about one state as probabilities, never prose. The request rides as JSON in the last user message of a chat completion: {\"state\": TEXT, \"questions\": {ID: QUESTION, ...}}, a QUESTION {\"type\": \"choice\" | \"noul\" (yes or no) | \"score\", \"instructions\": TEXT, \"criteria\": ...}, a choice's criteria {LABEL: RUBRIC or null, ...} with two labels at least. choices[0].message.content is the answer as JSON, {\"answers\": {ID: ...}, \"usage\": ...}: a choice its likeliest label and each label's probability, a noul the probability of yes, a score the probability-weighted level. Every question sees the same state on its own, so independent questions go in one call. 32K context; an image goes as an image_url part with a data: URL. A tenth of a plan request, $0.10 per 1M input tokens, output free:
  (let ((*auxiliary-model* (list :provider \"nodecode\" :model \"foresight-decide-v1\")))
    (nlk:decode-json (complete \"\" (shasht:write-json REQUEST nil) :max-tokens 1024 :fallback-p nil)))

The decider pin. general.decider_model is the decider /models' Decider tab picks: djev (DJev, model dgemma) on a machine of your own, http://127.0.0.1:8011 unless providers.djev.base_url moves it, called as POST <base>/v1/systemone with the same request shape. `/models decider djev` pins it, `/models decider none` takes it out. Decide v1 is the hosted decider; the pin does not offer it.")
    (:docs "the user reference, https://nodecode.ai/docs - what to tell a person asking how to install, sign in, pick a model, connect a channel, change a setting, update or fix Nodecode; every page as markdown at /docs/SLUG.md, the map at /docs/llms.txt" . "The user reference, https://nodecode.ai/docs: how to install, run, set up and fix Nodecode, written for the people who use it, not for its internals.

When someone asks how to do something with Nodecode - install it, sign in, pick a model, connect a channel, change a setting, update, roll back, recover from a failure - read the page that answers it and answer from it, linking https://nodecode.ai/docs/SLUG so they can read on. Every page is also markdown at https://nodecode.ai/docs/SLUG.md (the overview is index.md); https://nodecode.ai/docs/llms.txt is the map and https://nodecode.ai/docs/llms-full.txt all of it in one file. (nlk:http :get URL) answers (values BODY STATUS ...). The reference follows the published release: where a page and this image disagree, this image is the truth - answer from it and say the page is behind.

Pages, in reading order, SLUG - what it answers:
install - One line for Linux, macOS and Windows. What it prints, what it needs, where files land, and the mistakes people make.
quickstart - Install, start, connect a provider, send a request, keep one definition, leave it running.
first-run - The setup wizard: when it opens, what each stage shows, and the usual mistakes.
pricing - How a plan's requests are counted: what one request is, how many a month each plan gives, how many run at once, and when the month starts again. Then the API per token, paid from a balance you top up.
support - What works today on each platform, and what is limited or unfinished.
troubleshooting - The diagnostic commands, the log files, and common failures as symptom then fix.
faq - Short answers to what a newcomer asks first, each with the page that has the detail.
changelog - The release each platform runs on the stable channel, how to see your own build, and what changed.
shell - Keys, composer prefixes, header, footer and turn bar of the terminal UI.
sessions - Session ids, resume, rename, export, fork, the session tree, undo, crash recovery and throwaway stores.
slash-commands - Every built-in and cell slash command, with syntax, aliases and headless support.
cli - Every nodecode command and option, the rules for combining them, and the exit codes.
models - Providers, wire dialects, key lookup, Nodecode's own models, default and auxiliary model, effort, retries and the failover chain.
config - The config file, how to change it, and every core key with type and default.
environment - Every environment variable the nodecode binary and the installers read, by purpose.
files - Every file and folder Nodecode keeps: the home, the cache, and the folder it leaves in each project.
instructions - Which files Nodecode reads into the prompt, where the model finds them, and when an edit takes effect.
profiles - Separate homes under one binary: -p NAME, every nodecode profile verb, /profile and the active-profile file.
updates - The update command, the update config keys, channels, staging, the hot and restart paths, and manual steps.
security - What protects the machine, and what Nodecode does not protect.
gateway - The background gateway: its verbs, what it installs per platform, logout, automatic start and updates.
rpc - Drive Nodecode from a program: one command a line on stdin, one frame a line on stdout.
operations - Choose an owner, inspect it, recover from a crash, apply config changes, and know what survives a restart.
organism - What runs when you type nodecode, what it keeps, and what happens when it stops.
eval - The eval tool, its helpers, and how a definition is kept, held, skipped, inspected and undone.
context - The window budget, eviction, the stub it leaves, /evict, the working context, the recall forms and checkpoints.
cells - What a cell is, where to find one, how to turn one on or off, the hub, how cells load.
cell-api - How a cell is written: nle:define-cell, its clauses, tool contracts, the config section, hooks, notices, stop, and what Nodecode shows when one is written wrong.")
    (:fork "fork: whenever an ask splits into two or more independent investigations that each take several reads, run them at once with (fork \"task 1\" \"task 2\" ...) instead of one after another: each of up to 4 forks of this session holds this whole context, and the call answers their results. Also for a probe whose output would bury this context; never for a linear task or a quick read" . "Forking this turn: one verb of the kernel, called through eval. A refusal is
ERROR: FORK-ERROR with the reason.
  (fork \"task\" ... &key seconds from check keep)
 fork this session at the turn running it, once per task (at most 4), and
 wait for them all. Each fork holds this whole context, works its one task on
 this session's model, and answers. => one plist a task, in order: (:task
 :session :status :answer :ran) -- STATUS :completed, :failed, :cancelled or
 :timeout, ANSWER the fork's last words, RAN the first line of each eval it
 ran. SECONDS bounds each fork's turn (fork.seconds, 900, unless given): past
 it the fork's tools are refused and it answers where it stands.
  :from -N (or a turn id)
 fork N turns back instead of here: the forks hold what this session knew
 when that turn ended, and nothing after it.
  :from G (or (OLD NEW)) :check CHECK, tasks optional
 go back through the generations this home sealed, G to the newest, to the
 step where CHECK turned false. CHECK is a form answering true, e.g.
 (fboundp 'nle::help), or a shell command exiting 0 in this session's
 folder, e.g. \"test -f README.md\". Each generation is laid in a throwaway
 home and asked in a fresh process of today's binary, 4 at once (about a
 minute a round, 6 rounds over a thousand generations); nothing live is
 touched. => (:bisect (:verdict V :range (OLD NEW) :good G :bad G :changes
 LINES :rounds N :tested ROWS :says WORDS) :forks RESULTS): V :found;
 :clean when the newest passes, so the break is outside what a generation
 records; :program when the oldest fails too, so no generation explains it;
 :unanswered when an end could not be asked; :unsteady when the step's two
 generations, asked again, answer otherwise (a check reading what a boot
 varies). Tasks are forked after the search, told what it found.
  :keep NAME :check CHECK
 race versions of NAME, a plain function: fork n writes its version as
 fork-n/NAME, one defun taking every call NAME takes. Each version and what
 stands are put to CHECK in a fresh process; the ones that pass are timed on
 this image's own calls of NAME, one version serving each call; the fastest,
 if 5% faster than what stands (or passing where what stands fails), goes
 live HELD -- written to the layer after one clean call outside an eval,
 (revert 'NAME) putting the old one back until then. Beside :from G, NAME as
 the last good generation had it races too. => (:forks RESULTS :race (:keep
 NAME :standing ROW :versions ROWS :winner LABEL :says WORDS)).
A fork defines only names of its own -- one that already answers is refused --
and nothing it defines is written to the layer: the parent keeps what it chose.
When to fork: work that splits into branches that do not need each other --
a probe beside a measurement, one check over three folders -- or one probe
whose output would bury this context. When not to: a plain linear task, one
read, steps that each need the step before; do those here. Forks run at once
in the same folder, so give each a task that touches nothing another one does.
A task's first line names its fork -- in /sessions, and under the call in the
shell while the forks run -- so open each with a few words saying which it is.
A fork cannot fork, and its answer comes back here, never to a person. A fork
pays for its own work, not this context again: it starts from this session's
cached prefix. A wait past the eval's yield goes on in the background, and its
value arrives when the last fork is done.")
)
  "The topics (help) indexes, each (TOPIC SUMMARY . TEXT): the one-line
summary the index lists and the text the topic answers: :EVAL is the rest
of the eval tool's contract, the description keeping only what a first call
needs. A cell's manual joins through SET-HELP-TOPIC while it runs.
Mutable material.")

;;; The manual stays out of the prompt: every request carries one line per
;;; topic (context.lisp READ-HARNESS-SECTIONS, the `help' section), and a
;;; topic's text is read when a round needs it. Without this, a cell's
;;; primer would ride every request whole, consuming the operator's system
;;; message.

(defun set-help-topic (topic summary &optional text)
  "Put TOPIC on the manual: (help TOPIC) answers TEXT, and the index and the
help section list SUMMARY. A NIL SUMMARY takes TOPIC off. => TOPIC."
  (setf *help-topics* (append (remove topic *help-topics* :key #'car)
                              (and summary (list (list* topic summary text)))))
  topic)

(defun help-topic-lines ()
  "One `  :topic - summary' line per topic: what (help) indexes and the help
section carries."
  (with-output-to-string (out)
    (loop for (topic summary) in *help-topics*
          do (format out "  ~(~s~) - ~a~%" topic summary))))

(defun help-index (out verbs)
  "The (help) index onto OUT: each of VERBS with the first sentence of its
docstring, then the topics. VERBS are the symbols whose documentation is
read."
  (format out "eval is the one tool; (help 'verb) prints a verb's contract, (help :topic) a topic.~%verbs~%")
  (dolist (verb verbs)
    ;; the docstring's first sentence on one line, whitespace collapsed
    (let* ((flat (nlk:one-line
                  (or (and (fboundp verb) (documentation verb 'function))
                      "(no docstring)")))
           ;; A period ends a sentence when it closes a word — " . " is a
           ;; dotted pair in a lambda-list, not a full stop.
           (end (nth-value 1 (ppcre:scan "[^ ]\\.(?= |$)" flat))))
      (format out "  ~(~a~) - ~a~%" verb (string-left-trim " " (subseq flat 0 end)))))
  (format out "topics~%~a" (help-topic-lines)))

(defun help (&optional what)
  "The manual, from inside the image. (help) is the index: every verb with
the first sentence of its contract, then the topics. (help 'verb) is a
verb's whole docstring; (help :topic) is a topic's text; (help 'name) on a
memory or a skill you kept reads it from its file. A name this image does
not know answers the index so the next call can pick from it. Returns the
text."
  (with-output-to-string (out)
    (flet ((index () (help-index out *help-verbs*)))
      (let ((topic (and (or (keywordp what) (stringp what))
                        (assoc what *help-topics* :test #'string-equal))))
        (cond ((null what) (index))
              (topic (write-string (or (cddr topic) "(no text for this topic)") out))
              ((when-let (entry (and (or (stringp what) (and (symbolp what) (not (fboundp what))))
                                     (nlk:knowledge-entry-named what nlk:*scribe-session-id* :data t)))
                 ;; a memory or a prose skill: nothing in the image, read from its file
                 (receipt "calls" (list :verb "help" :family "recall"
                                        :source (nlk:knowledge-entry-name entry)))
                 (nlk:note-knowledge-use (nlk:knowledge-entry-name entry) "view"
                                         :session nlk:*scribe-session-id*)
                 (write-string (nlk:knowledge-help-text entry) out)))
              (t (let ((symbol (if (and (symbolp what) (fboundp what))
                                   what
                                   (find-symbol (string-upcase (string what)) '#:nodecode.evolved))))
                   (when (and symbol (fboundp symbol) (nlk:knowledge-entry-named symbol nlk:*scribe-session-id*))
                     ;; a definition the organism keeps: read, so the index ranks it
                     (nlk:note-knowledge-use symbol "view" :session nlk:*scribe-session-id*))
                   (cond ((not (and symbol (fboundp symbol)))
                          (format out "no help for ~s; the index:~%" what)
                          (index))
                         ((documentation symbol 'function)
                          (format out "~(~a~): ~a~@[~%~a~]" symbol (documentation symbol 'function)
                                  (nlk:definition-prose symbol)))
                         (t (format out "~(~a~) is defined but carries no docstring; (definition-text '~(~a~)) reads its body" symbol symbol))))))))))

;;; --- what the organism keeps and does not run --------------------------
;;; A memory and a prose skill are definitions the scribe files in a
;;; knowledge cell (knowledge.lisp), never loaded: the macros refuse a name
;;; the image already uses and options it does not know, and mark the symbol,
;;; so a code skill's own name can be kept again. The index is one harness
;;; section over everything kept, pinned per conversation (context.lisp
;;; READ-HARNESS-SECTIONS); (index) reads it now, /index shows it.

(nlk:define-section ("index")
  (:guide "The index section at the head of every conversation lists the definitions the organism keeps -- memories, skills, the layer's -- ranked by use.")
  ("hot_budget" :integer :default 8192 :min 500
                :doc "characters of the index below its first line: all described when they fit, else half for descriptions, the rest names")
  ("stale_days" :integer :default 30 :min 1
                :doc "a skill or a layer definition unused this long reads as cold; a memory never does"))

(defun index-settings ()
  "The index section's settings, :hot-budget and :stale-days."
  (nlk:section-settings (nlk:find-section '("index"))
                        (json-value (read-shared-config) :object "index")))

(defun index-text (session-id)
  "The index SESSION-ID reads: what its conversation is pinned with."
  (nlk:knowledge-surface session-id (index-settings)))

(defparameter +memory-types+ '(:feedback :user :project :reference :lesson)
  "The :type a memory takes.")

(defun check-knowledge-name (name operator)
  "Refuse NAME for OPERATOR's definition when the image already uses it --
a function, a macro, a variable, a class, a special operator, a locked
package's symbol -- unless it is a memory or a skill kept before."
  (unless (and name (symbolp name) (not (keywordp name)))
    (error "~(~a~) takes a name first: (~(~a~) name \"one sentence\" ...)" operator operator))
  (unless (get name 'nlk::knowledge)
    (when-let (what (cond ((special-operator-p name) "a special operator")
                          ((macro-function name) "a macro")
                          ((fboundp name) "a function")
                          ((boundp name) "a variable")
                          ((find-class name nil) "a class")
                          ((and (symbol-package name) (sb-ext:package-locked-p (symbol-package name)))
                           "a symbol of a locked package")))
      (error "`~(~a~)' already names ~a in this image; a memory or a skill takes a name of its own"
             name what))))

(defun check-knowledge-form (operator description options &key memory)
  "Refuse a knowledge form whose DESCRIPTION is not a sentence or whose
OPTIONS are not keyword pairs this OPERATOR takes, then at most one string:
:type for a MEMORY, :project, :sources, :cell and :origin for either."
  (unless (and (stringp description) (plusp (length (nlk:trimmed description))))
    (error "~(~a~) takes one sentence after its name, as a string: the line the index lists"
           operator))
  (multiple-value-bind (plist body) (nlk::knowledge-form-options (list* operator nil description options))
    (unless (= (length options) (+ (length plist) (if body 1 0)))
      (error "~(~a~) takes keyword options and then at most one string; ~s is neither"
             operator (nth (length plist) options)))
    (loop for (key value) on plist by #'cddr
          do (dispatch key eq
               (:type (unless (and memory (member value +memory-types+))
                        (error "~:[a skill has no :type~;:type is one of ~{~(~s~)~^ ~}~]"
                               memory +memory-types+)))
               (:project (unless (and (stringp value) (uiop:absolute-pathname-p value))
                           (error ":project is the repository's root as an absolute path, ~
                                   written out: a session under it reads the definition")))
               (:sources (unless (and (listp value) (every #'stringp value))
                           (error ":sources is a list of strings")))
               (:cell (unless (and (stringp value) (ppcre:scan "^[a-z0-9][a-z0-9-]*$" value))
                        (error ":cell names a cell folder: lower case, digits and hyphens")))
               (:origin (unless (and (stringp value) (plusp (length (nlk:trimmed value))))
                          (error ":origin names the world it was kept in, as a string: \"hermes\"")))
               (t (error "~(~a~) takes ~:[~;:type ~]:project :sources :cell :origin; ~s is none of them"
                         operator memory key))))))

(defun mark-knowledge (name kind)
  "NAME marked as knowledge of KIND, :memory or :skill; => NAME."
  ;; A call, so that evaluating a knowledge form compiles nothing: a SETF of
  ;; GET expands to a LET*, which EVAL compiles.
  (setf (get name 'nlk::knowledge) kind)
  name)

(defmacro define-memory (name description &rest options)
  "Keep a fact across sessions: NAME, then DESCRIPTION -- one declarative
sentence, the line the index lists -- then options: :type :feedback for a
preference or a correction the operator stated, :user for who they are,
:project, :reference or :lesson; :project \"ROOT\" for a fact about the
repository at ROOT, read only by sessions under it; :sources (\"session\"
...); :cell \"topic\" for a cell of that one subject; :origin \"world\" for
one another harness kept, which an import writes and the index ranks after
the organism's own until a keep or a helped sighting vouches for it. The why and how to
apply it ride as ;; lines inside the form, the closing paren on a line of
its own. Evaluated as a toplevel form it is one file in your knowledge
cell, committed and never loaded: (help 'name) reads it, (edit path old new)
on its file changes it, (unintern 'name) forgets it."
  (check-knowledge-name name 'define-memory)
  (check-knowledge-form 'define-memory description options :memory t)
  `(mark-knowledge ',name :memory))

(defmacro define-skill (name &rest definition)
  "Keep a procedure: (define-skill name \"one sentence\" ;; when to use ;;
the steps, one per ;; line ...) is prose, one file in your knowledge cell
like a memory, read with (help 'name), taking define-memory's options but
:type, :origin among them; (define-skill name (args) \"one sentence\" body...) is a procedure
this image runs: a defun, kept in your layer."
  (check-knowledge-name name 'define-skill)
  (cond ((null definition)
         (error "define-skill takes one sentence after its name, or a lambda list and a body"))
        ((listp (first definition))
         `(progn (mark-knowledge ',name :skill)
                 (defun ,name ,@definition)))
        (t (check-knowledge-form 'define-skill (first definition) (rest definition))
           `(mark-knowledge ',name :skill))))

(defun index (&optional text)
  "What you keep: (index) is the index a conversation is pinned with now --
the index section at the head of this one is it as of its last pin -- and
(index \"text\") finds the definitions, hot and cold, whose name,
description or file holds text, this project's first."
  (receipt "calls" (list* :verb "index" :family "recall" (and (stringp text) (list :source text))))
  (if (and (stringp text) (plusp (length (nlk:trimmed text))))
      (nlk:knowledge-search text nlk:*scribe-session-id* (index-settings))
      (index-text nlk:*scribe-session-id*)))

(defun index-slash (args session-id &aux (args (string-trim " " (or args ""))))
  "/index: bare, the meter and the picker -- hot first, a tab per cell,
Enter inserting $name; `reload', this conversation pinned again at its
next request; any other text, what (index text) finds."
  (cond ((string= args "")
         (let ((settings (index-settings)))
           (values (nlk:knowledge-meter session-id settings)
                   (let ((rows (nlk:knowledge-rows session-id settings)))
                     (list-dialog
                      :title "Index"
                      :context (nlk:knowledge-meter session-id settings)
                      :tabs (cons (list :label "all" :count (length rows) :value nil)
                                  (loop for label in (nlk:distinct (mapcar (lambda (row) (getf row :key)) rows))
                                        collect (list :label label :value label
                                                      :count (count label rows :key (lambda (row) (getf row :key))
                                                                               :test #'string=))))
                      :searchable t
                      :search-placeholder "Type to filter what is kept…"
                      :empty-label "Nothing kept yet — (help :layer) says how to keep a memory or a skill"
                      :hint "esc close  up/down navigate  enter insert $name  tab cell  type search"
                      :rows rows)))))
        ((string-equal args "reload")
         (nlk:mark-index-refresh session-id)
         "index: pinned again at this conversation's next request")
        (t (nlk:knowledge-search args session-id (index-settings)))))

;;; --- the tool table -----------------------------------------------------
;;; An alist of (name . plist), where the plist carries :schema for the provider
;;; and :function for execution. Mutable material: REGISTER-TOOL is how the
;;; organism gives itself new abilities at run time.

(defparameter *tools* '()
  "Registered tools, newest first. Each entry is (name schema function).")

(defun tool-description (schema)
  "What the provider is told the tool is for: a declared contract's own :DOC,
or the plist's :DESCRIPTION."
  (if (nlk:contract-p schema)
      schema.doc
      (getf schema :description "")))

(defun tool-parameters (schema)
  "The JSON-Schema object the provider is given for the tool's arguments:
projected from the declaration (NLK:CONTRACT-SCHEMA) when the tool declared
one, else the object its registration wrote by hand."
  (if (nlk:contract-p schema)
      (nlk:contract-schema schema)
      (getf schema :parameters)))

(defun tool-body-arguments (entry arguments &aux (schema (second entry)))
  "What a tool's body is called with: the materialized record when the tool
declared a contract, else the decoded ARGUMENTS table.

Materializing HERE -- inside the :TOOL chain, after every hook has had the
arguments -- is what keeps a hook that rewrote them under the same
declaration the preflight refused by; a rewrite the contract does not accept
fails the call closed instead of reaching the body. The chain's own op keeps
the table: it is a dispatch surface over the raw call, not the domain owner
the contract types."
  (if (nlk:contract-p schema)
      (nlk:contract-decode schema arguments)
      arguments))

(defun register-tool (name schema function)
  "Register or replace the tool called NAME. SCHEMA is what the provider is
shown and what a call is held to: an NLK:CONTRACT, the one declaration both
come from, or the plist (:description TEXT :parameters OBJECT) written by
hand, which stays the escape hatch for a schema no declaration carries (the
`eval' snippet's open `strings' object, for one).

With a contract, FUNCTION takes the materialized record -- a call that does
not satisfy the declaration is refused before any hook or body runs, and the
body cannot read a member the declaration does not carry because there is no
reader for one. With a plist, FUNCTION takes the decoded arguments hash
table.

FUNCTION returns a string — or (values STRING SETTLE),
SETTLE a function of no arguments the engine calls on the turn thread once
the body has finished and before the call's result fact records: the place
for whatever the body leaves for the turn's own tables rather than filing it
from the thread it ran on."
  (check-type name string)
  (check-type schema (or list nlk:contract))
  (check-type function (or function symbol))
  (setf *tools* (cons (list name schema function)
                      (remove name *tools* :key #'first :test #'string=)))
  name)

(defun find-tool (name)
  (find name *tools* :key #'first :test #'equal))

(defun unregister-tool (name)
  "Remove the tool called NAME — the cell that registered it is stopping.
=> NAME when one went, NIL when none was there."
  (check-type name string)
  (when (find-tool name)
    (setf *tools* (remove name *tools* :key #'first :test #'string=))
    name))

(defun tool-call-preflight (name raw-arguments entry)
  "Decode and validate one provider tool call before any tool-side work.
Returns (VALUES ARGUMENTS ERROR-TEXT). ERROR-TEXT is a structured refusal
with a repair instruction; a NIL ENTRY or malformed JSON never reaches a
workspace scan, hook, or tool function.

ARGUMENTS is the decoded table either way. A tool that declared a contract is
validated by decoding through it and the record is dropped here: the table is
what the :TOOL chain sees, and TOOL-BODY-ARGUMENTS
materializes the record again at the body, after the hooks. A tool that
registered a schema plist is checked against the small JSON-Schema dialect
those plists are written in -- `type' and `required', which is all a
hand-written schema is read for, and why `enum' went unchecked until a
declaration owned it."
  (flet ((refuse (code expected received repair)
           ;; the machine-readable refusal, returned before a tool body runs
           (return-from tool-call-preflight
             (values nil
                     (format nil "ERROR: tool preflight failed: ~a"
                             (encode-json-object
                              (make-json-object "code" code "tool" (or name "")
                                                "expected" expected "received" received
                                                "repair" repair "retryable" t))))))
         (keys (arguments)
           ;; the bounded, stable key list a refusal shows
           (coerce (sort (loop for key being the hash-keys of arguments
                               when (stringp key) collect key)
                         #'string<)
                   'vector)))
    (unless entry
      ;; A verb of the eval snippet called as a tool of its own (sh, look,
      ;; write-file: 39 DeepSeek calls, 2026-09): the repair is the call it meant.
      (let ((verb (and (stringp name) (find-symbol (string-upcase name) '#:nodecode.evolved))))
        (refuse "unknown_tool" #() #()
                (if (and verb (or (member verb *help-verbs*) (fboundp verb))
                         (find-tool "eval"))
                    (format nil "~a is a function inside eval, not a tool: call eval with {\"form\": \"(~(~a~) ...)\"}"
                            name name)
                    (format nil "choose a registered tool; available tools: ~{~a~^, ~}"
                            (mapcar #'first *tools*))))))
    (let ((arguments
            (with-handlers ((error () :invalid-json))
              (cond ((hash-table-p raw-arguments) raw-arguments)
                    ((stringp raw-arguments) (nlk:decode-json raw-arguments))))))
      (when (eq arguments :invalid-json)
        (refuse "invalid_arguments_json"
                (vector "JSON object")
                (vector (if (stringp raw-arguments) "string" "missing"))
                "send the arguments as a JSON object string"))
      (unless (hash-table-p arguments)
        (refuse "arguments_not_object"
                (vector "JSON object")
                (vector (if (null raw-arguments) "missing" (type-of raw-arguments)))
                "send an object containing the tool parameters"))
      (when (nlk:contract-p (second entry))
        (handler-case (nlk:contract-decode (second entry) arguments)
          (nlk:contract-refusal (refusal)
            (refuse "contract_refused"
                    (vector (nlk:contract-refusal-path refusal)
                            (nlk:contract-refusal-expected refusal))
                    (keys arguments)
                    (format nil "~a; the tool's parameters schema names every ~
member it declares" refusal))))
        (return-from tool-call-preflight (values arguments nil)))
      (let* ((schema (getf (second entry) :parameters))
             (properties (json-value schema :object "properties"))
             (missing (loop for key across (json-array schema "required")
                            unless (nth-value 1 (gethash key arguments))
                              collect key))
             (bad (and properties
                       (loop for key being the hash-keys of properties
                             for type = (json-value (gethash key properties)
                                                    :string "type")
                             for value = (gethash key arguments)
                             ;; the small JSON-schema primitive dialect the tool schemas use
                             for wire = (find (find-symbol (string-upcase type) :keyword)
                                              '(:object :array :string :integer :number))
                             when (and (nth-value 1 (gethash key arguments))
                                       (not (cond (wire (json-value value wire))
                                                  ((string-equal type "boolean")
                                                   (or (eq value t) (null value)))
                                                  ((string-equal type "null") (null value))
                                                  (t t))))
                               collect (list key type)))))
        (when missing
          (refuse "missing_required_argument"
                  (coerce missing 'vector)
                  (keys arguments)
                  (format nil "provide required key~:[s~;~]: ~{~a~^, ~}"
                          (= 1 (length missing)) missing)))
        (when bad
          (refuse "argument_type_mismatch"
                  (coerce bad 'vector)
                  (keys arguments)
                  (format nil "correct argument type~:[s~;~]: ~{~{~a requires ~a~}~^, ~}"
                          (= 1 (length bad)) bad)))
        (values arguments nil)))))

(defun tools-payload (&optional (tools *tools*))
  "Build provider schemas from TOOLS, omitting the key when none are admitted."
  (when tools
    ;; By name, whatever order they were registered in: the schemas sit in the
    ;; prompt prefix ahead of the whole conversation, and registration order is
    ;; this image's history. Without sorting by name, a tool registered again (a
    ;; cell restarted, its file made live) would move to the end, and every
    ;; session would re-read everything behind the tools.
    (coerce (loop for (name schema) in (sort (copy-list tools) #'string< :key #'first)
                  collect (json-object
                           "type" "function"
                           "function" (json-object
                                       "name" name
                                       "description" (tool-description schema)
                                       "parameters" (tool-parameters schema))))
            'vector)))

(defun snapshot-tool-bindings ()
  (loop for (name schema function) in *tools*
        collect (list (copy-seq name) (copy-json-value schema) function)))

;;; --- hooks ------------------------------------------------------------------
;;; The around-chain primitive — *HOOKS*, HOOK, UNHOOK, CALL-HOOKED — lives in
;;; the kernel (organism/hook.lisp): peripherals' correctness depends on it,
;;; which makes it floor material, and the same primitive now wraps any
;;; function name. This file dispatches the :TOOL point; the gateway
;;; dispatches :FRAME.

(defparameter +max-overflow-recoveries+ 8
  "How many rejection-driven eviction steps one provider call may take
before the overflow error stands. Each step drops the oldest retained
terminal turn or half the live turn's rounds, so 8 steps cover a ~256x
overshoot; the bound exists so a 400 that merely pattern-matches the
overflow needles cannot evict an entire session.")

(defparameter +overflow-needles+
  '("context" "prompt is too long" "input token count")
  "What a 400's detail says when the request was over the model's window:
OpenAI's and every compatible server's `maximum context length' and
`context_length_exceeded', Anthropic's `prompt is too long' and `exceed
context limit', Gemini's `input token count ... exceeds', llama.cpp's
`exceeds the available context size'. A bare `token' also read 'max_tokens'
exceeds the model's output limit as an overflow; a bare `too large' reads
`max_tokens is too large'. Neither is kept: an output-limit refusal is no
window's.")

(defun provider-overflow-error-p (condition)
  "Recognize provider context rejection from transport status and detail.
Nil status with overflow needles covers in-stream error frames, which carry
the provider's explanation but no HTTP status."
  (let ((status (provider-error-status condition))
        (detail (string-downcase (provider-error-detail condition))))
    (or (eql status 413)
        (and (or (eql status 400) (null status))
             (some (lambda (needle) (search needle detail)) +overflow-needles+)))))

(defun withheld-images-note (condition count)
  "The text that stands where COUNT images stood in the request CONDITION
failed: what was withheld, which lane refused the request that carried it,
with what status and detail, and what the model can do instead. The
detail is clipped and flattened to one line: the note rides every later
round of the turn."
  (let ((clipped (nlk:clip (substitute #\Space #\Newline
                                       (or (provider-error-detail condition) ""))
                           200)))
    (format nil "[~d image~:p withheld: ~a ~a did not accept the request that carried ~:[them~;it~]~@[ (status ~a)~]~@[: ~a~]. The request was sent again without ~:[them~;it~]. This model may read text only here: work from the file another way, its text, its dimensions, a DOM or OCR, or (select-model ...) one whose input includes image and look again.]"
            count
            (provider-error-provider condition)
            (provider-error-model condition)
            (= count 1)
            (provider-error-status condition)
            (and (plusp (length clipped)) clipped)
            (= count 1))))

(defun context-edge-length-cut-p (context finish-reason usage)
  "True when the round was cut by the context edge rather than by an output
cap the operator asked for: a length/max_tokens finish with no cap of ours
to blame — none was sent, the stream stopped short of it, or the cap was
not ours at all but our own shrink-to-fit arithmetic (SHRUNK-P), which is
the context edge wearing a max_tokens. Without the distinction, an
unclassified edge cut would record a truncated tool call as history and
walk into a doomed eviction. Without the shrunk arm, when window math sets
the cap to 1 and the model returns exactly 1, (< 1 1) would read the cut
as the model honouring a limit we meant rather than the context edge.
Deliberately NOT ceiling evidence — local serving stacks cut at their own
output caps too, and reading a cut as a window ceiling would durably
poison the budget; the cut contributes its usage total as a proven floor
and triggers one recovery, nothing more."
  (and (member finish-reason '("length" "max_tokens") :test #'equal)
       (multiple-value-bind (cap shrunk-p) (effective-max-output-tokens context)
         (let ((output (and usage usage.output-tokens)))
           (or (null cap)
               shrunk-p
               (and (integerp output) (< output cap)))))))

;;; --- the eval tool ------------------------------------------------------

(defparameter *eval-result-limit* 32000
  "Ceiling on a tool result string. An unbounded result becomes the next
prompt, so a form printing a huge structure would otherwise blow the context
window and the token bill. 32,000 characters, not the 8,000 it was:
256 rounds read source through (sh \"sed -n ...\")
71 times and hit the cut 60 times, and the store since the cut began naming
its slice form holds 298 cuts followed by 2 slices — the model re-reads
narrower instead, and a round on a relay costs nine seconds before the
first token. A read of a few hundred lines fits under this ceiling whole;
a grep of the tree still cuts, and should.")

(defun lex-form-structure (text &aux (tokens '())
                                     (depth 0)
                                     (n (length text))
                                     (i 0)
                                     (line 1)
                                     (line-start 0)
                                     (code-end 0)
                                     (code-line 1)
                                     (at-head t)
                                     (escape nil))
  "TEXT's structural tokens as the reader will find them, or the one verdict
the scan refuses: (values TOKENS CODE-END MISSING ESCAPE) on a text the
reader can be handed, (values NIL ERROR-TEXT) otherwise. A token is (:open POS
LINE COLUMN), (:close POS), (:orphan POS LINE), or (:head POS LINE COLUMN
PREVIOUS-CODE-END PREVIOUS-CODE-LINE) for the first code character of a line — the point
where indentation speaks. CODE-END is the index past the last code
character (a trailing comment is not code, so a closer inserted there
lands before it); MISSING is how many \"(\" never close. Strings and their
escapes, ;-comments, nested #|...|#, #\\x literals, |bars| and backslashed
characters are lexed as the reader lexes them, so a paren inside any of
them is not structure. A \")\" with nothing open is an (:orphan POS LINE)
token. ESCAPE is (CHAR POS) for the first \\n, \\t or \\r inside a string
literal, the escape the reader does not have (STRING-ESCAPE-REPORT), or NIL.
The one refusal: a string, |symbol| or #| comment that never ends,
named at its position, because SBCL's END-OF-FILE on it names nothing."
  (labels ((snippet (start)
             (let ((end (min n (+ start 40))))
               (format nil "~a~:[~;...~]" (subseq text start end) (< end n))))
           (fail (control &rest arguments)
             (return-from lex-form-structure
               (values nil (apply #'format nil control arguments))))
           (never-ends (what start)
             (fail "unbalanced form: ~a opened at char ~d never ends: ~a"
                   what start (snippet start)))
           (newline ()
             (incf line)
             (setf line-start (1+ i) at-head t))
           (head ()
             ;; The first code character of this line: where the line's
             ;; indentation is measured, and what code came before it.
             (when at-head
               (push (list :head i line (- i line-start) code-end code-line) tokens)
               (setf at-head nil)))
           (code (end)
             (setf code-end end code-line line))
           (skip-delimited (what start close)
             ;; A string or a |symbol|: CLOSE ends it, backslash escapes,
             ;; newlines inside count lines but open no indentation.
             (loop (incf i)
                   (when (>= i n) (never-ends what start))
                   (let ((c (char text i)))
                     (cond ((char= c #\\)
                            (when (and (char= close #\") (not escape) (< (1+ i) n)
                                       (not (member (char text (1+ i)) '(#\\ #\"))))
                              (setf escape (list (char text (1+ i)) i)))
                            (incf i))
                           ((char= c #\Newline) (incf line) (setf line-start (1+ i)))
                           ((char= c close) (incf i) (return)))))
             (code i)))
    (loop while (< i n)
          do (let ((c (char text i)))
               (case c
                 (#\Newline (newline) (incf i))
                 ((#\Space #\Tab #\Return #\Page) (incf i))
                 (#\; (loop while (and (< i n) (char/= (char text i) #\Newline))
                            do (incf i)))
                 (#\( (head)
                  (push (list :open i line (- i line-start)) tokens)
                  (incf depth)
                  (code (incf i)))
                 (#\) (head)
                  ;; A ")" with nothing open is an orphan: a token the
                  ;; completion drops, never a refusal — the text cannot
                  ;; read with it, and the tree without it is the
                  ;; model's own.
                  (if (zerop depth)
                      (push (list :orphan i line) tokens)
                      (progn (push (list :close i) tokens)
                             (decf depth)))
                  (code (incf i)))
                 (#\" (head) (skip-delimited "string" i #\"))
                 (#\| (head) (skip-delimited "|symbol|" i #\|))
                 (#\\ (head) (code (incf i 2)))
                 (#\# (let ((next (and (< (1+ i) n) (char text (1+ i)))))
                        (cond ((eql next #\\) (head) (code (incf i 3)))
                              ((eql next #\|)
                               ;; A block comment is not code: it neither
                               ;; moves CODE-END nor, when it opens a line,
                               ;; spends that line's head.
                               (let ((start i) (depth 1))
                                 (incf i 2)
                                 (loop (when (>= i n) (never-ends "#| comment" start))
                                       (let ((pair (and (< (1+ i) n)
                                                        (subseq text i (+ i 2)))))
                                         (cond ((equal pair "|#")
                                                (incf i 2)
                                                (when (zerop (decf depth)) (return)))
                                               ((equal pair "#|") (incf i 2) (incf depth))
                                               ((char= (char text i) #\Newline)
                                                (incf line)
                                                (setf line-start (1+ i) at-head t)
                                                (incf i))
                                               (t (incf i)))))))
                              (t (head) (code (incf i))))))
                 (t (head) (code (incf i))))))
    (values (nreverse tokens) code-end depth escape)))

(defun outermost-unclosed (tokens &aux (open '()))
  "The :OPEN token of the outermost paren TOKENS leave open, or NIL."
  (dolist (token tokens)
    (case (first token)
      (:open (push token open))
      (:close (pop open))))
  (car (last open)))

(defun one-line-completion-refusal (text missing tokens)
  "The verdict for a one-line snippet that leaves parens open: what is open,
where it started, and why no closer is inserted."
  (let* ((entry (outermost-unclosed tokens))
         (start (and entry (second entry)))
         (shown (and start
                     (let ((end (min (length text) (+ start 56))))
                       (format nil "~a~:[~;...~]" (subseq text start end)
                               (< end (length text)))))))
    (format nil "~d paren~:p left open and this snippet is one line, so there is no indentation to place a closer by. Closing at the end would not finish the form, it would move everything after it inside~@[ ~a~] — which reads, compiles and means something else. Close it yourself, or write the snippet across lines and the reader places the closers by indentation."
            missing shown)))

(defun complete-form-text (text)
  "(values COMPLETED NOTE), NOTE one bracketed line naming how many closers
went where and which form each site ended, NIL when TEXT balanced as
written and rides untouched; (values NIL ERROR-TEXT) for the two shapes no
completion answers (LEX-FORM-STRUCTURE).

The ARC-AGI-3 trials lost 29 of 289 tool calls to
\"unbalanced form\", and in 25 of them the tree was right and only the
last \")\" was missing; the model never appended it, it rewrote a smaller
form, and the steps shrank with every miss. Counting closers is the one
thing a token sampler does worse than an editor, and every language it
writes fluently carries the tree in its whitespace instead. So the reader
takes the tree from the shape (Parinfer's indent-mode contract, a decade
old): the model's own \")\" are authoritative wherever they stand, the
missing ones land at the end of the last code line before a line indented
at or left of the opener they close, and whatever is still open at the
end closes there. One line of text has one site, the end. A closer never
lands where a later \")\" of the model's would then close nothing: the
written depth after each site bounds what may close there, so a
completion always reads — indentation decides where, never whether. The
mirror miscount, a \")\" with nothing open (one game on the new reader: one
closer too many at the end of a multi-line form, 3 of its first 35
calls), is dropped and named the same way: the text cannot read with it,
and the tree without it is the one the model wrote.

One line of text has one site, the end — and that site is refused.
Indentation decides where a closer goes, and a single line carries none,
so the only place left is after everything, which does not finish the
form: it moves whatever followed the open paren inside it. One game: a
one-line snippet one paren short had the model's own test call closed
into the body of the function it was defining, COMP-LIST3 then called
itself four times per call, and the image died of a fatal heap exhaustion
at action 2 of 5000 with the wrong definition kept. Another game lost a
LABELS form the same way. The model that wrote 63-75% of its snippets
across lines (claude-opus-5, two winning games) needed no completion at
all — 0 repairs, 0 errors — while the one that wrote 100% of them on one
line triggered a repair every eleventh snippet. So a one-line snippet
that leaves parens open is answered, not completed: the count, the
opener, and the reason. That costs a round; a definition silently
reshaped costs a game. Interior sites on a multi-line snippet are
unchanged, and so is dropping a \")\" that closes nothing."
  (multiple-value-bind (tokens code-end missing) (lex-form-structure text)
    (cond ((stringp code-end) (values nil code-end))
          ((and (zerop missing) (notany (lambda (token) (eq :orphan (first token))) tokens))
           (values text nil))
          ;; A one-line snippet has no indentation to read, so the only site is
          ;; the end, and a closer there changes the form instead of ending
          ;; it. Answer instead of guessing.
          ((and (plusp missing)
                (not (find #\Newline text :end (min code-end (length text)))))
           (values nil (one-line-completion-refusal text missing tokens)))
          (t
           (let* ((count (length tokens))
                  ;; The written depth after each token, and its suffix
                  ;; minimum: how many closers may have been inserted before
                  ;; token I without a later ")" of the model's finding
                  ;; nothing to close.
                  (after (make-array count))
                  (bound (make-array count))
                  (depth 0))
             (loop for token in tokens for i from 0
                   do (case (first token)
                        (:open (incf depth))
                        (:close (decf depth)))
                      (setf (aref after i) depth))
             (loop for i from (1- count) downto 0
                   do (setf (aref bound i)
                            (if (= i (1- count))
                                (aref after i)
                                (min (aref after i) (aref bound (1+ i))))))
             (let ((open '())
                   (inserted 0)
                   (sites '()))
               (loop for token in tokens for i from 0
                     do (case (first token)
                          (:open (push token open))
                          (:close (pop open))
                          (:head
                           (destructuring-bind (pos line column previous-end previous-line) (rest token)
                             (declare (ignore pos line))
                             (when (and open (plusp previous-end))
                               (bind ((wanted (loop for entry in open
                                                    while (>= (fourth entry) column)
                                                    count t)) (allowed (- (aref bound i) inserted))
                                      (closing (min wanted allowed)))
                                 (when (plusp closing)
                                   (let ((outer nil))
                                     (dotimes (k closing) (setf outer (pop open)))
                                     (push (list previous-end closing
                                                 (format nil "after line ~d" previous-line)
                                                 (second outer))
                                           sites)
                                     (incf inserted closing)))))))))
               (let ((remaining (- missing inserted)))
                 (when (plusp remaining)
                   (push (list code-end remaining "at end" (second (car (last open))))
                         sites)))
               (setf sites (sort sites #'< :key #'first))
               (let* ((orphans (loop for token in tokens
                                     when (eq :orphan (first token)) collect (rest token)))
                      (completed
                        ;; One walk over the original positions: closers
                        ;; inserted at the sites, orphans skipped.
                        (with-output-to-string (out)
                          (let ((cursor 0)
                                (edits (sort (append (mapcar (lambda (site) (list (first site) :insert (second site)))
                                                             sites)
                                                     (mapcar (lambda (orphan) (list (first orphan) :drop 1))
                                                             orphans))
                                             #'< :key #'first)))
                            (dolist (edit edits)
                              (destructuring-bind (pos kind n) edit
                                (write-string text out :start cursor :end pos)
                                (ecase kind
                                  (:insert (loop repeat n do (write-char #\) out))
                                           (setf cursor pos))
                                  (:drop (setf cursor (1+ pos))))))
                            (write-string text out :start cursor))))
                      (note
                        (flet ((opener (start)
                                 ;; The opener's own line, at most forty
                                 ;; characters: a note stays one line.
                                 (let* ((eol (or (position #\Newline text :start start)
                                                 (length text)))
                                        (end (min eol (+ start 40))))
                                   (format nil "~a~:[~;...~]" (subseq text start end)
                                           (< end (length text))))))
                          (let ((closed
                                  (cond ((null sites) nil)
                                        ((= 1 (length sites))
                                         (destructuring-bind (pos n where start) (first sites)
                                           (declare (ignore pos))
                                           (format nil "closed ~d paren~:p ~a: ~a" n where (opener start))))
                                        (t (format nil "closed ~d parens: ~{~a~^; ~}" missing
                                                   (loop for (pos n where start) in sites
                                                         collect (format nil "~d ~a: ~a" n where (opener start)))))))
                                (dropped
                                  (and orphans
                                       (format nil "dropped ~d paren~:p that closed nothing~{ at char ~d (line ~d)~}"
                                               (length orphans)
                                               (loop for (pos line) in orphans append (list pos line))))))
                            (format nil "[reader ~@[~a~]~:[~;, ~]~@[~a~]]" closed (and closed dropped) dropped)))))
                 (values completed note))))))))

(defun string-escape-report (text)
  "The first backslash escape inside one of TEXT's string literals that the
Lisp reader does not have — any but \\\\ and \\\": \\n, \\t, a regex's \\|
or \\. — as one bracketed line, or NIL when there is none. In a Lisp
string a backslash only quotes the next character, so \"a\\nb\" reads as
anb: the newline the model meant would be gone without a sound, and the miss
would surface later as a file with no line breaks or a shell command run as one word. Lexed
by LEX-FORM-STRUCTURE — comments, |symbols| and #\\x literals
are not strings — and \"\\\\n\", an escaped backslash before an n, is exactly
the two characters it says."
  (destructuring-bind (&optional escape position)
      (nth-value 3 (lex-form-structure text))
    (when escape
      (format nil "[reader: \"\\~a\" at char ~d is inside a string literal, and in a Lisp string a backslash only escapes the next character - it reads as a plain ~a. A backslash the text must keep is written twice, \"\\\\~a\"; a line break is ~~% in (format nil \"...\") or a real newline; or send the text beside the form as a strings entry read by ($ \"name\")]"
              escape position escape escape))))

(defun reader-lines (printed)
  "The bracketed [reader ...] lines PRINTED opens with — what the reader
closed or dropped, the escape it does not have — each ending in a newline,
or the empty string: the hints an error result leads with, since an error
answers its own text and not what the snippet printed."
  (with-output-to-string (out)
    (loop for start = 0 then (1+ end)
          for end = (or (position #\Newline printed :start start) (length printed))
          while (and (< start (length printed)) (uiop:string-prefix-p "[reader" (subseq printed start end)))
          do (write-line (subseq printed start end) out))))

(defun qualified-symbol-name (symbol &aux (package (symbol-package symbol)))
  "SYMBOL as a form must write it from any package: PACKAGE:NAME, two
colons when internal."
  (if (null package)
      (format nil "#:~a" (symbol-name symbol))
      (format nil "~a~:[::~;:~]~a" (package-name package)
              (eq :external (nth-value 1 (find-symbol (symbol-name symbol) package)))
              (symbol-name symbol))))

(defun condition-symbol-hint (condition)
  "One line locating the name a failing form reached for, or NIL when
CONDITION names none. \"undefined function\" and \"not found in the
COMMON-LISP package\" are both true and neither says whether the thing
exists at all. The hint does: which packages define a symbol of exactly
that name and whether it is reachable unqualified from the eval snippet, else
defined names containing it, else that nothing by that name is defined
anywhere. Defined means fbound, bound or a class — the reader interns the
very name it then fails to call, so mere presence proves nothing."
  (let ((name (typecase condition
                (cell-error (let ((missing (cell-error-name condition)))
                              (and (symbolp missing) (symbol-name missing))))
                (sb-int:simple-reader-package-error
                 ;; (NAME PACKAGE) is a symbol missing from a package that
                 ;; exists; (PACKAGE) alone is a package that does not, and
                 ;; the report already says so.
                 (let ((arguments (simple-condition-format-arguments condition)))
                   (and (= 2 (length arguments)) (stringp (first arguments))
                        (first arguments)))))))
    (when (and name (plusp (length name)))
      (flet ((kind (symbol)
               ;; What the name means, or NIL: the reader interns the very
               ;; name it then fails to call, so mere presence proves nothing.
               (and (not (keywordp symbol))
                    (cond ((macro-function symbol) "macro")
                          ((fboundp symbol) "function")
                          ((boundp symbol) "variable")
                          ((find-class symbol nil) "class")))))
        (let* ((exact (loop for package in (list-all-packages)
                            for symbol = (find-symbol name package)
                            when (and symbol (eq (symbol-package symbol) package)
                                      (kind symbol))
                              collect symbol))
               (here (find-symbol name (find-package '#:nodecode.evolved)))
               ;; A class or variable of the exact name does not answer a
               ;; form that called it: the defined names containing it ride
               ;; along unless an exact function already does.
               (similar (unless (some #'fboundp exact)
                          (let ((found (loop for symbol in (remove-duplicates
                                                            (apropos-list name))
                                             when (and (kind symbol)
                                                       (not (member symbol exact)))
                                               collect symbol)))
                            (subseq found 0 (min 5 (length found)))))))
          (flet ((described (symbols)
                   (mapcar (lambda (symbol)
                             (format nil "~a (~a)" (qualified-symbol-name symbol)
                                     (kind symbol)))
                           symbols)))
            (cond (exact
                   (format nil "hint: ~a is ~{~a~^, ~}~:[~; — reachable unqualified from here~]~@[; similar: ~{~a~^, ~}~]"
                           name (described exact) (and here (member here exact))
                           (and similar (described similar))))
                  (similar
                   (format nil "hint: nothing named ~a is defined; similar: ~{~a~^, ~}"
                           name (described similar)))
                  (t (format nil "hint: nothing named ~a is defined in any package"
                             name)))))))))

(defparameter *value-print-ladder* '((8 200) (6 100) (5 50) (4 25) (3 12) (2 6) (1 3))
  "The (*PRINT-LEVEL* *PRINT-LENGTH*) pairs PRINT-VALUE-BUDGETED tries, widest
first, for a value whose plain print overflows its budget.")

(defun elided-part-forms (value &aux (parts '()))
  "The forms that read the bulky parts of VALUE from * in the next snippet, up
to three, each with the size of the part's print: (getf * :key) for a
plist, (nth i *) for a proper list, (aref * i) for a vector. NIL when no
part is bulky past *SNIPPET-BRIEF-LIMIT* or the shape names nothing — the
whole is then what to read, in slices. LIST-LENGTH guards the walk: a
circular value prints under *PRINT-CIRCLE* and must not be walked."
  (flet ((note (form part)
           (let ((size (length (format nil "~s" part))))
             (when (> size *snippet-brief-limit*)
               (push (cons form size) parts)))))
    (let ((count (and (listp value) (ignore-errors (list-length value)))))
      (cond ((and (integerp count) (plusp count) (evenp count)
                  (loop for (key nil) on value by #'cddr always (keywordp key)))
             (loop for (key part) on value by #'cddr
                   do (note (format nil "(getf * ~s)" key) part)))
            ((and (integerp count) (plusp count))
             (loop for part in value for i from 0
                   do (note (format nil "(nth ~d *)" i) part)))
            ((and (vectorp value) (not (stringp value)))
             (loop for part across value for i from 0
                   do (note (format nil "(aref * ~d)" i) part))))))
  (let ((largest (sort parts #'> :key #'cdr)))
    (subseq largest 0 (min 3 (length largest)))))

(defun multi-line-string-p (value)
  "Whether VALUE is a string with a newline in it — a file, a listing, a
command's output — read as text, not as a literal."
  (and (stringp value) (find #\Newline value) t))

(defun text-block (string)
  "STRING as the model reads a file: a header naming it as text with its
size, then the text itself — no quotes, no escapes. Printed as a literal,
every quote in a source file arrives as \\\" and every backslash doubled;
Without this, an escaped OLD pasted into (edit ...) would not match."
  (format nil "[text: ~:D line~:p, ~:D characters]~%~a"
          (+ (count #\Newline string)
             (if (char= #\Newline (char string (1- (length string)))) 0 1))
          (length string) string))

(defun print-value-budgeted (value budget)
  "VALUE's printed text within BUDGET characters: the plain print when it
fits. Otherwise a string is cut with its closing quote kept, and any other
value prints under the widest pair of *VALUE-PRINT-LADDER* that fits, so
the model reads an elided tree — ... for a cut tail, # for a cut depth —
where a character cut handed back an unbalanced blob (the ARC-AGI-3 trials
re-fetched every one of those). One leading bracket names the full size,
the settings, and the form that reads what the print lost: the value is
not gone with the call, it is * in the next snippet — (subseq * n) reads on
from where a string was cut, and a tree's bulky parts are named by the
form that reads each, (getf * :key), (nth i *), (aref * i)."
  (let ((plain (if (multi-line-string-p value)
                   (text-block value)
                   (format nil "~s" value))))
    (cond ((<= (length plain) budget) plain)
          ((multi-line-string-p value)
           (let ((keep (min (length value) (max 0 (- budget 200)))))
             (format nil "[value elided: a text of ~:D characters, ~:D lines, the first ~:D characters shown; (subseq * ~D) reads on in your next snippet]~%~a"
                     (length value) (count #\Newline value) keep keep
                     (subseq value 0 keep))))
          ((stringp value)
           (let ((keep (min (length value) (max 0 (- budget 160)))))
             (format nil "[value elided: a string of ~:D characters, the first ~:D shown; (subseq * ~D) reads on in your next snippet]~%~s"
                     (length value) keep keep (subseq value 0 keep))))
          (t
           (let ((where (if-let (parts (elided-part-forms value))
                          (format nil "~{~a is ~:D characters~^, ~}"
                                  (loop for (form . size) in parts
                                        collect form collect size))
                          "(nth i *) or (subseq * a b) reads a part")))
             (loop for (level length) in *value-print-ladder*
                   for text = (let ((*print-level* level) (*print-length* length))
                                (format nil "~s" value))
                   when (or (<= (length text) (- budget 160))
                            (equal (list level length) (car (last *value-print-ladder*))))
                     return (format nil "[value elided: ~:D characters in full, shown at *print-level* ~d *print-length* ~d; the whole value is * in your next snippet: ~a]~%~a"
                                    (length plain) level length where text)))))))

(defvar *eval-strings* nil
  "The executing eval call's strings object — entry name to literal
text — re-bound on whichever thread evaluates the form (the background
snippet's worker included, the way the ambient turn identity rides). NIL when
the call carried none. Read through the $ verb (exec.lisp).")

;;; --- the REPL's history variables -------------------------------------------
;;; Nothing the printer withholds is gone with the call. The values of a
;;; session's last three finished snippets are bound around every snippet of that
;;; session as the standard REPL variables — * ** *** the last value of each,
;;; / // /// the list of each snippet's values, one per form — and every cut the
;;; printer makes names the form that reads the rest from there.
;;; Without this, the bracket would say "ask for a slice next time", and there
;;; would be nothing to slice from: the store would hold elided results and
;;; not one would ever be followed by a slice.

(defvar *snippet-histories* (make-hash-table :test #'equal :synchronized t)
  "Session id -> the values of its last finished snippets, newest first, each
a list of one value per form. A snippet that failed leaves it as it was, the
REPL's own rule.")

(defparameter *snippet-history-depth* 3
  "How many finished snippets a session's history keeps: the three the REPL
variables can name.")

(defun snippet-history (key)
  "KEY's history, newest snippet first."
  (gethash key *snippet-histories*))

(defparameter *snippet-brief-limit* 200
  "Characters a non-final value of a snippet may take on its line. The snippet's
last value prints in full; the ones before it are for the eye.")

(defun print-value-brief (value &optional place)
  "VALUE on one short line: its plain print when it fits *SNIPPET-BRIEF-LIMIT*;
else, when the print has more than one line, its first line — a print that
leads with a summary line has one, and it stays — cut to the limit and
followed by what was elided; else its size and type. PLACE, when given, is
the value's place among the snippet's forms from 0, and the elision names
(nth PLACE /), which reads it whole in the next snippet. A snippet that saves a
grid and then acts on it wants the act's answer, not the grid — one game on
the new reader: five of eight elided results were a SETF's side value,
8,388 characters each, shown as a 25x25 corner nobody asked for. The
IPython convention, only the last expression shows, is right for inert
data and wrong for evidence: in the plan-tree ARC run 44% of one game's act
observations printed as their size because a read followed the act in the
snippet, and levels, actions and the action pairing left the one place the
model reads. A one-line print past the limit keeps the size form: cut
mid-structure it would be the unbalanced blob PRINT-VALUE-BUDGETED
replaced."
  (let* ((plain (format nil "~s" value))
         (newline (position #\Newline plain))
         (there (if place
                    (format nil "; (nth ~d /) reads it in your next snippet" place)
                    "")))
    (cond ((<= (length plain) *snippet-brief-limit*) plain)
          (newline
           (let ((line (subseq plain 0 (min newline *snippet-brief-limit*))))
             (format nil "~a~:[~;...~] [~:d more characters elided: only the snippet's last value prints in full~a]"
                     line (> newline *snippet-brief-limit*) (- (length plain) (length line)) there)))
          (t
           (format nil "[~:d characters, ~s, elided: only the snippet's last value prints in full~a]"
                   (length plain) (type-of value) there)))))

(defun eval-session-id ()
  "The session of the turn whose snippet is being evaluated, or NIL outside a
turn."
  (and *current-durable-turn*
       (nlk:durable-turn-session-id *current-durable-turn*)))

;;; --- the note a snippet opens with ------------------------------------------
;;; PRO-LONG (2607.20064) logs, per action, the agent's plan beside the action
;;; and its outcome, and the agent parses that log back; its ablation shows
;;; the plan line is what the log is worth. Here a snippet's plan is the comment
;;; it opens with — the one place a model writing no prose still writes a
;;; sentence — and the snippet's record (SNIPPET-RECORD, below the dispatch seam)
;;; carries it on the result fact, so the eviction stub
;;; can keep it after the round that carried the snippet is gone.

(defparameter *snippet-note-limit* 240
  "How many characters of a snippet's note the record keeps — one sentence,
cut with a disclosure past this, since the eviction stub prints every note
it carries on one line.")

(defvar *snippet-note* nil
  "The current eval's parsed leading comment, shared by its record and live announcements.
Bound inside EVAL-FORM-PRINTED, including background evaluations; NIL outside it.")

(defun snippet-note (text &aux (lines '()))
  "The note a snippet opens with: its leading comment lines — each a line whose
first non-blank character is a semicolon — with the semicolons and the
blanks around them stripped and the lines joined by one space, or NIL when
the snippet opens with a form. A comment after the first form is not a note:
the note is what the model says before the snippet acts, and the eviction
stub carries it once the round that carried the snippet has left the window."
  (with-input-from-string (in text)
    (loop for raw = (read-line in nil)
          while raw
          do (let ((line (string-trim '(#\Space #\Tab #\Return) raw)))
               (cond ((zerop (length line)))
                     ((char= (char line 0) #\;)
                      (let ((body (string-trim '(#\Space #\Tab)
                                               (string-left-trim ";" line))))
                        (when (plusp (length body)) (push body lines))))
                     (t (return))))))
  (when lines
    (stub-clip (format nil "~{~a~^ ~}" (nreverse lines)) *snippet-note-limit*)))

;;; --- the snippets the image watches ------------------------------------------
;;; A snippet that only read stays open. The image runs it again after each later
;;; snippet of its session, and the model sees it again only when its value
;;; changed, as one line: [watched FORM: OLD -> NEW], or for a long value the
;;; places that changed (WATCH-CHANGES). What a model reads again
;;; and again by hand -- a count, a level, a plan, its own rule as a predicate
;;; held against what the world did -- the image reads for it, and the model
;;; pays for the reading that moved and no other.
;;;
;;; Nothing marks a snippet to watch: the image observes what the snippet did. Its
;;; last form is watched when it finished inside *WATCH-SECONDS* and inside
;;; its eval call -- an evaluation the call handed to the background is read
;;; once, by its exit (SNIPPET-BACKGROUNDED-P) -- its value printed whole inside *WATCH-SIZE*, it names something
;;; that can change (a symbol beyond COMMON-LISP), and it reached nothing
;;; outside the image: it names nothing that assigns, destroys or defines, no
;;; verb stamped it (RECEIPT-CALL), it kept no definition and no hook, and its
;;; thread made no read or write system call, which every act on a socket, a
;;; file or a process takes (THREAD-IO-CALLS, Linux's count per thread; where
;;; the platform keeps none, nothing is watched). A run again cannot act
;;; either: a verb refuses inside it, and a run that reached outside anyway is
;;; closed and says so. A snippet that reached outside -- an act, a definition,
;;; a failure -- runs its session's watched snippets after itself.
;;;
;;; Retention: the watched snippets are live-only, *WATCH-LIMIT* per session. What
;;; each showed rides the result fact of the snippet that showed it, under
;;; "watched", and the eviction stub projects the newest value of each.

(defparameter *watch-limit* 8
  "How many snippets a session watches; one more closes the one that has gone
longest without changing.")

(defparameter *watch-width* 120
  "Characters a watched value shows whole in, OLD -> NEW; past it a change
shows the places that changed (WATCH-CHANGES).")

(defparameter *watch-size* 2000
  "Characters a watched value may print in. A snippet whose value prints longer
is read once.")

(defparameter *watch-seconds* 0.2
  "How long a snippet may take and be watched. A run again past five times
this closes it.")

(defparameter *watch-refused-symbols*
  '(setf setq psetf psetq incf decf push pop pushnew rotatef shiftf remf set
    makunbound fmakunbound remhash clrhash rplaca rplacd nconc delete
    delete-if delete-if-not delete-duplicates nreverse sort stable-sort fill
    replace map-into vector-push vector-push-extend in-package eval load)
  "Names a watched snippet may not mention: each assigns, destroys what it is
handed, or runs code the form does not show.")

(defstruct (watch (:copier nil))
  "One watched snippet: TEXT, its form as written, on one line; FORM, as read;
THUNK, the form compiled at its first run again; VALUE, as it printed last,
and OBJECT, a copy of what it printed; CHANGED, the tick it last changed on."
  (text "" :type string)
  (form nil)
  (thunk nil)
  (value "" :type string)
  (object nil)
  (changed 0 :type integer))

(nlk:access (watch watch) (stale watch))

(defvar *watches* (make-hash-table :test #'equal :synchronized t)
  "Session id -> the snippets it watches, newest first.")

(defvar *watches-lock* (bt2:make-lock :name "watches")
  "Held over each read-modify-write of a session's watched snippets: a
background snippet and the turn's next one can finish together.")

(defvar *watch-tick* 0
  "The clock CHANGED reads: one tick per watched value shown.")

(defvar *snippet-touched* nil
  "True once the evaluating snippet reached outside the image. Bound per snippet
by EVAL-FORM-PRINTED, set by EVALUATE-SNIPPET-FORM and RECEIPT-CALL.")

(defvar *watch-running* nil
  "True while a watched snippet runs again, where a verb refuses (RECEIPT-CALL).")

(defun thread-io-calls ()
  "The read and write system calls this thread has made, Linux's count per
thread, or NIL where the platform keeps none. Its own read is one of them,
counted by the next call."
  #-linux nil
  #+linux
  (let ((fd (sb-unix:unix-open "/proc/thread-self/io" sb-unix:o_rdonly 0))
        (buffer (make-array 256 :element-type '(unsigned-byte 8))))
    (when fd
      (let* ((count (unwind-protect
                         (sb-sys:with-pinned-objects (buffer)
                           (sb-unix:unix-read fd (sb-sys:vector-sap buffer) 256))
                      (sb-unix:unix-close fd)))
             (text (and count (plusp count)
                        (sb-ext:octets-to-string buffer :end count :external-format :latin-1))))
        (flet ((field (name)
                 (let ((at (and text (search name text))))
                   (and at (parse-integer text :start (+ at (length name)) :junk-allowed t)))))
          (let ((reads (field "syscr:"))
                (writes (field "syscw:")))
            (and reads writes (+ reads writes))))))))

(defun watchable-form-p (form)
  "Whether FORM may be watched: it names something that can change -- a
symbol beyond COMMON-LISP -- and nothing *WATCH-REFUSED-SYMBOLS* lists,
nothing that defines, and none of the REPL's history variables, which move
under every snippet."
  (let ((moving nil)
        (budget 4096)
        (homes (list (find-package '#:common-lisp) (find-package '#:keyword))))
    (labels ((note (symbol variable)
               (let ((name (symbol-name symbol)))
                 (cond ((or (member symbol *watch-refused-symbols*)
                            (eql 0 (search "DEFINE-" name))
                            (and (eq (symbol-package symbol) (first homes))
                                 (eql 0 (search "DEF" name)))
                            (and variable (member symbol '(* ** *** / // ///))))
                        (return-from watchable-form-p nil))
                       ((not (member (symbol-package symbol) homes))
                        (setf moving t)))))
             (walk (x quoted)
               (when (minusp (decf budget))
                 (return-from watchable-form-p nil))
               (typecase x
                 (symbol (note x (not quoted)))
                 (cons (let ((quoted (or quoted (member (car x) '(quote function)))))
                         (if (symbolp (car x)) (note (car x) nil) (walk (car x) quoted))
                         (loop for tail = (cdr x) then (cdr tail)
                               while (consp tail) do (walk (car tail) quoted)
                               finally (when tail (walk tail quoted))))))))
      (walk form nil)
      moving)))

;;; --- a cut-off read runs again ---------------------------------------------
;;; A restart that cuts a snippet off mid-run closes its call as interrupted
;;; (RECOVER-INTERRUPTED-TURNS), and the model, resumed, decides what to do
;;; about a call that may have half-acted. A snippet that only reads cannot
;;; have half-acted: its turn runs it again before the provider is asked
;;; (CLAIM-TURN-REPLAYS), as Pi Durable reruns a tool that declares a rerun
;;; safe. Here the form is what declares it: every operator it calls is
;;; COMMON-LISP's own and none of the ones that act, or one of the image's
;;; readers. Nothing is inferred from a run: the cut-off run never finished.

(defparameter *replay-operators* '(help index recall-log recall-entry list-evals $)
  "Operators beyond COMMON-LISP a snippet may call and still run again when
the restart that cut it off resumes its turn (REPLAY-SAFE-SNIPPET-P): each
only reads. Mutable material: a reader of the layer's own joins it with
PUSHNEW.")

(defparameter *replay-refused-operators*
  '(open with-open-file delete-file rename-file ensure-directories-exist
    compile-file compile funcall apply multiple-value-call progv require
    make-package delete-package rename-package import export unexport unintern
    shadow shadowing-import use-package unuse-package set-macro-character
    set-dispatch-macro-character)
  "COMMON-LISP operators a replayed snippet may not call, beyond what a
watched one may not (*WATCH-REFUSED-SYMBOLS*): each reaches the file system,
changes a package or the reader, or calls what the form does not name.")

(defun replay-safe-form-p (form)
  "Whether FORM, a toplevel form of a snippet a restart cut off, only reads:
every operator it calls is one of *REPLAY-OPERATORS*, or COMMON-LISP's and
none that defines, assigns or acts (*WATCH-REFUSED-SYMBOLS*,
*REPLAY-REFUSED-OPERATORS*). Quoted data is data, and the names a binding
form binds are no calls; anything the walk does not know is refused."
  (let ((budget 4096)
        (common (find-package '#:common-lisp)))
    (labels ((operator-p (symbol)
               (or (member symbol *replay-operators*)
                   (and (eq (symbol-package symbol) common)
                        (not (member symbol *watch-refused-symbols*))
                        (not (member symbol *replay-refused-operators*))
                        (not (eql 0 (search "DEF" (symbol-name symbol)))))))
             (forms (list) (every #'walk list))
             (walk (x)
               (when (minusp (decf budget))
                 (return-from replay-safe-form-p nil))
               (typecase x
                 (cons (and (ignore-errors (list-length x)) (call x)))
                 (t t)))
             (bindings (list)
               (and (listp list)
                    (every (lambda (binding)
                             (or (symbolp binding)
                                 (and (consp binding) (symbolp (car binding))
                                      (ignore-errors (list-length binding))
                                      (forms (cdr binding)))))
                           list)))
             (lambda-body (tail)
               (and (consp tail) (bindings (car tail)) (forms (cdr tail))))
             (clauses (list &optional keyed)
               ;; COND's clauses are forms; CASE's and HANDLER-CASE's open on
               ;; keys or a type and a variable list, which are no calls.
               (every (lambda (clause)
                        (and (consp clause) (ignore-errors (list-length clause))
                             (if keyed
                                 (forms (if (eq keyed :handler) (cddr clause) (cdr clause)))
                                 (forms clause))))
                      list))
             (call (x &aux (head (car x)))
               (case head
                 (quote t)
                 (function (let ((name (second x)))
                             (if (symbolp name)
                                 (operator-p name)
                                 (and (consp name) (eq 'lambda (car name)) (lambda-body (cdr name))))))
                 ((let let*) (and (bindings (second x)) (forms (cddr x))))
                 (lambda (lambda-body (cdr x)))
                 ((dolist dotimes) (and (consp (second x)) (forms (cdr (second x))) (forms (cddr x))))
                 ((multiple-value-bind destructuring-bind)
                  (and (listp (second x)) (forms (cddr x))))
                 (cond (clauses (cdr x)))
                 ((case ecase ccase typecase etypecase ctypecase)
                  (and (walk (second x)) (clauses (cddr x) t)))
                 (handler-case (and (walk (second x)) (clauses (cddr x) :handler)))
                 (t (cond ((and (consp head) (eq 'lambda (car head)))
                           (and (lambda-body (cdr head)) (forms (cdr x))))
                          ((and (symbolp head) (operator-p head)) (forms (cdr x)))
                          (t nil))))))
      (walk form))))

(defun replay-safe-snippet-p (arguments)
  "Whether an eval call's ARGUMENTS, its JSON as the model sent it, carry a
snippet whose every toplevel form only reads (REPLAY-SAFE-FORM-P), read as
the eval tool reads it (SNIPPET-FORMS): one a restart cut off may run again."
  (let* ((decoded (ignore-errors (nlk:decode-json arguments)))
         (text (and (hash-table-p decoded) (gethash "form" decoded)))
         (forms (and (stringp text) (snippet-forms text))))
    (and forms (every #'replay-safe-form-p forms) t)))

(defun watch-value-text (value)
  "VALUE as a watched snippet keeps it, printed. The second value is true when
it printed whole inside *WATCH-SIZE* and readably: a value worth watching."
  (let ((text (let ((*print-length* 512) (*print-level* 12) (*print-lines* nil) (*print-pretty* nil))
                (prin1-to-string value))))
    (values text (and (<= (length text) *watch-size*) (not (search "#<" text))))))

(defun watch-brief (text)
  "A printed value's TEXT as one line cut at *WATCH-WIDTH*."
  (nlk:one-line text :cap *watch-width* :ellipsis "…"))

(defun watch-print (object)
  "OBJECT printed for a change line, as a watched value prints."
  (values (watch-value-text object)))

(defun watch-changes (old new &optional path)
  "What changed from OLD to NEW, one entry per place: two lists of one
length are held place by place, a place named by its index path [i j], and
an element too long to show whole is opened the same way; two lists of
different lengths say what came (+) and what went (-); anything else reads
old -> new."
  (let ((at (and path (format nil "[~{~d~^ ~}] " (reverse path)))))
    (cond ((equal old new) nil)
          ((not (and (alexandria:proper-list-p old) (alexandria:proper-list-p new)))
           (list (format nil "~@[~a~]~a -> ~a" at (watch-print old) (watch-print new))))
          ((= (length old) (length new))
           (loop for a in old
                 for b in new
                 for i from 0
                 unless (equal a b)
                   append (if (and (consp a) (consp b)
                                   (> (length (watch-print a)) *watch-width*))
                              (watch-changes a b (cons i path))
                              (watch-changes-at a b (cons i path)))))
          (t (let ((came (remove-if (lambda (x) (member x old :test #'equal)) new))
                   (went (remove-if (lambda (x) (member x new :test #'equal)) old)))
               (list (format nil "~@[~a~]~@[+~a~]~:[~; ~]~@[-~a~]"
                             at (and came (watch-print came)) (and came went)
                             (and went (watch-print went)))))))))

(defun watch-changes-at (old new path)
  "The one entry of a place PATH that went from OLD to NEW."
  (list (format nil "[~{~d~^ ~}] ~a -> ~a" (reverse path) (watch-print old) (watch-print new))))

(defun watch-change-text (watch text object)
  "How WATCH changing to TEXT, OBJECT printed, reads on its line, never cut
short of what changed: a short new value whole after the old one's brief;
else the places that changed, when that says it in fewer characters than
the new value and in at most *WATCH-WIDTH* times four; else the new value
whole, `now ...'."
  (if (<= (length text) *watch-width*)
      (format nil "~a -> ~a" (watch-brief watch.value) (nlk:one-line text))
      (let ((places (format nil "~{~a~^; ~}" (watch-changes watch.object object))))
        (if (and (plusp (length places))
                 (< (length places) (min (length text) (* 4 *watch-width*))))
            (nlk:one-line places)
            (format nil "now ~a" (nlk:one-line text))))))

(defun watch-label (watch)
  "WATCH's form as its lines name it."
  (nlk:one-line watch.text :cap 60 :ellipsis "…"))

(defun watch-session ()
  "The session the evaluating snippet belongs to, whose snippets it watches, or
NIL outside one: a driver's own forms and a slash op watch nothing."
  (and *current-durable-turn* (nlk:durable-turn-session-id *current-durable-turn*)))

(defun watched-text (text entry value started &optional acted)
  "TEXT, a snippet's result, followed by what its session's watched snippets say.
ENTRY is the snippet's last form and its text as read, VALUE its value and
STARTED when it began; ACTED is whether a form before it reached outside
the image. A failed snippet passes no ENTRY. A last form that only read is
watched from now on and says so -- the model's reader is most often a
definition and its call, in one snippet -- and a snippet that reached outside,
before that form or in it, first runs the watched snippets again, each that
changed or closed adding its line."
  (let ((session (watch-session)))
    (if (null session)
        text
        (bind (((shown whole)
                (and entry (not *snippet-touched*) (not (snippet-backgrounded-p))
                     (< (- (get-internal-real-time) started)
                        (* *watch-seconds* internal-time-units-per-second))
                     (watchable-form-p (car entry))
                     (handler-case (watch-value-text value) (error () nil))))
               (line (and whole (form-source-line (cdr entry)))))
          ;; The form watched again is not run again: its value is the result.
          (format nil "~a~{~%~a~}" text
                  (append (when (or acted (not whole)) (run-watches session line))
                          (when whole
                            (watch-snippet session line (car entry) shown (copy-tree value)))))))))

(defun form-source-line (bytes)
  "BYTES, a form's text as READ-SNIPPET-FORMS keeps it, on one line without the
blanks and the comment lines the snippet opened with: the form as written."
  (let ((start 0))
    (loop (setf start (or (position-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return)))
                                           bytes :start start)
                          (length bytes)))
          (if (and (< start (length bytes)) (char= #\; (char bytes start)))
              (setf start (or (position #\Newline bytes :start start) (length bytes)))
              (return (nlk:one-line (subseq bytes start)))))))

(defun watch-snippet (session line form value object)
  "Watch FORM, written as LINE, for SESSION, its VALUE as it printed and
OBJECT what printed so. => the lines that say so, the cap's closing first."
  (let* ((watch (make-watch :text line :form form :value value :object object
                            :changed (incf *watch-tick*)))
         (stale nil))
    (bt2:with-lock-held (*watches-lock*)
      (let ((watches (remove line (gethash session *watches*) :key #'watch-text :test #'string=)))
        (when (>= (length watches) *watch-limit*)
          (setf stale (reduce (lambda (a b) (if (< (watch-changed b) (watch-changed a)) b a)) watches)
                watches (remove stale watches)))
        (setf (gethash session *watches*) (cons watch watches))))
    (receipt "watched" (list :form line :value (watch-brief value)))
    (append (when stale
              (receipt "watched" (list :form stale.text :closed "the cap"))
              (list (format nil "[no longer watching ~a: ~d others changed since]"
                            (watch-label stale) *watch-limit*)))
            (list "[watching: shown again when its value changes]"))))

(defun run-watches (session &optional except)
  "Run SESSION's watched snippets again, oldest first, all but the one written
EXCEPT. => the line of each that changed, or that did what a watched snippet
may not and is closed."
  (let ((lines '()))
    (dolist (watch (reverse (remove except (bt2:with-lock-held (*watches-lock*)
                                             (copy-list (gethash session *watches*)))
                                    :key #'watch-text :test #'equal)))
      (multiple-value-bind (value closed object) (rerun-watch watch)
        (cond (closed
               (bt2:with-lock-held (*watches-lock*)
                 (setf (gethash session *watches*) (remove watch (gethash session *watches*))))
               (receipt "watched" (list :form watch.text :closed closed))
               (push (format nil "[no longer watching ~a: ~a]" (watch-label watch) closed) lines))
              ((string/= value watch.value)
               (receipt "watched" (list :form watch.text :value (watch-brief value)))
               (push (format nil "[watched ~a: ~a]" (watch-label watch)
                             (watch-change-text watch value object))
                     lines)
               (setf watch.value value
                     watch.object object
                     watch.changed (incf *watch-tick*))))))
    (nreverse lines)))

(defun compile-watch (form)
  "FORM as a thunk, compiled with the compiler's notes muffled."
  (handler-bind ((warning (lambda (w)
                            (when (find-restart 'muffle-warning w)
                              (muffle-warning w)))))
    (compile nil `(lambda () ,form))))

(defun rerun-watch (watch)
  "WATCH's value as it prints now, and a copy of what printed so, as
(values TEXT NIL OBJECT) -- a failure prints as ERROR: and its type, a value
like any other -- or (values NIL WHY) when the run reached outside the image
or took five times *WATCH-SECONDS*, which closes it."
  (let* ((*watch-running* t)
         (*snippet-touched* nil)
         (started (get-internal-real-time))
         (io (thread-io-calls))
         (object nil)
         (value (handler-case
                    (let ((*package* (find-package '#:nodecode.evolved))
                          (*standard-output* (make-broadcast-stream))
                          (*error-output* (make-broadcast-stream))
                          (*trace-output* (make-broadcast-stream)))
                      (watch-value-text
                       (setf object (funcall (or watch.thunk
                                                 (setf watch.thunk (compile-watch watch.form)))))))
                  (nlk:turn-cancelled-condition (c) (error c))
                  ((or error storage-condition) (e) (format nil "ERROR: ~(~a~)" (type-of e))))))
    (cond ((or *snippet-touched* (not (and io (eql (thread-io-calls) (1+ io)))))
           (values nil "it reached outside the image"))
          ((> (- (get-internal-real-time) started)
              (* 5 *watch-seconds* internal-time-units-per-second))
           (values nil "it ran too long"))
          (t (values value nil (copy-tree object))))))

;;; --- source and print helpers ----------------------------------------------

(defun source-text (form)
  "FORM as the model wrote it, on one line: pretty, so (QUOTE X) reads back
as 'X, and with a margin wide enough that the printer does not fold it."
  (handler-case (let ((*print-pretty* t) (*print-right-margin* 400)
                      (*print-length* 20) (*print-level* 6))
                  (substitute #\Space #\Newline (prin1-to-string form)))
    (error () "[unprintable form]")))

(defun readably (value)
  "VALUE printed so its type is not in doubt: a string keeps its quotes, a
symbol does not gain any. Bounded like any other evidence."
  (let ((text (handler-case (let ((*print-readably* nil) (*print-escape* t)
                                  (*print-length* 12) (*print-level* 3)
                                  (*print-pretty* nil))
                              (prin1-to-string value))
                (error () "[unprintable]"))))
    (if (> (length text) 120) (concatenate 'string (subseq text 0 117) "...") text)))

(defun snippet-package (form)
  "The package FORM — an (IN-PACKAGE designator) — moves the rest of the
snippet into, or NIL for any other form. A package this image lacks is the
reader's verdict: it signals, so the snippet runs nothing, as a paren that
closes nothing does."
  (when (and (consp form) (symbolp (first form))
             (string= "IN-PACKAGE" (symbol-name (first form)))
             (consp (rest form)))
    (let ((designator (second form)))
      (or (and (or (symbolp designator) (stringp designator))
               (find-package designator))
          (error "no package named ~a; the snippet reads nothing" designator)))))

(defun read-snippet-forms (text &aux (*read-eval* nil)
                                     (*package* *package*)
                                     (eof (list :eof))
                                     (position 0)
                                     (forms '()))
  "Every toplevel form of TEXT in order, each with the bytes it was read
from: ((FORM . BYTES) ...). BYTES run from the previous form's end, so a
comment the model wrote above a definition rides with it into the layer.
A snippet that does not read runs nothing: the reader's verdict lands before
the first evaluation, never between two. A form (IN-PACKAGE ...) moves the
reader for the forms after it, as LOAD would — evaluating it moves the
evaluation the same way — so a snippet that opens with one reads and runs in
the package it names; the switch ends with the snippet."
  (loop
    (multiple-value-bind (form end) (read-from-string text nil eof :start position)
      (when (eq form eof)
        (return (nreverse forms)))
      (push (cons form (subseq text position end)) forms)
      (setf position end)
      (when-let (package (snippet-package form)) (setf *package* package)))))

(defparameter *snippet-warning-limit* 6
  "How many of a form's compiler warnings lead its result; the rest are
counted on one line.")

(defun undefined-name-warning-p (warning)
  "Whether WARNING is SBCL's deferred verdict on a name the form used and
the image lacks — `undefined function: F`, `undefined variable: *V*` — the
one style-warning the model must read, since the call will fail exactly
there. SBCL signals it as a plain SIMPLE-STYLE-WARNING with no class of its
own, so the text is the fact; this is a display filter, not a policy."
  (uiop:string-prefix-p "undefined " (princ-to-string warning)))

(defun surfaced-warning-p (warning)
  "Whether WARNING leads the form's result: every warning that is not a
style-warning — SBCL's compile-time verdicts (a literal of the wrong type,
a wrong argument count, an unknown keyword, an undefined variable), a
(WARN ...) the form ran — and, of the style-warnings, the undefined-name
report alone. Redefinitions, unused variables and the like stay muffled:
the model redefines constantly and would drown in them."
  (or (not (typep warning 'style-warning))
      (undefined-name-warning-p warning)))

(defun warning-lines (texts)
  "TEXTS — a form's surfaced warnings, in order — as the lines that lead
its result: one [warning: ...] each, a multi-line report continued under
its bracket, and past *SNIPPET-WARNING-LIMIT* one line counting the rest."
  (with-output-to-string (out)
    (loop for text in texts
          for index from 0
          when (< index *snippet-warning-limit*)
            do (let ((lines (uiop:split-string (string-right-trim '(#\Newline) text)
                                               :separator '(#\Newline))))
                 (format out "~&[warning: ~a~{~%  ~a~}]~%" (first lines) (rest lines))))
    (when (> (length texts) *snippet-warning-limit*)
      (format out "~&[+~d more warning~:p]~%"
              (- (length texts) *snippet-warning-limit*)))))

(defvar *snippet-unwound-hooks* nil
  "The hooks a failing form of this snippet registered and lost with its
error, as \"name key\" strings, bound per snippet by EVAL-FORM-PRINTED and
named on the error text.")

(defvar *arity-callee* nil
  "(CONDITION . NAME): the innermost frame when CONDITION, an argument-count
error, was signalled — the function that refused the call — captured
before the stack unwinds, bound per snippet by EVAL-FORM-PRINTED. SBCL's
report says \"invalid number of arguments: 4\" and nothing else; without
this, the function that refused the call would be unnamed.")

(defun arity-error-p (condition)
  (and (typep condition 'sb-int:simple-program-error)
       (let ((control (simple-condition-format-control condition)))
         (and (stringp control)
              (search "invalid number of arguments" control)
              t))))

(defun note-arity-callee (condition)
  "HANDLER-BIND arm: remember the frame that signalled CONDITION while it
is still on the stack. Declines, so the snippet's own arm still answers."
  (when (and (arity-error-p condition) (null *arity-callee*))
    (setf *arity-callee*
          (cons condition
                (ignore-errors
                 (block found
                   (sb-debug:map-backtrace
                    (lambda (frame)
                      (when-let (name (ignore-errors (sb-debug::frame-call frame)))
                        (return-from found
                          (let ((text (let ((*print-pretty* nil))
                                        (princ-to-string name))))
                            (if (> (length text) 90)
                                (format nil "~a..." (subseq text 0 90))
                                text)))))
                    :count 3)
                   nil))))))

(defun unwind-snippet-hooks (before)
  "Remove every hook registered since BEFORE, a snapshot of NLK::*HOOKS*,
and note each on *SNIPPET-UNWOUND-HOOKS*. A form that signals registers
nothing: the chain never keeps advice whose own form could not finish."
  (dolist (pair nlk::*hooks*)
    (let ((name (car pair))
          (old (cdr (assoc (car pair) before :test #'equal))))
      (dolist (entry (cdr pair))
        (unless (assoc (car entry) old :test #'string=)
          (when (ignore-errors (nlk:unhook name (car entry)))
            (push (format nil "~(~s~) ~s" name (car entry))
                  *snippet-unwound-hooks*)))))))

(defun evaluate-snippet-form (form text)
  "One form of the snippet: escrow what it displaces, evaluate, record or hold
the definition, name the changes the scribe cannot see, and answer the
value — the caller prints it, briefly or in full by its place in the snippet."
  (let* (;; Read BEFORE the eval, because a redefinition can only escrow
         ;; what it displaces while that is still standing: what does this
         ;; form take out from under the stack it was dispatched from
         ;; (provisional.lisp)?
         (escrow (nlk:definition-escrow form))
         ;; And what stands before it, for the changes the scribe will not
         ;; see — a definition built inside another form, a (setf
         ;; fdefinition) — named on the result afterwards, at the one
         ;; moment the rule applies.
         (before (nlk:definitions-before))
         ;; And what a file this snippet writes makes live: LOAD-SOURCE fills it
         ;; (exec.lisp's EDIT and WRITE-FILE), REPORT-UNKEPT-CHANGES subtracts
         ;; it. Per snippet, so a definition whose durable home is the file just
         ;; written is never reported as an unkept change.
         (nlk::*source-loaded-names* '())
         ;; The compiler's verdicts are the model's answer, ahead of the
         ;; value: every warning is muffled where a restart exists — so
         ;; SBCL prints no diagnostic block of its own — and the ones
         ;; SURFACED-WARNING-P keeps are collected here to lead the result
         ;; (WARNING-LINES).
         ;; :OVERRIDE T is load-bearing: SBCL defers undefined-name warnings
         ;; to the end of the enclosing compilation unit, which for a form
         ;; evaluated under ASDF or --load is outside the snippet's
         ;; bindings; a fresh unit here pulls that report inside the
         ;; handler, so it is rendered like the rest instead of dumped.
         (warnings '())
         ;; The hooks standing before the form: what it registers is
         ;; removed again if it does not finish (UNWIND-SNIPPET-HOOKS).
         (hooks-before nlk::*hooks*)
         (finished nil)
         (value (unwind-protect
                    (prog1
                        (handler-bind
                            ((warning
                               (lambda (w)
                                 (when (surfaced-warning-p w)
                                   ;; The report without SBCL's manual references,
                                   ;; which name a node in a book the model does not have.
                                   (let ((sb-int:*print-condition-references* nil))
                                     (pushnew (princ-to-string w) warnings :test #'string=)))
                                 (when (find-restart 'muffle-warning w)
                                   (muffle-warning w))))
                             (sb-int:simple-program-error #'note-arity-callee))
                          ;; What the form did beyond the image, read off
                          ;; its own thread: the watch rule (WATCHED-TEXT).
                          (let ((io (thread-io-calls)))
                            (prog1 (with-compilation-unit (:override t)
                                     (let ((nlk:*evaluating* t))
                                       (eval form)))
                              (unless (and io (eql (thread-io-calls) (1+ io))
                                           (eq hooks-before nlk::*hooks*))
                                (setf *snippet-touched* t)))))
                      (setf finished t))
                 (unless finished (unwind-snippet-hooks hooks-before)))))
    (when warnings
      (write-string (warning-lines (reverse warnings))))
    ;; The layer rides the success path only: a defining form that
    ;; evaluated is written to the organism's own folder with the model's
    ;; own bytes and committed. It never signals — a scribe
    ;; defect is one bracketed line on this eval's output, not a turn
    ;; error.
    ;;
    ;; Unless the form redefined a name that is RUNNING on that stack.
    ;; Evaluating is not evidence for the one definition whose failure
    ;; would sever the ingress that would undo it, so the write
    ;; waits for a traversal and the hold owns the same call, replayed.
    (let ((kept (or (nlk:hold-definitions escrow form text (length text))
                    (mapcar (lambda (key) (getf key :name))
                            (nlk:record-definitions form text (length text))))))
      (when kept (setf *snippet-touched* t))
      (nlk:report-unkept-changes before form kept))
    value))

(defun snippet-error-text (index count done message)
  "The error arms' text. MESSAGE alone for a snippet of one form — the
contract every reader of ERROR: already has — else led by which form of
the snippet failed, what evaluated before it and stands, and what never ran:
a snippet is not atomic, and the model must know exactly where it stopped."
  (if (<= count 1)
      (format nil "ERROR: ~a" message)
      (flet ((preview (text)
               (let ((line (subseq text 0 (or (position #\Newline text) (length text)))))
                 (if (> (length line) 60)
                     (format nil "~a..." (subseq line 0 60))
                     line))))
        (format nil "ERROR in form ~d of ~d (~{~a~^; ~}): ~a" index count
                (append (when (> index 1)
                          (list (format nil "form~:[s 1-~d~;~* 1~] evaluated and stand~:[~;s~]: ~{~a~^, ~}"
                                        (= index 2) (1- index) (= index 2)
                                        (mapcar #'preview (reverse done)))))
                        (when (< index count)
                          (list (if (= (1+ index) count)
                                    (format nil "form ~d not evaluated" count)
                                    (format nil "forms ~d-~d not evaluated" (1+ index) count)))))
                message))))

(defun eval-form-printed (form-string stream)
  "The eval snippet's core: read and evaluate FORM-STRING — a snippet of one or
more toplevel forms — with its printed output directed to STREAM, under
the snippet's REPL conventions. Returns (values VALUE-TEXT ERROR-P): the
printed value for one form, one => line per form for several — the last
in full, the ones before it briefly — or the failure rendered as text with ERROR-P true — a failed form is information
the model should see and correct, not a crash of the turn, and a snippet
that fails at its third form says so, with the first two standing. The
turn-cancel condition passes through (it is the worker's unwind), and so
does the background eval's abort condition (exec.lisp), a
SERIOUS-CONDITION that is not an ERROR precisely so no arm here can
flatten it into result text.

The text is completed before it is read (COMPLETE-FORM-TEXT): parens the
model left open close where its own shape says, and the result leads
with what was closed. The model's parens are never moved or removed.

Every snippet runs here, in this image, with full host authority, whoever
asked — that is the property that makes this substrate worth choosing."
  ;; Registered for the extent of the evaluation, and the guard started with
  ;; it: the runaway probe reaches a snippet evaluated in place only while this
  ;; thread is on its list, and a headless image has no gateway watcher to
  ;; start it (exec.lisp). Called, not wrapped in the macro, because exec.lisp
  ;; loads after this file.
  (call-with-evaluation-guarded
   (lambda ()
  (let (;; Printer authority. Worker threads arrive with bordeaux-threads'
         ;; WITH-STANDARD-IO-SYNTAX defaults (+standard-io-bindings+), under
         ;; which *PRINT-READABLY* is T: base-strings print as
         ;; #A((n) BASE-CHAR . ...) forms and any unprintable value — a
         ;; package, a stream — aborts the whole result with
         ;; PRINT-NOT-READABLE. The eval snippet owns its printed contract
         ;; instead: plain REPL syntax on every thread. The bindings wrap the
         ;; error arms too, because condition reporters PRIN1 their datum
         ;; under this same dynamic environment.
        (*print-readably* nil)
        (*print-escape* t)
        (*print-pretty* nil)
        ;; Circle detection is the one divergence from REPL defaults: a
        ;; form returning a circular structure must truncate into #n#
        ;; labels, not spin the printer forever.
        (*print-circle* t)
        (*print-length* nil)
        (*print-level* nil)
        (*print-lines* nil)
        (*print-base* 10)
        (*print-radix* nil)
        (*print-case* :upcase)
        (*print-array* t)
        (*print-gensym* t)
        ;; The snippet's progress, read by the error arms: which form is
        ;; running, of how many, and the printed values of those done.
        (index 0)
        (count 0)
        (done '())
        ;; The note the snippet opened with (SNIPPET-NOTE), read once the text
        ;; has completed, published on the way out.
        (*snippet-note* nil)
        ;; What a failing form lost and whom an arity error was calling,
        ;; read by the error arm below.
        (*snippet-unwound-hooks* '())
        (*arity-callee* nil)
        ;; Whether the snippet's last form reached outside the image, whether
        ;; the forms before it did, and when it began: what decides if it
        ;; is watched (WATCHED-TEXT).
        (*snippet-touched* nil)
        (acted nil)
        (started (get-internal-real-time)))
   (unwind-protect
    (handler-case
        (let* ((*package* (find-package '#:nodecode.evolved))
              ;; A form that prints is common; route it to STREAM rather
              ;; than letting it interleave with the operator's output.
              (*standard-output* stream)
              ;; EVAL compiles, and the compiler writes diagnostics to
              ;; *ERROR-OUTPUT*, not *STANDARD-OUTPUT*. Left unbound, a
              ;; style-warning from a model-written form prints to the
              ;; operator's terminal mid-turn, which reads as a crash
              ;; even though the error is already returned as text below.
              (*error-output* stream)
              (*trace-output* stream)
              ;; The ambient scribe session: the layer write below names it
              ;; in its commit and keys its durable fact off it.
              (nlk:*scribe-session-id*
                (and *current-durable-turn*
                     (nlk:durable-turn-session-id *current-durable-turn*)))
              ;; The REPL's history variables, this session's: * ** *** the
              ;; last values of its last three finished snippets, / // /// the
              ;; lists of their values. Dynamic bindings, so a gateway
              ;; evaluating several sessions' snippets at once gives each its
              ;; own, and a form that SETFs one changes only this snippet's.
              (history (snippet-history (or nlk:*scribe-session-id* "local")))
              (/ (first history)) (// (second history)) (/// (third history))
              (* (car (last /))) (** (car (last //))) (*** (car (last ///))))
          (multiple-value-bind (text note) (complete-form-text form-string)
            ;; The lexical verdict first: a ")" that closes nothing or a
            ;; string that never ends is named where it is, because the
            ;; reader's own report names nothing the model can act on.
            (unless text
              (display-fact "error" (json-object "code" "READER-ERROR"))
              (return-from eval-form-printed (values (format nil "ERROR: ~a" note) t)))
            (setf *snippet-note* (snippet-note text))
            ;; What the reader closed leads the result, then the reader's
            ;; one silent miss: both are hints, not gates — the snippet still
            ;; evaluates.
            (when note (format t "~&~a~%" note))
            (when-let (escapes (string-escape-report text)) (format t "~&~a~%" escapes))
            (let ((forms (read-snippet-forms text)))
              (when (null forms)
                (error "nothing to evaluate"))
              (setf count (length forms))
              (let ((last-value nil)
                    (answers '()))
                (dolist (entry forms)
                  (incf index)
                  ;; The last form is judged on its own for watching: what
                  ;; the forms before it did is whether the snippet acted.
                  (when (= index count)
                    (setf acted *snippet-touched*
                          *snippet-touched* nil
                          started (get-internal-real-time)))
                  (let ((value (evaluate-snippet-form (car entry) (cdr entry))))
                    (push value answers)
                    (if (= index count)
                        (setf last-value value)
                        (push (print-value-brief value (1- index)) done))))
                ;; The snippet finished: its values are the session's newest
                ;; history — * and / in the next snippet — before anything
                ;; prints, so a print that cuts can name them.
                (let* ((key (or nlk:*scribe-session-id* "local"))
                       (kept (cons (nreverse answers) (snippet-history key))))
                  (setf (gethash key *snippet-histories*)
                        (subseq kept 0 (min *snippet-history-depth* (length kept)))))
                (let ((final (print-value-budgeted
                              last-value
                              (max 400 (- *eval-result-limit* 200)))))
                  (push final done)
                  (values (watched-text (if (= count 1)
                                            final
                                            (format nil "~{=> ~a~^~%~}" (reverse done)))
                                        (car (last forms)) last-value started acted)
                          nil))))))
      ;; Esc interrupting mid-eval lands here as the cancel condition —
      ;; an ERROR subtype the arm below would flatten into result text.
      ;; The cancel is the turn's unwind, so it passes through.
      (nlk:turn-cancelled-condition (c) (error c))
      (error (e)
        ;; Retained on the result fact, not inferred by a renderer from output.
        (display-fact "error" (json-object "code" (symbol-name (type-of e))))
        (values (watched-text
                 (snippet-error-text
                  index count done
                  (format nil "~a: ~a~@[, calling ~a~]~@[~%~a~]~@[~%[hooks this form registered were removed with its error: ~{~a~^, ~}]~]"
                          (type-of e)
                          (nlk:safe-condition-report e)
                          (and (consp *arity-callee*)
                               (eq (car *arity-callee*) e)
                               (cdr *arity-callee*))
                          (ignore-errors (condition-symbol-hint e))
                          (reverse *snippet-unwound-hooks*)))
                 nil nil started)
                t))
      ;; A form that hits the debugger on a warning would hang a
      ;; non-interactive image; treat serious conditions as text too.
      (storage-condition (e)
        (display-fact "error" (json-object "code" (symbol-name (type-of e))))
        (values (snippet-error-text index count done
                                    (nlk:safe-condition-report e))
                t)))
    ;; The result fact carries the note as the snippet's record, for the
    ;; eviction stub.
    (publish-snippet-record *snippet-note*))))))

(defun truncate-eval-result (result)
  "Clamp RESULT to *EVAL-RESULT-LIMIT*, disclosing the cut. An unbounded
result becomes the next prompt."
  (if (> (length result) *eval-result-limit*)
      (concatenate 'string (subseq result 0 *eval-result-limit*)
                   (format nil "~&... [truncated at ~:D characters]" *eval-result-limit*))
      result))

(defun eval-string (form-string)
  "Read and evaluate FORM-STRING in the live image, returning its printed
value as a string; errors return as text, with ERROR-P true as the second
value (EVAL-FORM-PRINTED's). This is the inline dialect of the
eval snippet — the eval tool itself is registered in exec.lisp, where
EVAL-FORM-PRINTED is folded into the run tool's yield-and-background
lifecycle so a long form detaches to a background eval instead of pinning
the turn worker. The text is raw: the tool body is the model lane and
sanitizes it (exec.lisp DISCLOSE-SANITIZED-TEXT)."
  (check-type form-string string)
  (bind ((capture (make-string-output-stream))
         ((value-text error-p) (eval-form-printed form-string capture)))
    (values (truncate-eval-result
             (let ((printed (get-output-stream-string capture)))
               (cond (error-p (concatenate 'string (reader-lines printed) value-text))
                     ((string= printed "") value-text)
                     (t (format nil "~a~&~a" printed value-text)))))
            error-p)))

;;; The eval REGISTRATION lives in exec.lisp beside the background-eval
;;; lifecycle it dispatches into (the .asd blesses exec-after-turn exactly so
;;; the registry's tools can register through REGISTER-TOOL). This file keeps
;;; the eval snippet itself: EVAL-FORM-PRINTED and the inline EVAL-STRING.

;;; --- durable fact flush hook ---------------------------------------------

(defvar *turn-fact-committed-fn* nil
  "A dynamically bound zero-argument hook invoked after this turn commits facts.
The Gateway binds it to flush canonical durable facts; local callers leave it NIL.")

(defun notify-turn-facts-committed ()
  (when *turn-fact-committed-fn* (funcall *turn-fact-committed-fn*)))

;;; --- call-provider: retry policy folded over the lane layer ----------------

(defun message-tool-calls (message)
  "The tool_calls vector of MESSAGE, or NIL. Providers use :NULL for absent."
  (let ((calls (and (hash-table-p message) (gethash "tool_calls" message))))
    (and (vectorp calls) (plusp (length calls)) calls)))

(defun nameless-tool-call (message)
  "The first tool call of MESSAGE whose function carries no name, or NIL."
  (find-if (lambda (call) (not (json-value call :text "function" "name")))
           (or (message-tool-calls message) #())))

(defun assistant-message-empty-p (message)
  "True when MESSAGE carries no content and no tool calls — the Zig
empty-response contract: a stream that says nothing user-visible is a failed
attempt, not durable history. Reasoning alone is not an answer: a final
message whose only words are reasoning_content is invisible to the room and
to every later replay."
  (and (hash-table-p message)
       ;; CONTENT-TEXT is total over both wire shapes (string and typed part
       ;; vector), and empty means empty in either.
       (string= "" (content-text (gethash "content" message)))
       (null (message-tool-calls message))))

(defvar *provider-round* 0
  "The tool-round index of the provider call in flight, bound by AGENT-LOOP.
Ambient like *CURRENT-DURABLE-TURN* rather than an argument: CALL-PROVIDER's
lambda list is a stubbing seam (test doubles) and the round
is turn-loop state, not caller input.")

(defvar *previous-request-json* nil
  "The exact serialized body of the turn's previous provider round, as the
UTF-8 octets that went out, or NIL at round zero, which reads the session's
last kept body instead (NLK:LAST-REQUEST-BODY). Bound per turn by RUN;
CALL-PROVIDER reads it to stamp the request fact's prefix diagnosis and sets
it after the fact lands.")

(defvar *request-bodies-cap* nil
  "The bytes the kept request bodies may take (REQUEST-BODIES-CAP), read by
RUN as its turn starts so a refused config refuses before a round is spent;
NIL or 0 keeps none.")

(nlk:define-section ("request_bodies")
  (:guide "The exact body of every model call is kept in the store, for the Run view to show what was sent; past keep_mb the oldest go.")
  ("keep_mb" :integer :default 256 :min 0
             :doc "megabytes the kept request bodies may take in the store; 0 keeps none"))

(defun request-bodies-cap ()
  "The bytes the exact request bodies may take in the store: the shared
config's `request_bodies.keep_mb' in megabytes, 256 unless it says, 0 to keep
none; past it the oldest go (NLK:KEEP-REQUEST-BODY)."
  (* 1024 1024 (getf (nlk:section-settings (nlk:find-section '("request_bodies"))
                                          (json-value (read-shared-config) :object "request_bodies"))
                     :keep-mb)))

(defun json-path-at (body offset &aux (depth 0) (key nil) (index nil) (in-string nil) (escaped nil)
                                      (string-start 0) (pending nil))
  "Where byte OFFSET of the JSON object BODY — its UTF-8 octets — falls, as
a short path: a top-level key (\"tools\"), an element of a top-level array
(\"input[3]\"), or \"\" before the first key. A scan, not a parse — strings
and escapes are skipped, depth is counted, and JSON's structure is ASCII so
the bytes of a wider character never read as any of it — so it costs one
pass over the prefix and never refuses a body."
  (loop for i from 0 below (min (1+ offset) (length body))
        for byte = (aref body i)
        do (cond (in-string
                  (cond (escaped (setf escaped nil))
                        ((= byte #.(char-code #\\)) (setf escaped t))
                        ((= byte #.(char-code #\"))
                         (setf in-string nil)
                         (when (= depth 1)
                           (setf pending (sb-ext:octets-to-string
                                          body :start (1+ string-start) :end i
                                          :external-format :utf-8))))))
                 ((= byte #.(char-code #\")) (setf in-string t string-start i))
                 ((= byte #.(char-code #\:))
                  (when (and (= depth 1) pending)
                    (setf key pending pending nil index nil)))
                 ((= byte #.(char-code #\,))
                  (cond ((and (= depth 2) index) (incf index))
                        ((= depth 1) (setf pending nil))))
                 ((or (= byte #.(char-code #\{)) (= byte #.(char-code #\[)))
                  (incf depth)
                  (when (and (= depth 2) (= byte #.(char-code #\[))) (setf index 0)))
                 ((or (= byte #.(char-code #\})) (= byte #.(char-code #\])))
                  (decf depth)
                  (when (= depth 1) (setf index nil)))))
  (cond ((null key) "")
        (index (format nil "~a[~d]" key index))
        (t key)))

(defun request-prefix-diagnosis (previous current)
  "How much of CURRENT the PREVIOUS request body already sent: => (values
SHARED-BYTES PATH), the length of their common prefix and where CURRENT's
first differing byte falls (JSON-PATH-AT); (values NIL NIL) with no
previous body."
  (if (and (typep previous 'nlk:octets) (typep current 'nlk:octets))
      (let ((shared (or (mismatch previous current) (length previous))))
        (values shared (json-path-at current shared)))
      (values nil nil)))

(defun provider-usage-fact-args (usage)
  "USAGE's eight token counts as RECORD-TURN-USAGE keyword arguments, NIL
when the round reported no usage at all. One spelling of the block both the
voided length-cut fact and AGENT-LOOP's round close APPLY on top of their own
identity keywords; a count the lane never learned stays NIL, which the fact
drops rather than record as a zero the provider did not say."
  (when usage
    (list :input-tokens usage.input-tokens
          :output-tokens usage.output-tokens
          :cached-input-tokens usage.cached-input-tokens
          :total-tokens usage.total-tokens
          :reasoning-tokens usage.reasoning-tokens
          :cache-write-tokens usage.cache-write-tokens
          :cache-write-5m-tokens usage.cache-write-5m-tokens
          :cache-write-1h-tokens usage.cache-write-1h-tokens)))

(defun record-round-usage (turn &rest usage &key provider model &allow-other-keys)
  "Record one round's turn.usage fact on TURN from USAGE, RECORD-TURN-USAGE's
keywords, priced: COST-USD is what the catalog's price for MODEL makes of
the counts (MODEL-PRICE, ROUND-COST), COST-KNOWN whether there was one."
  ;; The one writer of the fact: a round's close, a voided attempt and a
  ;; cancel's cut all record through here, so each bills the same way. The
  ;; price is the one in the catalog as the round is recorded, so a later
  ;; price change never rewrites what a round cost.
  (let ((cost (apply #'round-cost (model-price model provider) usage)))
    (apply #'nlk:record-turn-usage turn
           (if cost (list* :cost-known t :cost-usd cost usage) usage))))

(defun cut-round-usage-estimate (context config cadence response-model
                                 response-effort text thinking calls)
  "What a round the cancel cut can still say about itself, for the usage
fact RUN records in its place: the identity the round ran under, and —
only once the stream proved the server processed the request (a part
arrived) — the counts the server never got to report. The prompt is the
calibrated context estimate, the same corrected number the output cap is
sized from; the output is everything that streamed, chars/4, with the
thinking share counted again as reasoning — the provider convention
(reasoning_tokens is a subset of output_tokens), so the divider reads the
same whichever way the turn ended.
=> the RECORD-TURN-USAGE argument list itself (:provider :model
:reasoning-effort :response-model :input-tokens :output-tokens
:reasoning-tokens), which both cut sites APPLY rather than re-spell;
counts NIL when unproven — never a zero the provider did not say."
  (let* ((provider-id config.provider)
         (model-id config.model)
         (served (plusp cadence.part-count))
         (thinking-chars (length thinking))
         (output-chars (+ (length text) thinking-chars
                          (loop for (nil . buf) in calls
                                sum (length (or (getf buf :arguments) ""))))))
    (list :provider provider-id
          :model model-id
          :reasoning-effort (or response-effort
                                config.reasoning-effort)
          :response-model response-model
          :input-tokens (and served
                             (round (* (compiled-context-estimated-tokens context)
                                       (context-estimate-ratio provider-id
                                                               model-id))))
          :output-tokens (and served (plusp output-chars)
                              (ceiling output-chars 4))
          :reasoning-tokens (and served (plusp thinking-chars)
                                 (ceiling thinking-chars 4)))))

(defparameter *continuation-reasoning-prompt*
  "Your previous reply was cut off mid-stream after ~d characters of ~
reasoning — the connection ended, not your answer, and nothing of your ~
final answer was sent yet. Pick your reasoning up from where it stopped ~
without restarting or repeating it. Then write your final answer in full, ~
self-contained from its first word, exactly as you would have had nothing ~
been cut."
  "The user nudge a continuation request ends with when only reasoning was
sealed (FORMAT control over the sealed character count). Without this
wording, the plainer `continue exactly from where you left off' would make
the model continue its cut thinking inside the ANSWER channel — the answer
would open mid-list before the real answer follows.")

(defparameter *continuation-reply-prompt*
  "Your previous reply was cut off mid-stream after ~d characters — the ~
connection ended, not your answer. Continue exactly from where your reply ~
left off; do not restart, and do not repeat what you already wrote."
  "The nudge when sealed answer text exists: the merged round joins the
continuation's text bare onto the sealed text, so the model must resume
the sentence, not the paragraph.")

(defun continuation-context (context reasoning text)
  "CONTEXT with a cut attempt's output replayed as the assistant's own
cut-off reply and the continuation nudge after it — the request a
continuation attempt sends (*CONTINUATION-REASONING-PROMPT* when only
reasoning was sealed, *CONTINUATION-REPLY-PROMPT* once answer text was). REASONING rides the lane's REASONING-CARRY: as
reasoning_content where the wire replays unsigned thinking (the chat lane),
as assistant text where it cannot (Anthropic, Responses, Google) — the model
still sees its own thought, as prose. Request-time only: the durable round
is the merged message CALL-PROVIDER returns, never these two."
  (let* ((config context.provider-config)
         (lane (find-lane-by-name config.lane))
         (carry lane.reasoning-carry)
         (assistant
           (ecase carry
             (:reasoning
              (json-object "role" "assistant" "content" text
                           :when (plusp (length reasoning))
                           "reasoning_content" reasoning))
             (:text
              (json-object "role" "assistant"
                           "content" (cond ((zerop (length reasoning)) text)
                                           ((zerop (length text)) reasoning)
                                           (t (format nil "~a~%~%~a"
                                                      reasoning text)))))))
         (nudge (json-object "role" "user"
                             "content" (if (plusp (length text))
                                           (format nil *continuation-reply-prompt*
                                                   (+ (length reasoning)
                                                      (length text)))
                                           (format nil *continuation-reasoning-prompt*
                                                   (length reasoning))))))
    (context-with context :messages (append (coerce context.messages 'list) (list assistant nudge)))))

(defun next-round-context (context metadata)
  "The context the next round starts from: CONTEXT, the one this round ended
with -- a failover holds for the turn -- unless a refusal sent this round to a
candidate, which answered it alone (CALL-PROVIDER's :RESUME-PROVIDER-CONFIG
in METADATA): then the selection's config again."
  (let ((resume (getf metadata :resume-provider-config)))
    (if resume (context-with context :provider-config resume) context)))

(defun call-provider (context &key (on-part *turn-part-fn*))
  "Call the selected provider lane with turn retry policy folded on top.
Returns (values MESSAGE USAGE FINISH-REASON RESPONSE-METADATA), where
RESPONSE-METADATA is the (:model :effort) plist the winning attempt's
:response-metadata part carried — what the provider says it ANSWERED with,
as opposed to what the request asked for. NIL entries mean the stream did
not say.

One context overflow is recovered by evicting one whole turn. Transient
failures retry under the dual budget (the providers/attempt.zig reducer):
request-scope failures (connect, DNS, non-200 head) spend a per-stream-attempt
request budget, stream-scope failures (truncation, in-stream errors, empty
responses) spend the stream budget and refill the request budget. Retry-After
is honored first; each retry is journaled as a durable turn.provider_retry
fact when a durable turn is executing. An assistant message with no content,
no tool calls, and no reasoning is a contract error fed back into the retry
loop rather than recorded as history; so is one carrying a tool call with no
name.

A successful round leaves a durable turn.provider_request fact — which exact
request went out (body sha256, message count, frozen context estimate) and
what the live stream looked like (part counts, ttft, duration) — the
turn_engine.zig recordProviderRequestEvent evidence, minus usage, which the
separate turn.usage fact owns here. The round index rides *PROVIDER-ROUND*."
  (let* (;; The attempt.zig reducer state: the attempt index is the durable
         ;; identity and counts every try; the stream attempt only advances on
         ;; stream-scope retries; the request counter is per stream attempt.
         (provider-attempt 1)
         (stream-attempt 1)
         (request-retries-used 0)
         ;; Failover state: the identity the turn was ADMITTED on (the
         ;; chain never leads back to it) and the candidates this call has
         ;; tried, oldest first, each (:provider P :model M :reason R).
         (selection-config context.provider-config)
         (selection-provider selection-config.provider)
         (selection-model selection-config.model)
         (fallback-tried '())
         ;; The selection's own failure, (:reason R :status S :said TEXT),
         ;; when it sent the round down the chain: the terminal copy opens
         ;; on it (AMEND-TERMINAL-FAILOVER-NOTE).
         (selection-verdict nil)
         ;; Whether a refusal moved this round to a candidate: the candidate
         ;; answers this round alone (NEXT-ROUND-CONTEXT).
         (refusal-fallback-p nil)
         (overflow-recoveries 0)
         (length-cut-recovered-p nil)
         ;; Whether this round's images were already withheld: the
         ;; withholding runs once, and a second failure is the lane's.
         (images-withheld-p nil)
         (cadence (make-stream-cadence))
         ;; Accumulates streamed text, thinking, and in-flight tool-call
         ;; fragments so a cancel that unwinds mid-stream can still carry
         ;; what the user already paid for out to RUN's durable-closure
         ;; backstop — every block the stream created persists, not just
         ;; the plain text.
         (partial (make-string-output-stream))
         (partial-reasoning (make-string-output-stream))
         (partial-calls '())
         ;; The continuation seal: everything earlier cut attempts of this
         ;; round streamed and the policy chose to continue from rather
         ;; than re-run. Each continuation request carries it back as the
         ;; model's own cut-off reply; the winning attempt's message is
         ;; merged onto it. Empty until a cut past the rerun ceiling.
         (sealed-reasoning "")
         (sealed-text "")
         ;; What the stream said about itself. A retry voids the previous
         ;; attempt's answer the same way it voids its text: the next
         ;; attempt's :response-metadata overwrites both slots.
         (response-model nil)
         (response-effort nil)
         (observer (lambda (part)
                     (observe-stream-cadence cadence part)
                     (case part.kind
                       (:response-metadata
                        (let ((model part.model) (effort part.reasoning-effort))
                          (when (stringp model) (setf response-model model))
                          (when (stringp effort) (setf response-effort effort))))
                       (:text-delta
                        (write-string (or part.text "") partial))
                       (:reasoning-delta (write-string (or part.text "") partial-reasoning))
                       ((:tool-input-start :tool-input-delta :tool-call)
                        (let ((entry (assoc (or part.index 0)
                                            partial-calls)))
                          (unless entry
                            (setf entry (cons (or part.index 0)
                                              (list :id nil :name nil
                                                    :arguments "")))
                            (push entry partial-calls))
                          (let ((buf (cdr entry)))
                            (when (stringp part.id) (setf (getf buf :id) part.id))
                            (when (stringp part.tool-name) (setf (getf buf :name) part.tool-name))
                            (when (stringp part.arguments)
                              (setf (getf buf :arguments)
                                    part.arguments))))))
                     (when on-part (funcall on-part part))))
         (config context.provider-config))
    (labels ((continuing-p ()
               (or (plusp (length sealed-reasoning))
                   (plusp (length sealed-text))))
             (past-rerun-ceiling-p ()
               ;; Whether this attempt streamed past the rerun ceiling: a cut
               ;; there is continued, never re-run — and on its own lane,
               ;; since a lane that carried that much output is serving the
               ;; model. NIL when the attempt streamed nothing.
               (let ((streamed-ms (stream-cadence-duration-ms cadence))
                     (ceiling config.stream-rerun-ceiling))
                 (and ceiling (integerp streamed-ms)
                      (>= streamed-ms (* ceiling 1000)))))
             (attempt-context ()
               ;; The request this attempt sends: the round's context, or —
               ;; once a cut attempt has been sealed for continuation — that
               ;; context followed by the sealed output as the assistant's
               ;; own cut-off reply and one user nudge to go on from it.
               ;; Request-time only, never durable history (the context
               ;; engine's stub-splice posture): the merged round message is
               ;; what history records.
               (if (continuing-p)
                   (continuation-context context sealed-reasoning sealed-text)
                   context))
             (merge-continuation (message)
               ;; The winning attempt's message, with the seal restored in
               ;; front of what it streamed: reasoning joined on a newline
               ;; (the model picks up mid-sentence, and its first line says
               ;; so), text joined bare (a cut mid-answer resumes the
               ;; sentence). => (values MESSAGE CONTINUED-FROM), where
               ;; CONTINUED-FROM is the (:reasoning CHARS :content CHARS)
               ;; offset plist the assistant fact carries, or NIL when
               ;; nothing was continued.
               (if (not (continuing-p))
                   (values message nil)
                   (let ((reasoning-prefix (if (plusp (length sealed-reasoning))
                                               (format nil "~a~%" sealed-reasoning)
                                               ""))
                         (reasoning (gethash "reasoning_content" message))
                         (content (gethash "content" message)))
                     (setf (gethash "reasoning_content" message)
                           (concatenate 'string reasoning-prefix
                                        (if (stringp reasoning) reasoning "")))
                     (when (plusp (length sealed-text))
                       (setf (gethash "content" message)
                             (cond ((stringp content)
                                    (concatenate 'string sealed-text content))
                                   ((and (vectorp content) (plusp (length content)))
                                    (concatenate
                                     'vector
                                     (vector (json-object "type" "text"
                                                          "text" sealed-text))
                                     content))
                                   (t sealed-text))))
                     (values message
                             (list :reasoning (length reasoning-prefix)
                                   :content (length sealed-text))))))
             (void-partial-stream ()
               (get-output-stream-string partial)
               (get-output-stream-string partial-reasoning)
               (setf partial-calls '())
               ;; The cadence is per attempt: the next attempt's rollup and
               ;; the rerun ceiling both read this attempt alone, never the
               ;; sum of every attempt the call has run.
               (setf cadence (make-stream-cadence)))
             (seal-streamed-attempt (detail disposition &key (estimate-p t))
               ;; What the failed attempt had streamed is kept, not wiped:
               ;; one turn.attempt_voided fact carries the reasoning and
               ;; text whole (the cancel backstop's unclipped rule), and —
               ;; unless the provider itself reported the round's usage
               ;; (ESTIMATE-P NIL, the length cut) — an estimated, voided
               ;; turn.usage bills the spend. Consumes the partial buffers;
               ;; the caller voids the rest. A failure before the first
               ;; delta seals nothing and records nothing. => (values TEXT
               ;; THINKING) as streamed, both "" when nothing did.
               (let ((text (get-output-stream-string partial))
                     (thinking (get-output-stream-string partial-reasoning)))
                 (when (and *current-durable-turn*
                            (or (plusp (length text))
                                (plusp (length thinking))))
                   (nlk:record-attempt-voided
                    *current-durable-turn*
                    :attempt provider-attempt
                    :reasoning thinking :text text :detail detail
                    :stream-duration-ms (stream-cadence-duration-ms cadence)
                    :disposition disposition)
                   (when-let (cut (and estimate-p
                                       (cut-round-usage-estimate
                                        context config cadence
                                        response-model response-effort
                                        text thinking partial-calls)))
                     (apply #'record-round-usage *current-durable-turn*
                            :voided t :estimated t cut))
                   (notify-turn-facts-committed))
                 (values text thinking)))
             (extend-seal (text thinking)
               ;; A cut attempt past the rerun ceiling joins the seal: the
               ;; next attempt continues from everything streamed so far.
               ;; Reasoning segments join on a newline, text segments bare —
               ;; the same seams MERGE-CONTINUATION uses.
               (when (plusp (length thinking))
                 (setf sealed-reasoning
                       (if (plusp (length sealed-reasoning))
                           (format nil "~a~%~a" sealed-reasoning thinking)
                           thinking)))
               (when (plusp (length text))
                 (setf sealed-text (concatenate 'string sealed-text text))))
             (verdict-of (e)
               ;; What failure E said of its model: the failover label, the
               ;; HTTP status, and the provider's own words on one line.
               (list :reason (provider-failure-reason e)
                     :status (provider-error-status e)
                     :said (let ((said (nlk:one-line (provider-said (provider-error-detail e)))))
                             (if (plusp (length said)) said (provider-failure-reason e)))))
             (note-candidate-verdict (e)
               ;; The candidate the round is on failed with E: its own
               ;; verdict joins the tried list. A tried entry is pushed
               ;; verdictless and gets one when its attempt fails or its
               ;; config refuses — never the verdict of the model it took
               ;; over from, which is the fallback fact's reason.
               (when (and fallback-tried (null (getf (first fallback-tried) :reason)))
                 (setf (first fallback-tried)
                       (list* :provider (getf (first fallback-tried) :provider)
                              :model (getf (first fallback-tried) :model)
                              (verdict-of e)))))
             (amend-terminal-failover-note (e)
               ;; The honest terminal copy: when a chain was walked and every
               ;; candidate failed, the failure that stands lists every model
               ;; the round tried, the selection first, each with what its
               ;; provider said (the PROVIDER-ERROR report).
               (note-candidate-verdict e)
               (when fallback-tried
                 (setf (provider-error-chain e)
                       (append (and selection-verdict
                                    (list (list* :provider selection-provider
                                                 :model selection-model
                                                 selection-verdict)))
                               (reverse fallback-tried)))))
             (next-candidate (chain)
               ;; The first entry of CHAIN this round has not run on: not
               ;; the model it runs on now, not the selection, not one tried.
               (find-if
                (lambda (entry)
                  (let ((pair (list (getf entry :provider)
                                    (getf entry :model))))
                    (and (not (equal pair (list config.provider config.model)))
                         (not (equal pair (list selection-provider
                                                selection-model)))
                         (not (member pair fallback-tried
                                      :key (lambda (tried)
                                             (list (getf tried :provider)
                                                   (getf tried :model)))
                                      :test #'equal)))))
                chain))
             (try-fallback (e)
               ;; One chain step: re-aim the round at the next configured
               ;; candidate this turn has not tried — resolved through the
               ;; same freeze as the selection (one lane, one credential,
               ;; one capability) — after sealing what the failed attempt
               ;; streamed. => T when the round was re-aimed; NIL when the
               ;; chain has nothing left and the caller's failure stands.
               (note-candidate-verdict e)
               ;; Off the chain, E is the selection's own failure.
               (unless fallback-tried
                 (setf selection-verdict (verdict-of e)))
               (let* ((current-provider config.provider)
                      (current-model config.model)
                      ;; Off the selection, only a failure whose trigger is
                      ;; on leaves for the chain.
                      (next (next-candidate (failover-chain-for e fallback-tried))))
                 (when next
                   (let ((provider (getf next :provider))
                         (model (getf next :model))
                         (reason (provider-failure-reason e)))
                     (push (list :provider provider :model model :reason nil)
                           fallback-tried)
                     (handler-case
                         (let ((candidate (failover-candidate-config provider model)))
                           ;; What the failing attempt streamed is sealed under
                           ;; its own attempt index first, then the seal is
                           ;; cleared: the candidate answers the round from
                           ;; scratch, never as another model's continuation.
                           (seal-streamed-attempt (provider-error-detail e) "rerun")
                           (setf sealed-reasoning "" sealed-text "")
                           ;; The candidate's attempt is timed from its own
                           ;; request: a cadence carried over kept the
                           ;; refused attempt's first token, and the
                           ;; candidate's TTFT came out negative.
                           (void-partial-stream)
                           (when *current-durable-turn*
                             (nlk:record-provider-fallback
                              *current-durable-turn*
                              :from-provider current-provider
                              :from-model current-model
                              :to-provider provider
                              :to-model model
                              :reason reason
                              :status (provider-error-status e)
                              :attempt provider-attempt
                              :detail (provider-error-detail e))
                             (notify-turn-facts-committed))
                           (setf config candidate
                                 context (context-with context
                                                       :provider-config candidate)
                                 ;; The candidate's lane gets its own budgets:
                                 ;; a spent budget on the primary says nothing
                                 ;; about the candidate's.
                                 stream-attempt 1
                                 request-retries-used 0)
                           (incf provider-attempt)
                           (emit-stream-part
                            on-part :stream-fallback
                            :payload (list :from-provider current-provider
                                           :from-model current-model
                                           :to-provider provider
                                           :to-model model
                                           :reason reason
                                           :status (provider-error-status e)))
                           t)
                       (error (refusal)
                         ;; A candidate whose own config cannot name a wire is
                         ;; recorded with the refusal as its reason — the
                         ;; tried list holds it, and the chain walks on.
                         (setf (getf (first fallback-tried) :reason) "unresolved"
                               (getf (first fallback-tried) :said)
                               (nlk:one-line (princ-to-string refusal)))
                         (when *current-durable-turn*
                           (nlk:record-provider-fallback
                            *current-durable-turn*
                            :from-provider current-provider
                            :from-model current-model
                            :to-provider provider
                            :to-model model
                            :reason "unresolved"
                            :attempt provider-attempt
                            :detail (princ-to-string refusal))
                           (notify-turn-facts-committed))
                         (try-fallback e)))))))
             (announce-retry (e scope delay detail attempt limit continuation)
               ;; One retry, announced twice over. The durable fact carries
               ;; the pre-apply reducer state (the Zig
               ;; notifyRetry-before-applyRetry order): ATTEMPT names the
               ;; attempt that FAILED. The live part then tells consumers to
               ;; void what that attempt buffered before the retry runs,
               ;; carrying the countdown so a retrying provider looks
               ;; different from a stalled one; its ATTEMPT and LIMIT are the
               ;; failing scope's own budget, not the fact's running index.
               (when *current-durable-turn*
                 (nlk:record-provider-retry
                  *current-durable-turn*
                  :attempt provider-attempt
                  :scope (retry-scope-name scope)
                  :stream-attempt stream-attempt
                  :request-retries-used request-retries-used
                  :status (provider-error-status e)
                  :delay-seconds (float delay)
                  :detail detail
                  :provider (provider-error-provider e)
                  :model (provider-error-model e))
                 (notify-turn-facts-committed))
               ;; The provider's own sentence rides too: `retrying 4/4
               ;; in 38s' over a 503 may say nothing of the high demand
               ;; behind it.
               (emit-stream-part on-part :stream-retry
                                 :payload (list :attempt attempt
                                                :limit limit
                                                :delay-seconds (float delay)
                                                :continuation continuation
                                                :status (provider-error-status e)
                                                :provider (provider-error-provider e)
                                                :reason (provider-said (provider-error-detail e)))))
             (evict-and-retarget (&optional (reason "provider_rejected"))
               ;; One shared recovery eviction step (context.lisp owns the
               ;; policy): true when the context was re-aimed at the thinned
               ;; window; NIL when nothing further is evictable and the
               ;; provider's verdict must stand. REASON rides both facts, so
               ;; a step the governor took reads as the admission it was and
               ;; not as a rejection.
               (multiple-value-bind (turns boundary)
                   (evict-context-one-step
                    context.session-id
                    context.history-turns
                    context.history-boundary
                    :live-turn *current-durable-turn*
                    :reason reason)
                 (when turns
                   (void-partial-stream)
                   ;; The step recorded a floor: the stub lists it as the
                   ;; block the next round's will, so the retry is that
                   ;; round's prefix.
                   (let ((floors (and context.session-id
                                      (nlk:retained-history-floors context.session-id))))
                     (setf context
                           (context-with
                            context
                            :messages (messages-with-visible-images
                                       (messages-at-boundary turns boundary floors)
                                       context.visible-images)
                            :boundary boundary :history-turns turns
                            :floors floors)))
                   t)))
             (pre-call-govern ()
               ;; Admission is deliberately soft: known pressure gets a
               ;; bounded number of existing durable eviction steps, while an
               ;; unknown window remains unknown and is left to the provider.
               ;; Recompute after every step because the retained history and
               ;; its estimate both changed.
               (let ((steps 0))
                 (loop
                   (let* ((config context.provider-config)
                          (provider-id config.provider)
                          (model-id config.model)
                          (estimate (round (* (compiled-context-estimated-tokens context)
                                              (context-estimate-ratio provider-id
                                                                      model-id))))
                          (decision (context-pressure-decision
                                     estimate
                                     (effective-context-budget config))))
                     (case (getf decision :status)
                       (:unknown
                        (note-phase "context-pressure status=unknown estimate=~d" estimate)
                        (return steps))
                       (:ok
                        (when (plusp steps)
                          (note-phase
                           "context-pressure status=evicted steps=~d estimate=~d budget=~d"
                           steps estimate (getf decision :budget)))
                        (return steps))
                       (:pressure
                        (cond
                          ((>= steps +max-pre-call-governor-steps+)
                           (note-phase
                            "context-pressure status=capped steps=~d estimate=~d budget=~d"
                            steps estimate (getf decision :budget))
                           (return steps))
                          ((evict-and-retarget "budget_fill")
                           (incf steps))
                          (t
                           (note-phase
                            "context-pressure status=unresolved estimate=~d budget=~d"
                            estimate (getf decision :budget))
                           (return steps)))))))))
             (refuse-without-output-room ()
               ;; The admission the governor cannot make. PRE-CALL-GOVERN
               ;; evicts until the estimate fits the budget or its step cap
               ;; bites, and then the request goes out whatever it looks
               ;; like. When what it looks like is a prompt filling the
               ;; whole window, the shrink-to-fit cap would collapse and the
               ;; only reply the model could give would be a length finish
               ;; with nothing in it — which the loop would read as its
               ;; final word, and the turn would end silently. Refusing here
               ;; costs no round trip and routes into the ladder below:
               ;; images are withheld and the round is re-sent without them,
               ;; which is the actual remedy when an attachment is what
               ;; filled the window; with none to withhold the refusal stands
               ;; and the turn fails saying so.
               (multiple-value-bind (cap shrunk-p)
                   (effective-max-output-tokens context)
                 (when (and shrunk-p (integerp cap)
                            (< cap +min-usable-output-tokens+))
                   (let* ((config context.provider-config)
                          (provider-id config.provider)
                          (model-id config.model)
                          (estimate (round (* (compiled-context-estimated-tokens context)
                                              (context-estimate-ratio provider-id
                                                                      model-id)))))
                     (note-phase
                      "output-room status=refused cap=~d estimate=~d budget=~a"
                      cap estimate (effective-context-budget config))
                     (error 'provider-no-output-room
                            :detail
                            (format nil "this request leaves ~d token~:p for ~
                                         output: ~:d estimated prompt tokens ~
                                         against a ~:d-token context window, ~
                                         and an answer needs ~d"
                                    cap estimate
                                    (or (effective-context-budget config) 0)
                                    +min-usable-output-tokens+))))))
             (withhold-images-and-retarget (e)
               ;; The other recovery step: the request carried images and
               ;; failed, so the round is re-sent without them, a note
               ;; where each stood (context.lisp WITHHOLD-IMAGE-GROUPS says
               ;; why). Before the ladder and outside its budgets: a relay
               ;; reports a refused image as the same 503 as an outage, and
               ;; the request minus its images tells the two apart in one
               ;; attempt where the ladder spent 88 seconds failing. The
               ;; turn's staging is marked the same way, so no later round
               ;; sends the refused bytes and LOOK refuses the next image
               ;; with the reason. The failed attempt is sealed and
               ;; journaled like any retry — a turn.provider_retry fact whose
               ;; detail names the withholding, the live reset — with no
               ;; wait, and no attempt or limit on the live part because no
               ;; ladder is being climbed. => T when images were withheld,
               ;; NIL when the request carried none.
               (multiple-value-bind (groups count)
                   (withhold-image-groups
                    context.visible-images
                    (lambda (n) (withheld-images-note e n)))
                 (when (plusp count)
                   (when *current-durable-turn*
                     (withhold-turn-images
                      (nlk:durable-turn-turn-id *current-durable-turn*)
                      (lambda (n) (withheld-images-note e n))))
                   (seal-streamed-attempt (provider-error-detail e) "rerun")
                   (announce-retry e :request 0
                                   (format nil "~d image~:p withheld after: ~a"
                                           count (provider-error-detail e))
                                   nil nil nil)
                   (incf provider-attempt)
                   (void-partial-stream)
                   (setf context
                         (context-with
                          context
                          :visible-images groups
                          :messages (messages-with-visible-images
                                     (messages-at-boundary
                                      context.history-turns
                                      context.history-boundary
                                      context.history-floors)
                                     groups)))
                   t))))
     (loop
      (handler-case
          ;; The attempt runs as the identity it targets: a failure names the
          ;; candidate a failover re-aimed the round at, not the selection
          ;; RUN bound.
          (let* ((*provider* config.provider)
                 (*model* config.model)
                 (lane (progn (pre-call-govern)
                              (refuse-without-output-room)
                              (find-lane-by-name config.lane)))
                 (request-context (attempt-context)))
            (multiple-value-bind (message usage finish-reason request-json)
                ;; The gc.lisp seam: a full collection fires shortly after
                ;; dispatch, inside the provider wait that is already dead
                ;; time. This is what keeps promoted garbage from ratcheting
                ;; across a session — without it nothing in production ever
                ;; ran a full GC. One timer per attempt; the unwind-protect
                ;; inside the macro unschedules it on every retry path.
                (nlk:collect-during-wait
                  (handler-bind ((nlk:turn-cancelled-condition
                                   (lambda (condition)
                                     (let ((text (get-output-stream-string
                                                  partial))
                                           (thinking (get-output-stream-string
                                                      partial-reasoning)))
                                       ;; A cancel mid-continuation closes
                                       ;; the round with the seal in front
                                       ;; of what streamed since, the same
                                       ;; merge a finished continuation
                                       ;; gets, and says where the join is.
                                       (when (continuing-p)
                                         (let ((prefix (if (plusp (length sealed-reasoning))
                                                           (format nil "~a~%" sealed-reasoning)
                                                           "")))
                                           (setf (nlk:turn-cancelled-condition-partial-continued-from
                                                  condition)
                                                 (list :reasoning (length prefix)
                                                       :content (length sealed-text))
                                                 thinking (concatenate 'string prefix thinking)
                                                 text (concatenate 'string sealed-text text))))
                                       (when (plusp (length text))
                                         (setf (nlk:turn-cancelled-condition-partial-text
                                                condition)
                                               text))
                                       (when (plusp (length thinking))
                                         (setf (nlk:turn-cancelled-condition-partial-reasoning
                                                condition)
                                               thinking))
                                       (when partial-calls
                                         (setf (nlk:turn-cancelled-condition-partial-tool-calls
                                                condition)
                                               (mapcar #'cdr
                                                       (sort (copy-list partial-calls)
                                                             #'< :key #'car))))
                                       (setf (nlk:turn-cancelled-condition-partial-usage
                                              condition)
                                             (cut-round-usage-estimate
                                              context config cadence
                                              response-model response-effort
                                              text thinking partial-calls))))))
                    ;; One fold per lane: a round with no observer IS the
                    ;; blocking call. OBSERVER is NIL when nothing wants parts.
                    (setf cadence.sent-ms (elapsed-ms 0))
                    ;; Watched until its first token while a candidate a
                    ;; slow start would leave for stands behind it.
                    (call-with-attempt-watch
                     (next-candidate (failover-chain-for "slow_start" fallback-tried))
                     (lambda ()
                       (funcall lane.stream-symbol
                                request-context
                                :on-part observer)))))
              (let* ((provider-id config.provider)
                     (model-id config.model)
                     (total (and usage usage.total-tokens))
                     (output (and usage
                                  usage.output-tokens))
                     (cut-recovered nil))
                ;; A tool call with no name fails the attempt before the
                ;; round can count as history: the wire (an aggregator
                ;; converting a streamed call chunk by chunk) did not carry
                ;; the call whole, and nothing is registered under "" to
                ;; dispatch it to. Its id and the head of its arguments are
                ;; the diagnosis.
                (when-let (nameless (nameless-tool-call message))
                  (let ((arguments (or (json-value nameless :string
                                                   "function" "arguments")
                                       "")))
                    (error 'provider-malformed-tool-call
                           :detail (format nil "provider returned a tool ~
                                                  call with no name (id ~a, ~
                                                  arguments ~a)"
                                           (gethash "id" nameless)
                                           (subseq arguments
                                                   0 (min 80 (length arguments)))))))
                ;; Window evidence and the measured prompt from every round
                ;; the server actually processed: the usage total is a PROVEN
                ;; context-window floor, and the prompt as the server's own
                ;; tokenizer counted it is what the next request's estimate is
                ;; anchored on (OBSERVE-CONTEXT-ESTIMATE).
                ;; Every lane normalizes INPUT to cache-exclusive with CACHED
                ;; disjoint, so input+cached is the full prompt when input is
                ;; reported; total-output is the fallback (a lane-derived
                ;; total may be cache-exclusive, which only under-measures —
                ;; safe for a floor, corrected here for the ratio).
                (when (and (integerp total) (plusp total)
                           (nlk:store-open-p))
                  (nlk:observe-context-window-floor provider-id model-id total)
                  (let ((input usage.input-tokens) (cached usage.cached-input-tokens))
                    (observe-context-estimate
                     provider-id model-id
                     (compiled-context-estimated-tokens context)
                     (cond ((integerp input) (max 1 (+ input (or cached 0))))
                           ((integerp output) (max 1 (- total output)))
                           (t total)))))
                ;; A context-edge length cut is a failed attempt, not durable
                ;; history: the truncated message (and any half-written tool
                ;; call in it) is discarded, its real spend bills as a voided
                ;; usage fact, one recovery eviction re-aims the window, and
                ;; the loop re-streams. Once per call: a second cut means the
                ;; cause is not the window (a server output cap), and the
                ;; round records as-is.
                (when (and (not length-cut-recovered-p)
                           (context-edge-length-cut-p context finish-reason
                                                      usage)
                           (evict-and-retarget))
                  (setf length-cut-recovered-p t
                        cut-recovered t)
                  (when *current-durable-turn*
                    (apply #'record-round-usage
                           *current-durable-turn*
                           :model model-id
                           :provider provider-id
                           :response-model response-model
                           :reasoning-effort
                           (or response-effort
                               config.reasoning-effort)
                           :finish-reason finish-reason
                           :voided t
                           (provider-usage-fact-args usage))
                    (notify-turn-facts-committed))
                  ;; The cut message is sealed into the transcript like any
                  ;; other voided attempt (the provider's own usage already
                  ;; billed it above, so no estimate), then live consumers
                  ;; void what is left, the same signal a transport retry
                  ;; sends.
                  (seal-streamed-attempt "context-edge length cut" "rerun"
                                         :estimate-p nil)
                  (emit-stream-part on-part :stream-retry
                                    :payload (list :attempt 1 :limit 1
                                                   :delay-seconds 0.0)))
                (unless cut-recovered
                  (multiple-value-bind (merged continued-from)
                      (merge-continuation message)
                    (setf message merged)
                    ;; The emptiness check runs on the merged message: a
                    ;; continued attempt whose own stream is still silent
                    ;; carries the sealed cut text, so the seal counts — and
                    ;; reasoning does not. Without this, a final whose only
                    ;; words were reasoning_content would complete a turn
                    ;; the room could never show.
                    (when (assistant-message-empty-p message)
                      ;; A refusal said nothing on purpose: the verdict stands, never re-asked.
                      (if (member finish-reason '("refusal" "content_filter") :test #'equal)
                          (error 'provider-refused
                                 :detail (format nil "the provider refused to answer (finish reason ~a)"
                                                 finish-reason))
                          (error 'provider-empty-response
                                 :detail "provider returned an empty assistant message")))
                    ;; Lanes return the exact serialized request, its UTF-8
                    ;; octets, as a fourth value; a test stub that returns three
                    ;; leaves REQUEST-JSON nil and simply records no fact.
                    (when (and *current-durable-turn* (typep request-json 'nlk:octets))
                      (multiple-value-bind (prefix-shared-bytes prefix-diverged-in)
                          (request-prefix-diagnosis
                           (or *previous-request-json*
                               ;; Round zero's previous request is the
                               ;; session's last, the turn before's final
                               ;; round, as its kept body holds it; an
                               ;; instrument that cannot read it reads nothing.
                               (and (plusp (or *request-bodies-cap* 0))
                                    (ignore-errors
                                     (nlk:last-request-body
                                      (nlk:durable-turn-session-id *current-durable-turn*)))))
                           request-json)
                       (nlk:record-turn-provider-request
                       *current-durable-turn*
                       :provider provider-id
                       :model model-id
                       :provider-round *provider-round*
                       :transport-attempts provider-attempt
                       ;; The bytes the session's previous request already
                       ;; sent, and where this one first differs — what
                       ;; separates a harness prefix leak from the
                       ;; provider's own cache miss.
                       :prefix-shared-bytes prefix-shared-bytes
                       :prefix-diverged-in prefix-diverged-in
                       :provider-message-count
                       (length request-context.messages)
                       :body-sha256 (nlk::sha256-text request-json)
                       :estimated-context-tokens
                       (compiled-context-estimated-tokens context)
                       ;; No lisp lane has a per-request parallel-tool-call
                       ;; switch, so the effective value is always true (the
                       ;; Zig effectiveParallelToolCalls default arm).
                       :parallel-tool-calls-p t
                       :emitted-tool-calls-p (not (null (message-tool-calls
                                                         message)))
                       :stream-part-count cadence.part-count
                       :text-delta-count cadence.text-delta-count
                       :ttft-ms (stream-cadence-ttft-ms cadence)
                       :stream-duration-ms (stream-cadence-duration-ms
                                            cadence))
                       ;; The bytes themselves, as a delta (bodies.lisp).
                       (when (plusp (or *request-bodies-cap* 0))
                         (nlk:keep-request-body
                          (nlk:durable-turn-session-id *current-durable-turn*)
                          (nlk:durable-turn-turn-id *current-durable-turn*)
                          *provider-round* request-json :cap *request-bodies-cap*))
                       (setf *previous-request-json* request-json)))
                    ;; The context rides out as a fifth value: a failover
                    ;; this call performed must hold for the turn's later
                    ;; rounds — the next turn re-snapshots the selection.
                    (return (values message usage finish-reason
                                    (list :model response-model
                                          :effort response-effort
                                          :continued-from continued-from
                                          :resume-provider-config (and refusal-fallback-p
                                                                       selection-config))
                                    context)))))))
        (provider-error (e)
          (cond
            ;; A request that carried images is re-sent without them first:
            ;; a configuration that names no wire is not the images' fault,
            ;; nor an address nothing listens on, nor a second failure of the
            ;; same round.
            ((and (not images-withheld-p)
                  (not (typep e '(or provider-config-error provider-connection-refused)))
                  (withhold-images-and-retarget e))
             (setf images-withheld-p t))
            ;; Before the overflow arm, which it would otherwise match on
            ;; its needles: our own refusal is not the server's verdict, and
            ;; the estimate that produced it must never be observed as a
            ;; window ceiling. Nothing left to withhold means it stands.
            ((typep e 'provider-no-output-room)
             (seal-streamed-attempt (provider-error-detail e) "stood")
             (error e))
            ((and (< overflow-recoveries +max-overflow-recoveries+)
                  (provider-overflow-error-p e))
             ;; Ceiling evidence: the server measured this exact request as
             ;; over its window, and our calibrated estimate of it bounds
             ;; the window from above (estimate-grade — a later proven
             ;; floor beats it).
             (let ((provider-id config.provider) (model-id config.model))
               (when (nlk:store-open-p)
                 (nlk:observe-context-window-ceiling
                  provider-id model-id
                  (max 1 (round (* (compiled-context-estimated-tokens context)
                                   (context-estimate-ratio provider-id
                                                           model-id)))))))
             ;; One shared recovery step (terminal turn, else live-turn
             ;; rounds); nothing evictable means the rejection stands.
             ;; Step until it fits, not once: with an unknown window the
             ;; rejection arm is the ONLY eviction (no trigger fires), and a
             ;; model switch can leave the retained history many steps over
             ;; the new edge. Each step durably shrinks retained history —
             ;; monotone, so the loop terminates — and the cap keeps a
             ;; misclassified 400 from evicting a whole session before the
             ;; error is allowed to stand.
             (unless (evict-and-retarget) (error e))
             (incf overflow-recoveries))
            ;; On a candidate (the round has walked the chain), a failure no
            ;; retry cures is that candidate's verdict, not the round's: the
            ;; chain walks on, and only past its last entry does it stand.
            ;; Without this, a relay's 400 for a model it does not serve would
            ;; end a turn with the next entry untried.
            ((and fallback-tried
                  (not (retryable-provider-error-p e))
                  (try-fallback e))
             nil)
            ;; A refusal is the selection's verdict on this one request, not
            ;; on the turn: with a chain configured, its first candidate
            ;; answers this round and the next round is the selection's
            ;; again. Without a fallback, a refusal would be terminal
            ;; instead of trying the fallback.
            ((and (typep e 'provider-refused)
                  (null fallback-tried)
                  (try-fallback e))
             (setf refusal-fallback-p t)
             nil)
            ;; A slow start leaves for the candidate that stood behind its
            ;; attempt at once: retried, the same cold start would be
            ;; waited out again. With the chain spent it is a stall like any
            ;; other, and the ladder below retries it.
            ((and (typep e 'provider-slow-start)
                  (try-fallback e))
             nil)
            (t
             (unless (retryable-provider-error-p e)
               (seal-streamed-attempt (provider-error-detail e) "stood")
               (amend-terminal-failover-note e)
               (error e))
             ;; Route the failure to its budget by capture scope: :request
             ;; failures spend the per-stream-attempt request budget, every
             ;; other scope spends the stream budget and refills the request
             ;; one (the attempt.zig reset rule). Scope picks the budget,
             ;; never the verdict.
             (let* ((scope (provider-error-scope e))
                    (request-p (eq scope :request))
                    (scope-retry
                      (if request-p
                          (1+ request-retries-used)
                          stream-attempt))
                    ;; Past the rerun ceiling a cut attempt is continued,
                    ;; not re-run: no lane resumes a generation, so a re-run
                    ;; repeats everything from zero, while a continuation
                    ;; hands what streamed back as the model's own cut-off
                    ;; reply and asks it to go on (ATTEMPT-CONTEXT). Under
                    ;; the ceiling the re-run is the cheaper, cleaner
                    ;; prompt. Request-scope failures streamed nothing.
                    (continue-p (and (not request-p) (past-rerun-ceiling-p))))
               ;; A refused connection spends the request budget at once:
               ;; nothing listens there, and the next attempt hears the same.
               (let ((exhausted (or (typep e 'provider-connection-refused)
                                    (if request-p
                                        (>= request-retries-used config.max-request-retries)
                                        (>= (1- stream-attempt) config.max-stream-retries)))))
                 (cond
                   ((and exhausted (try-fallback e))
                    ;; The chain took the round: the failed attempt is sealed
                    ;; by the fallback itself (disposition rerun), the loop
                    ;; re-runs against the candidate, and the ladder below is
                    ;; NOT walked — no retry fact, no counters, no sleep.
                    nil)
                   (exhausted
                    ;; An exhausted budget keeps its last attempt too — and
                    ;; the failure says what survived.
                    (seal-streamed-attempt (provider-error-detail e) "stood")
                    (when (continuing-p)
                      (setf (provider-error-note e)
                            "(the stream budget is spent; what streamed across the attempts is kept)"))
                    (amend-terminal-failover-note e)
                    (error e))
                   (t
                    ;; The failed attempt may have streamed parts: seal them
                    ;; durably before the retry fact, under the attempt index
                    ;; that fact names, and join them to the continuation seal
                    ;; when the policy continues.
                    (multiple-value-bind (text thinking)
                        (seal-streamed-attempt (provider-error-detail e)
                                               (if continue-p "continued" "rerun"))
                      (when continue-p (extend-seal text thinking)))
                    (let ((delay (provider-retry-delay
                                  scope-retry
                                  config.retry-base-delay
                                  :retry-after (provider-error-retry-after e)
                                  :status (provider-error-status e))))
                      (announce-retry
                       e scope delay (provider-error-detail e)
                       scope-retry
                       (if request-p config.max-request-retries config.max-stream-retries)
                       (and continue-p t))
                      (if request-p
                          (incf request-retries-used)
                          (setf stream-attempt (1+ stream-attempt)
                                request-retries-used 0))
                      (incf provider-attempt)
                      ;; The announcement above already hydrated the rows a client
                      ;; keeps; voiding the cancel backstop's accumulation the same
                      ;; way clears only what it did not.
                      (void-partial-stream)
                      ;; The backoff itself is a cancellable wait: Esc during a
                      ;; long Retry-After pause interrupts the sleep instead of
                      ;; letting the retry fire first.
                      (nlk:with-cancellable-wait (*current-durable-turn*)
                        (sleep delay)))))))))))))))

;;; --- tool dispatch ------------------------------------------------------

(defvar *tool-result-metadata* nil
  "Metadata the executing tool attaches to its own durable result: NIL, or
a JSON-object hash table (\"image\", ... — the
turn-stream tool metadata contract; \"snippet\", the eval snippet's record, the
note it opened with, which the context engine's eviction stub projects).
RUN-TOOL-BODY binds it to NIL around each tool body, on whichever thread
runs it; a tool SETFs it, and the value rides the
TURN.TOOL_RESULT fact into both the live stream and replay.")

(defvar *executing-tool-call-id* nil
  "The provider call id of the tool call currently executing, or NIL outside
tool dispatch. The lisp analog of the Zig ToolContext.tool_call_id: a tool
body that registers durable side state keyed by its call (the background
shell registry) reads it here instead of widening every tool lambda list.")

(defvar *tool-call-failed* nil
  "Whether the tool call currently executing failed. RUN-TOOL-BODY binds it
to NIL around each tool body, on whichever thread runs it; FAILURE sets it,
and the value rides the TURN.TOOL_RESULT fact as \"failed\".")

;;; --- the snippet's record ----------------------------------------------------
;;; A snippet's result fact carries, under its metadata's \"snippet\" key, the note
;;; the snippet opened with, so a projection can print it its own way. Written
;;; only here, read by the context engine when the round that carried the
;;; snippet leaves the window (context.lisp, the eviction stub). A durable fact
;;; the stub projects byte-identically on every refold: no summariser, no
;;; model call, nothing the log did not already say.

(defun snippet-record (note)
  "The object a snippet's result fact carries under \"snippet\", or NIL for a snippet
that opened with no note: \"note\", the line the snippet opened with."
  (when note
    (let ((record (make-json-object)))
      (setf (gethash "note" record) note)
      record)))

(defun display-fact (key value)
  "Put VALUE on the result fact of the tool call this code runs under, under
KEY, and return VALUE: the display envelope a transcript reads a card from
— the snippet's record, a spawned child, an image — one object per key,
replaced rather than appended (RECEIPT is the appending lane). The envelope
rides the TURN.TOOL_RESULT fact into the live stream and replay alike, so
a card drawn from it is the same on both paths; keep what rides here small
and referential, since it is durable history. Outside a tool call — a
driver's own forms, a slash op — there is no fact to ride and nothing is
recorded."
  (when *executing-tool-call-id*
    (unless (hash-table-p *tool-result-metadata*)
      (setf *tool-result-metadata* (make-json-object)))
    (setf (gethash key *tool-result-metadata*) value))
  value)

(defun failure (text)
  "TEXT, the result of the tool call this code runs under, recorded as that
call's failure: the fact carries \"failed\", and a transcript draws the call
failed from it. For what knows a call failed without signalling — a tool's
own verdict, a hook's veto: (failure \"ERROR: refused ...\") in place of the
bare refusal. A body that signals is failed by the dispatch seam without it.
Outside a tool call there is no fact to ride, and nothing is recorded."
  ;; The flag, not the text, is the record: no reader parses `ERROR:' or an
  ;; `[exit N]' line back out of a result.
  (when *executing-tool-call-id*
    (setf *tool-call-failed* t))
  text)

(defun publish-snippet-record (note)
  "Attach the snippet's record to the result fact of the tool call evaluating
it, under the metadata's \"snippet\" key."
  (when-let (record (and *executing-tool-call-id* (snippet-record note)))
    (display-fact "snippet" record)))

(defparameter *receipt-limit* 64
  "How many receipts one key keeps on one result fact. A snippet that loops a
verb a thousand times says what it did in the first sixty-four; the fact
is not the place for the rest.")

(defun receipt (key value)
  "Append VALUE to the KEY list on the result fact of the tool call this
code runs under, and return VALUE: a record of what the call did, as many
per call as it did things. VALUE is JSON in the eval snippet's convention — a
keyword plist is an object, a vector an array, strings and numbers as they
are. The list rides the TURN.TOOL_RESULT fact into the live stream and
replay alike, as every key of the envelope does; past *RECEIPT-LIMIT*
entries a key takes no more. Outside a tool call — a driver's own forms, a
slash op — there is no fact to ride, and nothing is recorded.

The verbs a snippet calls stamp themselves under \"calls\", {:verb :source},
which is how a transcript names what a snippet of free Lisp did without
reading the Lisp (activity.lisp): a cell verb stamps the same way. A verb
stamps before it acts, and a stamp that acts (CALL-ACTS-P) is the act's
mark, the core's verb and a cell's alike: the snippet acted, so it is never
watched, and a watched snippet's run again, which only reads, refuses it
here (RERUN-WATCH) -- what reaches nothing outside the image acts too."
  (when (and (equal key "calls") (call-acts-p value))
    (setf *snippet-touched* t)
    (when *watch-running*
      (error "~a refused: a watched snippet only reads"
             (if (hash-table-p value) (gethash "verb" value) (getf value :verb)))))
  (when *executing-tool-call-id*
    (unless (hash-table-p *tool-result-metadata*)
      (setf *tool-result-metadata* (make-json-object)))
    (let ((kept (gethash key *tool-result-metadata*)))
      (unless (and (vectorp kept) (not (stringp kept)))
        (setf kept #()))
      (when (< (length kept) *receipt-limit*)
        (setf (gethash key *tool-result-metadata*)
              (concatenate 'vector kept (vector (plist-json value)))))))
  value)

(defparameter *receipt-source-limit* 1000
  "How many characters of a verb's source its call receipt keeps: a command
string's head names what it runs, and a heredoc's body is not a source.")

(defun receipt-source (source)
  "SOURCE — the path, command or id a call is about — as a receipt names
it: a string cut to *RECEIPT-SOURCE-LIMIT*, a pathname's namestring, a
number as it is, a list joined by spaces; NIL for anything else."
  (typecase source
    (string (if (> (length source) *receipt-source-limit*)
                (subseq source 0 *receipt-source-limit*)
                source))
    (pathname (namestring source))
    (number source)
    (cons (format nil "~{~a~^ ~}" source))))

(defun call-acts-p (stamp)
  "Whether STAMP, one call's receipt under \"calls\", says the call acts: it
names no family, or one that changes a file, the image or the world -- a
family *ACTIVITY-FAMILIES* does not group. A read stamps its family (help, a
recall, a search) and stays a read. Without this, nck:answer-choices, a
cell verb that only hands the running answer its buttons would be read as a
read and run again after every later snippet, putting an earlier question's
buttons on later answers."
  (let* ((word (if (hash-table-p stamp)
                   (gethash "family" stamp)
                   (and (consp stamp) (getf stamp :family))))
         (family (and (stringp word) (find-symbol (string-upcase word) '#:keyword))))
    (not (and family (getf (activity-family-plist family) :group)))))

(defun receipt-call (verb &optional source)
  "Stamp VERB's call on the running tool call's result fact under \"calls\"
(RECEIPT), with SOURCE — the path, command or id the call is about — cut to
*RECEIPT-SOURCE-LIMIT*. A verb stamps before it acts: the stamp names no
family, so it acts (CALL-ACTS-P), and a watched snippet's run again refuses
it there."
  (receipt "calls" (json-object "verb" verb :opt "source" (receipt-source source))))

;;; --- the round's calls ---------------------------------------------------
;;; A round's calls run one after another in model order, each body on the
;;; turn thread: a call's fact, then its result fact, then the next call's —
;;; the one shape the wire fold pairs and every provider accepts. One
;;; workspace bracket per call: the diff document rides its result, the
;;; checkpoint anchors that post-tree.

(defstruct (tool-run (:conc-name run-))
  "One call of a round through its dispatch: what preflight decoded, what
the body answered and whether it failed, and whether its result fact is
durable yet. Live-only."
  call-id name raw-arguments entry arguments refusal
  text settle metadata failed duration-ms closed started)

(nlk:access (run tool-run))

(defun prepare-tool-call (call context)
  "Decode one provider CALL against CONTEXT's frozen tool bindings — the
pure half of admission, no fact recorded, so the round's budget can meter
the decoded calls before any call's fact lands. => a TOOL-RUN
whose REFUSAL is the preflight's structured refusal text, or NIL when the
call may run."
  (bind ((function-spec (gethash "function" call))
         (name (and (hash-table-p function-spec) (gethash "name" function-spec)))
         (raw-arguments (and (hash-table-p function-spec)
                             (gethash "arguments" function-spec))) (call-id (gethash "id" call))
         (entry (and (stringp name) (find name context.tool-bindings :key #'first :test #'string=)))
         ((arguments refusal) (tool-call-preflight name raw-arguments entry)))
    (make-tool-run :call-id (if (stringp call-id) call-id "")
                   :name (if (stringp name) name "unknown")
                   :raw-arguments (if (stringp raw-arguments) raw-arguments "")
                   :entry entry :arguments arguments :refusal refusal)))

(defun run-tool-body (run turn &aux (t0 (get-internal-real-time))
                                    (settle nil)
                                    (*tool-result-metadata* nil)
                                    (*tool-call-failed* nil)
                                    (*executing-tool-call-id* run.call-id))
  "Run RUN's tool body on the current thread, the turn thread, and keep what
it answered: the TEXT, the
SETTLE thunk a body may return as its second value, the metadata the body
attached, whether it failed, and the measured wall time. The body is a
cancellable-wait region: Esc interrupts a parked or spinning body
immediately instead of waiting for it to return. There is no dispatch-seam deadline — long work is
the tool's own concern, and EVAL yields a still-running form to the
background-eval lifecycle (exec.lisp) at its yield window instead of
aborting it.

The :TOOL hook chain runs INSIDE this discipline on purpose: a hook that
signals fail-closes this call through the ERROR arm, a hook's refusal string
journals as an ordinary TURN.TOOL_RESULT — failed when the hook says so
(FAILURE), as the ERROR arm's text always is — and a cancel raised under a
hook rethrows like any tool's. The settle thunk never reaches the chain — a hook
sees and rewrites the text alone — and a call that failed has none. A
cancel is the turn's unwind, never a tool result the ERROR arm would
absorb: it leaves this function as the condition, and RUN-TOOL-CALL's cleanup
closes the call it cut."
  (setf run.started t0)
  (unwind-protect
       (setf run.text
             (handler-case
                 (nlk:with-cancellable-wait (turn)
                   (call-hooked
                    :tool
                    (list :name run.name :arguments run.arguments :call-id run.call-id)
                    (lambda (op)
                      (multiple-value-bind (text thunk)
                          (funcall (third run.entry)
                                   (tool-body-arguments run.entry
                                                        (getf op :arguments)))
                        (setf settle (and (functionp thunk) thunk))
                        text))))
               ;; First arm wins: a cancel is the turn's unwind, never a
               ;; tool result the ERROR arm would absorb.
               (nlk:turn-cancelled-condition (c) (error c))
               (error (e)
                 (setf settle nil)
                 (failure (format nil "ERROR: ~a: ~a" (type-of e) e))))
             run.settle settle)
    ;; One measurement, two consumers: the live phase note and the
    ;; durable fact carry the same number.
    (setf run.metadata (and (hash-table-p *tool-result-metadata*)
                                  *tool-result-metadata*)
          run.failed *tool-call-failed*
          run.duration-ms (round (* 1000 (- (get-internal-real-time) t0))
                                       internal-time-units-per-second))
    (note-phase "tool name=~a ms=~d" run.name run.duration-ms)))

(defun settle-tool-run (run)
  "Run RUN's settle thunk on the turn thread under the call's own result
metadata and failure bindings, so a receipt it stamps rides the result fact.
A settle that signals answers as the call's error, the body's text set
aside; a cancel raised in it is the turn's unwind."
  (when-let (settle run.settle)
    (let ((*executing-tool-call-id* run.call-id)
          (*tool-result-metadata* run.metadata)
          (*tool-call-failed* run.failed))
      (handler-case (funcall settle)
        (nlk:turn-cancelled-condition (c) (error c))
        (error (e)
          (setf run.text (failure (format nil "ERROR: ~a: ~a" (type-of e) e)))))
      (setf run.metadata (and (hash-table-p *tool-result-metadata*)
                                    *tool-result-metadata*)
            run.failed *tool-call-failed*))))

(defun run-tool-call (run turn)
  "Run one call of the round and answer its tool message: its call fact, its
body on the turn thread, its settle thunk, and its result fact, with the
workspace bracket and the checkpoint that surround the body.

From the call fact to the result fact the call stands open in the log, and
every exit from this function closes it: the reply records the tool's own
result, and an unwind that never reaches the reply — Esc, landing anywhere
in the window, the baseline walk included — or a store that refuses the
reply records `interrupted by user' for the call. A call left open is the
one shape every provider rejects on every later round; the fold answers it
on the wire (WIRE-MESSAGES-FROM-EVENTS), and this seam keeps the log from
needing that answer. Only a process death leaves a call open now, and boot
recovery closes those (RECOVER-INTERRUPTED-TURNS)."
  (when turn
    (nlk:ensure-turn-not-cancelled turn)
    (with-phase-note ("fact-call")
      (nlk:record-tool-call turn run.call-id run.name run.raw-arguments)
      (notify-turn-facts-committed)))
  (when run.refusal
    ;; Admission refused the call: malformed provider data consumes no file
    ;; walk, hook, or tool-side effect, and the structured refusal is its
    ;; result, a failed one.
    (when turn
      (with-phase-note ("fact-result")
        (nlk:record-tool-result turn run.call-id run.name
                                run.refusal :failed t)
        (setf run.closed t)
        (notify-turn-facts-committed)))
    (return-from run-tool-call
      (message "tool" run.refusal :name run.name :tool-call-id run.call-id)))
  (unwind-protect
       (progn
         ;; A cancel already flagged closes the call here, before the
         ;; baseline walk spends anything on it.
         (when turn (nlk:ensure-turn-not-cancelled turn))
         (multiple-value-bind (store baseline)
             (if turn
                 (with-phase-note ("baseline")
                   (nlk:open-workspace-bracket turn :label run.call-id))
                 (values nil nil))
           (when turn (nlk:ensure-turn-not-cancelled turn))
           ;; The stack this call was dispatched from, so anything a body
           ;; evaluates can ask what it is standing on — a background eval
           ;; included, whose own worker thread knows nothing of the turn
           ;; that asked for it (provisional.lisp).
           (let ((nlk:*caller-frames* (nlk:backtrace-names)))
             (run-tool-body run turn))
           ;; A body that unwound on the cancel left its text NIL; the flag
           ;; it unwound on is still set, and here it becomes the turn's
           ;; unwind.
           (when turn (nlk:ensure-turn-not-cancelled turn))
           (settle-tool-run run)
           (let ((post-tree nil)
                 (anchor nil))
             (when (and store baseline)
               ;; Close the bracket before the result records: a changed
               ;; tree becomes the workspace_diff display metadata riding
               ;; that result — error replies too, since a failed body may
               ;; still have written files. The checkpoint below anchors
               ;; this tree instead of walking.
               (multiple-value-bind (tree document)
                   (with-phase-note ("post-tree")
                     (nlk:close-workspace-bracket store baseline))
                 (setf post-tree tree)
                 (when document
                   (unless (hash-table-p run.metadata) (setf run.metadata (make-json-object)))
                   (setf (gethash "workspace_diff" run.metadata) document))))
             ;; The spend line rides the fact's metadata beside what the
             ;; tool said, and only the model's copy renders it.
             (let ((text run.text)
                   (metadata (spend-metadata run.metadata)))
               (when turn
                 (setf anchor
                       (with-phase-note ("fact-result")
                         (prog1 (nlk:record-tool-result
                                 turn run.call-id run.name text
                                 :duration-ms run.duration-ms
                                 :failed run.failed
                                 :metadata metadata)
                           (setf run.closed t)
                           (notify-turn-facts-committed)))))
               ;; Workspace checkpoint: the bracket's post-tree anchored and
               ;; recorded under the call, no walk of its own. Runs outside
               ;; any store transaction and never fails the turn.
               (when (and turn (not (string= run.call-id "")))
                 (with-phase-note ("checkpoint")
                   (nlk:checkpoint-capture-boundary
                    turn
                    :boundary-kind "tool_result"
                    :tool-call-id run.call-id
                    :pre-tree-sha baseline
                    :tree-sha post-tree
                    :anchor-position anchor)))
               (message "tool" (tool-result-text text metadata :seq anchor)
                        :name run.name
                        :tool-call-id run.call-id)))))
    ;; The unwind arm: the call fact is durable and no result fact followed.
    ;; The call closes the way a mid-stream cancel closes the calls it caught
    ;; in flight; without the result a row would read `running' in a cancelled
    ;; turn, live and on every replay, and the next round's wire would carry
    ;; an unanswered call. The turn is still open here; RUN's handler ends it
    ;; after this unwind.
    (when (and turn (not run.closed))
      ;; The model reads the words; the card, who stopped the call and what
      ;; it had written by then (CALL-STOP-DOCUMENT).
      (ignore-errors
       (nlk:record-tool-result turn run.call-id run.name "interrupted by user"
                               :metadata (when-let (stop (call-stop-document
                                                          turn run.call-id run.started))
                                           (json-object "stopped" stop))))
      (ignore-errors (notify-turn-facts-committed)))))

(defun execute-tool-calls (calls context &aux (turn *current-durable-turn*)
                                              (closing (turn-closing-p))
                                              (refused nil))
  "Run the round's CALLS — a vector in model order — and answer their tool
messages in that order: preflight decodes every call (pure, no fact), the
running turn's budget meters what preflight let through (METER-CALL: a call
past the budget answers its refusal and runs nothing), and each call runs in
turn (RUN-TOOL-CALL). A cancel already flagged refuses the round before
anything spends on it.

The budget's refusal gives the model one round to answer with what it has.
A round that calls a tool anyway, after an earlier round's calls were
refused, records its own refusals and then ends the turn."
  (when turn (nlk:ensure-turn-not-cancelled turn))
  (let ((runs (map 'list (lambda (call) (prepare-tool-call call context)) calls)))
    (dolist (run runs)
      (unless run.refusal (when-let (refusal (meter-call)) (setf run.refusal refusal refused t))))
    (prog1 (loop for run in runs
                 collect (run-tool-call run turn))
      ;; The turn's own unwind, as a flagged cancel would raise it at this
      ;; boundary: every call of the round has closed, and RUN records
      ;; the reason on the cancelled fact.
      (when (and closing refused turn)
        (error 'nlk:turn-cancelled-condition
               :turn-id (nlk:durable-turn-turn-id turn)
               :reason "budget spent: the round after the refusal called tools again")))))

(defun execute-tool-call (call context)
  "Run CALL alone: EXECUTE-TOOL-CALLS over a round of just it. => its tool
message."
  (first (execute-tool-calls (vector call) context)))

;;; --- the ambient freeze and the per-round working-set rebuild -----------
;;; The engine (context.lisp) is a pure function of a CONTEXT-REQUEST; these
;;; two seams are where the ambient state it must not read gets frozen: the
;;; live tool table, the mutable system prompt, the provider configuration
;;; specials, and the session's standing harness sections.

;;; --- the live turn's image attachments ---------------------------------
;;; A turn's images are live turn state. The durable log carries the text
;;; — the prompt's [Image #N] placeholders, a LOOK's one-line receipt — and
;;; the attachments belong to the turn they arrived in, for as many provider
;;; rounds as that turn runs. They are grouped by the message they arrived
;;; with: the turn's inputs (its start and its steers) under the NIL anchor,
;;; what a tool attached under that tool call's id, so the engine can place
;;; each behind the message it belongs to (context.lisp
;;; ATTACH-VISIBLE-IMAGES). Keyed by turn id because the writers (the
;;; gateway's start_turn admission and its steer dispatch, a LOOK on the
;;; tool's thread) run off the turn worker while the reader below runs on
;;; it; RUN forgets the entry in the same unwind-protect that finishes the
;;; turn.

(defvar *turn-images-lock* (bt2:make-lock :name "turn-images"))

(defvar *turn-images* (make-hash-table :test #'equal)
  "TURN-ID -> its attachments as (ANCHOR . IMAGES) groups in the order each
anchor's first image arrived: ANCHOR NIL for the turn's inputs, a tool call
id for what that call attached; IMAGES the (MEDIA-TYPE . BASE64) conses,
oldest first.")

(defun turn-images (turn-id)
  "TURN-ID's staged attachments: the (ANCHOR . IMAGES) groups, oldest group
first, each group's images oldest first."
  (when turn-id
    (bt2:with-lock-held (*turn-images-lock*)
      (loop for (anchor . images) in (gethash turn-id *turn-images*)
            collect (cons anchor (copy-list images))))))

(defun attach-turn-images (turn-id images &key anchor)
  "Append IMAGES to TURN-ID's attachments under ANCHOR: NIL, the default,
for an input's own attachments — a steer that lands mid-turn adds its own
images to the turn it steered, and the next round carries all of them —
or the id of the tool call attaching them, so they land behind that call's
result rather than on the prompt."
  (when (and turn-id images)
    (bt2:with-lock-held (*turn-images-lock*)
      (let* ((groups (gethash turn-id *turn-images*))
             (group (assoc anchor groups :test #'equal)))
        (if group
            (setf (cdr group) (append (cdr group) (copy-list images)))
            (setf (gethash turn-id *turn-images*)
                  (append groups (list (cons anchor (copy-list images))))))))))

(defun forget-turn-images (turn-id)
  "Drop TURN-ID's attachments. Their horizon is the turn, so the entry dies
with it rather than holding megabytes of base64 for the life of the image."
  (when turn-id
    (bt2:with-lock-held (*turn-images-lock*)
      (remhash turn-id *turn-images*))))

(defun withhold-turn-images (turn-id note-fn)
  "Replace TURN-ID's staged images with their notes, group by group
(context.lisp WITHHOLD-IMAGE-GROUPS): the rounds after a provider refused
them carry the note where each image was and never the refused bytes
again. => the images withheld, zero when none were staged."
  (if (null turn-id)
      0
      (bt2:with-lock-held (*turn-images-lock*)
        (multiple-value-bind (groups count)
            (withhold-image-groups (gethash turn-id *turn-images*) note-fn)
          (when (plusp count)
            (setf (gethash turn-id *turn-images*) groups))
          count))))

(defun turn-images-withheld-p (turn-id)
  "True once a provider has refused TURN-ID's images this turn — the mark
WITHHOLD-TURN-IMAGES leaves, read by LOOK to refuse the next image with
the reason rather than send it into the same refusal."
  (when turn-id
    (bt2:with-lock-held (*turn-images-lock*)
      (loop for (nil . images) in (gethash turn-id *turn-images*)
            thereis (and (find :withheld images :key #'car) t)))))

;;; --- the prompt a turn runs under -------------------------------------------
;;; The conversation's pinned prompt (context.lisp, the prompt a conversation
;;; keeps), read at a turn's first round: pinned there when nothing pins yet
;;; or an eviction moved the floor under the pin, and every change since the
;;; model was last told recorded on the running turn as the reminder the fold
;;; sends where it stands. A pin holds the standing sections alone: the live
;;; ones are told at the turn, the pin's own among them, so a room's lanes
;;; pin one prefix and part only at their own asks.

(defun current-prompt (session-id bindings)
  "=> (values BASE SECTIONS TOOLS STANDING): the prompt SESSION-ID would be
given if it began now — the system prompt, its standing and live sections,
and the payload of the tool BINDINGS — and of SECTIONS the standing ones,
the ones a pin holds."
  (let ((standing (kept-sections (read-harness-sections session-id))))
    (values (copy-seq *system-prompt*)
            (kept-sections (append standing (read-live-sections session-id)))
            (tools-payload bindings)
            standing)))

(defun turn-prompt (session-id bindings)
  "=> (values BASE SECTIONS TOOLS): the prompt SESSION-ID's turn runs under.
Its conversation's pinned prompt (NLK:SESSION-PROMPT-FACTS) — pinned now, on the running turn, when
none stands or an eviction moved the floor under it, and with every change
since the model was last told recorded there as the reminder the fold
sends, the live sections a new pin leaves out among them. With no store or
no running turn of SESSION-ID's nothing is recorded: the pin stands as it
is, or the prompt is the current one."
  (multiple-value-bind (base sections tools standing) (current-prompt session-id bindings)
    (let* ((store-p (and session-id (nlk:store-open-p)))
           (turn (and store-p *current-durable-turn*
                      (equal session-id (nlk:durable-turn-session-id *current-durable-turn*))
                      *current-durable-turn*))
           (prompt (and store-p (fold-prompt-facts (nlk:session-prompt-facts session-id))))
           (floor (if store-p (nlk:retained-history-floor session-id) 0)))
      (when turn
        (when (or (null prompt) (/= floor prompt.floor))
          ;; The standing sections alone: a lane's `where' or the notice
          ;; board in a pin is a head no sibling shares, and each new lane
          ;; of a room re-read the room's whole history behind it.
          (nlk:record-turn-context-prompt
           turn
           :pin t
           :base base
           :sections (map 'vector (lambda (section) (vector (car section) (cdr section)))
                          standing)
           :tools (or tools #())
           :floor floor)
          ;; What the fact reads back as is what every later request
          ;; sends, so this one sends it too.
          (setf prompt (fold-prompt-facts (nlk:session-prompt-facts session-id))))
        (when-let (change (prompt-change prompt base sections tools))
          (apply #'nlk:record-turn-context-prompt turn change)))
      (if prompt
          (values prompt.base prompt.sections (and (plusp (length prompt.tools)) prompt.tools))
          (values base sections tools)))))

(defun snapshot-context-request (session-id &key prior-context prompt tool-bindings
                                 messages history-turns (history-boundary 0))
  "Freeze one round's engine input into a CONTEXT-REQUEST.

Policy comes from PRIOR-CONTEXT after round zero — a turn's provider
config, tool payloads, base prompt and harness sections never drift
mid-turn — and from the ambient configuration at round zero, where the
prompt is the conversation's (TURN-PROMPT): PROMPT, the (BASE SECTIONS
TOOLS) the turn's first round already read with TOOL-BINDINGS, or read here.
This is the one seam that reads the specials; COMPILE-TURN-CONTEXT itself
is pure."
  (let* ((bindings (cond (prior-context prior-context.tool-bindings)
                         (tool-bindings)
                         (t (snapshot-tool-bindings))))
         (config (if prior-context
                     prior-context.provider-config
                     (snapshot-effective-provider-config)))
         (session-id (or session-id (and prior-context prior-context.session-id))))
    (destructuring-bind (base sections tools)
        (cond (prior-context (list prior-context.base-system-prompt
                                   prior-context.harness-sections
                                   prior-context.tools))
              (prompt)
              (t (multiple-value-list (turn-prompt session-id bindings))))
      (make-context-request
       :session-id session-id
       :provider-config config
       :base-system-prompt base
       :harness-sections sections
       :tools tools
       :tool-bindings bindings
       :history-turns history-turns
       :history-boundary history-boundary
       :history-floors (and session-id (nlk:store-open-p)
                            (nlk:retained-history-floors session-id))
       :messages messages
       ;; The live turn's attachments are ambient state too, and this is the
       ;; seam that freezes ambient state: the engine reads the frozen groups.
       :visible-images
       (turn-images (and *current-durable-turn*
                         (nlk:durable-turn-turn-id *current-durable-turn*)))))))

(defparameter +context-warning+
  "[context ~a of ~a, past the ~d% line: the next request evicts your oldest history unless it shrinks first. (context :drop SPANS :keep IDS :note TEXT) shrinks it in one call, and the note keeps what you still need. One warning until it shrinks.]"
  "What the model reads once its window crosses the eviction line: the size,
the line, and the one call that moves first.")

(defun warned-before-evicting-p (turns over window &aux (live *live-turn*)
                                                        (turn *current-durable-turn*))
  "T when this request warns the model instead of evicting: OVER, the
estimate past the eviction line (CHOOSE-HISTORY-BOUNDARY), came with no
warning since the window was last under it, in the running turn of TURNS,
whose session hydrates. The warning is a durable reminder fact, so every
refold sends it where it stood; a request under the line re-arms it."
  ;; The paper's harness never evicts unasked: a nudge, then rollback on
  ;; overflow (arXiv 2609.37725, facebookresearch/context-language-models).
  ;; Here the policy stays the backstop, one request later, and the provider's
  ;; overflow arm (EVICT-CONTEXT-ONE-STEP) still catches a window it misjudged.
  ;; A window already past whole has no room for the warning's round: the
  ;; request would come back refused, so the policy acts at once.
  (cond ((null live) nil)
        ((null over) (setf (live-turn-context-warned live) nil))
        ((or (live-turn-context-warned live) (null turn) (null turns)
             (not (realp window)) (> over window)
             (not (equal (history-turn-turn-id (car (last turns)))
                         (nlk:durable-turn-turn-id turn))))
         nil)
        (t (nlk:record-turn-context-prompt
            turn :text (format nil +context-warning+
                               (format-token-count (round over)) (format-token-count window)
                               (round (* 100 *context-eviction-trigger-ratio*))))
           (setf (live-turn-context-warned live) t))))

(defun hydrate-session (session &key tools system-prompt head budget)
  "Replace SESSION's heap working set with the runtime fold.

TOOLS, SYSTEM-PROMPT and HEAD (the head message), when supplied, are the
prompt the request will carry — the frozen compiled context's after round
zero, the turn's prompt at it (TURN-PROMPT) — so the eviction estimate
matches the request that will actually be sent; without them the live
globals apply (external callers —
this defaulting lives here, not in the engine, because a tool registered
mid-turn must not skew the eviction estimate for a request that will never
advertise it, and the engine reads no ambient state).

No-op when the store is closed, so isolated loop tests keep heap-only
history. When the store is open the log is the authority: stale heap
messages are discarded, not merged.

The BUDGET default rides the learned-window belief (a derived capability
number corrected by durable floor/ceiling evidence) and the estimates ride
the per-model calibration ratio, so the eviction trigger fires in the
server's own token scale. A live-turn relief persists its fact here and the
turn is folded again from the log, so this round sends what every later
fold of the fact reproduces."
  (check-type session session)
  (when (nlk:store-open-p)
    (bind ((turns (history-turns-for-session session))
           (floors (nlk:retained-history-floors (session-id session)))
           (floor (or (car (last floors)) 0))
           (calibration (context-estimate-ratio *provider* *model*))
           (window (or budget
                       (context-window-belief
                        *provider* *model*
                        (model-context-tokens *model*)
                        (model-context-tokens-operator-p *model*))))
           ((boundary live-plan over) (choose-history-boundary
                                       turns floor
                                       :budget window
                                       :tools (or tools (tools-payload))
                                       :system-prompt (or system-prompt *system-prompt*)
                                       :head head
                                       :calibration calibration
                                       :floors floors)))
      (when (warned-before-evicting-p turns over window)
        ;; Told, not evicted: this request carries the warning and all it
        ;; held, and the next one evicts unless the model shrank it.
        (setf turns (history-turns-for-session session)
              boundary (floor-boundary turns floor)
              live-plan nil))
      (when (> boundary (floor-boundary turns floor))
        (persist-history-boundary (session-id session) turns
                                  (floor-boundary turns floor) boundary))
      (when (and live-plan *current-durable-turn* turns
                 (equal (history-turn-turn-id (car (last turns)))
                        (nlk:durable-turn-turn-id *current-durable-turn*)))
        (multiple-value-bind (watermark fields)
            (live-relief-fields (car (last turns))
                                (getf live-plan :thin) (getf live-plan :drop))
          (when fields
            (apply #'nlk:record-turn-context-eviction
                   *current-durable-turn* watermark fields)
            ;; The fold that reads the fact is the relief, this round and
            ;; every one after it, so this round sends what the next refolds.
            (setf turns (history-turns-for-session session)))))
      (setf session.history-turns turns
            session.history-boundary boundary
            session.messages (messages-at-boundary turns boundary floors))))
  session)

;;; --- the loop -----------------------------------------------------------

(defun round-handed-off-work-p (calls)
  "T when any call in this round left its evaluation running in the
background — its form outlived the call's yield window and the harness will
wake the session with the result. The terminal message that follows such a
round is the model standing on a handoff, not an answer."
  (loop for call across calls
        thereis (call-background-running-p (gethash "id" call))))

(defun claim-turn-exits (session &aux (live *live-turn*)
                                      (share (and live (budget-share live))))
  "Claim the background exits SESSION's running turn reads at a round
boundary (CLAIM-EXIT-WAKES): its own evaluations', and those of turns that
completed before it. => their ids. NIL outside a durable turn, and once its
budget is spent: a read would be refused like any call, so the exit waits
for the idle wake."
  (when (and *current-durable-turn* (not (turn-closing-p)) (not (and share (>= share 1))))
    (claim-exit-wakes (exec-registry-instance) (session-id session)
                      (nlk:durable-turn-turn-id *current-durable-turn*))))

(defvar *replays-claimed* (make-hash-table :test #'equal :synchronized t)
  "Turn id -> T once this boot's resumed turn looked for its cut-off reads
(CLAIM-TURN-REPLAYS): one look per turn per boot.")

(defun turn-replay-arguments (turn-id &aux (events (nlk:events :turn-id turn-id))
                                           (calls (make-hash-table :test #'equal))
                                           (after 0))
  "The arguments of TURN-ID's cut-off reads, in the order the model made the
calls: each eval call the restart closed (+RESTART-TOOL-RESULT+) after the
turn's newest assistant message, whose snippet only reads
(REPLAY-SAFE-SNIPPET-P)."
  ;; After the newest assistant message: a replay round is an assistant
  ;; message of its own, so the reads it answered are behind it, and a
  ;; restart that cuts the replay itself off leaves its calls the newest.
  (dolist (event events)
    (typecase event
      (nlk::assistant-message (setf after (nlk:event-log-position event)))
      (nlk:tool-call-event (setf (gethash (nlk:tool-call-event-call-id event) calls) event))))
  (loop for event in events
        for call = (and (typep event 'nlk:tool-result-event)
                        (> (nlk:event-log-position event) after)
                        (equal nlk::+restart-tool-result+ (nlk:tool-result-event-result event))
                        (gethash (nlk:tool-result-event-call-id event) calls))
        when (and call (equal "eval" (nlk:tool-call-event-tool-name call))
                  (replay-safe-snippet-p (nlk:tool-call-event-arguments call)))
          collect (nlk:tool-call-event-arguments call)))

(defun claim-turn-replays (&aux (turn *current-durable-turn*)
                                (turn-id (and turn (nlk:durable-turn-turn-id turn))))
  "The cut-off reads the running turn runs again before its provider is
asked (TURN-REPLAY-ARGUMENTS), claimed once: only a turn this boot resumed
(*RESUMED-TURNS*) has any, and only at its first boundary. => their
arguments, or NIL."
  (when (and turn-id (not (turn-closing-p))
             (gethash turn-id *resumed-turns*)
             (not (shiftf (gethash turn-id *replays-claimed*) t)))
    (turn-replay-arguments turn-id)))

(defun replay-message (arguments)
  "A resumed turn's round decided for the model: one eval call per cut-off
read, each carrying the ARGUMENTS the model wrote, as a provider's assistant
message would carry them (EXIT-WAKE-MESSAGE's shape)."
  (make-json-object
   "role" "assistant"
   "content" :null
   "tool_calls" (coerce (loop for each in arguments
                              collect (chat-tool-call-object (nlk:make-durable-id "call") "eval" each))
                        'vector)))

(defun read-turn-exits (session ids &rest loop-arguments)
  "Go on with SESSION's turn from a round of the harness's own: one
(eval-status ID) call per claimed exit of IDS (EXIT-WAKE-MESSAGE), recorded
and run as a model's calls are, then AGENT-LOOP with LOOP-ARGUMENTS. However
the turn's loop ends, a claim whose read never ran -- the turn cut first, the
read refused -- goes back, so the idle wake still carries that exit."
  (unwind-protect
       (apply #'agent-loop session :prefill (exit-wake-message ids) loop-arguments)
    (release-exit-wakes (exec-registry-instance) ids)))

 (defun agent-loop (session &key (round 0) context handoff-p prefill)
   "Fold SESSION history through one immutable compiled turn context. => the
round's message, T as a second value when a parked steer ended the turn at a
boundary it would otherwise have continued past (COMPLETE-TURN :STEERED),
and T as a third when the terminal message is the model's first word after a
round that left work running in the background (HANDOFF-P): the word is
status, not the ask's answer, and the exit wake carries the result in.
PREFILL, when given, is this round's assistant message already decided --
a harness turn's calls (an exit wake's reads) -- and no provider is asked
for it: it is recorded and its calls run exactly as a provider's would be.
Background work that exited since the last boundary is read first, in a
round of the harness's own (READ-TURN-EXITS): before the provider is asked,
and where a terminal answer would end the turn."
   (when *current-durable-turn*
     (nlk:ensure-turn-not-cancelled *current-durable-turn*))
   ;; The boundary before a provider round: the turn's start, or the tool
   ;; batch just run. An exit that landed meanwhile is read first, so the
   ;; provider is asked with its value in hand.
   (unless prefill
     (when-let (ids (claim-turn-exits session))
       (return-from agent-loop
         (read-turn-exits session ids :round round :context context :handoff-p handoff-p)))
     ;; A read the restart cut off runs again first, in a round of the
     ;; harness's own, so the resumed provider is asked with its value in hand.
     (when-let (replays (claim-turn-replays))
       (return-from agent-loop
         (agent-loop session :prefill (replay-message replays)
                             :round round :context context :handoff-p handoff-p))))
   (sync-context-workspace session)
   ;; The :ROUND point (hook.lisp): a fact a hook records here -- a reminder,
   ;; NLK:RECORD-TURN-CONTEXT-PROMPT's :TEXT -- is in this round's request.
   ;; Isolated: a hook's own error is said and stepped over, never the turn's.
   (call-hooked :round (list :session-id (session-id session) :turn *current-durable-turn*
                             :round round)
                (constantly nil)
                :isolate t)
   ;; The turn's first round reads the conversation's prompt ahead of the
   ;; fold, so a change it records is in this round's history and its
   ;; estimate; later rounds keep the round before's.
   (let* ((bindings (unless context (snapshot-tool-bindings)))
          (prompt (unless context
                    (with-phase-note ("prompt")
                      (multiple-value-list (turn-prompt (session-id session) bindings))))))
     ;; Every provider round follows newly committed assistant/tool facts, so
     ;; context estimation and overflow recovery use the exact durable projection.
     (with-phase-note ("hydrate")
       (if context
           (hydrate-session session
                            :tools context.tools
                            :system-prompt context.system-prompt
                            :head context.head-message
                            ;; Later rounds read the learned budget through the one
                            ;; sanctioned drift seam: the frozen capability number
                            ;; is the prior, evidence observed THIS turn (a
                            ;; context-edge cut, a rejection ceiling) must reach
                            ;; the very next round's eviction estimate.
                            :budget (effective-context-budget context.provider-config))
           (destructuring-bind (base sections tools) prompt
             (multiple-value-bind (system-prompt head) (prompt-messages base sections)
               (hydrate-session session :tools tools :system-prompt system-prompt :head head)))))
     (let ((context (with-phase-note ("context-compile")
                      (compile-turn-context
                       (snapshot-context-request
                        (session-id session)
                        :prior-context context
                        :prompt prompt
                        :tool-bindings bindings
                        :messages session.messages
                        :history-turns session.history-turns
                        :history-boundary session.history-boundary)))))
       (multiple-value-bind (message usage finish-reason response-metadata
                               active-context)
           (if prefill
               (values prefill nil nil nil nil)
               (let ((*provider-round* round))
                 (call-provider context)))
         ;; A failover this round performed is the round's own identity: every
         ;; fact below — the usage fact's provider/model, the next round's
         ;; context, the tools this round dispatches — reads the config the
         ;; round ENDED with, never the selection it started from.
         (setf context (or active-context context))
         ;; Durable closure BEFORE the cancel check: this round's message
         ;; is fully streamed and fully billed whatever happens next.
         ;; Cancelling between the provider's return and the fact commit
         ;; used to lose the text AND the usage while still paying for
         ;; both — and boot resume then re-billed the identical round.
         (append-message session message)
         ;; The round into the turn's meter: the context it filled is read
         ;; against the budget this round's lane believed (the learned window).
         (note-round (string/= "" (content-text (gethash "content" message)))
                     usage
                     (effective-context-budget context.provider-config))
         (when *current-durable-turn*
           (nlk:record-assistant-message
            *current-durable-turn* message
            :continued-from (getf response-metadata :continued-from)
            ;; The pictures the answer names, read now for the transcript to
            ;; draw: the operator's, never a provider's.
            :metadata (when-let (images (answer-images (content-text (gethash "content" message))))
                        (json-object "images" images)))
           ;; finish_reason and usage are round evidence, not message content:
           ;; the message replays to the provider verbatim, the fact does not.
           (when (or usage finish-reason)
             (apply #'record-round-usage
              *current-durable-turn*
              :model (effective-provider-config-model
                      context.provider-config)
              ;; What the stream answered with, beside what the request asked
              ;; for: a relay is free to serve another model id, and the
              ;; divider must be able to say so rather than echo the ask.
              :response-model (getf response-metadata :model)
              ;; The frozen lane identity rides the same fact as the counts:
              ;; the finish divider names what THIS round bound, which a
              ;; later /models or /think must not retroactively rewrite.
              :provider (effective-provider-config-provider context.provider-config)
              ;; The effort the round RAN under: the provider's own report
              ;; when the stream carried one (it resolves an unset ask to its
              ;; own default), the requested effort otherwise.
              :reasoning-effort
              (or (getf response-metadata :effort)
                  (effective-provider-config-reasoning-effort context.provider-config))
              :finish-reason finish-reason
              (provider-usage-fact-args usage)))
           (notify-turn-facts-committed))
         ;; The cancel is honored only after the round durably closed.
         (when *current-durable-turn*
           (nlk:ensure-turn-not-cancelled *current-durable-turn*))
         (let ((calls (message-tool-calls message)))
           (cond
             (calls
              (dolist (message (execute-tool-calls calls context))
                (append-message session message))
              ;; A steer parked during this round ends the turn here, at the
              ;; boundary it would otherwise have continued past: the steer is
              ;; the operator's next turn (NLK:STEER-PARKED-P; dequeue promotes
              ;; it onto this one), and the results just recorded are where it
              ;; picks up. The second value marks the completion steered.
              (if (and *current-durable-turn*
                       (nlk:steer-parked-p *current-durable-turn*))
                  (values message t nil)
                  ;; A failover this round performed holds for the next
                  ;; round: ACTIVE-CONTEXT is the context the round ENDED with.
                  (agent-loop session :round (1+ round)
                              :context (next-round-context context response-metadata)
                              ;; The round just run is what a terminal answer
                              ;; would be standing on -- unless it was the
                              ;; harness's reads, which are not the model's
                              ;; word: the handoff before them still stands.
                              :handoff-p (if prefill
                                             handoff-p
                                             (round-handed-off-work-p calls)))))
             ;; A terminal answer ends the turn, unless background work
             ;; exited before it did: the turn reads the exits and the model
             ;; speaks again. A steer parked while it streamed is the next
             ;; turn, not another round of this one, and that turn reads them
             ;; first. HANDOFF-P rides out with it: a word spoken right after
             ;; work was handed off is status, not the ask's answer.
             (t (let ((ids (unless (and *current-durable-turn*
                                        (nlk:steer-parked-p *current-durable-turn*))
                             (claim-turn-exits session))))
                  (if ids
                      (read-turn-exits session ids :round (1+ round)
                                                   :context (next-round-context context response-metadata)
                                                   :handoff-p handoff-p)
                      (values message nil handoff-p))))))))))


;;; --- operator entry point -----------------------------------------------

(defvar *session* nil
  "The default session RUN uses when called without one.")

(defun current-session ()
  (or *session*
      (setf *session*
            (if (nlk:store-open-p)
                (intern-session (nlk:durable-session-id (nlk:create-session)))
                (make-session)))))

(defun bind-live-session (session)
  "With the store open, replace SESSION with the interned cache of its id."
  (check-type session session)
  (if (nlk:store-open-p)
      (intern-session (session-id session))
      session))

(defun turn-still-open-p (turn)
  "True when TURN exists and carries no durable terminal fact yet — the guard
both of RUN's terminal handler arms open with, so a turn some other path
already closed is never closed twice."
  (and turn
       (not (nlk::turn-terminal-p (nlk:durable-turn-turn-id turn)))))

(defun run (input &key (session (current-session)) command-id invocation admitted-turn
                       images prefill)
  "Run INPUT after durable admission, committing exactly one outcome.
IMAGES are the input's (MEDIA-TYPE . BASE64) attachments; they ride this
turn's provider rounds attached to its visible user message and are
forgotten with the turn. PREFILL is a harness turn's first round, already
decided (AGENT-LOOP): the turn opens on its calls rather than on a provider.
A durable per-session model selection outranks the process default for the
whole turn: the binding is dynamic, so it rides into AGENT-LOOP's context
compilation without threading arguments. The same selection's effort pick
outranks the default effort the same way while the turn's effective
(provider, model) target admits it (EFFECTIVE-EFFORT)."
  (setf session (bind-live-session session))
  (multiple-value-bind (session-provider session-model)
      (if (nlk:store-open-p)
          (nlk:session-model-selection (session-id session))
          (values nil nil))
   (let* ((session-effort (nlk:session-model-effort (session-id session)))
         (turn nil)
         (*current-durable-turn* nil)
         ;; Round zero's previous request is the turn before's (CALL-PROVIDER).
         (*previous-request-json* nil)
         (*request-bodies-cap* (request-bodies-cap))
         ;; The running turn's meter (meter.lisp): opened once the turn
         ;; is admitted, when its id and its budget can be read.
         (*live-turn* nil)
         (*provider* (or session-provider *provider*))
         (*model* (or session-model *model*))
         ;; The effort pick resolves AFTER the session model binding so it
         ;; addresses the turn's actual target.
         (*reasoning-effort*
           (effective-effort session-effort *provider* *model*)))
    (with-cleanup ((when turn
                     (forget-turn-images (nlk:durable-turn-turn-id turn))
                     ;; With the log open the working set is the next hydration's to
                     ;; rebuild, so it does not outlive the turn that folded it; the
                     ;; latch goes after it, so no turn admitted meanwhile loses its fold.
                     (when (nlk:store-open-p)
                       (forget session))) (finish-turn session))
      (admit session input)
      (setf turn (or admitted-turn
                     (nlk:admit-turn
                      (session-id session)
                      (or command-id (nlk::make-durable-id "command"))
                      input :invocation invocation))
            *current-durable-turn* turn)
      (attach-turn-images (nlk:durable-turn-turn-id turn) images)
      (setf *live-turn* (open-live-turn (session-id session)
                                        (nlk:durable-turn-turn-id turn)))
      (handler-case
          (progn
            ;; A home that chose no model has only the target the image was
            ;; built with, which nobody connected: its turn fails with the
            ;; far end's refusal as raw JSON (OpenCode's free tier now answers
            ;; only OpenCode). It fails here, saying what to do.
            (unless (or session-provider (default-model-configured-p))
              (error 'provider-config-error
                     :detail "no model is chosen yet: /setup connects a provider and picks one"))
            ;; A session keeps the model and the effort it began on: its first
            ;; turn pins the pair it runs and the effort it runs at, so a pick
            ;; in another session, which moves the default, reaches only
            ;; sessions not yet begun (SELECT-SESSION-MODEL, SET-SESSION-EFFORT).
            (unless (or (and session-provider session-model) (not (nlk:store-open-p)))
              (nlk:record-session-model-selection (session-id session)
                                                  :provider *provider* :model *model*
                                                  :effort (or session-effort *reasoning-effort*)))
            ;; The user_message workspace checkpoint: both admission
            ;; paths (in-process and gateway pre-admitted) converge
            ;; here, after the admission facts committed and outside
            ;; any store transaction — but INSIDE the terminal-outcome
            ;; handlers: from this point the turn is durably STARTED,
            ;; so any signal must reach FAIL-TURN rather than strand
            ;; an open turn that boot-resume would re-bill. A
            ;; boot-resumed turn already carries this capture from its
            ;; pre-crash run; re-stamping would leave duplicates whose
            ;; predecessor resolution is ambiguous for undo.
            (when nlk::*workspace-capture-enabled*
              (unless (nlk:turn-boundary-captured-p
                       (session-id session)
                       (nlk:durable-turn-turn-id turn)
                       "user_message")
                (nlk:checkpoint-capture-boundary
                 turn
                 :boundary-kind "user_message"
                 :anchor-position (nlk:session-log-cursor
                                   (session-id session))
                 ;; The walk overlaps the first provider request; the
                 ;; fact lands ahead of the first round's facts.
                 :defer t)))
            (multiple-value-bind (message steered handoff)
                (agent-loop
                 session
                 :prefill prefill
                 ;; A resumed turn continues its round progression:
                 ;; each completed round left a durable assistant
                 ;; closure, and the count is 0 for fresh turns.
                 :round (if (and turn (nlk:store-open-p))
                            (nlk:turn-assistant-round-count
                             (nlk:durable-turn-turn-id turn))
                            0))
              ;; Assistant messages are durable per provider round. A
              ;; parked steer ended the loop at a boundary it would
              ;; have continued past: the completion says so.
              (nlk:complete-turn turn nil
                                 :message-count (length session.messages)
                                 :steered steered
                                 ;; The turn ended on a word spoken while
                                 ;; its own background work was still in
                                 ;; flight: the completion says how much,
                                 ;; so every consumer knows that word is
                                 ;; not the ask's answer.
                                 :background-pending
                                 (and handoff
                                      (turn-background-pending
                                       (session-id session)
                                       (nlk:durable-turn-turn-id turn))))
              (notify-turn-facts-committed)
              ;; The assistant-boundary checkpoint is the only fence
              ;; that catches writes by tool-spawned threads after the
              ;; last tool round; a no-op capture allocates nothing.
              (nlk:checkpoint-capture-boundary
               turn
               :boundary-kind "assistant_message"
               :anchor-position (nlk:session-log-cursor
                                 (session-id session)))
              (values (message-content message) message session)))
        (nlk:turn-cancelled-condition (condition)
          (when (turn-still-open-p turn)
            ;; A cancel that unwound mid-stream still durably closes
            ;; the round with what streamed (the Zig
            ;; appendOpenAssistantMessageCompletionsLocked backstop):
            ;; the text was billed whether recorded or not, and
            ;; without the fact a resume re-bills the identical round.
            ;; Every block the stream created persists — text, the
            ;; thinking trace, and in-flight tool calls — in the same
            ;; wire shape a completed round records, so the transcript
            ;; keeps what the user watched arrive and replay stays
            ;; well-formed (each persisted call closes with an
            ;; interrupted result).
            (let ((streamed (nlk:turn-cancelled-condition-partial-text
                             condition))
                  (thinking (nlk:turn-cancelled-condition-partial-reasoning
                             condition))
                  (calls (nlk:turn-cancelled-condition-partial-tool-calls
                          condition)))
              (when (or (and (stringp streamed) (plusp (length streamed)))
                        (and (stringp thinking) (plusp (length thinking)))
                        calls)
                (ignore-errors
                  (let ((message
                          (json-object
                           "role" "assistant"
                           "content" (if (and (stringp streamed)
                                              (plusp (length streamed)))
                                         streamed
                                         "")
                           :when (and (stringp thinking)
                                      (plusp (length thinking)))
                           "reasoning_content" thinking
                           :when calls "tool_calls"
                           (map 'vector
                                (lambda (buf)
                                  (chat-tool-call-object
                                   (or (getf buf :id) "")
                                   (or (getf buf :name) "")
                                   (or (getf buf :arguments) "")))
                                calls))))
                    (nlk:record-assistant-message
                     turn message
                     :continued-from
                     (nlk:turn-cancelled-condition-partial-continued-from
                      condition))
                    (dolist (buf calls)
                      (nlk:record-tool-call
                       turn (or (getf buf :id) "")
                       (or (getf buf :name) "unknown")
                       (or (getf buf :arguments) ""))
                      (nlk:record-tool-result
                       turn (or (getf buf :id) "")
                       (or (getf buf :name) "unknown")
                       "interrupted by user"
                       :metadata (when-let (stop (call-stop-document turn (or (getf buf :id) "")))
                                   (json-object "stopped" stop)))))))
              ;; The cut round's usage, estimated: the server never
              ;; reported, but the spend was real and the resting
              ;; divider must still name the model, the effort and the
              ;; cost the way a completed turn's does.
              (when-let (cut (nlk:turn-cancelled-condition-partial-usage
                              condition))
                (ignore-errors
                  (apply #'record-round-usage turn
                         :estimated t cut))))
            (nlk:cancel-turn turn (nlk::turn-cancelled-condition-reason condition))
            (notify-turn-facts-committed))
          (error condition))
        (error (condition)
          (when (turn-still-open-p turn)
            (nlk:fail-turn turn condition
                           :evidence (provider-failure-evidence condition))
            (notify-turn-facts-committed))
          (error condition)))))))
