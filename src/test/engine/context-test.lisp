;;;; context-test.lisp --- focused proofs for the context engine (context.lisp).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three invariants the extraction must hold: the compile is a pure function
;;;; of its CONTEXT-REQUEST, successive round compiles are byte-stable prefixes
;;;; below the eviction trigger (the prompt-cache contract), and the operator
;;;; eviction op plans over the HEAD-VISIBLE fold — the raw-log planning bug a
;;;; forked session used to hit through the TUI slash path.

(in-package #:nodecode.test)

(nlk:access (again nodecode.evolved::compiled-turn-context)
            (first-compile nodecode.evolved::compiled-turn-context)
            (later nodecode.evolved::compiled-turn-context)
            (prior nodecode.evolved::compiled-turn-context)
            (second-compile nodecode.evolved::compiled-turn-context) (turn-a nlk::durable-turn))

(deftest context-pressure-keeps-unknown-windows-unknown ()
  (is-shape (nle::context-pressure-decision 100000 nil) (:status eq :unknown) (:budget null)))

(deftest context-pressure-marks-known-overage ()
  (is-shape (nle::context-pressure-decision 86 100) (:status eq :pressure) (:trigger = 85))
  (is (eq :ok (getf (nle::context-pressure-decision 85 100) :status))))

(deftest context-compile-is-pure (let* ((user (nle::message "user" "hello"))
                                        (request (nle::make-context-request
                                                  :session-id nil
                                                  :provider-config (test-provider-config)
                                                  :base-system-prompt "BASE"
                                                  :harness-sections '(("tone" . "stay terse"))
                                                  :messages (list user)))
                                        (first-compile (nle::compile-turn-context request))
                                        (second-compile (nle::compile-turn-context request))))
  ;; The request object is built once and compiled twice on purpose: purity is
  ;; a claim about one request, so LOCAL-CONTEXT — which builds a fresh
  ;; request per call — cannot state it.
  (is-shape first-compile (.messages equalp second-compile.messages)
    (.system-prompt second-compile.system-prompt) (.head-message equalp second-compile.head-message)
    (.system-prompt "BASE" "a section not the harness's own is no system text"))
  (is (search "<harness key=\"tone\">" (head-text first-compile)))
  (is (eq user (aref first-compile.messages 0))))

(defun session-round-context (session &rest keys)
  "SESSION's next round compiled over its hydrated window; KEYS reach
SNAPSHOT-CONTEXT-REQUEST."
  (nle::compile-turn-context
   (apply #'nle::snapshot-context-request (nle:session-id session)
          :messages session.messages :history-turns session.history-turns
          :history-boundary session.history-boundary keys)))

(deftest context-compile-prefix-stable-across-rounds (with-temp-store ())
  (seed-completed-turn "ctx-prefix" "one" "alpha")
  (seed-completed-turn "ctx-prefix" "two" "beta")
  (let ((nle::*api-key* "k")
        (session (intern-durable-session "ctx-prefix")))
    (nle:hydrate-session session)
    (flet ((wire (message) (shasht:write-json message nil)))
      (let* ((round-0 (session-round-context session))
             (before round-0.messages))
        (seed-completed-turn "ctx-prefix" "three" "gamma")
        (nle:hydrate-session session :tools round-0.tools :system-prompt round-0.system-prompt)
        (let ((after (nle::compiled-turn-context-messages
                      (session-round-context session :prior-context round-0))))
          (is (< (length before) (length after)))
          (is (every (lambda (old new) (equal (wire old) (wire new))) before after)))))))

(deftest operator-eviction-plans-over-visible-fold (with-temp-store ())
  (let ((turn-a (seed-completed-turn "ctx-evict-fork" "first" "alpha")))
    (seed-completed-turn "ctx-evict-fork" "second" "beta")
    ;; Rewind the head to turn A — the second turn leaves the visible
    ;; chain — then grow a new branch from A. The raw log now holds three
    ;; turns; the head-visible fold holds two.
    (nlk::navigate-session-head "ctx-evict-fork"
                                turn-a.turn-id)
    (seed-completed-turn "ctx-evict-fork" "third" "gamma")
    (multiple-value-bind (plan floor event-id)
        (nle::evict-session-context "ctx-evict-fork" :percent 50)
      (is plan "a forked session still plans an eviction")
      (is-shape plan (:evicted-turn-count = 1) (:retained-turn-count = 1) (:requested-percent = 50))
      (is (and (stringp event-id) (plusp (length event-id))))
      (is (and (integerp floor)
               (= floor (getf plan :retained-from-sequence)))))))

;;; --- the visible turn's image attachments ---------------------------------
;;; Images belong to the message they arrived with: an input's attachments
;;; join the newest user message, ahead of nothing and behind the text they
;;; were staged with; what a tool attached (LOOK) rides behind the round of
;;; tool results its call sits in, as one user message of image parts.

(defun images-compiled (messages images)
  "The messages a context compiled over MESSAGES with IMAGES visible carries."
  (nle::compiled-turn-context-messages (local-context messages :visible-images images)))

(deftest context-compile-attaches-images-to-visible-user-message ()
  (let* ((messages (list (nle::message "user" "older")
                         (nle::message "assistant" "sure")
                         (nle::message "user" "[Image #1] what is this?")))
         (compiled (images-compiled messages '((nil ("image/png" . "QUJD"))))))
    (is-let* ((parts (nle::message-content-parts (gethash "content" (aref compiled 2)))))
      ((gethash "content" (aref compiled 0)) "older") ((gethash "content" (aref compiled 1)) "sure")
      ((length parts) = 2 "the visible message carries text plus one image")
      ((mapcar #'nle::content-part-type parts) '("text" "image_url"))
      ((gethash "text" (first parts)) "[Image #1] what is this?")
      ((gethash "url" (gethash "image_url" (second parts))) "data:image/png;base64,QUJD")
      ((gethash "content" (third messages)) "[Image #1] what is this?"))))

(deftests context-compile (messages images total content)
  (let ((compiled (images-compiled messages images)))
    (is (= total (length compiled)) "no message is invented to carry the images")
    (is (equal content (gethash "content" (aref compiled 0))) "the first message keeps its text"))
  (without-images-keeps-string-content (list (nle::message "user" "hello")) nil 1 "hello")
  (drops-images-with-no-user-message
   (list (nle::message "assistant" "sure")) '((nil ("image/png" . "QUJD"))) 1 "sure")
  (drops-a-tool-image-whose-call-left-the-window
   (list (nle::message "user" "look at it") (nle::message "assistant" "nothing to see"))
   '(("call-gone" ("image/png" . "QUJD"))) 2 "look at it"))

(defun tool-round-messages ()
  "A prompt, an assistant round of two tool calls and their two results:
the window a LOOK's image lands in at the round after the call."
  (list (nle::message "user" "look at it")
        (nle::message "assistant" ""
                      :tool-calls (vector (wire-tool-call "call-look" "eval"
                                                          "{\"form\":\"(look \\\"a.png\\\")\"}")
                                          (wire-tool-call "call-other" "eval"
                                                          "{\"form\":\"(+ 1 2)\"}")))
        (nle::message "tool" "[image: a.png, image/png 1x1, 70 bytes; shown to you at the next round, after this result]"
                      :name "eval" :tool-call-id "call-look")
        (nle::message "tool" "3" :name "eval" :tool-call-id "call-other")))

(deftest context-compile-places-a-tool-image-behind-its-round-of-results ()
  (is-let* ((compiled (images-compiled (tool-round-messages)
                                       '(("call-look" ("image/png" . "QUJD")))))
            (image-message (aref compiled 4))
            (parts (nle::message-content-parts (gethash "content" image-message))))
    ((length compiled) = 5) ((gethash "content" (aref compiled 0)) "look at it")
    ((gethash "tool_call_id" (aref compiled 2)) "call-look")
    ((gethash "tool_call_id" (aref compiled 3)) "call-other")
    ((gethash "role" image-message) "user") ((nle::content-part-type (first parts)) "image_url")
    ((length parts) = 1 "the message carries the image alone")
    ((gethash "url" (gethash "image_url" (first parts))) "data:image/png;base64,QUJD")))

(deftest context-compile-places-a-tool-image-ahead-of-the-round-that-follows ()
  ;; With another round already behind the looking one, the image sits
  ;; between the rounds — behind its own round's results, ahead of the
  ;; next assistant message — not at the tail of the window.
  (let* ((messages (append (tool-round-messages)
                           (list (nle::message "assistant" "" 
                                               :tool-calls (vector (wire-tool-call "call-later" "eval" "{\"form\":\"1\"}")))
                                 (nle::message "tool" "1" :name "eval" :tool-call-id "call-later"))))
         (compiled (images-compiled messages '(("call-look" ("image/png" . "QUJD"))))))
    (is (= 7 (length compiled)) "one message joins the window")
    (is (equal "user" (gethash "role" (aref compiled 4))))
    (is (vectorp (gethash "tool_calls" (aref compiled 5))))))

(deftest context-compile-places-an-input-image-and-a-tool-image-each-with-its-own ()
  (is-let* ((compiled (images-compiled (tool-round-messages)
                                       '((nil ("image/png" . "QUJD"))
                                         ("call-look" ("image/jpeg" . "REVG")))))
            (prompt-parts (nle::message-content-parts (gethash "content" (aref compiled 0))))
            (tail-parts (nle::message-content-parts (gethash "content" (aref compiled 4)))))
    ((length compiled) = 5 "the input's image joins the prompt, the tool's joins the tail")
    ((gethash "url" (gethash "image_url" (second prompt-parts))) "data:image/png;base64,QUJD")
    ((gethash "url" (gethash "image_url" (first tail-parts))) "data:image/jpeg;base64,REVG")))

(deftest every-lane-carries-a-tool-image-behind-its-round ()
  ;; The user-message shape is the one every lane already carries: the
  ;; chat wire as image_url parts, Anthropic as an image block merged into
  ;; the round's user turn behind every tool_result, GenAI as inlineData,
  ;; Responses as input_image. No lane learned anything for LOOK.
  (flet ((context (provider)
           (local-context (tool-round-messages)
                          :provider-config (test-provider-config :provider provider)
                          :visible-images '(("call-look" ("image/png" . "QUJD"))))))
    (is-let* ((messages (gethash "messages" (nle::request-body (context "openai-completions"))))
              (last (alexandria:last-elt messages))
              (image (gethash "image_url" (aref (gethash "content" last) 0))))
      ((length messages) = 6) ((gethash "role" last) "user" "chat: the image is a user message")
      ((gethash "url" image) "data:image/png;base64,QUJD"))
    (is-let* ((messages (gethash "messages" (nle::anthropic-request-body (context "anthropic"))))
              (blocks (gethash "content" (alexandria:last-elt messages))))
      ((length messages) = 3)
      ((map 'list (lambda (block) (gethash "type" block)) blocks)
       '("tool_result" "tool_result" "image") "anthropic: two tool_results, then the image")
      ((gethash "data" (gethash "source" (aref blocks 2))) "QUJD"))
    (is-let* ((contents (gethash "contents" (nle::google-request-body (context "google"))))
              (last (alexandria:last-elt contents)))
      ((gethash "role" last) "user" "google: the image is a user content")
      ((gethash "data" (gethash "inlineData" (aref (gethash "parts" last) 0))) "QUJD"))
    (is-let* ((input (gethash "input" (nle::responses-request-body (context "openai-responses"))))
              (last (alexandria:last-elt input))
              (part (aref (gethash "content" last) 0)))
      ((gethash "role" last) "user" "responses: the image is a user item")
      ((gethash "type" part) "input_image" "responses: an input_image part")
      ((gethash "image_url" part) "data:image/png;base64,QUJD"))))

;;; =========================================================================
;;; Wire-safe tool arguments (the truncated-call replay-poisoning class)
;;; =========================================================================
;;; A second output-length cut in one call can land mid-arguments; the
;;; journal keeps the truncated string as raw truth, but strict template
;;; engines (llama.cpp / nlohmann) parse tool-call arguments while rendering
;;; INCOMING history. Without the fold, replaying an unparseable string would
;;; fail every later round and brick the session. The fold is the single
;;; wire authority: nothing unparseable may leave it, and nothing parseable
;;; may be altered by it.

(deftest wire-safe-arguments-pass-valid-json-untouched ()
  (dolist (text (list "{\"form\":\"(+ 1 2)\"}" (format nil " {\"a\": 1}~%")))
    (is (eq text (nle::wire-safe-tool-arguments text)))))

(deftest wire-safe-arguments-replace-unparseable-strings ()
  (let* ((cut "{\"form\": \"(let* ((py \\\"import math\\nfrom PIL")
         (stand-in (nle::wire-safe-tool-arguments cut)))
    (is (not (equal cut stand-in)) "the truncated string is replaced")
    (is (hash-table-p (ignore-errors (shasht:read-json stand-in t nil t))))
    (is (search "(let* ((py" (gethash "truncated_arguments"
                                      (shasht:read-json stand-in))))
    (is (equal stand-in (nle::wire-safe-tool-arguments cut))))
  (is (not (equal "{\"a\":1}garbage"
                  (nle::wire-safe-tool-arguments "{\"a\":1}garbage"))))
  (is (equal "{}" (nle::wire-safe-tool-arguments "")))
  (is (< (length (nle::wire-safe-tool-arguments
                  (format nil "{\"form\": \"~a" (make-string 5000 :initial-element #\x))))
         1000)))

(defun record-tool-round (turn call-id arguments result &key reasoning metadata)
  "Record on TURN one provider round calling eval once as CALL-ID with
ARGUMENTS, as the real loop records it: the assistant message carrying the
call (and REASONING, when given), the call fact, and the RESULT fact carrying
METADATA."
  (nlk:record-assistant-message
   turn (nlk:json-object "role" "assistant" "content" ""
                         "tool_calls" (vector (wire-tool-call call-id "eval" arguments))
                         :opt "reasoning_content" reasoning))
  (record-tool-use turn call-id "eval" arguments result :metadata metadata))

(defun record-reply (turn content &rest fields)
  "Record on TURN an assistant message of CONTENT, FIELDS (key value...) beside it."
  (nlk:record-assistant-message
   turn (apply #'nlk:make-json-object "role" "assistant" "content" content fields)))

(deftest fold-replaces-truncated-stored-tool-arguments (with-temp-store ())
  (let ((broken "{\"form\": \"(let* ((py \\\"import math\\nfrom PIL"))
    ;; The journal shape: the once-per-call length-cut cap fell
    ;; through to the record-as-is path, storing an assistant message whose tool
    ;; call carries truncated arguments, plus the paired error result.
    (nlk:create-session :id "wire-safe-stored")
    (with-durable-turn (turn "wire-safe-stored" :input "draw an animation" :complete t)
      (record-tool-round turn "call-cut" broken "ERROR: unparseable arguments"))
    (is-let* ((folded (nle::wire-messages-from-events
                       (nle::collect-session-events "wire-safe-stored")))
              (assistant (find-if (lambda (m) (nlk:json-value m :array "tool_calls")) folded))
              (call (aref (gethash "tool_calls" assistant) 0))
              (arguments (gethash "arguments" (gethash "function" call))))
      ((gethash "id" call) "call-cut") ((gethash "name" (gethash "function" call)) "eval")
      ((hash-table-p (ignore-errors (shasht:read-json arguments t nil t))) is)
      ((equal broken arguments) not) ((wire-row "tool" folded "role") is))
    ;; The journal keeps the raw truth: the stored payload is not rewritten.
    (let* ((event (session-event "wire-safe-stored"
                                 :type 'nlk::assistant-message))
           (stored (gethash "message" (nlk::message-event-payload event)))
           (call (aref (gethash "tool_calls" stored) 0)))
      (is (equal broken (gethash "arguments" (gethash "function" call)))))))

(deftest fold-sanitizes-synthetic-flush-and-keeps-valid-bytes (with-temp-store ())
  ;; Entry point two: call/result facts journaled without a stored assistant
  ;; message flush as a synthetic tool-call message. Broken arguments are
  ;; replaced there too; valid arguments ride through byte-identical.
  (let ((valid "{\"form\":\"(+ 1 2)\"}")
        (broken "{\"form\": \"(loop"))
    (seed-completed-turn "wire-safe-flush" "hi" "done"
                         :tools (list (list "call-ok" "eval" valid "3")
                                      (list "call-x" "eval" broken
                                            "ERROR: unparseable arguments")))
    (let* ((folded (nle::wire-messages-from-events
                    (nle::collect-session-events "wire-safe-flush")))
           (calls (loop for m in folded
                        append (coerce (nlk:json-value m :array "tool_calls") 'list)))
           (ok (wire-row "call-ok" calls))
           (cut (wire-row "call-x" calls)))
      (is (equal valid (gethash "arguments" (gethash "function" ok))))
      (let ((arguments (gethash "arguments" (gethash "function" cut))))
        (is (hash-table-p (ignore-errors
                            (shasht:read-json arguments t nil t))))
        (is (not (equal broken arguments)))))))

(deftest request-body-arguments-parse-after-a-truncated-call (with-temp-store ())
  ;; The integration seam the incident died on: hydrate the poisoned store,
  ;; compile, and prove every tool-call arguments string in the outgoing
  ;; chat-lane body parses — what llama.cpp's template engine enforces.
  (nlk:create-session :id "wire-safe-body")
  (with-durable-turn (turn "wire-safe-body" :input "draw an animation" :complete t)
    (record-tool-round turn "call-cut" "{\"form\": \"(let* ((py \\\"import math"
                       "ERROR: unparseable arguments"))
  (let ((nle::*api-key* "k")
        (session (nle:intern-session "wire-safe-body")))
    (nle:hydrate-session session)
    (let* ((body (nle::request-body
                  (local-context session.messages)))
           (wire (gethash "messages" body)))
      (is (loop for message across wire
                always (loop for call across (or (nlk:json-value message :array "tool_calls") #())
                             always (ignore-errors
                                      (shasht:read-json
                                       (nlk:json-value call :string "function" "arguments")
                                       t nil t)
                                      t)))))))

;;; =========================================================================
;;; Within-turn eviction (the userless-request / resurrection incident class)
;;; =========================================================================

(defun hydrate-thin (session budget &key refold)
  "HYDRATE-SESSION under BUDGET with no tools and a one-letter prompt: the
window a within-turn eviction test thins. REFOLD first drops the session's
hydration cache, so the fold is rebuilt from the log alone."
  (when refold (nle::invalidate-session-hydration-cache (nle:session-id session)))
  (nle:hydrate-session session :budget budget :tools #() :system-prompt "S"))

(defun eviction-stub (messages)
  "The turn-eviction stub among MESSAGES, or NIL."
  (find "[turn history evicted:" messages :test #'uiop:string-prefix-p
        :key (lambda (message) (nlk:json-value message :string "content"))))

(defun seed-live-tool-round (turn round &optional record)
  "One completed provider round on the live TURN: an assistant message
carrying its tool call (the stored-tool-calls wire shape the real loop
records), the call fact, and a bulky result — whose fact carries RECORD as its
snippet record, when given."
  (record-tool-round turn (format nil "call-~d" round) "{}"
                     (format nil "round ~d ~a" round
                             (make-string 400 :initial-element #\x))
                     :metadata (and record (nle::make-json-object "snippet" record))))

(defun message-content-texts (messages)
  (loop for message in messages
        when (hash-table-p message)
          collect (nle::content-text (gethash "content" message))))

(defmacro with-live-turn ((turn session session-id command-id &key (rounds 0) (thick 0) whole limit)
                          &body body)
  "The live-turn eviction fixture: a store holding SESSION-ID, TURN admitted
on it under COMMAND-ID as the current turn with ROUNDS tool rounds seeded,
SESSION its interned session, and a fresh calibration table."
  ;; THICK rounds of SEED-THICK-ROUND follow the ROUNDS, a relief keeping the
  ;; newest WHOLE of them whole.
  ;; LIMIT then hydrates SESSION under that budget (HYDRATE-THIN).
  `(with-session-store (,session-id)
     (with-durable-turn (,turn ,session-id :command-id ,command-id
                               :input "the task" :current t)
       (let ((nle::*context-estimate-calibration* (make-hash-table :test #'equal))
             ,@(when whole `((nle::*live-whole-rounds* ,whole)))
             (,session (nle:intern-session ,session-id)))
         (dotimes (round ,rounds) (seed-live-tool-round ,turn round))
         (dotimes (round ,thick) (seed-thick-round ,turn round))
         ,@(when limit `((hydrate-thin ,session ,limit)))
         ,@body))))

(deftest live-turn-round-eviction-keeps-the-user-anchor ()
  (with-live-turn (turn session "live-thin" "thin-command" :rounds 3 :limit 200)
    (let* ((messages session.messages)
           (contents (message-content-texts messages)))
      (is (find-if #'nle::user-role-message-p messages))
      (is (eviction-stub messages))
      (is (rows-carry-p "round 2" contents))
      (is (not (rows-carry-p "round 0" contents))))
    (is (= 1 (count-session-events "live-thin"
                                   "turn.context.evicted")))
    (is (zerop (nlk:retained-history-floor "live-thin")))
    ;; Refold from the log: the durable fact alone reproduces the
    ;; thinning, and a stable window plans nothing new.
    (hydrate-thin session 200 :refold t)
    (is (= 1 (count-session-events "live-thin"
                                   "turn.context.evicted")))
    (is (not (rows-carry-p "round 0" (message-content-texts
                                      session.messages))))))

(deftests evicted-rounds-stay-out-when-the-turn-continues (session-id refold-p)
  (with-live-turn (turn session session-id "continue-command" :rounds 3 :limit 200)
    (is (= 1 (count-session-events session-id "turn.context.evicted")))
    ;; The turn continues past its own eviction: a new round lands above the
    ;; watermark.
    (seed-live-tool-round turn 3)
    ;; A huge budget plans nothing new, so what keeps the evicted rounds out
    ;; is the fold (or the cache the fact rewound), never a second eviction.
    (hydrate-thin session 1000000 :refold refold-p)
    (let ((contents (message-content-texts session.messages)))
      (is (rows-carry-p "round 3" contents) "the round that landed is in the fold")
      (is (not (rows-carry-p "round 0" contents)))
      (is (eviction-stub session.messages))
      (is (= 1 (count-session-events session-id "turn.context.evicted")))))
  ;; The open unit rides the hydration cache between rounds, and an eviction
  ;; is the one thing that rewrites messages it already holds: the stub lands
  ;; behind the user anchor and the oldest rounds leave. Nothing invalidates
  ;; the cache here — the fact in the tail has to rewind it by itself, or the
  ;; evicted rounds sail on in the cached unit and the request the model
  ;; answered is not the one the log says it answered.
  (through-the-cache "live-cached" nil)
  ;; The F1 regression: the floor-based design counted a straddling live
  ;; turn as retained again the moment new events grew its end past the
  ;; floor, resurrecting every evicted message and re-evicting each round.
  (through-a-refold "live-grow" t))

(deftests choose-history-boundary (end-sequence rounds round-starts budget live-plan-check)
  (nlk:bind ((live (nle::make-history-turn
                    :turn-id "live" :start-sequence 1 :end-sequence end-sequence
                    :messages (cons (nle::message "user" "task") rounds)
                    :round-starts round-starts
                    :terminal-p nil))
             ((boundary live-plan) (nle::choose-history-boundary (list live) 0 :budget budget
                                                                 :tools #() :system-prompt "S")))
    (is (= 0 boundary) "whole-turn eviction never touches the live turn")
    live-plan-check)
  (never-evicts-the-live-turn-whole
   2 (list (nle::message "assistant" (make-string 4000 :initial-element #\y))) (list 2) 50
   (is (null live-plan)))
  ;; rounds with nothing to thin: the relief comes to dropping them
  (plans-live-round-eviction
   9 (loop for tag in '("r0" "r1" "r2")
           collect (nle::message "assistant"
                                 (format nil "~a ~a" tag (make-string 400 :initial-element #\z))))
   (list 3 5 7) 100
   (is (<= 1 (getf live-plan :drop) 2))))

(deftest live-relief-fields-move-each-watermark-to-a-round-boundary ()
  (let* ((rounds (loop for i below 3
                       collect (nle::message "assistant" (format nil "r~d" i))
                       collect (nle::message "tool" (format nil "out~d" i) :name "eval"
                                                    :tool-call-id (format nil "c~d" i))))
         (live (nle::make-history-turn
                :turn-id "live" :start-sequence 10 :end-sequence 40
                :messages (cons (nle::message "user" "task") rounds)
                :round-starts (list 20 30 35)
                :terminal-p nil)))
    ;; thinning alone: the rounds stay, no round counts as gone
    (multiple-value-bind (watermark fields) (nle::live-relief-fields live 2 0)
      (is (= 0 watermark))
      (is-shape fields (:thinned = 34) (:evicted-round-count = 0) (:evicted-start-sequence = 0)))
    ;; a drop past the thinning: the thinned watermark never trails the other
    (multiple-value-bind (watermark fields) (nle::live-relief-fields live 1 2)
      (is (= 34 watermark))
      (is-shape fields (:thinned = 34) (:evicted-round-count = 2) (:evicted-message-count = 4)
        (:evicted-start-sequence = 10)))
    ;; nothing moved, or the newest round asked for: no fact
    (is (null (nle::live-relief-fields live 0 0)))
    (is (null (nle::live-relief-fields live 3 0)))))

(defun seed-thick-round (turn round)
  "One completed round on the live TURN the way a long game makes them: the
call, 800 characters of reasoning, and a 2,000-character result opening on
its round's own line."
  (record-tool-round turn (format nil "call-~d" round) "{}"
                     (format nil "result ~d~%~a" round (make-string 2000 :initial-element #\r))
                     :reasoning (format nil "thought ~d ~a" round
                                        (make-string 800 :initial-element #\t))))

(defun tool-texts (messages)
  "The content of each tool message among MESSAGES, in order."
  (loop for message in messages
        when (equal "tool" (gethash "role" message))
          collect (nle::content-text (gethash "content" message))))

(deftest a-relief-thins-older-results-before-any-round-leaves ()
  (with-live-turn (turn session "thin-first" "thin-first-command" :thick 12 :whole 4 :limit 10000)
    (let ((texts (tool-texts session.messages))
          (whole (make-string 2000 :initial-element #\r)))
      (is (= 1 (count-session-events "thin-first" "turn.context.evicted")))
      (is (not (eviction-stub session.messages)) "no round left")
      (is (= 12 (length texts)) "every round's result still stands")
      (is (every (lambda (text) (search "[thinned from 2,009 characters; (getf (recall-entry" text))
                 (subseq texts 0 8)))
      (is (every (lambda (text) (search whole text)) (subseq texts 8)) "the newest four whole")
      ;; no round the relief found standing replays its reasoning
      (is (notany (lambda (message) (gethash "reasoning_content" message)) session.messages)))
    ;; the fact alone reproduces the window, byte for byte, and plans nothing new
    (let ((before (mapcar #'nle::encode-json-object session.messages)))
      (hydrate-thin session 10000 :refold t)
      (is (equal before (mapcar #'nle::encode-json-object session.messages)))
      (is (= 1 (count-session-events "thin-first" "turn.context.evicted"))))))

(deftest a-thinned-result-names-the-entry-that-reads-it-whole ()
  (with-live-turn (turn session "thin-recall" "thin-recall-command" :thick 12 :whole 4)
    (hydrate-thin session 10000)
    (let* ((stand-in (first (tool-texts session.messages)))
           (seq (parse-integer stand-in :start (+ (search "recall-entry " stand-in)
                                                  (length "recall-entry "))
                                        :junk-allowed t)))
      (is (eql 0 (search "result 0" stand-in)) "the head leads")
      (is (equal (format nil "result 0~%~a" (make-string 2000 :initial-element #\r))
                 (getf (nlk:recall-entry seq) :text))))))

(deftest a-relief-drops-rounds-only-once-thinning-is-not-enough ()
  (with-live-turn (turn session "thin-drop" "thin-drop-command" :thick 12 :whole 4 :limit 3000)
    (let ((texts (tool-texts session.messages))
          (contents (message-content-texts session.messages)))
      (is (eviction-stub session.messages))
      (is (not (rows-carry-p "result 0" contents)) "the oldest round left")
      ;; the newest round stands whole
      (is (search (make-string 2000 :initial-element #\r) (car (last texts))))
      (is (search "[thinned from" (first texts)) "every round between stands thinned"))
    (let ((fact (nlk:events :session-id "thin-drop" :kind "turn.context.evicted"
                            :as :instance)))
      (is (plusp (nlk:turn-context-evicted-watermark fact)))
      (is (> (nlk:turn-context-evicted-thinned fact) (nlk:turn-context-evicted-watermark fact))))))

(deftest a-thinned-window-keeps-its-prefix-as-the-turn-goes-on ()
  (with-live-turn (turn session "thin-grow" "thin-grow-command" :thick 12 :whole 4 :limit 10000)
    (let ((before (mapcar #'nle::encode-json-object session.messages)))
      (seed-thick-round turn 12)
      (hydrate-thin session 10000)
      (let ((after (mapcar #'nle::encode-json-object session.messages)))
        ;; every message sent before the new round is sent again unchanged
        (is (equal before (subseq after 0 (length before))))
        (is (= 1 (count-session-events "thin-grow" "turn.context.evicted")))
        ;; the round after the relief replays its reasoning
        (is (= 1 (count-if (lambda (message) (gethash "reasoning_content" message))
                           session.messages)))))))

(deftest a-recovery-step-thins-before-it-drops ()
  (with-live-turn (turn session "thin-step" "thin-step-command" :thick 12 :whole 4)
    (flet ((step-once ()
             (nle::evict-context-one-step "thin-step" (nle::history-turns-for-session session) 0
                                          :live-turn turn)))
      (let ((turns (step-once)))
        (is turns)
        (is (search "[thinned from" (first (tool-texts (nle::history-turn-messages (car (last turns)))))))
        (is (not (eviction-stub (nle::history-turn-messages (car (last turns)))))))
      (let ((turns (step-once)))
        (is turns)
        ;; with nothing left to thin, half the rounds leave
        (is (eviction-stub (nle::history-turn-messages (car (last turns))))))
      (is (= 2 (count-session-events "thin-step" "turn.context.evicted"))))))

(deftest messages-at-boundary-re-splices-a-user-anchor ()
  ;; Defense in depth for the Qwen no-user-query 500: a retained tail that
  ;; carries no user message gets the newest evicted one back behind the
  ;; stub.
  (let* ((turns (list (nle::make-history-turn
                       :turn-id "a" :start-sequence 1 :end-sequence 2
                       :messages (list (nle::message "user" "anchor me")
                                       (nle::message "assistant" "old"))
                       :terminal-p t)
                      (nle::make-history-turn
                       :turn-id "b" :start-sequence 3 :end-sequence 4
                       :messages (list (nle::message "assistant" "tail"))
                       :terminal-p nil)))
         (messages (nle::messages-at-boundary turns 1)))
    (is (search "history evicted" (gethash "content" (first messages))))
    (is-shape (second messages) (nle::user-role-message-p is) ("content" "anchor me"))))

(defun visible-history-turns (session-id)
  "SESSION-ID's history turns, folded from the events its head can see."
  (nle::history-turns-from-events (nle::collect-visible-session-events session-id)))

;;; --- the learned window belief and the estimator calibration ---------------

(deftest context-window-belief-resolves-evidence-over-prior (with-temp-store ())
  (is (= 16000 (nle::context-window-belief "belief-p" "belief-m" 16000 nil)))
  (nlk:observe-context-window-floor "belief-p" "belief-m" 24575)
  (is (= 24575 (nle::context-window-belief "belief-p" "belief-m" 16000 nil)))
  (is (= 16000 (nle::context-window-belief "belief-p" "belief-m" 16000 t)))
  (nlk:observe-context-window-ceiling "belief-p2" "belief-m2" 8000)
  (is (= 8000 (nle::context-window-belief "belief-p2" "belief-m2" 16000 nil))))

(deftest context-estimate-ratio-is-the-newest-measured-prompt ()
  (let ((nle::*context-estimate-calibration* (make-hash-table :test #'equal)))
    (is (= 1.0d0 (nle::context-estimate-ratio "cal-p" "cal-m")))
    (nle::observe-context-estimate "cal-p" "cal-m" 1000 2000)
    (is (= 2.0d0 (nle::context-estimate-ratio "cal-p" "cal-m")))
    (nle::observe-context-estimate "cal-p" "cal-m" 1000 1000)
    (is (= 1.0d0 (nle::context-estimate-ratio "cal-p" "cal-m")))
    ;; the reasoning replayed by
    ;; a 38-character reference is invisible to chars/4.
    (nle::observe-context-estimate "cal-p" "cal-m" 22286 92863)
    (is (= 92863 (round (* 22286 (nle::context-estimate-ratio "cal-p" "cal-m")))))))

(deftest context-window-belief-unknown-needs-a-ceiling (with-temp-store ())
  (is (null (nle::context-window-belief "unk-p" "unk-m" nil nil)))
  (nlk:observe-context-window-floor "unk-p" "unk-m" 66377)
  (is (null (nle::context-window-belief "unk-p" "unk-m" nil nil)))
  (nlk:observe-context-window-ceiling "unk-p" "unk-m" 130000)
  (is (= 130000 (nle::context-window-belief "unk-p" "unk-m" nil nil)))
  (nlk:observe-context-window-floor "unk-p" "unk-m" 140000)
  (is (null (nle::context-window-belief "unk-p" "unk-m" nil nil))))

(deftest unknown-window-runs-without-a-trigger (with-temp-store ())
  (seed-completed-turn "no-budget" "one" (make-string 4000 :initial-element #\x))
  (seed-completed-turn "no-budget" "two" (make-string 4000 :initial-element #\y))
  (is-values (boundary live-drop)
      (nle::choose-history-boundary (visible-history-turns "no-budget") 0
                                    :budget nil :tools #() :system-prompt "S")
    ((zerop boundary) is) (live-drop null "and the live turn is not thinned")))

;;; --- eviction stubs project what the rounds left behind ---------------------

(deftest definition-display-scans-the-exact-source ()
  (is-each (nle::definition-display)
    ("DEFUN" "WRITE-TEXT-FILE"
     (format nil "(defun write-text-file (path text~%    &key append-p)~%  path)")
     "(write-text-file path text &key append-p)"
     "a defun carries its lambda list, downcased, one line")
    ("DEFPARAMETER" "*PY-SRC*" "(defparameter *py-src* (list 1 2))" "*py-src*"
     "a variable is its bare name — a value form is not a lambda list")
    ("DEFMETHOD" "FOLD-FRAME" "(defmethod fold-frame :around (acc frame) acc)"
     "(fold-frame acc frame)" "method qualifiers are skipped, the lambda list survives")
    ("DEFUN" "NULLARY" "(defun nullary () 9)" "(nullary)"
     "an empty lambda list still reads as a call shape")
    ("DEFUN" "BROKEN" "(defun broken (unclosed" "broken"
     "text the scan cannot follow degrades to the bare name")
    ("DEFUN" "AUXED" "(defun auxed (path &key force &aux (text (read-file path))) text)"
     "(auxed path &key force)"
     "&aux names the definition's own bindings, so the call shape ends there")
    ("DEFUN" "ALL-AUX" "(defun all-aux (&aux (a 1)) a)" "(all-aux)"
     "a lambda list that is nothing but &aux is an empty call shape"))
  (is (char= #\… (char (nle::definition-display
                        "DEFUN" "LONG"
                        (format nil "(defun long (~a) 1)"
                                (make-string 200 :initial-element #\a)))
                       119))))

(deftest standing-clause-dedupes-newest-first-and-caps ()
  (is (null (nle::standing-clause "defined" '())) "no definitions, no clause")
  (is-table (expected definitions) (equal expected (nle::standing-clause "defined" definitions))
    ("defined: b · a" (list (list 1 "A" "a") (list 2 "B" "b-old") (list 3 "B" "b")))
    ("defined: a" (list (list 1 "A" "a") (list 2 "B" "b") (list 3 "B" nil))))
  (is (null (nle::standing-clause "defined" (list (list 1 "A" "a") (list 2 "A" nil)))))
  (let ((clause (nle::standing-clause
                 "defined"
                 (loop for i from 1 to 30
                       collect (list i (format nil "N~d" i)
                                     (format nil "n~d" i))))))
    (is-carrying clause ("n30" "the newest definitions are the ones shown") "+6 more")))

(defun seed-definition-fact (session-id name operator text &key forgotten)
  "Record NAME's definition fact on SESSION-ID — or, FORGOTTEN, the fact that
forgot it."
  (nlk::record-organism-fact
   session-id (if forgotten nlk::+kind-definition-forgotten+ nlk::+kind-definition-recorded+)
   (if forgotten "organism.definition_forget" "organism.definition_record")
   (list :names (vector name) :operator operator :text text
         :layer "test" :commit (if forgotten "abd" "abc"))
   'nlk::record-definitions))

(deftests turn-eviction-stub (session-id seed check)
  ;; Each case seeds the live turn's rounds and facts, thins it, and checks
  ;; the stub; the durable facts alone must then reproduce the projection.
  (with-live-turn (turn session session-id "stub-command")
    seed
    (hydrate-thin session 200)
    (is-present (stub (eviction-stub session.messages)) "a stub stands in for the rounds"
      check
      (hydrate-thin session 200 :refold t)
      (is-present (again (eviction-stub session.messages)) "the refold rebuilds the stub"
        (is (equal (gethash "content" stub) (gethash "content" again))))))
  (drops-forgotten-definitions
   "def-forgot"
   (progn (seed-live-tool-round turn 0)
          (seed-definition-fact "def-forgot" "WRITE-TEXT-FILE" "DEFUN"
                                "(defun write-text-file (path text &key append-p) path)")
          (seed-definition-fact "def-forgot" "WRITE-TEXT-FILE" "FMAKUNBOUND" "(fmakunbound 'x)"
                                :forgotten t)
          (dolist (round '(1 2)) (seed-live-tool-round turn round)))
   (is (null (search "defined:" (gethash "content" stub)))
       (format nil "a forgotten name is not surviving state: ~a" (gethash "content" stub))))
  (projects-recorded-definitions
   "def-thin"
   (progn (seed-live-tool-round turn 0)
          (seed-definition-fact "def-thin" "WRITE-TEXT-FILE" "DEFUN"
                                "(defun write-text-file (path text &key append-p) path)")
          (dolist (round '(1 2)) (seed-live-tool-round turn round)))
   (is (search "defined: (write-text-file path text &key append-p)" (gethash "content" stub))))
  ;; What the evicted rounds concluded: PRO-LONG's log entry -- the agent's
  ;; plan, the action, the outcome -- projected into the stub from the snippet
  ;; records the result facts carry.
  (carries-the-evicted-snippets-records
   "snippet-thin"
   (loop for note in '(nil "L1: left moves the box 3 columns" "L1: the box stops at the wall" nil
                       "L2: the newest round")
         for n from 0 do (seed-live-tool-round turn n (and note (snippet-record-object note))))
   (is-carrying (content (gethash "content" stub))
     ("L1: left moves the box 3 columns" content)
     "L1: the box stops at the wall"
     (is (< (search "3 columns" content) (search "stops at the wall" content)) "oldest first")
     (:absent "the newest round")
     ("(recall-entry seq)" "the road sign names a read that exists"))))

;;; --- what the evicted rounds concluded ---------------------------------------
;;; PRO-LONG's log entry — the agent's plan, the action, the outcome —
;;; projected into the stub from the snippet records the result facts carry.

(defun snippet-record-object (note)
  "The \"snippet\" object SNIPPET-RECORD writes for NOTE."
  (nlk:json-object :opt "note" note))

(deftest snippet-digest-line-is-the-seq-and-the-note ()
  (is (equal "seq 8 · only a note" (nle::snippet-digest-line 8 (snippet-record-object "only a note"))))
  (is (equal "seq 7" (nle::snippet-digest-line 7 (snippet-record-object nil))) "no note, the seq alone")
  (let ((nle::*eviction-stub-line-width* 20))
    (let ((line (nle::snippet-digest-line 9 (snippet-record-object (make-string 80 :initial-element #\n)))))
      (is (= 20 (length line)) "a long note is cut to the line width"))))

(deftest notes-block-caps-with-a-disclosure ()
  (is (null (nle::notes-block '())) "no records, no block")
  (let ((nle::*eviction-stub-note-limit* 4))
    (let ((block (nle::notes-block (loop for i from 1 to 7
                                         collect (list (* 10 i) (format nil "seq ~d · note-~d" (* 10 i) i))))))
      (is (search "+3 earlier: (recall-log :before 40)" block) block)
      (is (search "seq 70 · note-7" block) "the newest lines stand")
      (is (null (search "note-3" block)) "the older ones are counted")
      (is (< (search "note-4" block) (search "note-7" block)) "oldest first"))))

(deftest history-eviction-stub-carries-evicted-turn-records (with-temp-store ())
  (seed-completed-turn "snippet-whole" "one" "alpha")
  (with-durable-turn (turn "snippet-whole" :command-id "snippet-whole-2" :input "two" :complete t)
    (seed-live-tool-round turn 0 (snippet-record-object "L3: the door opens on the key"))
    (record-reply turn "beta"))
  (seed-completed-turn "snippet-whole" "three" "gamma")
  (let* ((turns (visible-history-turns "snippet-whole"))
         (stub (nle::history-eviction-stub turns 2)))
    (is-present (stub stub) "boundary 2 yields a stub"
      (is (search "L3: the door opens on the key" (gethash "content" stub))))
    (is (null (search "L3:" (gethash "content" (nle::history-eviction-stub turns 1)))))))

(deftest history-eviction-stub-projects-evicted-turn-definitions (with-temp-store ())
  (seed-completed-turn "def-whole" "one" "alpha")
  (seed-definition-fact "def-whole" "*PY-SRC*" "DEFPARAMETER"
                        "(defparameter *py-src* \"x\")")
  (seed-completed-turn "def-whole" "two" "beta")
  (is-present (stub (nle::history-eviction-stub (visible-history-turns "def-whole") 1))
      "boundary 1 yields a stub"
      (is (search "defined: *py-src*" (gethash "content" stub)))))

(deftest history-eviction-stub-carries-what-the-watched-snippets-showed
    (with-session-store ("watch-stub"))
  ;; The keyframe at the cut is computed, not written: the newest value each
  ;; watched snippet showed, read off the result facts that showed it.
  (with-durable-turn (turn "watch-stub" :current t :complete t)
    (execute-wire-call "watch-1" "eval" "{\"form\": \"(defparameter *stub-n* 3)\"}")
    (execute-wire-call "watch-2" "eval" "{\"form\": \"(+ *stub-n* 0)\"}")
    (execute-wire-call "watch-3" "eval" "{\"form\": \"(setf *stub-n* 2)\"}"))
  (seed-completed-turn "watch-stub" "two" "beta")
  (is-present (stub (nle::history-eviction-stub (visible-history-turns "watch-stub") 1))
      "boundary 1 yields a stub"
      (is-carrying (content (gethash "content" stub))
        ("watching: (+ *stub-n* 0) = 2" "the newest value the watched snippet showed")
        (:absent "= 3" "an older value gives way"))))

(deftest history-eviction-stub-lists-the-blocks-its-floors-cut ()
  ;; Each eviction moved the floor from where the last one left it: the stub
  ;; lists one block per eviction, oldest first, with a runnable recall aimed
  ;; at the newest, and one eviction is one block the header already names.
  (let ((turns (loop for (id start end) in '(("t1" 1 3) ("t2" 4 6) ("t3" 7 9) ("t4" 10 12))
                     collect (make-forced-eviction-turn id start end 100))))
    (flet ((content (floors) (gethash "content" (nle::history-eviction-stub turns 3 floors))))
      (is (equal (format nil "[history evicted: seq 1–9 · 3 turns · recall: (recall-log :match \"text\" :before 10)]~%[evicted in 2 blocks, oldest first; a block's seqs aim a recall: (recall-log :match \"text\" :after 3 :before 10)~% seq 1–3 · 1 turns~% seq 4–9 · 2 turns]")
                 (content '(4 10))))
      ;; The floor at the boundary splits nothing: the block this round's
      ;; eviction adds reads the same before its floor is a fact and after, so
      ;; the next round's request is this one's prefix.
      (is (equal (content '(4 10)) (content '(4))))
      (is (equal "[history evicted: seq 1–9 · 3 turns · recall: (recall-log :match \"text\" :before 10)]"
                 (content '(10))))
      (is (equal (content '(10)) (content '()))))))

(deftest eviction-blocks-fold-the-oldest-past-the-limit ()
  ;; The ARC-AGI-3 runs evicted 290 times in a turn: past the limit the oldest
  ;; blocks fold into one line that spans them and counts their evictions.
  (let* ((turns (loop for index from 0 below 15
                      collect (make-forced-eviction-turn (format nil "t~d" index)
                                                         (+ 1 (* 3 index)) (+ 3 (* 3 index)) 100)))
         (floors (loop for index from 1 below 15 collect (+ 1 (* 3 index))))
         (blocks (nle::eviction-blocks turns floors))
         (folded (nle::fold-eviction-blocks blocks 12)))
    (is (= 15 (length blocks)))
    (is-shape folded (length = 12) (first '(1 12 4 4)) (rest (last blocks 11)))
    (let ((content (gethash "content" (nle::history-eviction-stub turns 15 floors))))
      (is (search "[evicted in 12 blocks, oldest first;" content) content)
      (is (search (format nil "~% seq 1–12 · 4 turns · 4 evictions~%") content) content)
      (is (search "(recall-log :match \"text\" :after 42 :before 46)" content) content))))

(deftest a-lane-stands-on-the-eviction-its-room-recorded (with-temp-store ())
  ;; The room behind a Discord lane is where the operator evicts, and it never
  ;; sends a request of its own: the eviction reaches its lanes or nothing. A
  ;; lane forked after /evict cuts the room's history where the room does,
  ;; and its stub is the room's.
  (dolist (input '("one" "two" "three" "four"))
    (seed-completed-turn "room-l" input "ok"))
  (is (plusp (getf (nle::evict-session-context "room-l" :percent 50) :evicted-turn-count)))
  (nlk:create-session :id "lane-l" :parent "room-l")
  (seed-completed-turn "lane-l" "the ask" "the answer")
  (let* ((room (visible-history-turns "room-l"))
         (lane (visible-history-turns "lane-l"))
         (room-boundary (nle::floor-boundary room (nlk:retained-history-floor "room-l")))
         (lane-boundary (nle::floor-boundary lane (nlk:retained-history-floor "lane-l"))))
    (is (plusp room-boundary))
    (is (= room-boundary lane-boundary) "the lane cuts where its room does")
    (is (= (1+ (length room)) (length lane)))
    ;; the lane's window opens on the room's stub
    (is (equal (gethash "content"
                        (nle::history-eviction-stub room room-boundary
                                                    (nlk:retained-history-floors "room-l")))
               (gethash "content"
                        (first (nle::messages-at-boundary lane lane-boundary
                                                          (nlk:retained-history-floors "lane-l"))))))))

(deftest a-forced-eviction-keeps-a-token-count-once-over-a-bound (with-temp-store ())
  ;; KEEP cuts to a token count rather than a share, OVER holds the cut
  ;; until the retained history passes it, and REASON names who asked: the
  ;; channel kit bounds a room nobody sends requests on with these three.
  (let ((line (make-string 400 :initial-element #\a)))
    (dotimes (index 8)
      (seed-completed-turn "bound" (format nil "ask ~d ~a" index line) line)))
  (let ((total (reduce #'+ (visible-history-turns "bound") :key #'nle::history-turn-tokens)))
    ;; at the bound nothing goes, nor from a history already inside KEEP
    (is (null (nle:evict-session-context "bound" :over total :keep 100)))
    (is (null (nle:evict-session-context "bound" :keep total)))
    (multiple-value-bind (plan floor)
        (nle:evict-session-context "bound" :over (1- total) :keep (floor total 3)
                                           :reason "room_budget")
      (is-present plan "past the bound, the cut lands"
        (is (<= (getf plan :retained-estimated-tokens) (floor total 3)))
        (is (= floor (nlk:retained-history-floor "bound")))
        (is (equal "room_budget"
                   (nlk:context-evicted-reason
                    (session-event "bound" :kind "session.context.evicted"))))))
    ;; the cut brought it under the bound, so it waits for the bound again
    (is (null (nle:evict-session-context "bound" :over (1- total) :keep 100)))))

;;; --- the prefix invariant: a sent message is never rewritten ----------------
;;; Reasoning rides with its message, and each round's fold begins with the
;;; previous round's, byte for byte. Without this, a round's reasoning shed
;;; one round later would re-render the whole tool-call chain upstream and pin
;;; cached_tokens at the ask on every request.

(deftest unsigned-reasoning-rides-with-its-message (with-session-store ("rsn"))
  (with-durable-turn (turn "rsn" :command-id "c-1" :input "first ask" :complete t)
    (record-reply turn "done" "reasoning_content" "terminal thinking"))
  (with-durable-turn (turn "rsn" :command-id "c-2" :input "second ask")
    (flet ((assistants (turn)
             (remove-if-not (lambda (m) (equal "assistant" (nlk:json-value m :string "role")))
                            (nle::history-turn-messages turn))))
      (record-reply turn "one" "reasoning_content" "old round thinking")
      (record-reply turn "two" "reasoning_content" "signed thinking"
                    "reasoning_blocks" (vector (nlk:make-json-object "thinking" "signed thinking" "signature" "sig")))
      (record-reply turn "three" "reasoning_content" "newest thinking")
      (let* ((turns (visible-history-turns "rsn"))
             (prior (assistants (first turns)))
             (live (assistants (car (last turns)))))
        (is (equal "terminal thinking"
                   (gethash "reasoning_content" (first prior))))
        (is (equal '("old round thinking" "signed thinking" "newest thinking")
                   (mapcar (lambda (m) (gethash "reasoning_content" m)) live)))))))

(defun fold-wire-texts (session-id)
  "SESSION-ID's fold at boundary 0, each message as its wire JSON text: what
the chat lane sends, message by message."
  (mapcar (lambda (message) (shasht:write-json message nil))
          (nle::messages-at-boundary (visible-history-turns session-id) 0)))

(deftest a-rounds-fold-begins-with-the-previous-rounds (with-session-store ("pfx"))
  (with-durable-turn (turn "pfx" :command-id "c-1" :input "ask")
    (record-tool-round turn "call-1" "{\"form\":\"(+ 1 1)\"}" "2" :reasoning "first thinking")
    (let ((after-one (fold-wire-texts "pfx")))
      (record-tool-round turn "call-2" "{\"form\":\"(+ 1 1)\"}" "2" :reasoning "second thinking")
      (let ((after-two (fold-wire-texts "pfx")))
        (is (= 3 (length after-one)) "ask, call, result")
        (is (= 5 (length after-two)) "then the second call and its result")
        (is (equal after-one (subseq after-two 0 (length after-one))))
        (record-reply turn "done" "reasoning_content" "final thinking")
        (nlk:complete-turn turn nil)
        (setf turn nil)
        (with-durable-turn (next "pfx" :command-id "c-2" :input "again")
          (let ((next-fold (fold-wire-texts "pfx")))
            (is (= 7 (length next-fold)) "the answer, then the next ask")
            (is (equal after-two (subseq next-fold 0 (length after-two))))))))))

;;; --- the help section and the live tail -------------------------------------
;;; The core projects one line per topic of the manual — static while the
;;; running cells are, so it rides the cached prefix — and what the
;;; peripherals say right now rides the tail of the messages, behind every
;;; cache breakpoint: whatever can change between rounds sits behind
;;; everything that cannot. A topic's text never rides a request.

(deftest harness-sections-carry-the-manual-a-line-a-topic-and-the-tail-the-board ()
  (is-present (text (cell-section nil "help")) "the help section rides every session, cells or none"
    (loop for (topic summary) in nle::*help-topics*
          do (is (search (format nil "  ~(~s~) - ~a" topic summary) text)
                 (format nil "the line for ~s" topic)))
    (is-carrying text ("(help :topic)" "it names the read")
      (:absent "Defining is keeping" "never a topic's text")))
  (is (null (nle::read-live-sections nil)))
  (with-saved-globals (nle::*help-topics*)
    (nle:set-help-topic :probe "the probe line" "THE PROBE MANUAL")
    (is-carrying (text (cell-section nil "help"))
      ("  :probe - the probe line" "a topic set is a line")
      (:absent "THE PROBE MANUAL" "and its text stays in the image"))
    (is (equal "THE PROBE MANUAL" (nle:help :probe)))
    (nle:set-help-topic :probe nil)
    (is (null (search ":probe" (cell-section nil "help"))) "a topic taken off is no line"))
  (with-saved-globals ((nlk::*cells* (list (nlk::make-cell :name "nodecode-channel-kit"
                                                             :kind :peripheral :state :loaded
                                                             :started-p t))))
    (nlk:note-standing "channels.discord"
                       "discord: unconfigured — channels.discord is absent" :warning)
    ;; what the folder loaded is (cells)'s, never a section
    (is (equal '("help") (mapcar #'car (nle::read-harness-sections nil))))
    (let ((live (nle::read-live-sections nil)))
      (is (equal '(("notices" . "discord: unconfigured — channels.discord is absent"))
                 live))
      (is (search "<harness key=\"notices\">"
                  (gethash "content" (nle::sections-message live)))))
    (nlk:note-standing "channels.discord" nil :info)
    (is (null (nle::read-live-sections nil)) "and rides nothing")))

(deftest the-help-section-is-the-cores-own (with-temp-store ())
  ;; A durable row of the name is replaced by the core's own: one authority
  ;; per name.
  (nlk:set-harness-section "help-section-session" "help" "A SESSION'S OWN TEXT")
  (is-carrying (text (cell-section "help-section-session" "help"))
    ("(help :topic)" "the core's text stands")
    (:absent "A SESSION'S OWN TEXT" "and the row it displaced is not read")))

;;; --- the head: the standing text the system message does not carry ---------
;;; The instructions file, a pinned index, a room's contract ride one user
;;; message at the head of the conversation, joined ahead of the first user
;;; message's own text on every lane — inside the cached prefix, before the
;;; Anthropic breakpoints — and the system message keeps the harness's own
;;; sections alone.

(deftest the-head-rides-every-lane-ahead-of-the-first-user-message ()
  (let* ((sections '(("folder" . "HERE") ("instructions" . "THE RULES") ("help" . "THE TOPICS")))
         (history (list (nle::message "user" "hi")
                        (nle::message "assistant" "hello")
                        (nle::message "user" "again")))
         (context (local-context history :harness-sections sections))
         (system-only (local-context history :harness-sections (remove "instructions" sections
                                                                       :key #'car :test #'string=))))
    (is-carrying (system context.system-prompt) ("HERE" "the folder is the harness's own")
      ("THE TOPICS" "and so is the manual") (:absent "THE RULES" "the instructions file is not"))
    (is-carrying (head (head-text context)) "<harness key=\"instructions\">" (:absent "HERE"))
    (is (null (nle::compiled-turn-context-head-message system-only)) "no head without its sections")
    ;; the compiled history is untouched
    (is (equalp context.messages (nle::compiled-turn-context-messages system-only)))
    ;; the estimate counts the head
    (is (> (nle::compiled-context-estimated-tokens context)
           (nle::compiled-context-estimated-tokens system-only)))
    ;; chat completions: no message of its own, the first user message leads with it
    (let ((wire (gethash "messages" (nle::request-body context))))
      (is (= 4 (length wire)) "chat: the system message and the three")
      (is-present (text (gethash "content" (aref wire 1))) "the first user message"
        (is (eql 0 (search "<harness key=\"instructions\">" text)) "the head leads it")
        (is (search "hi" text) "and its own text follows"))
      (is (equal "again" (gethash "content" (aref wire 3))) "the last user message is its own"))
    ;; Responses
    (let ((input (gethash "input" (nle::responses-request-body context))))
      (is (= 3 (length input)) "responses: the three, the system prompt riding as instructions")
      (is (eql 0 (search "<harness key=\"instructions\">"
                         (gethash "text" (aref (gethash "content" (aref input 0)) 0))))))
    ;; GenAI: the first user content, turns still alternating
    (let ((contents (gethash "contents" (nle::google-request-body context))))
      (is (= 3 (length contents)) "google: three contents")
      (is (search "THE RULES" (gethash "text" (aref (gethash "parts" (aref contents 0)) 0)))))
    ;; Anthropic: in the first message, before the breakpoints mark the prefix
    (let* ((messages (gethash "messages" (nle::anthropic-request-body context)))
           (first-block (aref (gethash "content" (aref messages 0)) 0)))
      (is (= 3 (length messages)) "anthropic: three messages")
      (is (eql 0 (search "<harness key=\"instructions\">" (gethash "text" first-block))))))
  ;; A history with no user message: the head stands first, as a message of its own.
  (let* ((head (nle::message "user" "HEAD"))
         (joined (nle::join-user-message (vector (nle::message "assistant" "a")) head)))
    (is (= 2 (length joined)))
    (is (eq head (aref joined 0)))))

(deftest compiled-turns-share-one-prompt-text ()
  ;; Built afresh, every turn in flight held its own copy of the same tens of kilobytes.
  (let ((sections (list (cons "folder" (make-string 500 :initial-element #\f))
                        (cons "where" (make-string 500 :initial-element #\w)))))
    (multiple-value-bind (system head) (nle::prompt-messages "BASE" (copy-tree sections))
      (multiple-value-bind (system-again head-again) (nle::prompt-messages "BASE" (copy-tree sections))
        (is (eq system system-again) "the system prompt")
        (is (eq (gethash "content" head) (gethash "content" head-again)) "the head message's text")))))

(deftest an-empty-section-is-no-block ()
  ;; a cell pins "" for nothing to show: the pin holds, the prompt carries nothing
  (is-carrying (head (gethash "content" (nle::sections-message '(("a" . "") ("b" . "B TEXT")))))
    "<harness key=\"b\">" (:absent "<harness key=\"a\">"))
  (is (null (nle::sections-message '(("a" . "")))) "all empty is no message")
  (is (equal "base" (nle::harness-overlay-prompt "base" '(("a" . ""))))))

;;; --- the prompt a conversation keeps -----------------------------------------
;;; The first request pins the system prompt, the sections and the tools; a
;;; later turn sends those bytes again, and what changed in between stands in
;;; the history as a <system-reminder> at the turn that saw it, so every
;;; request is a prefix of the next.

(defun payload-tool (name)
  "A tool payload object offering NAME."
  (nlk:json-object "type" "function" "function" (nlk:json-object "name" name)))

(defmacro prompt-fact (&rest specs)
  "A conversation-prompt fact whose payload is the JSON-OBJECT of SPECS."
  `(make-instance 'nlk:turn-context-prompt-event :payload (nlk:json-object ,@specs)))

(deftest a-prompt-change-is-told-as-a-reminder-of-what-moved ()
  (let* ((index (format nil "~{- entry ~d~^~%~}" (loop for i below 20 collect i)))
         (sections (list (cons "memory-index" index) (cons "notices" "a notice")))
         (pin (prompt-fact "pin" t "base" "BASE" "floor" 0
                           "sections" (map 'vector (lambda (s) (vector (car s) (cdr s))) sections)
                           "tools" (vector (payload-tool "eval") (payload-tool "report_issue"))))
         (prompt (nle::fold-prompt-facts (list pin))))
    (is (equal sections (nle::conversation-prompt-told-sections prompt)) "the pin is what was told")
    ;; nothing moved: an empty section is none, and a tool the pin lacks is not offered
    (is (null (nle::prompt-change prompt "BASE" (append sections (list (cons "empty" "")))
                                  (vector (payload-tool "eval") (payload-tool "report_issue")
                                          (payload-tool "new_tool")))))
    (let ((change (nle::prompt-change prompt "BASE"
                                      (list (cons "memory-index" (format nil "~a~%- entry 20" index))
                                            (cons "where" "Where you are: #general"))
                                      (vector (payload-tool "eval")))))
      (is (null (getf change :base)) "the system prompt held")
      (is (equalp #("report_issue") (getf change :withdrawn)) "the pinned tool no longer bound")
      ;; a section rewritten and one new whole, one withdrawn by its name alone
      (is (equalp (vector (vector "memory-index" (format nil "~a~%- entry 20" index))
                          (vector "where" "Where you are: #general")
                          (vector "notices"))
                  (getf change :sections)))
      (is-carrying (text (getf change :text))
        ("<system-reminder>" "a reminder")
        ("<harness key=\"memory-index\" changed=\"lines\">" "a long section tells its lines")
        ("+ - entry 20" "the line added")
        (:absent "- entry 3" "and no line that stood")
        ;; a new section goes whole
        (is (search (nlk:harness-section-block "where" "Where you are: #general") text))
        ("<harness key=\"notices\">(withdrawn)</harness>" "a gone one is withdrawn")
        ("No longer available: report_issue" "an unbound tool is named"))
      ;; the change folded in: what was told moves, the pin does not
      (let ((told (nle::fold-prompt-facts
                   (list pin (prompt-fact "base" "BASE TWO" "sections" (getf change :sections)
                                          "withdrawn" (getf change :withdrawn)
                                          "text" (getf change :text))))))
        (is (equal "BASE" (nle::conversation-prompt-base told)) "the pin sends its base")
        (is (equal "BASE TWO" (nle::conversation-prompt-told-base told)))
        (is (equal '("memory-index" "where") (mapcar #'car (nle::conversation-prompt-told-sections told))))
        ;; told once, a change is not told again
        (is (null (nle::prompt-change told "BASE TWO"
                                      (nle::conversation-prompt-told-sections told)
                                      (vector (payload-tool "eval")))))
        (is-carrying (text (getf (nle::prompt-change told "BASE TWO"
                                                     (nle::conversation-prompt-told-sections told)
                                                     (vector (payload-tool "eval") (payload-tool "report_issue")))
                                 :text))
          ("Available again: report_issue" "a tool bound again is named")
          (:absent "<harness" "and nothing else is said"))))))

(deftest a-reminder-stands-where-it-was-noticed-never-inside-a-call (with-session-store ("held"))
  ;; A resumed turn notices a change behind the round it had reached; a call
  ;; still open there answers first, so the wire never splits a call from its
  ;; result.
  (with-durable-turn (turn "held" :input "go")
    (nlk:record-assistant-message
     turn (wire-message "assistant" :content ""
                        :tool-calls (vector (wire-tool-call "c1" "eval" "{}"))))
    (nlk:record-tool-call turn "c1" "eval" "{}")
    (nlk:record-turn-context-prompt turn :text "<system-reminder>R</system-reminder>")
    (nlk:record-tool-result turn "c1" "eval" "3")
    (let ((wire (wire-fold "held")))
      (is (equal '("user" "assistant" "tool" "system")
                 (mapcar (lambda (message) (gethash "role" message)) wire)))
      (is (equal "<system-reminder>R</system-reminder>"
                 (gethash "content" (car (last wire))))))))

(defun turn-request (session-id input command-id &aux (asked nil))
  "Run INPUT as a turn of SESSION-ID under COMMAND-ID, the streaming provider
answering `ok' => the compiled context of the turn's one request."
  (with-streaming-provider-stub (context nil
                                 (setf asked context)
                                 (values (wire-message "assistant" :content "ok") nil "stop"))
    (nle:run input :session (nle:intern-session session-id) :command-id command-id))
  asked)

(deftest a-conversation-keeps-its-first-prompt-and-is-told-what-changed (with-temp-store ())
  (ensure-durable-session "kept")
  (with-saved-globals (nle::*tools*)
    (nle:register-tool "probe" (list :description "d" :parameters (make-hash-table :test #'equal))
                       (lambda (arguments) (declare (ignore arguments)) "ok"))
    (unwind-protect
         (let ((nle::*api-key* "k")
               (base nle:*system-prompt*))
           (labels ((turn (input) (turn-request "kept" input (format nil "kept-~a" input)))
                    (wire (context)
                      (map 'list #'nlk:encode-json-object (nle::request-messages context)))
                    (content (message) (nle::content-text (gethash "content" message)))
                    (last-text (context)
                      (content (alexandria:last-elt (nle::request-messages context))))
                    (reminders (context)
                      (count-if (lambda (message) (search "<system-reminder>" (content message)))
                                (nle::request-messages context))))
             (nlk:note-standing "prompt.test" "FIRST NOTICE" :warning)
             (let ((one (turn "one")))
               (is (null (search "FIRST NOTICE" (first (wire one)))) "a notice never rides the head")
               (is-carrying (text (last-text one))
                 ("<system-reminder>" "it is told at the pin's own turn, after the ask")
                 ("FIRST NOTICE" "the notice standing then"))
               (is (= 1 (reminders one)))
               (nlk:note-standing "prompt.test" "SECOND NOTICE" :warning)
               (nle::unregister-tool "probe")
               (let* ((nle:*system-prompt* (format nil "~a~%ONE MORE LINE" base))
                      (two (turn "two")))
                 ;; the system message is the first turn's
                 (is (equal (nle::compiled-turn-context-system-prompt one)
                            (nle::compiled-turn-context-system-prompt two)))
                 ;; and so are the tools, the one unbound since included
                 (is (equal (nlk:encode-json-object (nle::compiled-turn-context-tools one))
                            (nlk:encode-json-object (nle::compiled-turn-context-tools two))))
                 ;; the first turn's request is a prefix of the second's
                 (is (equal (wire one) (subseq (wire two) 0 (length (wire one)))))
                 (is-carrying (text (last-text two))
                   ("<system-reminder>" "the turn ends its history with the reminder")
                   ("SECOND NOTICE" "the notice that changed")
                   ("+ ONE MORE LINE" "the system prompt's new line")
                   ("No longer available: probe" "the tool unbound"))
                 (let ((three (turn "three")))
                   ;; the second turn's request, reminder and all, is a prefix of the third's
                   (is (equal (wire two) (subseq (wire three) 0 (length (wire two)))))
                   (is (= 2 (reminders three)) "told once: nothing new is said")
                   ;; an eviction moves the window's head: the prompt is taken afresh
                   (nlk:advance-history-floor "kept" (1+ (nlk:session-log-cursor "kept")))
                   (let ((four (turn "four")))
                     ;; re-pinned at the eviction: the system prompt now
                     (is (search "ONE MORE LINE" (nle::compiled-turn-context-system-prompt four)))
                     ;; and the tools now
                     (is (notany (lambda (tool) (equal "probe" (nle::tool-payload-name tool)))
                                 (nle::compiled-turn-context-tools four)))
                     (is (= 1 (reminders four)))
                     (is-carrying (text (last-text four))
                       ("SECOND NOTICE" "the board, told again under the new pin")
                       (:absent "ONE MORE LINE" "and nothing the pin holds")
                       (:absent "No longer available" "nor a tool the pin never offered"))))))
             ;; a fork keeps the prompt its parent paid for
             (let ((anchor (nlk:event-turn-id (first (session-events "kept" :kind "turn.started")))))
               (nlk:create-session :id "kept-fork" :parent "kept" :anchor-turn-id anchor)
               (multiple-value-bind (fork-base sections tools)
                   (nle::turn-prompt "kept-fork" (nle::snapshot-tool-bindings))
                 (declare (ignore sections))
                 (is (equal base fork-base) "the fork's system prompt is its parent's pin")
                 ;; and so are its tools, the one unbound since included
                 (is (find "probe" tools :key #'nle::tool-payload-name :test #'equal))))))
      (nlk:note-standing "prompt.test" nil :info))))

(deftest a-rooms-lanes-pin-one-prefix-and-part-at-their-own-asks (with-session-store ("room"))
  ;; Every lane of a Discord room pins at its first request, and a
  ;; lane's `where' in that pin would make each lane's head its own, so each
  ;; new lane would re-read the room's whole history.
  (seed-completed-turn "room" "a line in the room" "an answer in the room")
  (let ((anchor (nlk:event-turn-id (first (session-events "room" :kind "turn.started"))))
        (nle::*api-key* "k")
        (rounds '()))
    (dolist (lane '("lane-a" "lane-b"))
      (nlk:create-session :id lane :parent "room" :anchor-turn-id anchor))
    (nle:hook 'nle::read-live-sections "where-test"
              (lambda (next session-id)
                (append (funcall next session-id)
                        (list (cons "where" (format nil "this lane is ~a" session-id))))))
    (flet ((ask (lane)
             (push (turn-request lane (format nil "ask in ~a" lane) (format nil "~a-ask" lane))
                   rounds)
             (map 'list #'nlk:encode-json-object (nle::request-messages (first rounds)))))
      (let ((a (ask "lane-a"))
            (b (ask "lane-b")))
        (is (search "a line in the room" (first a)) "the room's history rides ahead of the ask")
        (is (equal (nle::compiled-turn-context-system-prompt (first rounds))
                   (nle::compiled-turn-context-system-prompt (second rounds))))
        (is (equal (butlast a 2) (butlast b 2)) "the head and the room's history are one prefix")
        (is (search "ask in lane-b" (car (last b 2))) "the lanes part at their own asks")
        (is (null (search "this lane is" (first b))) "a lane's `where' never rides the head")
        (is (search "this lane is lane-b" (car (last b))) "it is told after the ask")))))


(defun rules-notice (needle)
  "The standing notice about a rules file whose path carries NEEDLE, or NIL."
  (second (find-if (lambda (entry &aux (text (second entry)))
                     (and (search "is not read" text) (search needle text)))
                   (nlk:standing-notices))))

(defun rules-said (needle)
  "Every notice said about a rules file whose path carries NEEDLE, newest first."
  (loop for (text) in (nlk:notice-log :limit nlk:+notice-log-size+)
        when (and (search "is not read" text) (search needle text)) collect text))

(defun rules-size-words (length)
  "The words the refusal of a rules file of LENGTH characters carries."
  (list (format nil "is not read: ~a" (nlk:size-text length))
        (format nil "trim to ~a" (nlk:size-text nle::+instructions-byte-limit+))))

(deftest harness-sections-carry-the-working-directory-instructions ()
  ;; A project's AGENTS.md — or CLAUDE.md, or .cursorrules — rides as the
  ;; `instructions' section of every session whose directory sits under it,
  ;; found by walking up to the repository root and never past it; the
  ;; file is the authority, nothing is stored, and a file over the harness
  ;; budget is refused and said once.
  (with-temp-workspace (dir "AGENTS.md" "proj/.git/HEAD" "proj/deep/x" "other/y")
    (write-temp-file (merge-pathnames "AGENTS.md" dir) "root rules")
    (with-temp-store ()
      (loop for (id folder) in '(("root" "") ("deep" "proj/deep/") ("other" "other/"))
            do (nlk:create-session :id id :cwd (namestring (merge-pathnames folder dir))))
      (flet ((instructions (session-id) (cell-section session-id "instructions")))
        (is-present (text (instructions "root")) "the session at the root reads its AGENTS.md"
          (is-carrying text "root rules" ("AGENTS.md" "and says which file")))
        (is-present (text (instructions "other")) "a directory below it walks up to the file"
          (is (search "root rules" text)))
        (is (null (instructions "deep")))
        (write-temp-file (merge-pathnames "proj/CLAUDE.md" dir) "claude rules")
        (is-present (text (instructions "deep")) "CLAUDE.md is read where there is no AGENTS.md"
          (is (search "claude rules" text)))
        (write-temp-file (merge-pathnames "proj/AGENTS.md" dir)
                         (make-string 70000 :initial-element #\x))
        (is (null (instructions "deep")) "a file over the budget is no section")
        (is (null (instructions "deep")))
        ;; Said once per path, never on the board: a project's file is one
        ;; folder's, and a standing notice would ride every other session's
        ;; prompt until a restart.
        (let ((said (rules-said "proj/AGENTS.md")))
          (is (= 1 (length said)) "said once, not at every build")
          (is (every (lambda (words) (search words (first said))) (rules-size-words 70000))))
        (is (null (rules-notice "proj/AGENTS.md")) "and stands on no board")
        (write-temp-file (merge-pathnames "proj/AGENTS.md" dir) "deep rules")
        (is (search "deep rules" (instructions "deep")) "trimmed, it is read at the next build")
        (is (null (instructions nil)) "no session, no section")
        (is (search "<harness key=\"instructions\">"
                    (nle::harness-overlay-prompt "base" (nle::read-harness-sections "root"))))))))

(deftest harness-sections-read-every-rules-file-from-the-repository-root-down ()
  ;; A monorepo's root AGENTS.md and a package's own both apply to a
  ;; session in the package: every directory from the session's up to the
  ;; repository root gives its file, root first, the nearer last so it
  ;; wins, and nothing above the root is read.
  (with-temp-workspace (dir "AGENTS.md" "mono/.git/HEAD" "mono/packages/api/src/x")
    (write-temp-file (merge-pathnames "AGENTS.md" dir) "outside rules")
    (write-temp-file (merge-pathnames "mono/AGENTS.md" dir) "root rules")
    (write-temp-file (merge-pathnames "mono/packages/CLAUDE.md" dir) "packages rules")
    (write-temp-file (merge-pathnames "mono/packages/api/AGENTS.md" dir) "api rules")
    (with-temp-store ()
      (loop for (id folder) in '(("api" "mono/packages/api/src/") ("root" "mono/"))
            do (nlk:create-session :id id :cwd (namestring (merge-pathnames folder dir))))
      (flet ((instructions (session-id) (cell-section session-id "instructions"))
             (at (needle text) (or (search needle text) -1)))
        (is-present (text (instructions "api")) "a session in a package reads every file up to the root"
          (is (< -1 (at "root rules" text) (at "packages rules" text) (at "api rules" text)) "root first, the nearest last")
          (is (null (search "outside rules" text)) "and nothing above the repository root")
          (is (search nle::*instructions-chain-note* text) "it says the nearer one wins")
          (is-carrying text "From " ("mono/AGENTS.md:" "each file named") ("packages/CLAUDE.md:") ("api/AGENTS.md:")))
        (is-present (text (instructions "root")) "one file reads as it always did"
          (is (eql 0 (search "From " text)) "with no note ahead of it"))
        (write-temp-file (merge-pathnames "mono/packages/CLAUDE.md" dir) (format nil "  ~%"))
        (is-present (text (instructions "api")) "a blank file is passed over and the walk goes on"
          (is (search "root rules" text))
          (is (null (search "packages/CLAUDE.md" text))))
        (write-temp-file (merge-pathnames "mono/packages/api/AGENTS.md" dir) "root rules")
        (is-present (text (instructions "api")) "a copy of a farther file's text"
          (is (= 1 (loop for at = (search "root rules" text) then (search "root rules" text :start2 (1+ at)) while at count t)) "is read once")
          (is (search "api/AGENTS.md:" text) "as the nearer file"))
        (write-temp-file (nlk:home "AGENTS.md") "house rules")
        (write-temp-file (merge-pathnames "mono/packages/CLAUDE.md" dir) "house rules")
        (is (null (search "house rules" (instructions "api"))) "what the home's AGENTS.md holds rides once, as operator-rules")
        (is (search "house rules" (cell-section "api" "operator-rules")))
        ;; The budget holds the whole: from the nearest out, a file that would
        ;; pass it is refused, said once, and the walk goes on.
        (write-temp-file (merge-pathnames "mono/packages/api/AGENTS.md" dir) (make-string 40000 :initial-element #\a))
        (write-temp-file (merge-pathnames "mono/packages/CLAUDE.md" dir) (make-string 30000 :initial-element #\p))
        (is-present (text (instructions "api")) "the nearest file is read"
          (is (search "aaaa" text))
          (is (null (search "pppp" text)) "the one that would pass the budget is not")
          (is (search "root rules" text) "and a farther one that fits still is"))
        (instructions "api")
        (let ((said (rules-said "packages/CLAUDE.md")))
          (is (= 1 (length said)) "said once")
          (is (search "with the nearer rules files it makes" (first said)) "and why"))
        (is (null (rules-notice "packages/CLAUDE.md")) "on no board")))))

(deftest harness-sections-carry-the-home-operator-rules ()
  ;; am-101: the home's own AGENTS.md -- where an import lands another
  ;; harness's global CLAUDE.md, AGENTS.md or GEMINI.md -- rides as the
  ;; `operator-rules' section of every session, one in a folder with no
  ;; instructions of its own too, beside and ahead of a project's
  ;; `instructions'. Blank or missing is no section.
  (with-temp-workspace (dir "proj/.git/HEAD" "elsewhere/.git/HEAD")
    (with-temp-store ()
      (nlk:create-session :id "elsewhere" :cwd (namestring (merge-pathnames "elsewhere/" dir)))
      (nlk:create-session :id "proj" :cwd (namestring (merge-pathnames "proj/" dir)))
      (flet ((rules (session-id) (cell-section session-id "operator-rules")))
        (is (null (rules "elsewhere")) "no file, no section")
        (write-temp-file (nlk:home "AGENTS.md") (format nil "  ~%~%"))
        (is (null (rules "elsewhere")) "a blank file is no section")
        (write-temp-file (nlk:home "AGENTS.md") (format nil "# House rules~%~%Use just.~%"))
        (is-present (text (rules "elsewhere")) "a session in an unrelated folder reads the home's rules"
          (is-carrying text "Use just." ("AGENTS.md:" "and says which file")))
        (write-temp-file (merge-pathnames "proj/AGENTS.md" dir) "project rules")
        (is (search "Use just." (rules "proj")) "both ride")
        (is (search "project rules" (cell-section "proj" "instructions")))
        (let ((names (mapcar #'car (nle::read-harness-sections "proj"))))
          (is (member "instructions" (member "operator-rules" names :test #'string=) :test #'string=) "ahead"))
        (is (search "<harness key=\"operator-rules\">"
                    (nle::harness-overlay-prompt "base" (nle::read-harness-sections "elsewhere"))))
        ;; xh-201: past the budget the file is refused whole, and that was a
        ;; five-second toast naming an absolute path and a byte count, once
        ;; per image. It stands on the board until the file fits.
        (write-temp-file (nlk:home "AGENTS.md") (make-string 70000 :initial-element #\r))
        (is (null (rules "elsewhere")) "past the budget, no section")
        (is-present (said (rules-notice "AGENTS.md")) "a notice stands for it"
          (is (every (lambda (words) (search words said))
                     (cons "AGENTS.md is not read" (rules-size-words 70000)))))
        ;; The board labels a row with its key: `rules', not the file's
        ;; absolute path, which pushed the words off the /cells row.
        (is (equal (rules-notice "AGENTS.md") (second (assoc "rules" (nlk:standing-notices) :test #'string=))))
        ;; A session opened in the home itself reads the file as its project's:
        ;; the notice still stands, said once over two builds, not at each.
        (nlk:create-session :id "at-home" :cwd (namestring (nlk:home)))
        (is (null (cell-section "at-home" "instructions")))
        (is (null (rules "at-home")))
        (is (= 1 (length (rules-said "AGENTS.md"))) "said once")
        (is (rules-notice "AGENTS.md") "and it still stands")
        (write-temp-file (nlk:home "AGENTS.md") (format nil "# House rules~%~%Use just.~%"))
        (is (search "Use just." (rules "elsewhere")) "trimmed, it is read again")
        (is (null (rules-notice "AGENTS.md")) "and the notice comes down")))))

(deftest harness-sections-name-the-session-folder (with-temp-workspace (dir "a b/x"))
  ;; The session's own directory rides as
  ;; the `folder' section.
  (with-temp-store ()
    (let ((here (namestring (merge-pathnames "a b/" dir))))
      (nlk:create-session :id "here" :cwd here)
      (flet ((folder (session-id) (cell-section session-id "folder")))
        (is-present (text (folder "here")) "a session reads its folder"
          (is (search here text) "named whole, a space and all")
          ;; and the home, apart from it
          (is (search (string-right-trim "/" (namestring (nlk:home))) text)))
        (is (null (folder nil)) "no session, no section")))))

;;; --- an image the provider would not take ---------------------------------
;;; The images are withheld — a note where each stood — and the compiled
;;; context keeps the groups it placed so a retarget places them again.

(deftest withhold-image-groups-replaces-each-group-with-one-note ()
  (macrolet ((withholding ((input note) &body checks)
               `(multiple-value-bind (groups count)
                    (nle::withhold-image-groups ,input (lambda (n) (format nil ,note n)))
                  ,@checks)))
    (withholding ('((nil ("image/png" . "QUJD") ("image/jpeg" . "REVG"))
                    ("call-look" ("image/png" . "QUJD")))
                  "~d withheld")
      (is (= 3 count) "every image counts")
      (is (equal '((nil (:withheld . "2 withheld"))
                   ("call-look" (:withheld . "1 withheld")))
                 groups)))
    (withholding ('((nil (:withheld . "old"))) "~d")
      (is (zerop count) "a group already withheld has nothing left to withhold")
      (is (equal '((nil (:withheld . "old"))) groups) "and stands as it was"))
    (withholding ('((nil (:withheld . "old") ("image/png" . "QUJD"))) "~d new")
      (is (= 1 count) "an image staged after a withholding is the one withheld")
      (is (equal '((nil (:withheld . "old") (:withheld . "1 new"))) groups)))
    (withholding ('() "~d")
      (is (zerop count) "no groups, nothing withheld")
      (is (null groups) "and nothing invented"))))

(deftest context-compile-places-a-withheld-note-where-the-image-stood ()
  (is-let* ((messages (tool-round-messages))
            (groups '((nil (:withheld . "[prompt image withheld]"))
                      ("call-look" (:withheld . "[look image withheld]"))))
            (context (local-context messages :visible-images groups))
            (compiled context.messages)
            (prompt-parts (nle::message-content-parts
                           (gethash "content" (aref compiled 0))))
            (tail (aref compiled 4))
            (tail-parts (nle::message-content-parts (gethash "content" tail))))
    ((length compiled) = 5) ((mapcar #'nle::content-part-type prompt-parts) '("text" "text"))
    ((gethash "text" (second prompt-parts)) "[prompt image withheld]")
    ((gethash "role" tail) "user") ((mapcar #'nle::content-part-type tail-parts) '("text"))
    ((gethash "text" (first tail-parts)) "[look image withheld]") (context.visible-images groups)))

(deftest context-with-places-the-images-again-after-a-retarget ()
  ;; An eviction re-aims the window from the history turns; the images the
  ;; round was compiled with must land in the re-derived messages too, or a
  ;; recovery mid-round silently drops what the model was about to see.
  (let* ((context (local-context (tool-round-messages)
                                 :visible-images '(("call-look" ("image/png" . "QUJD")))))
         (again (nle::context-with
                 context
                 :messages (nle::messages-with-visible-images
                            (nle::messages-at-boundary
                             context.history-turns
                             context.history-boundary)
                            context.visible-images)))
         (compiled again.messages))
    (is (= 5 (length compiled)) "the image message is placed again")
    (is (equal "data:image/png;base64,QUJD"
               (gethash "url" (gethash "image_url"
                                       (first (nle::message-content-parts
                                               (gethash "content" (aref compiled 4))))))))
    (is (equal context.visible-images again.visible-images))))

;;; --- the standing block is the turn's -----------------------------------------
;;; The provider's prefix cache is a byte match: a section rewritten between
;;; rounds (a memory index after a keep, an MCP primer after a connect, the
;;; gateway's image line after an attach) re-prefills everything behind it.
;;; After round zero the sections come from the prior context; a change made
;;; mid-turn is visible at the next turn's first round.

(deftest standing-sections-freeze-for-the-turn (with-temp-store ())
  (let ((nle::*api-key* "k")
        (version "v1")
        (messages (list (wire-message "user" :content "hi"))))
    (flet ((compile-round (&rest keys)
             (nle::compile-turn-context
              (apply #'nle::snapshot-context-request "ctx-freeze" :messages messages keys))))
      (with-stubbed-fdefinition (nle::read-harness-sections (session-id)
                                 (list (cons "memory-index" version)))
        (let* ((round-0 (compile-round))
               (prompt-0 (head-text round-0)))
          (is (search "v1" prompt-0) "round zero reads the standing sections")
          (setf version "v2")
          (let ((round-1 (compile-round :prior-context round-0)))
            (is (equal prompt-0 (head-text round-1)))
            (is (equal round-0.harness-sections round-1.harness-sections))
            (is (equal prompt-0 (head-text (nle::context-with round-1 :messages messages)))))
          (is (search "v2" (head-text (compile-round)))))))))

;;; --- the turn's memo: a round's body is the last round's bytes plus what is
;;; new, and the estimate walks a frozen part once a turn ------------------------

(deftest a-round-s-body-assembles-to-the-bytes-of-a-cold-encode (with-temp-store ())
  (nle::clear-hydration-cache)
  (seed-completed-turn "ctx-memo" "one" "alpha"
                       :tools '(("c1" "eval" "{\"form\": \"1\"}" "1")))
  (seed-completed-turn "ctx-memo" "two" "beta")
  (let* ((nle::*api-key* "k")
         (session (intern-durable-session "ctx-memo"))
         (turn (nlk:admit-turn "ctx-memo" "ctx-memo-open" "three"))
         (nle::*current-durable-turn* turn)
         (lanes (list (cons "messages" #'nle::request-body)
                      (cons "input" #'nle::responses-request-body))))
    (flet ((compile-round (prior)
             (nle:hydrate-session
              session
              :tools (and prior prior.tools)
              :system-prompt (and prior prior.system-prompt))
             (session-round-context session :prior-context prior))
           (land-round (n)
             ;; the round's call and its result, as the loop records them
             (record-tool-round turn (format nil "open-~d" n) "{\"form\": \"1\"}"
                                (format nil "result ~d" n))))
      (let ((context nil))
        (dotimes (round 3)
          (when (plusp round) (land-round round))
          (setf context (compile-round context))
          (let ((cold (compile-round nil)))
            (loop for (key . lane) in lanes
                  do (let* ((body (funcall lane context))
                            (verbatims (count-if #'nlk:json-verbatim-p (gethash key body)))
                            (octets (nlk:encode-json-body body key context.memo)))
                       (is (equal (nlk:sha256-text (nlk:encode-json-octets (funcall lane cold)))
                                  (nlk:sha256-text octets))
                           (format nil "round ~d ~a: the assembled body is the cold body"
                                   round key))
                       (if (zerop round)
                           (is (zerop verbatims)
                               (format nil "round 0 ~a: nothing is a verbatim yet" key))
                           (is (plusp verbatims)
                               (format nil "round ~d ~a: the closed history came back as verbatims"
                                       round key)))))))))))

(deftest the-estimate-walks-a-frozen-part-once-a-turn ()
  (let* ((messages (list (nle::message "user" "hello") (nle::message "assistant" "wörld")))
         (context (local-context messages))
         (whole (+ (nle::estimate-json-tokens context.system-prompt)
                   (nle::estimate-json-tokens context.messages)
                   (nle::estimate-json-tokens (or context.tools #())))))
    (is (= whole (nle::compiled-context-estimated-tokens context)))
    (let* ((more (append messages (list (nle::message "user" "more"))))
           (later (nle::context-with context :messages more))
           ;; the reference, walked whole and outside the count below
           (expected (+ (nle::estimate-json-tokens later.system-prompt)
                        (nle::estimate-json-tokens (coerce more 'vector))
                        (nle::estimate-json-tokens (or later.tools #()))))
           (walked 0))
      (with-stubbed-fdefinition (nlk:encode-json-object (object)
                                 (incf walked)
                                 (funcall original object))
        (is (= expected (nle::compiled-context-estimated-tokens later)))
        (is (= 1 walked))))))
