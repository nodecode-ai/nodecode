(in-package #:nodecode.test)

;;; --- the reasoning-loop guard (engine/loop-guard.lisp) ----------------------
;;; A model that loops streams until its output limit, and the idle watchdog
;;; never fires on a lively stream. The guard cuts the attempt and the retry
;;; ladder re-runs it, so the loop never reaches history. The detector gates
;;; drive LOOP-DETECTOR-PUSH directly; the round gates drive CALL-PROVIDER over
;;; turn-test's canned SSE helpers.

(defun loop-detector-verdict (semantic-p &rest texts)
  "What a detector at SEMANTIC-P says once TEXTS stream through it, each a
delta, and the stream ends: the loop in words, or NIL."
  (let ((detector (nle::make-loop-detector semantic-p)))
    (or (some (lambda (text) (nle::loop-detector-push detector text)) texts)
        (nle::loop-detector-flush detector))))

(defun repeated (text times)
  "TIMES copies of TEXT as one list of deltas."
  (make-list times :initial-element text))

(deftest an-exact-cycle-of-whole-lines-is-a-loop-past-a-kilobyte ()
  ;; 30 copies of one 41-character line cover more than 1 KB. 20 stay under
  ;; the long floor, as a summarizer restating its status line does.
  (let ((line (format nil "Still checking the parser configuration.~%")))
    (is (search "41-character passage" (apply #'loop-detector-verdict nil (repeated line 30))))
    (is (null (apply #'loop-detector-verdict nil (repeated line 20))))))

(deftest a-short-stutter-needs-four-repeats-and-180-characters ()
  ;; "so " 60 times covers 180 characters; 58 times, 174.
  (flet ((stutter (times)
           (nle::exact-suffix-cycle (format nil "I think ~{~a~}" (repeated "so " times)))))
    (is (stutter 60))
    (is (null (stutter 58)))))

(deftest digits-and-whitespace-never-loop-and-punctuation-only-past-a-kilobyte ()
  ;; A zero-filled dump is ordinary output at any length; one punctuation
  ;; cycle is a loop past 1024 characters, and not yet at 1000.
  (flet ((ending (head unit times)
           (nle::exact-suffix-cycle (format nil "~a~{~a~}" head (repeated unit times)))))
    (is (null (ending (format nil "dump:~%") (format nil "0000 0000 0000~%") 300)))
    (is (null (ending "Really" "?!" 500)))
    (is (ending "Really" "?!" 520))))

(defun drifting-paragraph (i)
  "Paragraph I of a loop that restates one intent with a drifting verb."
  (format nil "Pass ~d. I need to ~a that the configuration loader handles the empty case correctly before I continue with the remaining checks, because a dropped default section would make every later step read stale values.~%~%"
          i (nth (mod i 5) '("verify" "confirm" "double-check" "validate" "recheck"))))

(deftest near-duplicate-paragraphs-loop-at-the-semantic-level-only ()
  ;; The exact level reads no byte-identical cycle in them.
  (let ((paragraphs (loop for i from 1 to 12 collect (drifting-paragraph i))))
    (is (search "near-identical paragraphs" (apply #'loop-detector-verdict t paragraphs)))
    (is (null (apply #'loop-detector-verdict nil paragraphs)))))

(defun shuffled-words (words seed)
  "WORDS in an order SEED picks."
  (let ((vector (coerce words 'vector)))
    (loop for i from (1- (length vector)) downto 1
          do (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
             (rotatef (aref vector i) (aref vector (mod (ash seed -8) (1+ i)))))
    (coerce vector 'list)))

(defparameter *filler-words*
  '("just" "doing" "it" "pushing" "ahead" "maintaining" "momentum" "keep" "going" "steady"
    "progress" "moving" "forward" "staying" "focused" "on" "the" "task" "now" "carefully"
    "continuing" "onward" "with" "full" "attention" "and" "energy" "today"))

(defun filler-paragraph (i &optional anchor)
  "Paragraph I of reshuffled filler, naming ANCHOR when one is given."
  (format nil "~@[Now `~a`: ~]~{~a~^ ~}.~%~%" anchor (shuffled-words *filler-words* (* 7919 i))))

(deftest reshuffled-filler-that-names-nothing-new-is-a-stall ()
  (let ((paragraphs (loop for i from 1 to 14 collect (filler-paragraph i))))
    (is (search "reshuffle its recent wording" (apply #'loop-detector-verdict t paragraphs)))))

(deftest naming-a-new-file-each-paragraph-is-progress-not-a-stall ()
  ;; Per-file work recycles its wording and names a new target each time.
  (let ((paragraphs (loop for i from 1 to 30 collect (filler-paragraph i (format nil "stage_~d.lisp" i)))))
    (is (null (apply #'loop-detector-verdict t paragraphs)))))

(deftest code-never-feeds-the-paragraph-checks ()
  ;; Read as prose, these paragraphs are one fingerprint forty times: the
  ;; literals are all that changes, and digits never count. One indented
  ;; skeleton with different literals is code, not a loop.
  (let ((paragraphs
          (loop for i from 1 to 40
                collect (format nil "Next row.~%    (setf (gethash \"key-~d\" table) (list ~d ~d))~%    (incf total ~d)~%    (push (make-entry :index ~d :weight ~d) entries)~%~%"
                                i i (* i i) i i (* 3 i)))))
    (is (null (apply #'loop-detector-verdict t paragraphs)))))

(deftest a-long-healthy-chain-is-never-a-loop ()
  (let ((paragraphs (loop for i from 1 to 60
                          collect (format nil "Looking at `stage_~d.lisp`: ~{~a~^ ~}.~%~%"
                                          i (subseq (shuffled-words *filler-words* (* 31 i)) 0 20)))))
    (is (null (apply #'loop-detector-verdict t paragraphs)))))

(deftests the-loop-guard-level-follows (model setting level)
  (is (eq level (nle::loop-guard-level model setting)))
  (claude "claude-opus-5.5" nil :off)
  (claude-behind-a-relay "anthropic/claude-sonnet-5" nil :off)
  (deepseek "deepseek-v4-flash" nil :semantic)
  (gemini "google/gemini-3.5-flash" nil :semantic)
  (grok "x-ai/grok-4" nil :semantic)
  (every-other-model "gpt-5.5" nil :exact)
  (the-operator-over-the-family "deepseek-v4-flash" :off :off)
  (the-operator-raising-a-level "gpt-5.5" :semantic :semantic))

(deftests a-providers-loop-guard-reads (entry level)
  (is (eq level (nle::configured-loop-guard-level entry)))
  (off (nlk:json-object "loop_guard" "off") :off)
  (false (nlk:decode-json "{\"loop_guard\": false}") :off)
  (exact (nlk:json-object "loop_guard" " Exact ") :exact)
  (semantic (nlk:json-object "loop_guard" "semantic") :semantic)
  (nothing (nlk:json-object "proxy" "socks5://x") nil)
  (a-word-it-does-not-know (nlk:json-object "loop_guard" "loud") nil)
  (no-entry nil nil))

(deftest a-loop-reads-as-a-broken-answer ()
  (let ((condition (make-condition 'nle::provider-reasoning-loop :detail "the model looped")))
    (is (equal "broken_answer" (nle::failover-trigger condition)) "the failover chain reads it as a broken answer")
    (is (equal "reasoning_loop" (nle::provider-failure-reason condition)))
    (is (nle::retryable-provider-error-p condition))))

;;; --- the round: a looping attempt is re-run, the loop dropped ---------------

(defun looping-reasoning-stream ()
  "A stream whose reasoning restates one line until it would run out."
  (apply #'make-sse-stream
         (reasoning-chunk-json (format nil "Let me look at the request first.~%"))
         (append (repeated (reasoning-chunk-json (format nil "Still checking the parser configuration.~%")) 120)
                 (list (chunk-json "LOOPED" :finish-reason "stop")))))

(defun request-text (content)
  "A captured request body as text."
  (if (stringp content) content (sb-ext:octets-to-string (coerce content '(vector (unsigned-byte 8))) :external-format :utf-8)))

(defmacro with-scripted-attempts ((calls bodies) (&rest streams) &body body)
  "Run BODY with dex:post answering STREAMS in turn, the last one again after
them, counting into CALLS and keeping each request body, in order, in BODIES."
  (let ((scripts (gensym "SCRIPTS")))
    `(let ((,calls 0) (,bodies '()) (,scripts (list ,@(mapcar (lambda (s) `(lambda () ,s)) streams)))
           (nle::*retry-base-delay* 0))
       (with-stubbed-fdefinition (dex:post (url &rest args)
                                  (incf ,calls)
                                  (setf ,bodies (append ,bodies (list (request-text (getf args :content)))))
                                  (values (funcall (nth (min (1- ,calls) (1- (length ,scripts))) ,scripts)) 200))
         ,@body))))

(deftest a-looping-attempt-is-re-run-and-the-loop-never-reaches-the-round ()
  (let ((nle::*model* "deepseek-v4-flash")
        (kinds '()))
    (with-scripted-attempts (calls bodies) ((looping-reasoning-stream) (make-sse-stream (chunk-json "substance")))
      (let ((message (nle::call-provider (user-context)
                                         :on-part (lambda (part) (push (nle::stream-part-kind part) kinds)))))
        (is (equal "substance" (gethash "content" message)))
        (is (not (search "Still checking" (or (gethash "reasoning_content" message) ""))))
        (is (= 2 calls) "the loop spent one retry")
        (is (find :stream-retry kinds) "live consumers void what the loop streamed")
        (is (not (search "Still checking" (second bodies))) "the re-run asks the same question, not the loop")))))

(deftest claude-is-never-guarded ()
  (let ((nle::*model* "claude-sonnet-5"))
    (with-scripted-attempts (calls bodies) ((looping-reasoning-stream) (make-sse-stream (chunk-json "substance")))
      (is (equal "LOOPED" (gethash "content" (nle::call-provider (user-context)))))
      (is (= 1 calls)))))

(deftest the-operators-loop-guard-off-holds-a-guarded-model-unguarded ()
  (with-temp-shared-config ("{\"providers\": {\"quiet\": {\"sdk\": \"openai-completions\",
    \"base_url\": \"https://quiet.example/v1\", \"api_key\": \"k\", \"loop_guard\": \"off\"}}}")
    (let ((nle::*provider* "quiet")
          (nle::*model* "deepseek-v4-flash")
          (nle::*endpoint* nil)
          (nle::*api-key* nil))
      (with-scripted-attempts (calls bodies) ((looping-reasoning-stream) (make-sse-stream (chunk-json "substance")))
        ;; providers.quiet.loop_guard "off" holds DeepSeek unguarded
        (is (equal "LOOPED" (gethash "content" (nle::call-provider (user-context)))))
        (is (= 1 calls))))))

(deftest a-loop-past-the-rerun-ceiling-is-re-run-never-continued ()
  ;; A ceiling of 0 puts every cut attempt past it: a truncation there is
  ;; continued, its stream handed back as the model's own reply. A loop handed
  ;; back that way is the loop resumed, so it is re-run.
  (let ((nle::*model* "deepseek-v4-flash")
        (nle::*stream-rerun-ceiling* 0))
    (with-scripted-attempts (calls bodies) ((looping-reasoning-stream) (make-sse-stream (chunk-json "substance")))
      (let ((message (nle::call-provider (user-context))))
        (is (equal "substance" (gethash "content" message)))
        (is (not (search "Still checking" (or (gethash "reasoning_content" message) ""))))
        (is (not (search "Still checking" (second bodies))) "the re-run carries no seal of the loop")))))

(deftest a-loop-in-the-answer-is-re-run ()
  (let ((nle::*model* "gpt-5.5"))
    (with-scripted-attempts (calls bodies)
        ((apply #'make-sse-stream (chunk-json (format nil "Answer:~%"))
                (repeated (chunk-json (format nil "The value is unchanged.~%")) 120))
         (make-sse-stream (chunk-json "substance")))
      (is (equal "substance" (gethash "content" (nle::call-provider (user-context)))))
      (is (= 2 calls)))))

(deftest tool-call-arguments-are-never-read-as-a-loop ()
  ;; What follows a tool call's start is its arguments: a file the model
  ;; writes may repeat one line a thousand times.
  (let ((nle::*model* "gpt-5.5")
        (payload (format nil "~{~a~}" (repeated "    pass\\n" 400))))
    (with-scripted-attempts (calls bodies)
        ((make-sse-stream (chunk-json "Writing it.")
                          (tool-call-chunk-json 0 "call_1" "eval" (format nil "{\"form\": \"~a\"}" payload))))
      (let ((message (nle::call-provider (user-context))))
        (is (= 1 calls))
        (is (plusp (length (gethash "tool_calls" message))))))))

(deftests a-servers-repetition-stop-is-a-loop (last-chunk)
  ;; vLLM's repetition detection stops a looping generation itself and says
  ;; so in either field: what it kept is the loop cut short.
  (let ((nle::*model* "deepseek-v4-flash"))
    (with-scripted-attempts (calls bodies)
        ((make-sse-stream (chunk-json "partial LOOPED answer") last-chunk)
         (make-sse-stream (chunk-json "substance")))
      (is (equal "substance" (gethash "content" (nle::call-provider (user-context)))))
      (is (= 2 calls))
      (is (not (search "LOOPED" (second bodies))) "the re-run carries none of what it kept")))
  (as-finish-reason (chunk-json "" :finish-reason "repetition"))
  (as-stop-reason-beside-a-clean-finish
   (json-line (nlk:json-object "choices" (vector (nlk:json-object "delta" (nlk:json-object)
                                                                 "finish_reason" "stop"
                                                                 "stop_reason" "repetition_detected"))))))

(deftest a-loop-on-every-attempt-fails-within-the-stream-budget ()
  (let ((nle::*model* "deepseek-v4-flash")
        (nle::*max-stream-retries* 2))
    (with-scripted-attempts (calls bodies) ((looping-reasoning-stream))
      (let ((condition (signals-error nle::provider-reasoning-loop (nle::call-provider (user-context)))))
        (is condition "a model that loops every time fails the round")
        (is (search "the model looped" (nle::provider-error-detail condition))))
      (is (= 3 calls) "the first attempt and its two retries"))))
