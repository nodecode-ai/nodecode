;;;; turn-test.lisp --- A2 gate: session admission, the eval tool, the loop.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two tiers. Everything up to the network tests runs offline and deterministic
;;;; by stubbing CALL-PROVIDER, which is exactly the seam that isolates the loop
;;;; from the provider. The two live tests are gated on NODECODE_LIVE=1 and
;;;; prove the plan's stated gate: a real turn completes, and a tool call round
;;;; trips through EVAL and back into the loop.
;;;;
;;;; Stubbing is done by rebinding a function via SB-EXT, not by injecting a
;;;; parameter into the loop: the loop is the mutation target and should not grow
;;;; test seams.

(in-package #:nodecode.test)

(nlk:access (active nodecode.evolved::compiled-turn-context) (b nle::display-block)
            (candidate nle::effective-provider-config) (cap nle::model-capability)
            (chat nodecode.evolved::compiled-turn-context) (config nle::effective-provider-config)
            (context nodecode.evolved::compiled-turn-context) (first-unit nle::history-turn)
            (metadata nle::stream-part) (part nle::stream-part) (process nle::exec-process)
            (request-context nodecode.evolved::compiled-turn-context) (row nle::transcript-row)
            (unit nle::history-turn) (usage nle::provider-usage))

;;; --- helpers ------------------------------------------------------------

(defun wire-message (role &key content tool-calls)
  "Build a provider-shaped assistant/tool message for stubs."
  (nlk:json-object "role" role
                   "content" (or content "")
                   :opt "tool_calls" tool-calls))

(defun wire-tool-call (id name arguments-json)
  "Build one OpenAI-shaped tool call. ARGUMENTS-JSON is a string, as providers
actually send it."
  (nlk:json-object "id" id
                   "type" "function"
                   "function" (nlk:json-object "name" name
                                               "arguments" arguments-json)))

(defun execute-wire-call (id name arguments &optional (context (compiled-context '())))
  "EXECUTE-TOOL-CALL over the wire call WIRE-TOOL-CALL builds from ID, NAME and
ARGUMENTS, in CONTEXT — by default a compiled context holding no messages."
  (nle::execute-tool-call (wire-tool-call id name arguments) context))

(defun compiled-context (messages &optional prior-context)
  (nle::compile-turn-context
   (nle::snapshot-context-request
    nil :prior-context prior-context :messages messages)))

(defun user-context (&optional (text "x"))
  "A compiled turn context holding exactly one user message — the whole
argument almost every provider-lane gate below wants, and the reason none of
them spells the message out."
  (compiled-context (list (nle::message "user" text))))

(defun live-p ()
  (equal (uiop:getenv "NODECODE_LIVE") "1"))

;;; --- the harness teaches the control surface ----------------------------

(deftest the-system-prompt-is-four-lines-that-point-inside ()
  ;; The prompt is a pointer, not the manual: carrying the whole manual would
  ;; spend every round on rules that apply once or never. The manual has other
  ;; homes — the tool description, the tool result at the moment a rule applies,
  ;; HELP read once and cached with the conversation — so the prompt is a
  ;; pointer: one tool, and where everything else is. It keeps what no moment
  ;; teaches: the organism's disposition, the economy that makes keeping pay,
  ;; and the ask to post findings while it works.
  (is-carrying (prompt nle:*system-prompt*)
    (is (< (length prompt) 600) "short enough to be a pointer")
    (is (= 3 (count #\Newline prompt)) "four sentences, one per line")
    ("organism that adapts" "the disposition: it changes itself to fit")
    ("eval" "the one tool is named")
    ("persists across boots" "defining is keeping")
    ("(help)" "and the way in")
    ("spend them once" "the economy that makes keeping pay")
    ("Keep the operator posted" "the commentary ask")))

(deftest help-teaches-the-selection-pair (let ((index (nle:help))
                                               (operator (nle:help :operator))))
  ;; A verb the harness does not name may as well not exist — and the
  ;; index is where a verb is named now.
  (dolist (needle '("  models - " "  select-model - " "  select-effort - "))
    (is (search needle index) (format nil "the index lists ~a" needle)))
  (dolist (needle '("(models)" "(select-model" "(select-effort"))
    (is (search needle operator) (format nil "the operator topic teaches ~a" needle))))

(deftest help-docs-topic-sends-a-user-question-to-the-reference ()
  (is-carrying (text (nle:help :docs))
    ("https://nodecode.ai/docs/SLUG.md" "every page is markdown at its own address")
    ("https://nodecode.ai/docs/llms.txt" "the map")
    ("install - " "the pages are listed by slug")
    ("this image is the truth" "a stale page loses to the image"))
  (is (search "https://nodecode.ai/docs" (second (assoc :docs nle::*help-topics*))) "the summary every session carries names the reference"))

(deftest help-indexes-verbs-and-topics-and-answers-a-miss-with-the-index ()
  (is-carrying (index (nle:help))
    (dolist (verb nle::*help-verbs*)
      (is (search (format nil "~%  ~(~a~) - " verb) index)
          (format nil "the index lists ~a" verb)))
    ("define-hook - Register (lambda LAMBDA-LIST . BODY) on NAME"
     "a dotted pair in a docstring is not a full stop")
    (loop for (topic) in nle::*help-topics*
          do (is (search (format nil "~%  ~(~s~) - " topic) index)
                 (format nil "the index lists ~s" topic)))
    ("(help 'verb)" "the index teaches the verb read")
    ("(help :topic)" "and the topic read"))
  (is (eql 0 (search (format nil "sh: ~a" (documentation 'nle:sh 'function))
                     (nle:help 'sh))))
  (is (search (nlk:definition-prose 'nle:sh) (nle:help 'sh)) "the verb's manual follows its contract")
  ;; (help :eval) is the rest of the contract, never the description again
  (is-carrying (text (nle:help :eval)) "(eval-interrupt N)" "(sh (list" "(patch name old new)"
    (:absent (getf (second (nle::find-tool "eval")) :description)))
  (is (string= (nle:help "layer") (nle:help :layer)))
  (is-carrying (miss (nle:help 'no-such-verb-anywhere))
    (is (eql 0 (search "no help for " miss)) "a miss says so")
    ("NO-SUCH-VERB-ANYWHERE; the index" "and names what it missed")
    ("  sh - " "and answers the index so the next call can pick")))

;;; --- session state ------------------------------------------------------

(deftest session-starts-empty-and-carries-an-id (let ((session (nle::make-session))))
  (is (stringp (nle:session-id session)) "session has a string id")
  (is (null session.messages) "history starts empty")
  (is (not session.in-flight-p)))

(deftest admit-sets-the-latch-without-duplicating-durable-input ()
  (let ((session (nle::make-session)))
    (nle:admit session "hello")
    (is (nle::session-in-flight-p session) "latch is set")
    (is (null session.messages))))

(deftest admit-refuses-a-re-entrant-turn (let ((session (nle::make-session))))
  ;; The concrete hazard: the only tool is EVAL, so a model can call (run "...")
  ;; from inside its own tool call and recurse without bound.
  (nle:admit session "first")
  (is (signals-error nle::turn-in-flight
        (nle:admit session "second"))))

(deftest admit-refuses-oversized-input (let ((session (nle::make-session))
                                             (nle::*max-input-characters* 8)))
  (is (signals-error nle::input-too-large
        (nle:admit session "far too long for the limit"))))

(deftest admit-refuses-a-non-string (let ((session (nle::make-session))))
  (is (signals-error type-error (nle:admit session 42))))

(deftest finishing-a-turn-only-releases-the-process-latch (let ((session (nle::make-session))))
  (nle:admit session "hello")
  (nle::finish-turn session)
  (is (null session.in-flight-p) "latch released")
  (is (progn (nle::finish-turn session) t)))

(deftest history-is-bounded-so-the-live-heap-stays-flat (let ((session (nle::make-session))
                                                              (nle::*max-history-messages* 3)))
  (dotimes (i 10)
    (nle::append-message
     session (nle::message "user" (format nil "m~d" i))))
  (is (= 3 (length session.messages)) "history trims to the bound")
  (is (equal "m9" (nle::message-content
                   (first (last session.messages)))))
  (is (equal "m7" (nle::message-content
                   (first session.messages)))))

(deftest forget-clears-in-heap-history (let ((session (nle::make-session))))
  (nle:admit session "hello")
  (nle:forget session)
  (is (null session.messages) "history cleared"))

;;; --- the eval tool ------------------------------------------------------

(deftest eval-tool-computes-rather-than-recalls ()
  (is-eval "(expt 2 30)" (:= "1073741824" "arithmetic is evaluated")))

(deftest eval-tool-returns-errors-as-text ()
  ;; A failed form is information for the model, not a crash of the turn.
  (is-eval "(/ 1 0)"
    ("ERROR" "division by zero is reported as text")))

(deftest eval-tool-reports-unreadable-input-as-text ()
  (is-eval "(princ \"never closed"
    ("ERROR" "a malformed form is reported, not signalled")))

(deftest eval-tool-answers-the-parens-left-open-on-one-line ()
  ;; A closer at the end of a one-line snippet does not finish the form; it
  ;; moves whatever followed the open paren inside it. The refusal is not
  ;; SBCL's opaque END-OF-FILE: it carries the count, the opener and the way
  ;; out.
  (is-eval "(let ((x 2)) (list x (* x 3)"
    ("2 parens left open" result)
    ("(let ((x 2)) (list x (* x 3)" "the opener is shown")
    ("across lines" "and the way out is named")
    (:absent "(2 6)" "nothing was evaluated")))

(deftest eval-tool-closes-by-indentation-across-lines ()
  ;; Two definitions, the first never closed: its indentation says the
  ;; second is a sibling, not its body, so the closers land after line 3
  ;; and nothing nests that the model did not nest.
  (is-eval (format nil "(defun snippet-indent-f (x)~%  (let ((y (* x 2)))~%    (list x y)~%(snippet-indent-f 4)")
    ("[reader closed 2 parens after line 3: (defun snippet-indent-f (x)" result)
    ("=> SNIPPET-INDENT-F" "the first form defined its function")
    ("=> (4 8)" "and the second called it as a sibling")))

(deftest eval-tool-never-orphans-a-closer-the-model-wrote ()
  ;; Indentation alone would close the LET after line 1, but the model's
  ;; own ")" on line 2 closes it: the written depth bounds every site, so
  ;; the one missing closer goes to the end and every paren still reads.
  (is-eval (format nil "(let ((a 1))~%(list a))~%(list 2")
    ("[reader closed 1 paren at end: (list 2]" result)
    ("=> (1)" result)
    ("=> (2)" result)))

(deftest eval-tool-drops-the-closer-that-closes-nothing ()
  ;; The mirror miscount: sb26's first 35 calls on the new reader had 3
  ;; multi-line forms with one closer too many at the end. An orphan ")"
  ;; cannot read; dropping it leaves the tree the model wrote, and the
  ;; note says so.
  (is-eval "(list 1 2))"
    ("[reader dropped 1 paren that closed nothing at char 10 (line 1)]" result)
    ("(1 2)" "the form evaluated without it"))
  (is-eval (format nil "(defun snippet-orphan-f (x)~%  (list x)))~%(snippet-orphan-f 3")
    ("[reader closed 1 paren at end: (snippet-orphan-f 3, dropped 1 paren that closed nothing at char 39 (line 2)]"
     result)
    ("=> (3)" "both repairs in one snippet")))

(deftest eval-tool-names-the-lexeme-that-never-ends ()
  (dolist (case '(("string opened at char 7 never ends: \"open" "(princ \"open" "an open string")
                  ("#| comment opened at char 7 never ends" "(princ #| open" "an open block comment")
                  ("|symbol| opened at char 7 never ends" "(princ |open" "an open bar symbol")))
    (destructuring-bind (verdict snippet why) case
      (is (search verdict (nle:eval-string snippet)) why))))

(deftest eval-tool-balance-scan-lexes-like-the-reader ()
  ;; Parens inside strings, escapes, character literals, both comment
  ;; kinds and |bars| are not structure: a form carrying all of them
  ;; evaluates, and one balanced only by such a paren is still refused.
  (is (equal "(1 2 41 1 1)"
             (nle:eval-string "(progn (list (length \"(\") (length \"\\\")\") (char-code #\\)) (progn #| ( #| ( |# |# 1) (length (symbol-name '|)|))) ; )
)")))
  ;; The string's ")" is not a closer, so this one-line snippet is one paren
  ;; short and is answered rather than completed.
  (is-eval "(list \")\"" ("1 paren left open" "a paren inside a string does not close a form")))

(deftest eval-snippet-evaluates-every-toplevel-form-in-order ()
  ;; Every toplevel form of the text runs, in order, one => line each: a
  ;; snippet is the unit of work, not a form. The ARC trials sent 303
  ;; one-form one-liners because the tool said "a single form", and the
  ;; second form of any two would have been dropped without a sound.
  (let ((result (nle:eval-string "(defparameter *snippet-a* 1) (incf *snippet-a*) (* *snippet-a* 10)")))
    (is (equal (format nil "=> *SNIPPET-A*~%=> 2~%=> 20") result) result))
  (is-eval "(* *snippet-a* 10)" (:= "20" "one form answers as it always has, no => prefix")))

(deftest eval-snippet-a-failing-form-names-itself-and-the-rest-stands ()
  (is-eval "(defparameter *snippet-b* 5) (error \"boom\") (setf *snippet-b* 0)"
    ("ERROR in form 2 of 3 (form 1 evaluated and stands: *SNIPPET-B*; form 3 not evaluated): SIMPLE-ERROR: boom"
     result))
  (is-eval "*snippet-b*" (:= "5" "form 1 stands, form 3 never ran")))

(deftest a-one-line-snippet-is-answered-not-completed ()
  ;; A one-line snippet one paren short would have the model's own test call
  ;; closed into the body of the function it was defining, and the image
  ;; would die of a fatal heap exhaustion with the wrong definition kept.
  (is-eval "(progn (defun one-line-gate (n) (+ n 1)) (one-line-gate 3)"
    ("ERROR" result)
    ("1 paren left open" "the count")
    ("(progn (defun one-line-gate" "and the opener")
    (is (null (find-symbol "ONE-LINE-GATE" :nodecode.evolved))))
  (is-eval "(progn
  (defun multi-line-gate (n) (+ n 1))
  (multi-line-gate 3)"
    ("closed" "a multi-line snippet still completes by indentation")
    ("4" result)
    (is (let ((symbol (find-symbol "MULTI-LINE-GATE" :nodecode.evolved)))
          (and symbol (fboundp symbol)))))
  (is-eval "(+ 1 2)"
    ("3" "a balanced one-line snippet is untouched")
    (:absent "reader" "with no reader note"))
  (is-eval "(+ 1 2))" ("3" "an extra closer on one line still drops, as before"))
  (let ((symbol (find-symbol "MULTI-LINE-GATE" :nodecode.evolved)))
    (when (and symbol (fboundp symbol)) (fmakunbound symbol))))

(deftest an-evaluation-is-guarded-without-a-gateway ()
  ;; Without the guard, a synchronous snippet in a headless image is in no
  ;; list at all and the model's own recursion takes the image down with
  ;; "Heap exhausted, game over".
  (is (null nle::*background-exec-watcher*) "no gateway watcher in the test image")
  (let ((nle::*eval-heap-soft-fraction* 0.0001)
        (nle::*eval-guard-interval-seconds* 0.05))
    (is-eval "(let ((acc nil)) (dotimes (i 40000000) (push (make-array 64) acc)) (length acc))"
      ("runaway" result)
      ("heap runaway" "named as the heap, not the stack")))
  (is nle::*eval-guard-worker* "the guard runs after an evaluation")
  (is (null (nle::guarded-evaluating-threads)) "and the thread list empties")
  (is-eval "(+ 1 1)" ("2" "an ordinary snippet is untouched")))

(deftest the-responses-lane-replays-encrypted-reasoning-without-a-summary ()
  ;; The summary is the visible gloss; the encrypted content is the state.
  ;; Without this, requiring the gloss would drop the state with it.
  (let ((message (make-hash-table :test #'equal)))
    (setf (gethash "role" message) "assistant"
          (gethash "content" message) ""
          (gethash "reasoning_content" message) ""
          (gethash "reasoning_items" message)
          (vector (nle::json-object "id" "rs_123" "encrypted_content" "ENCRYPTED-BLOB")))
    (let* ((context (nle::make-compiled-turn-context
                     :system-prompt "s" :messages (vector message) :tools nil
                     :provider-config (nle::make-effective-provider-config
                                       :provider "p" :model "m" :api-key "k"
                                       :endpoint "http://x/responses"
                                       :lane "openai-responses"
                                       :reasoning-effort "xhigh")))
           (json (with-output-to-string (out)
                   (shasht:write-json
                    (coerce (nle::responses-input-items context) 'vector) out))))
      (is (search "\"rs_123\"" json) json)
      (is (search "ENCRYPTED-BLOB" json) "the encrypted state rides with it")
      (is (search "\"summary\": []" json) "and the absent gloss is an empty list, not a blank part"))))

(deftest eval-snippet-an-error-report-bounds-its-datum ()
  ;; An error report bounds its datum: without this, a 64x64 frame
  ;; handed to COUNT would print the whole array under one line of
  ;; TYPE-ERROR.
  (is-eval "(count 5 (make-array '(64 64) :initial-element 5))"
    ("ERROR: TYPE-ERROR" result)
    ("..." "the datum prints at the ladder's rung, the cut marked")
    (is (< (length result) 1500) (format nil "~d characters" (length result))))
  (is-eval "(error \"~a\" (make-string 3000 :initial-element #\\x))"
    ("ERROR: SIMPLE-ERROR: xxx" result)
    ("[report elided: 3,000 characters in full]" "a long report is cut and counted")
    (is (< (length result) 2200) (format nil "~d characters" (length result)))))

(deftest eval-snippet-a-reader-error-runs-nothing ()
  ;; The reader's verdict lands before the first evaluation, never between
  ;; two: a snippet that does not read runs nothing.
  (is-eval "(defparameter *snippet-c* 1) (list 1 . 2 . 3)"
    ("ERROR" result))
  (is-eval "*snippet-c*" ("UNBOUND-VARIABLE" "the first form never evaluated")))

(deftest eval-raw-text-reads-exactly-what-is-written ()
  ;; Quotes, backslashes, parens and a newline at the end: nothing escaped,
  ;; nothing lost -- the file a model writes is the file on disk.
  (is-eval (format nil "(string= #>T~%\"q\" \\ (~%T (coerce (list #\\\" #\\q #\\\" #\\Space #\\\\ #\\Space #\\( #\\Newline) 'string))")
    (:= "T" "quotes, a backslash and an open paren read as themselves"))
  (is-eval (format nil "(length #>E~%a\\nb~%E)")
    (:= "5" "a backslash-n is two characters, then the line's newline")
    (:absent "[reader" "and no escape warning: raw text is not a string literal")))

(deftest eval-raw-text-ends-at-a-line-that-starts-with-its-tag ()
  (is-eval (format nil "(equal (list #>END~%abc~%END 42) (list (format nil \"abc~~%\") 42))")
    (:= "T" "what follows the closing tag on its line reads on"))
  (is-eval (format nil "(equal (list #>END~%  abc~%  END) (list (format nil \"  abc~~%\")))")
    (:= "T" "an indented closing line closes it, and the text keeps its own indentation"))
  (is-eval (format nil "(equal (list #>END~%ENDING~%END) (list (format nil \"ENDING~~%\")))")
    (:= "T" "a line that only begins with the tag's letters is text")))

(deftest eval-raw-text-is-not-structure-for-completion ()
  ;; A Python line with an open paren inside raw text must neither open a
  ;; form nor take a closer; the snippet's own missing paren still lands by
  ;; indentation.
  (is-eval (format nil "(defparameter *raw-text-a*~%  (concatenate 'string #>PY~%def f(:~%PY~%    \"x\")")
    ("*RAW-TEXT-A*" result))
  (is-eval "(string= *raw-text-a* (format nil \"def f(:~%x\"))" (:= "T" "the raw text and the string after it, as written")))

(deftest eval-raw-text-that-never-ends-runs-nothing ()
  (is-eval (format nil "(defparameter *raw-text-b* 1)~%(list #>END~%abc")
    ("raw text" result)
    ("never ends" "the open raw text is named"))
  (is-eval "(boundp '*raw-text-b*)" (:= "NIL" "nothing ran"))
  (is-eval (format nil "(list #>~%abc)")
    ("#> wants a tag" "a #> without a tag says what it wants")))

(deftest eval-raw-text-reads-in-a-thread-whose-readtable-lacks-it ()
  ;; The dumped binary's eval met `no dispatch function defined for #\>':
  ;; its readtable never had it. A snippet reads raw text whatever readtable
  ;; its thread was given.
  (let ((*readtable* (copy-readtable nil)))
    (is-eval (format nil "(length #>E~%ab~%E)") (:= "3" "raw text reads in a plain standard readtable"))))

(deftest raw-text-loads-from-a-file ()
  ;; The layer keeps the bytes a snippet was read from, and a launch loads
  ;; them with LOAD: the image's readtable reads raw text, not only eval's.
  (let ((path (uiop:tmpize-pathname (merge-pathnames "raw-text.lisp" (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output :if-exists :supersede)
             (format out "(in-package #:nodecode.evolved)~%(defparameter *raw-text-c* #>END~%a \"b\" \\c~%END)~%"))
           (load path)
           (is (equal (format nil "a \"b\" \\c~%") (symbol-value (find-symbol "*RAW-TEXT-C*" '#:nodecode.evolved))) "loaded as written"))
      (delete-file path))))

(deftest eval-value-elides-as-a-tree-not-a-cut ()
  ;; A value past the limit prints under *PRINT-LEVEL*/*PRINT-LENGTH*
  ;; instead of being cut mid-token: the model reads a tree with an
  ;; elided tail, not an unbalanced blob it must fetch again. Judged at
  ;; the 8,000 ceiling this gate was written against.
  (let* ((nle::*eval-result-limit* 8000)
         (result (nle:eval-string "(make-list 5000 :initial-element '(1 (2 3)))")))
    (is (< (length result) nle::*eval-result-limit*) "within the limit")
    (is (search "[value elided: " result) result)
    (is (search " ...)" result) "the list tail is elided, not cut")
    (is (= (count #\( result) (count #\) result)) "the printed value balances"))
  (let* ((nle::*eval-result-limit* 8000)
         (result (nle:eval-string "(make-string 20000 :initial-element #\\a)")))
    (is (search "[value elided: a string of 20,000 characters" result) result)
    (is (< (length result) nle::*eval-result-limit*) "within the limit")
    (is (char= #\" (char result (1- (length result)))))))

(deftest eval-snippet-binds-the-repls-history-variables ()
  ;; Without this, the value in the plist field holding the answer would be
  ;; gone with the call. The REPL's rule: * is the last finished snippet's last
  ;; value, / the list of its values one per form, ** and // the snippet
  ;; before, and a failed snippet leaves them as they were.
  (nle:eval-string "(+ 1 1) (+ 2 2) (+ 3 3)")
  (is-eval "(list * /)"
    (:= "(6 (2 4 6))" "* is the last value of the last snippet, / its values one per form"))
  (is-eval "(list ** //)" (:= "(6 (2 4 6))" "** and // reach the snippet before"))
  (nle:eval-string "(list :answer (make-string 9000 :initial-element #\\a))")
  (is-eval "(error \"boom\")" ("ERROR" "a failing snippet"))
  (is-eval "(length (getf * :answer))"
    (:= "9000" "a failed snippet leaves the history as it was: * is the last snippet that finished")))

(deftest eval-snippet-history-is-the-sessions-own (with-session-store ("history-a"))
  ;; A gateway evaluates several sessions' snippets at once: the bindings are
  ;; dynamic and keyed by the session, never one image-wide register.
  (ensure-durable-session "history-b")
  (let ((a (nlk:admit-turn "history-a" "history-a-cmd" "go"))
        (b (nlk:admit-turn "history-b" "history-b-cmd" "go")))
    (nlk:with-cleanup ((nlk:cancel-turn b "test cleanup") (nlk:cancel-turn a "test cleanup"))
      (let ((nle::*current-durable-turn* a))
        (nle:eval-string "(list :mine 1)"))
      (let ((nle::*current-durable-turn* b))
        (is-eval "*" (:= "NIL" "another session's snippet sees none of it")))
      (let ((nle::*current-durable-turn* a))
        (is-eval "(getf * :mine)" (:= "1" "the session finds its own"))))))

(deftest eval-value-cut-names-the-form-that-reads-the-rest (let ((nle::*eval-result-limit* 8000)))
  ;; Without this, a cut that asks for a slice would name nothing a
  ;; slice could be taken from. Every cut names its form, and the form
  ;; reads the whole from *.
  (let* ((result (nle:eval-string "(make-string 20000 :initial-element #\\a)"))
         (at (search "; (subseq * " result))
         (from (and at (parse-integer result :start (+ at 12) :junk-allowed t))))
    (is (integerp from) result)
    (when from
      (is (search (format nil "; (subseq * ~d) reads on in your next snippet]" from) result) result)
      (is (equal (princ-to-string (- 20000 from))
                 (nle:eval-string (format nil "(length (subseq * ~d))" from))))))
  (is-eval "(list :id 1 :output \"\" :value (make-string 9000 :initial-element #\\v))"
    ("the whole value is * in your next snippet: (getf * :VALUE) is 9,002 characters]" result))
  (is-eval "(length (getf * :value))" (:= "9000" "the plist's bulky part, read whole"))
  (is-eval "(make-list 5000 :initial-element '(1 (2 3)))"
    ("the whole value is * in your next snippet: (nth i *) or (subseq * a b) reads a part]" result))
  (is-eval "(length *)" (:= "5000" "a list of small parts is read in slices"))
  (is-eval "(make-string 9000 :initial-element #\\b) :last"
    ("[9,002 characters, (SIMPLE-ARRAY CHARACTER (9000)), elided: only the snippet's last value prints in full; (nth 0 /) reads it in your next snippet]"
     result))
  (is-eval "(length (nth 0 /))" (:= "9000" "an earlier form's value, read whole")))

(deftest eval-snippet-prints-only-its-last-value-in-full ()
  ;; ft09 on the new reader: (setf *before-grid* (copy-grid (arc:frame)))
  ;; (arc:act ...) printed the saved grid, 8,388 characters elided to a
  ;; corner, five times. Only the last value shows in full.
  (is-eval "(defparameter *snippet-h* nil) (setf *snippet-h* (make-list 3000 :initial-element 1)) (length *snippet-h*)"
    ("=> [6,001 characters, CONS, elided: only the snippet's last value prints in full; (nth 1 /) reads it in your next snippet]"
     result)
    ("=> 3000" "the last value in full")
    (is (< (length result) 400) "and the snippet's result stays short")))

(deftest snippet-note-is-the-leading-comment-block ()
  ;; PRO-LONG's plan line, in the one place a model writing no prose still
  ;; writes a sentence: the comment a snippet opens with.
  (is (equal "L1: left moves the box continued"
             (nle::snippet-note (format nil ";; L1: left moves the box~%;;; continued~%(+ 1 2)"))))
  (is (equal "after a blank"
             (nle::snippet-note (format nil "~%  ; after a blank~%(+ 1 2)"))))
  (is (null (nle::snippet-note "(+ 1 2) ; not a note")))
  (is (null (nle::snippet-note (format nil "(+ 1 2)~%;; too late"))))
  (is (null (nle::snippet-note ";;")) "an empty comment is no note")
  (let ((note (nle::snippet-note (format nil ";; ~a~%(+ 1 2)"
                                         (make-string 400 :initial-element #\n)))))
    (is (= nle::*snippet-note-limit* (length note)) "a monster note is cut")
    (is (char= #\… (char note (1- (length note)))) "with a disclosure")))

(deftest snippet-record-carries-the-note ()
  ;; What the eviction stub projects: the note the snippet opened with.
  (is (equal "L1: three" (gethash "note" (nle::snippet-record "L1: three"))) "the note rides")
  (is (null (nle::snippet-record nil)) "no note, no object"))

(deftest a-snippet-publishes-its-record-on-its-result-fact (with-session-store ("snippet-rec"))
  ;; The durable seam: a snippet evaluated by a tool call lands its note under
  ;; the result fact's metadata, where the fold reads it after the round is
  ;; gone; a snippet without one adds no key.
  (with-durable-turn (turn "snippet-rec" :command-id "snippet-cmd" :input "go" :current t)
    (execute-wire-call "call-noted" "eval" "{\"form\": \";; L1: three\\n(+ 1 2)\"}")
    (execute-wire-call "call-plain" "eval" "{\"form\": \"(+ 1 2)\"}")
    (let* ((facts (remove-if-not
                   (lambda (event) (typep event 'nlk:tool-result-event))
                   (nle::collect-visible-session-events "snippet-rec")))
           (noted (find "call-noted" facts
                        :key #'nlk:tool-result-event-call-id :test #'equal))
           (plain (find "call-plain" facts
                        :key #'nlk:tool-result-event-call-id :test #'equal))
           (record (let ((metadata (nlk::tool-result-event-metadata noted)))
                     (and (hash-table-p metadata) (gethash "snippet" metadata)))))
      (is-present (record record) "the noted snippet's fact carries its record"
        (is (equal "L1: three" (gethash "note" record)) "the note"))
      (let ((metadata (nlk::tool-result-event-metadata plain)))
        (is (not (and (hash-table-p metadata) (gethash "snippet" metadata))))))))

(defun watchable-source-p (source)
  "Whether the snippet SOURCE, read in the package the eval snippet reads in, may
be watched."
  (and (nle::watchable-form-p (let ((*package* (find-package '#:nodecode.evolved)))
                           (read-from-string source)))
       t))

(deftest a-watched-snippet-only-reads-what-can-change ()
  ;; The rule reads the form: something in it can move, and nothing in it
  ;; assigns, destroys, defines or reads the REPL's history.
  (is-table (expected source) (eq expected (watchable-source-p source))
    (t "(length (masks))" "a reader the model wrote")
    (t "*level*" "a variable")
    (t "(reduce #'* (sizes))" "* named as a function is not the history")
    (nil "(+ 1 2)" "nothing in it can change")
    (nil "(setf *level* 2)" "an assignment")
    (nil "(incf (gethash :k *seen*))" "an assignment through a place")
    (nil "(sort *moves* #'<)" "destroys what it is handed")
    (nil "(defun masks () nil)" "a definition")
    (nil "(length *)" "the last snippet's value moves under every snippet")
    (nil "(eval-interrupt 1006)" "stops a background evaluation")
    (nil "(eval-status 1006)" "takes the output since the last look")
    (nil "(eval-await 1006)" "parks on a background evaluation and takes its output")
    (t "(list-evals)" "listing the evaluations only reads")))

(deftest a-read-snippet-is-watched-and-comes-back-only-when-it-changes
    (with-session-store ("watch"))
  ;; The model reads once; the image reads after every later snippet and
  ;; speaks only for the reading that moved.
  (with-durable-turn (turn "watch" :current t)
    (nle:eval-string "(defparameter *watch-n* 3)")
    (is-eval "(+ *watch-n* 0)"
      (:= (format nil "3~%[watching: shown again when its value changes]")))
    (is-eval "(defparameter *watch-other* 1)"
      (:absent "[watched" "what it reads did not move, so nothing comes back"))
    (is-eval "(setf *watch-n* 2)"
      ("[watched (+ *watch-n* 0): 3 -> 2]"
       "an assignment is not watched: it runs the watched snippets, and the moved one comes back"))
    (is-eval "(setf *watch-other* 5)" (:absent "[watched"))))

(deftest a-reader-defined-and-called-in-one-snippet-is-watched
    (with-session-store ("watch-reader"))
  ;; The model's reader is a definition and its call in one snippet. The
  ;; call is the read, and the one watched; the definition makes the snippet
  ;; act, so the snippets already watched run first.
  (with-durable-turn (turn "watch-reader" :current t)
    (nle:eval-string "(defparameter *watch-seen* 1) (defparameter *watch-left* 5)")
    (is-eval "*watch-left*" ("[watching:"))
    (nle:eval-string "(setf *watch-left* 4)")
    (is-eval "(defun watch-facts () (list :seen *watch-seen*)) (watch-facts)"
      ("[watching:" "the call that closes the snippet is watched")
      (:absent "[watched (watch-facts)" "a form watched again is not run again"))
    (is-eval "(setf *watch-seen* 2)" ("[watched (watch-facts): (:SEEN 1) -> (:SEEN 2)]"))))

(deftest a-long-watched-value-names-the-places-that-changed
    (with-session-store ("watch-long"))
  ;; Watched, a change to it says where, not both whole.
  (with-durable-turn (turn "watch-long" :current t)
    (nle:eval-string "(defparameter *watch-snippets* (loop for i below 40 collect (list i (* 2 i)))) (defparameter *watch-level* 1)")
    (is-eval "(list *watch-snippets* :level *watch-level*)" ("[watching:" "a long read is watched too"))
    (is-eval "(push (list 99 99) *watch-snippets*)"
      ("[watched (list *watch-snippets* :level *watch-level*): [0] +((99 99))]"
       "a list that grew says what came, at its index path"))
    (is-eval "(setf *watch-level* 2)"
      ("[watched (list *watch-snippets* :level *watch-level*): [2] 1 -> 2]"
       "a place inside a long value shows whole"))
    (is-eval "(setf *watch-snippets* (loop for i below 40 collect (list (+ i 50) i)))"
      ("[watched (list *watch-snippets* :level *watch-level*): now (((50 0) (51 1)"
       "a change as long as the value shows the new value whole")
      (:absent "…" "never cut"))))

(deftest a-watched-snippet-that-would-act-is-closed-before-it-does
    (with-session-store ("watch-act"))
  ;; Watched snippets only read: one that would act on a run again is refused
  ;; at its verb's stamp, before the verb runs, and closed.
  (with-durable-turn (turn "watch-act" :current t)
    (nle:eval-string "(defparameter *watch-armed* nil) (defparameter *watch-fired* 0)")
    (nle:eval-string "(defun watch-fire () (if *watch-armed* (progn (sh \"true\") (incf *watch-fired*)) :idle))")
    (is-eval "(watch-fire)" ("[watching:"))
    (is-eval "(setf *watch-armed* t)"
      ("[no longer watching (watch-fire): it reached outside the image]"))
    (let ((fired (symbol-value (find-symbol "*WATCH-FIRED*" '#:nodecode.evolved))))
      (is (eql 0 fired) "the verb refused before it ran"))
    (is-eval "(setf *watch-armed* nil)" (:absent "watch-fire" "a closed snippet stays closed"))))

(deftest a-snippet-that-reaches-outside-the-image-is-not-watched
    (with-session-store ("watch-io"))
  ;; A read that took a system call - a file, a socket, a process - may not
  ;; be repeated behind the model's back, whatever it returned.
  (with-durable-turn (turn "watch-io" :current t)
    (nle:eval-string "(defparameter *watch-path* (namestring (asdf:system-source-file :nodecode)))")
    (is-eval "(length (uiop:read-file-string *watch-path*))" (:absent "[watching"))
    (is-eval "(length *watch-path*)" ("[watching:" "the same session watches a read of the image"))))

(deftest a-cell-verb-that-acts-in-the-image-is-never-watched
    (with-session-store ("watch-stamp"))
  ;; A cell verb that only hands the running answer its buttons, reaching
  ;; nothing outside the image, would be read as a read without the rule
  ;; below: watched, it would run again after every later snippet and put an
  ;; earlier question's buttons on later answers. A cell verb's stamp says
  ;; what it is: one naming no family, or one that changes something, acts;
  ;; one naming a read's family stays a read.
  (with-durable-turn (turn "watch-stamp" :current t)
    (nle:eval-string "(defparameter *watch-handed* 0) (defparameter *watch-shown* 1) (defparameter *watch-armed* nil)")
    (nle:eval-string "(defun watch-hand () (receipt \"calls\" (list :verb \"cell:hand\")) (incf *watch-handed*))")
    (nle:eval-string "(defun watch-show () (receipt \"calls\" (list :verb \"cell:show\" :family \"recall\")) *watch-shown*)")
    (nle:eval-string "(defun watch-maybe () (if *watch-armed* (watch-hand) :idle))")
    (is-eval "(watch-hand)" (:absent "[watching" "a verb that acts in the image is not watched"))
    (is-eval "(watch-show)" ("[watching:" "a cell's read stays watched"))
    (is-eval "(setf *watch-shown* 2)" ("[watched (watch-show): 1 -> 2]"))
    (is-eval "(watch-maybe)" ("[watching:"))
    (is-eval "(setf *watch-armed* t)"
      ("[no longer watching (watch-maybe): it reached outside the image]"
       "a run again that reaches the stamp is closed"))
    (let ((handed (symbol-value (find-symbol "*WATCH-HANDED*" '#:nodecode.evolved))))
      (is (eql 1 handed) "the verb ran once: no run again repeated it, and the closed one refused before it acted"))))

(deftest eval-tool-hints-where-a-misqualified-symbol-lives ()
  ;; "Symbol RECALL-LOG not found in the COMMON-LISP package" is true and
  ;; useless; the hint names the package that holds it, and that the eval
  ;; snippet reaches it bare.
  (is-eval "(cl:recall-log)"
    ("SIMPLE-READER-PACKAGE-ERROR" "the reader's error is kept")
    ("hint: RECALL-LOG is NODECODE.KERNEL:RECALL-LOG (function) — reachable unqualified from here"
     result)))

(deftest eval-tool-hints-that-an-invented-name-is-defined-nowhere ()
  ;; The reader interns the name it then fails to call, so presence in the
  ;; evolved package must not read as existence.
  (is-eval "(make-batch-output-stream)"
    ("UNDEFINED-FUNCTION" "the error is reported")
    ("hint: nothing named MAKE-BATCH-OUTPUT-STREAM is defined in any package" result))
  (is-eval "(list *no-such-parameter*)"
    ("hint: nothing named *NO-SUCH-PARAMETER* is defined in any package"
     "an unbound variable gets the same answer")))

(deftest eval-tool-hints-the-nearest-defined-name ()
  ;; A qualified miss in a package that exists answers that package's own
  ;; exports, the names sharing text with the miss first.
  (is-eval "(uiop:run-prog \"x\")"
    ("hint: UIOP/DRIVER exports no RUN-PROG; its exported names: run-program" result)))

(deftest eval-tool-hints-what-a-package-exports-for-a-verb-it-lacks ()
  ;; `cron:list' was answered "LIST is COMMON-LISP:LIST -- reachable
  ;; unqualified from here" 16 times in three days: true of every package,
  ;; and no lead to the cron verb the form reached for.
  (let ((package (make-package (string (gensym "HINT-PROBE-")) :use '(#:common-lisp))))
    (unwind-protect
         (flet ((verb (name &key (export t))
                  (let ((symbol (intern name package)))
                    (setf (fdefinition symbol) (lambda () nil))
                    (when export (export symbol package)))))
           (verb "LIST-JOBS") (verb "SHOW") (verb "HIDDEN" :export nil)
           (let* ((name (package-name package))
                  (listed (nle:eval-string (format nil "(~a:list)" name)))
                  (hidden (nle:eval-string (format nil "(~a:hidden)" name))))
             (is (search (format nil "hint: ~a exports no LIST; its exported names: list-jobs, show" name) listed) listed)
             (is (not (search "COMMON-LISP:LIST" listed)) "a symbol the package already reaches is no lead")
             (is (search (format nil "hint: ~a exports no HIDDEN (~a::HIDDEN is internal: function)" name name) hidden) hidden)))
      (delete-package package))))

(deftest eval-tool-hints-past-an-exact-name-that-is-not-a-function ()
  ;; SB-EXT:FILE-EXISTS is a condition class; a form that called
  ;; FILE-EXISTS wanted the predicate beside it.
  (is-eval "(uiop:file-exists \"x\")"
    ("hint: FILE-EXISTS is SB-EXT:FILE-EXISTS (class); similar: " result)
    ("UIOP/FILESYSTEM:FILE-EXISTS-P (function)" result)))

(deftest eval-tool-keeps-a-missing-package-report-bare ()
  ;; "Package SB-IO does not exist" already says everything; no hint rides.
  (is-eval "(sb-io:get-string)"
    ("Package SB-IO does not exist" result)
    (:absent "hint:" "nothing to add")))

(deftest eval-names-sh-to-the-model ()
  (is-carrying (description (gethash "description"
                                     (gethash "function" (aref (nle::tools-payload) 0))))
    ("(sh \"ls -la\")" "the tool description shows the shell verb verbatim")
    (:absent "run-program" "the elided run-program examples are gone")
    (is (search "  sh - " (nle:help)) "the index names it")))

(deftest eval-tool-survives-a-condition-whose-report-re-signals ()
  ;; A form compiled with errors is a SIMPLE-PROGRAM-ERROR whose
  ;; format-control embeds the model's own bytes (SBCL splices the failing
  ;; source and diagnostic in with no arguments), so ~A on the caught
  ;; condition would re-run FORMAT over a broken control string and signal
  ;; a fresh FORMAT-ERROR from inside the handler arm, past every
  ;; protection, killing the whole --disable-debugger image.
  ;; The LET wrapper is load-bearing: it forces EVAL through the compiler,
  ;; where the constant control string fails at macroexpansion; a bare call
  ;; errors at runtime with a report that prints safely.
  (is-eval "(let ((text (format nil \"plan \\\"quoted\\\" end~\"))) text)"
    (is (stringp result) "the arm answers instead of re-signalling")
    ("ERROR" "the compile failure is reported as text")
    ("FORMAT" "the salvaged diagnostic still names the broken directive")))

(deftest eval-tool-captures-printed-output ()
  (is-eval "(progn (princ \"side\") :done)"
    ("side" "printed output is captured")
    ("DONE" "the value is still reported")))

(deftest eval-tool-leaks-nothing-to-the-operators-terminal ()
  ;; EVAL compiles, so a model-written form that references an undefined
  ;; function emits a compiler style-warning on *ERROR-OUTPUT*. That stream is
  ;; the operator's terminal, not the model's tool result, so a leak there looks
  ;; like a crashed turn. The diagnostic belongs in the returned string only.
  (let* ((leaked (make-string-output-stream))
         (result (let ((*error-output* leaked)
                       (*trace-output* leaked))
                   (nle:eval-string "(no-such-function-anywhere 1)"))))
    (is (string= "" (get-output-stream-string leaked)))
    (is (search "ERROR" result) "the failure is reported to the model instead")))

(deftest eval-tool-truncates-a-runaway-result ()
  ;; An unbounded result becomes the next prompt.
  (let* ((nle::*eval-result-limit* 100)
         (result (nle:eval-string "(make-string 5000 :initial-element #\\x)")))
    (is (<= (length result) 200) "result is truncated")
    (is (search "truncated" result) "truncation is disclosed")))

(deftest eval-tool-owns-its-printer-syntax (let ((*print-readably* t)
                                                 (*print-base* 16)
                                                 (*print-pretty* t)))
  ;; Worker threads arrive with bordeaux-threads' WITH-STANDARD-IO-SYNTAX
  ;; defaults, under which *PRINT-READABLY* is T. The regression: base-strings
  ;; printed as #A((n) BASE-CHAR . ...) forms and any unprintable value killed
  ;; the whole result with PRINT-NOT-READABLE — three tool rounds where one
  ;; would do, and escaped noise inflating the transcript.
  (is-eval "(list (symbol-name :run-program))"
    (:= "(\"RUN-PROGRAM\")" "base-strings print as plain literals, not #A forms"))
  (is-eval "255" (:= "255" "numbers print in decimal whatever the caller's radix"))
  (is-eval "(find-package :cl)"
    (:absent "PRINT-NOT-READABLE" "unprintable values report as #<...>, not as a printer error")
    ("#<PACKAGE" "the value still names itself")))

(deftest eval-tool-refuses-read-time-evaluation ()
  ;; READ-FROM-STRING with *READ-EVAL* left true would evaluate #. at read time,
  ;; before any of this function's error handling applies.
  (is-eval "'#.(+ 1 2)"
    ("ERROR" "#. is refused rather than evaluated at read time")))

;;; --- the tool table -----------------------------------------------------

(deftest eval-is-registered-by-default ()
  (is (nle::find-tool "eval") "the eval tool is registered"))

(deftest tools-payload-has-the-provider-shape (let* ((payload (nle::tools-payload))
                                                     (entry (aref payload 0))))
  (is (vectorp payload) "payload is a JSON array")
  (is (equal "function" (gethash "type" entry)) "entry is a function tool")
  (let ((fn (gethash "function" entry)))
    (is (equal "eval" (gethash "name" fn)) "name is carried")
    (is (hash-table-p (gethash "parameters" fn)) "parameters is an object")))

(deftest register-tool-replaces-rather-than-duplicates (let ((nle::*tools* nle::*tools*)))
  ;; Mutable material: the organism registering a tool twice should not end up
  ;; advertising two tools with one name.
  (nle:register-tool "probe" (list :description "d" :parameters (make-hash-table :test #'equal))
                     (lambda (args) (declare (ignore args)) "one"))
  (nle:register-tool "probe" (list :description "d" :parameters (make-hash-table :test #'equal))
                     (lambda (args) (declare (ignore args)) "two"))
  (is (= 1 (count "probe" nle::*tools* :key #'first :test #'string=)))
  (is (equal "two" (funcall (third (nle::find-tool "probe")) nil))))

(deftest tools-payload-reads-the-same-however-the-tools-arrived (let ((nle::*tools* '())))
  ;; The schemas sit in the prompt prefix. A tool registered again goes to the
  ;; head of *TOOLS*, so a cell restarted moved its tool to the end of the
  ;; payload and every session re-read its conversation.
  (flet ((tool (name)
           (nle:register-tool name (list :description "d" :parameters (make-hash-table :test #'equal))
                              (lambda (args) (declare (ignore args)) name)))
         (names ()
           (loop for entry across (nle::tools-payload)
                 collect (nlk:json-value (nlk:json-value entry :object "function") :text "name"))))
    (mapc #'tool '("probe" "eval" "websearch"))
    (is (equal '("eval" "probe" "websearch") (names)))
    (nle:unregister-tool "eval")
    (tool "eval")
    (tool "probe")
    (is (equal '("eval" "probe" "websearch") (names)) "restarted, the same payload")))

;;; --- tool dispatch ------------------------------------------------------

(deftest tool-call-arguments-arrive-as-a-json-string ()
  ;; OpenAI-compatible providers send arguments as a JSON string, not an object,
  ;; so dispatch has to parse a second time. Verified against the live provider.
  (let* ((call (wire-tool-call "call-1" "eval" "{\"form\": \"(* 6 7)\"}"))
         (result (nle::execute-tool-call
                  call (compiled-context '()))))
    (is (equal "tool" (gethash "role" result)) "result is a tool message")
    (is (equal "call-1" (gethash "tool_call_id" result)) "call id is echoed back")
    (is (equal "42" (nle::message-content result)) "the form was evaluated")))

(deftest an-unknown-tool-answers-rather-than-signals ()
  (let* ((call (wire-tool-call "call-2" "no_such_tool" "{}"))
         (result (nle::execute-tool-call
                  call (compiled-context '()))))
    (is (search "\"code\":\"unknown_tool\"" (nle::message-content result)))))

(deftest unparseable-tool-arguments-answer-rather-than-signal ()
  (let* ((call (wire-tool-call "call-3" "eval" "{not json"))
         (result (nle::execute-tool-call
                  call (compiled-context '()))))
    (is (search "ERROR" (nle::message-content result)))))

(deftest missing-form-error-echoes-what-was-received ()
  ;; A model told only "requires form" retries the identical call; one shown
  ;; that its arguments decoded to {} can rewrite them.
  (let* ((call (wire-tool-call "call-4" "eval" "{\"from\": \"(f)\"}"))
         (result (nle::execute-tool-call
                  call (compiled-context '()))))
    (is-carrying (text (nle::message-content result))
      "\"code\":\"missing_required_argument\"" "\"expected\":[\"form\"]"
      ("\"received\":[\"from\"]" "the keys it did receive are shown back"))))

(deftest tool-preflight-refuses-before-running-the-tool (let ((called nil)
                                                              (nle::*tools* '())))
  ;; The refusal is an admission result, not a tool exception: it identifies
  ;; the failure, preserves the received keys, and gives the model a direct
  ;; repair instruction before any hook or tool body can run.
  (nle:register-tool
   "typed-probe"
   (list :description "probe"
         :parameters
         (nlk:make-json-object
          "type" "object"
          "properties" (nlk:make-json-object
                        "count" (nlk:make-json-object "type" "integer"))
          "required" (vector "count")))
   (lambda (arguments)
     (declare (ignore arguments))
     (setf called t)
     "ran"))
  (let ((result (execute-wire-call "call-preflight" "typed-probe" "{\"coount\":\"3\"}")))
    (is-carrying (content (nle::message-content result))
      ("tool preflight failed" "preflight answers as a tool result")
      ("missing_required_argument" "the error has a stable code")
      ("coount" "received keys are named")
      ("count" "the required key is named")
      ("retryable" "the model is told this can be repaired"))
    (is (null called) "the malformed call never reaches the tool body")))

(deftest tool-preflight-reports-a-type-mismatch (let ((nle::*tools* '())))
  (nle:register-tool
   "typed-probe"
   (list :description "probe"
         :parameters
         (nlk:make-json-object
          "type" "object"
          "properties" (nlk:make-json-object
                        "count" (nlk:make-json-object "type" "integer"))))
   (lambda (arguments) (declare (ignore arguments)) "ran"))
  (let ((result (execute-wire-call "call-type" "typed-probe" "{\"count\":\"three\"}")))
    (is-carrying (content (nle::message-content result))
      ("argument_type_mismatch" "the type error has a stable code")
      ("count" "the invalid field is named")
      ("integer" "the expected type is named"))))


;;; --- a tool whose shape is one declaration --------------------------------
;;; The divergence a hand-written schema beside a hand-written body allowed:
;;; the provider was told one shape and the body read another, and nothing in
;;; the image compared them. A contract is both projections of one FIELDS list.

(nlk:define-contract probe-tool-arguments
  (:doc "A probe tool whose arguments are declared, not written twice.")
  ("count" :integer :required t :slot how-many :doc "how many")
  ("label" :text :default "none" :doc "what to call it")
  ("mode" :string :default "fast" :options '("fast" "slow") :doc "the mode"))

(defun register-contracted-probe (seen)
  "Register `typed_probe' against the declaration, recording the value its body
was called with in the SEEN box."
  (nle:register-tool "typed_probe" (nlk:contract 'probe-tool-arguments)
                     (lambda (arguments)
                       (setf (car seen) arguments)
                       (format nil "ran ~a"
                               (probe-tool-arguments-how-many arguments)))))

(deftest tool-contract-advertises-the-shape-it-dispatches (with-saved-globals (nle::*tools*))
  (let ((seen (list nil))
        (nle::*tools* '()))
    (register-contracted-probe seen)
    (let* ((function (nlk:json-value (aref (nle::tools-payload) 0) :object "function"))
           (parameters (nlk:json-value function :object "parameters"))
           (contract (nlk:contract 'probe-tool-arguments)))
      (is (equal "typed_probe" (nlk:json-value function :text "name")))
      (is (equal contract.doc
                 (nlk:json-value function :text "description")))
      (is (equalp (nlk:contract-schema contract) parameters))
      (is (equalp #("count") (nlk:json-value parameters :array "required")))
      (is (equalp #("fast" "slow")
                  (nlk:json-value parameters :array "properties" "mode" "enum"))))
    (is (equal "ran 2"
               (nle::message-content
                (execute-wire-call "call-typed-ok" "typed_probe" "{\"count\":2}"))))
    (is (probe-tool-arguments-p (car seen)))
    (is (equal "none" (probe-tool-arguments-label (car seen))))
    (is (equal "fast" (probe-tool-arguments-mode (car seen))))))

(deftest tool-contract-refuses-a-malformed-call-before-the-body (with-saved-globals (nle::*tools*))
  (let ((seen (list nil))
        (nle::*tools* '()))
    (register-contracted-probe seen)
    (is-carrying (text (nle::message-content
                        (execute-wire-call "call-typed-type" "typed_probe"
                                           "{\"count\":\"2\"}")))
      ("tool preflight failed" "the refusal is a tool result, as every preflight is")
      ("contract_refused" "under a code of its own")
      ("count" "naming the member's wire path")
      ("an integer" "and what the declaration wanted")
      ("retryable" "in the shape the model already repairs from"))
    (is (null (car seen)) "the malformed call never reached the body")
    (is-carrying (text (nle::message-content
                        (execute-wire-call "call-typed-missing" "typed_probe" "{}")))
      ("contract_refused") ("count must be present"))
    (is (null (car seen)))
    (is-carrying (text (nle::message-content
                        (execute-wire-call "call-typed-enum" "typed_probe"
                                           "{\"count\":1,\"mode\":\"medium\"}")))
      ("contract_refused") ("one of fast, slow"))
    (is (null (car seen)) "including the enum the preflight could not check before")))

(deftest tool-contract-holds-against-a-hook-that-rewrote-the-arguments ()
  ;; The :TOOL chain sees the decoded table, because advice is a surface over
  ;; the raw call. The record is materialized INSIDE the chain, so a hook that
  ;; rewrote the arguments is held to the same declaration the preflight
  ;; refused by — a rewrite it does not accept fails the call closed rather
  ;; than reaching the body.
  (with-saved-globals (nle::*tools* nle:*hooks*)
    (let ((seen (list nil))
          (nle::*tools* '()))
      (register-contracted-probe seen)
      (nle:hook :tool "rewrite-inward"
                (lambda (op next)
                  (funcall next (list* :arguments (nlk:json-object "count" 9) op))))
      (is (equal "ran 9" (nle::message-content
                          (execute-wire-call "call-typed-hook" "typed_probe"
                                             "{\"count\":1}"))))
      (nle:hook :tool "rewrite-inward"
                (lambda (op next)
                  (funcall next (list* :arguments (nlk:json-object "count" "nine") op))))
      (setf (car seen) nil)
      (is-carrying (text (nle::message-content
                          (execute-wire-call "call-typed-hook-bad" "typed_probe"
                                             "{\"count\":1}")))
        ("count must be an integer" "a rewrite it does not accept refuses"))
      (is (null (car seen)) "and the body never ran"))))

(deftest tool-dispatch-lands-its-measured-duration-on-the-fact (with-session-store ("tool-timing"))
  ;; The dispatch seam already measured the tool body for the live phase
  ;; note; the durable TURN.TOOL_RESULT fact now carries the same number,
  ;; so per-tool timing survives as event-log truth (recall surfaces it).
  ;; The unknown-tool refusal executed nothing and invents nothing.
  (let ((nle::*tools* nle::*tools*))
    (nle:register-tool "timed_probe"
                       (list :description "d"
                             :parameters (make-hash-table :test #'equal))
                       (lambda (args)
                         (declare (ignore args))
                         (sleep 0.01)
                         "ok"))
    (with-durable-turn (turn "tool-timing" :command-id "tool-timing-cmd" :input "go" :current t)
      (execute-wire-call "call-t1" "timed_probe" "{}")
      (execute-wire-call "call-t2" "ghost" "{}")
      (let* ((facts (session-events "tool-timing"
                                    :type 'nlk:tool-result-event))
             (measured (find "call-t1" facts
                             :key #'nlk:tool-result-event-call-id
                             :test #'equal))
             (refusal (find "call-t2" facts
                            :key #'nlk:tool-result-event-call-id
                            :test #'equal)))
        (is (typep (nlk:tool-result-event-duration-ms measured)
                   '(integer 1)))
        (is (null (nlk:tool-result-event-duration-ms refusal)))))))

;;; --- the :tool hook point ------------------------------------------------
;;; The around-chain contract: veto, rewrite in, rewrite out, wrap, order,
;;; and the structural fix over tool wrapping — a hook is not registered AS
;;; the tool, so re-registering the tool cannot shed it.

(defun hook-probe-tool (ran &optional (answer "probe ran"))
  "Register a probe tool that records the arguments it was called with by
pushing them onto the RAN box, then answer ANSWER."
  (nle:register-tool "probe" (list :description "probe" :parameters nil)
                     (lambda (arguments)
                       (push arguments (car ran))
                       answer))
  (compiled-context '()))

(deftest tool-hook-veto-answers-as-an-ordinary-tool-result (with-saved-globals (nle::*tools*))
  (let* ((ran (list '()))
         (context (hook-probe-tool ran)))
    (nle:hook :tool "veto"
              (lambda (op next)
                (declare (ignore next))
                (format nil "ERROR: refused: ~a" (getf op :name))))
    (let ((result (execute-wire-call "call-h1" "probe" "{}" context)))
      (is (equal "tool" (gethash "role" result)))
      (is (equal "call-h1" (gethash "tool_call_id" result)))
      (is (equal "ERROR: refused: probe" (nle::message-content result)))
      (is (null (car ran)) "the tool body never ran"))))

(deftest tool-hook-can-rewrite-the-arguments-inward (with-saved-globals (nle::*tools*))
  (let* ((ran (list '()))
         (context (hook-probe-tool ran)))
    (nle:hook :tool "rewrite"
              (lambda (op next &aux (arguments (nlk:json-object "form" "(rewritten)")))
                (funcall next (list* :arguments arguments op))))
    (execute-wire-call "call-h2" "probe" "{\"form\": \"(original)\"}" context)
    (is (equal "(rewritten)" (gethash "form" (first (car ran)))))))

(deftest tool-hook-can-rewrite-the-result-outward (with-saved-globals (nle::*tools*))
  (let* ((ran (list '()))
         (context (hook-probe-tool ran "inner answer")))
    (nle:hook :tool "redact"
              (lambda (op next)
                (format nil "[~a]" (funcall next op))))
    (let ((result (execute-wire-call "call-h3" "probe" "{}" context)))
      (is (equal "[inner answer]" (nle::message-content result)))
      (is (= 1 (length (car ran))) "and the tool ran exactly once"))))

(deftest tool-hook-chain-runs-first-registered-outermost (with-saved-globals (nle::*tools*))
  (let* ((ran (list '()))
         (context (hook-probe-tool ran))
         (trace '()))
    (nle:hook :tool "outer"
              (lambda (op next)
                (push :outer-in trace)
                (prog1 (funcall next op) (push :outer-out trace))))
    (nle:hook :tool "inner"
              (lambda (op next)
                (push :inner-in trace)
                (prog1 (funcall next op) (push :inner-out trace))))
    (execute-wire-call "call-h4" "probe" "{}" context)
    (is (equal '(:outer-in :inner-in :inner-out :outer-out)
               (reverse trace)))))

(deftest tool-hook-registry-replaces-by-key-in-place (let ((order '())))
  (nle:hook :tool "a" (lambda (op next)
                        (declare (ignore op next)) (push :a order)))
  (nle:hook :tool "b" (lambda (op next)
                        (declare (ignore op next)) (push :b order)))
  (nle:hook :tool "a" (lambda (op next)
                        (declare (ignore op next)) (push :a2 order)))
  (is (equal '("a" "b")
             (mapcar #'car (cdr (assoc :tool nle:*hooks*)))))
  (is (nle:unhook :tool "a") "unhook reports the removal")
  (is (not (nle:unhook :tool "a")) "and a second unhook reports nothing")
  (is (equal '("b") (mapcar #'car (cdr (assoc :tool nle:*hooks*)))))
  (nle:unhook :tool "b")
  (is (null (assoc :tool nle:*hooks*))))

(deftest tool-hook-survives-tool-re-registration (with-saved-globals (nle::*tools*))
  ;; THE STRUCTURAL REGRESSION over the wrapper era: a wrapper registered AS
  ;; the tool was shed soundlessly by any later REGISTER-TOOL of the same
  ;; name. A hook is not in the tool table at all.
  (let ((ran (list '())))
    (hook-probe-tool ran "before")
    (nle:hook :tool "guarding"
              (lambda (op next)
                (if (equal "probe" (getf op :name))
                    "ERROR: refused"
                    (funcall next op))))
    ;; Something re-registers the tool mid-life — the organism giving
    ;; itself a new ability. The context compiles AFTER, as a fresh
    ;; round's snapshot would.
    (let ((context (hook-probe-tool ran "after")))
      (is (equal "ERROR: refused"
                 (nle::message-content
                  (execute-wire-call "call-h5" "probe" "{}" context))))
      (is (null (car ran)) "and still short-circuits the new body"))))

(deftest a-signalling-tool-hook-fails-the-call-closed (with-saved-globals (nle::*tools*))
  ;; A hook that crashed has no opinion, and a hook with no opinion must not
  ;; be mistaken for one that approved: the ERROR arm turns the signal into
  ;; an ERROR: result and the tool body never runs.
  (let* ((ran (list '()))
         (context (hook-probe-tool ran)))
    (nle:hook :tool "broken"
              (lambda (op next)
                (declare (ignore op next))
                (error "hook exploded")))
    (let ((result (execute-wire-call "call-h6" "probe" "{}" context)))
      (is (search "ERROR:" (nle::message-content result)))
      (is (search "hook exploded" (nle::message-content result)))
      (is (null (car ran)) "the tool body never ran"))))

;;; --- the :round hook point -----------------------------------------------
;;; One model request about to be built: every round of a turn, its first
;;; included, before the history folds, so what a hook records there is in
;;; that very request -- a running turn hears it without stopping.

(deftest round-hook-runs-every-round-and-its-reminder-reaches-that-request
    (with-session-store ("round-hook"))
  (let ((nle::*tools* nle::*tools*)
        (seen '())
        (asked '()))
    (nle:register-tool "step" (probe-schema) (constantly "stepped"))
    (nle:hook :round "probe"
              (lambda (op next)
                (push (list (getf op :session-id) (getf op :round) (and (getf op :turn) t)) seen)
                (when (= 1 (getf op :round))
                  (nlk:record-turn-context-prompt (getf op :turn) :text "[round note]"))
                (funcall next op)))
    (with-provider-stub (context
                         (push (coerce (nle::compiled-turn-context-messages context) 'list) asked)
                         (if (= 1 (length asked))
                             (wire-message "assistant" :tool-calls
                                           (vector (wire-tool-call "c-round" "step" "{}")))
                             (wire-message "assistant" :content "done")))
      (nle:run "go" :session (nle:intern-session "round-hook") :command-id "round-hook-cmd"))
    (flet ((noted-p (messages)
             (some (lambda (message)
                     (search "[round note]" (nle::content-text (gethash "content" message))))
                   messages)))
      ;; once a round, with the session and its running turn
      (is (equal '(("round-hook" 0 t) ("round-hook" 1 t)) (reverse seen)))
      (is (not (noted-p (second asked))) "the first request carries no note")
      (is (noted-p (first asked)) "the round that recorded it sends it, mid-turn"))))

(deftest a-runaway-eval-backgrounds-at-the-yield-window ()
  ;; One eval form globbing the whole monorepo pinned a turn worker at
  ;; full CPU for half an hour. The fix is the run tool's own AX, not a
  ;; deadline: at the yield window the call hands back a running envelope
  ;; and the form keeps evaluating in the background, where
  ;; (eval-interrupt ID) can abort it.
  (nlk:with-cleanup ((nle::destroy-all-execs
                      (nle::exec-registry-instance)))
    (let* ((start (get-internal-real-time))
           (result (execute-wire-call "call-spin"
                                      "eval"
                                      (nlk:json-object "form" "(loop)" "yield_time_ms" 200)))
           (content (nle::message-content result))
           (elapsed (/ (- (get-internal-real-time) start)
                       internal-time-units-per-second)))
      (is (search "\"running\"" content))
      (is (search "eval_id" content) "the envelope carries the handle")
      (is (search "eval-await" content))
      (is (< elapsed 10) "the yield window fired, not the form's return")
      (let ((id (getf (first (last (nle::list-evals)))
                      :id)))
        (is (integerp id) "the eval is listed")
        (let ((plist (nle::eval-interrupt id)))
          (is (eq :exited (getf plist :status)))
          (is (eql 130 (getf plist :exit-code))))))))

(deftest a-backgrounded-eval-streams-output-and-lands-its-value ()
  ;; The handoff envelope carries what the form printed before the window,
  ;; and the value rides the follow-up await — never the output stream.
  (nlk:with-cleanup ((nle::destroy-all-execs
                      (nle::exec-registry-instance)))
    (let* ((result (execute-wire-call "call-slow"
                                      "eval"
                                      (nlk:json-object
                                       "form" "(progn (princ \"tick\") (terpri) (sleep 0.5) 41)"
                                       "yield_time_ms" 100)))
           (content (nle::message-content result)))
      (is (search "\"running\"" content) "the form outlived the window")
      (is (search "tick" content) "output printed before the window is carried")
      (let* ((id (getf (first (last (nle::list-evals))) :id))
             (plist (nle::eval-await id :until-exit t
                                                          :yield-ms 10000)))
        (is (eq :exited (getf plist :status)) "the await parks through to the exit")
        (is (equal "41" (getf plist :value)) "the value lands on the follow-up")
        (is (eql 0 (getf plist :exit-code)) "a clean form exits 0")))))

(deftest background-eval-nets-a-condition-escaping-the-seam ()
  ;; Nothing may escape an eval thread: under --disable-debugger an
  ;; unhandled condition in any thread quits the whole image. What reaches
  ;; RUN-BACKGROUND-EVAL's net is the seam's own failure — a condition
  ;; whose report re-signals when printed, or a cancel re-signalled with
  ;; no worker unwind above it — so the net must land a durable error
  ;; exit, not a dead organism.
  (nlk:with-cleanup ((nle::destroy-all-execs
                      (nle::exec-registry-instance)))
    (multiple-value-bind (id process)
        (nle::spawn-background-eval
         (nle::exec-registry-instance)
         "(escaping thunk)"
         (lambda (stream)
           (declare (ignore stream))
           (error "thunk escape")))
      (declare (ignore id))
      (loop repeat 200
            until (eq :exited
                      process.status)
            do (sleep 0.02))
      (is (eq :exited process.status))
      (is (eql 1 process.exit-code))
      (is (search "thunk escape"
                  process.eval-value)))))

(deftest a-fast-eval-answers-inline-through-the-background-seam ()
  ;; The vast majority of forms finish inside the window; their result must
  ;; be byte-for-byte the inline EVAL-STRING shape, printed output recomposed
  ;; with the value.
  (let ((result (execute-wire-call "call-fast"
                                   "eval"
                                   (nlk:json-object "form" "(progn (princ \"side\") :done)"))))
    (is (equal (format nil "side~%:DONE")
               (nle::message-content result)))))

(deftest a-deliberate-park-runs-inline-never-backgrounded ()
  ;; (eval-await ...) exists to outwait the yield window; backgrounding it
  ;; would hand back an eval id for a form whose whole point is waiting.
  ;; The observable is the dispatch decision — no registry entry spawns —
  ;; because the eval RESULT is package-state-dependent (evolve tests
  ;; unintern the evolved package's symbols mid-suite).
  (is (nle::inline-eval-form-p "(eval-await 999999)"))
  (let ((before (length (nle::list-evals))))
    (execute-wire-call "call-park" "eval" (nlk:json-object "form" "(eval-await 999999)"))
    (is (= before (length (nle::list-evals))))))

(defvar *tool-cancel-entered* nil
  "Semaphore the Esc-interrupt test's eval'd form signals on entry.")

(deftest esc-interrupts-a-running-tool-call ()
  ;; The incident's second half: cancellation was only checked between tool
  ;; calls, so Esc was powerless against a running eval. The tool body is a
  ;; cancellable-wait region — the cancel interrupts the parked yield wait
  ;; immediately and unwinds as the ordinary turn-cancelled condition, while
  ;; the evaluation itself survives as a background eval (cleaned up below).
  (unwind-protect
   (with-session-store ("tool-cancel")
    (let* ((turn (nlk:admit-turn "tool-cancel" "tool-cancel-command" "hello"))
           (entered (sb-thread:make-semaphore :name "tool-entered"))
           (outcome nil)
           ;; Snapshotted here, not in the worker: a bare MAKE-THREAD inherits
           ;; no dynamic bindings, so an in-thread COMPILED-CONTEXT reads the
           ;; process globals — and its provider snapshot signals off-net when
           ;; those name a provider the suite's config state cannot resolve.
           (context (compiled-context '()))
           (start (get-internal-real-time)))
      (setf *tool-cancel-entered* entered)
      (let ((worker
              (sb-thread:make-thread
               (lambda ()
                 (setf outcome
                       (nlk:with-handlers ((nlk:turn-cancelled-condition () :cancelled))
                         (let ((nle::*current-durable-turn*
                                 turn))
                           (execute-wire-call "call-esc"
                                              "eval"
                                              "{\"form\": \"(progn (sb-thread:signal-semaphore nodecode.test::*tool-cancel-entered*) (sleep 30))\"}"
                                              context)))))
               :name "tool-cancel-worker")))
        (sb-thread:wait-on-semaphore entered)
        (nlk:request-cancel-turn "tool-cancel"
                                 turn.turn-id "esc")
        (sb-thread:join-thread worker)
        (let ((elapsed (/ (- (get-internal-real-time) start)
                          internal-time-units-per-second)))
          (is (eq :cancelled outcome))
          (is (< elapsed 10))))))
   (nle::destroy-all-execs
    (nle::exec-registry-instance))))

;;; --- a round's calls ------------------------------------------------------
;;; A round's calls run one after another in model order, each body on the
;;; turn thread: a call's fact, its result, then the next call's. Facts,
;;; messages and the wire fold keep that order.

(defun tool-fact-order (session-id)
  "(:CALL . id) and (:RESULT . id) for every tool call and result fact of
SESSION-ID, in log order."
  (loop for event in (session-events session-id :created nil)
        for kind = event.kind
        when (equal kind "turn.tool_call_started")
          collect (cons :call (nlk:tool-call-event-call-id event))
        when (equal kind "turn.tool_result")
          collect (cons :result (nlk:tool-result-event-call-id event))))

(defun probe-schema ()
  "A tool schema with no required parameters."
  (list :description "d" :parameters (make-hash-table :test #'equal)))

(deftest a-round-s-calls-run-one-after-another-in-model-order (with-session-store ("sequence"))
  ;; Three calls of one round: each body starts after the one before it
  ;; ended, each call's fact is followed by its own result fact, and the wire
  ;; fold pairs every call in model order.
  (let ((nle::*tools* nle::*tools*)
        (trace '())
        (rounds 0))
    (nle:register-tool "step" (probe-schema)
                       (lambda (args &aux (id (gethash "id" args)))
                         (push (list :start id) trace)
                         (sleep 0.05)
                         (push (list :end id) trace)
                         (format nil "stepped ~a" id)))
    (with-provider-stub
        (context
         (incf rounds)
         (if (= rounds 1)
             (wire-message "assistant"
                           :tool-calls (vector (wire-tool-call "c1" "step" "{\"id\": \"c1\"}")
                                               (wire-tool-call "c2" "step" "{\"id\": \"c2\"}")
                                               (wire-tool-call "c3" "step" "{\"id\": \"c3\"}")))
             (wire-message "assistant" :content "done")))
      (nle:run "go" :session (nle:intern-session "sequence") :command-id "sequence-cmd"))
    (is (= 2 rounds))
    (is (equal '((:start "c1") (:end "c1") (:start "c2") (:end "c2")
                 (:start "c3") (:end "c3"))
               (reverse trace)) "no body started before the one ahead of it ended")
    (is (equal '((:call . "c1") (:result . "c1") (:call . "c2") (:result . "c2")
                 (:call . "c3") (:result . "c3"))
               (tool-fact-order "sequence")))
    (is (equal '("stepped c1" "stepped c2" "stepped c3")
               (mapcar #'nlk:tool-result-event-result
                       (session-events "sequence" :type 'nlk:tool-result-event))))
    (multiple-value-bind (messages clean-p)
        (nle::wire-messages-from-events (session-events "sequence" :created nil))
      (is clean-p "every call paired on the wire")
      (is (equal '("c1" "c2" "c3")
                 (loop for message in messages
                       when (equal "tool" (gethash "role" message))
                         collect (gethash "tool_call_id" message)))))))

(deftest settle-thunks-run-on-the-turn-thread-before-the-result-fact (with-session-store ("settle"))
  ;; A body may answer (values TEXT SETTLE): the engine calls SETTLE on the
  ;; turn thread, in model order, before the call's result fact records,
  ;; under the call's own metadata binding, so a receipt it stamps rides that
  ;; fact.
  (let ((nle::*tools* nle::*tools*)
        (settled '())
        (driver sb-thread:*current-thread*))
    (nle:register-tool "twice" (probe-schema)
                       (lambda (args &aux (id (gethash "id" args)))
                         (values (format nil "body ~a" id)
                                 (lambda ()
                                   (push (cons id sb-thread:*current-thread*) settled)
                                   (nle:receipt "settled" id)))))
    (with-durable-turn (turn "settle" :command-id "settle-cmd" :input "go" :current t)
      (let ((messages (nle::execute-tool-calls
                       (vector (wire-tool-call "t1" "twice" "{\"id\": \"t1\"}")
                               (wire-tool-call "t2" "twice" "{\"id\": \"t2\"}"))
                       (compiled-context '()))))
        (is (equal '("t1" "t2")
                   (mapcar (lambda (message) (gethash "tool_call_id" message)) messages)))
        ;; each body answers in model order, its seq after it.
        (is (every (lambda (message id)
                     (eql 0 (search (format nil "body ~a~%[seq " id) (gethash "content" message))))
                   messages '("t1" "t2")))
        (is (equal '("t1" "t2") (mapcar #'car (reverse settled))) "settled in model order")
        (is (every (lambda (entry) (eq driver (cdr entry))) settled) "on the turn thread")
        (let ((results (session-events "settle" :type 'nlk:tool-result-event)))
          (is (equal '("t1" "t2") (mapcar #'nlk:tool-result-event-call-id results)))
          (is (every (lambda (result)
                       (let* ((metadata (nlk::tool-result-event-metadata result))
                              (settled (and (hash-table-p metadata)
                                            (gethash "settled" metadata))))
                         (and (vectorp settled)
                              (= 1 (length settled))
                              (equal (nlk:tool-result-event-call-id result)
                                     (aref settled 0)))))
                     results)))))))

(deftest a-call-s-checkpoint-carries-its-post-tree-and-its-own-diff (with-temp-store ())
  ;; Each call is one workspace bracket: a baseline walk (no turn-start
  ;; capture here) and a closing walk, and a checkpoint anchors that tree
  ;; under the call, whose result carries the diff document for that call
  ;; alone.
  (let ((workspace (temp-directory "callwalk")))
    (if (not (nlk:git-available-p))
        (is t "git unavailable: no walk to count")
        (with-workspace-captures (t)
          (nlk:create-session :id "callwalk" :cwd (namestring workspace))
          (let ((nle::*tools* nle::*tools*)
                (walks 0))
            (nle:register-tool "write" (probe-schema)
                               (lambda (args)
                                 (write-temp-file
                                  (merge-pathnames (gethash "file" args) workspace) "x")
                                 "wrote"))
            (let ((turn (nlk:admit-turn "callwalk" "callwalk-cmd" "go"))
                  (context (compiled-context '())))
              (with-stubbed-fdefinitions
                  ((nlk:capture-workspace-tree (store)
                    (incf walks)
                    (funcall original store)))
                (let ((nle::*current-durable-turn* turn))
                  (nle::execute-tool-calls
                   (vector (wire-tool-call "w1" "write" "{\"file\": \"a.txt\"}")
                           (wire-tool-call "w2" "write" "{\"file\": \"b.txt\"}"))
                   context)))
              (is (= 4 walks) "a baseline walk and a closing walk for each call")
              (let ((captures (nlk:session-workspace-captures "callwalk"))
                    (results (session-events "callwalk" :type 'nlk:tool-result-event)))
                (is (= 2 (length captures)) "one checkpoint per call")
                (is (equal '("w1" "w2")
                           (sort (mapcar (lambda (capture) (getf capture :tool-call-id)) captures)
                                 #'string<)) "each keyed by its call")
                (dolist (capture captures)
                  (is (= 40 (length (or (getf capture :tree-sha) ""))))
                  (is (not (equal (getf capture :pre-tree-sha) (getf capture :tree-sha)))))
                (flet ((paths (call-id)
                         (let* ((result (find call-id results
                                              :key #'nlk:tool-result-event-call-id
                                              :test #'equal))
                                (metadata (and result
                                               (nlk::tool-result-event-metadata result)))
                                (document (and (hash-table-p metadata)
                                               (gethash "workspace_diff" metadata))))
                           (and (hash-table-p document)
                                (map 'list (lambda (file) (gethash "path" file))
                                     (gethash "files" document))))))
                  (is (equal '("a.txt") (paths "w1")) "the first call's diff is its own")
                  (is (equal '("b.txt") (paths "w2")) "and the second's")))
              (nlk:complete-turn turn nil)))))))

;;; --- the loop, offline --------------------------------------------------

(deftest loop-returns-on-a-plain-answer (let ((session (nle::make-session))))
  (nle:admit session "hello")
  (with-stub-provider ((wire-message "assistant" :content "hi"))
    (let ((message (nle:agent-loop session)))
      (is (equal "hi" (nle::message-content message)) "answer returned")))
  (is (= 1 (length session.messages))))

(deftest a-turn-that-ends-waiting-on-its-own-background-work-says-so ()
  ;; The model's first word after a round that handed work off to the
  ;; background is status, not the ask's answer: the completion carries the
  ;; count of evaluations still in flight, so every consumer can tell. The
  ;; evaluation is interrupted on the way out.
  (with-session-store ("bg-pending")
    (let ((rounds 0))
      (with-provider-stub
          (context
           (incf rounds)
           (case rounds
             (1 (wire-message
                 "assistant"
                 :tool-calls
                 (vector (wire-tool-call
                          "c1" "eval"
                          "{\"form\": \"(sh \\\"sleep 30\\\")\", \"yield_time_ms\": 200}"))))
             (t (wire-message "assistant" :content "On it — digging."))))
        (nlk:with-cleanup ((dolist (entry (nle::list-evals))
                             (ignore-errors (nle::eval-interrupt (getf entry :id) :force t))))
          (nle:run "dig" :session (nle:intern-session "bg-pending")
                         :command-id "bg-pending-command")
          (let ((payloads (nlk:events :session-id "bg-pending"
                                      :kind nlk::+kind-turn-completed+
                                      :order :newest :as :payloads)))
            (is-present (completion (first payloads)) "the turn completed"
              (is (eql 1 (gethash "background-pending" completion)))))
          (is (null (nle::session-in-flight-p
                     (nle:intern-session "bg-pending")))))))))

(deftest a-turn-answered-after-a-handoff-that-finished-is-not-marked ()
  ;; The handoff's evaluation is gone by the time the model answers: the
  ;; completion carries no count — the word is the ask's answer.
  (with-session-store ("bg-done")
    (let ((rounds 0))
      (with-provider-stub
          (context
           (incf rounds)
           (case rounds
             (1 (wire-message
                 "assistant"
                 :tool-calls
                 (vector (wire-tool-call
                          "c1" "eval"
                          "{\"form\": \"(+ 20 22)\"}"))))
             (t (wire-message "assistant" :content "42"))))
        (nle:run "compute" :session (nle:intern-session "bg-done")
                           :command-id "bg-done-command")
        (let ((payloads (nlk:events :session-id "bg-done"
                                    :kind nlk::+kind-turn-completed+
                                    :order :newest :as :payloads)))
          (is-present (completion (first payloads)) "the turn completed"
            (is (null (gethash "background-pending" completion)))))))))

(deftest loop-runs-a-tool-then-recurs (let ((session (nle::make-session))))
  (nle:admit session "compute")
  (with-stub-provider ((wire-message "assistant"
                                     :tool-calls (vector (wire-tool-call "c1" "eval"
                                                                        "{\"form\": \"(+ 20 22)\"}")))
                       (wire-message "assistant" :content "42"))
    (let ((message (nle:agent-loop session)))
      (is (equal "42" (nle::message-content message)) "final answer returned")))
  (let ((roles (mapcar (lambda (m) (gethash "role" m)) session.messages)))
    (is (equal '("assistant" "tool" "assistant") roles))))

(deftest loop-runs-tool-rounds-unbounded (let* ((session (nle::make-session))
                                                (rounds 64)
                                                (seen 0)))
  ;; Parity with the Zig turn engine: there is no round ceiling. The model
  ;; decides when to stop, so a turn that needs far more than the old twelve
  ;; rounds gets them, and the recursion stays flat while it does.
  (nle:admit session "keep going")
  (with-provider-stub (nil (incf seen)
                           (if (< seen rounds)
                               (wire-message "assistant"
                                             :tool-calls
                                             (vector (wire-tool-call "c" "eval"
                                                                     "{\"form\": \"1\"}")))
                               (wire-message "assistant" :content "done")))
    (let ((message (nle:agent-loop session)))
      (is (equal "done" (nle::message-content message)))
      (is (= rounds seen)))))

(deftest run-releases-the-latch-even-when-the-provider-fails (let ((session (nle::make-session))))
  ;; A failed turn that left the latch set would wedge the session permanently.
  (with-provider-stub (nil (error 'nle::provider-error
                                  :detail "synthetic"))
    (handler-case (nle:run "hello" :session session) (error () nil)))
  (is (null session.in-flight-p))
  (is (nle:admit session "again") "the session accepts a later turn"))

;;; --- request shape ------------------------------------------------------

(deftest request-body-carries-the-system-prompt-first ()
  (let* ((messages (list (nle::message "user" "hi")))
         (body (nle::request-body (compiled-context messages)))
         (wire (gethash "messages" body)))
    (is (equal "system" (gethash "role" (aref wire 0))))
    (is (equal "user" (gethash "role" (aref wire 1))))
    (is (gethash "tools" body) "tools are advertised")))

(deftest request-body-re-roles-history-system-messages ()
  ;; Strict chat templates reject any system message past position 0 (the
  ;; Qwen-family "System message must be at the beginning" 500): the eviction
  ;; stubs ride the history as system-role messages, and the chat lane must
  ;; wire them as user text — the same downgrade the Anthropic and GenAI lanes
  ;; already apply.
  (let* ((stub (nle::message
                "system"
                "[history evicted: seq 1–2 · 1 turns · recall: (recall-log :match \"text\" :before 3)]"))
         (messages (list stub (nle::message "user" "hi")))
         (body (nle::request-body (compiled-context messages)))
         (wire (gethash "messages" body)))
    (is (equal "system" (gethash "role" (aref wire 0))))
    (is (equal "user" (gethash "role" (aref wire 1))))
    (is (search "history evicted" (gethash "content" (aref wire 1))))
    (is (loop for index from 1 below (length wire)
              never (equal "system" (gethash "role" (aref wire index)))))
    (is (equal "system" (gethash "role" stub)))))

(deftest request-body-serializes-to-json ()
  (is-carrying (json (with-output-to-string (out)
                       (shasht:write-json (nle::request-body
                                           (user-context "hi"))
                                          out)))
    ("\"model\"" "model is present")
    ("eval" "the tool is present")))

(deftest compiled-context-freezes-request-shape-and-provider-config ()
  (let* ((messages (list (nle::message "user" "original")))
         (nle::*endpoint* "https://initial.invalid")
         (nle::*model* "initial-model")
         (nle::*api-key* "initial-key")
         (nle::*request-timeout* 17)
         (nle::*max-stream-retries* 4)
         (nle::*retry-base-delay* 3)
         (nle::*system-prompt* "initial prompt")
         (context (compiled-context messages))
         (config context.provider-config))
    (setf nle::*endpoint* "https://mutated.invalid"
          nle::*model* "mutated-model"
          nle::*api-key* "mutated-key"
          nle::*request-timeout* 99
          nle::*max-stream-retries* 0
          nle::*retry-base-delay* 9
          nle::*system-prompt* "mutated prompt")
    (let* ((body (nle::request-body context))
           (wire (gethash "messages" body)))
      (is (equal "initial-model" (gethash "model" body)) "model is captured")
      ;; the system message opens on the prompt frozen at compile time.
      (is (eql 0 (search "initial prompt" (nle::message-content (aref wire 0)))))
      (is (null (search "mutated prompt" (nle::message-content (aref wire 0)))))
      (is (equal "original" (nle::message-content (aref wire 1)))))
    (is (equal "https://initial.invalid"
               config.endpoint))
    (is (equal "initial-key"
               config.api-key))
    (is (= 17 config.request-timeout))
    (is (= 4 config.max-stream-retries))))

(deftest compiled-context-freezes-tool-schema-and-function ()
  (let* ((called nil)
         (parameters (make-hash-table :test #'equal))
         (nle::*tools* '()))
    (nle:register-tool "frozen" (list :description "original" :parameters parameters)
                       (lambda (args) (declare (ignore args)) (setf called :original) "ok"))
    (let ((context (compiled-context '())))
      (nle:register-tool "frozen" (list :description "replacement" :parameters parameters)
                         (lambda (args) (declare (ignore args)) (setf called :replacement) "bad"))
      (let* ((tool (aref context.tools 0))
             (fn (gethash "function" tool))
             (result (execute-wire-call "frozen-call" "frozen" "{}" context)))
        (is (equal "original" (gethash "description" fn)) "advertised schema is frozen")
        (is (equal "ok" (nle::message-content result)))
        (is (eq :original called) "replacement was not invoked")))))

(deftest tool-registered-after-context-compilation-is-refused (let ((called nil)
                                                                    (nle::*tools* '())))
  (let ((context (compiled-context '())))
    (nle:register-tool "late" (list :description "late" :parameters (make-hash-table :test #'equal))
                       (lambda (args) (declare (ignore args)) (setf called t) "bad"))
    (let ((result (execute-wire-call "late-call" "late" "{}" context)))
      (is (search "\"code\":\"unknown_tool\"" (nle::message-content result)))
      (is (null called) "unadmitted function is not called"))))

;;; --- live provider gate -------------------------------------------------
;;; Skipped unless NODECODE_LIVE=1. These are the plan's stated A2 gate.

(deftest live-turn-completes-end-to-end ()
  (if (not (live-p))
      (is t "skipped: set NODECODE_LIVE=1 to run against a real provider")
      (nlk:bind ((session (nle::make-session))
                 ((text message) (nle:run "Reply with exactly the word: ready" :session session)))
        (is (stringp text) "a text answer came back")
        (is (plusp (length text)) "the answer is not empty")
        (is (equal "assistant" (gethash "role" message))))))

(deftest live-tool-call-round-trips-through-eval ()
  (if (not (live-p))
      (is t "skipped: set NODECODE_LIVE=1 to run against a real provider")
      (let ((session (nle::make-session)))
        (nle:run "What is (* 37 41)? Use the eval tool to compute it; do not answer from memory."
                 :session session)
        (let ((roles (mapcar (lambda (m) (gethash "role" m))
                            session.messages)))
          (is (member "tool" roles :test #'equal) "a tool result entered the history")
          (let ((tool-message (find "tool" session.messages
                                    :key (lambda (m) (gethash "role" m)) :test #'equal)))
            (is (search "1517" (nle::message-content tool-message))))))))

;;; =========================================================================

;;; =========================================================================
;;; B1 gates: SSE parser, streaming provider call, lane dispatch
;;; =========================================================================

;;; --- B1: parse-sse-line -------------------------------------------------

(deftests parse-sse-line-classifies (line kind kind-why value-check value-why)
  (multiple-value-bind (got value) (nle::parse-sse-line line)
    (is (eq kind got) kind-why)
    (is value-check value-why))
  (data "data: {\"id\":\"x\"}" :data "data: prefix yields :data"
        (equal "{\"id\":\"x\"}" value) "payload is the string after 'data: '")
  (done "data: [DONE]" :done "[DONE] yields :done" (null value) "value is NIL for :done")
  (empty "" :empty "blank line yields :empty" (null value) "value is NIL for :empty")
  (comment ": keep-alive" :comment ": prefix yields :comment" (stringp value) "comment carries its text"))

;;; --- B1: SSE stream helpers ---------------------------------------------

(defun json-line (table)
  "TABLE as one JSON object on a single line — the shape every canned SSE
frame below is a data: line of, and the only place this file pins
*PRINT-PRETTY* to keep a frame from arriving wrapped."
  (with-output-to-string (out)
    (let ((*print-pretty* nil))
      (shasht:write-json table out))))

(defun make-sse-stream (&rest data-strings &aux (buf (make-string-output-stream)))
  "Return a string-input-stream of canned SSE lines from DATA-STRINGS.
Each string becomes one data: line followed by a blank separator line.
A [DONE] sentinel is appended at the end."
  (dolist (s data-strings)
    (format buf "data: ~a~%" s)
    (format buf "~%"))
  (format buf "data: [DONE]~%")
  (make-string-input-stream (get-output-stream-string buf)))

(defun chunk-json (content &key finish-reason)
  "Build a minimal streaming chunk JSON string with the given delta.content."
  (json-line (nlk:json-object
              "choices" (vector (nlk:json-object
                                 "delta" (nlk:json-object
                                          :opt "content" content)
                                 :opt "finish_reason" finish-reason)))))

(defun tool-call-chunk-json (idx id fn-name fn-args)
  "Build a streaming chunk carrying one tool_call delta at IDX."
  (json-line (nlk:json-object
              "choices"
              (vector (nlk:json-object
                       "finish_reason" "tool_calls"
                       "delta" (nlk:json-object
                                "tool_calls"
                                (vector (nlk:json-object
                                         "index" idx
                                         "id" id
                                         "type" "function"
                                         "function" (nlk:json-object
                                                     "name" fn-name
                                                     "arguments" fn-args)))))))))

(defun reasoning-chunk-json (reasoning)
  "Build a streaming chunk carrying one delta.reasoning_content fragment."
  (json-line (nlk:json-object
              "choices" (vector (nlk:json-object
                                 "delta" (nlk:json-object
                                          "reasoning_content" reasoning))))))

;;; --- B1: mock the streaming path by patching dex:post ------------------
;;; The test goal is to drive call-provider-streaming's read loop against
;;; deterministic input without making a real HTTP request.
;;; Strategy: rebind dex:post to return the already-open mock stream with
;;; status 200, so the real read loop runs against it.  The lane's
;;; STREAM-SYMBOL is its only entry, so call-provider always lands here.
;;;
;;; Note: call-provider-streaming uses dex:post (not dex:fetch) because
;;; dex:fetch requires a destination argument and is a download helper, not
;;; a streaming POST primitive.

(defmacro with-mock-sse-stream (frame &body body &aux (mock (gensym "MOCK-STREAM")))
  "Run BODY with *STREAM* T and dex:post answering a canned SSE stream, so
CALL-PROVIDER-STREAMING's read loop runs against deterministic bytes instead
of a real HTTP request.

FRAME owns the framing, which is why no call site names a stream constructor:

  ((chunk-json \"hi\") usage)   a complete stream — the forms are its data: lines
  (:truncated (chunk-json \"h\")) the same lines, ending without [DONE]
  (:stream OBJECT)             an already-built stream, for the sites that
                               hold onto it to assert it was closed"
  `(let ((,mock ,(case (first frame)
                   (:stream (second frame))
                   (:truncated `(make-truncated-sse-stream ,@(rest frame)))
                   (t `(make-sse-stream ,@frame)))))
     (with-stubbed-fdefinition (dex:post (&rest args) (values ,mock 200))
       ,@body)))

(defmacro streamed-message (&rest lines)
  "Every value CALL-PROVIDER-STREAMING answers for USER-CONTEXT over a complete
canned stream of LINES — the message, the usage, the finish reason."
  `(with-mock-sse-stream (,@lines)
     (nle::call-provider-streaming (user-context))))

;;; --- B1 gate: text parts observed in order --------------------------------

(deftest streaming-text-parts-observed-in-order (let ((received '())))
  (with-mock-sse-stream ((chunk-json "Hello") (chunk-json ", ")
                         (chunk-json "world"))
    (nle::call-provider-streaming
     (user-context "hi")
     :on-part (lambda (part) (when (eq :text-delta part.kind) (push part.text received)))))
  (is (equal '("Hello" ", " "world") (nreverse received))))

;;; --- B1 gate: fully assembled content -----------------------------------

(deftest streaming-assembles-full-content (multiple-value-bind (message usage finish-reason))
  (streamed-message (chunk-json "alpha")
                    (chunk-json "beta")
                    (chunk-json "gamma" :finish-reason "stop"))
(declare (ignore usage))
(is (equal "alphabetagamma" (gethash "content" message)))
(is (equal "assistant" (gethash "role" message)))
(is (equal "stop" finish-reason))
(is (null (nth-value 1 (gethash "finish_reason" message)))))

;;; --- B1 gate: works without delta-fn ------------------------------------

(deftest streaming-no-delta-fn-still-returns-message ()
  (let ((message (streamed-message (chunk-json "ok"))))
    (is (equal "ok" (gethash "content" message)))))

;;; --- B1 gate: tool call reconstructed -----------------------------------

(deftest streaming-tool-call-reconstructed ()
  (let* ((message (streamed-message (tool-call-chunk-json 0 "call-99" "eval"
                                                          "{\"form\": \"(+ 1 2)\"}")))
         (calls (gethash "tool_calls" message)))
    (is (and (vectorp calls) (= 1 (length calls))))
    (let* ((call (aref calls 0))
           (fn   (gethash "function" call)))
      (is (equal "call-99" (gethash "id" call)))
      (is (equal "eval" (gethash "name" fn)))
      (is (search "form" (or (gethash "arguments" fn) ""))))))

;;; --- streamed tool arguments: bridge seed/snapshot supersession -----------
;;; OpenAI-compatible bridges over Anthropic models serialize the tool_use
;;; block's empty input as a leading "{}" arguments chunk before the real
;;; JSON. Blind concatenation yields {}{"form":...}, which decodes as an
;;; empty object and loses the call's input.

(deftest tool-args-accumulate-supersedes-a-closed-value ()
  (dolist (case '(("{}" "{\"form\": \"(f)\"}" "{\"form\": \"(f)\"}"
                   "a {} seed is replaced by the next value, not prefixed to it")
                  ("{}" "{}" "{}" "a doubled seed collapses to one")
                  ("{\"a\":1}" "{\"a\":2}" "{\"a\":2}"
                   "a complete snapshot is superseded by the next snapshot")))
    (destructuring-bind (buffer delta expected why) case
      (is (equal expected (nle::accumulate-tool-arguments buffer delta)) why))))

(deftest tool-args-accumulate-appends-incremental-deltas ()
  (dolist (case '(("" "{\"fo" "{\"fo" "the first fragment appends to the empty buffer")
                  ("{\"fo" "rm\":1}" "{\"form\":1}" "an open value keeps appending")
                  ("{}" " " "{} " "a non-value fragment after a closed value still appends")
                  ("{\"a\":\"{" "\"}" "{\"a\":\"{\"}" "a brace inside a string does not close the value")))
    (destructuring-bind (buffer delta expected why) case
      (is (equal expected (nle::accumulate-tool-arguments buffer delta)) why))))

(deftest streaming-tool-args-bridge-seed-superseded ()
  (let* ((seed (tool-call-chunk-json 0 "call-7" "eval" "{}"))
         ;; The follow-up delta carries arguments only — a repeated name
         ;; would itself concatenate, which real streams never do.
         (args-only
           (json-line (nlk:json-object
                       "choices"
                       (vector (nlk:json-object
                                "finish_reason" "tool_calls"
                                "delta"
                                (nlk:json-object
                                 "tool_calls"
                                 (vector (nlk:json-object
                                          "index" 0
                                          "function"
                                          (nlk:json-object
                                           "arguments"
                                           "{\"form\": \"(+ 1 2)\"}"))))))))))
    (let* ((message (streamed-message seed args-only))
           (call (aref (gethash "tool_calls" message) 0))
           (fn (gethash "function" call)))
      (is (equal "{\"form\": \"(+ 1 2)\"}" (gethash "arguments" fn))))))

;;; --- thinking mode: reasoning_content round-trip -------------------------
;;; The provider 400 this locks out: "The `reasoning_content` in the thinking
;;; mode must be passed back to the API." Streamed reasoning must survive the
;;; message reconstruction, and every replayed assistant message must carry
;;; the field when the model's thinking mode demands it.

(deftest streaming-preserves-reasoning-content ()
  (let ((message (streamed-message (reasoning-chunk-json "thinking ")
                                   (reasoning-chunk-json "hard")
                                   (chunk-json "answer" :finish-reason "stop"))))
    (is (equal "thinking hard" (gethash "reasoning_content" message)))
    (is (equal "answer" (gethash "content" message)))))

(deftest streaming-without-reasoning-adds-no-reasoning-key ()
  (let ((message (streamed-message (chunk-json "plain"))))
    (is (null (nth-value 1 (gethash "reasoning_content" message))))))

(deftest request-body-backfills-reasoning-for-thinking-models ()
  ;; Thinking mode is endpoint configuration, not an in-band conversation
  ;; fact: a deepseek-family model must see reasoning_content on every
  ;; replayed assistant message, including hydrated tool rounds that never
  ;; carried one (the proven Zig rule).
  (let* ((nle::*model* "deepseek-v4-flash-free")
         (with-reasoning (nle::message "assistant" "seen"))
         (bare (nle::message "assistant" "bare")))
    (setf (gethash "reasoning_content" with-reasoning) "already thought")
    (let* ((body (nle::request-body
                  (compiled-context
                   (list (nle::message "user" "hi")
                         with-reasoning bare))))
           (wire (gethash "messages" body)))
      (is (equal "already thought"
                 (gethash "reasoning_content" (aref wire 2))))
      (is (equal "" (gethash "reasoning_content" (aref wire 3))))
      (is (null (nth-value 1 (gethash "reasoning_content" (aref wire 0)))))
      (is (null (nth-value 1 (gethash "reasoning_content" (aref wire 1))))))))

(deftest chat-request-projects-away-other-lane-reasoning-state ()
  ;; A Responses/Google/Anthropic answer may be replayed after failover to the
  ;; chat lane. Only the chat lane's fields may cross its request boundary.
  (let ((assistant (nle::message "assistant" "answer")))
    (setf (gethash "reasoning_content" assistant) "thinking"
          (gethash "reasoning_items" assistant)
          (vector (nle::json-object "id" "rs-1" "encrypted_content" "opaque"))
          (gethash "thought_signatures" assistant)
          (nle::json-object "call-1" "sig-google")
          (gethash "reasoning_blocks" assistant)
          (vector (nle::json-object "thinking" "thinking" "signature" "sig-anthropic")))
    (let* ((body (nle::request-body
                  (compiled-context
                   (list (nle::message "user" "hi") assistant))))
           (wire (aref (gethash "messages" body) 2)))
      (is (equal "answer" (gethash "content" wire)))
      (is (equal "thinking" (gethash "reasoning_content" wire)))
      (dolist (key '("reasoning_items" "thought_signatures" "reasoning_blocks"))
        (is (null (nth-value 1 (gethash key wire)))
            (format nil "chat wire drops ~a" key)))
      (is (not (eq assistant wire)) "projection never reuses history"))))

(deftest responses-body-gives-every-thinking-assistant-message-reasoning ()
  ;; Without this, a prefilled (eval-status N) call has no reasoning item,
  ;; and the endpoint thinks whatever the request asks; every exit wake on
  ;; deepseek-flash would die 400. A round with a replayable item stays that
  ;; item; one without carries its reasoning_text, " " when it has none
  ;; (DeepSeek refuses ""); another family is as it was.
  (flet ((input (model)
           (let ((replayed (nle::message "assistant" "seen"))
                 (thought (nle::message "assistant" "chat lane"))
                 (wake (nle::message "assistant" :null
                                     :tool-calls (vector (wire-tool-call "c1" "eval" "{}")))))
             (setf (gethash "reasoning_items" replayed)
                   (vector (nle::json-object "id" "rs_1" "encrypted_content" "handle"))
                   (gethash "reasoning_content" thought) "thought on chat")
             (map 'list (lambda (item)
                          (or (gethash "type" item) (gethash "role" item)))
                  (gethash "input" (nle::responses-request-body
                                    (local-context
                                     (list (nle::message "user" "hi") replayed thought wake
                                           (nle::message "tool" "1" :name "eval"
                                                                    :tool-call-id "c1"))
                                     :provider-config (test-provider-config
                                                       :provider "openai-responses"
                                                       :model model))))))))
    (is (equal '("user" "reasoning" "assistant" "reasoning" "assistant"
                 "reasoning" "function_call" "function_call_output")
               (input "deepseek-flash")))
    (is (equal '("user" "reasoning" "assistant" "assistant"
                 "function_call" "function_call_output")
               (input "gpt-6")))))

(deftest responses-thinking-answer-a-wake-follows-replays-its-reasoning-as-text ()
  ;; The exit wake right behind the turn's answer would fail 400 "The
  ;; `reasoning_text` in the thinking mode must be passed back" -- the
  ;; endpoint takes an answer's encrypted handle only while a user message
  ;; follows it. An answer the wake's call follows, past a reminder, carries
  ;; its text as a reasoning_text part; one a user message follows keeps the
  ;; summary; another family is as it was.
  (flet ((answer-item (model &rest after)
           (let ((answer (nle::message "assistant" "done")))
             (setf (gethash "reasoning_items" answer)
                   (vector (nle::json-object "id" "rs_1" "text" "thought it through"
                                             "encrypted_content" "handle"))
                   (gethash "reasoning_content" answer) "thought it through")
             (find "reasoning"
                   (gethash "input" (nle::responses-request-body
                                     (local-context
                                      (list* (nle::message "user" "hi") answer after)
                                      :provider-config (test-provider-config
                                                        :provider "openai-responses"
                                                        :model model))))
                   :key (lambda (item) (gethash "type" item)) :test #'equal)))
         (text (item key)
           (gethash "text" (aref (gethash key item) 0))))
    (let* ((wake (list (nle::message "system" "a reminder")
                       (nle::message "assistant" :null
                                     :tool-calls (vector (wire-tool-call "c1" "eval" "{}")))
                       (nle::message "tool" "1" :name "eval" :tool-call-id "c1")))
           (continued (apply #'answer-item "deepseek-flash" wake))
           (closed (answer-item "deepseek-flash" (nle::message "user" "next")))
           (other (apply #'answer-item "gpt-6" wake)))
      (is (equal "rs_1" (gethash "id" continued)))
      (is (equal "handle" (gethash "encrypted_content" continued)))
      (is (equal "thought it through" (text continued "content")))
      (is (zerop (length (gethash "summary" continued))))
      (is (equal "thought it through" (text closed "summary")))
      (is (null (nth-value 1 (gethash "content" closed))))
      (is (equal "thought it through" (text other "summary")))
      (is (null (nth-value 1 (gethash "content" other)))))))

(deftest responses-thinking-reasoning-carries-the-text-it-has ()
  (let* ((thought (nle::message "assistant" :null
                                :tool-calls (vector (wire-tool-call "c1" "eval" "{}"))))
         (bare (nle::message "assistant" :null
                             :tool-calls (vector (wire-tool-call "c2" "eval" "{}")))))
    (setf (gethash "reasoning_content" thought) "thought on chat")
    (flet ((text (message)
             (let ((item (first (nle::responses-message-items message t))))
               (is (null (nth-value 1 (gethash "id" item))) "no item id to name")
               (gethash "text" (aref (gethash "content" item) 0)))))
      (is (equal "thought on chat" (text thought)))
      (is (equal " " (text bare))))))

(deftest request-body-stays-byte-identical-outside-thinking-mode ()
  (let* ((nle::*model* "unremarkable-chat-model")
         (body (nle::request-body
                (compiled-context
                 (list (nle::message "user" "hi")
                       (nle::message "assistant" "plain")))))
         (wire (gethash "messages" body)))
    (is (null (nth-value 1 (gethash "reasoning_content" (aref wire 2)))))))

;;; =========================================================================
;;; B3 gates: tool-input parts fire in-flight per SSE chunk
;;; =========================================================================

;;; --- B3 gate: tool-input parts fire per fragment with accumulated state --

(deftest streaming-tool-input-parts-fire-per-fragment (let ((calls '())))
  ;; A :tool-input-delta part must fire for each tool_call delta chunk carrying
  ;; the fully accumulated name and args so far, so the UI can render partial
  ;; tool calls in-flight rather than waiting for turn completion.
  (with-mock-sse-stream
      ;; Two chunks that together build up the full tool call: name arrives
      ;; whole in the first chunk; arguments grow across both chunks.
      ((tool-call-chunk-json 0 "call-b3" "eval" "{\"form\": \"(+ 1 ")
       (tool-call-chunk-json 0 "call-b3" "" "2)\"}"))
    (nle::call-provider-streaming
     (user-context)
     :on-part (lambda (part)
                (when (eq :tool-input-delta part.kind)
                  (push (list part.index part.tool-name part.arguments) calls)))))
  ;; CALLS is newest-first due to PUSH; reverse for chronological order.
  (let ((ordered (nreverse calls)))
    (is (= 2 (length ordered)))
    ;; After the first chunk the name is fully present; args are partial.
    (let ((first-call (first ordered)))
      (is (= 0 (first first-call)))
      (is (equal "eval" (second first-call)))
      (is (search "(+ 1 " (third first-call))))
    ;; After the second chunk args are complete; name unchanged.
    (let ((last-call (second ordered)))
      (is (equal "eval" (second last-call)))
      (is (search "(+ 1 2)" (third last-call))))))

;;; --- B3 gate: :on-part kwarg takes precedence over the dynamic var -------

(deftest streaming-on-part-kwarg-takes-precedence (let ((kwarg-calls '())
                                                        (dynvar-calls '())))
  ;; When both the :on-part kwarg and *turn-part-fn* are set, only the kwarg
  ;; fires: the explicit argument is the injection point.
  (with-mock-sse-stream ((tool-call-chunk-json 0 "c1" "eval" "{}"))
    (let ((nle::*turn-part-fn*
           (lambda (part) (declare (ignore part)) (push :dynvar dynvar-calls))))
      (nle::call-provider-streaming
       (user-context)
       :on-part (lambda (part)
                  (declare (ignore part))
                  (push :kwarg kwarg-calls)))))
  (is (plusp (length kwarg-calls)) "kwarg observer fired")
  (is (null dynvar-calls) "dynamic-var observer was not called when kwarg present"))

;;; =========================================================================
;;; F0 gates: the stream-part waist
;;; =========================================================================

(defun collect-part-tags (context &aux (tags '()))
  "Run the streaming fold over the current mock stream and return the ordered
list of part kinds (the lisp analogue of the Zig part-tag goldens)."
  (nle::call-provider-streaming context :on-part (lambda (part) (push part.kind tags)))
  (nreverse tags))

(deftest streaming-part-tag-sequence-is-golden ()
  (with-mock-sse-stream ((reasoning-chunk-json "hmm")
                         (chunk-json "hi")
                         (tool-call-chunk-json 0 "c1" "eval" "{\"form\": \"1\"}"))
    (is (equal '(:stream-start :response-metadata
                 :reasoning-start :reasoning-delta
                 :reasoning-end :text-start :text-delta
                 :tool-input-start :tool-input-delta
                 :tool-input-end :tool-call :text-end :finish)
               (collect-part-tags (user-context))))))

(deftest streaming-reasoning-parts-carry-text (let ((received '())))
  (with-mock-sse-stream ((reasoning-chunk-json "deep ")
                         (reasoning-chunk-json "thought")
                         (chunk-json "answer"))
    (nle::call-provider-streaming
     (user-context)
     :on-part (lambda (part) (when (eq :reasoning-delta part.kind) (push part.text received)))))
  (is (equal '("deep " "thought") (nreverse received))))

;;; =========================================================================
;;; F1 gates: stream integrity
;;; =========================================================================

(defun usage-chunk-json (&key prompt completion total cached reasoning
                              cache-write write-5m write-1h input output)
  "The terminal usage chunk: empty choices, usage object present, and every
field of the chat usage dialect expressible from one place.

Every key is optional and NIL means absent, because absent and zero are
different reports: a relay that serves one spelling and zeroes the other is
the live shape three of the gates below exist to pin, and a fixture that
defaulted its numbers could not state it."
  (json-line
   (nlk:json-object
    "choices" #()
    "usage" (nlk:json-object
             :opt "prompt_tokens" prompt
             :opt "completion_tokens" completion
             :opt "total_tokens" total
             :opt "prompt_tokens_details"
             (and cached (nlk:json-object "cached_tokens" cached))
             :opt "completion_tokens_details"
             (and reasoning (nlk:json-object "reasoning_tokens" reasoning))
             :opt "cache_creation_input_tokens" cache-write
             :opt "cache_creation"
             (and (or write-5m write-1h)
                  (nlk:json-object "ephemeral_5m_input_tokens" write-5m
                                   "ephemeral_1h_input_tokens" write-1h))
             :opt "input_tokens" input
             :opt "output_tokens" output))))

(deftest streaming-captures-usage-with-disjoint-input ()
  (multiple-value-bind (message usage finish-reason)
      (streamed-message (chunk-json "ok" :finish-reason "stop")
                        (usage-chunk-json :prompt 100 :completion 20
                                          :total 120 :cached 30))
    (declare (ignore message))
    (is (equal "stop" finish-reason) "finish reason survives the usage chunk")
    (is (= 70 usage.input-tokens))
    (is (= 30 usage.cached-input-tokens))
    (is (= 20 usage.output-tokens) "output tokens carried")
    (is (= 120 usage.total-tokens) "total carried")))

(deftest streaming-usage-absent-stays-nil-not-zero (multiple-value-bind (message usage))
  (streamed-message (chunk-json "ok"))
(declare (ignore message))
(is (null usage) "a provider that reported nothing yields NIL, never zeros"))

(deftest streaming-request-asks-for-usage (let ((captured nil)))
  (with-stubbed-fdefinition (dex:post (url &rest args)
                             (setf captured (getf args :content))
                             (values (make-sse-stream (chunk-json "ok")) 200))
    (nle::call-provider-streaming (user-context)))
  (let* ((body (nlk:decode-json captured))
         (options (gethash "stream_options" body)))
    (is (hash-table-p options) "stream_options is sent")
    (is (eq t (gethash "include_usage" options)) "usage is requested")
    (is (eq t (gethash "stream" body)) "stream stays requested")))

(deftest streaming-in-stream-error-frame-signals (with-mock-sse-stream)
  ((chunk-json "partial")
   "{\"error\": {\"message\": \"upstream fell over\", \"code\": 500}}")
(let ((condition (signals-error nle::provider-error
                   (nle::call-provider-streaming (user-context)))))
  (is condition "an in-stream error frame fails the attempt")
  (is (eql 500 (nle::provider-error-status condition)))
  (is (search "upstream fell over" (nle::provider-error-detail condition)))))

(defun make-truncated-sse-stream (&rest data-strings &aux (buf (make-string-output-stream)))
  "Canned SSE lines that end without the [DONE] sentinel."
  (dolist (s data-strings)
    (format buf "data: ~a~%~%" s))
  (make-string-input-stream (get-output-stream-string buf)))

(deftest streaming-truncation-signals-stream-incomplete ()
  (with-mock-sse-stream (:truncated (chunk-json "half an ans"))
    (is (signals-error nle::provider-stream-incomplete
          (nle::call-provider-streaming (user-context))))))

(deftest call-provider-retries-a-truncated-stream (let ((calls 0)
                                                        (tags '())
                                                        (nle::*retry-base-delay* 0)))
  (with-stubbed-fdefinition (dex:post (&rest args)
                             (incf calls)
                             (values (if (= calls 1)
                                         (make-truncated-sse-stream (chunk-json "lost"))
                                         (make-sse-stream (chunk-json "recovered")))
                                     200))
    (let ((message (nle::call-provider
                    (user-context)
                    :on-part (lambda (part)
                               (push (nle::stream-part-kind part) tags)))))
      (is (equal "recovered" (gethash "content" message)))))
  (is (= 2 calls) "truncation consumed exactly one retry")
  (let ((ordered (nreverse tags)))
    (is (equal '(:stream-start :response-metadata :text-start :text-delta
                 :stream-retry
                 :stream-start :response-metadata :text-start :text-delta
                 :text-end :finish)
               ordered))))

(deftest call-provider-retries-an-empty-response (let ((calls 0)
                                                       (nle::*retry-base-delay* 0)))
  ;; A clean stream that said nothing is a contract error, not history.
  (with-stubbed-fdefinition (dex:post (&rest args)
                             (incf calls)
                             (values (if (= calls 1)
                                         (make-sse-stream)
                                         (make-sse-stream (chunk-json "substance")))
                                     200))
    (let ((message (nle::call-provider (user-context))))
      (is (equal "substance" (gethash "content" message)))))
  (is (= 2 calls) "the empty response consumed exactly one retry"))

(deftest call-provider-retries-a-reasoning-only-final (let ((calls 0)
                                                            (nle::*retry-base-delay* 0)))
  ;; Without this, a reasoning-only final would be accepted as empty and the
  ;; turn would complete in silence; reasoning alone is not an answer, so the
  ;; retry ladder re-asks.
  (with-stubbed-fdefinition (dex:post (&rest args)
                             (incf calls)
                             (values (if (= calls 1)
                                         (make-sse-stream (reasoning-chunk-json "leaked tool call text"))
                                         (make-sse-stream (chunk-json "substance")))
                                     200))
    (let ((message (nle::call-provider (user-context))))
      (is (equal "substance" (gethash "content" message)))))
  (is (= 2 calls) "the reasoning-only final consumed exactly one retry"))

(deftest call-provider-never-resends-a-refusal (let ((calls 0)
                                                     (nle::*retry-base-delay* 0)))
  ;; A "refusal" stop_reason with no content block is read as an empty
  ;; response. Without this, the request would be sent again and refused
  ;; again.
  (with-stubbed-fdefinition (dex:post (&rest args)
                             (incf calls)
                             (values (make-sse-stream
                                      (anthropic-event "message_start" "message" (json-obj))
                                      (anthropic-event "message_delta"
                                                       "delta" (json-obj "stop_reason" "refusal"
                                                                         "stop_details" (json-obj "type" "refusal")))
                                      (anthropic-event "message_stop"))
                                     200))
    (is-carrying (text (princ-to-string (signals-error nle::provider-refused
                                          (nle::call-provider (anthropic-user-context)))))
      ("refused" "the failure says the provider refused") ("finish reason refusal" "and names the finish")))
  (is (= 1 calls) "sent once, never resent"))

(defclass paced-sse-stream (sb-gray:fundamental-character-input-stream)
  ((inner :initarg :inner)
   (pace :initarg :pace))
  (:documentation "A canned SSE body that sleeps PACE seconds before every
line: bytes keep arriving, but the whole body takes many PACEs of wall
time — the shape of a long generation from a slow model."))

(defmethod sb-gray:stream-read-line ((stream paced-sse-stream))
  (with-slots (inner pace) stream
    (sleep pace)
    (let ((line (read-line inner nil nil)))
      (values line (null line)))))

(defmethod sb-gray:stream-read-char ((stream paced-sse-stream))
  (or (read-char (slot-value stream 'inner) nil nil) :eof))

(deftest lively-stream-is-never-bounded-by-wall-time ()
  ;; The idle watchdog is the only clock a streaming attempt runs under: as
  ;; long as bytes keep arriving the attempt may take as long as the model
  ;; needs. Each line here arrives well inside the per-read timeout while
  ;; the body as a whole takes many times that timeout — a total ceiling
  ;; (the 600s clock that cut a 10-minute local generation and retried the
  ;; identical request into the same wall) would fail this stream.
  (let* ((chunks (loop for i below 25 collect (chunk-json (format nil "~d " i))))
         (body (apply #'make-sse-stream chunks))
         (nle::*request-timeout* 0.1))
    (with-mock-sse-stream (:stream (make-instance 'paced-sse-stream
                                                  :inner body :pace 0.02))
      (let ((message (nle::call-provider-streaming (user-context))))
        (is (= 25 (length (uiop:split-string
                           (string-right-trim " " (gethash "content" message))
                           :separator " "))))))))

;;; =========================================================================
;;; F2 gates: retry policy
;;; =========================================================================

(deftest retry-classification-matches-the-zig-set ()
  (flet ((retryable (status)
           (nle::retryable-provider-error-p
            (make-condition 'nle::provider-error :status status))))
    (is (retryable nil) "nil status (timeout, truncation) is retryable")
    (dolist (status '(408 409 425 429 500 599)) ; 599 stands for every 5xx
      (is (retryable status) (format nil "~d is retryable" status)))
    (dolist (status '(400 401 413 422))
      (is (not (retryable status)) (format nil "~d is not retryable" status)))))

(deftest retry-after-wins-over-backoff-and-is-capped ()
  (is (= 7 (nle::provider-retry-delay 1 1 :retry-after 7)))
  (is (= 60 (nle::provider-retry-delay 1 1 :retry-after 3600)))
  (let ((delay (nle::provider-retry-delay 3 1)))
    (is (and (<= 3.5 delay) (<= delay 4.5))))
  (let ((delay (nle::provider-retry-delay 10 1)))
    (is (< delay 9) "back-off is capped at 8 seconds plus jitter")))

(deftest service-unavailable-has-its-own-delay-floor ()
  ;; The one per-status override (retry_policy.zig service_unavailable):
  ;; a 503's first retry waits at least ~5 seconds however small the
  ;; configured base, and its cap rises to 60; a 429 rides the plain curve.
  (let ((delay (nle::provider-retry-delay 1 0 :status 503)))
    (is (and (<= 4.5 delay) (<= delay 5.5)) "503 floors the base at 5 seconds"))
  (let ((delay (nle::provider-retry-delay 5 1 :status 503)))
    (is (<= delay 66) "the 503 cap is 60 seconds, not 8"))
  (let ((delay (nle::provider-retry-delay 1 0 :status 429)))
    (is (< delay 1) "429 has no floor: the plain curve applies")))

(defmacro with-lane-failures ((calls-var plan &rest success-message-args)
                              &body body)
  "Stub the streaming lane with PLAN, a list of retry scopes; each call
consumes one entry and fails with that scope (NIL status, so retryable),
and the first call past the plan succeeds. CALLS-VAR counts lane calls."
  `(let ((,calls-var 0)
         (plan-left (copy-list ,plan))
         (nle::*retry-base-delay* 0))
     (with-streaming-provider-stub
         (context on-part
          (incf ,calls-var)
          (nlk:if-let (scope (pop plan-left))
            (error 'nle::provider-error
                   :scope scope
                   :detail (format nil "planned ~a failure" scope))
            (wire-message "assistant"
                          ,@(or success-message-args
                                '(:content "survived")))))
       ,@body)))

(deftest request-budget-is-per-stream-attempt-and-refills (let ((nle::*max-stream-retries* 5)
                                                                (nle::*max-request-retries* 4)))
  ;; The attempt.zig reset rule: a stream retry zeroes the request counter,
  ;; so four request failures fit before AND after a stream failure under
  ;; max-request-retries 4 — the budgets multiply, never add.
  (with-lane-failures (calls (list :request :request :request :request
                                   :stream
                                   :request :request :request :request))
    (let ((message (nle::call-provider
                    (user-context))))
      (is (equal "survived" (gethash "content" message))))
    (is (= 10 calls))))

(deftest request-budget-exhaustion-is-terminal (let ((nle::*max-stream-retries* 5)
                                                     (nle::*max-request-retries* 4)))
  ;; A fifth request-scope failure exceeds max-request-retries 4 within one
  ;; stream attempt, however much stream budget remains.
  (with-lane-failures (calls (list :request :request :request :request
                                   :request))
    (is (signals-error nle::provider-error
          (nle::call-provider (user-context))))
    (is (= 5 calls) "exactly four request retries ran")))

(deftest stream-budget-bounds-stream-failures (let ((nle::*max-stream-retries* 1)
                                                    (nle::*max-request-retries* 4)))
  (with-lane-failures (calls (list :stream :stream))
    (is (signals-error nle::provider-error
          (nle::call-provider (user-context))))
    (is (= 2 calls) "exactly one stream retry ran")))

(deftest provider-retry-is-a-durable-fact (with-session-store ("retry-fact"))
  (with-durable-turn (turn "retry-fact" :command-id "retry-fact-cmd" :input "go" :current t :retry-delay 0)
    (let* (;; A sentinel model proves the fact captures the failing request's
           ;; identity from the signal-time specials, not a stale global. The
           ;; lane stays ambient: sentinel lane names cannot resolve.
           (nle::*model* "retry-model-x")
           (calls 0))
      (with-streaming-provider-stub
          (context on-part
           (incf calls)
           (if (= calls 1)
               (error 'nle::provider-error
                      :status 502 :detail "brownout" :retry-after 0
                      :scope :request)
               (wire-message "assistant" :content "after retry")))
        (nle::call-provider (user-context)))
      (let ((retries (remove-if-not (lambda (event)
                                      (typep event 'nlk:provider-retry-event))
                                    (nle::collect-session-events "retry-fact"))))
        (is (= 1 (length retries)) "one retry produced one durable fact")
        (is (= 502 (nlk:provider-retry-event-status (first retries))))
        (is (= 1 (nlk:provider-retry-event-attempt (first retries))))
        (is (equal "provider_request"
                   (nlk:provider-retry-event-scope (first retries))))
        (is (= 1 (nlk:provider-retry-event-stream-attempt (first retries))))
        (is (= 0 (nlk:provider-retry-event-request-retries-used (first retries))))
        (is (equal "retry-model-x"
                   (nlk:provider-retry-event-model (first retries))))
        (is (equal nle::*provider*
                   (nlk:provider-retry-event-provider (first retries))))))))

(defun voided-attempt-facts (session-id)
  "The turn.attempt_voided facts of SESSION-ID, oldest first."
  (remove-if-not (lambda (event) (typep event 'nlk::attempt-voided-event))
                 (nle::collect-session-events session-id)))

(deftest stream-retry-seals-what-the-failed-attempt-streamed (with-session-store ("seal-retry"))
  ;; Without this, reasoning streamed before a cut and retry would be
  ;; lost. What a failed attempt streamed is sealed as a durable
  ;; turn.attempt_voided fact — reasoning and text whole — plus an
  ;; estimated, voided usage fact, BEFORE the retry fact and the
  ;; :stream-retry part that tells live consumers to clear the rest.
  (with-durable-turn (turn "seal-retry" :command-id "seal-retry-cmd" :input "go" :current t :retry-delay 0)
    (let* ((calls 0)
           (kinds '()))
      (with-streaming-provider-stub
          (context on-part
           (incf calls)
           (cond ((= calls 1)
                  (nle::emit-stream-part on-part :stream-start)
                  (nle::emit-stream-part on-part :reasoning-delta
                                         :text "twenty minutes of ")
                  (nle::emit-stream-part on-part :reasoning-delta
                                         :text "thought")
                  (nle::emit-stream-part on-part :text-delta
                                         :text "half an ans")
                  (error 'nle::provider-stream-incomplete
                         :detail "stream ended before the [DONE] sentinel"))
                 (t (wire-message "assistant" :content "recovered"))))
        (let ((message (nle::call-provider
                        (user-context)
                        :on-part (lambda (part)
                                   (push part.kind
                                         kinds)))))
          (is (equal "recovered" (gethash "content" message)))))
      (is (= 2 calls) "the cut consumed exactly one retry")
      (let ((voided (voided-attempt-facts "seal-retry")))
        (is (= 1 (length voided)) "one failed attempt sealed one fact")
        (when voided
          (let ((fact (first voided)))
            (is (equal "twenty minutes of thought"
                       (nlk::attempt-voided-event-reasoning fact)))
            (is (equal "half an ans" (nlk::attempt-voided-event-text fact)))
            (is (= 1 (nlk::attempt-voided-event-attempt fact)))
            (is (equal "rerun" (nlk::attempt-voided-event-disposition fact)))
            (is (search "[DONE]" (nlk::attempt-voided-event-detail fact)))
            (is (integerp (nlk::attempt-voided-event-stream-duration-ms fact))))))
      (let* ((events (nle::collect-session-events "seal-retry"))
             (voided-at (position-if (lambda (e)
                                       (typep e 'nlk::attempt-voided-event))
                                     events))
             (retry-at (position-if (lambda (e)
                                      (typep e 'nlk:provider-retry-event))
                                    events))
             (usages (remove-if-not (lambda (e)
                                      (typep e 'nlk:turn-usage-event))
                                    events)))
        (is (and voided-at retry-at (< voided-at retry-at)))
        (is (= 1 (length usages)) "the cut attempt's spend billed once")
        (when usages
          (is (nlk:turn-usage-event-voided (first usages)))
          (is (nlk:turn-usage-event-estimated (first usages)))
          (is (eql 7 (nlk:turn-usage-event-reasoning-tokens (first usages))))))
      (is (equal '(:stream-start :reasoning-delta :reasoning-delta :text-delta
                   :stream-retry)
                 (subseq (reverse kinds) 0 5))))))

;;; --- model failover: the chain walks before the failure stands ----------

(defun stub-effective-provider-config (provider model)
  "A synthetic effective-provider-config for failover tests. The lane is a
registered dialect and the endpoint is inert: every test here stubs the
lane function, so nothing dials."
  (nle::make-effective-provider-config
   :provider provider :lane "openai-completions" :endpoint "http://stub.invalid"
   :model model :api-key "stub"))

(defun provider-fallback-facts (session-id)
  "The turn.provider_fallback facts of SESSION-ID, oldest first."
  (remove-if-not (lambda (event) (typep event 'nlk:provider-fallback-event))
                 (nle::collect-session-events session-id)))

(deftest failover-chain-reads-the-shared-config-and-skips-half-entries ()
  (with-temp-file (path :contents "{\"failover\": {\"chain\": [{\"provider\": \"alt\", \"model\": \"m2\"}, {\"provider\": \"alt2\"}, {}]}}")
    (nlk:bind ((nle::*shared-config-path* path) (chain (nle::failover-chain)))
      (is (= 1 (length chain)) "only the complete entry is read")
      (is (equal "alt" (getf (first chain) :provider)))
      (is (equal "m2" (getf (first chain) :model)))))
  (with-temp-file (path)
    (let ((nle::*shared-config-path* path))
      (is (null (nle::failover-chain)) "an absent config is no chain"))))

(deftest a-candidate-freezes-its-own-endpoint-not-the-selections (with-temp-file)
  ;; Without this, the candidate's endpoint would be composed through the
  ;; AMBIENT selection, so a fallback would re-dial the dead primary instead
  ;; of the candidate's own base_url.
  (path :contents
        "{\"providers\": {\"alt\": {\"api_key\": \"k\", \"base_url\": \"https://alt.example/v1\", \"sdk\": \"openai-completions\"}}}")
(let* ((nle::*shared-config-path* path)
       (nle::*provider* "failproof")
       (nle::*model* "dead-model")
       (candidate (nle::failover-candidate-config "alt" "m2")))
  (is (equal "alt" candidate.provider))
  (is (equal "m2" candidate.model))
  (is (uiop:string-prefix-p "https://alt.example/v1" candidate.endpoint))
  (is (not (search "failproof"
                   candidate.endpoint)))))

(deftest a-spent-budget-falls-over-to-the-configured-chain (with-session-store ("failover-ok"))
  (with-durable-turn (turn "failover-ok" :command-id "failover-ok-cmd" :input "go" :current t :retry-delay 0)
    (let* ((nle::*max-stream-retries* 0)
           (nle::*max-request-retries* 0)
           (selection nle::*model*)
           (calls 0)
           (parts '()))
      (with-stubbed-fdefinitions
          ((nle::failover-chain () '((:provider "alt" :model "alt-model")))
           (nle::failover-candidate-config (provider model)
             (stub-effective-provider-config provider model))
           (nle::call-provider-streaming (context &key on-part)
             (incf calls)
             (if (equal "alt-model" (nle::effective-provider-config-model context.provider-config))
                 (wire-message "assistant" :content "answered on the candidate")
                 (progn
                   (nle::emit-stream-part on-part :reasoning-delta
                                          :text "six minutes of thinking")
                   (error 'nle::provider-error
                          :status 503 :scope :request
                          :detail "upstream overloaded")))))
        (multiple-value-bind (message usage finish metadata active)
            (nle::call-provider
             (user-context)
             :on-part (lambda (part)
                        (when (eq :stream-fallback part.kind) (push part.payload parts))))
          (declare (ignore usage finish metadata))
          (is (equal "answered on the candidate" (gethash "content" message)))
          (is (equal "alt-model" (nle::effective-provider-config-model active.provider-config)))
          (is (= 1 (length parts)))
          (when parts
            (is (equal "alt" (getf (first parts) :to-provider)))
            (is (equal "overloaded" (getf (first parts) :reason))))))
      (is (= 2 calls) "one failure, one candidate attempt: no retry was spent")
      (is (equal selection nle::*model*))
      (let ((voided (voided-attempt-facts "failover-ok")))
        (is (= 1 (length voided)))
        (when voided
          (is (equal "rerun" (nlk::attempt-voided-event-disposition
                               (first voided))))))
      (let ((facts (provider-fallback-facts "failover-ok")))
        (is (= 1 (length facts)) "the move is one durable fact")
        (when facts
          (let ((fact (first facts)))
            (is (equal "alt" (nlk:provider-fallback-event-to-provider fact)))
            (is (equal "alt-model" (nlk:provider-fallback-event-to-model fact)))
            (is (equal "overloaded" (nlk:provider-fallback-event-reason fact)))
            (is (= 503 (nlk:provider-fallback-event-status fact)))
            (is (= 1 (nlk:provider-fallback-event-attempt fact)))
            (is (search "upstream overloaded"
                        (nlk:provider-fallback-event-detail fact)))))))))

(deftest a-refusal-sends-that-round-alone-to-the-chain (with-session-store ("refusal-fallback"))
  ;; With a chain configured, the candidate answers a refused post-eviction
  ;; round, and the turn's next round is the selection's again.
  (with-durable-turn (turn "refusal-fallback" :command-id "refusal-fallback-cmd" :input "go" :current t :retry-delay 0)
    (let ((selection nle::*model*)
          (calls 0))
      (with-stubbed-fdefinitions
          ((nle::failover-chain () '((:provider "alt" :model "alt-model")))
           (nle::failover-candidate-config (provider model)
             (stub-effective-provider-config provider model))
           (nle::call-provider-streaming (context &key on-part)
             (declare (ignorable on-part))
             (incf calls)
             (if (equal "alt-model" (nle::effective-provider-config-model context.provider-config))
                 (wire-message "assistant" :content "answered on the candidate")
                 (error 'nle::provider-refused
                        :detail "the provider refused to answer (finish reason refusal)"))))
        (multiple-value-bind (message usage finish metadata active)
            (nle::call-provider (user-context))
          (declare (ignore usage finish))
          (is (equal "answered on the candidate" (gethash "content" message)))
          ;; the round is the candidate's: its usage and tools read the candidate
          (is (equal "alt-model" (nle::effective-provider-config-model active.provider-config)))
          ;; the next round starts on the selection again
          (is (equal selection (nle::effective-provider-config-model
                                (nle::compiled-turn-context-provider-config
                                 (nle::next-round-context active metadata)))))))
      (is (= 2 calls) "the refused request was not resent to the selection")
      (let ((facts (provider-fallback-facts "refusal-fallback")))
        (is (= 1 (length facts)) "the move is one durable fact")
        (when facts
          (is (equal "refused" (nlk:provider-fallback-event-reason (first facts)))))))))

(deftest a-candidate-after-a-streamed-refusal-times-its-own-first-token
    (with-session-store ("refusal-ttft"))
  ;; The selection streams, then refuses; the candidate waits before its first
  ;; token. The candidate's request fact is timed from its own request: a
  ;; cadence carried over from the refused attempt kept that attempt's first
  ;; token, and the wait came out negative (the qa collector refused the page).
  (with-durable-turn (turn "refusal-ttft" :command-id "refusal-ttft-cmd" :input "go" :current t :retry-delay 0)
    (with-stubbed-fdefinitions
        ((nle::failover-chain () '((:provider "alt" :model "alt-model")))
         (nle::failover-candidate-config (provider model)
           (stub-effective-provider-config provider model))
         (nle::call-provider-streaming (context &key on-part)
           (cond ((equal "alt-model" (nle::effective-provider-config-model context.provider-config))
                  (sleep 0.05)
                  (nle::emit-stream-part on-part :text-delta :text "answered")
                  ;; The request's octets, as a lane returns them: without
                  ;; them no request fact is recorded.
                  (values (wire-message "assistant" :content "answered") nil "stop"
                          (sb-ext:string-to-octets "{\"model\":\"alt-model\",\"messages\":[]}")))
                 (t
                  (nle::emit-stream-part on-part :reasoning-delta :text "thinking")
                  (sleep 0.2)
                  (error 'nle::provider-refused
                         :detail "the provider refused to answer (finish reason refusal)")))))
      (nle::call-provider (user-context))))
  (let ((fact (find-if (lambda (event) (typep event 'nlk:provider-request-event))
                       (nle::collect-session-events "refusal-ttft"))))
    (is-present (fact fact) "the candidate's request fact"
      (is (equal "alt-model" (nlk:provider-request-event-model fact)))
      (is (<= 50 (nlk:provider-request-event-ttft-ms fact) 190) "timed from the candidate's request, never from the refused attempt's first token")
      (is (= 1 (nlk:provider-request-event-stream-part-count fact)) "the candidate's parts alone"))))

(deftest failover-triggers-read-the-shared-config ()
  (with-temp-file (path :contents "{\"failover\": {\"on\": [\"refusal\", \"bogus\", \"rate_limit\"]}}")
    (let ((nle::*shared-config-path* path))
      (is (equal '("rate_limit" "refusal")
                 (handler-bind ((warning #'muffle-warning)) (nle::failover-triggers))) "the named triggers, in their own order, an unknown id skipped")))
  (with-temp-file (path :contents "{\"failover\": {\"on\": []}}")
    (let ((nle::*shared-config-path* path))
      (is (null (nle::failover-triggers)) "an empty list is every trigger off")))
  (with-temp-file (path)
    (let ((nle::*shared-config-path* path))
      (is (equal '("rate_limit" "outage" "slow_start" "broken_answer" "refusal") (nle::failover-triggers)) "none set is every trigger on"))))

(deftest a-failure-names-its-trigger-by-class-and-status ()
  (flet ((trigger (type &rest initargs) (nle::failover-trigger (apply #'make-condition type initargs))))
    (is (equal "refusal" (trigger 'nle::provider-refused :detail "refused")))
    (is (equal "rate_limit" (trigger 'nle::provider-error :status 429 :detail "slow down")))
    (is (equal "broken_answer" (trigger 'nle::provider-empty-response :detail "nothing")))
    (is (equal "slow_start" (trigger 'nle::provider-slow-start :detail "no first token in 60 s")))
    (is (equal "outage" (trigger 'nle::provider-error :status 503 :detail "overloaded")))
    (is (equal "outage" (trigger 'nle::provider-error :detail "no response for 60 s")) "a stall below HTTP is an outage")))

(deftest a-trigger-switched-off-keeps-the-failure-on-the-selection ()
  (let ((calls 0))
    (with-stubbed-fdefinitions
        ((nle::failover-chain () '((:provider "alt" :model "alt-model")))
         (nle::failover-triggers () '("rate_limit" "outage" "broken_answer"))
         (nle::failover-candidate-config (provider model)
           (stub-effective-provider-config provider model))
         (nle::call-provider-streaming (context &key on-part)
           (declare (ignorable on-part))
           (incf calls)
           (if (equal "alt-model" (nle::effective-provider-config-model context.provider-config))
               (wire-message "assistant" :content "answered on the candidate")
               (error 'nle::provider-refused
                      :detail "the provider refused to answer (finish reason refusal)"))))
      (signals-error nle::provider-refused (nle::call-provider (user-context))))
    (is (= 1 calls) "with refusal off, the chain is never asked")))

(deftest a-probe-asks-one-model-and-never-the-chain ()
  (let ((calls 0))
    (with-stubbed-fdefinitions
        ((nle::failover-chain () '((:provider "alt" :model "alt-model")))
         (nle::failover-candidate-config (provider model)
           (stub-effective-provider-config provider model))
         (nle::call-provider-streaming (context &key on-part)
           (declare (ignorable on-part))
           (incf calls)
           (if (equal "alt-model" (nle::effective-provider-config-model context.provider-config))
               (wire-message "assistant" :content "OK")
               (error 'nle::provider-error :status 503 :scope :request :detail "upstream overloaded"))))
      (multiple-value-bind (ok reason) (nle::probe-model "dead" "dead-model")
        (is (null ok) "the dead model's own verdict")
        (is (equal "overloaded" reason)))
      (is (= 1 calls) "no retry and no candidate")
      (is (eq t (nle::probe-model "alt" "alt-model")) "a model that answers is OK"))))

(defun refusal-hold-probes (verdicts rounds)
  "The rounds of a ROUNDS-round turn that asked the selection, under the
turn's refusal hold, when each ask answers the next of VERDICTS (T, a
refusal; NIL, an answer; past the list, an answer)."
  (let* ((selection (user-context))
         (candidate (nle::context-with selection :provider-config
                                       (stub-effective-provider-config "alt" "alt-model")))
         (hold (nle::make-refusal-hold))
         (context selection)
         (probes '()))
    (dotimes (round rounds (nreverse probes))
      (setf context
            (if (eq context selection)
                (progn (push round probes)
                       (if (pop verdicts)
                           ;; CALL-PROVIDER's answer for a refused round: the
                           ;; candidate's context, the selection to resume
                           (nle::next-round-context
                            candidate
                            (list :resume-provider-config
                                  (nle::compiled-turn-context-provider-config selection))
                            hold)
                           (nle::next-round-context selection nil hold)))
                (nle::next-round-context context nil hold)))
      (unless (member context (list selection candidate))
        ;; the resumed selection is a fresh context over the same config
        (setf context (if (eq (nle::compiled-turn-context-provider-config context)
                              (nle::compiled-turn-context-provider-config selection))
                          selection
                          candidate))))))

(deftest a-refused-selection-waits-on-its-candidate-doubling-to-the-cap ()
  ;; After the Nth refusal in a row the candidate answers 1, 2, 4 ... rounds
  ;; (at most *REFUSAL-HOLD-CAP*) before the selection is asked again; an
  ;; answer ends the streak.
  (is (equal '(0 2 5 10 11 13) (refusal-hold-probes '(t t t nil t nil) 14)))
  (let ((probes (refusal-hold-probes (make-list 40 :initial-element t) 40)))
    (is (equal '(0 2 5 10 19 28 37) probes) "the wait stops doubling at the cap")
    ;; proofs/RefusalHold.lean: refused asks <= 4 + rounds/9 at cap 8, and the
    ;; selection is asked at least once every cap+1 rounds
    (is (<= (length probes) (+ 4 (floor 40 9))))
    (is (every (lambda (a b) (<= (- b a) (1+ nle::*refusal-hold-cap*))) probes (rest probes))))
  (is (equal '(0 1 2 3 4 5) (refusal-hold-probes '() 6)) "no refusal, no wait")
  (let ((nle::*refusal-hold-cap* 0))
    (is (equal '(0 1 2 3 4 5) (refusal-hold-probes '(t t t t t t) 6)) "a cap of 0 asks the selection every round, as before the hold")))

(deftest a-failover-past-a-spent-budget-holds-for-the-turn ()
  ;; Only a refusal's round is the candidate's alone: a round the chain took
  ;; for a spent budget names no resume, and the next round stays on it.
  (let ((context (user-context)))
    (is (eq context (nle::next-round-context context '(:model "alt-model"))))))

(deftest a-slow-start-leaves-for-its-candidate-without-a-retry ()
  ;; The selection took the request and said nothing. The attempt is watched
  ;; with the candidate behind it, a skip cuts the wait, and the round answers
  ;; on the candidate: one fallback fact, slow_start, and no retry, with the
  ;; budgets at their defaults -- retried, the cold start is waited out again.
  (with-session-store ("failover-slow")
    (with-durable-turn (turn "failover-slow" :command-id "failover-slow-cmd" :input "go" :current t :retry-delay 0)
      (let ((turn-id (nlk:durable-turn-turn-id turn))
            (watched '()))
        (with-stubbed-fdefinitions
            ((nle::failover-chain () '((:provider "alt" :model "alt-model")))
             (nle::failover-candidate-config (provider model)
               (stub-effective-provider-config provider model))
             (nle::call-provider-streaming (context &key on-part)
               (declare (ignore on-part))
               (push (and nle::*attempt-watch* (nle::attempt-watch-next nle::*attempt-watch*)) watched)
               (if (equal "alt-model" (nle::effective-provider-config-model context.provider-config))
                   (wire-message "assistant" :content "answered on the candidate")
                   (progn
                     (is (equal '(:provider "alt" :model "alt-model") (nle::provider-attempt-next turn-id)) "the waiting attempt names its candidate")
                     (is (equal '(:provider "alt" :model "alt-model") (nle::skip-provider-attempt turn-id "skipped from the room")) "a skip answers where the round goes")
                     (is (null (nle::provider-attempt-next turn-id)) "a skipped wait offers no second skip")
                     (error (nle::attempt-cut-condition :scope :stream))))))
          (let ((message (nle::call-provider (user-context))))
            (is (equal "answered on the candidate" (gethash "content" message)))))
        (is (equal '((:provider "alt" :model "alt-model") nil) (reverse watched)) "the selection was watched; the candidate, with nothing behind it, was not")
        (is (null (nle::provider-attempt-next turn-id)) "nothing waits once the round is done")
        (let ((facts (provider-fallback-facts "failover-slow")))
          (is (= 1 (length facts)) "the move is one durable fact")
          (when facts
            (is (equal "slow_start" (nlk:provider-fallback-event-reason (first facts))))
            (is (equal "skipped from the room" (nlk:provider-fallback-event-detail (first facts))))))
        (is (null (remove-if-not (lambda (event) (typep event 'nlk:provider-retry-event))
                                 (nle::collect-session-events "failover-slow"))) "no retry was spent on the cold start")))))

(deftest an-attempt-with-nothing-behind-it-is-never-watched ()
  ;; No chain: no candidate, no watch, and no deadline -- the attempt waits as
  ;; long as its idle clock lets it, as before the watch existed.
  (with-session-store ("failover-none")
    (with-durable-turn (turn "failover-none" :command-id "failover-none-cmd" :input "go" :current t :retry-delay 0)
      (let ((seen :unset))
        (with-stubbed-fdefinitions
            ((nle::failover-chain () '())
             (nle::call-provider-streaming (context &key on-part)
               (declare (ignore context on-part))
               (setf seen nle::*attempt-watch*)
               (wire-message "assistant" :content "ok")))
          (nle::call-provider (user-context)))
        (is (null seen) "no watch without a candidate")
        (is (null (nle::provider-attempt-next (nlk:durable-turn-turn-id turn))) "and nothing to skip")))))

(deftest first-token-seconds-read-the-shared-config ()
  (flet ((seconds (json)
           (with-temp-file (path :contents json)
             (let ((nle::*shared-config-path* path) (nle::*shared-config-memo* nil))
               (nle::first-token-seconds)))))
    (is (= 60 (seconds "{}")) "60 unless set")
    (is (= 20 (seconds "{\"failover\": {\"first_token_seconds\": 20}}")))
    (is (= 0 (seconds "{\"failover\": {\"first_token_seconds\": 0}}")) "0 is never")
    (is (= 60 (seconds "{\"failover\": {\"first_token_seconds\": -5}}")) "a negative number is no setting")))

(deftest an-exhausted-chain-names-every-candidate-in-the-terminal-copy ()
  (with-session-store ("failover-dead")
    (with-durable-turn (turn "failover-dead" :command-id "failover-dead-cmd" :input "go" :current t :retry-delay 0)
      (let* ((nle::*max-stream-retries* 0)
             (nle::*max-request-retries* 0))
        (with-stubbed-fdefinitions
            ((nle::failover-chain ()
               '((:provider "alt-a" :model "ma")
                 (:provider "alt-b" :model "mb")))
             (nle::failover-candidate-config (provider model)
               (stub-effective-provider-config provider model))
             (nle::call-provider-streaming (context &key on-part)
               (declare (ignore context on-part))
               (error 'nle::provider-error
                      :status 503 :scope :request
                      :detail "{\"error\":{\"message\":\"every lane is down\"}}")))
          (let* ((context (user-context))
                 (who (format nil "~a/~a"
                              (nle::effective-provider-config-provider context.provider-config)
                              (nle::effective-provider-config-model context.provider-config))))
            (handler-case
                (progn (nle::call-provider context)
                       (is nil "the round cannot succeed"))
              (nle::provider-error (e)
                ;; Every model the round tried is a line of its own, the
                ;; selection first, each with the sentence its provider's JSON
                ;; body said.
                (is-carrying (detail (princ-to-string e))
                  ("every model failed" "the terminal copy says the chain was walked")
                  (is (search (format nil "~%1. ~a · HTTP 503: every lane is down" who) detail) "the selection's own failure opens the list")
                  (is (search (format nil "~%2. alt-a/ma · HTTP 503: every lane is down") detail) "then the first candidate's")
                  (is (search (format nil "~%3. alt-b/mb · HTTP 503: every lane is down") detail) "and the last one's")
                  (:absent "overloaded" "no reason label stands in for the provider's words"))))))
        (is (= 2 (length (provider-fallback-facts "failover-dead"))))))))

(deftest a-refused-selection-opens-an-exhausted-chains-copy ()
  ;; The selection refused (a stop_reason refusal, no status); the first
  ;; candidate's relay answered a 400 whose JSON message was its own Chinese
  ;; sentence, the second a 400 saying the model is unavailable. The copy that
  ;; stood named only the last model and reduced the first candidate to
  ;; `http_400': the refusal that sent the round down the chain was nowhere.
  (with-session-store ("failover-refused")
    (with-durable-turn (turn "failover-refused" :command-id "failover-refused-cmd" :input "go" :current t :retry-delay 0)
      (let* ((nle::*max-stream-retries* 0)
             (nle::*max-request-retries* 0)
             (context (user-context))
             (who (format nil "~a/~a"
                          (nle::effective-provider-config-provider context.provider-config)
                          (nle::effective-provider-config-model context.provider-config))))
        (with-stubbed-fdefinitions
            ((nle::failover-chain ()
               '((:provider "alt-a" :model "ma")
                 (:provider "alt-b" :model "mb")))
             (nle::failover-candidate-config (provider model)
               (stub-effective-provider-config provider model))
             (nle::call-provider-streaming (context &key on-part)
               (declare (ignore on-part))
               (let ((model (nle::effective-provider-config-model context.provider-config)))
                 (cond ((equal model "ma")
                        (error 'nle::provider-error :status 400 :scope :request
                                                    :detail "{\"error\":{\"message\":\"请求参数不正确，请检查请求体后重试。\",\"code\":\"invalid_request\"}}"))
                       ((equal model "mb")
                        (error 'nle::provider-error :status 400 :scope :request
                                                    :detail "{\"error\":{\"type\":\"server_error\",\"message\":\"Upstream request failed: Model is unavailable.\"}}"))
                       (t (error 'nle::provider-refused
                                 :detail "the provider refused to answer (finish reason refusal)"))))))
          (handler-case
              (progn (nle::call-provider context)
                     (is nil "the round cannot succeed"))
            (nle::provider-error (e)
              (is-carrying (detail (princ-to-string e))
                ("every model failed · /models picks another model" "one headline, then the models")
                (is (search (format nil "~%1. ~a: the provider refused to answer (finish reason refusal)" who) detail) "the selection's refusal is the first line")
                (is (search (format nil "~%2. alt-a/ma · HTTP 400: 请求参数不正确，请检查请求体后重试。") detail) "the relay's own sentence, verbatim")
                (is (search (format nil "~%3. alt-b/mb · HTTP 400: Upstream request failed: Model is unavailable.") detail) "the model the failure stood on")
                (:absent "http_400" "no label hides what a candidate answered")))))))))

(deftest a-candidates-refusal-walks-on-and-the-copy-names-who-refused ()
  ;; the primary's pool was empty (503), the first candidate was a
  ;; model the relay does not serve (400), and the turn failed there with the
  ;; second candidate untried, its copy saying the primary had answered 400.
  (with-session-store ("failover-400")
    (with-durable-turn (turn "failover-400" :command-id "failover-400-cmd" :input "go" :current t :retry-delay 0)
      (let* ((nle::*max-stream-retries* 0)
             (nle::*max-request-retries* 0)
             (selection nle::*model*)
             (answer-on "mb"))
        (with-stubbed-fdefinitions
            ((nle::failover-chain ()
               '((:provider "alt-a" :model "ma")
                 (:provider "alt-b" :model "mb")))
             (nle::failover-candidate-config (provider model)
               (stub-effective-provider-config provider model))
             (nle::call-provider-streaming (context &key on-part)
               (declare (ignore on-part))
               (let ((model (nle::effective-provider-config-model context.provider-config)))
                 (cond ((equal model answer-on)
                        (wire-message "assistant" :content "answered on the second candidate"))
                       ((equal model "ma")
                        (error 'nle::provider-error :status 400 :scope :request
                                                    :detail "no such model"))
                       (t (error 'nle::provider-error
                                 :status 503 :scope :request
                                 :detail "{\"error\":{\"message\":\"the pool is empty\"}}"))))))
          ;; A candidate's 400 walks the chain on.
          (is (equal "answered on the second candidate"
                     (gethash "content" (nle::call-provider (user-context)))))
          ;; Each move names why the model it left failed.
          (let ((facts (provider-fallback-facts "failover-400")))
            (is (equal '(("overloaded" "alt-a") ("http_400" "alt-b"))
                       (mapcar (lambda (fact)
                                 (list (nlk:provider-fallback-event-reason fact)
                                       (nlk:provider-fallback-event-to-provider fact)))
                               facts))))
          (setf answer-on nil)
          (handler-case
              (progn (nle::call-provider (user-context))
                     (is nil "the round cannot succeed"))
            (nle::provider-error (e)
              (is-carrying (detail (princ-to-string e))
                (is (eql 0 (search (format nil "every model failed~%1. ") detail)) "one headline, then the models")
                (is (search (format nil "/~a · HTTP 503: the pool is empty~%2. " selection) detail) "the selection's failure opens the list")
                (is (search (format nil "~%2. alt-a/ma · HTTP 400: no such model") detail) "each candidate with what its provider said")
                (is (search (format nil "~%3. alt-b/mb · HTTP 503: the pool is empty") detail) "down to the candidate the failure stood on"))))
          (is (equal selection nle::*model*) "the selection binding is untouched"))))))

(deftest a-refused-connection-spends-the-request-budget-at-once
    (let ((nle::*max-request-retries* 4) (nle::*retry-base-delay* 0) (calls 0) (parts '())))
  ;; Nothing listens at the address, and the next attempt hears the same:
  ;; the round stands after one call with four retries left, or the chain
  ;; takes it at once -- a stopped local server with a cloud model behind it.
  (flet ((refuse ()
           (incf calls)
           (error 'nle::provider-connection-refused
                  :detail "nothing listens at 127.0.0.1:1 — is the server running?")))
    (with-streaming-provider-stub (nil nil (refuse))
      (is-carrying (text (princ-to-string (signals-error nle::provider-connection-refused
                                            (nle::call-provider (user-context)))))
        ("nothing listens at 127.0.0.1:1 — is the server running?" "the final words name the address")
        ("provider request failed" "in the failure's own sentence")))
    (is (= 1 calls) "no retry against an address nothing listens on")
    (setf calls 0)
    (with-stubbed-fdefinitions
        ((nle::failover-chain () '((:provider "alt" :model "alt-model")))
         (nle::failover-candidate-config (provider model)
           (stub-effective-provider-config provider model))
         (nle::call-provider-streaming (context &key on-part)
           (if (equal "alt-model" (nle::effective-provider-config-model context.provider-config))
               (progn (incf calls) (wire-message "assistant" :content "answered on the candidate"))
               (refuse))))
      (let ((message (nle::call-provider
                      (user-context)
                      :on-part (lambda (part)
                                 (when (eq :stream-fallback part.kind) (push part.payload parts))))))
        (is (equal "answered on the candidate" (gethash "content" message)))))
    (is (= 2 calls) "one refusal, one candidate: no retry was spent")
    (is (equal '("connection_refused") (mapcar (lambda (payload) (getf payload :reason)) parts)) "the move names the refusal")))

(deftest an-unresolvable-candidate-is-recorded-and-skipped (with-session-store ("failover-bad"))
  (with-durable-turn (turn "failover-bad" :command-id "failover-bad-cmd" :input "go" :current t :retry-delay 0)
    (let* ((nle::*max-stream-retries* 0)
           (nle::*max-request-retries* 0))
      (with-stubbed-fdefinitions
          ((nle::failover-chain ()
             '((:provider "broken" :model "m0")
               (:provider "alt" :model "alt-model")))
           (nle::failover-candidate-config (provider model)
             (if (equal "broken" provider)
                 (error 'nle::provider-config-error
                        :detail "provider broken names no sdk")
                 (stub-effective-provider-config provider model)))
           (nle::call-provider-streaming (context &key on-part)
             (declare (ignore on-part))
             (if (equal "alt-model" (nle::effective-provider-config-model context.provider-config))
                 (wire-message "assistant" :content "the second candidate")
                 (error 'nle::provider-error
                        :status 503 :scope :request :detail "down"))))
        (let ((message (nle::call-provider (user-context))))
          (is (equal "the second candidate" (gethash "content" message)))))
      (let ((facts (provider-fallback-facts "failover-bad")))
        (is (= 2 (length facts)) "the refusal is a fact of its own")
        (when facts
          (is (equal "unresolved" (nlk:provider-fallback-event-reason (first facts))))
          (is (equal "alt" (nlk:provider-fallback-event-to-provider (second facts)))))))))

(deftest with-no-chain-a-spent-budget-stands (with-session-store ("failover-none"))
  (with-durable-turn (turn "failover-none" :command-id "failover-none-cmd" :input "go" :current t :retry-delay 0)
    (let* ((nle::*max-stream-retries* 0)
           (nle::*max-request-retries* 0))
      (with-stubbed-fdefinitions
          ((nle::failover-chain () nil)
           (nle::call-provider-streaming (context &key on-part)
             (declare (ignore context on-part))
             (error 'nle::provider-error
                    :status 503 :scope :request :detail "down")))
        (handler-case
            (progn (nle::call-provider (user-context))
                   (is nil "no chain means the failure stands"))
          (nle::provider-error (e)
            (is-carrying (detail (princ-to-string e))
              ("provider request failed [" "the one model's own copy")
              (:absent "every model failed" "no chain was walked")))))
      (is (null (provider-fallback-facts "failover-none"))))))


(defun reasoning-message (reasoning content)
  "A provider-shaped assistant message carrying a thinking trace."
  (nlk:json-object "role" "assistant" "content" content
                   "reasoning_content" reasoning))

(defun last-two-messages (context)
  (let ((messages (coerce context.messages 'list)))
    (last messages 2)))

(deftest rerun-ceiling-continues-a-long-stream-failure (with-session-store ("seal-continue"))
  ;; Past the rerun ceiling a cut attempt is continued, not re-run: the
  ;; next request carries the sealed output back as the assistant's own
  ;; cut-off reply plus one user nudge, the round's message is the seal
  ;; merged with what the continuation streamed, and the assistant fact
  ;; says where the join is so a transcript shows the prefix once.
  (let* ((nle::*stream-rerun-ceiling* 1/100)
         (turn (nlk:admit-turn "seal-continue" "seal-continue-cmd" "go"))
         (nle::*current-durable-turn* turn)
         (nle::*retry-base-delay* 0)
         (calls 0)
         (kinds '())
         (carried nil))
    (nlk:with-cleanup ((nlk:cancel-turn turn "test cleanup"))
      (with-streaming-provider-stub
          (context on-part
           (incf calls)
           (cond ((= calls 1)
                  (nle::emit-stream-part on-part :reasoning-delta
                                         :text "a long think")
                  (sleep 0.02)
                  (nle::emit-stream-part on-part :reasoning-delta :text "...")
                  (nle::emit-stream-part on-part :text-delta :text "Half")
                  (error 'nle::provider-stream-incomplete
                         :detail "stream ended before the [DONE] sentinel"))
                 (t
                  (setf carried (last-two-messages context))
                  (reasoning-message "and then more" " an answer."))))
        (multiple-value-bind (message usage finish meta)
            (nle::call-provider (user-context "question")
                                :on-part (lambda (part)
                                           (push part.kind
                                                 kinds)))
          (declare (ignore usage finish))
          (is (= 2 calls) "one continuation ran, no re-run")
          (is (equal (format nil "a long think...~%and then more")
                     (gethash "reasoning_content" message)))
          (is (equal "Half an answer." (gethash "content" message)))
          (is (equal '(:reasoning 16 :content 4) (getf meta :continued-from)))
          (is-present (assistant carried) "the continuation carried two messages"
            (let ((nudge (second carried)))
              (is (equal "assistant" (gethash "role" (first carried))))
              (is (equal "a long think..."
                         (gethash "reasoning_content" (first carried))))
              (is (equal "Half" (gethash "content" (first carried))))
              (is (equal "user" (gethash "role" nudge)))
              (is (search "cut off" (gethash "content" nudge)))
              (is (search "19 characters" (gethash "content" nudge)))))
          ;; The agent loop's recorder, with the offsets the fact
          ;; contract carries.
          (nlk:record-assistant-message turn message
                                        :continued-from
                                        (getf meta :continued-from)))))
    (let* ((events (nle::collect-session-events "seal-continue"))
           (voided (remove-if-not (lambda (e)
                                    (typep e 'nlk::attempt-voided-event))
                                  events))
           (assistant (find-if (lambda (e) (typep e 'nlk::assistant-message))
                               events)))
      (is (= 1 (length voided)) "the cut attempt sealed one fact")
      (when voided
        (is (equal "continued"
                   (nlk::attempt-voided-event-disposition (first voided)))))
      (is (= 1 (count-if (lambda (e) (typep e 'nlk:provider-retry-event))
                         events)))
      (is-present assistant "the merged round is the durable assistant fact"
        (is (= 16 (nlk::assistant-message-continued-reasoning-chars assistant)))
        (is (= 4 (nlk::assistant-message-continued-content-chars assistant)))))
    (is (equal '(:stream-retry) (remove-if-not (lambda (k) (eq k :stream-retry))
                                               kinds)))))

(deftest continuation-cut-again-continues-from-the-whole-seal ()
  ;; A continuation that is itself cut past the ceiling seals its own
  ;; output and the next continuation carries everything so far — the
  ;; cuts lose nothing and progress is monotone.
  (let* ((nle::*stream-rerun-ceiling* 1/100)
         (nle::*retry-base-delay* 0)
         (nle::*max-stream-retries* 5)
         (calls 0)
         (seen '()))
    (with-streaming-provider-stub
        (context on-part
         (incf calls)
         (push (gethash "reasoning_content" (first (last-two-messages context)))
               seen)
         (cond ((< calls 3)
                (nle::emit-stream-part on-part :reasoning-delta
                                       :text (format nil "part ~d" calls))
                (sleep 0.02)
                (nle::emit-stream-part on-part :reasoning-delta :text ".")
                (error 'nle::provider-stream-incomplete :detail "cut"))
               (t (reasoning-message "part 3." "done"))))
      (let ((message (nle::call-provider (user-context))))
        (is (equal (format nil "part 1.~%part 2.~%part 3.")
                   (gethash "reasoning_content" message)))
        (is (= 3 calls) "two continuations ran")
        (is (equal (list nil "part 1." (format nil "part 1.~%part 2."))
                   (reverse seen)))))))

(deftest continuation-budget-exhaustion-stands-with-the-seal (with-session-store ("seal-spent"))
  ;; The continuation spends the stream budget like any stream retry;
  ;; when it runs out the failure stands, the last attempt is sealed too,
  ;; and the detail says what survived.
  (let* ((nle::*stream-rerun-ceiling* 1/100)
         (nle::*max-stream-retries* 1)
         (turn (nlk:admit-turn "seal-spent" "seal-spent-cmd" "go"))
         (nle::*current-durable-turn* turn)
         (nle::*retry-base-delay* 0)
         (calls 0)
         (condition nil))
    (nlk:with-cleanup ((nlk:cancel-turn turn "test cleanup"))
      (with-streaming-provider-stub
          (context on-part
           (incf calls)
           (nle::emit-stream-part on-part :reasoning-delta
                                  :text (format nil "think ~d" calls))
           (sleep 0.02)
           (nle::emit-stream-part on-part :reasoning-delta :text "!")
           (error 'nle::provider-stream-incomplete :detail "cut"))
        (setf condition
              (signals-error nle::provider-stream-incomplete
                (nle::call-provider (user-context))))))
    (is (= 2 calls) "the budget allowed exactly one continuation")
    (when condition
      (is (search "cut (the stream budget is spent; what streamed across the attempts is kept)"
                  (princ-to-string condition))))
    (let ((voided (voided-attempt-facts "seal-spent")))
      (is (equal '("continued" "stood")
                 (mapcar #'nlk::attempt-voided-event-disposition voided)))
      (is (equal '("think 1!" "think 2!")
                 (mapcar #'nlk::attempt-voided-event-reasoning voided))))))

(deftest continuation-context-carries-reasoning-as-text-on-signed-lanes ()
  ;; Anthropic replays only signed thinking, so the seal rides as the
  ;; assistant's text there; the chat lane replays reasoning_content.
  (let ((anthropic (nle::continuation-context
                    (anthropic-user-context "q") "my thought" "my text"))
        (chat (nle::continuation-context (user-context "q") "my thought" "")))
    (let ((assistant (first (last-two-messages anthropic))))
      (is (equal (format nil "my thought~%~%my text")
                 (gethash "content" assistant)))
      (is (null (gethash "reasoning_content" assistant))))
    (let ((assistant (first (last-two-messages chat))))
      (is (equal "my thought" (gethash "reasoning_content" assistant)))
      (is (equal "" (gethash "content" assistant))))
    (is (= (+ 1 2) (length chat.messages)))))

(deftest rerun-ceiling-ignores-request-scope-failures (let* ((nle::*stream-rerun-ceiling* 0)
                                                             (nle::*retry-base-delay* 0)
                                                             (nle::*max-request-retries* 4)
                                                             (calls 0)))
  ;; Nothing streamed before a request-scope failure, so there is nothing
  ;; to re-run: the ceiling is a stream-scope rule only.
  (with-streaming-provider-stub
      (context on-part
       (incf calls)
       (if (= calls 1)
           (error 'nle::provider-error :status 502 :scope :request
                                       :detail "brownout")
           (wire-message "assistant" :content "after retry")))
    (is (equal "after retry"
               (gethash "content" (nle::call-provider (user-context))))))
  (is (= 2 calls) "one retry ran"))

(deftest provider-error-report-carries-provenance ()
  ;; The reported failure names its target: a session following a dead
  ;; default must say which (lane, model) 502'd, not just "status 502"
  ;; (the discord-vs-TUI ambiguity this provenance exists to kill).
  (let* ((nle::*provider* "lane-x")
         (nle::*model* "model-y")
         (condition (make-condition 'nle::provider-error
                                    :status 502 :detail "error code: 502")))
    (is (equal "lane-x" (nle::provider-error-provider condition)))
    (is (equal "model-y" (nle::provider-error-model condition)))
    (is (search "provider request failed [lane-x model-y] with status 502: error code: 502"
                (princ-to-string condition)))
    (is (search "[lane-x model-y]"
                (princ-to-string (make-condition 'nle::provider-stream-incomplete
                                                 :detail "stream cut"))))))

(deftest provider-error-report-says-the-providers-own-sentence ()
  ;; A mistyped key would print its whole 401 body as JSON, the first thing
  ;; a new operator reads.
  (is (equal "API key is invalid."
             (nle::provider-said "{\"type\":\"error\",\"error\":{\"type\":\"authentication_error\",\"message\":\"API key is invalid.\"},\"request_id\":null}")))
  (is (equal "high \"demand\"" (nle::provider-said "{\"error\": {\"code\": 503, \"message\": \"high \\\"demand\\\"\", \"stat")) "a body the cut left unparseable")
  ;; the cut inside the message: Google's quota 429 returns its JSON
  (is (equal "over quota. * limit: 20, mod…" (nle::provider-said "{\"error\": {\"code\": 429, \"message\": \"over quota.\\n* limit: 20, mod")))
  (is (equal "error code: 502" (nle::provider-said "error code: 502")))
  (let ((nle::*provider* "anthropic")
        (nle::*model* "m"))
    (is (search "with status 401: API key is invalid. · /models saves a new key"
                (princ-to-string (make-condition 'nle::provider-error :status 401
                                                 :detail "{\"error\":{\"message\":\"API key is invalid.\"}}"))))
    ;; Any other refusal is the model's.
    (is (search "with status 400: no signature · /models picks another model"
                (princ-to-string (make-condition 'nle::provider-error :status 400 :detail "no signature"))))
    ;; Google refuses a key it does not know with a 400, and that one is /models's (xa-202)
    (let ((body "{\"error\":{\"code\":400,\"message\":\"API key not valid. Please pass a valid API key.\",\"status\":\"INVALID_ARGUMENT\",\"details\":[{\"reason\":\"API_KEY_INVALID\"}]}}"))
      (is (search "with status 400: API key not valid. Please pass a valid API key. · /models saves a new key"
                  (princ-to-string (make-condition 'nle::provider-error :status 400 :detail body
                                                                        :evidence-body body)))))
    ;; a provider that failed is no one's to fix here: no remedy at all
    (is (not (search " · " (princ-to-string (make-condition 'nle::provider-error :status 503 :detail "busy")))))))

(deftest a-refused-key-is-one-rule-for-connect-and-the-import-check ()
  ;; The import's check (the kit's VERIFY-TARGET forwards this label) reads
  ;; a 403 as a refused key and Google's 400 API_KEY_INVALID as not, while
  ;; /models reads them the other way. Without this, an imported bad Gemini
  ;; key would read `not checked' and its row would never offer a new key.
  (flet ((reason (status &optional body)
           (nle::provider-failure-reason
            (make-condition 'nle::provider-error :status status :detail (or body "") :evidence-body body))))
    (is (equal "auth" (reason 401)))
    (is (equal "auth" (reason 400 "{\"error\":{\"code\":400,\"details\":[{\"reason\":\"API_KEY_INVALID\"}]}}")))
    (is (equal "http_400" (reason 400 "{\"error\":{\"message\":\"bad request\"}}")))
    ;; a key scoped away from a model is not a refused key
    (is (equal "forbidden" (reason 403)))))

(deftest turn-usage-is-a-durable-fact (with-session-store ("usage-fact"))
  (with-streaming-provider-stub
      (context on-part
       (values (wire-message "assistant" :content "counted")
               (nle::make-provider-usage
                :input-tokens 70 :output-tokens 20
                :cached-input-tokens 30 :total-tokens 120
                :reasoning-tokens 7 :cache-write-tokens 40
                :cache-write-5m-tokens 25
                :cache-write-1h-tokens 15)
               "stop"))
    (nle:run "count me" :session (nle:intern-session "usage-fact")
             :command-id "usage-fact-cmd"))
  (let ((facts (remove-if-not (lambda (event)
                                (typep event 'nlk:turn-usage-event))
                              (nle::collect-session-events "usage-fact"))))
    (is (= 1 (length facts)) "one provider round produced one usage fact")
    (let ((fact (first facts)))
      (is (= 70 (nlk:turn-usage-event-input-tokens fact)))
      (is (= 30 (nlk:turn-usage-event-cached-input-tokens fact)))
      (is (= 7 (nlk:turn-usage-event-reasoning-tokens fact)))
      (is (= 40 (nlk:turn-usage-event-cache-write-tokens fact)))
      (is (= 25 (nlk:turn-usage-event-cache-write-5m-tokens fact)))
      (is (= 15 (nlk:turn-usage-event-cache-write-1h-tokens fact)))
      (is (equal "stop" (nlk:turn-usage-event-finish-reason fact)))
      (is (plusp (length (nlk:turn-usage-event-model fact))))
      (is (plusp (length (nlk:turn-usage-event-provider fact))))))
  (let ((messages (wire-fold "usage-fact")))
    (is (notany (lambda (msg) (nth-value 1 (gethash "finish_reason" msg)))
                messages))))


(deftest turn-usage-records-the-identity-the-stream-answered-with ()
  ;; The finish divider must be able to say a relay served another model, and
  ;; which effort the round actually ran under, instead of echoing the ask
  ;; back at the operator as if it were evidence.
  (with-session-store ("usage-served")
    (let ((nle::*reasoning-effort* nil))
      (with-streaming-provider-stub
          (context on-part
           (nle::emit-stream-part on-part :response-metadata
                                  :id "resp_1"
                                  :model "gpt-5.6-sol-2026-08-01"
                                  :reasoning-effort "medium")
           (values (wire-message "assistant" :content "served")
                   (nle::make-provider-usage :input-tokens 5 :output-tokens 1)
                   "stop"))
        (nle:run "who answered"
                 :session (nle:intern-session "usage-served")
                 :command-id "usage-served-cmd"))
      (let ((fact (find-if (lambda (event)
                             (typep event 'nlk:turn-usage-event))
                           (nle::collect-session-events "usage-served"))))
        (is fact "the round left a usage fact")
        (is (equal "gpt-5.6-sol-2026-08-01"
                   (nlk:turn-usage-event-response-model fact)))
        (is (equal "medium" (nlk:turn-usage-event-reasoning-effort fact)))))))

;;; =========================================================================
;;; F3 gates: request controls and capability metadata
;;; =========================================================================

(deftest request-body-omits-unset-controls (let* ((nle::*temperature* nil)
                                                  (nle::*top-p* nil)
                                                  (nle::*max-completion-tokens* nil)
                                                  (nle::*tool-choice* nil)
                                                  (nle::*reasoning-effort* nil)
                                                  (body (nle::request-body
                                                         (user-context "hi")))))
  (dolist (key '("temperature" "top_p" "max_tokens" "tool_choice" "reasoning_effort"))
    (is (null (nth-value 1 (gethash key body)))
        (format nil "unset control ~a is an absent key, not a default" key))))

(deftest request-body-carries-configured-controls (let* ((nle::*temperature* 0.5)
                                                         (nle::*top-p* 0.9)
                                                         (nle::*max-completion-tokens* 256)
                                                         (nle::*tool-choice* "auto")
                                                         (nle::*reasoning-effort* "low")
                                                         (body (nle::request-body
                                                                (user-context "hi")))))
  (is (= 0.5 (gethash "temperature" body)) "temperature is sent")
  (is (= 0.9 (gethash "top_p" body)) "top_p is sent")
  (is (= 256 (gethash "max_tokens" body)) "max_tokens is sent")
  (is (equal "auto" (gethash "tool_choice" body)) "tool_choice is sent")
  (is (equal "low" (gethash "reasoning_effort" body)) "reasoning_effort is sent"))

(deftest controls-are-frozen-into-the-compiled-context ()
  (let* ((nle::*temperature* 0.2)
         (nle::*provider* "openai-completions")
         (context (user-context "hi"))
         (config context.provider-config))
    (setf nle::*temperature* 0.9
          nle::*provider* "anthropic")
    (is (= 0.2 config.temperature))
    (is (equal "openai-completions"
               config.provider))))

(deftest model-context-window-unknown-stays-nil (let ((nle::*model-capability-overrides*
                                                        '(("deepseek" . (:context-tokens 128000))))
                                                      (nle::*models-catalog-path* nil)))
  (is (= 128000 (nle::model-context-tokens "deepseek-v4-flash-free")))
  (is (null (nle::model-context-tokens "unknown-model"))))

(defparameter *catalog-fixture-json*
  "{\"acme\": {\"npm\": \"@ai-sdk/openai-compatible\",
                  \"api\": \"https://api.acme.dev/v1\",
                  \"models\": {
        \"acme-large\": {\"id\": \"acme-large\", \"reasoning\": true,
                          \"limit\": {\"context\": 200000, \"output\": 64000}},
        \"acme-mini\": {\"id\": \"acme-mini\", \"reasoning\": false,
                         \"limits\": {\"input\": 32000, \"output\": 8000}}}},
       \"exotic\": {\"npm\": \"@exotic/custom-sdk\", \"models\": {}}}"
  "A minimal models.dev-shaped api.json: two models under one provider
carrying the npm/api lane facts, plus a provider on an unmapped npm
package. What WITH-CATALOG-FIXTURE writes unless a test names its own.")

(defmacro with-catalog-fixture ((path-var &optional (json-text '*catalog-fixture-json*))
                                &body body)
  "Run BODY over a scratch models.dev api.json holding JSON-TEXT, with
*MODELS-CATALOG-PATH* naming it, PATH-VAR bound to it, and every catalog
and capability memo cleared so no earlier test's read can serve this one.

JSON-TEXT defaults to *CATALOG-FIXTURE-JSON*; a test whose claim is about
the catalog's own content — which namespace's ladder wins, what a
budget-only row declares — passes its own document instead."
  `(with-temp-file (,path-var :contents ,json-text)
     (let ((nle::*models-catalog-path* ,path-var)
           (nle::*models-catalog-memo* nil)
           (nle::*models-catalog-probe* nil)
           (nle::*model-capability-memo* nil)
           (nle::*model-capability-overrides* '())
           (nle::*context-tokens-env-override* nil))
       ,@body)))

;;; --- the catalog refresh ----------------------------------------------------
;;; Nothing refetched a cache that existed. These gates dial a loopback server,
;;; never models.dev.

(defparameter +crlf+ (coerce '(#\Return #\Linefeed) 'string))

(defun serve-catalog-once (body &key hold)
  "Listen on a loopback port and answer the first request with 200 and BODY,
then close. HOLD, a semaphore, is waited on before the answer, so a gate can
look at the fetch while it is still in flight. => (values URL THREAD)."
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                                        :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port listener))
         (thread
           (nlk:spawn "catalog-fixture-server"
             (nlk:with-cleanup ((usocket:socket-close listener))
               (let ((socket (usocket:socket-accept listener)))
                 (unwind-protect
                      (let ((stream (usocket:socket-stream socket))
                            (octets (sb-ext:string-to-octets body :external-format :utf-8)))
                        ;; the request head ends at CR LF CR LF
                        (loop with tail = 0
                              for octet = (read-byte stream nil)
                              while octet
                              do (setf tail (logand #xFFFFFFFF (logior (ash tail 8) octet)))
                              until (= tail #x0D0A0D0A))
                        (when hold (bt2:wait-on-semaphore hold))
                        (write-sequence
                         (sb-ext:string-to-octets
                          (format nil "HTTP/1.1 200 OK~a~
                                           Content-Type: application/json~a~
                                           Content-Length: ~d~a~
                                           Connection: close~a~a"
                                  +crlf+ +crlf+ (length octets) +crlf+ +crlf+ +crlf+)
                          :external-format :latin-1)
                         stream)
                        (write-sequence octets stream)
                        (finish-output stream))
                   (usocket:socket-close socket)))))))
    (values (format nil "http://127.0.0.1:~d/api.json" port) thread)))

(defun age-file (path seconds)
  "Set PATH's mtime SECONDS into the past."
  (let ((then (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0) seconds)))
    (sb-posix:utimes path then then)))

(defvar *memo-raced* nil
  "The memo READERS-THAT-MISS-A-JSON-MEMO-TOGETHER-PARSE-IT-ONCE races on: a
global, so the readers' threads share it.")

(deftest readers-that-miss-a-json-memo-together-parse-it-once ()
  ;; A boot's first readers miss at once — its target reads and the
  ;; picker warm on the catalog — and the file is parsed for all of them once.
  (with-temp-file (path :contents "{\"acme\": {}}")
    (setf *memo-raced* nil)
    (let* ((parses (list 0))
           (gate (bt2:make-semaphore))
           (decoder (lambda (octets)
                      (sb-ext:atomic-incf (car parses))
                      (sleep 0.1)
                      (nlk:decode-json octets)))
           (readers (loop repeat 3
                          collect (bt2:make-thread
                                   (lambda ()
                                     (bt2:wait-on-semaphore gate)
                                     (nle::read-json-file-memo '*memo-raced* path
                                                               :decoder decoder))))))
      (bt2:signal-semaphore gate :count 3)
      (is (every #'hash-table-p (mapcar #'bt2:join-thread readers)))
      (is (= 1 (car parses)) "one parse"))))

(deftest models-catalog-read-refetches-a-stale-cache-behind-itself (with-catalog-fixture (path))
  (let ((hold (bt2:make-semaphore)))
    (multiple-value-bind (url server) (serve-catalog-once "{\"fresh\": {\"models\": {}}}"
                                                          :hold hold)
      (let ((nle::*models-catalog-url* url)
            (nle::*models-catalog-fetch* nil)
            (nle::*models-catalog-probe-seconds* 0)
            ;; no retry window: only the fetch in flight and the TTL hold a read back
            (nle::*models-catalog-retry-seconds* 0))
        (age-file path (* 2 nle::*models-catalog-ttl-seconds*))
        (is (gethash "acme" (nle::models-catalog-table)))
        (let ((fetch (car nle::*models-catalog-fetch*)))
          (is (and fetch (bt2:thread-alive-p fetch)) "the stale read started a fetch")
          (nle::models-catalog-table)
          (is (eq fetch (nle::refresh-models-catalog)))
          (bt2:signal-semaphore hold)
          (is (null (bt2:join-thread fetch)) "the fetch landed")
          (bt2:join-thread server)
          (let ((root (nle::models-catalog-table)))
            (is (and (gethash "fresh" root) (null (gethash "acme" root)))))
          (is (eq fetch (car nle::*models-catalog-fetch*))))))))

(deftest models-catalog-fresh-or-unfetchable-reads-start-nothing ()
  (with-catalog-fixture (path)
    (let ((nle::*models-catalog-url* "http://127.0.0.1:1/api.json")
          (nle::*models-catalog-fetch* nil))
      (is (gethash "acme" (nle::models-catalog-table)))
      (is (null nle::*models-catalog-fetch*) "a cache inside its TTL is not refetched")))
  (with-temp-file (path)
    (let ((nle::*models-catalog-path* path)
          (nle::*models-catalog-memo* nil)
          (nle::*models-catalog-probe* nil)
          (nle::*models-catalog-fetch* nil))
      (is (null nle::*models-catalog-url*) "the hermetic posture fetches nothing")
      (is (null (nle::models-catalog-table)))
      (is (null (nle::refresh-models-catalog)))
      (is (null nle::*models-catalog-fetch*)))))

(deftest models-catalog-failed-refetch-keeps-the-old-cache-and-waits (with-catalog-fixture (path))
  (nlk:bind (((url server) (serve-catalog-once "<html>sign in to the wifi</html>"))
             (nle::*models-catalog-url* url) (nle::*models-catalog-fetch* nil)
             (nle::*models-catalog-probe-seconds* 0))
    (age-file path (* 2 nle::*models-catalog-ttl-seconds*))
    (nle::models-catalog-table)
    (let ((fetch (car nle::*models-catalog-fetch*)))
      (is (search "not a JSON object" (or (bt2:join-thread fetch) "")))
      (bt2:join-thread server)
      (is (gethash "acme" (nle::models-catalog-table)) "the old cache still serves")
      (is (eq fetch (car nle::*models-catalog-fetch*)))
      (let ((nle::*models-catalog-retry-seconds* 0))
        (nle::models-catalog-table)
        (let ((retry (car nle::*models-catalog-fetch*)))
          (is (not (eq fetch retry)) "past the window a stale read tries again")
          ;; running, it answers the failure before it; ended, its own (xm-204)
          (is (stringp (nle::models-catalog-fetch-failure)) "a retry never hides a failure")
          (is (stringp (bt2:join-thread retry)) "the closed server refuses it")
          (is (gethash "acme" (nle::models-catalog-table))))))))

(defparameter +awaited-catalog-json+
  "{\"anthropic\": {\"models\": {\"claude-await-test\": {\"reasoning\": true, \"reasoning_options\": [{\"type\": \"effort\", \"values\": [\"low\", \"medium\", \"high\", \"xhigh\", \"max\"]}]}}}}"
  "A catalog whose one model declares a ladder past high: adaptive thinking.")

(deftest a-turn-on-a-box-with-no-catalog-freezes-the-rows-its-first-fetch-lands ()
  ;; the ARC sandbox's cache is a tmpfs, so each game's first
  ;; turn froze claude-opus-5-5 before the fetch landed -- no ladder, budget
  ;; thinking, a 400. The freeze waits for the first fetch in flight; a retry
  ;; after a failed fetch holds no turn back.
  (flet ((frozen-ladder ()
           (nle::effective-provider-config-effort-levels
            (nle::snapshot-effective-provider-config "anthropic" "claude-await-test"))))
    (dolist (failed-before '(nil "models.dev unreachable"))
      (with-temp-file (path)
        (nlk:bind ((hold (bt2:make-semaphore))
                   ((url server) (serve-catalog-once +awaited-catalog-json+ :hold hold))
                   (nle::*models-catalog-path* path) (nle::*models-catalog-url* url)
                   (nle::*models-catalog-memo* nil) (nle::*models-catalog-probe* nil)
                   (nle::*models-catalog-probe-seconds* 0) (nle::*models-catalog-retry-seconds* 0)
                   (nle::*model-capability-memo* nil) (nle::*model-capability-overrides* '())
                   ;; a fetch already failed, or none has run
                   (nle::*models-catalog-fetch*
                     (and failed-before
                          (let ((thread (bt2:make-thread (lambda () failed-before))))
                            (bt2:join-thread thread)
                            (list* thread 0 nil)))))
          (if failed-before
              (progn (is (null (frozen-ladder)))
                     (bt2:signal-semaphore hold))
              (progn (bt2:make-thread (lambda () (sleep 0.2) (bt2:signal-semaphore hold)))
                     (is (equal '("low" "medium" "high" "xhigh" "max") (frozen-ladder)))))
          (ignore-errors (bt2:join-thread (car nle::*models-catalog-fetch*)))
          (bt2:join-thread server))))))

(deftest capability-resolves-from-the-models-dev-cache (with-catalog-fixture (path))
  (let ((cap (nle::resolve-model-capability "acme-large")))
    (is (= 200000 cap.context-tokens))
    (is (= 64000 cap.output-limit))
    (is (nle::model-capability-reasoning-p cap) "reasoning survives")
    (is (nle::model-capability-known-p cap) "the model is known"))
  (let ((cap (nle::resolve-model-capability "acme-mini")))
    (is (= 32000 cap.context-tokens))
    (is (= 8000 cap.output-limit))
    (is (not cap.reasoning-p) "reasoning false stays false"))
  (let ((cap (nle::resolve-model-capability "reseller/acme-large")))
    (is (= 200000 cap.context-tokens)))
  (let ((cap (nle::resolve-model-capability "nobody-knows-this")))
    (is (null cap.context-tokens))
    (is (null cap.output-limit))
    (is (not cap.known-p) "unknown is not no"))
  (let ((nle::*model-capability-overrides*
          '(("acme-large" . (:context-tokens 111)))))
    (let ((cap (nle::resolve-model-capability "acme-large")))
      (is (= 111 cap.context-tokens))
      (is (= 64000 cap.output-limit)))))

(deftest catalog-provider-resolves-lane-and-endpoint (with-catalog-fixture (path))
  ;; The /models picker lists auth-credentialed catalog providers (opencode
  ;; et al.) that have no config entry; selecting one must resolve a lane
  ;; and an endpoint from the catalog's own npm/api facts, or the gateway's
  ;; eager validation refuses a row its own picker offered.
  (is (equal "openai-completions"
             (nle::provider-lane-name (nle::find-provider-lane "acme"))))
  (let ((nle::*provider* "acme")
        (nle::*endpoint* nil))
    (is (equal "https://api.acme.dev/v1/chat/completions"
               (nle::effective-endpoint))))
  (signals-error nle::provider-error (nle::find-provider-lane "exotic"))
  (signals-error nle::provider-error (nle::find-provider-lane "nowhere")))

(deftest agent-max-output-tokens-shrinks-to-fit ()
  (is (null (nle::agent-max-output-tokens nil 100000 50)))
  (is (= 64000 (nle::agent-max-output-tokens 64000 nil 50)))
  (is (= 8000 (nle::agent-max-output-tokens 8000 100000 500)))
  (is (= (- 20000 12000 4096) (nle::agent-max-output-tokens 64000 20000 12000)))
  ;; No floor at 1. A cap of one token is not a smaller answer, it is a
  ;; request that cannot answer. Without this, a single forced token would
  ;; be returned on finish_reason=length and the loop would read that as
  ;; its final word. What is unusable is the caller's to refuse, so the
  ;; shrink says plainly that nothing is left.
  (is (zerop (nle::agent-max-output-tokens 64000 10000 9999)))
  (is (nth-value 1 (nle::agent-max-output-tokens 64000 10000 9999)))
  (is (not (nth-value 1 (nle::agent-max-output-tokens 8000 100000 500))))
  (is (not (nth-value 1 (nle::agent-max-output-tokens 64000 nil 50)))))

(defun image-message (base64-chars &optional (media-type "image/png"))
  "One user message carrying a single image content part whose data: URI
holds BASE64-CHARS characters of payload — the shape LOOK stages and
ATTACH-VISIBLE-IMAGES puts in front of the round."
  (nle::message "user"
                (vector (nle::make-image-content-part
                         (format nil "data:~a;base64,~a" media-type
                                 (make-string base64-chars :initial-element #\A))))))

(deftest an-attached-image-costs-an-image-not-the-length-of-its-base64 ()
  ;; An attached image costs an image token estimate, not the length of its
  ;; base64 characters. Reading the data URI as prompt text would count every
  ;; four base64 characters as a token and overcount the image.
  (let* ((payload 400000)                       ; a 300KB image, base64
         (plain (compiled-context (list (nle::message "user" "look"))))
         (with-image (compiled-context (list (nle::message "user" "look")
                                             (image-message payload))))
         (added (- (nle::compiled-context-estimated-tokens with-image)
                   (nle::compiled-context-estimated-tokens plain))))
    (is (< (abs (- added nle::+image-estimated-tokens+)) 64)
        (format nil "the image added ~:d tokens, not the ~:d an image costs"
                added nle::+image-estimated-tokens+))
    (is (< added (floor payload 4))
        (format nil "and not the ~:d its characters would" (floor payload 4)))))

(deftest a-length-finish-under-our-own-shrunk-cap-is-a-context-edge-cut ()
  ;; Without this, when the window math sets max_tokens low and the model
  ;; returns exactly that many tokens, the cut is read as the model
  ;; honouring our limit, so the recovery eviction never fires and the
  ;; empty round becomes the turn's answer.
  (let* ((tight (nle::context-with
                 (user-context (make-string 16000 :initial-element #\x))
                 :provider-config (nle::make-effective-provider-config
                                   :provider "p" :model "m" :api-key "k"
                                   :lane "openai-completions"
                                   :endpoint "http://stub.invalid"
                                   :context-tokens 8192
                                   :context-tokens-operator-p t
                                   :output-limit 65536)))
         (roomy (nle::context-with
                 (user-context "x")
                 :provider-config (nle::make-effective-provider-config
                                   :provider "p" :model "m" :api-key "k"
                                   :lane "openai-completions"
                                   :endpoint "http://stub.invalid"
                                   :context-tokens 200000
                                   :context-tokens-operator-p t
                                   :output-limit 65536))))
    (multiple-value-bind (cap shrunk-p) (nle::effective-max-output-tokens tight)
      (is (zerop cap) "the prompt fills the window, leaving nothing for output")
      (is shrunk-p "and it is our arithmetic that says so, not the model's limit"))
    (is (nle::context-edge-length-cut-p
         tight "length" (nle::make-provider-usage :output-tokens 1 :total-tokens 2)))
    (multiple-value-bind (cap shrunk-p) (nle::effective-max-output-tokens roomy)
      (is (= 65536 cap)) (is (not shrunk-p)))
    (is (not (nle::context-edge-length-cut-p
              roomy "length"
              (nle::make-provider-usage :output-tokens 65536 :total-tokens 65536))))))

(deftest a-round-with-no-room-to-answer-is-refused-before-dispatch ()
  ;; The second line of defence. Whatever made the prompt fill the window —
  ;; a mis-estimate, an attachment, history the governor could not evict
  ;; further — a request that leaves no room for output can only come back
  ;; on finish_reason=length with nothing in it, and the loop then has to
  ;; read that as the model's final word. Refusing costs no round trip, and
  ;; the turn fails saying what happened instead of completing on silence.
  (let ((context (nle::context-with
                  (user-context (make-string 40000 :initial-element #\x))
                  :provider-config (nle::make-effective-provider-config
                                    :provider "p" :model "m" :api-key "k"
                                    :lane "openai-completions"
                                    :endpoint "http://stub.invalid"
                                    :context-tokens 8192
                                    :context-tokens-operator-p t
                                    :output-limit 4096)))
        (calls 0))
    (with-stubbed-fdefinitions
        ((nle::call-provider-streaming (context &key on-part)
           (declare (ignore context on-part))
           (incf calls)
           (wire-message "assistant" :content "never reached")))
      (signals-error nle::provider-no-output-room (nle::call-provider context)))
    (is (zerop calls) "no round trip is spent on a request that cannot answer")))

(deftest chat-request-carries-capability-derived-max-tokens (with-catalog-fixture (path))
  (let* ((nle::*model* "acme-large")
         (nle::*max-completion-tokens* nil)
         (body (nle::request-body
                (user-context "hi"))))
    (is (= 64000 (gethash "max_tokens" body))))
  (let* ((nle::*model* "acme-large")
         (nle::*max-completion-tokens* 256)
         (body (nle::request-body
                (user-context "hi"))))
    (is (= 256 (gethash "max_tokens" body)))))

(deftest anthropic-max-tokens-falls-back-to-32k-and-clamps-thinking ()
  (nlk:bind ((nle::*models-catalog-path* nil) (nle::*max-completion-tokens* nil)
             (nle::*reasoning-effort* nil)
             (body (nle::anthropic-request-body
                    (anthropic-user-context))))
    (is (= 32000 (gethash "max_tokens" body))))
  (nlk:bind ((nle::*models-catalog-path* nil) (nle::*max-completion-tokens* 2048)
             (nle::*reasoning-effort* "low")
             (body (nle::anthropic-request-body
                    (anthropic-user-context))) (thinking (gethash "thinking" body)))
    (is (= 2048 (gethash "max_tokens" body)))
    (is (= 1024 (gethash "budget_tokens" thinking))))
  (nlk:bind ((nle::*models-catalog-path* nil) (nle::*max-completion-tokens* 1500)
             (nle::*reasoning-effort* "low") (nle::*temperature* 0.6)
             (body (nle::anthropic-request-body
                    (anthropic-user-context))))
    (is (null (gethash "thinking" body)))
    (is (= 0.6 (gethash "temperature" body)))))

(deftest gateway-model-op-mutates-and-reports ()
  (let ((nle::*gateway-operator-token* "token-1")
        (nle::*provider* "openai-completions")
        (nle::*model* "before-model")
        (nle::*endpoint* nil))
    (flet ((request (method body)
             (let ((headers (nlk:json-object "authorization" "Bearer token-1")))
               (list :request-method method
                     :request-uri "/api/gateway/model"
                     :headers headers
                     ;; The body-parse gate requires the declared type, as
                     ;; every real client sends it.
                     :content-type "application/json"
                     :content-length (length body)
                     :raw-body (make-string-input-stream body))))
           (response-json (response)
             (nlk:decode-json (third response))))
      (let ((get-response (nle::handle-gateway-model-http (request :get ""))))
        (is (= 200 (first get-response)) "GET reports the configured identity")
        (is (equal "before-model" (gethash "model" (response-json get-response)))))
      (let ((post-response
              (nle::handle-gateway-model-http
               (request :post "{\"model\": \"after-model\", \"provider\": \"anthropic\"}"))))
        (is (= 200 (first post-response)) "POST accepts a model change")
        (let ((json (response-json post-response)))
          (is (equal "after-model" (gethash "model" json)) "the new model is effective")
          (is (equal "anthropic" (gethash "provider" json)) "the new lane is effective")
          (is (search "anthropic.com" (gethash "endpoint" json)))))
      (is (equal "after-model" nle::*model*))
      (let ((bad (nle::handle-gateway-model-http
                  (request :post "{\"provider\": \"no-such-lane\"}"))))
        (is (= 400 (first bad)) "an unregistered lane is refused")))))

;;; =========================================================================
;;; F4 gates: the anthropic messages lane
;;; =========================================================================

(defun anthropic-context (messages &aux (nle::*provider* "anthropic")
                                        (nle::*endpoint* nil))
  (compiled-context messages))

(defun anthropic-user-context (&optional (text "x"))
  "USER-CONTEXT's Messages-lane twin: one user message, compiled against the
anthropic lane."
  (anthropic-context (list (nle::message "user" text))))

(deftest anthropic-request-hoists-system-and-converts-history ()
  (let* ((assistant (wire-message "assistant" :content "let me check"
                                  :tool-calls (vector (wire-tool-call
                                                       "call-7" "eval"
                                                       "{\"form\": \"(+ 1 2)\"}"))))
         (tool-reply (nle::message "tool" "3"
                                                     :name "eval"
                                                     :tool-call-id "call-7"))
         (body (nle::anthropic-request-body
                (anthropic-context
                 (list (nle::message "user" "add")
                       assistant tool-reply)))))
    (is (integerp (gethash "max_tokens" body)) "max_tokens is always present")
    (let ((system (gethash "system" body)))
      (is (vectorp system) "system is hoisted to the top level")
      (is (hash-table-p (gethash "cache_control" (aref system 0)))))
    (let ((messages (gethash "messages" body)))
      (is (= 3 (length messages)) "user, assistant, tool-result-as-user")
      (let ((assistant-blocks (gethash "content" (aref messages 1))))
        (is (equal "text" (gethash "type" (aref assistant-blocks 0))))
        (let ((tool-use (aref assistant-blocks 1)))
          (is (equal "tool_use" (gethash "type" tool-use)) "tool calls become tool_use")
          (is (equal "call-7" (gethash "id" tool-use)) "the call id survives")
          (is (hash-table-p (gethash "input" tool-use)))))
      (let ((result-blocks (gethash "content" (aref messages 2))))
        (is (equal "user" (gethash "role" (aref messages 2))))
        (is (equal "tool_result" (gethash "type" (aref result-blocks 0))))
        (is (equal "call-7" (gethash "tool_use_id" (aref result-blocks 0))))))
    (let ((tools (gethash "tools" body)))
      (is (vectorp tools) "tools are declared")
      (is (equal "eval" (gethash "name" (aref tools 0))) "tool name survives")
      (is (hash-table-p (gethash "input_schema" (aref tools 0)))))))

(deftest anthropic-signed-thinking-replays-as-a-thinking-block ()
  ;; A tool-use continuation is refused unless the signed thinking block
  ;; replays ahead of the assistant's other content. Unsigned reasoning (from
  ;; a chat-lane history) cannot be signed after the fact and is dropped.
  (let* ((signed (wire-message "assistant" :content "checking"
                               :tool-calls (vector (wire-tool-call
                                                    "call-9" "eval" "{}"))))
         (unsigned (wire-message "assistant" :content "plain")))
    (setf (gethash "reasoning_content" signed) "pondering"
          (gethash "reasoning_blocks" signed)
          (vector (nle::json-object "thinking" "pondering" "signature" "sig-abc"))
          (gethash "reasoning_content" unsigned) "unsigned thought")
    (let* ((body (nle::anthropic-request-body
                  (anthropic-context
                   (list (nle::message "user" "go")
                         signed
                         (nle::message "tool" "3"
                                                         :name "eval"
                                                         :tool-call-id "call-9")
                         unsigned))))
           (messages (gethash "messages" body))
           (signed-blocks (gethash "content" (aref messages 1)))
           (unsigned-blocks (gethash "content" (aref messages 3))))
      (let ((thinking (aref signed-blocks 0)))
        (is (equal "thinking" (gethash "type" thinking)))
        (is (equal "pondering" (gethash "thinking" thinking)))
        (is (equal "sig-abc" (gethash "signature" thinking))))
      (is (equal "text" (gethash "type" (aref unsigned-blocks 0)))))))

;;; --- thinking bound to its prefix ---------------------------------------------
;;; Without this, an eviction that rewrites the prefix would leave every later
;;; request replaying thinking signed against the old one, each answered with a
;;; 400 `The block is bound to a different conversation'.

(defmacro with-adaptive-claude ((&key (ladder ''("low" "medium" "high" "xhigh" "max"))
                                      context-tokens)
                                &body body)
  "BODY on the anthropic lane against claude-binding-test, whose LADDER
reaches past high unless told otherwise -- adaptive thinking -- in a window of
CONTEXT-TOKENS when given."
  `(nlk:bind ((nle::*provider* "anthropic") (nle::*endpoint* nil)
              (nle::*models-catalog-path* nil) (nle::*model* "claude-binding-test")
              (nle::*model-capability-overrides*
                (list (list* "claude-binding-test" :effort-levels ,ladder
                             (and ,context-tokens (list :context-tokens ,context-tokens)))))
              (nle::*reasoning-effort* "xhigh"))
     ,@body))

(defun bound-round (n prefix)
  "Round N's assistant message as the lane assembles it: one call, and
thinking signed sig-N against PREFIX (none when NIL)."
  (let ((message (wire-message "assistant" :tool-calls (vector (wire-tool-call
                                                                (format nil "call-~d" n) "eval" "{}")))))
    (setf (gethash "reasoning_content" message) (format nil "thought ~d" n)
          (gethash "reasoning_blocks" message)
          (vector (nle::json-object "thinking" (format nil "thought ~d" n)
                                    "signature" (format nil "sig-~d" n))))
    (when prefix
      (setf (gethash "reasoning_prefix" message) prefix))
    message))

(defun bound-result (n)
  "The tool message answering BOUND-ROUND N's call."
  (nle::message "tool" "3" :name "eval" :tool-call-id (format nil "call-~d" n)))

(defun replayed-signatures (body)
  "The thinking signatures BODY, a Messages request, replays, in order."
  (loop for message across (gethash "messages" body)
        append (loop for block across (gethash "content" message)
                     when (equal "thinking" (gethash "type" block))
                       collect (gethash "signature" block))))

(deftest anthropic-thinking-replays-only-behind-the-prefix-it-was-signed-against ()
  (flet ((prefix (&rest messages)
           (nth-value 2 (nle::anthropic-request-body (anthropic-context messages))))
         (replayed (&rest messages)
           (replayed-signatures (nle::anthropic-request-body (anthropic-context messages)))))
    (let ((ask (nle::message "user" "count"))
          (stub (nle::message "system" "[turn history evicted: 1 round · seq 2–3]")))
      (with-adaptive-claude ()
        (let* ((one (bound-round 1 (prefix ask)))
               (two (bound-round 2 (prefix ask one (bound-result 1))))
               (three (bound-round 3 (prefix ask stub two (bound-result 2)))))
          ;; an unchanged prefix replays every round's thinking
          (is (equal '("sig-1" "sig-2") (replayed ask one (bound-result 1) two (bound-result 2))))
          ;; behind an eviction stub none replays, the in-flight round's included
          (is (null (replayed ask stub two (bound-result 2))))
          ;; a round signed behind the stub replays beside the shed ones
          (is (equal '("sig-3") (replayed ask stub two (bound-result 2) three (bound-result 3))))
          ;; any rewrite of what came before is a changed prefix
          (is (null (replayed (nle::message "user" "count again") one (bound-result 1))))
          ;; a message that names no prefix replays no thinking
          (is (null (replayed ask (bound-round 1 nil) (bound-result 1))))))
      ;; a budget request digests nothing and replays every signed block, as
      ;; budget thinking wants
      (with-adaptive-claude (:ladder '("low" "medium" "high"))
        (is (null (prefix ask)))
        (is (equal '("sig-2") (replayed ask stub (bound-round 2 nil) (bound-result 2))))))))

(deftest anthropic-thinking-replays-only-behind-the-thinking-it-was-signed-after ()
  ;; The API binds a block to the thinking ahead of it too: a round signed
  ;; while an earlier round's thinking was shed must not replay once that
  ;; thinking rides again (400 `created as the first thinking block after its
  ;; prompt, but another one now precedes it').
  (flet ((prefix (&rest messages)
           (nth-value 2 (nle::anthropic-request-body (anthropic-context messages))))
         (replayed (&rest messages)
           (replayed-signatures (nle::anthropic-request-body (anthropic-context messages)))))
    (let ((ask (nle::message "user" "count")))
      (with-adaptive-claude ()
        (let* ((shed (bound-round 1 "a prefix this request does not carry"))
               (live (bound-round 1 (prefix ask)))
               ;; round 2 signed while round 1's thinking was shed
               (after-shed (bound-round 2 (prefix ask shed (bound-result 1))))
               ;; round 2 signed behind round 1's thinking
               (after-live (bound-round 2 (prefix ask live (bound-result 1)))))
          (is (not (equal (prefix ask shed (bound-result 1))
                          (prefix ask live (bound-result 1)))))
          ;; each round replays in the context it was signed in
          (is (equal '("sig-2") (replayed ask shed (bound-result 1) after-shed (bound-result 2))))
          (is (equal '("sig-1" "sig-2") (replayed ask live (bound-result 1)
                                                  after-live (bound-result 2))))
          ;; round 1's thinking riding again sheds the round signed without it
          (is (equal '("sig-1") (replayed ask live (bound-result 1)
                                          after-shed (bound-result 2))))
          ;; and round 1's thinking shed sheds the round signed behind it
          (is (null (replayed ask shed (bound-result 1) after-live (bound-result 2)))))))))

(deftest anthropic-thinking-never-replays-to-another-model ()
  ;; A refusal's fallback round (CALL-PROVIDER) is another model's: what one
  ;; model signed is bound to it, so neither model's thinking reaches the other.
  (flet ((on (model &rest messages)
           (nlk:bind ((nle::*model* model))
             (multiple-value-list (nle::anthropic-request-body (anthropic-context messages))))))
    (nlk:bind ((nle::*provider* "anthropic") (nle::*endpoint* nil) (nle::*models-catalog-path* nil)
               (nle::*model-capability-overrides*
                 (loop for model in '("claude-binding-test" "claude-binding-alt")
                       collect (list model :effort-levels '("low" "medium" "high" "xhigh" "max"))))
               (nle::*reasoning-effort* "xhigh"))
      (let* ((ask (nle::message "user" "count"))
             (one (bound-round 1 (third (on "claude-binding-test" ask))))
             (two (bound-round 2 (third (on "claude-binding-alt" ask one (bound-result 1))))))
        (is (not (equal (third (on "claude-binding-test" ask)) (third (on "claude-binding-alt" ask)))))
        ;; the candidate's round sheds the selection's thinking
        (is (null (replayed-signatures (first (on "claude-binding-alt" ask one (bound-result 1))))))
        ;; back on the selection, its own thinking replays and the candidate's does not
        (is (equal '("sig-1") (replayed-signatures
                               (first (on "claude-binding-test" ask one (bound-result 1)
                                          two (bound-result 2))))))))))

(deftest anthropic-stamps-a-thinking-reply-with-the-prefix-it-is-bound-to ()
  (with-adaptive-claude ()
    (with-mock-sse-stream (:stream (make-anthropic-stream))
      (let ((prefix (nth-value 2 (nle::anthropic-request-body (anthropic-user-context "add")))))
        (is (stringp prefix))
        (is (equal prefix (gethash "reasoning_prefix"
                                   (nle::call-anthropic-streaming (anthropic-user-context "add"))))))))
  ;; a budget reply is bound to no prefix
  (with-adaptive-claude (:ladder '("low" "medium" "high"))
    (with-mock-sse-stream (:stream (make-anthropic-stream))
      (is (null (nth-value 1 (gethash "reasoning_prefix"
                                      (nle::call-anthropic-streaming (anthropic-user-context "add")))))))))

(defun bound-stream (n &key (call-p t) (thought 1600))
  "Round N's canned Messages stream: THOUGHT characters of thinking signed
sig-N, then a step call, or the answer `done' when CALL-P is false."
  ;; No usage: a measured prompt would recalibrate the estimates the window
  ;; evicts against.
  (apply #'make-sse-stream
         (anthropic-event "message_start" "message" (json-obj))
         (anthropic-event "content_block_start" "index" 0
                          "content_block" (json-obj "type" "thinking"))
         (anthropic-event "content_block_delta" "index" 0
                          "delta" (json-obj "type" "thinking_delta"
                                            "thinking" (make-string thought :initial-element #\t)))
         (anthropic-event "content_block_delta" "index" 0
                          "delta" (json-obj "type" "signature_delta"
                                            "signature" (format nil "sig-~d" n)))
         (append
          (if call-p
              (list (anthropic-event "content_block_start" "index" 1
                                     "content_block" (json-obj "type" "tool_use"
                                                               "id" (format nil "toolu-~d" n)
                                                               "name" "step"))
                    (anthropic-event "content_block_delta" "index" 1
                                     "delta" (json-obj "type" "input_json_delta" "partial_json" "{}"))
                    (anthropic-event "message_delta" "delta" (json-obj "stop_reason" "tool_use")))
              (list (anthropic-event "content_block_start" "index" 1
                                     "content_block" (json-obj "type" "text"))
                    (anthropic-event "content_block_delta" "index" 1
                                     "delta" (json-obj "type" "text_delta" "text" "done"))
                    (anthropic-event "message_delta" "delta" (json-obj "stop_reason" "end_turn"))))
          (list (anthropic-event "message_stop")))))

(defun relieved-request-index (bodies)
  "The index of the first of BODIES, a turn's Messages requests in order, that
replays none of the thinking its earlier rounds signed: the request a relief
of the window went out in front of. Every request before it replays all."
  (position-if (lambda (body) (null (replayed-signatures body))) bodies :start 1))

(defun bound-window ()
  "The window AN-EVICTION-SHEDS-THE-THINKING-IT-UNBOUND-AND-A-RESUME-SENDS-THE-REST
runs in: one BOUND-STREAM turn on a session of its own, in no known window,
gives the governor's estimate of each request, and the window puts the
eviction trigger halfway between the fourth and the fifth."
  (let ((nle::*tools* nle::*tools*) (estimates '()) (calls 0))
    (nle:register-tool "step" (probe-schema) (lambda (arguments) (declare (ignore arguments)) "stepped"))
    (nlk:create-session :id "bound-window")
    (with-adaptive-claude ()
      (with-stubbed-fdefinitions ((nle::context-pressure-decision (estimated budget)
                                    (push estimated estimates)
                                    (funcall original estimated budget))
                                  (dex:post (endpoint &rest arguments)
                                    (incf calls)
                                    (values (bound-stream calls :call-p (/= calls 6)) 200)))
        (nle:run "count" :session (nle:intern-session "bound-window") :command-id "bound-window-1")))
    (setf estimates (reverse estimates))
    (assert (<= 5 (length estimates)) () "the window's turn made ~d requests, not 6" (length estimates))
    (ceiling (/ (+ (nth 3 estimates) (nth 4 estimates)) 2)
             nle::*context-eviction-trigger-ratio*)))

(deftest an-eviction-sheds-the-thinking-it-unbound-and-a-resume-sends-the-rest
    (with-restartable-store (path "bound"))
  ;; The od2 seam end to end: the real lane signs and stamps each round, the
  ;; window relieves the live turn -- its older rounds thinned, the oldest
  ;; gone if that is not enough -- and the requests that follow -- the
  ;; in-flight round's continuation, the next round, and a new turn on the
  ;; store reopened -- replay only the thinking signed after the relief.
  ;; The window is sized to the prompt, the system message and the tool
  ;; manual most of it, whose size moves with the source (6,000, 3,000 and
  ;; 9,500 each went stale): BOUND-WINDOW measures it, so the window relieves
  ;; once, at the fifth request, and the second turn fits behind it.
  (let ((window (bound-window)))
    (with-adaptive-claude (:context-tokens window)
      (let ((nle::*tools* nle::*tools*) (bodies '()) (calls 0) (answer-at 6))
        (nle:register-tool "step" (probe-schema) (lambda (arguments) (declare (ignore arguments)) "stepped"))
        (with-stubbed-fdefinition (dex:post (endpoint &rest arguments)
                                    (push (nlk:decode-json (getf arguments :content)) bodies)
                                    (incf calls)
                                    (values (bound-stream calls :call-p (/= calls answer-at)) 200))
          (nle:run "count" :session (nle:intern-session "bound") :command-id "bound-1")
          (let* ((turn (reverse bodies))
                 (at (relieved-request-index turn)))
            ;; the turn ran every round, and the window was relieved inside it
            (is (= answer-at (length turn)))
            (is (= 1 (count-session-events "bound" "turn.context.evicted")))
            (is (and at (plusp at)))
            (when at
              ;; the continuation after the relief replays none, the in-flight
              ;; round's included; the next round replays the one signed after it
              (is (null (replayed-signatures (nth at turn))))
              (is (equal (list (format nil "sig-~d" (1+ at))) (replayed-signatures (nth (1+ at) turn))))
              ;; a new turn on the reopened store replays every round signed after the relief
              (reopen-store path)
              (setf answer-at (1+ calls))
              (nle:run "again" :session (nle:intern-session "bound") :command-id "bound-2")
              (is (equal (loop for n from (1+ at) to (length turn) collect (format nil "sig-~d" n))
                         (replayed-signatures (nth (length turn) (reverse bodies))))))))))))

(deftest anthropic-history-sends-no-empty-text-block ()
  ;; A channel lane records the organism's own post as a turn whose user
  ;; message is "", which the chat wire takes and the Messages API refuses
  ;; (400 `text content blocks must be non-empty'): an empty message is left
  ;; out and its neighbours merge, an empty part is dropped.
  (let* ((thought (let ((message (wire-message "assistant" :content "")))
                    (setf (gethash "reasoning_content" message) "unsigned thought")
                    message))
         (call (wire-message "assistant" :tool-calls (vector (wire-tool-call
                                                              "call-1" "eval" "{}"))))
         (messages (gethash "messages"
                            (nle::anthropic-request-body
                             (anthropic-context
                              (list (nle::message "user" "hi")
                                    (wire-message "assistant" :content "hello")
                                    (nle::message "user" "")
                                    (wire-message "assistant" :content "my own post")
                                    (nle::message "user" "look")
                                    thought
                                    call
                                    (nle::message "tool" (vector (nle::make-text-content-part ""))
                                                  :name "eval" :tool-call-id "call-1")
                                    (nle::message "user"
                                                  (vector (nle::make-text-content-part "")
                                                          (nle::make-image-content-part
                                                           "data:image/png;base64,QUJD")))))))))
    ;; the empty user message and the empty assistant one are left out
    (is (equal '("user" "assistant" "user" "assistant" "user")
               (map 'list (lambda (message) (gethash "role" message)) messages)))
    ;; the post merges into the reply before it
    (is (equal '("text" "text") (map 'list (lambda (block) (gethash "type" block))
                                     (gethash "content" (aref messages 1)))))
    (loop for message across messages
          do (is (plusp (length (gethash "content" message))))
             (loop for block across (gethash "content" message)
                   when (equal "text" (gethash "type" block))
                     do (is (plusp (length (gethash "text" block))))))
    (let ((last (gethash "content" (aref messages 4))))
      (is (equal "" (gethash "content" (aref last 0))) "an empty tool result is \"\"")
      (is (equal "image" (gethash "type" (aref last 1))) "the image stands without its empty text"))))

(deftest anthropic-thinking-budget-suppresses-temperature ()
  (let* ((nle::*reasoning-effort* "low")
         (nle::*temperature* 0.7)
         (body (nle::anthropic-request-body
                (anthropic-user-context "think"))))
    (let ((thinking (gethash "thinking" body)))
      (is (hash-table-p thinking) "a reasoning effort enables thinking")
      (is (integerp (gethash "budget_tokens" thinking)) "with a token budget"))
    (is (null (nth-value 1 (gethash "temperature" body))))
    (is (> (gethash "max_tokens" body)
           (gethash "budget_tokens" (gethash "thinking" body))))))

(defun json-obj (&rest pairs)
  "The waist's runtime object constructor under a short local name: the
anthropic fixtures below nest four deep, and the full spelling reflows
every one of them."
  (apply #'nlk:make-json-object pairs))

(defun anthropic-event (type &rest pairs)
  (json-line (apply #'json-obj "type" type pairs)))

(defun make-anthropic-stream ()
  "A canned Messages stream: thinking, text, one tool call, usage, stop."
  (make-sse-stream
   (anthropic-event "message_start"
                    "message" (json-obj "usage" (json-obj "input_tokens" 50
                                                          "cache_read_input_tokens" 10)))
   (anthropic-event "content_block_start" "index" 0
                    "content_block" (json-obj "type" "thinking"))
   (anthropic-event "content_block_delta" "index" 0
                    "delta" (json-obj "type" "thinking_delta" "thinking" "pondering"))
   (anthropic-event "content_block_delta" "index" 0
                    "delta" (json-obj "type" "signature_delta" "signature" "sig-abc"))
   (anthropic-event "content_block_start" "index" 1
                    "content_block" (json-obj "type" "text"))
   (anthropic-event "content_block_delta" "index" 1
                    "delta" (json-obj "type" "text_delta" "text" "the answer"))
   (anthropic-event "content_block_start" "index" 2
                    "content_block" (json-obj "type" "tool_use"
                                              "id" "toolu-1" "name" "eval"))
   (anthropic-event "content_block_delta" "index" 2
                    "delta" (json-obj "type" "input_json_delta"
                                      "partial_json" "{\"form\": \"(+ 2 2)\"}"))
   (anthropic-event "message_delta"
                    "delta" (json-obj "stop_reason" "tool_use")
                    "usage" (json-obj "output_tokens" 25))
   (anthropic-event "message_stop")))

(deftest a-reply-with-two-thinking-blocks-replays-both ()
  ;; claude-opus-5-5 answers a round [thinking, thinking, tool_use]; without
  ;; keeping the two thinking blocks separate, text and signature run
  ;; together, which the API's binding check reads as a block whose
  ;; predecessor is missing (kind=predecessor_missing).
  (with-mock-sse-stream
      (:stream (make-sse-stream
                (anthropic-event "message_start" "message" (json-obj "usage" (json-obj "input_tokens" 5)))
                (anthropic-event "content_block_start" "index" 0 "content_block" (json-obj "type" "thinking"))
                (anthropic-event "content_block_delta" "index" 0
                                 "delta" (json-obj "type" "thinking_delta" "thinking" "first"))
                (anthropic-event "content_block_delta" "index" 0
                                 "delta" (json-obj "type" "signature_delta" "signature" "sig-a"))
                (anthropic-event "content_block_start" "index" 1 "content_block" (json-obj "type" "thinking"))
                (anthropic-event "content_block_delta" "index" 1
                                 "delta" (json-obj "type" "signature_delta" "signature" "sig-b"))
                (anthropic-event "content_block_start" "index" 2
                                 "content_block" (json-obj "type" "tool_use" "id" "toolu-1" "name" "eval"))
                (anthropic-event "content_block_delta" "index" 2
                                 "delta" (json-obj "type" "input_json_delta" "partial_json" "{}"))
                (anthropic-event "message_delta" "delta" (json-obj "stop_reason" "tool_use")
                                 "usage" (json-obj "output_tokens" 9))
                (anthropic-event "message_stop")))
    (let* ((message (nle::call-anthropic-streaming (anthropic-user-context "go")))
           (body (nle::anthropic-request-body
                  (anthropic-context (list (nle::message "user" "go") message
                                           (nle::message "tool" "ok" :name "eval" :tool-call-id "toolu-1")))))
           (replayed (gethash "content" (aref (gethash "messages" body) 1))))
      (is (equalp (vector (nle::json-object "thinking" "first" "signature" "sig-a")
                          (nle::json-object "thinking" "" "signature" "sig-b"))
                  (gethash "reasoning_blocks" message)) "each block apart, in wire order")
      (let ((thinking (loop for block across replayed
                            when (equal "thinking" (gethash "type" block))
                              collect (list (gethash "thinking" block) (gethash "signature" block)))))
        (is (equal '(("first" "sig-a") ("" "sig-b")) thinking) "both replay, the empty one too, as they came"))
      (is (equal "tool_use" (gethash "type" (aref replayed 2))) "and the call after them"))))

(deftest anthropic-stream-assembles-chat-shape ()
  (with-mock-sse-stream (:stream (make-anthropic-stream))
    (multiple-value-bind (message usage finish-reason)
        (nle::call-anthropic-streaming (anthropic-user-context "add"))
      (is (equal "the answer" (gethash "content" message)))
      (is (equal "pondering" (gethash "reasoning_content" message)))
      (is (equalp (vector (nle::json-object "thinking" "pondering" "signature" "sig-abc"))
                  (gethash "reasoning_blocks" message)) "the block, its signature beside its text")
      (let ((calls (gethash "tool_calls" message)))
        (is (and (vectorp calls) (= 1 (length calls))))
        (let ((call (aref calls 0)))
          (is (equal "toolu-1" (gethash "id" call)) "the call id survives")
          (is (equal "eval" (gethash "name" (gethash "function" call))))
          (is (search "(+ 2 2)" (gethash "arguments" (gethash "function" call))))))
      (is (equal "tool_calls" finish-reason))
      (is (= 50 usage.input-tokens))
      (is (= 10 usage.cached-input-tokens))
      (is (= 25 usage.output-tokens)))))

(deftest anthropic-stream-without-message-stop-is-truncation (with-mock-sse-stream)
  (:truncated
   (anthropic-event "content_block_start" "index" 0
                    "content_block" (json-obj "type" "text"))
   (anthropic-event "content_block_delta" "index" 0
                    "delta" (json-obj "type" "text_delta" "text" "half")))
(is (signals-error nle::provider-stream-incomplete
      (nle::call-anthropic-streaming (anthropic-user-context)))))

(deftest anthropic-lane-is-dispatched-by-call-provider (let ((captured-headers nil)))
  (with-stubbed-fdefinition (dex:post (url &rest args)
                             (setf captured-headers (getf args :headers))
                             (values (make-anthropic-stream) 200))
    (let ((message (nle::call-provider (anthropic-user-context "add"))))
      (is (equal "the answer" (gethash "content" message)))))
  (is (assoc "x-api-key" captured-headers :test #'equal))
  (is (assoc "anthropic-version" captured-headers :test #'equal))
  (is (null (assoc "authorization" captured-headers :test #'equal)))
  (let ((ua (cdr (assoc "user-agent" captured-headers :test #'string-equal))))
    (is (and ua (eql 0 (search "nodecode_cli/" ua))))))

(deftest user-agent-identifies-nodecode-cli ()
  ;; Every provider request carries product/version (os release; arch)
  ;; terminal, the shape codex_cli_rs and pi send; dexador's default named
  ;; the HTTP library instead.
  (let ((ua (nle::user-agent)))
    (is (eql 0 (search "nodecode_cli/" ua)) "leads with the product token")
    (is (search (format nil "(~a ~a; ~a)" (software-type) (software-version) (machine-type)) ua))
    (is (not (find #\Newline ua)) "one line")
    (is (every (lambda (c) (< (char-code c) 128)) ua) "ascii, as a header must be")
    (is (search "/" (subseq ua 0 (position #\Space ua))) "product/version before the first space"))
  (let ((h (nle::with-user-agent '(("x-api-key" . "k")))))
    (is (equal "user-agent" (car (first h))) "with-user-agent puts the header first")
    (is (equal '("x-api-key" . "k") (second h)) "and keeps the caller's headers")
    (is (equal '(("user-agent" . "custom")) (nle::with-user-agent '(("user-agent" . "custom")))))))

(deftest an-openrouter-round-names-nodecode-for-the-app-rankings ()
  ;; OpenRouter ranks apps by the tokens that reach it carrying HTTP-Referer:
  ;; without the header, a nodecode round there counts for no app at all.
  (flet ((headers-sent (endpoint &aux (captured nil))
           (let ((nle::*endpoint* endpoint))
             (with-stubbed-fdefinition (dex:post (url &rest args)
                                        (declare (ignore url))
                                        (setf captured (getf args :headers))
                                        (values (make-sse-stream (chunk-json "ok")) 200))
               (nle::call-provider-streaming (user-context))))
           captured)
         (header (name headers) (cdr (assoc name headers :test #'string-equal))))
    (let ((sent (headers-sent "https://openrouter.ai/api/v1/chat/completions")))
      (is (equal "https://nodecode.ai" (header "http-referer" sent)))
      (is (equal "NODECODE" (header "x-openrouter-title" sent)))
      (is (equal "cli-agent,personal-agent" (header "x-openrouter-categories" sent)))
      (is (header "authorization" sent) "the credential still rides beside them"))
    (let ((sent (headers-sent "https://api.deepseek.com/v1/chat/completions")))
      (is (notany (lambda (name) (header name sent))
                  '("http-referer" "x-openrouter-title" "x-openrouter-categories")) "another host hears no app name")))
  (is (equal '(("HTTP-Referer" . "https://me.example")
               ("X-OpenRouter-Title" . "NODECODE")
               ("X-OpenRouter-Categories" . "cli-agent,personal-agent"))
             (nle::with-app-attribution "https://eu.openrouter.ai/api/v1/chat/completions"
                                        '(("HTTP-Referer" . "https://me.example")))) "a referer the caller set stands, once"))

(deftest system-version-carries-the-commit-the-build-names ()
  ;; The .asd composes the declared version — base plus the commit its build
  ;; names, the pack script's stamp outranking git — and *USER-AGENT-VERSION*
  ;; reports exactly that. The form every reader reaches for must not be a
  ;; bare `0.0.1': without it, the installed build would report `nodecode 0.0.1'
  ;; while the same revision built from the checkout would report the commit
  ;; suffix. src/tools/pack.sh stamps the revision it was handed; nodecode.asd
  ;; holds the one rule, read here through the system definition itself
  ;; (LOAD-ASD binds ASDF-USER, which is where NODECODE-VERSION lands).
  (let ((version (asdf:component-version (asdf:find-system "nodecode"))))
    (is (stringp version))
    (is (every (lambda (part) (and (plusp (length part)) (every #'digit-char-p part))) (uiop:split-string (subseq version 0 (position #\+ version)) :separator ".")) "the declared base leads, dotted numbers")
    (is (find #\+ version) "the commit rides it — this checkout has git to ask")
    (is (equal version (nle::compose-user-agent-version))))
  (let ((rule (symbol-function (find-symbol "NODECODE-VERSION" :asdf-user)))
        (saved (sb-posix:getenv "NODECODE_BUILD_COMMIT")))
    (nlk:with-cleanup ((if saved
                           (sb-posix:setenv "NODECODE_BUILD_COMMIT" saved 1)
                           (ignore-errors (sb-posix:unsetenv "NODECODE_BUILD_COMMIT"))))
      (sb-posix:setenv "NODECODE_BUILD_COMMIT" "abc1234" 1)
      (is (equal "0.0.1+abc1234" (funcall rule "0.0.1")) "the stamp outranks git")
      (is (equal "0.0.1+abc1234" (funcall rule "0.0.1+stale")))
      (sb-posix:setenv "NODECODE_BUILD_COMMIT" "  " 1)
      (is (not (search "abc1234" (funcall rule "0.0.1"))) "a blank stamp is no stamp")
      (sb-posix:unsetenv "NODECODE_BUILD_COMMIT")
      (is (not (search "abc1234" (funcall rule "0.0.1")))))))



;;; --- typed content parts: one owner of the content shape -------------------

(deftest content-parts-normalize-both-wire-shapes ()
  (is (equal '() (nle::message-content-parts "")))
  (is (equal '() (nle::message-content-parts :null)))
  (let ((parts (nle::message-content-parts "hello")))
    (is (= 1 (length parts)) "a string is one text part")
    (is (equal "text" (nle::content-part-type (first parts)))))
  (let ((parts (nle::message-content-parts
                (vector (nle::make-text-content-part "look: ")
                        (nle::make-image-content-part "data:image/png;base64,QUJD")))))
    (is (= 2 (length parts)) "a part vector keeps its parts in order")
    (is (equal "image_url" (nle::content-part-type (second parts)))))
  (is (equal "look: " (nle::content-text
                       (vector (nle::make-text-content-part "look: ")
                               (nle::make-image-content-part "data:x;base64,QQ==")))))
  (is (equal "verbatim" (nle::content-text "verbatim"))))

(deftest anthropic-converts-typed-content-parts ()
  (let* ((multipart (nle::message
                     "user"
                     (vector (nle::make-text-content-part "what is this?")
                             (nle::make-image-content-part
                              "data:image/png;base64,QUJD"))))
         (body (nle::anthropic-request-body
                (anthropic-context (list multipart))))
         (messages (gethash "messages" body))
         (blocks (gethash "content" (aref messages 0))))
    (is (= 2 (length blocks)) "both parts convert")
    (is (equal "text" (gethash "type" (aref blocks 0))))
    (is (equal "what is this?" (gethash "text" (aref blocks 0))))
    (let ((image (aref blocks 1)))
      (is (equal "image" (gethash "type" image)))
      (let ((source (gethash "source" image)))
        (is (equal "base64" (gethash "type" source)))
        (is (equal "image/png" (gethash "media_type" source)))
        (is (equal "QUJD" (gethash "data" source)))))))

(deftest empty-response-contract-is-total-over-content-shapes ()
  (is (nle::assistant-message-empty-p
       (nle::message "assistant" (vector))))
  (is (not (nle::assistant-message-empty-p
            (nle::message
             "assistant" (vector (nle::make-text-content-part "said something")))))))

(deftest reasoning-only-is-an-empty-response ()
  ;; Reasoning alone is not an answer: a final whose only words are
  ;; reasoning_content is a failed attempt, so the retry ladder re-asks and
  ;; the room never loses a turn to silence.
  (let ((reasoning-only (nle::message "assistant" (vector))))
    (setf (gethash "reasoning_content" reasoning-only)
          "10,462 chars of thinking, no answer")
    (is (nle::assistant-message-empty-p reasoning-only) "reasoning alone leaves the message empty")))

(deftest anthropic-part-tag-sequence-is-golden (let ((tags '())))
  ;; The anthropic bracket order differs from chat by design: blocks close on
  ;; the stop_reason frame (flushOpenBlocks: text, reasoning, tool drain), not
  ;; at [DONE], and the signature surfaces as its own replay-material part.
  (with-mock-sse-stream (:stream (make-anthropic-stream))
    (nle::call-anthropic-streaming
     (anthropic-user-context "add")
     :on-part (lambda (part) (push part.kind tags))))
  (is (equal '(:stream-start :response-metadata
               :reasoning-start :reasoning-delta :reasoning-signature
               :text-start :text-delta
               :tool-input-start :tool-input-delta
               :text-end :reasoning-end :tool-input-end :tool-call
               :finish)
             (nreverse tags))))

(deftest chat-usage-carries-cache-writes-and-reasoning (multiple-value-bind (message usage))
  (streamed-message (chunk-json "ok" :finish-reason "stop")
                    (usage-chunk-json :prompt 100 :completion 20 :total 120
                                      :cached 30 :reasoning 7
                                      :cache-write 40 :write-5m 25 :write-1h 15))
(declare (ignore message))
(is (= 70 usage.input-tokens))
(is (= 30 usage.cached-input-tokens))
(is (= 7 usage.reasoning-tokens))
(is (= 40 usage.cache-write-tokens))
(is (= 25 usage.cache-write-5m-tokens))
(is (= 15 usage.cache-write-1h-tokens)))

(deftest chat-usage-prefers-the-count-the-relay-actually-served ()
  ;; Captured live off a6api's chat endpoint: a relay that converts a
  ;; Responses upstream into chat chunks emits BOTH dialects and zeroes the
  ;; spelling it did not serve. 0 is true in Lisp, so the old
  ;; (or input_tokens prompt_tokens) took the zero and the round's entire
  ;; prompt and completion counts vanished off the finish divider.
  (multiple-value-bind (message usage)
      (streamed-message (chunk-json "ok" :finish-reason "stop")
                        (usage-chunk-json :prompt 4393 :completion 7 :total 4400
                                          :cached 3840 :reasoning 0
                                          :input 0 :output 0))
    (declare (ignore message))
    (is (= 553 usage.input-tokens))
    (is (= 3840 usage.cached-input-tokens))
    (is (= 7 usage.output-tokens))
    (is (= 4400 usage.total-tokens))
    (is (= 0 usage.reasoning-tokens))))

(deftest chat-usage-keeps-a-reported-zero-when-it-is-the-only-report ()
  ;; The other half of the rule: unknown stays NIL, but a provider that says
  ;; zero must not be upgraded to unknown by the preference for positives.
  (multiple-value-bind (message usage)
      (streamed-message (chunk-json "ok" :finish-reason "stop")
                        (usage-chunk-json :prompt 0 :completion 12))
    (declare (ignore message))
    (is (= 0 usage.input-tokens))
    (is (= 12 usage.output-tokens))))

(deftest anthropic-usage-carries-cache-write-split (with-mock-sse-stream)
  ((anthropic-event "message_start"
                    "message" (json-obj
                               "usage" (json-obj
                                        "input_tokens" 50
                                        "cache_read_input_tokens" 10
                                        "cache_creation" (json-obj
                                                          "ephemeral_5m_input_tokens" 200
                                                          "ephemeral_1h_input_tokens" 100))))
   (anthropic-event "content_block_start" "index" 0
                    "content_block" (json-obj "type" "text"))
   (anthropic-event "content_block_delta" "index" 0
                    "delta" (json-obj "type" "text_delta" "text" "hi"))
   (anthropic-event "message_delta"
                    "delta" (json-obj "stop_reason" "end_turn")
                    "usage" (json-obj "output_tokens" 25))
   (anthropic-event "message_stop"))
(multiple-value-bind (message usage)
    (nle::call-anthropic-streaming (anthropic-user-context))
  (declare (ignore message))
  (is (= 300 usage.cache-write-tokens))
  (is (= 200 usage.cache-write-5m-tokens))
  (is (= 100 usage.cache-write-1h-tokens))
  (is (= 75 usage.total-tokens))
  (is (null usage.reasoning-tokens))))

;;; =========================================================================
;;; stream-part projection gates: reasoning rows and stream reset
;;; =========================================================================

(deftest streamed-reasoning-projects-its-own-lane ()
  (let ((state (nle::ingest-stream-body nil :reasoning "deep ")))
    (setf state (nle::ingest-stream-body state :reasoning "thought"))
    (setf state (nle::ingest-stream-body state :text "answer"))
    (let ((rows (nle::transcript-rows state)))
      (is (= 2 (length rows)) "reasoning coalesces apart from text")
      (is (eq :reasoning (nle::transcript-row-kind (first rows))))
      (is (equal "deep thought" (nle::transcript-row-text (first rows))))
      (is (eq :text (nle::transcript-row-kind (second rows)))))))

(deftest reasoning-survives-the-turn-settle ()
  ;; The defect: the streamed thinking row was live-only and no fact ever
  ;; claimed it, so the settle at the terminal fact dropped it and the
  ;; transcript lost its reasoning the moment the turn ended. The durable
  ;; assistant message carries `reasoning_content`; folding it hydrates the
  ;; same row the deltas built.
  (let ((state (nle::ingest-stream-body nil :reasoning "deep ")))
    (setf state (nle::ingest-stream-body state :reasoning "thought"))
    (setf state (nle::ingest-stream-body state :text "answ"))
    (setf state (nle::ingest-message-fact state "assistant" "answer"
                                          :reasoning "deep thought"))
    (let ((row (first (nle::transcript-rows state))))
      (is (eq :reasoning row.kind))
      (is (nle::transcript-row-hydrated row))
      (is (eq :persisted-history row.retention)))
    (setf state (nle::ingest-turn-terminal state "completed"))
    (let ((rows (remove :finish-divider (nle::transcript-rows state)
                        :key #'nle::transcript-row-kind)))
      (is (= 2 (length rows)) "reasoning and content both survive the settle")
      (is (equal "deep thought" (nle::transcript-row-text (first rows))))
      (is (equal "answer" (nle::transcript-row-text (second rows)))))))

(deftest reasoning-fact-opens-a-row-when-no-delta-streamed ()
  ;; Replay: the log is folded without deltas (a resumed session, a client
  ;; that connected mid-turn). The fact alone must produce the thinking row,
  ;; ahead of the content it preceded.
  (let ((state (nle::ingest-message-fact nil "assistant" "answer"
                                         :reasoning "considered it")))
    (let ((rows (nle::transcript-rows state)))
      (is (= 2 (length rows)) "the fact opens both rows")
      (is (eq :reasoning (nle::transcript-row-kind (first rows))))
      (is (equal "considered it" (nle::transcript-row-text (first rows))))
      (is (eq :text (nle::transcript-row-kind (second rows)))))))

(deftest reasoning-facts-pair-with-their-own-round ()
  ;; A multi-round turn streams one thinking span per round and commits one
  ;; fact per round, in the same order. Hydration walks oldest-first so the
  ;; second round's trace cannot land on the first round's row.
  (let ((state (nle::ingest-stream-body nil :reasoning "round one")))
    (setf state (nle::ingest-stream-tool state 0 "read_file" "{}"))
    (setf state (nle::ingest-stream-body state :reasoning "round two"))
    (setf state (nle::ingest-message-fact state "assistant" ""
                                          :reasoning "first thought"))
    (setf state (nle::ingest-message-fact state "assistant" "answer"
                                          :reasoning "second thought"))
    (let ((rows (remove-if-not (lambda (row) (eq :reasoning row.kind)) (nle::transcript-rows state))))
      (is (= 2 (length rows)) "one thinking row per round, still two")
      (is (equal "first thought" (nle::transcript-row-text (first rows))))
      (is (equal "second thought" (nle::transcript-row-text (second rows)))))))

(deftest stream-reset-voids-unhydrated-rows-and-the-settle-drops-them ()
  ;; The reset keeps what the failed attempt streamed (voided, for the
  ;; turn.attempt_voided fact to seal) and touches no durable-fact row; a
  ;; voided row no fact ever confirms goes at the settle, like every other
  ;; unconfirmed stream row.
  (let ((state (nle::ingest-stream-body nil :text "doomed partial")))
    (setf state (nle::ingest-message-fact state "user" "durable question"))
    (setf state (nle::ingest-stream-reset state))
    (let ((rows (nle::transcript-rows state)))
      (is (= 2 (length rows)) "the unconfirmed stream row is kept")
      (is (nle::transcript-row-voided (first rows)) "marked voided")
      (is (not (nle::transcript-row-voided (second rows)))))
    (nle::settle-tail-rows state)
    (let ((rows (nle::transcript-rows state)))
      (is (= 1 (length rows)) "the settle drops the unconfirmed voided row")
      (is (equal "durable question" (nle::transcript-row-text (first rows)))))))

;;; --- B3 gate: streamed tool calls project as live-only rows --------------

(deftest streamed-tool-calls-project-live-only-blocks ()
  ;; Each in-flight tool call is one live-only row, upserted by stream index,
  ;; rendering in arrival order.
  (let ((state (nle::ingest-stream-tool nil 0 "eval" "(+ 1")))
    (setf state (nle::ingest-stream-tool state 1 "read_file" "/etc"))
    (let ((blocks (nle:project-state state)))
      (is (= 2 (length blocks)))
      (is (every (lambda (b) (eq :live-only b.retention))
                 blocks))
      (is (search "Eval" (first (nle::display-block-plain-lines (first blocks)))))
      (is (search "read_file" (first (nle::display-block-plain-lines (second blocks))))))))

;;; --- B3 gate: a tool delta upserts its row rather than appending ---------

(deftest streamed-tool-delta-upserts-by-index ()
  (let ((state (nle::ingest-stream-tool nil 0 "eval" "(+ 1")))
    (setf state (nle::ingest-stream-tool state 0 "eval" "(+ 1 2)"))
    (let ((rows (nle::transcript-rows state)))
      (is (= 1 (length rows)) "growing arguments stay on one row")
      (is (equal "(+ 1 2)" (nle::transcript-row-tool-args (first rows)))))))

;;; --- B3 gate: later rounds never re-adopt a completed row -----------------

(deftest later-round-stream-deltas-never-clobber-a-hydrated-row ()
  ;; Provider stream indices restart at 0 every tool round. Round 2's deltas
  ;; must open a fresh row, not adopt round 1's hydrated row — the observed
  ;; failure paired round 1's result with round 3's arguments in the
  ;; transcript.
  (let ((state (nle::ingest-stream-tool nil 0 "eval"
                                        "(multiple-value-list (find-symbol \"X\"))")))
    (setf state (nle::ingest-tool-call-fact
                 state "call-1" "eval"
                 "(multiple-value-list (find-symbol \"X\"))"))
    (setf state (nle::ingest-tool-result-fact
                 state "call-1" "eval" "(NIL NIL)"))
    (setf state (nle::ingest-stream-tool state 0 "eval"
                                         "(mapcar #'package-name (list-all-packages))"))
    (let ((rows (nle::transcript-rows state)))
      (is (= 2 (length rows)))
      (is (equal "(multiple-value-list (find-symbol \"X\"))"
                 (nle::transcript-row-tool-args (first rows))))
      (is (equal "(NIL NIL)" (nle::transcript-row-text (first rows))))
      (is (equal "(mapcar #'package-name (list-all-packages))"
                 (nle::transcript-row-tool-args (second rows)))))))

;;; --- B3 gate: rows render in chronology, tool round before the answer ----

(deftest streamed-tool-row-precedes-streamed-text ()
  ;; A tool round streamed before the text answer renders before it: row
  ;; order is arrival order, owned by the projection, not the renderer.
  (let ((state (nle::ingest-stream-tool nil 0 "eval" "(* 6")))
    (setf state (nle::ingest-stream-body state :text "The answer is "))
    (let ((blocks (nle:project-state state)))
      (is (= 2 (length blocks)))
      (is (search "Eval" (first (nle::display-block-plain-lines (first blocks)))))
      (is (search "The answer is" (first (block-body-lines (second blocks)))))
      (is (every (lambda (b) (eq :live-only b.retention))
                 blocks)))))

;;; --- B3 gate: streamed tool rows settle away after turn completion -------

(deftest streamed-tool-rows-settled-after-turn-completion (with-temp-store ())
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "b3-clear")))
    (nlt:init model)
    ;; Simulate a tool delta arriving.
    (setf model.state (nle::ingest-stream-tool model.state 0 "eval" "(+ 1 2)"))
    (is (= 1 (length (nle:project-state model.state))))
    ;; The turn settles: no durable fact confirmed the row.
    (nle::settle-tail-rows model.state)
    (is (null (nle:project-state model.state)))))

;;; --- runtime fold -------------------------------------------------------

(defun message-text (message)
  (and (hash-table-p message) (gethash "content" message)))

(deftest the-fold-pages-every-event-not-the-first-200 (with-temp-store ())
  (seed-user-inputs "fold-long"
                    (loop for i from 0 below 250
                          collect (format nil "m~d" i)))
  (let ((recalled (wire-fold "fold-long")))
    (is (= 250 (length recalled)) "the fold includes every durable message")
    (is (equal "m0" (message-text (first recalled))) "oldest first")
    (is (equal "m249" (message-text (car (last recalled)))) "newest last")))

(deftest the-fold-signals-when-the-payload-hash-does-not-match (with-temp-store (store))
  (ensure-durable-session "fold-corrupt")
  (nlk:admit-turn "fold-corrupt" "fold-corrupt-cmd" "ok")
  (sqlite:execute-non-query
   (nlk::require-open-store)
   "update events set payload_hash = 'sha256:0000000000000000000000000000000000000000000000000000000000000000'
       where session_id = ?"
   "fold-corrupt")
  (is (signals-error nlk:payload-hash-mismatch
        (wire-fold "fold-corrupt"))))

(deftest hydrate-replaces-stale-heap-from-the-log (with-session-store ("fold-hydrate"))
  (nlk:admit-turn "fold-hydrate" "fold-hydrate-cmd" "from-log")
  (let ((session (nle:intern-session "fold-hydrate")))
    (nle:append-message session (nle:message "user" "stale-heap"))
    (nle:hydrate-session session)
    (is (= 1 (length session.messages)))
    (is (equal "from-log"
               (nle::message-content
                (first session.messages))))))

(deftest run-sends-the-full-fold-to-the-provider (with-session-store ("fold-run"))
  (seed-user-inputs "fold-run"
                    (loop for i from 0 below 210
                          collect (format nil "old-~d" i)))
  (let ((seen nil)
        (session (nle:intern-session "fold-run")))
    (with-provider-stub (context
                         (setf seen (coerce
                                     context.messages
                                     'list))
                         (wire-message "assistant" :content "ok"))
      (nle:run "latest" :session session :command-id "fold-run-cmd"))
    (is (= 211 (length seen)) "210 prior messages plus the admitted input")
    (is (equal "old-0" (message-text (first seen))) "the fold starts at the oldest row")
    (is (equal "latest" (message-text (car (last seen)))))))

(deftest interned-session-reuses-the-object-across-turns (with-session-store ("fold-reuse"))
  (let ((session (nle:intern-session "fold-reuse"))
        (seen '()))
    (with-provider-stub (context
                         (push (length
                                context.messages)
                               seen)
                         (wire-message "assistant" :content "ok"))
      (nle:run "first" :session session :command-id "fold-reuse-1")
      (nle:run "second" :session session :command-id "fold-reuse-2"))
    (is (eq session (nle:intern-session "fold-reuse")))
    (is (equal '(3 1) seen))))


(deftest stored-assistant-tool-message-replays-without-synthesis ()
  (with-session-store ("wire-replay")
    (let* ((turn (nlk:admit-turn "wire-replay" "wire-command" "calculate"))
           (call (wire-tool-call "call-1" "eval" "{\"form\":\"(+ 1 1)\"}"))
           (assistant (wire-message "assistant" :content "thinking"
                                    :tool-calls (vector call))))
      (setf (gethash "reasoning" assistant) "kept")
      (nlk:record-assistant-message turn assistant)
      (nlk:record-tool-call turn "call-1" "eval" "{\"form\":\"(+ 1 1)\"}")
      (nlk:record-tool-result turn "call-1" "eval" "2")
      (nlk:complete-turn turn nil)
      (let ((messages (wire-fold "wire-replay")))
        (is (= 3 (length messages)) "stored assistant message is not duplicated")
        (is (equal '("user" "assistant" "tool")
                   (mapcar (lambda (message) (gethash "role" message)) messages)))
        (is (equal "kept" (gethash "reasoning" (second messages))))))))

(deftest orphan-tool-result-never-reaches-provider-context (with-session-store ("wire-orphan"))
  (let* ((turn (nlk:admit-turn "wire-orphan" "orphan-command" "probe"))
         (call (wire-tool-call "call-real" "eval" "{\"form\":\"(+ 1 1)\"}"))
         (assistant (wire-message "assistant" :content ""
                                  :tool-calls (vector call))))
    (nlk:record-assistant-message turn assistant)
    (nlk:record-tool-call turn "call-real" "eval" "{\"form\":\"(+ 1 1)\"}")
    ;; The dispatch seam invoked directly from a EVAL form journals
    ;; real call/result facts under an id no assistant message issued.
    ;; Replaying that result would 400 every later round of the session.
    (nlk:record-tool-call turn "probe-1" "oversized_probe" "{}")
    (nlk:record-tool-result turn "probe-1" "oversized_probe" "oversized")
    (nlk:record-tool-result turn "call-real" "eval" "2")
    (nlk:complete-turn turn nil)
    (let* ((messages (wire-fold "wire-orphan"))
           (tool-ids (loop for msg in messages
                           when (equal "tool" (gethash "role" msg))
                             collect (gethash "tool_call_id" msg))))
      (is (equal '("call-real") tool-ids)))))

(deftest fold-synthesized-tool-call-still-pairs-its-result (with-session-store ("wire-synth"))
  (let ((turn (nlk:admit-turn "wire-synth" "synth-command" "go")))
    ;; No stored assistant message: the fold synthesizes the assistant
    ;; tool_calls message from the call fact, which issues the id, so the
    ;; orphan guard must keep this result.
    (nlk:record-tool-call turn "call-synth" "eval" "{\"form\":\"1\"}")
    (nlk:record-tool-result turn "call-synth" "eval" "1")
    (nlk:complete-turn turn nil)
    (let ((messages (wire-fold "wire-synth")))
      (is (equal '("user" "assistant" "tool")
                 (mapcar (lambda (message) (gethash "role" message)) messages)))
      (is (equal "call-synth" (gethash "tool_call_id" (third messages)))))))

(deftest recursive-tool-round-sees-assistant-call-and-result ()
  (with-session-store ("tool-round-context")
    (let ((contexts '())
          (round 0))
      (with-provider-stub (context
                           (push (coerce context.messages 'list) contexts)
                           (incf round)
                           (if (= round 1)
                               (wire-message "assistant" :content ""
                                             :tool-calls
                                             (vector (wire-tool-call "round-call" "eval"
                                                                     "{\"form\":\"(+ 1 1)\"}")))
                               (wire-message "assistant" :content "done")))
        (nle:run "calculate" :session (nle:intern-session "tool-round-context")
                 :command-id "tool-round-command"))
      (let ((second-context (first contexts)))
        (is (equal '("user" "assistant" "tool")
                   (mapcar (lambda (message) (gethash "role" message)) second-context)))))))

(deftest retained-floor-is-monotone-and-the-fold-remains-complete ()
  (with-session-store ("context-floor")
    (seed-completed-turn "context-floor" "first" "one")
    (seed-completed-turn "context-floor" "second" "two")
    (let* ((session (nle:intern-session "context-floor"))
           (turns (nle::history-turns-for-session session))
           (floor (nle::history-turn-start-sequence (second turns))))
      (nlk:advance-history-floor "context-floor" floor
                                 :evicted-start-sequence 1
                                 :evicted-end-sequence (1- floor)
                                 :evicted-turn-count 1)
      (nlk:advance-history-floor "context-floor" 1)
      (is (= floor (nlk:retained-history-floor "context-floor")))
      (is (= 4 (length (wire-fold (nle:session-id session)))))
      (nle:hydrate-session session)
      (is (= 1 session.history-boundary)))))


(defmacro with-folded-context ((context session-id command-id input) &body body)
  "Run BODY over CONTEXT: a turn context compiled from SESSION-ID\'s whole
folded log, for a turn admitted on it under COMMAND-ID carrying INPUT.

Three more names are bound around BODY because the window cases read them.
TURN is the admitted turn, and it is also *CURRENT-DURABLE-TURN* — where
the retry ladder looks when it bills a voided attempt. SESSION is the
interned heap session the fold came from. CALLS is a counter at zero for
the provider stub to advance, since every case here counts rounds. The
history boundary is 0, so whatever terminal turns the log already holds are
what the eviction ladder has to work with.

The turn is cancelled under `test cleanup\' however BODY leaves: none of
these cases completes its turn — the claim is about what the provider did,
not about how the turn ended — and a stranded open turn is re-billed by the
next boot resume."
  `(with-durable-turn (turn ,session-id :command-id ,command-id :input ,input :current t)
     (let* ((session (nle:intern-session ,session-id))
            (,context (nle::compile-turn-context
                       (nle::snapshot-context-request
                        ,session-id
                        :messages (wire-fold ,session-id)
                        :history-turns (nle::history-turns-for-session session)
                        :history-boundary 0)))
            (calls 0))
       (declare (ignorable session calls))
       ,@body)))

(deftest provider-overflow-evicts-and-retries-exactly-once (with-temp-store ())
  (seed-completed-turn "overflow-once" "old question" "old answer")
  (with-folded-context (context "overflow-once" "overflow-command" "oversized")
    (with-streaming-provider-stub
        (request-context on-part
         (incf calls)
         (if (= calls 1)
             (error 'nle::provider-error
                    :status 413 :detail "context too large")
             (wire-message "assistant" :content
                           (if (search "history evicted"
                                       (message-text (aref request-context.messages 0)))
                               "recovered" "missing stub"))))
      (let ((reply (nle::call-provider context)))
        (is (equal "recovered" (gethash "content" reply)))))
    (is (= 2 calls) "one overflow produces exactly one retry")
    (is (> (nlk:retained-history-floor "overflow-once") 0))
    ;; Ceiling evidence: the rejection bounds the window from above with
    ;; the request's calibrated estimate.
    (multiple-value-bind (floor ceiling)
        (nlk:context-window-observation
         (nle::effective-provider-config-provider context.provider-config)
         (nle::effective-provider-config-model
          context.provider-config))
      (declare (ignore floor))
      (is (integerp ceiling)))))

(deftest provider-overflow-with-nothing-evictable-propagates (with-session-store ("overflow-stuck"))
  ;; A bare live turn (no completed rounds, no terminal history) has nothing
  ;; to evict: the live turn is NEVER evicted whole — that path once produced
  ;; a request with no user message at all (the Qwen no-user-query 500) and
  ;; a floor the continuing turn would immediately resurrect across. The
  ;; provider's rejection stands as the turn's honest outcome.
  (with-folded-context (context "overflow-stuck" "stuck-command" "giant paste")
    (with-streaming-provider-stub
        (request-context on-part
         (incf calls)
         (error 'nle::provider-error
                :status 413 :detail "context too large"))
      (multiple-value-bind (value condition)
          (ignore-errors (nle::call-provider context))
        (is (null value) "the rejection is not absorbed")
        (is (typep condition 'nle::provider-error))))
    (is (= 1 calls) "nothing evictable means no blind retry")
    (is (zerop (nlk:retained-history-floor "overflow-stuck")))))

(deftest provider-output-limit-400-evicts-nothing (with-temp-store ())
  ;; A 400 saying `max_tokens' exceeds the model's output limit would read as
  ;; an overflow on the bare `token' needle and evict turns. Only the window's
  ;; words evict.
  (flet ((overflow-p (status detail)
           (nle::provider-overflow-error-p
            (make-condition 'nle::provider-error :status status :detail detail))))
    (dolist (detail '("Invalid request: 'max_tokens' exceeds the model's output limit of 8192"
                      "max_tokens: 100000 > 64000, which is the maximum allowed number of output tokens"
                      "max_tokens is too large: 200000. This model supports at most 128000 completion tokens"
                      "invalid token"))
      (is (not (overflow-p 400 detail)) detail))
    (dolist (detail '("This model's maximum context length is 128000 tokens. However, your messages resulted in 130000 tokens."
                      "prompt is too long: 205000 tokens > 200000 maximum"
                      "input length and `max_tokens` exceed context limit: 198000 + 64000 > 200000"
                      "The input token count (1234567) exceeds the maximum number of tokens allowed (1048576)."
                      "the request exceeds the available context size, try increasing it"))
      (is (overflow-p 400 detail) detail))
    (is (overflow-p 413 "request_too_large")))
  (seed-completed-turn "output-limit" "old question" "old answer")
  (with-folded-context (context "output-limit" "output-limit-command" "a question")
    (with-streaming-provider-stub
        (request-context on-part
         (incf calls)
         (error 'nle::provider-error
                :status 400 :detail "Invalid request: 'max_tokens' exceeds the model's output limit of 8192"))
      (multiple-value-bind (value condition)
          (ignore-errors (nle::call-provider context))
        (is (null value))
        (is (typep condition 'nle::provider-error))))
    (is (= 1 calls) "a 400 no eviction can cure is sent once")
    (is (zerop (nlk:retained-history-floor "output-limit")) "and evicts nothing")))

(deftest context-edge-length-cut-voids-evicts-and-retries (with-temp-store ())
  ;; A finish=length round that our own max_tokens cannot explain is the
  ;; server's context edge. The truncated message is a failed attempt, not
  ;; durable history; its spend bills as a voided usage fact; its usage total
  ;; is a proven window floor (never a ceiling — local stacks cut at their
  ;; own output caps too); one recovery eviction re-aims the window and the
  ;; loop re-streams.
  (seed-completed-turn "length-cut" "old question" "old answer")
  (with-folded-context (context "length-cut" "cut-command" "current task")
    (with-streaming-provider-stub
        (request-context on-part
         (incf calls)
         (if (= calls 1)
             (values (wire-message "assistant"
                                   :content "truncated mid-thought")
                     (nle::make-provider-usage :input-tokens 100
                                               :output-tokens 50
                                               :total-tokens 150)
                     "length" nil)
             (values (wire-message "assistant" :content "recovered")
                     (nle::make-provider-usage :input-tokens 40
                                               :output-tokens 5
                                               :total-tokens 45)
                     "stop" nil)))
      (multiple-value-bind (reply usage finish-reason)
          (nle::call-provider context)
        (declare (ignore usage finish-reason))
        (is (equal "recovered" (gethash "content" reply)))))
    (is (= 2 calls) "one cut produces exactly one recovery retry")
    (let ((voided (remove-if-not #'nlk:turn-usage-event-voided
                                 (session-events
                                  "length-cut"
                                  :type 'nlk:turn-usage-event))))
      (is (= 1 (length voided)))
      (is (equal "length"
                 (nlk:turn-usage-event-finish-reason (first voided))))
      (is (eql 150 (nlk:turn-usage-event-total-tokens (first voided)))))
    (is (> (nlk:retained-history-floor "length-cut") 0))
    (nlk:bind ((config context.provider-config)
               ((floor ceiling) (nlk:context-window-observation config.provider config.model)))
      (is (integerp floor))
      (is (null ceiling)))))

(deftest second-length-cut-in-a-call-records-as-is (with-temp-store ())
  ;; Once per call: a second cut means the cause is not the window (a server
  ;; output cap), and the round records rather than looping.
  (seed-completed-turn "length-cap" "old question" "old answer")
  (seed-completed-turn "length-cap" "older question" "older answer")
  (with-folded-context (context "length-cap" "cap-command" "current task")
    (with-streaming-provider-stub
        (request-context on-part
         (incf calls)
         (values (wire-message "assistant" :content
                               (format nil "cut ~d" calls))
                 (nle::make-provider-usage :input-tokens 10
                                           :output-tokens 5
                                           :total-tokens 15)
                 "length" nil))
      (multiple-value-bind (reply usage finish-reason)
          (nle::call-provider context)
        (declare (ignore usage))
        (is (equal "cut 2" (gethash "content" reply)))
        (is (equal "length" finish-reason))))
    (is (= 2 calls) "exactly one recovery, then the round records")))

(deftest steer-arriving-mid-round-ends-the-turn-at-the-boundary (with-session-store ("steer-cut"))
  ;; A steer parked while a tool round is in flight ends the turn at that
  ;; round's boundary instead of letting the loop continue: the completion
  ;; reads steered, the tool results just recorded are the turn's last
  ;; facts, and the steer waits for promotion as the next turn — parented
  ;; on the one it cut short. No steer input fact is ever written.
  (let ((rounds 0))
    (with-provider-stub
        (context
         (incf rounds)
         (case rounds
           (1
            (nlk:admit-active-turn-input
             "steer-cut" "steer-command" "also do this"
             :disposition :steer
             :expected-turn-id
             (nlk:durable-turn-turn-id nle::*current-durable-turn*))
            (wire-message "assistant"
                          :tool-calls (vector (wire-tool-call
                                               "c1" "eval"
                                               "{\"form\": \"(+ 20 22)\"}"))))
           (t (error "the loop continued past the boundary"))))
      (nle:run "start" :session (nle:intern-session "steer-cut")
                       :command-id "steer-start-command")
      (is (= 1 rounds))
      (let* ((started (nlk:events :session-id "steer-cut"
                                  :kind nlk::+kind-turn-started+
                                  :order :oldest :as :instance))
             (completion (nlk:events :session-id "steer-cut"
                                     :kind nlk::+kind-turn-completed+
                                     :as :instance)))
        (is-present completion "the turn completed"
            (is (nlk:turn-completed-steered completion)))
        (is (nlk:events :session-id "steer-cut" :kind nlk::+kind-tool-result+
                        :as :exists))
        (is (null (nlk:events :session-id "steer-cut"
                              :kind nlk::+kind-input-committed+
                              :where '("json_extract(payload, '$.disposition') = 'steer'")
                              :as :instance)))
        (let ((next (nlk::dequeue-next-turn "steer-cut")))
          (is-present next "the steer promotes as the next turn"
              (is (equal "also do this" (second next)) "with its text")
              (is (equal (nlk:event-turn-id started)
                         (nlk:turn-started-parent-turn-id
                          (nlk:events :turn-id (nlk:durable-turn-turn-id (first next))
                                      :kind nlk::+kind-turn-started+
                                      :as :instance))))))))))

(deftest steer-arriving-during-a-terminal-round-runs-next-not-in-this-turn ()
  ;; A steer parked while the final answer streams forces no further round
  ;; of the same turn: the turn completes as it would have — not steered,
  ;; nothing was cut — and the steer promotes as the next turn.
  (with-session-store ("steer-terminal")
    (let ((rounds 0))
      (with-provider-stub
          (context
           (incf rounds)
           (case rounds
             (1
              (nlk:admit-active-turn-input
               "steer-terminal" "steer-command" "also do this"
               :disposition :steer
               :expected-turn-id
               (nlk:durable-turn-turn-id nle::*current-durable-turn*))
              (wire-message "assistant" :content "first answer"))
             (t (error "unexpected extra round"))))
        (let ((text (nle:run "start"
                             :session (nle:intern-session "steer-terminal")
                             :command-id "steer-start-command")))
          (is (= 1 rounds) "the terminal answer ends the turn; no delivery round")
          (is (equal "first answer" text) "the answer stands as the turn's")
          (is (not (nlk:turn-completed-steered
                    (nlk:events :session-id "steer-terminal"
                                :kind nlk::+kind-turn-completed+ :as :instance))))
          (let ((next (nlk::dequeue-next-turn "steer-terminal")))
            (is (equal "also do this" (and next (second next))))))))))

;;; =========================================================================
;;; Tier-2 soundness gates: error degradation, failure evidence, redacted
;;; thinking, the provider_request fact, cache breakpoints, cancellation
;;; =========================================================================

;;; --- the degraded-error rule (turn_engine.zig:4383) -----------------------

(deftest streaming-error-frame-with-declared-completion-degrades (let ((warned nil)
                                                                       (tags '())))
  ;; A stray error chunk from a flaky relay must not turn a delivered round
  ;; into a dead turn when the provider still declared normal completion.
  (with-mock-sse-stream
      ((chunk-json "partial")
       "{\"error\": {\"message\": \"junk chunk from a relay\", \"code\": 500}}"
       (chunk-json "rest" :finish-reason "stop"))
    (handler-bind ((warning (lambda (w) (setf warned t) (muffle-warning w))))
      (multiple-value-bind (message usage finish-reason)
          (nle::call-provider-streaming
           (user-context)
           :on-part (lambda (part) (push part.kind tags)))
        (declare (ignore usage))
        (is (equal "partialrest" (gethash "content" message)))
        (is (equal "stop" finish-reason))
        (is warned "the degraded error is surfaced as a warning")
        (is (member :error tags))))))

(deftest streaming-error-frame-then-truncation-signals-the-error (with-mock-sse-stream)
  ;; EOF after an in-stream error: the recorded error frame names the failure
  ;; better than generic truncation, and its code drives retry classification.
  (:truncated "{\"error\": {\"message\": \"overloaded\", \"code\": 429}}")
(let ((condition (signals-error nle::provider-error
                   (nle::call-provider-streaming (user-context)))))
  (is condition "the attempt fails")
  (is (not (typep condition 'nle::provider-stream-incomplete)))
  (is (eql 429 (nle::provider-error-status condition)))))

(deftest anthropic-error-event-without-completion-signals (with-mock-sse-stream)
  (:truncated
   (anthropic-event "error"
                    "error" (json-obj "type" "overloaded_error"
                                      "message" "busy")))
(let ((condition (signals-error nle::provider-error
                   (nle::call-anthropic-streaming (anthropic-user-context)))))
  (is condition "an error event without message_stop fails the attempt")
  (is (eql 529 (nle::provider-error-status condition)))))

(deftest anthropic-error-event-with-declared-completion-degrades (let ((warned nil)))
  (with-mock-sse-stream
      ((anthropic-event "content_block_start" "index" 0
                        "content_block" (json-obj "type" "text"))
       (anthropic-event "content_block_delta" "index" 0
                        "delta" (json-obj "type" "text_delta" "text" "kept"))
       (anthropic-event "error"
                        "error" (json-obj "type" "api_error" "message" "blip"))
       (anthropic-event "message_delta"
                        "delta" (json-obj "stop_reason" "end_turn"))
       (anthropic-event "message_stop"))
    (handler-bind ((warning (lambda (w) (setf warned t) (muffle-warning w))))
      (let ((message (nle::call-anthropic-streaming
                      (anthropic-user-context))))
        (is (equal "kept" (gethash "content" message)))
        (is warned "the degraded error event is surfaced as a warning")))))

;;; --- failure evidence, non-SSE detection, frame cap ------------------------

(deftest streaming-truncation-carries-bounded-evidence ()
  (with-mock-sse-stream (:truncated (chunk-json "half"))
    (let ((condition (signals-error nle::provider-stream-incomplete
                       (nle::call-provider-streaming (user-context)))))
      (is condition "truncation still signals")
      (is (search "half" (nle::provider-error-evidence-body condition)))
      (is (search "\"messages\""
                  (nle::provider-error-evidence-request-body condition)))
      (is (not (nle::provider-error-stream-complete-p condition))))))

(deftest streaming-non-sse-200-is-a-decisive-failure ()
  (let ((headers (nlk:json-object "content-type" "application/json")))
    (with-stubbed-fdefinition (dex:post (&rest args)
                               (values (make-string-input-stream
                                        "{\"error\": \"proxy answered in full\"}")
                                       200 headers))
      (let ((condition (signals-error nle::provider-non-sse-response
                         (nle::call-provider-streaming (user-context)))))
        (is condition "a 200 with a non-SSE content type fails decisively")
        (is (eql 200 (nle::provider-error-status condition)))
        (is (not (nle::retryable-provider-error-p condition)))
        (is (search "proxy answered"
                    (nle::provider-error-evidence-body condition)))
        (is (search "application/json" (nle::provider-error-detail condition)))))))

(deftest sse-event-over-the-evidence-bound-still-folds ()
  ;; DeepSeek's response.completed echoes the whole think: 250-310 KB on a
  ;; 60K-token round. The frame folds like any other, and only what is
  ;; kept as failure evidence is clipped to the bound.
  (let ((huge (make-string (1+ nle::+max-provider-evidence-bytes+)
                           :initial-element #\a)))
    (let ((message (streamed-message (chunk-json huge)
                                     (chunk-json "tail" :finish-reason "stop"))))
      (is (= (+ (length huge) 4) (length (gethash "content" message))))
      (is (string= "tail" (subseq (gethash "content" message) (length huge)))))
    (with-mock-sse-stream (:truncated (chunk-json huge))
      (let ((condition (signals-error nle::provider-stream-incomplete
                         (nle::call-provider-streaming (user-context)))))
        (is (<= (length (nle::provider-error-evidence-body condition))
                nle::+max-provider-evidence-bytes+))))))

(deftest streaming-socket-closes-on-every-exit ()
  (let ((ok-stream (make-sse-stream (chunk-json "ok"))))
    (with-mock-sse-stream (:stream ok-stream)
      (nle::call-provider-streaming (user-context)))
    (is (not (open-stream-p ok-stream))))
  (let ((bad-stream (make-truncated-sse-stream (chunk-json "half"))))
    (with-mock-sse-stream (:stream bad-stream)
      (nlk:with-handlers ((nle::provider-error () nil))
        (nle::call-provider-streaming (user-context))))
    (is (not (open-stream-p bad-stream)))))

;;; --- redacted thinking: capture and replay ---------------------------------

(deftest anthropic-redacted-thinking-is-captured-and-replayed (let ((tags '())))
  (with-mock-sse-stream
      ((anthropic-event "message_start" "message" (json-obj "id" "msg-r"))
       (anthropic-event "content_block_start" "index" 0
                        "content_block" (json-obj "type" "redacted_thinking"
                                                  "data" "opaque-blob"))
       (anthropic-event "content_block_start" "index" 1
                        "content_block" (json-obj "type" "text"))
       (anthropic-event "content_block_delta" "index" 1
                        "delta" (json-obj "type" "text_delta" "text" "visible"))
       (anthropic-event "message_delta"
                        "delta" (json-obj "stop_reason" "end_turn"))
       (anthropic-event "message_stop"))
    (let ((message (nle::call-anthropic-streaming
                    (anthropic-user-context)
                    :on-part (lambda (part)
                               (push part.kind tags)))))
      (let ((redacted (gethash "reasoning_redacted_data" message)))
        (is (and (vectorp redacted) (= 1 (length redacted))
                 (equal "opaque-blob" (aref redacted 0)))))
      (is (member :reasoning-start tags))
      (multiple-value-bind (role blocks)
          (nle::anthropic-message-blocks message t)
        (is (equal "assistant" role) "replay stays on the assistant message")
        (let ((first-block (first blocks)))
          (is (equal "redacted_thinking" (gethash "type" first-block)))
          (is (equal "opaque-blob" (gethash "data" first-block))))))))

;;; --- anthropic cache breakpoints -------------------------------------------

(deftest anthropic-cache-breakpoints-cover-tail-round-system-and-tools ()
  (nlk:bind ((assistant (wire-message "assistant" :content "checking"
                                      :tool-calls (vector (wire-tool-call
                                                           "call-1" "eval" "{}"))))
             (body (nle::anthropic-request-body
                    (anthropic-context
                     (list (nle::message "user" "add")
                           assistant
                           (nle::message "tool" "3"
                                                           :name "eval"
                                                           :tool-call-id "call-1")
                           (nle::message "user" "thanks"))))) (messages (gethash "messages" body))
             (system (gethash "system" body)) (tools (gethash "tools" body))
             (tail-blocks (gethash "content" (aref messages (1- (length messages))))) (total 0))
    (flet ((count-marks (blocks)
             (when (vectorp blocks)
               (loop for block across blocks
                     when (and (hash-table-p block)
                               (gethash "cache_control" block))
                       do (incf total)))))
      (count-marks system)
      (count-marks tools)
      (loop for message across messages
            do (count-marks (gethash "content" message))))
    (is (= 4 total) "exactly the four-breakpoint budget is spent")
    (is (hash-table-p (gethash "cache_control"
                               (aref tail-blocks (1- (length tail-blocks))))))
    (is (hash-table-p
         (gethash "cache_control"
                  (let ((first-user-blocks (gethash "content" (aref messages 0))))
                    (aref first-user-blocks (1- (length first-user-blocks)))))))
    (is (hash-table-p (gethash "cache_control"
                               (aref system (1- (length system))))))
    (is (hash-table-p (gethash "cache_control"
                               (aref tools (1- (length tools))))))))

(deftest anthropic-cache-policy-respects-explicit-markers-and-the-1h-beta ()
  ;; Explicit markers spend the budget first; the policy takes what is left.
  (let* ((tail-block (json-obj "type" "text" "text" "t"))
         (body (json-obj
                "system" (coerce (loop repeat 4
                                       collect (json-obj
                                                "type" "text" "text" "s"
                                                "cache_control"
                                                (json-obj "type" "ephemeral")))
                                 'vector)
                "messages" (vector (json-obj "role" "user"
                                             "content" (vector tail-block))))))
    (nle::apply-anthropic-cache-breakpoints body)
    (is (null (gethash "cache_control" tail-block))))
  ;; The one-hour lifetime stamps its ttl and demands its beta header.
  (let* ((nle::*anthropic-cache-breakpoints*
           '(:static "1h" :conversation nil))
         (system-block (json-obj "type" "text" "text" "s"))
         (body (json-obj "system" (vector system-block)
                         "messages" (vector)))
         (betas (nle::apply-anthropic-cache-breakpoints body)))
    (is (member "extended-cache-ttl-2025-04-11" betas :test #'equal))
    (is (equal "1h" (gethash "ttl" (gethash "cache_control" system-block))))))

;;; --- the turn.provider_request durable fact --------------------------------

(deftest provider-request-is-a-durable-fact (with-session-store ("request-fact"))
  (let ((request-json (sb-ext:string-to-octets "{\"model\":\"fact-model\",\"messages\":[]}")))
    (with-streaming-provider-stub
        (context on-part
         (nle::emit-stream-part on-part :stream-start)
         (nle::emit-stream-part on-part :text-delta :text "count")
         (nle::emit-stream-part on-part :text-delta :text "ed")
         (values (wire-message "assistant" :content "counted")
                 nil "stop" request-json))
      (nle:run "go" :session (nle:intern-session "request-fact")
               :command-id "request-fact-cmd"))
    (let ((facts (remove-if-not (lambda (event)
                                  (typep event 'nlk:provider-request-event))
                                (nle::collect-session-events "request-fact"))))
      (is (= 1 (length facts)) "one provider round left one request fact")
      (let ((fact (first facts)))
        (is (equal (nlk::sha256-text request-json)
                   (nlk:provider-request-event-body-sha256 fact)))
        (is (= 1 (nlk:provider-request-event-transport-attempts fact)))
        (is (= 0 (nlk:provider-request-event-provider-round fact)))
        (is (= 3 (nlk:provider-request-event-stream-part-count fact)))
        (is (= 2 (nlk:provider-request-event-text-delta-count fact)))
        (is (integerp (nlk:provider-request-event-ttft-ms fact)))
        (is (integerp (nlk:provider-request-event-stream-duration-ms fact)))
        (is (eq t (nlk:provider-request-event-parallel-tool-calls-p fact)))
        (is (not (nlk:provider-request-event-emitted-tool-calls-p fact)))
        (is (plusp (nlk:provider-request-event-provider-message-count fact)))
        (is (integerp (nlk:provider-request-event-estimated-context-tokens fact)))
        (is (equal "openai-completions"
                   (nlk:provider-request-event-provider fact)))))))

(deftest a-round-with-no-text-still-times-its-first-token
    (with-session-store ("silent-ttft"))
  ;; Nine rounds in ten stream thinking and a tool call and no text; the
  ;; first word is whatever comes first, timed from the request going out.
  (let ((request-json (sb-ext:string-to-octets "{\"model\":\"m\",\"messages\":[]}")))
    (with-streaming-provider-stub
        (context on-part
         (sleep 0.05)
         (nle::emit-stream-part on-part :stream-start)
         (nle::emit-stream-part on-part :reasoning-delta :text "thinking")
         (values (wire-message "assistant" :content "done") nil "stop" request-json))
      (nle:run "go" :session (nle:intern-session "silent-ttft")
               :command-id "silent-ttft-cmd"))
    (let ((fact (find-if (lambda (event) (typep event 'nlk:provider-request-event))
                         (nle::collect-session-events "silent-ttft"))))
      (is (= 0 (nlk:provider-request-event-text-delta-count fact)))
      ;; The wait before the stream is part of the first token's time.
      (is (<= 50 (nlk:provider-request-event-ttft-ms fact))))))

;;; --- pre-request cancellation ----------------------------------------------
;;; The one fold carries this alone now: a cancel that landed before the round
;;; opened must signal without a request ever leaving. CANCELLABLE-POST wraps
;;; the POST in NLK:WITH-CANCELLABLE-WAIT, whose ENTER-CANCELLABLE-WAIT signals
;;; for an already-cancelled turn before anything blocks — so the DEX:POST stub
;;; below must never fire, whatever observer the caller passed.

(deftest provider-call-refuses-to-run-after-cancellation (with-session-store ("cancel-block"))
  (with-durable-turn (turn "cancel-block" :command-id "cancel-block-cmd" :input "go" :current t)
    (let* ((posted nil))
      (nlk:request-cancel-turn "cancel-block"
                               turn.turn-id
                               "operator stop")
      (with-stubbed-fdefinition (dex:post (&rest args)
                                 (setf posted t)
                                 (values "{}" 200))
        (is (signals-error nlk:turn-cancelled-condition
              (nle::call-provider-streaming (user-context) :on-part nil))))
      (is (not posted) "no request left the process after cancellation"))))

;;; =========================================================================
;;; Tier-3 gates: credential chain
;;; =========================================================================

(defmacro with-temp-auth ((path-var json-text) &body body)
  "Run BODY with PATH-VAR naming a scratch auth.json holding JSON-TEXT,
removed on unwind. The credential chain takes the file by :AUTH-PATH, so
nothing global is bound here — a test that wants no auth file at all names
a path that does not exist instead."
  `(with-temp-file (,path-var :contents ,json-text)
     ,@body))

(deftest credential-chain-orders-config-auth-then-env ()
  (with-temp-auth (auth "{\"api_keys\":{\"anthropic\":{\"provider\":\"anthropic\",\"key\":\"from-auth\"}}}")
    ;; Config tier wins outright.
    (let ((nle::*api-key* "from-config"))
      (is (equal "from-config"
                 (nle::credential-key (nle::resolve-provider-credential
                                       "anthropic" :auth-path auth)))))
    ;; Auth file api_keys is the second tier.
    (let ((nle::*api-key* nil))
      (is (equal "from-auth"
                 (nle::credential-key (nle::resolve-provider-credential
                                       "anthropic" :auth-path auth)))))
  ;; Environment ladder: derived name first, family default second.
  (let ((nle::*api-key* nil))
    (with-stubbed-fdefinition (nle::credential-env (name)
                               (and (equal name "ANTHROPIC_API_KEY") "from-env"))
      (is (equal "from-env"
                 (nle::credential-key (nle::resolve-provider-credential
                                       "anthropic" :auth-path #p"/nonexistent/auth.json")))))
    (with-stubbed-fdefinition (nle::credential-env (name) nil)
      (is (eq :public
              (nle::credential-source (nle::resolve-provider-credential
                                       "anthropic" :auth-path #p"/nonexistent/auth.json"))))))))

(deftest credential-command-indirection-is-live-and-decisive ()
  (let ((nle::*api-key* "!printf secret-from-command"))
    (is (equal "secret-from-command"
               (nle::credential-key (nle::resolve-provider-credential
                                     "openai-completions" :auth-path #p"/nonexistent/auth.json")))))
  (let ((nle::*api-key* "!false"))
    (is (signals-error nle::credential-error
          (nle::resolve-provider-credential
           "openai-completions" :auth-path #p"/nonexistent/auth.json"))))
  (let ((nle::*api-key* "!true"))
    (is (signals-error nle::credential-error
          (nle::resolve-provider-credential
           "openai-completions" :auth-path #p"/nonexistent/auth.json")))))

(deftest corrupt-auth-file-never-falls-through (let ((nle::*api-key* nil)))
  ;; An ambiguous OAuth entry is the store-owning cell's rule now
  ;; (nodecode-codex-auth); a file the kernel cannot parse is still the kernel's.
  (with-temp-auth (auth "{not json")
    (is (signals-error nle::credential-error
          (nle::resolve-provider-credential
           "anthropic" :auth-path auth)))))

;;; --- configured providers: the shared-config tier ---------------------------
;;; The lisp mirror of the providers/root.zig config collapse over
;;; ~/.nodecode/config.jsonc: providers.<id>.sdk names the lane, base_url
;;; the endpoint base, api_key the config credential, the stream_* fields
;;; the per-turn retry tuning, and general.default_* the boot seed.

(defmacro with-temp-shared-config ((json-text &optional path-var) &body body)
  "Bind *SHARED-CONFIG-PATH* to a fixture file holding JSON-TEXT for BODY,
deleting it after. PATH-VAR, when given, is bound to that path for a test
that reads the bytes back rather than going through the reader."
  (let ((path (or path-var (gensym "PATH"))))
    `(with-temp-file (,path :contents ,json-text :type "jsonc")
       (let ((nle::*shared-config-path* ,path))
         ,@body))))

(deftest an-sdk-family-env-key-rides-only-to-its-vendors-own-endpoint ()
  ;; OPENAI_API_KEY is OpenAI's, ANTHROPIC_API_KEY Anthropic's: a keyless
  ;; provider of their family -- the build's OpenCode Zen lane, Nodecode's own
  ;; API, a configured one at another host -- never sends them. Each lane's
  ;; vendor still gets its own, and a probe, which composes no endpoint,
  ;; counts neither.
  (with-temp-shared-config ("{\"providers\": {\"elsewhere\": {\"sdk\": \"anthropic\", \"base_url\": \"https://api.elsewhere.test/anthropic\"}}}")
    (let ((nle::*api-key* nil) (nle::*endpoint* nil) (none #p"/nonexistent/auth.json"))
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (cdr (assoc name '(("OPENAI_API_KEY" . "sk-openai") ("ANTHROPIC_API_KEY" . "sk-ant"))
                                             :test #'equal)))
        (flet ((credential (provider &rest args)
                 (apply #'nle::resolve-provider-credential provider :auth-path none args)))
          (let ((zen (credential "openai-completions")))
            (is (eq :public (nle::credential-source zen)) "the keyless Zen lane stays keyless")
            (is (equal '(("authorization" . "Bearer public")) (nle::provider-auth-headers :openai-completions (nle::credential-key zen))) "and its round carries no OpenAI key"))
          (let ((nodecode (credential "openai-completions" :endpoint "https://api.nodecode.ai/v1/chat/completions"))
                (elsewhere (credential "elsewhere"))
                (openai-chat (credential "openai-completions" :endpoint "https://api.openai.com/v1/chat/completions"))
                (plain-http (credential "openai-completions" :endpoint "http://api.openai.com/v1/chat/completions")))
            (is (eq :public (nle::credential-source nodecode)) "nor does another chat-completions host")
            (is (eq :public (nle::credential-source elsewhere)) "nor does an anthropic-wire provider at another host")
            (is (equal "sk-openai" (nle::credential-key openai-chat)) "the chat wire at OpenAI's own host takes OPENAI_API_KEY")
            (is (eq :public (nle::credential-source plain-http)) "but never over another scheme"))
          (is (equal "sk-openai" (nle::credential-key (credential "openai-responses"))) "OpenAI's own lane takes it at api.openai.com")
          (is (equal "sk-ant" (nle::credential-key (credential "anthropic"))) "Anthropic's own provider takes ANTHROPIC_API_KEY")
          (is (eq :public (nle::credential-source (credential "openai-responses" :probe t))) "a probe composes no endpoint, so it counts no family default"))))))

(deftest configured-provider-sdk-names-the-lane-and-base-url-the-endpoint ()
  (with-temp-shared-config ("{
  // the Zig-shared operator config, comments and all
  \"providers\": {
    \"local-oai\": {\"sdk\": \"openai-responses\", \"base_url\": \"https://oai.example/v1\", \"api_key\": \"k1\"},
    \"local-ant\": {\"sdk\": \"anthropic\", \"base_url\": \"https://ant.example/v1/\", \"api_key\": \"k2\"},
    \"local-chat\": {\"sdk\": \"openai-completions\", \"base_url\": \"https://chat.example/v1\", \"api_key\": \"k3\"},
    \"local-goog\": {\"sdk\": \"google\", \"base_url\": \"https://goog.example/v1beta\", \"api_key\": \"k4\"},
    \"anthropic\": {},
  }
}")
    (dolist (case '(("local-oai" "openai-responses" "the openai-responses sdk routes to the openai-responses lane")
                    ("local-ant" "anthropic" "the anthropic sdk routes to the anthropic lane")
                    ("local-chat" "openai-completions" "the openai-completions sdk routes to the chat lane")
                    ("anthropic" "anthropic" "a bare config entry falls back to the id parse")))
      (destructuring-bind (provider lane why) case
        (is (equal lane (nle::provider-lane-name (nle::find-provider-lane provider))) why)))
    (is (signals-error nle::provider-error
          (nle::find-provider-lane "no-such-provider")))
    (let ((nle::*endpoint* nil))
      (dolist (case '(("local-ant" "https://ant.example/v1/messages"
                       "base_url is right-trimmed of slashes and gets the lane path")
                      ("local-oai" "https://oai.example/v1/responses" "the responses lane appends /responses")
                      ("local-chat" "https://chat.example/v1/chat/completions" "the chat lane appends /chat/completions")
                      ("local-goog" "https://goog.example/v1beta" "the google lane's configured endpoint IS the base")
                      ("anthropic" "https://api.anthropic.com/v1/messages" "no base_url means the lane default")))
        (destructuring-bind (provider endpoint why) case
          (let ((nle::*provider* provider))
            (is (equal endpoint (nle::effective-endpoint)) why)))))))

;;; --- lane resolution: the operator's declaration -----------------------------

(defparameter *lane-fixture-config* "{
  \"providers\": {
    \"chat\": {\"sdk\": \"openai-completions\", \"base_url\": \"https://chat.example/v1\", \"api_key\": \"k\",
             \"models\": [{\"id\": \"pinned\", \"sdk\": \"anthropic\"}, {\"id\": \"plain\"}]},
    \"single\": {\"sdk\": \"anthropic\", \"base_url\": \"https://single.example\", \"api_key\": \"k\"},
    \"open\": {\"base_url\": \"https://open.example/v1\", \"api_key\": \"k\"},
    \"typo\": {\"sdk\": \"openia\", \"base_url\": \"https://typo.example/v1\", \"api_key\": \"k\"}
  }
}"
  "A chat provider with one model pinned to another wire, an anthropic
provider, an entry naming no sdk, and one misspelling it.")

(defun lane-refusal (provider)
  "The detail FIND-PROVIDER-LANE refuses PROVIDER with, or \"\" when it
does not refuse."
  (handler-case (progn (nle::find-provider-lane provider) "")
    (nle::provider-error (e) (nle::provider-error-detail e))))

(deftest the-configured-sdk-is-the-lane (with-temp-shared-config (*lane-fixture-config*))
  ;; Which wire a model rides is the
  ;; operator's declaration — providers.<id>.sdk, or a models[].sdk pin —
  ;; and an entry naming none is refused by name, never tried on every wire.
  (let ((nle::*models-catalog-path* nil))
    (is (equal "openai-completions" (nle::configured-provider-lane "chat")))
    (is (equal "anthropic" (nle::configured-provider-lane "single")))
    (is (equal "anthropic" (nle::configured-provider-lane "anthropic")))
    (is (null (nle::configured-provider-lane "nowhere")))
    (is (null (nle::configured-provider-lane "open")))
    (is (search "providers.open.sdk" (lane-refusal "open")))
    (is (search "openai-completions" (lane-refusal "open")))
    (is (null (nle::configured-provider-lane "typo")))
    (is (search "unknown sdk \"openia\"" (lane-refusal "typo")))
    (is (search "providers.open.sdk" (nle::selection-refusal "open")))
    (is (null (nle::selection-refusal "chat")))
    (is (equal "anthropic" (nle::resolve-model-lane "chat" "pinned")))
    (is (equal "openai-completions" (nle::resolve-model-lane "chat" "plain")))
    (is (null (nle::resolve-model-lane "open" "plain")))
    ;; A selection is refused before anything moves — and the refusal
    ;; prints as its detail alone, since the pair it would otherwise be
    ;; attributed to is whatever happened to be selected.
    (let ((nle::*provider* "single") (nle::*model* "x") (nle::*endpoint* nil))
      (let ((refusal (handler-case
                         (progn (nle::select-default-model :provider "open" :model "plain")
                                nil)
                       (nle::provider-error (e) e))))
        (is (search "providers.open.sdk" (nle::provider-error-detail refusal)))
        (is (equal (nle::provider-error-detail refusal) (princ-to-string refusal))))
      (is (equal "single" nle::*provider*) "and the selection did not move")
      (is (equal "x" nle::*model*) "nor the model"))
    ;; A model picked under the sdk-less provider already selected is
    ;; refused the same way, by the provider's name, not left to fail at
    ;; the freeze.
    (let ((nle::*provider* "open") (nle::*model* "x") (nle::*endpoint* nil))
      (is (search "providers.open.sdk"
                  (handler-case (progn (nle::select-default-model :model "plain") "")
                    (nle::provider-error (e) (nle::provider-error-detail e)))))
      (is (equal "x" nle::*model*) "and moves nothing"))))

(deftest effective-endpoint-composes-the-declared-lane ()
  (with-temp-shared-config (*lane-fixture-config*)
    (let ((nle::*models-catalog-path* nil)
          (nle::*endpoint* nil)
          (nle::*provider* "chat")
          (nle::*model* "plain"))
      (is (equal "https://chat.example/v1/chat/completions"
                 (nle::effective-endpoint)))
      (let ((config (nle::snapshot-effective-provider-config)))
        (is (equal "openai-completions" config.lane))
        (is (equal "https://chat.example/v1/chat/completions" config.endpoint)))
      (let ((nle::*model* "pinned"))
        (is (equal "https://chat.example/v1/messages" (nle::effective-endpoint)))
        (is (equal "anthropic"
                   (nle::effective-provider-config-lane
                    (nle::snapshot-effective-provider-config))))))))

(defun pinned-default (provider model)
  "The config text whose general.default_model pins PROVIDER and MODEL."
  (format nil "{\"general\": {\"default_model\": {\"provider\": ~s, \"model\": ~s}}}"
          provider model))

(deftest an-unknown-provider-id-is-refused-and-never-reaches-zen (with-catalog-fixture (catalog))
  ;; vr-101: a pin naming a provider nothing knows ("nosuchprov", the typo
  ;; "anthropc") was skipped at the seed, so the build's keyless default lane
  ;; carried the pin's model to OpenCode Zen as "public" on every turn. The
  ;; id is seeded as written and refused by name at every resolution; no
  ;; endpoint and no credential is resolved for it.
  (with-saved-globals (nle::*auxiliary-model* nle::*model-config-refusal*)
    (with-stubbed-fdefinition (nle::credential-env (name) nil)
      (with-temp-shared-config ((pinned-default "nosuchprov" "some-model"))
        (let ((nle::*provider* "openai-completions") (nle::*model* "before")
              (nle::*endpoint* nil) (nle::*api-key* nil))
          (nle::seed-model-config-from-shared-config)
          (is (equal "nosuchprov" nle::*provider*) "the pin is seeded as written")
          (is (equal "some-model" nle::*model*))
          (let ((refusal (nle::provider-error-detail
                          (signals-error nle::provider-config-error
                            (nle::snapshot-effective-provider-config)))))
            (is (search "provider nosuchprov is not one this box knows" refusal))
            (is (search "/setup" refusal) "and says what to do")
            ;; the words every shell shows at boot
            (is (equal refusal (nle::selection-refusal "nosuchprov"))))
          (signals-error nle::provider-config-error (nle::effective-endpoint))
          (signals-error nle::provider-config-error
            (nle::resolve-provider-credential "nosuchprov"
                                              :auth-path #p"/nonexistent/auth.json"))))
      ;; No catalog to ask: a catalog provider cannot be told from a typo yet,
      ;; and is refused, not carried to Zen, until the list lands.
      (with-temp-shared-config ((pinned-default "acme" "acme-large"))
        (let ((nle::*models-catalog-path* nil) (nle::*models-catalog-memo* nil)
              (nle::*provider* "openai-completions") (nle::*model* "before")
              (nle::*endpoint* nil) (nle::*api-key* nil))
          (nle::seed-model-config-from-shared-config)
          (is (search "has not loaded yet"
                      (nle::provider-error-detail
                       (signals-error nle::provider-config-error
                         (nle::snapshot-effective-provider-config)))))))
      (let ((nle::*api-key* nil) (nle::*endpoint* nil)
            (nle::*auth-file-path* #p"/nonexistent/auth.json"))
        ;; A known provider with no key rides its own endpoint, never Zen's.
        (dolist (case '(("acme" "acme-large" "https://api.acme.dev/v1/chat/completions")
                        ("anthropic" "claude-x" "https://api.anthropic.com/v1/messages")))
          (destructuring-bind (provider model endpoint) case
            (with-temp-shared-config ((pinned-default provider model))
              (let ((nle::*provider* "openai-completions") (nle::*model* "before"))
                (nle::seed-model-config-from-shared-config)
                (is (equal endpoint (nle::effective-provider-config-endpoint
                                     (nle::snapshot-effective-provider-config)))
                    provider)))))
        ;; The build's own keyless default is the one target Zen and "public" serve.
        (with-temp-shared-config ("{}")
          (let* ((nle::*provider* "openai-completions") (nle::*model* "deepseek-v4-flash-free")
                 (config (nle::snapshot-effective-provider-config)))
            (is (equal "https://opencode.ai/zen/v1/chat/completions" config.endpoint))
            (is (equal "public" config.api-key))))))))

(defparameter *no-endpoint-catalog-json*
  "{\"openai\": {\"npm\": \"@ai-sdk/openai\", \"models\": {}},
    \"firstparty\": {\"npm\": \"@ai-sdk/openai\", \"models\": {}},
    \"groqish\": {\"npm\": \"@ai-sdk/groq\", \"models\": {}}}"
  "models.dev's openai on the Responses package with no api base; another
provider on that package, and one on a package no lane rides, with none
either.")

(defparameter *no-endpoint-config* "{
  \"providers\": {
    \"acme\": {\"sdk\": \"openai-completions\", \"api_key\": \"acme-secret-key\"},
    \"groqish\": {\"sdk\": \"openai-completions\", \"api_key\": \"groq-secret-key\"},
    \"firstparty\": {\"sdk\": \"openai-responses\", \"api_key\": \"k\"},
    \"openai\": {\"sdk\": \"openai-responses\", \"api_key\": \"k\"},
    \"anthropic\": {\"sdk\": \"anthropic\", \"api_key\": \"k\",
                  \"models\": [{\"id\": \"chatty\", \"sdk\": \"openai-completions\"}]}
  }
}"
  "Entries with a key and no base_url on a lane that is not theirs, two of
them catalog ids; openai on the Responses lane, its family's; and a lane's
own provider with one model pinned to another lane.")

(deftest a-provider-with-no-endpoint-of-its-own-is-refused-and-never-reaches-zen
    (with-catalog-fixture (catalog *no-endpoint-catalog-json*))
  ;; Without this, a provider with no base_url would ride the chat lane's
  ;; default with its own key. A lane's default is the address
  ;; of the lane's own provider, by name or family; any other provider is
  ;; refused by name, at the freeze, at the pick and in the words every
  ;; shell shows.
  (with-temp-shared-config (*no-endpoint-config*)
    (let ((nle::*endpoint* nil) (nle::*api-key* nil) (nle::*model* "m"))
      (dolist (provider '("acme" "groqish" "firstparty"))
        (let ((refusal (nle::selection-refusal provider)))
          (is (search (format nil "providers.~a has no base_url, so its requests have nowhere to go" provider)
                      refusal)
              provider)
          (is (search (format nil "set providers.~a.base_url in" provider) refusal) "and names the key to set"))
        ;; the turn is refused in the same words, before any dial
        (let ((nle::*provider* provider))
          (is (equal (nle::selection-refusal provider)
                     (nle::provider-error-detail
                      (signals-error nle::provider-config-error
                        (nle::snapshot-effective-provider-config)))))
          (signals-error nle::provider-config-error (nle::effective-endpoint))))
      ;; openai keeps its family's address, as does the lane's own provider --
      ;; but not on a lane that is not its own
      (let ((nle::*provider* "openai"))
        (is (null (nle::selection-refusal "openai")))
        (is (equal "https://api.openai.com/v1/responses" (nle::effective-endpoint))))
      (let ((nle::*provider* "anthropic"))
        (is (equal "https://api.anthropic.com/v1/messages" (nle::effective-endpoint)))
        (let ((nle::*model* "chatty"))
          (signals-error nle::provider-config-error (nle::effective-endpoint))))
      (let ((nle::*provider* "anthropic") (nle::*model* "x"))
        (let ((refusal (handler-case (progn (nle::select-default-model :provider "acme" :model "m") nil)
                         (nle::provider-config-error (e) (nle::provider-error-detail e)))))
          (is (search "providers.acme has no base_url" refusal) "a pick is refused")
          (is (equal "anthropic" nle::*provider*) "and moves nothing")))
      ;; The explicit override is every turn's address while it stands.
      (let ((nle::*endpoint* "http://127.0.0.1:9/v1/chat/completions") (nle::*provider* "acme"))
        (is (null (nle::selection-refusal "acme")))
        (is (equal nle::*endpoint* (nle::effective-endpoint))))
      ;; No models.dev list: openai rides its address still, and a refusal
      ;; says the list that could name one has not loaded.
      (let ((nle::*models-catalog-path* nil) (nle::*models-catalog-memo* nil))
        (let ((nle::*provider* "openai"))
          (is (null (nle::selection-refusal "openai")))
          (is (equal "https://api.openai.com/v1/responses" (nle::effective-endpoint))))
        (let ((refusal (nle::selection-refusal "acme")))
          (is (search "providers.acme has no base_url, and the models.dev list that could name its address has not loaded yet"
                      refusal))
          (is (search "or try again in a moment" refusal))))))
  ;; the refusal names the config as the operator reads their own paths
  (let ((nle::*shared-config-path* (merge-pathnames ".nodecode/config.jsonc" (user-homedir-pathname))))
    (is (equal "~/.nodecode/config.jsonc" (nle::shared-config-where)))))

(deftest an-unknown-provider-offline-says-models-dev-could-not-be-reached ()
  ;; Offline the list never loads, and `try again in a moment' was said to
  ;; every turn of a pinned deepseek whose key was saved (xm-204): a fetch
  ;; that failed is named, with the way on that needs no list, and so it is
  ;; while a retry runs: the first turn after each retry window said `in a
  ;; moment' again. Before any fetch has ended, a moment is the truth.
  (with-temp-shared-config ((pinned-default "deepseek" "deepseek-chat"))
    (let* ((nle::*models-catalog-path* nil) (nle::*models-catalog-memo* nil)
           (thread (bt2:make-thread (lambda () "host not found")))
           (hold (bt2:make-semaphore))
           (retry (bt2:make-thread (lambda () (bt2:wait-on-semaphore hold) nil))))
      (bt2:join-thread thread)
      (let ((nle::*models-catalog-fetch* (list thread (get-universal-time))))
        (is-carrying (text (nle::selection-refusal "deepseek"))
          "the models.dev list that would name it could not be fetched (host not found)"
          "try again once models.dev answers" "providers.deepseek with an sdk and a base_url"
          (:absent "in a moment")))
      (let ((nle::*models-catalog-fetch* (list* retry (get-universal-time) "host not found")))
        ;; a retry in flight still names the failure before it
        (is (search "could not be fetched (host not found)" (nle::selection-refusal "deepseek")))
        (bt2:signal-semaphore hold)
        (bt2:join-thread retry)
        (is (null (nle::models-catalog-fetch-failure)) "and a retry that landed clears it"))
      (let ((nle::*models-catalog-fetch* nil))
        (is (search "has not loaded yet" (nle::selection-refusal "deepseek")))))))

(deftest a-tool-call-with-no-name-fails-the-attempt ()
  ;; A call with no name is not a call to dispatch; it is a contract failure
  ;; like an empty message, retried on the stream budget, and the failure
  ;; names the call. Without this, it would be dispatched and fail as
  ;; `no tool named "" is registered`.
  (with-temp-shared-config (*lane-fixture-config*)
    (let ((nle::*models-catalog-path* nil)
          (nle::*endpoint* nil)
          (nle::*provider* "single")
          (nle::*model* "plain")
          (nle::*retry-base-delay* 0)
          (nle::*max-stream-retries* 1)
          (calls 0))
      (with-stubbed-fdefinition (nle::call-anthropic-streaming (context &key on-part)
                                 (incf calls)
                                 (values (wire-message
                                          "assistant"
                                          :tool-calls (vector
                                                       (wire-tool-call "tool-0" "eval" "{}")
                                                       (wire-tool-call "tool-1" ""
                                                                       "{\"arguments\":\"{\\\"form\\\": \"}")))
                                         nil "tool_calls" "{}"))
        (let ((condition (signals-error nle::provider-malformed-tool-call
                           (nle::call-provider
                            (compiled-context (list (nle::message "user" "use it now")))))))
          (is (= 2 calls) "the budget allowed one retry, on the same declared lane")
          (when condition
            (is (search "tool call with no name (id tool-1"
                        (nle::provider-error-detail condition)))))))))

(deftest configured-provider-api-key-is-the-config-tier (with-temp-shared-config ("{\"providers\": {
    \"local-chat\": {\"sdk\": \"openai-completions\", \"base_url\": \"https://chat.example/v1\", \"api_key\": \"config-key\"},
    \"local-cmd\": {\"sdk\": \"openai-completions\", \"base_url\": \"https://chat.example/v1\", \"api_key\": \"!printf cmd-key\"}}}"))
  (let ((nle::*api-key* nil))
    (is (equal "config-key"
               (nle::credential-key (nle::resolve-provider-credential
                                     "local-chat" :auth-path #p"/nonexistent/auth.json"))))
    (is (equal "cmd-key"
               (nle::credential-key (nle::resolve-provider-credential
                                     "local-cmd" :auth-path #p"/nonexistent/auth.json"))))
    (is (eq :config
            (nle::provider-auth-state
             "local-chat" :auth-path #p"/nonexistent/auth.json"))))
  (let ((nle::*api-key* "process-key"))
    (is (equal "process-key"
               (nle::credential-key (nle::resolve-provider-credential
                                     "local-chat" :auth-path #p"/nonexistent/auth.json"))))))

(deftest configured-provider-inventory-collects-credentialed-ids-and-models ()
  (with-temp-shared-config ("{
  \"providers\": {
    \"zeta\": {\"sdk\": \"openai-responses\", \"api_key\": \"k\", \"models\": [{\"id\": \"m-2\", \"context_window\": 128000}, {\"id\": \"m-1\"}]},
    \"keyless\": {\"sdk\": \"anthropic\", \"models\": [{\"id\": \"never-listed\"}]}
  }
}")
    (with-temp-auth (auth "{\"api_keys\":{\"alpha\":{\"provider\":\"alpha\",\"key\":\"a\"}},\"oauth_tokens\":{\"beta:profile\":{\"access_token\":\"t\"}}}")
      (multiple-value-bind (ids models)
          (nle::configured-provider-inventory :auth-path auth)
        (is (equal '("alpha" "beta" "zeta") ids))
        (is (= 2 (length models)))
        (let ((m2 (find "m-2" models
                        :key (lambda (m) (getf m :model-id)) :test #'equal))
              (m1 (find "m-1" models
                        :key (lambda (m) (getf m :model-id)) :test #'equal)))
          (is (and m2 (eql 128000 (getf m2 :context-window))))
          (is (and m1 (null (getf m1 :context-window)))))))))

(deftest configured-context-window-is-the-operator-budget ()
  ;; Without an explicit context_window, the eviction trigger would never arm
  ;; and input tokens per call would grow without bound.
  ;; providers.<id>.models[].context_window is the operator's own window: the
  ;; capability seam reads it as explicit configuration, and the learned-window
  ;; belief returns an operator window verbatim.
  (let ((nle::*model-capability-overrides* '())
        (nle::*context-tokens-env-override* nil)
        (nle::*model-capability-memo* nil)
        (nle::*shared-config-memo* nil)
        (nle::*provider* "relay"))
    (with-temp-shared-config ("{
  \"providers\": {
    \"relay\": {\"sdk\": \"openai-responses\", \"api_key\": \"k\",
               \"models\": [{\"id\": \"big-model\", \"context_window\": 205882},
                            {\"id\": \"plain-model\"},
                            {\"id\": \"bad-model\", \"context_window\": \"lots\"}]}
  }
}")
      (is (eql 205882 (nle::model-context-tokens "big-model")))
      (is (nle::model-context-tokens-operator-p "big-model"))
      (is (eql 205882 (nle::context-window-belief
                       "relay" "big-model"
                       (nle::model-context-tokens "big-model")
                       (nle::model-context-tokens-operator-p "big-model"))))
      (is (null (nle::model-context-tokens "plain-model")))
      (let ((warned nil))
        (handler-bind ((warning (lambda (w)
                                  (setf warned (princ-to-string w))
                                  (muffle-warning w))))
          (is (null (nle::model-context-tokens "bad-model"))))
        (is (and warned (search "bad-model" warned))))
      (is (null (nle::model-capability-context-tokens
                 (nle::resolve-model-capability "big-model" "elsewhere"))))
      (let ((nle::*context-tokens-env-override* 100000))
        (is (eql 100000 (nle::model-context-tokens "big-model")))))
    (with-temp-shared-config ("{\"providers\": {\"relay\": {\"sdk\": \"openai-responses\", \"api_key\": \"k\",
  \"models\": [{\"id\": \"big-model\", \"context_window\": 99000}]}}}")
      (is (eql 99000 (nle::model-context-tokens "big-model"))))))

;;; --- provider model listings -----------------------------------------------
;;; A provider is {base_url, sdk, api_key}; GET <base_url>/models is where
;;; its models come from. The parse is pure; the cache file is the one
;;; authority both shells read; a failed fetch is a recorded fact.

(deftest provider-listing-parses-every-lane-shape ()
  (multiple-value-bind (rows error)
      (nle::parse-provider-listing
       "{\"object\":\"list\",\"data\":[{\"id\":\"zeta-2\",\"object\":\"model\"},{\"id\":\"alpha\",\"display_name\":\"Alpha\",\"max_model_len\":262144},{\"id\":\"zeta-2\"},{\"id\":\"  \"}]}")
    (is (null error) "an openai-shaped data[] listing parses")
    (is (equal '("alpha" "zeta-2") (mapcar (lambda (row) (getf row :id)) rows)))
    (is (equal "Alpha" (getf (first rows) :display)))
    (is (eql 262144 (getf (first rows) :context-window)))
    (is (null (getf (second rows) :context-window))))
  (multiple-value-bind (rows error)
      (nle::parse-provider-listing
       "{\"models\":[{\"name\":\"models/gemini-x\",\"displayName\":\"Gemini X\",\"inputTokenLimit\":1048576}]}")
    (is (null error) "a google-shaped models[] listing parses")
    (is (equal "gemini-x" (getf (first rows) :id)))
    (is (equal "Gemini X" (getf (first rows) :display)) "displayName rides too")
    (is (eql 1048576 (getf (first rows) :context-window))))
  (multiple-value-bind (rows error) (nle::parse-provider-listing "not json")
    (is (null rows) "garbage yields no rows")
    (is (equal "unparseable listing" error) "and says so"))
  (multiple-value-bind (rows error) (nle::parse-provider-listing "{\"ok\":true}")
    (is (null rows) "an object without data[]/models[] yields no rows")
    (is (search "neither" error) "and names the missing shape")))

(deftest provider-auth-headers-follow-the-lane-family ()
  (is (equal '(("x-api-key" . "k") ("anthropic-version" . "2023-06-01"))
             (nle::provider-auth-headers :anthropic "k")))
  (is (equal '(("x-goog-api-key" . "k")) (nle::provider-auth-headers :google "k")))
  (is (equal '(("authorization" . "Bearer k"))
             (nle::provider-auth-headers :openai-completions "k"))))

(deftest provider-listing-sends-the-key-alone-as-its-bearer ()
  ;; The credential the chain resolves is a plist; a listing that sent the
  ;; whole plist would get a 401 from every provider that checks the key.
  (with-temp-shared-config ("{
  \"providers\": {
    \"keyed\": {\"sdk\": \"openai-completions\", \"base_url\": \"https://keyed.example/v1\", \"api_key\": \"sk-listing\"}
  }
}")
    (let ((sent nil))
      (with-stubbed-fdefinition (nle::http-fetch (url &rest args)
                                 (setf sent (getf args :headers))
                                 (values "{\"data\":[{\"id\":\"m-1\"}]}" 200))
        (nle::list-provider-models "keyed")
        (is (equal "Bearer sk-listing" (cdr (assoc "authorization" sent :test #'equal))))))))

(deftest listing-urls-add-a-v1-retry-for-a-bare-host ()
  (is (equal '("https://a6api.com/v1/models") (nle::listing-urls "https://a6api.com/v1/")))
  (is (equal '("https://g.example/v1beta/models") (nle::listing-urls "https://g.example/v1beta")))
  (is (equal '("https://api-slb.muskapi.cc/models" "https://api-slb.muskapi.cc/v1/models")
             (nle::listing-urls "https://api-slb.muskapi.cc"))))

(deftest provider-key-check-reads-each-answer ()
  ;; Google answers a key it does not know with a 400 whose ErrorInfo names
  ;; API_KEY_INVALID, not a 401. Without this, /setup would save a mistyped key
  ;; as connected: the reason keeps the code the body names, and that code is
  ;; a verdict; a bare 400 is not. Nor is a 5xx or a 404, which would read
  ;; `connected': no verdict either way.
  (with-temp-file (nle::*provider-models-cache-path*)
    (is-table (body status expected)
        (equal expected
               (with-stubbed-fdefinition (nlk:http (method url &rest args) (values body status))
                 (multiple-value-bind (verdict words) (nle::provider-key-check "google" "k")
                   (list verdict (and words (subseq words (or (search "refused" words) 0)))))))
        ("{\"error\":{\"code\":400,\"status\":\"INVALID_ARGUMENT\",\"details\":[{\"reason\":\"API_KEY_INVALID\"}]}}"
         400 '(:refused "refused this key (HTTP 400 API_KEY_INVALID)"))
        ("" 400 '(:unchecked "generativelanguage.googleapis.com:443 answered HTTP 400"))
        ("{\"error\":{\"type\":\"authentication_error\"}}"
         401 '(:refused "refused this key (HTTP 401 authentication_error)"))
        ("{\"error\":{\"message\":\"down\"}}" 500 '(:unchecked "generativelanguage.googleapis.com:443 answered HTTP 500"))
        ("" 404 '(:unchecked "generativelanguage.googleapis.com:443 answered HTTP 404"))
        ("<html>sign in</html>" 200 '(:unchecked "generativelanguage.googleapis.com:443 answered with no model list"))
        ("{\"models\":[{\"name\":\"models/g-1\"}]}" 200 '(:works nil)))
    ;; a failure below HTTP names the address it met, not only its fact
    (with-stubbed-fdefinition (nlk:http (method url &rest args) (error 'usocket:ns-host-not-found-error))
      (is (equal '(:unchecked "host not found at generativelanguage.googleapis.com:443")
                 (multiple-value-list (nle::provider-key-check "google" "k")))))))

(deftest provider-key-check-bounds-a-check-nothing-answers
    (let ((nle::*provider-models-fetch-timeout-seconds* 2) (start (get-internal-real-time))))
  ;; A listing that takes the connection and never answers held `checking...'
  ;; for the fetch timeout on each URL a bare host tries, then read `saved' and
  ;; connected: one bound over the whole check, said as it is.
  (with-temp-shared-config ("{\"providers\": {\"bare\": {\"sdk\": \"openai-completions\", \"base_url\": \"http://127.0.0.1:9\"}}}")
    (with-temp-file (nle::*provider-models-cache-path*)
      (with-stubbed-fdefinition (nlk:http (method url &rest args) (sleep 30))
        (multiple-value-bind (verdict words) (nle::provider-key-check "bare" "k")
          (is (eq :unchecked verdict))
          (is (equal "no answer from 127.0.0.1:9 in 2 s" words))
          ;; one bound over both URLs: the two bounds of old took 4 s, and a
          ;; loaded machine keeps well inside the margin
          (is (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 3.5)))))))

(deftest provider-key-check-reads-a-port-nothing-listens-on
    (let ((port (closed-loopback-port)) (start (get-internal-real-time))
          (nle::*provider-models-fetch-timeout-seconds* 10)))
  ;; Without this, a key saved for a stopped local server reads `saved' and
  ;; connects, and the first turn then waits ten minutes: the check hears the
  ;; refusal at once and it is a verdict on the address, so nothing is saved.
  (with-temp-shared-config ((format nil "{\"providers\": {\"local\": {\"sdk\": \"openai-completions\", ~
                                          \"base_url\": \"http://127.0.0.1:~d/v1\"}}}" port))
    (with-temp-file (nle::*provider-models-cache-path*)
      (multiple-value-bind (verdict words) (nle::provider-key-check "local" "k")
        (is (eq :unreachable verdict))
        (is (equal (format nil "local could not check the key: nothing listens at 127.0.0.1:~d" port)
                   words))
        (is (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 2) "at once, not the fetch timeout")))))

(deftest provider-key-check-sends-no-key-where-a-turn-would-not-go ()
  ;; The key check for providers.acme {sdk: openai-completions} with no
  ;; base_url asks where a turn would dial. A provider with no address of its
  ;; own has none. Without this refusal, the check would borrow the chat
  ;; lane's default and send the key to that endpoint, so it says no base_url.
  (with-temp-shared-config ("{\"providers\": {\"acme\": {\"sdk\": \"openai-completions\"}}}")
    (with-temp-file (nle::*provider-models-cache-path*)
      (let ((urls '()))
        (with-stubbed-fdefinition (nlk:http (method url &rest args) (push url urls) (values "{}" 200))
          (multiple-value-bind (verdict words) (nle::provider-key-check "acme" "acme-secret-key")
            (is (eq :unchecked verdict))
            (is (search "providers.acme has no base_url" words) "the words are the refusal's")))
        (is (null urls) "nothing is fetched")))))

(deftest provider-listing-refuses-without-a-base-url-or-lane (with-temp-shared-config ("{
  \"providers\": {
    \"catalogued\": {\"sdk\": \"anthropic\", \"api_key\": \"k\"},
    \"odd\": {\"sdk\": \"mystery\", \"base_url\": \"http://127.0.0.1:1/v1\"}
  }
}"))
  (multiple-value-bind (rows error) (nle::list-provider-models "catalogued")
    (is (null rows) "no base_url means nothing to ask")
    (is (equal "no base_url configured" error) "and the reason says so"))
  (multiple-value-bind (rows error) (nle::list-provider-models "odd")
    (is (null rows) "an unknown sdk has no lane to list through")
    (is (search "unknown sdk \"mystery\"" error)))
  (is (equal '("odd") (nle::listable-provider-ids))))

(deftest an-auth-credentialed-catalog-provider-lists-from-the-catalog-base ()
  ;; The base a turn dials and the base a listing asks are one rule.
  (with-catalog-fixture (catalog)
    (with-temp-auth (auth "{\"api_keys\":{\"acme\":{\"provider\":\"acme\",\"key\":\"k\"},\"exotic\":{\"provider\":\"exotic\",\"key\":\"k\"}}}")
      (with-temp-shared-config ("{
  \"providers\": {
    \"odd\": {\"sdk\": \"openai-completions\", \"base_url\": \"http://127.0.0.1:1/v1\"},
    \"pinned\": {\"sdk\": \"openai-completions\", \"base_url\": \"http://127.0.0.1:2/v1\", \"api_key\": \"k\"}
  }
}")
        (let ((nle::*auth-file-path* auth)
              (nle::*provider-models-memo* nil))
          (is (equal "https://api.acme.dev/v1" (nle::provider-base-url "acme")) "the catalog's api base is the credentialed provider's base")
          (is (equal "http://127.0.0.1:2/v1" (nle::provider-base-url "pinned")) "a configured base_url outranks the catalog")
          (is (null (nle::provider-base-url "exotic")) "a catalog entry naming no api base names nothing")
          (is (equal '("acme" "odd" "pinned") (nle::listable-provider-ids)) "the credentialed catalog provider lists; the api-less one does not")
          (with-temp-file (nle::*provider-models-cache-path*
                           :contents (format nil "{\"acme\": {\"fetched_at\": ~d, \"base_url\": \"https://api.acme.dev/v1\", \"error\": null, \"models\": [{\"id\": \"acme-large:free\"}]}}"
                                             (get-universal-time)))
            (let ((entry (nle::provider-models-cache-entry "acme")))
              (is-present entry "the listing cached against the catalog base reads back"
                (is (getf entry :fresh-p))
                (is (equal '("acme-large:free") (mapcar (lambda (row) (getf row :id)) (getf entry :models))) "the rows the endpoint listed and the catalog never carried")))
            (is (equal '("odd" "pinned") (nle::stale-listable-provider-ids)) "a fresh listing is not refetched; the never-listed config providers are")))))))

(deftest provider-models-cache-entry-reads-fresh-stale-and-moved (with-temp-shared-config ("{
  \"providers\": {
    \"local\": {\"sdk\": \"openai-completions\", \"base_url\": \"http://127.0.0.1:1/v1\"},
    \"moved\": {\"sdk\": \"openai-completions\", \"base_url\": \"http://127.0.0.1:2/v1\"}
  }
}"))
  (let ((now (get-universal-time)))
    (with-temp-file (nle::*provider-models-cache-path*
                     :contents (format nil "{
  \"local\": {\"fetched_at\": ~d, \"base_url\": \"http://127.0.0.1:1/v1\", \"error\": null,
              \"models\": [{\"id\": \"q-5\", \"context_window\": 32768}, {\"id\": \"q-8\", \"display\": \"Q8\"}]},
  \"moved\": {\"fetched_at\": ~d, \"base_url\": \"http://elsewhere/v1\", \"error\": null, \"models\": [{\"id\": \"gone\"}]},
  \"unconfigured\": {\"fetched_at\": ~d, \"base_url\": \"x\", \"error\": \"HTTP 401\", \"models\": []}
}" now now now))
      (let ((nle::*provider-models-memo* nil))
        (let ((entry (nle::provider-models-cache-entry "local")))
          (is-present entry "the configured provider's entry reads"
            (is (getf entry :fresh-p) "a just-fetched entry is fresh")
            (is (null (getf entry :error)) "null error reads as none")
            (is (equal '("q-5" "q-8")
                       (mapcar (lambda (row) (getf row :id)) (getf entry :models))))
            (is (eql 32768 (getf (first (getf entry :models)) :context-window)))
            (is (equal "Q8" (getf (second (getf entry :models)) :display)))))
        (is (null (nle::provider-models-cache-entry "moved")))
        (is (null (nle::provider-models-cache-entry "unconfigured")))
        (is (equal '("moved") (nle::stale-listable-provider-ids)))
        (let ((nle::*provider-models-ttl-seconds* 0))
          (let ((entry (nle::provider-models-cache-entry "local")))
            (is (and entry (not (getf entry :fresh-p)))))
          (is (equal '("local" "moved") (nle::stale-listable-provider-ids))))))))

(deftest provider-models-refresh-records-failures-in-one-write ()
  ;; SETF: the fetch runs on worker threads, which never see a LET binding
  ;; of the shared config path.
  (with-temp-file (nle::*shared-config-path*
                   :type "jsonc" :setf t
                   :contents "{
  \"providers\": {
    \"down\": {\"sdk\": \"openai-completions\", \"base_url\": \"http://127.0.0.1:1/v1\", \"api_key\": \"k\"}
  }
}")
    (with-temp-file (nle::*provider-models-cache-path*)
      (let ((nle::*provider-models-memo* nil)
            (nle::*provider-models-fetch-timeout-seconds* 2))
        (is (null (nle::provider-models-cache)) "no cache file before the first refresh")
        (is (equal '("down") (nle::refresh-provider-models '("down"))))
        (let ((entry (nle::provider-models-cache-entry "down")))
          (is-present entry "a failed fetch is still a cache entry"
            (is (stringp (getf entry :error)))
            (is (null (getf entry :models)) "with no rows")
            (is (getf entry :fresh-p))))
        (is (equal '() (nle::stale-listable-provider-ids)))
        (nle::drop-provider-models-cache '("down"))
        (is (null (nle::provider-models-cache-entry "down")))
        (is (equal '("down") (nle::stale-listable-provider-ids))))))
  (let ((nle::*provider-models-cache-path* nil))
    (is (null (nle::refresh-provider-models '("down"))))))

(deftest stream-cut-detail-never-prints-the-stream ()
  ;; The web page's cut-off note said `stream read failed mid-body:
  ;; END-OF-FILE: end of file on #<SB-SYS:FD-STREAM for "socket ...">'
  ;; the read error's printed self, socket object and all.
  (with-input-from-string (stream "")
    (let ((eof (make-condition 'end-of-file :stream stream)))
      (is (equal "the provider closed the connection mid-answer"
                 (nle::stream-cut-detail eof)))))
  (is (equal "the connection broke mid-answer (connection reset)"
             (nle::stream-cut-detail (make-condition 'cl+ssl::ssl-error-syscall))))
  (let ((detail (nle::stream-cut-detail
                 (make-condition 'sb-int:simple-stream-error
                                 :stream *standard-input*
                                 :format-control "Couldn't read from ~s"
                                 :format-arguments (list *standard-input*)))))
    (is (equal "the connection broke mid-answer (simple-stream-error)" detail))
    (is (not (search "#<" detail)))))

(deftest transport-failure-label-names-the-transport-fact ()
  ;; cl+ssl's wording for a peer's TCP reset is `no listing (An I/O error
  ;; occurred: undocumented reason (SSL_get_error: 5). ERR_print_errors()`.
  ;; The label reads the condition's class, never its text, and the turn's
  ;; request failure reads through the same function.
  (flet ((label (type &rest initargs)
           (nle::transport-failure-label (apply #'make-condition type initargs)))
         (transient-p (type &rest initargs)
           (nle::transient-transport-failure-p
            (apply #'make-condition type initargs))))
    (is (equal "connection reset" (label 'cl+ssl::ssl-error-syscall)))
    (is (equal "connection reset" (label 'usocket:connection-reset-error)))
    (is (equal "timeout" (label 'sb-sys:deadline-timeout :seconds 5)))
    (is (equal "timeout" (label 'usocket:timeout-error)))
    (is (equal "connection refused" (label 'usocket:connection-refused-error)))
    (is (equal "host not found" (label 'usocket:ns-host-not-found-error)))
    (is (equal "tls certificate rejected" (label 'cl+ssl::ssl-error-verify)))
    (is (equal "tls failure" (label 'cl+ssl::ssl-error-ssl)))
    ;; a class no row names keeps its name, never its printed self
    (is (equal "the request failed (simple-error)"
               (label 'simple-error :format-control "boom")))
    (is (equal "the request to 127.0.0.1:9 failed (simple-error)"
               (nle::transport-failure-label (make-condition 'simple-error :format-control "maybe invalid header")
                                             "http://127.0.0.1:9/v1/models")))
    (is (transient-p 'cl+ssl::ssl-error-syscall) "a reset earns a second attempt")
    (is (transient-p 'usocket:connection-reset-error))
    (is (transient-p 'cl+ssl::ssl-error-ssl))
    (is (not (transient-p 'cl+ssl::ssl-error-verify)) "a rejected certificate does not")
    (is (not (transient-p 'usocket:connection-refused-error)) "nor a refusal")
    (is (not (transient-p 'sb-sys:deadline-timeout :seconds 5))))
  ;; Given the address it dialed, a refusal says where nothing listens.
  (flet ((refused-at (url)
           (nle::transport-failure-label (make-condition 'usocket:connection-refused-error) url)))
    (is (equal "nothing listens at 127.0.0.1:1" (refused-at "http://127.0.0.1:1/v1/models")))
    (is (equal "nothing listens at api.example.com:443" (refused-at "https://api.example.com/v1")))
    (is (equal "timeout" (nle::transport-failure-label (make-condition 'usocket:timeout-error)
                                                       "http://127.0.0.1:1/v1")) "any other failure keeps its label")))

(deftest a-host-not-found-names-the-host-and-never-a-class ()
  ;; The import's check, offline, read `Anthropic not checked: PROVIDER-ERROR:
  ;; provider request failed [anthropic claude-opus-5-5]: host not found':
  ;; the reason labelled the provider error itself, its class and its report,
  ;; and the label named no host. The request's own failure names
  ;; the address it dialed, and the reason is that transport fact.
  (is (equal "host not found at api.anthropic.com:443"
             (nle::transport-failure-label (make-condition 'usocket:ns-host-not-found-error)
                                           "https://api.anthropic.com/v1/messages")))
  (let ((condition (handler-case
                       (nle::cancellable-post ("{}" "https://api.anthropic.com/v1/messages")
                         (error 'usocket:ns-host-not-found-error))
                     (nle::provider-error (condition) condition))))
    (is (typep condition 'nle::provider-error))
    (is (equal "host not found at api.anthropic.com:443" (nle::provider-failure-reason condition)))
    (is-carrying (text (princ-to-string condition))
      "host not found at api.anthropic.com:443" (:absent "PROVIDER-ERROR")))
  ;; A socket closed before the head printed the stream it read, `END-OF-FILE:
  ;; end of file on #<SB-SYS:FD-STREAM ...>', and the walk put it on its rows.
  (let ((condition (handler-case
                       (nle::cancellable-post ("{}" "https://api.anthropic.com/v1/messages")
                         (error 'end-of-file :stream *standard-input*))
                     (nle::provider-error (condition) condition))))
    (is-carrying (text (nle::provider-failure-reason condition))
      "the connection closed before an answer from api.anthropic.com:443"
      (:absent "#<") (:absent "END-OF-FILE")))
  ;; in-stream words with no status are the reason as said, never with a class in front
  (is (equal "in-stream provider error: busy"
             (nle::provider-failure-reason
              (make-condition 'nle::provider-error :detail "in-stream provider error: busy"))))
  (is (equal "unknown" (nle::provider-failure-reason (make-condition 'nle::provider-error))) "a detail nobody wrote"))

(deftest provider-listing-retries-a-reset-once-and-names-it ()
  ;; A listing opens one connection; without a retry a transient reset on
  ;; that connection would leave the picker reporting `no listing`. One
  ;; transient failure earns one immediate second attempt; the reason
  ;; recorded is the transport fact; and the fetch deadline cuts a GET
  ;; parked where dexador's read timeout never looks.
  (with-temp-shared-config ("{
  \"providers\": {
    \"flaky\": {\"sdk\": \"openai-completions\", \"base_url\": \"https://flaky.example/v1\", \"api_key\": \"k\"}
  }
}")
    (let ((calls 0))
      (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                 (if (= (incf calls) 1)
                                     (error 'cl+ssl::ssl-error-syscall)
                                     (values "{\"data\":[{\"id\":\"m-1\"}]}" 200)))
        (multiple-value-bind (rows error) (nle::list-provider-models "flaky")
          (is (equal '("m-1") (mapcar (lambda (row) (getf row :id)) rows)))
          (is (null error) "with nothing to report")
          (is (= 2 calls) "exactly one retry")))
      (setf calls 0)
      (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                 (incf calls)
                                 (error 'cl+ssl::ssl-error-syscall))
        (multiple-value-bind (rows error) (nle::list-provider-models "flaky")
          (is (null rows) "two resets answer no rows")
          (is (equal "connection reset" error))
          (is (= 2 calls) "the second reset is recorded, not retried again")))
      (setf calls 0)
      (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                 (incf calls)
                                 (error 'usocket:connection-refused-error))
        (multiple-value-bind (rows error) (nle::list-provider-models "flaky")
          (is (null rows))
          (is (equal "nothing listens at flaky.example:443" error) "a refusal names where")
          (is (= 1 calls) "and not retried")))
      (setf calls 0)
      (let ((nle::*provider-models-fetch-timeout-seconds* 1)
            (started (get-internal-real-time)))
        ;; A wait on a pipe nobody writes: the shape of cl+ssl's handshake
        ;; wait, which dexador's read timeout does not reach.
        (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                   (incf calls)
                                   (multiple-value-bind (in out) (sb-posix:pipe)
                                     (nlk:with-cleanup ((sb-posix:close in) (sb-posix:close out))
                                       (sb-sys:wait-until-fd-usable in :input nil))))
          (multiple-value-bind (rows error) (nle::list-provider-models "flaky")
            (is (null rows))
            (is (equal "timeout" error))
            (is (= 1 calls) "a timeout is not retried")
            (is (< (/ (- (get-internal-real-time) started)
                      internal-time-units-per-second)
                   5))))))))

(deftest provider-models-refresh-keeps-the-rows-a-failure-would-empty ()
  ;; SETF: the fetch runs on worker threads, which never see a LET binding
  ;; of the shared config path.
  (with-temp-file (nle::*shared-config-path*
                   :type "jsonc" :setf t
                   :contents "{
  \"providers\": {
    \"blip\": {\"sdk\": \"openai-completions\", \"base_url\": \"https://blip.example/v1\", \"api_key\": \"k\"},
    \"moved\": {\"sdk\": \"openai-completions\", \"base_url\": \"https://moved.example/v2\", \"api_key\": \"k\"}
  }
}")
    (let ((then (- (get-universal-time) 3600)))
      (with-temp-file (nle::*provider-models-cache-path*
                       :contents (format nil "{
  \"blip\": {\"fetched_at\": ~d, \"base_url\": \"https://blip.example/v1\", \"error\": null,
            \"models\": [{\"id\": \"kept-1\", \"context_window\": 4096}, {\"id\": \"kept-2\"}]},
  \"moved\": {\"fetched_at\": ~d, \"base_url\": \"https://moved.example/v1\", \"error\": null,
             \"models\": [{\"id\": \"gone\"}]}
}" then then))
        (let ((nle::*provider-models-memo* nil))
          (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                     (error 'cl+ssl::ssl-error-syscall))
            (is (equal '("blip" "moved")
                       (nle::refresh-provider-models '("blip" "moved")))))
          (let ((entry (nle::provider-models-cache-entry "blip")))
            (is-present entry "the failed refresh is still an entry"
              (is (equal "connection reset" (getf entry :error)) "with the reason")
              (is (equal '("kept-1" "kept-2")
                         (mapcar (lambda (row) (getf row :id)) (getf entry :models))))
              (is (eql 4096 (getf (first (getf entry :models)) :context-window)))
              (is (getf entry :fresh-p))))
          (let ((entry (nle::provider-models-cache-entry "moved")))
            (is-present entry "a provider whose base_url moved is refetched, not inherited"
              (is (null (getf entry :models)) "rows from another base_url are not kept")
              (is (equal "connection reset" (getf entry :error)))))
          (let ((nle::*provider-models-failure-ttl-seconds* 0))
            (is (not (getf (nle::provider-models-cache-entry "blip") :fresh-p)))
            (is (equal '("blip" "moved") (nle::stale-listable-provider-ids))))
          (let ((nle::*provider-models-ttl-seconds* 0))
            (is (getf (nle::provider-models-cache-entry "blip") :fresh-p))))))))

(deftest configured-provider-tuning-freezes-into-the-turn-config ()
  (with-temp-shared-config ("{\"providers\": {\"tuned\": {
    \"sdk\": \"openai-completions\", \"base_url\": \"https://tuned.example/v1\",
    \"api_key\": \"tuned-key\", \"stream_idle_timeout_ms\": 60000,
    \"stream_max_retries\": 10, \"request_max_retries\": 2,
    \"stream_rerun_ceiling_seconds\": 600,
    \"proxy\": \"socks5://localhost:1055\"},
    \"unbounded\": {\"sdk\": \"openai-completions\",
    \"base_url\": \"https://unbounded.example/v1\", \"api_key\": \"k\",
    \"stream_rerun_ceiling_seconds\": 0}}}")
    (let ((nle::*provider* "unbounded")
          (nle::*model* "m")
          (nle::*endpoint* nil)
          (nle::*api-key* nil))
      (is (null (nle::effective-provider-config-stream-rerun-ceiling
                 (nle::compiled-turn-context-provider-config
                  (user-context "hi"))))))
    (nlk:bind ((nle::*provider* "tuned") (nle::*model* "tuned-model") (nle::*endpoint* nil)
               (nle::*api-key* nil)
               (config (nle::compiled-turn-context-provider-config
                        (user-context "hi"))))
      (is (= 600 config.stream-rerun-ceiling))
      (is (= 60 config.request-timeout))
      (is (= 10 config.max-stream-retries))
      (is (= 2 config.max-request-retries))
      (is (equal "https://tuned.example/v1/chat/completions" config.endpoint))
      (is (equal "tuned-key"
                 config.api-key))
      (is (equal "socks5://localhost:1055"
                 config.proxy))))
  (with-temp-shared-config ("{\"providers\": {\"direct\": {
    \"sdk\": \"openai-completions\", \"base_url\": \"https://direct.example/v1\",
    \"api_key\": \"k\"}}}")
    (let ((nle::*provider* "direct")
          (nle::*model* "direct-model")
          (nle::*endpoint* nil)
          (nle::*api-key* nil))
      (is (null (nle::effective-provider-config-proxy
                 (nle::compiled-turn-context-provider-config
                  (user-context "hi"))))))))

(deftest shared-config-general-defaults-seed-the-selection ()
  (with-temp-shared-config ("{
  \"general\": {\"default_model\": {\"provider\": \"seeded\", \"model\": \"seeded-model\"}},
  \"providers\": {\"seeded\": {\"sdk\": \"openai-completions\", \"base_url\": \"https://seed.example/v1\", \"api_key\": \"k\"}}
}")
    (let ((nle::*provider* "openai-completions")
          (nle::*model* "before-model")
          (nle::*endpoint* nil))
      (is (nle::seed-model-config-from-shared-config))
      (is (equal "seeded" nle::*provider*))
      (is (equal "seeded-model" nle::*model*))
      (is (equal "https://seed.example/v1/chat/completions"
                 (nle::effective-endpoint)))))
  (with-temp-shared-config ("{\"general\": {\"default_model\": {\"provider\": \"unresolvable\"}}}")
    (let ((nle::*provider* "openai-completions")
          (nle::*model* "before-model")
          (nle::*endpoint* nil))
      (nle::seed-model-config-from-shared-config)
      (is (equal "openai-completions" nle::*provider*))
      (is (equal "before-model" nle::*model*))))
  ;; A configured entry is seeded whether or not its lane resolves — the
  ;; refusal then names it.
  (with-temp-shared-config ("{
  \"general\": {\"default_model\": {\"provider\": \"nosdk\", \"model\": \"m\"}},
  \"providers\": {\"nosdk\": {\"base_url\": \"https://nosdk.example/v1\", \"api_key\": \"k\"}}
}")
    (let ((nle::*provider* "openai-completions")
          (nle::*model* "before-model")
          (nle::*endpoint* nil))
      (is (nle::seed-model-config-from-shared-config) "seeding reports a change")
      (is (equal "nosdk" nle::*provider*))
      (is (search "providers.nosdk.sdk"
                  (handler-case (progn (nle::snapshot-effective-provider-config) "")
                    (nle::provider-error (e) (nle::provider-error-detail e))))))))

;;; --- writing the shared config back -----------------------------------------
;;; The one path that mutates the operator's own file. It answers an outcome
;;; rather than warning into a log, because since the store rows went away this
;;; write is the ONLY durability a model selection has: a silent failure here is
;;; the whole bug coming back.

(deftest shared-config-write-is-disabled-without-a-path ()
  ;; The hermetic default (support.lisp CALL-WITH-HERMETIC-MACHINE-STATE sets
  ;; the path to NIL) must read as "not configured", never as a write. This is
  ;; the tripwire for the incident that docstring records: a /models test that
  ;; rewrote the developer's real ~/.nodecode/config.jsonc.
  (is (null nle::*shared-config-path*))
  (multiple-value-bind (outcome detail)
      (nle::persist-shared-model-pin "default_model" "p" "m")
    (is (eq :disabled outcome) "a NIL path is disabled, not failed")
    (is (null detail) "and carries no error to report")))

(deftest shared-config-write-sets-provider-and-model-in-one-write ()
  (with-temp-shared-config ("{\"general\": {\"theme\": \"T\"}}" path)
    (is (eq :written (nle::persist-shared-model-pin "default_model" "openai-completions" "m1")))
    (is-carrying (written (uiop:read-file-string path))
      ("\"default_model\": { \"provider\": \"openai-completions\", \"model\": \"m1\" }"
       "the provider and the model landed as one pin object")
      ("\"theme\": \"T\"" "and the section's other member survived"))
    (is (equal "m1" (nlk:json-value (nle::read-shared-config) :string
                                    "general" "default_model" "model"))))
  (with-temp-shared-config ("{\"general\": {}}" path)
    (let ((before (uiop:read-file-string path)))
      (dolist (case '(("" . "m") ("p" . "") ("  " . "m")))
        (multiple-value-bind (outcome detail)
            (nle::persist-shared-model-pin "default_model" (car case) (cdr case))
          (is (eq :failed outcome)
              (format nil "~s is refused: a half pair names a model under
                           whatever lane the file happened to carry" case))
          (is detail "and says so")))
      (is (equal before (uiop:read-file-string path))))))

(deftest shared-config-write-creates-the-file-and-its-directory ()
  (let* ((directory (temp-path "cfgdir"))
         (path (merge-pathnames "config.jsonc" directory)))
    (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree directory :validate t)))
      (is (null (probe-file path)) "the fixture names nothing yet")
      (is (eq :written (nle::persist-shared-config
                        '("general") '(:default_provider "p" :default_model "m1")
                        :file path)))
      (let ((nle::*shared-config-path* path)
            (nle::*shared-config-memo* nil))
        (is (equal "p" (nlk:json-value (nle::read-shared-config) :string
                                       "general" "default_provider")))
        (is (equal "m1" (nlk:json-value (nle::read-shared-config) :string
                                        "general" "default_model")))))))

(deftest shared-config-write-answers-the-diff-it-made ()
  (with-temp-shared-config ("{\"general\": {\"theme\": \"T\"}}" path)
    (multiple-value-bind (outcome detail)
        (nle::persist-shared-config '("general") '(:default_provider "p" :theme "U"))
      (is (eq :written outcome))
      (is (equal "general: set default_provider, theme (1 added, 1 replaced)" detail)))
    (multiple-value-bind (outcome detail)
        (nle::persist-shared-config '("general" "theme") "U")
      (is (eq :written outcome))
      (is (equal "general.theme: unchanged" detail) "a satisfied write says so"))
    (multiple-value-bind (outcome detail)
        (nle::persist-shared-config '("general" "theme" "deeper") "x")
      (is (eq :failed outcome) "a path through a scalar is refused")
      (is (search "not an object" detail) "with the splice's own reason")
      (is (search "\"theme\": \"U\"" (uiop:read-file-string path))))))

(deftest config-set-and-config-get-are-the-model-facing-pair ()
  ;; CONFIG-SET answers the diff and signals a refusal; CONFIG-GET answers
  ;; the eval snippet's plist and never a secret's value.
  (with-temp-shared-config ("{
  // kept
  \"providers\": { \"p\": { \"sdk\": \"openai-responses\", \"api_key\": \"sk-live\" } },
  \"gateway\": { \"operator_token\": \"tok\" }
}" path)
    (is (equal "channels.discord: set bot_token_env, require_mention (2 added, 0 replaced)"
               (nle:config-set '("channels" "discord")
                               '(:bot_token_env "DISCORD_BOT_TOKEN" :require_mention t))))
    (is (search "// kept" (uiop:read-file-string path)) "the comment survived")
    (let ((section (nle:config-get '("channels" "discord"))))
      (is (equal "DISCORD_BOT_TOKEN" (getf section :bot_token_env)))
      (is (eq t (getf section :require_mention))))
    (is (equal "[set]" (getf (nle:config-get '("providers" "p")) :api_key)))
    (is (equal "[set]" (nle:config-get '("gateway" "operator_token"))))
    (is (equal "openai-responses" (nle:config-get '("providers" "p" "sdk"))))
    (is (null (nle:config-get '("channels" "telegram"))) "an absent path is NIL")
    (is (equal "[set]" (getf (getf (nle:config-get) :gateway) :operator_token)))
    (is (handler-case (progn (nle:config-set '("providers" "p" "sdk" "x") 1) nil)
          (error (e) (search "not an object" (princ-to-string e)))))
    (is (handler-case (progn (nle:config-set '("a") nil) nil)
          (error (e) (search "NIL is ambiguous" (princ-to-string e))))))
  (is (handler-case (progn (nle:config-set '("a") 1) nil)
        (error (e) (search "not writable" (princ-to-string e))))))

(deftest shared-config-write-refuses-to-write-a-secret-back-as-its-redaction ()
  ;; If a section read by CONFIG-GET is written back after dropping one
  ;; member, every api_key in it has already been replaced with the sentinel,
  ;; so live keys would be overwritten with the sentinel. The reader redacts
  ;; and the writer refuses, both naming the one constant, so the pair cannot
  ;; drift apart.
  (with-temp-shared-config ("{
  \"providers\": {
    \"a\": { \"sdk\": \"openai-responses\", \"api_key\": \"sk-live-a\" },
    \"b\": { \"sdk\": \"anthropic\", \"api_key\": \"sk-live-b\" }
  },
  \"gateway\": { \"operator_token\": \"tok\" }
}" path)
    (let ((before (uiop:read-file-string path))
          (section (nle:config-get '("providers"))))
      (is (equal nle::*redacted-secret* (getf (getf section :a) :api_key)))
      (multiple-value-bind (outcome detail)
          (nle::persist-shared-config '("providers") section)
        (is (eq :failed outcome) "writing that section back is refused")
        (is (search "providers.a.api_key" detail) "the refusal names each member")
        (is (search "providers.b.api_key" detail))
        (is (search ":remove" detail) "and points at the verb that was wanted"))
      (is (equal before (uiop:read-file-string path)))
      (multiple-value-bind (outcome detail)
          (nle::persist-shared-config '("gateway" "operator_token")
                                      nle::*redacted-secret*)
        (is (eq :failed outcome) "a secret named directly by path is refused too")
        (is (search "gateway.operator_token" detail)))
      (is (eq :written (nle::persist-shared-config '("providers" "a" "sdk")
                                                   "openai-completions")))
      (is (equal "sk-live-a" (nlk:json-value (nle::read-shared-config) :string
                                             "providers" "a" "api_key")))
      (is (eq :written (nle::persist-shared-config '("providers" "a" "api_key")
                                                   "sk-rotated"))))))

(deftest shared-config-write-leaves-the-file-the-operators-alone ()
  ;; config.jsonc holds every provider's api_key and the gateway's
  ;; operator_token: the write that creates it makes it 0600, and a file
  ;; another writer left readable by the group or by others is narrowed back
  ;; to its owner (2026-10-26: the operator's own file read 0644).
  (let* ((directory (temp-path "cfgmode"))
         (path (merge-pathnames "config.jsonc" directory)))
    (flet ((mode () (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring path))))))
      (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree directory :validate t)))
        (is (eq :written (nle::persist-shared-config '("general") '(:theme "T") :file path)))
        (is (= #o600 (mode)) "the file the write creates is the operator's alone")
        (sb-posix:chmod (namestring path) #o644)
        (is (eq :written (nle::persist-shared-config '("general" "theme") "U" :file path)))
        (is (= #o600 (mode)) "and a file left 0644 is narrowed to 0600")))))

(deftest config-get-reads-no-credential-in-any-shape ()
  ;; A Lean model of SECRET-KEY-P (2026-10-26) found these: a secret's name
  ;; had to end in api_key, token, secret or password spelled with `_', and
  ;; only a leaf's own name counted, so an AWS secret_access_key, an MCP
  ;; server's Authorization header, a camelCase apiKey, a key list under
  ;; api_key and a password inside a URL all read back in clear. A name
  ;; further up never counts: sub-cookie is a provider's id.
  (with-temp-shared-config ("{
  \"providers\": {
    \"aws\": { \"access_key_id\": \"AKIDPLAIN\", \"secret_access_key\": \"FAKESAK1\",
             \"base_url\": \"https://u:FAKEPW2@db.example/v1?key=FAKEQ3&model=m\" },
    \"cc\": { \"apiKey\": \"FAKECAMEL4\", \"api_key\": [\"FAKEARR5\"], \"token_file\": \"/run/t\" },
    \"sub-cookie\": { \"sdk\": \"anthropic\" }
  },
  \"mcp\": { \"servers\": { \"s\": {
    \"url\": \"https://mcp.example/x\",
    \"headers\": { \"Authorization\": \"Bearer FAKEHDR6\", \"Cookie\": \"sid=FAKECK7\", \"X-Api-Key\": \"FAKEX8\" },
    \"env\": { \"AWS_SECRET_ACCESS_KEY\": \"FAKEENV9\", \"DATABASE_URL\": \"postgres://app:FAKEPW10@h/db\" } } } },
  \"x\": { \"max_tokens\": 5 }
}" path)
    (let ((text (prin1-to-string (nle:config-get))))
      (dolist (fake '("FAKESAK1" "FAKEPW2" "FAKEQ3" "FAKECAMEL4" "FAKEARR5" "FAKEHDR6"
                      "FAKECK7" "FAKEX8" "FAKEENV9" "FAKEPW10"))
        (is (not (search fake text)) fake))
      (dolist (kept '("AKIDPLAIN" "db.example/v1" "model=m" "/run/t" "https://mcp.example/x"))
        (is (search kept text) kept)))
    (is (equal "[set]" (nle:config-get '("mcp" "servers" "s" "headers" "Authorization"))))
    (is (equal "anthropic" (nle:config-get '("providers" "sub-cookie" "sdk"))) "an id ending in a secret's word hides nothing")
    (let ((before (uiop:read-file-string path)))
      (multiple-value-bind (outcome detail)
          (nle::persist-shared-config '("mcp" "servers" "s") (nle:config-get '("mcp" "servers" "s")))
        (is (eq :failed outcome) "the redacted read written back is refused")
        (is (search "env.database_url" (string-downcase detail)) "a URL carrying the sentinel among them"))
      (is (equal before (uiop:read-file-string path)) "and nothing was written"))))

(deftest shared-config-write-removes-a-member-and-says-so (with-temp-shared-config ("{
  // the operator's own note
  \"providers\": {
    \"keep\": { \"sdk\": \"openai-responses\", \"api_key\": \"sk-live\" },
    \"gone\": { \"sdk\": \"anthropic\", \"api_key\": \"sk-dead\" }
  }
}" path))
  ;; The half of the incident that had no primitive: an object value merges,
  ;; so `remove this provider' had no seam and the model fell back to editing
  ;; the operator's file as raw text.
  (is (equal "providers.gone: removed"
             (nle:config-set '("providers" "gone") :remove)))
  (is-carrying (text (uiop:read-file-string path))
    ("// the operator's own note" "the comment survived")
    (:absent "sk-dead" "and the member is gone whole"))
  (let ((providers (nlk:json-value (nle::read-shared-config) :object "providers")))
    (is (equal 1 (hash-table-count providers)) "one provider left")
    (is (equal "sk-live" (nlk:json-value (nle::read-shared-config) :string
                                         "providers" "keep" "api_key"))))
  (is (equal "providers.gone: unchanged" (nle:config-set '("providers" "gone") :remove)))
  (is (equal "providers.keep.api_key: removed; 1 secret"
             (nle:config-set '("providers" "keep" "api_key") :remove))))

(deftest shared-config-write-counts-the-secrets-it-moved ()
  ;; The signal the incident's own diff line was missing: `0 added, 6
  ;; replaced' read as bookkeeping. A write that moves credentials says how
  ;; many, on every shape of the line.
  (with-temp-shared-config ("{\"providers\": {\"p\": {\"sdk\": \"openai-responses\"}}}")
    (is (equal "providers.p: set api_key (1 added, 0 replaced); 1 secret"
               (nle:config-set '("providers" "p")
                               '(:api_key "sk-one" :sdk "openai-responses"))))
    (is (equal "providers.p.api_key: replaced; 1 secret"
               (nle:config-set '("providers" "p" "api_key") "sk-two")))
    (is (equal "providers.p.sdk: unchanged"
               (nle:config-set '("providers" "p" "sdk") "openai-responses")))))

(deftest shared-config-write-leaves-an-unchanged-file-alone ()
  (with-temp-shared-config ("{\"general\": {}}" path)
    (nle::persist-shared-model-pin "default_model" "p" "m1")
    (let ((stamp (file-write-date path))
          (text (uiop:read-file-string path)))
      (is (eq :written (nle::persist-shared-model-pin "default_model" "p" "m1")))
      (is (eql stamp (file-write-date path)) "no rename happened")
      (is (equal text (uiop:read-file-string path)) "and nothing changed"))))

(deftest shared-config-write-clears-the-reader-memo ()
  ;; FILE-WRITE-DATE is second-granular and the memo keys on (path, size, mtime),
  ;; so two same-length writes inside one second would read back stale without
  ;; the explicit clear.
  (with-temp-shared-config ("{\"general\": {\"default_model\": {\"provider\": \"p\", \"model\": \"aaa\"}}}")
    (is (equal "aaa" (nlk:json-value (nle::read-shared-config) :string
                                     "general" "default_model" "model")))
    (nle::persist-shared-model-pin "default_model" "p" "bbb")
    (is (equal "bbb" (nlk:json-value (nle::read-shared-config) :string
                                     "general" "default_model" "model")))))

(deftest shared-config-write-reports-a-malformed-file-instead-of-corrupting-it ()
  (with-temp-shared-config ("{\"general\": {\"default_model\": \"x\" " path)
    (let ((before (uiop:read-file-string path)))
      (multiple-value-bind (outcome detail)
          (nle::persist-shared-model-pin "default_model" "p" "m1")
        (is (eq :failed outcome) "an unparseable document is refused")
        (is (search "malformed" detail)))
      (is (equal before (uiop:read-file-string path))))))

(deftest shared-config-seed-yields-to-an-explicit-env-override ()
  ;; The providers/root.zig ladder, and the whole ladder now that the store
  ;; rows are gone: an explicit NODECODE_* override at launch outranks the
  ;; persisted default; with none set, the config pair is what a boot restores.
  (with-temp-shared-config ("{\"general\": {\"default_model\": {\"provider\": \"openai-completions\",
                                                           \"model\": \"from-config\"}}}")
    (let ((nle::*provider* "openai-completions")
          (nle::*model* "before"))
      (nle::seed-model-config-from-shared-config)
      (is (equal "from-config" nle::*model*)))
    (let ((nle::*provider* "openai-completions")
          (nle::*model* "from-env"))
      (sb-posix:setenv "NODECODE_MODEL" "from-env" 1)
      (nlk:with-cleanup ((sb-posix:unsetenv "NODECODE_MODEL"))
        (nle::seed-model-config-from-shared-config)
        (is (equal "from-env" nle::*model*))))))

(deftest shared-config-write-preserves-a-realistic-document (with-temp-shared-config ("{
  \"$schema\": \"https://nodecode.ai/config.schema.json\",
  // the operator's own notes survive a model switch
  \"providers\": {
    \"loco-al\": {\"sdk\": \"openai-completions\", \"base_url\": \"https://x/v1\",
                \"models\": [\"a\", \"b\"], \"stream_max_retries\": 3,},
    \"anthropic\": {}
  },
  \"general\": {
    \"context_window_policy\": {\"evict_trigger_pct\": 85},
    \"default_model\": {\"provider\": \"loco-al\", \"model\": \"old-model\"},
    \"theme\": \"Tungstene\"
  },
  \"gateway\": {\"operator_token\": \"secret\"}
}
" path))
  ;; The regression net for the operator's real file: comments, key order, a
  ;; trailing comma, nested objects and arrays, and the empty provider block
  ;; that a decode-and-re-encode writer would have turned into `false`.
  (let ((before (uiop:read-file-string path)))
    (is (eq :written (nle::persist-shared-model-pin "default_model" "loco-al" "new-model")))
    (let ((after (uiop:read-file-string path)))
      (is (equal (replace-once before "\"old-model\"" "\"new-model\"") after))
      (let ((decoded (let ((nle::*shared-config-memo* nil))
                       (nle::read-shared-config))))
        (is (equal "new-model" (nlk:json-value decoded :string
                                               "general" "default_model" "model")))
        (is (equal 85 (nlk:json-value decoded :integer "general"
                                      "context_window_policy" "evict_trigger_pct")))
        (is (equal "secret" (nlk:json-value decoded :string
                                            "gateway" "operator_token")))
        (is-present (anthropic (nlk:json-value decoded :object
                                               "providers" "anthropic"))
            "the empty provider block is still an object"
            (is (zerop (hash-table-count anthropic))))))))

(deftest credential-point-answers-a-key-and-its-request-attributes ()
  ;; The seam an auth cell rides: after the config tier and auth.json's
  ;; api_keys answered nothing, a hook on :CREDENTIAL answers the credential,
  ;; and what it carries — an address, headers — reaches the request.
  (let ((nle::*provider* "openai-responses")
        (nle::*api-key* nil)
        (captured-url nil)
        (captured-headers nil)
        (asked nil))
    (with-temp-auth (auth "{}")
      (let ((nle::*auth-file-path* auth))
        (nle:hook :credential "credential-point-test"
                  (lambda (op next)
                    (declare (ignore next))
                    (setf asked op)
                    (nle:make-credential
                     "point-tok" :oauth
                     (list :endpoint "https://point.example/v1/responses"
                           :headers (list (cons "x-point" "yes"))))))
        (nlk:with-cleanup ((nle:unhook :credential "credential-point-test"))
          (with-stubbed-fdefinition
              (dex:post (url &rest args)
               (setf captured-url url
                     captured-headers (getf args :headers))
               (values (make-truncated-sse-stream
                        "{\"type\":\"response.output_text.delta\",\"item_id\":\"m\",\"delta\":\"ok\"}"
                        "{\"type\":\"response.completed\",\"response\":{}}")
                       200))
            (nle::call-responses-streaming (user-context)))))
    (is (equal "https://point.example/v1/responses" captured-url) "the credential's address")
    (is (equal "Bearer point-tok" (cdr (assoc "authorization" captured-headers :test #'equal))))
    (is (equal "yes" (cdr (assoc "x-point" captured-headers :test #'equal))) "its header rides")
    (is (equal "openai-responses" (getf asked :provider)) "the op names the provider")
    (is (eq :openai (getf asked :family)) "and its lane family")
    (is (equal "https://api.openai.com/v1/responses" (getf asked :endpoint)) "the address it dials"))))

(deftest credential-attribute-moves-the-openai-cache-key ()
  ;; A credential that names a discriminator routes to a prompt-cache shard of
  ;; its own: the same prompt and model, a different key.
  (let ((nle::*provider* "openai-responses")
        (nle::*api-key* nil)
        (nle::*auth-file-path* #p"/nonexistent/auth.json"))
    (flet ((cache-key (attributes)
             (nle:hook :credential "cache-shard-test"
                       (lambda (op next)
                         (declare (ignore op next))
                         (nle:make-credential "tok" :oauth attributes)))
             (unwind-protect (nle::openai-prompt-cache-key (user-context))
               (nle:unhook :credential "cache-shard-test"))))
      (is (not (equal (cache-key nil) (cache-key (list :cache-key "point")))) "its own shard"))))

(deftest compiled-config-freezes-credential-provenance (let ((nle::*api-key* "frozen-key")))
  (let ((config (nle::compiled-turn-context-provider-config
                 (user-context "hi"))))
    (is (equal "frozen-key"
               config.api-key))
    (is (null (nle::config-credential-attributes config)) "a config credential carries none")))

;;; =========================================================================
;;; Tier-3 gates: the Google GenAI lane
;;; =========================================================================

(deftest google-url-appends-model-and-sse-suffix ()
  (is (equal "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:streamGenerateContent?alt=sse"
             (nle::google-stream-url
              "https://generativelanguage.googleapis.com/v1beta/"
              "gemini-2.5-flash")))
  (is (equal "https://g/v1beta/tunedModels/model-1:streamGenerateContent?alt=sse"
             (nle::google-stream-url
              "https://g/v1beta" "tunedModels/model-1"))))

(deftest google-request-body-speaks-the-genai-dialect ()
  (let* ((nle::*provider* "google")
         (nle::*model* "gemini-2.5-flash")
         (nle::*api-key* "k")
         (nle::*reasoning-effort* "high")
         (nle::*temperature* 0.5)
         (nle::*top-p* 0.9)
         (nle::*system-prompt* "be canonical")
         (assistant (wire-message "assistant" :content "calling"))
         (tool-call (wire-tool-call "call-7" "eval" "{\"form\":\"(+ 1 2)\"}"))
         (tool-msg (nle::message
                    "tool" "3" :name "eval" :tool-call-id "call-7"))
         (stub (nle::message "system" "history stub")))
    (setf (gethash "tool_calls" assistant) (vector tool-call))
    (let* ((context (compiled-context
                     (list stub
                           (nle::message "user" "go")
                           assistant tool-msg)))
           (body (nle::google-request-body context))
           (generation (gethash "generationConfig" body))
           (thinking (gethash "thinkingConfig" generation))
           (instruction (gethash "systemInstruction" body))
           (contents (gethash "contents" body)))
      (is (= (gethash "temperature" generation) 0.5) "temperature rides generationConfig")
      (is (null (gethash "topP" generation)) "topP is unexpressible on this lane")
      (is (eq t (gethash "includeThoughts" thinking)) "thoughts are requested when effort is on")
      (is (equal "high" (gethash "thinkingLevel" thinking)) "effort becomes thinkingLevel")
      ;; the base leads the instruction; the help section follows it
      (is (eql 0 (search "be canonical" (gethash "text" (aref (gethash "parts" instruction) 0)))))
      (is (= 4 (length contents)) "one contents entry per message, no merging")
      (let ((entry (aref contents 0)))
        (is (equal "user" (gethash "role" entry)) "a history system stub rides as user")
        (is (search "[system]" (gethash "text" (aref (gethash "parts" entry) 0)))))
      (let* ((entry (aref contents 2))
             (parts (gethash "parts" entry)))
        (is (equal "model" (gethash "role" entry)) "assistant role is model")
        (let ((fc (gethash "functionCall" (aref parts 1))))
          (is (equal "eval" (gethash "name" fc)) "tool calls become functionCall parts")
          (is (equal "(+ 1 2)" (gethash "form" (gethash "args" fc))))))
      (let* ((entry (aref contents 3))
             (fr (gethash "functionResponse" (aref (gethash "parts" entry) 0)))
             (response (gethash "response" fr)))
        (is (equal "user" (gethash "role" entry)) "tool results ride the user role")
        (is (equal "eval" (gethash "name" fr)) "functionResponse names the tool")
        (is (equal "eval" (gethash "name" response)))
        (is (equal "3" (gethash "content" response)) "the payload nests under content"))
      (let* ((tools (gethash "tools" body))
             (declared (gethash "functionDeclarations" (aref tools 0))))
        (is (plusp (length declared)) "function declarations ride one tools entry")
        (is (gethash "parametersJsonSchema" (aref declared 0)) "the schema rides verbatim as JSON Schema")))))

(deftest google-stream-folds-text-thought-and-whole-tool-calls (let ((nle::*provider* "google")
                                                                     (nle::*api-key* "k")
                                                                     (tags '())))
  (with-mock-sse-stream
      (:truncated
       "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"considering\",\"thought\":true,\"thoughtSignature\":\"sig-1\"}]}}]}"
       "{\"error\":{\"code\":429,\"message\":\"ignored noise\"}}"
       "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"answer \"}]}}]}"
       "{\"candidates\":[{\"content\":{\"parts\":[{\"functionCall\":{\"name\":\"eval\",\"args\":{\"form\":\"(f)\"}}}]},\"finishReason\":\"STOP\"}],\"usageMetadata\":{\"promptTokenCount\":3,\"candidatesTokenCount\":5,\"totalTokenCount\":8,\"thoughtsTokenCount\":2,\"cachedContentTokenCount\":1}}")
    (multiple-value-bind (message usage finish-reason request-json)
        (nle::call-google-streaming (user-context) :on-part (lambda (part) (push part.kind tags)))
      (is (equal "answer " (gethash "content" message)) "text parts accumulate")
      (is (equal "considering" (gethash "reasoning_content" message)))
      (let ((call (aref (gethash "tool_calls" message) 0)))
        (is (equal "tool-0" (gethash "id" call)) "tool ids are minted locally")
        (is (equal "{\"form\":\"(f)\"}"
                   (gethash "arguments" (gethash "function" call)))))
      (is (equal "tool_calls" finish-reason))
      (is (= 2 usage.input-tokens))
      (is (= 5 usage.output-tokens))
      (is (= 8 usage.total-tokens))
      (is (= 2 usage.reasoning-tokens))
      (is (= 1 usage.cached-input-tokens))
      (is (typep request-json 'nlk:octets) "the serialized request, as its octets, is the fourth value"))
    (let ((ordered (nreverse tags)))
      (is (eq :stream-start (first ordered)) "the stream opens with stream-start")
      (is (member :reasoning-signature ordered) "the signature surfaces as a part")
      (is (eq :finish (car (last ordered))) "the stream closes with finish")
      (is (not (member :error ordered))))))

(deftest google-tool-call-replays-its-thought-signature (let ((nle::*provider* "google")
                                                              (nle::*model* "gemini-3.8-flash")
                                                              (nle::*api-key* "k")))
  ;; Gemini 3 refuses the next round of a tool turn when the step's first
  ;; functionCall comes back without the thoughtSignature it arrived with;
  ;; a parallel call after it arrives, and replays, unsigned.
  (with-mock-sse-stream
      (:truncated
       "{\"candidates\":[{\"content\":{\"parts\":[{\"functionCall\":{\"name\":\"eval\",\"args\":{\"form\":\"(f)\"}},\"thoughtSignature\":\"sig-a\"},{\"functionCall\":{\"name\":\"eval\",\"args\":{\"form\":\"(g)\"}}}]},\"finishReason\":\"STOP\"}]}")
    (let* ((message (nle::call-google-streaming (user-context)))
           (contents (gethash "contents"
                              (nle::google-request-body
                               (compiled-context
                                (list (nle::message "user" "go")
                                      message
                                      (nle::message "tool" "1" :name "eval" :tool-call-id "tool-0")
                                      (nle::message "tool" "2" :name "eval" :tool-call-id "tool-1"))))))
           (parts (gethash "parts" (aref contents 1))))
      ;; the message keeps the signature under its call's id
      (is (equal "sig-a" (nlk:json-value message :string "thought_signatures" "tool-0")))
      (is (= 2 (length parts)))
      ;; the signed call replays its signature on the same part, the other unsigned
      (is (equal "sig-a" (gethash "thoughtSignature" (aref parts 0))))
      (is (null (nth-value 1 (gethash "thoughtSignature" (aref parts 1))))))))

(deftest google-eof-is-normal-completion (let ((nle::*provider* "google")
                                               (nle::*api-key* "k")))
  ;; No [DONE], no terminal event: end of stream is the google done marker,
  ;; so a stream that just stops is a completed round, never truncation.
  (with-mock-sse-stream
      (:truncated
       "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"done\"}]}}]}")
    (multiple-value-bind (message usage finish-reason)
        (nle::call-google-streaming
         (user-context))
      (is (equal "done" (gethash "content" message)) "the fold returns the message")
      (is (null usage) "no usageMetadata means no usage, not zeros")
      (is (null finish-reason) "an absent finishReason stays absent"))))

;;; =========================================================================
;;; Tier-3 gates: the OpenAI Responses lane
;;; =========================================================================

(deftest responses-request-is-stateless-and-replays-reasoning-by-value ()
  (let* ((nle::*provider* "openai-responses")
         (nle::*model* "gpt-5-mini")
         (nle::*api-key* "k")
         (nle::*reasoning-effort* "medium")
         (nle::*temperature* 0.7)
         (nle::*system-prompt* "be stateless")
         (assistant (wire-message "assistant" :content "answering")))
    (setf (gethash "reasoning_content" assistant) "thought hard"
          (gethash "reasoning_items" assistant)
          (vector (nle::json-object "id" "rs_1" "text" "thought hard" "encrypted_content" "enc-blob"))
          (gethash "tool_calls" assistant)
          (vector (wire-tool-call "call-9" "eval" "{\"form\":\"(g)\"}")))
    (let* ((context (compiled-context
                     (list (nle::message "user" "go")
                           assistant
                           (nle::message
                            "tool" "9" :name "eval" :tool-call-id "call-9"))))
           (body (nle::responses-request-body context))
           (input (gethash "input" body)))
      (is (eq :false (gethash "store" body)))
      (is (eq t (gethash "stream" body)) "stream is always true")
      (is (null (gethash "temperature" body)))
      (let ((reasoning (gethash "reasoning" body)))
        (is (equal "medium" (gethash "effort" reasoning)))
        (is (equal "auto" (gethash "summary" reasoning))))
      (is (find "reasoning.encrypted_content" (gethash "include" body)
                :test #'equal))
      (is (equal context.system-prompt (gethash "instructions" body)) "the prompt is instructions")
      (is (equal "user" (gethash "role" (aref input 0))) "never an input item")
      (let ((item (aref input 1)))
        (is (equal "reasoning" (gethash "type" item)))
        (is (equal "rs_1" (gethash "id" item)))
        (is (equal "enc-blob" (gethash "encrypted_content" item)))
        (is (equal "thought hard"
                   (gethash "text" (aref (gethash "summary" item) 0)))))
      (let ((item (aref input 2)))
        (is (equal "assistant" (gethash "role" item)))
        (is (equal "output_text"
                   (gethash "type" (aref (gethash "content" item) 0)))))
      (let ((item (aref input 3)))
        (is (equal "function_call" (gethash "type" item)))
        (is (equal "call-9" (gethash "call_id" item)))
        (is (equal "{\"form\":\"(g)\"}" (gethash "arguments" item))))
      (let ((item (aref input 4)))
        (is (equal "function_call_output" (gethash "type" item)))
        (is (equal "call-9" (gethash "call_id" item)))
        (is (equal "9" (gethash "output" item))))
      (let ((tool (aref (gethash "tools" body) 0)))
        (is (equal "function" (gethash "type" tool)))
        (is (gethash "name" tool) "the tool shape is flat, not nested")))))

(deftest responses-stream-folds-the-typed-event-walk (let ((nle::*provider* "openai-responses")
                                                           (nle::*api-key* "k")
                                                           (tags '())))
  (with-mock-sse-stream
      (:truncated
       "{\"type\":\"response.created\",\"response\":{\"id\":\"resp_1\",\"model\":\"gpt-5-mini\",\"created_at\":1755100000}}"
       "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs_1\",\"encrypted_content\":\"enc-start\"}}"
       "{\"type\":\"response.reasoning_summary_text.delta\",\"item_id\":\"rs_1\",\"summary_index\":0,\"delta\":\"weighing\"}"
       "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs_1\",\"encrypted_content\":\"enc-done\"}}"
       "{\"type\":\"response.output_item.added\",\"output_index\":1,\"item\":{\"type\":\"function_call\",\"id\":\"fc_1\",\"call_id\":\"call-9\",\"name\":\"eval\"}}"
       "{\"type\":\"response.function_call_arguments.delta\",\"output_index\":1,\"delta\":\"{\\\"form\\\":\"}"
       "{\"type\":\"response.function_call_arguments.delta\",\"output_index\":1,\"delta\":\"\\\"(h)\\\"}\"}"
       "{\"type\":\"response.output_text.delta\",\"item_id\":\"msg_1\",\"delta\":\"running it\"}"
       "{\"type\":\"response.completed\",\"response\":{\"usage\":{\"input_tokens\":30090,\"output_tokens\":104,\"total_tokens\":30194,\"input_tokens_details\":{\"cached_tokens\":28416},\"output_tokens_details\":{\"reasoning_tokens\":7}}}}")
    (multiple-value-bind (message usage finish-reason request-json)
        (nle::call-responses-streaming
         (user-context)
         :on-part (lambda (part)
                    (push part.kind tags)))
      (is (equal "running it" (gethash "content" message)))
      (is (equal "weighing" (gethash "reasoning_content" message)))
      (let ((items (gethash "reasoning_items" message)))
        (is (= 1 (length items)))
        (is (equal "rs_1" (gethash "id" (aref items 0))))
        (is (equal "weighing" (gethash "text" (aref items 0))))
        ;; the done event's encrypted content overrides the start value
        (is (equal "enc-done" (gethash "encrypted_content" (aref items 0)))))
      (let ((call (aref (gethash "tool_calls" message) 0)))
        (is (equal "call-9" (gethash "id" call)))
        (is (equal "{\"form\":\"(h)\"}"
                   (gethash "arguments" (gethash "function" call)))))
      (is (equal "tool_calls" finish-reason))
      (is (= 1674 usage.input-tokens))
      (is (= 28416 usage.cached-input-tokens))
      (is (= 30194 usage.total-tokens))
      (is (= 7 usage.reasoning-tokens))
      (is (typep request-json 'nlk:octets)))
    (let ((ordered (nreverse tags)))
      (is (eq :stream-start (first ordered)))
      (is (member :reasoning-start ordered) "the reasoning span opened")
      (is (eq :finish (car (last ordered)))))))

(deftest responses-metadata-carries-what-the-stream-answered-with ()
  ;; The Responses stream names the model it is actually running and the
  ;; effort it resolved the request's silence to, and nests its cache-write
  ;; count inside input_tokens_details. All three were parsed into nothing.
  (let ((nle::*provider* "openai-responses")
        (nle::*api-key* "k")
        (metadata nil))
    (with-mock-sse-stream
        (:truncated
         "{\"type\":\"response.created\",\"response\":{\"id\":\"resp_1\",\"model\":\"gpt-5.6-sol-2026-08-01\",\"created_at\":1755100000,\"reasoning\":{\"effort\":\"medium\"},\"service_tier\":\"auto\"}}"
         "{\"type\":\"response.output_text.delta\",\"item_id\":\"msg_1\",\"delta\":\"ok\"}"
         "{\"type\":\"response.completed\",\"response\":{\"usage\":{\"input_tokens\":4393,\"output_tokens\":7,\"total_tokens\":4400,\"input_tokens_details\":{\"cached_tokens\":3840,\"cache_write_tokens\":512},\"output_tokens_details\":{\"reasoning_tokens\":4}}}}")
      (multiple-value-bind (message usage)
          (nle::call-responses-streaming
           (user-context)
           :on-part (lambda (part) (when (eq :response-metadata part.kind) (setf metadata part))))
        (declare (ignore message))
        (is metadata "the attempt announced itself")
        (is (equal "gpt-5.6-sol-2026-08-01" metadata.model))
        (is (equal "medium" metadata.reasoning-effort))
        (is (= 553 usage.input-tokens))
        (is (= 512 usage.cache-write-tokens))
        (is (= 4 usage.reasoning-tokens))))))

(deftest responses-summary-parts-keep-their-boundary (let ((nle::*provider* "openai-responses")
                                                           (nle::*api-key* "k")
                                                           (deltas '())
                                                           (tags '())))
  ;; One reasoning item streams as a sequence of independently-titled
  ;; summary parts. Captured live from a gpt-5 endpoint, each part is one
  ;; `**Header**` and nothing else, so concatenating them without their
  ;; boundary produces `**A****B**` — one unreadable run in the thinking
  ;; block, and the same run again on replay because reasoning_content is
  ;; the durable string the transcript rehydrates from.
  (with-mock-sse-stream
      (:truncated
       "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs_1\",\"encrypted_content\":\"enc\"}}"
       "{\"type\":\"response.reasoning_summary_part.added\",\"item_id\":\"rs_1\",\"summary_index\":0,\"part\":{\"type\":\"summary_text\",\"text\":\"\"}}"
       "{\"type\":\"response.reasoning_summary_text.delta\",\"item_id\":\"rs_1\",\"summary_index\":0,\"delta\":\"**Analyzing puzzle inconsistencies**\"}"
       "{\"type\":\"response.reasoning_summary_part.done\",\"item_id\":\"rs_1\",\"summary_index\":0,\"part\":{\"type\":\"summary_text\",\"text\":\"**Analyzing puzzle inconsistencies**\"}}"
       "{\"type\":\"response.reasoning_summary_part.added\",\"item_id\":\"rs_1\",\"summary_index\":1,\"part\":{\"type\":\"summary_text\",\"text\":\"\"}}"
       "{\"type\":\"response.reasoning_summary_text.delta\",\"item_id\":\"rs_1\",\"summary_index\":1,\"delta\":\"**Identifying no valid solution**\"}"
       "{\"type\":\"response.reasoning_summary_part.done\",\"item_id\":\"rs_1\",\"summary_index\":1,\"part\":{\"type\":\"summary_text\",\"text\":\"**Identifying no valid solution**\"}}"
       "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs_1\",\"encrypted_content\":\"enc\"}}"
       "{\"type\":\"response.output_text.delta\",\"item_id\":\"msg_1\",\"delta\":\"no solution\"}"
       "{\"type\":\"response.completed\",\"response\":{\"usage\":{\"total_tokens\":9}}}")
    (let ((message (nle::call-responses-streaming
                    (user-context)
                    :on-part
                    (lambda (part)
                      (push part.kind tags)
                      (when (eq :reasoning-delta part.kind) (push part.text deltas))))))
      (is (equal (format nil "**Analyzing puzzle inconsistencies**~%~%~
**Identifying no valid solution**")
                 (gethash "reasoning_content" message)))
      (is (equal (format nil "**Analyzing puzzle inconsistencies**~%~%~
**Identifying no valid solution**")
                 (apply #'concatenate 'string (reverse deltas)))))
    (let ((ordered (nreverse tags)))
      (is (= 2 (count :reasoning-start ordered)))
      (is (= 2 (count :reasoning-end ordered))))))

(deftest responses-reasoning-text-deltas-are-the-think (let ((nle::*provider* "openai-responses")
                                                             (nle::*api-key* "k")
                                                             (deltas '())
                                                             (tags '())))
  ;; DeepSeek's Responses lane streams the reasoning itself as content
  ;; parts (response.reasoning_text.delta, content_index) and no summary:
  ;; it folds like a summary part, and the encrypted handle still rides.
  (with-mock-sse-stream
      (:truncated
       "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs_1\",\"status\":\"in_progress\",\"content\":[],\"summary\":[],\"encrypted_content\":\"handle-0\"}}"
       "{\"type\":\"response.reasoning_text.delta\",\"content_index\":0,\"delta\":\"Reflect. \",\"item_id\":\"rs_1\",\"output_index\":0}"
       "{\"type\":\"response.reasoning_text.delta\",\"content_index\":0,\"delta\":\"The turn: one eval.\",\"item_id\":\"rs_1\",\"output_index\":0}"
       "{\"type\":\"response.reasoning_text.done\",\"content_index\":0,\"item_id\":\"rs_1\",\"output_index\":0,\"text\":\"Reflect. The turn: one eval.\"}"
       "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs_1\",\"status\":\"completed\",\"content\":[{\"type\":\"reasoning_text\",\"text\":\"Reflect. The turn: one eval.\"}],\"summary\":[],\"encrypted_content\":\"handle-0\"}}"
       "{\"type\":\"response.output_text.delta\",\"item_id\":\"msg_1\",\"delta\":\"done\"}"
       "{\"type\":\"response.completed\",\"response\":{\"usage\":{\"total_tokens\":9}}}")
    (let ((message (nle::call-responses-streaming
                    (user-context)
                    :on-part
                    (lambda (part)
                      (push part.kind tags)
                      (when (eq :reasoning-delta part.kind) (push part.text deltas))))))
      (is (equal "Reflect. The turn: one eval."
                 (gethash "reasoning_content" message)))
      (is (equal "Reflect. The turn: one eval."
                 (apply #'concatenate 'string (reverse deltas))))
      (is (equal "done" (gethash "content" message)))
      (is (equal "handle-0" (gethash "encrypted_content"
                                     (aref (gethash "reasoning_items" message) 0)))))
    (let ((ordered (nreverse tags)))
      (is (= 1 (count :reasoning-start ordered)))
      (is (= 1 (count :reasoning-end ordered))))))

(deftest responses-keeps-every-reasoning-item-of-a-response (let ((nle::*provider* "openai-responses")
                                                                  (nle::*api-key* "k")))
  ;; A response may carry multiple reasoning items before a function call,
  ;; each with its own encrypted content. Without this, the message would keep
  ;; only the last item's state, and the first item's reasoning would never
  ;; reach a later round. Each item stays its own, in order.
  (with-mock-sse-stream
      (:truncated
       "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs_A\",\"encrypted_content\":\"enc-A-start\"}}"
       "{\"type\":\"response.reasoning_summary_part.added\",\"item_id\":\"rs_A\",\"output_index\":0,\"summary_index\":0}"
       "{\"type\":\"response.reasoning_summary_text.delta\",\"item_id\":\"rs_A\",\"output_index\":0,\"summary_index\":0,\"delta\":\"first\"}"
       "{\"type\":\"response.reasoning_summary_part.added\",\"item_id\":\"rs_A\",\"output_index\":0,\"summary_index\":1}"
       "{\"type\":\"response.reasoning_summary_text.delta\",\"item_id\":\"rs_A\",\"output_index\":0,\"summary_index\":1,\"delta\":\"second\"}"
       "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs_A\",\"encrypted_content\":\"enc-A\"}}"
       "{\"type\":\"response.output_item.added\",\"output_index\":1,\"item\":{\"type\":\"reasoning\",\"id\":\"rs_B\",\"encrypted_content\":\"enc-B-start\"}}"
       "{\"type\":\"response.reasoning_summary_text.delta\",\"item_id\":\"rs_B\",\"output_index\":1,\"summary_index\":0,\"delta\":\"third\"}"
       "{\"type\":\"response.output_item.done\",\"output_index\":1,\"item\":{\"type\":\"reasoning\",\"id\":\"rs_B\",\"encrypted_content\":\"enc-B\"}}"
       "{\"type\":\"response.output_item.added\",\"output_index\":2,\"item\":{\"type\":\"function_call\",\"id\":\"fc_1\",\"call_id\":\"call-1\",\"name\":\"eval\"}}"
       "{\"type\":\"response.function_call_arguments.delta\",\"output_index\":2,\"delta\":\"{}\"}"
       "{\"type\":\"response.completed\",\"response\":{\"usage\":{\"total_tokens\":9}}}")
    (let* ((message (nle::call-responses-streaming (user-context)))
           (items (gethash "reasoning_items" message)))
      (is (= 2 (length items)) "both items, not the last one alone")
      (is (equal '("rs_A" "rs_B") (map 'list (lambda (i) (gethash "id" i)) items)))
      ;; each item its own text, its parts a blank line apart, and its own
      ;; encrypted content, from its done event
      (is (equal (list (format nil "first~%~%second") "third")
                 (map 'list (lambda (i) (gethash "text" i)) items)))
      (is (equal '("enc-A" "enc-B") (map 'list (lambda (i) (gethash "encrypted_content" i)) items)))
      (let ((input (gethash "input" (nle::responses-request-body
                                     (local-context
                                      (list (nle::message "user" "go") message
                                            (nle::message "tool" "1" :name "eval"
                                                                     :tool-call-id "call-1"))
                                      :provider-config (test-provider-config
                                                        :provider "openai-responses"
                                                        :model "gpt-6"))))))
        ;; every item replays, in order, before the call
        (is (equal '("user" "reasoning" "reasoning" "function_call" "function_call_output")
                   (map 'list (lambda (item) (or (gethash "type" item) (gethash "role" item)))
                        input)))
        (is (equal '("enc-A" "enc-B")
                   (map 'list (lambda (i) (gethash "encrypted_content" (aref input i))) '(1 2))))
        (is (equal (list (format nil "first~%~%second") "third")
                   (map 'list (lambda (i)
                                (gethash "text" (aref (gethash "summary" (aref input i)) 0)))
                        '(1 2))))))))

(deftest responses-requires-an-explicit-terminal-event (let ((nle::*provider* "openai-responses")
                                                             (nle::*api-key* "k")))
  (with-mock-sse-stream
      (:truncated
       "{\"type\":\"response.output_text.delta\",\"item_id\":\"msg_1\",\"delta\":\"half\"}")
    (is (signals-error nle::provider-stream-incomplete
          (nle::call-responses-streaming
           (user-context))))))

(deftest responses-failed-terminal-rejects-partial-tools ()
  ;; A failed response is an attempt failure, never a callable message.
  (let ((nle::*provider* "openai-responses")
        (nle::*api-key* "k"))
    (dolist (code '("upstream_unavailable" "insufficient_quota"
                    "context_length_exceeded" nil))
      (let* ((parts '())
             (condition
               (signals-error nle::provider-error
                 (with-mock-sse-stream
                     (:truncated
                      "{\"type\":\"response.output_item.added\",\"output_index\":1,\"item\":{\"type\":\"function_call\",\"id\":\"fc_1\",\"call_id\":\"call-9\",\"name\":\"eval\"}}"
                      "{\"type\":\"response.function_call_arguments.delta\",\"output_index\":1,\"delta\":\"{\\\"form\\\":\\\"(nle:submit\"}"
                      (nle::encode-json-object
                       (nlk:json-object
                        "type" "response.failed"
                        "response" (nlk:json-object
                                    "error" (if code
                                                (nlk:json-object "code" code "message" "upstream stopped")
                                                :null)))))
                   (nle::call-responses-streaming
                    (user-context)
                    :on-part (lambda (part) (push part parts)))))))
        (is condition "response.failed signals even with partial tool arguments")
        (when condition
          (is (search "response.failed" (nle::provider-error-evidence-body condition)) "the terminal frame is retained as evidence")
          (is (nle::provider-error-evidence-request-body condition) "the request remains available for diagnosis")
          (cond
            ((equal code "insufficient_quota")
             (is (eql 400 (nle::provider-error-status condition)))
             (is (not (nle::retryable-provider-error-p condition))))
            ((equal code "context_length_exceeded")
             (is (nle::provider-overflow-error-p condition)))
            (t (is (nle::retryable-provider-error-p condition))))
          (when code
            (is (search code (nle::provider-error-detail condition)) "the upstream error code is not replaced with a tool JSON error")))
        (let ((ordered (nreverse parts)))
          (is (find :error ordered :key #'nle::stream-part-kind) "the failure is visible to stream consumers")
          (is (eq :finish (nle::stream-part-kind (car (last ordered)))) "the failed attempt still closes its stream bracket"))))))

(deftest responses-failed-terminal-retries-before-returning-a-message ()
  (let ((nle::*provider* "openai-responses")
        (nle::*api-key* "k")
        (nle::*max-stream-retries* 1)
        (nle::*retry-base-delay* 0)
        (calls 0)
        (parts '()))
    (with-stubbed-fdefinition
        (dex:post (&rest args)
          (incf calls)
          (values
           (if (= calls 1)
               (make-truncated-sse-stream
                "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"function_call\",\"call_id\":\"failed-call\",\"name\":\"eval\"}}"
                "{\"type\":\"response.function_call_arguments.delta\",\"output_index\":0,\"delta\":\"{\\\"form\\\":\\\"(nle:submit\"}"
                "{\"type\":\"response.failed\",\"response\":{\"error\":{\"code\":\"upstream_unavailable\",\"message\":\"upstream stopped\"}}}")
               (make-truncated-sse-stream
                "{\"type\":\"response.output_text.delta\",\"item_id\":\"msg_1\",\"delta\":\"recovered\"}"
                "{\"type\":\"response.completed\",\"response\":{\"usage\":{\"input_tokens\":20,\"output_tokens\":3}}}"))
           200))
      (let ((message (nle::call-provider
                      (user-context)
                      :on-part (lambda (part) (push part parts)))))
        (is (= 2 calls) "response.failed consumes one stream retry")
        (is (equal "recovered" (gethash "content" message)))
        (is (null (nle::message-tool-calls message)) "the partial failed tool is never handed to dispatch")
        (is (find :stream-retry parts :key #'nle::stream-part-kind) "consumers see the retry, not an eval preflight failure")))))

(deftest responses-terminal-error-code-table (let ((nle::*provider* "openai-responses")
                                                   (nle::*api-key* "k")))
  (flet ((run-error-stream (code)
           (let* ((tags '())
                  (condition
                    (signals-error nle::provider-error
                      (with-mock-sse-stream
                          (:truncated
                           (format nil "{\"type\":\"error\",\"code\":~a,\"message\":\"boom\"}"
                                   (if code (format nil "\"~a\"" code) "null")))
                        (nle::call-responses-streaming
                         (user-context)
                         :on-part (lambda (part)
                                    (push part.kind tags)))))))
             (values condition (nreverse tags)))))
    (multiple-value-bind (e tags) (run-error-stream "insufficient_quota")
      (is (and e (not (nle::retryable-provider-error-p e))))
      (is (member :error tags) "the error part still lands")
      (is (member :finish tags) "the stream is well-formed through finish"))
    (let ((e (run-error-stream "context_length_exceeded")))
      (is (and e (nle::provider-overflow-error-p e))))
    (let ((e (run-error-stream "server_is_overloaded")))
      (is (and e (nle::retryable-provider-error-p e))))
    (let ((e (run-error-stream nil)))
      (is (and e (nle::retryable-provider-error-p e))))))

(deftest floor-boundary-counts-fully-evicted-history ()
  (let ((turns (list (nle::make-history-turn :turn-id "t1" :start-sequence 1
                                             :end-sequence 3 :terminal-p t)
                     (nle::make-history-turn :turn-id "t2" :start-sequence 4
                                             :end-sequence 6 :terminal-p t))))
    (is (= 0 (nle::floor-boundary turns 0)) "no floor evicts nothing")
    (is (= 1 (nle::floor-boundary turns 4)))
    (is (= 2 (nle::floor-boundary turns 7)))))

(defun make-forced-eviction-turn (id start end tokens &optional (terminal t))
  (nle::make-history-turn :turn-id id :start-sequence start :end-sequence end
                          :messages (list (nle:message "user" "x"))
                          :terminal-p terminal :token-estimate tokens))

(deftest plan-eviction-through-names-the-turn-it-stops-after ()
  ;; Evicting through a turn takes it and every older retained turn: a turn
  ;; spread over two units goes whole, the newest turn and the live tail
  ;; never go, and a turn not retained plans nothing.
  (let ((turns (list (make-forced-eviction-turn "t1" 1 2 100)
                     (make-forced-eviction-turn "t2" 3 4 100)
                     (make-forced-eviction-turn "t2" 5 6 50)
                     (make-forced-eviction-turn "t3" 7 8 100))))
    (let ((plan (nle::plan-eviction-through turns "t2")))
      (is (= 3 (getf plan :evicted-turn-count)) "both of t2's units go with it")
      (is (= 7 (getf plan :retained-from-sequence)))
      (is (= 250 (getf plan :evicted-estimated-tokens)))
      (is (= 71 (getf plan :realized-percent))))
    (is (= 1 (getf (nle::plan-eviction-through turns "t1") :evicted-turn-count)))
    (is (null (nle::plan-eviction-through turns "t3")) "the newest turn stays")
    (is (null (nle::plan-eviction-through turns "gone")) "a turn not retained plans nothing"))
  ;; The live tail never goes.
  (is (null (nle::plan-eviction-through
             (list (make-forced-eviction-turn "closed" 1 2 100)
                   (make-forced-eviction-turn "live" 3 4 100 nil)
                   (make-forced-eviction-turn "next" 5 6 100 nil))
             "live"))))

(deftest plan-forced-eviction-matches-zig-boundary-walk ()
  ;; The Zig planForcedEviction contract: whole turns are the unit, the
  ;; boundary is the first turn edge at or past the percent target, the
  ;; newest turn is always retained, and an empty plan means nothing folds.
  (let ((plan (nle::plan-forced-eviction
               (list (make-forced-eviction-turn "t1" 1 2 100)
                     (make-forced-eviction-turn "t2" 3 4 100)
                     (make-forced-eviction-turn "t3" 5 6 100)
                     (make-forced-eviction-turn "t4" 7 8 100))
               50)))
    (is (= 2 (getf plan :evicted-turn-count)))
    (is (= 50 (getf plan :realized-percent)))
    (is (= 5 (getf plan :retained-from-sequence)))
    (is (= 4 (getf plan :evicted-end-sequence)))
    (is (= 200 (getf plan :evicted-estimated-tokens)))
    (is (= 200 (getf plan :retained-estimated-tokens))))
  (let ((plan (nle::plan-forced-eviction
               (list (make-forced-eviction-turn "t1" 1 2 100)
                     (make-forced-eviction-turn "t2" 3 4 100))
               100)))
    (is (= 1 (getf plan :evicted-turn-count))))
  (is (null (nle::plan-forced-eviction
             (list (make-forced-eviction-turn "only" 1 2 100)) 100)))
  (is (null (nle::plan-forced-eviction '() 50)))
  (is (null (nle::plan-forced-eviction
             (list (make-forced-eviction-turn "live-1" 1 2 100 nil)
                   (make-forced-eviction-turn "live-2" 3 4 100 nil))
             100)))
  (let ((plan (nle::plan-forced-eviction
               (list (make-forced-eviction-turn "closed" 1 2 100)
                     (make-forced-eviction-turn "live" 3 4 100 nil))
               100)))
    (is (= 1 (getf plan :evicted-turn-count))))
  (is (null (nle::plan-forced-eviction
             (list (make-forced-eviction-turn "tiny-1" 1 2 10)
                   (make-forced-eviction-turn "tiny-2" 3 4 10))
             1))))

(deftest history-eviction-stub-carries-zig-typography ()
  ;; The renderEvictionStub span typography: en dash and middle dots. The
  ;; trailing clause names RECALL-LOG — a road sign must point at a read the
  ;; organism actually ships, so the gate holds the copy and the fboundp
  ;; claim together: whoever renames one must move the other.
  (let ((stub (nle::history-eviction-stub
               (list (make-forced-eviction-turn "t1" 1 3 100)
                     (make-forced-eviction-turn "t2" 4 6 100))
               1)))
    (is (equal "[history evicted: seq 1–3 · 1 turns · recall: (recall-log :match \"text\" :before 4)]"
               (gethash "content" stub)))
    (is (fboundp 'nlk:recall-log))))

;;; =========================================================================
;;; C2 gates: hydration cache
;;; =========================================================================

(deftest hydration-cache-folds-only-the-tail (with-temp-store ())
  (nle::clear-hydration-cache)
  (nlk:create-session :id "hc")
  (let ((session (nle::make-session :id "hc")))
    (dotimes (i 2)
      (let ((turn (nlk:admit-turn "hc" (format nil "hc-cmd-~d" i)
                                  (format nil "input ~d" i))))
        (nlk:record-assistant-message
         turn (wire-message "assistant"
                            :content (format nil "answer ~d" i)))
        (nlk:complete-turn turn nil)))
    (let ((afters '()))
      (with-stubbed-fdefinition (nlk:page-events (&rest args)
                                 (push (getf args :after) afters)
                                 (apply original args))
        (let ((cold (nle::history-turns-for-session session)))
          (is (= 2 (length cold)) "the cold fold sees both turns")
          (is (member 0 afters))
          (setf afters '())
          (let ((warm (nle::history-turns-for-session session)))
            (is (= 2 (length warm)) "the warm fold sees the same turns")
            (is (eq (first cold) (first warm)))
            (is (and afters (every #'plusp afters))))
          ;; A third turn lands; only the tail folds.
          (let ((turn (nlk:admit-turn "hc" "hc-cmd-2" "input 2")))
            (nlk:record-assistant-message
             turn (wire-message "assistant" :content "answer 2"))
            (nlk:complete-turn turn nil))
          (setf afters '())
          (let ((extended (nle::history-turns-for-session session)))
            (is (= 3 (length extended)) "the new turn joins the units")
            (is (every #'plusp afters))))))))

(defun open-turn-round (turn n &aux (call-id (format nil "open-~d" n)))
  "Land the facts one tool-calling round of an open TURN leaves."
  (nlk:record-assistant-message
   turn (wire-message "assistant" :content ""
                      :tool-calls (vector (wire-tool-call call-id "eval" "{}"))))
  (nlk:record-tool-call turn call-id "eval" "{}")
  (nlk:record-tool-result turn call-id "eval" (format nil "result ~d" n)))

(defun unit-contents (unit) (mapcar (lambda (message) (gethash "content" message)) unit.messages))

(deftest hydration-cache-extends-the-open-turn-round-by-round (with-temp-store ())
  ;; 93% of the operator's provider rounds are round one or later of a
  ;; multi-round turn, and the cache used to stop at the last CLOSED turn:
  ;; every round re-paged, re-shaped, re-measured and re-encoded every
  ;; round the turn had already sent.
  (nle::clear-hydration-cache)
  (nlk:create-session :id "hco")
  (let* ((session (nle::make-session :id "hco"))
         (turn (nlk:admit-turn "hco" "hco-cmd" "ask")))
    (open-turn-round turn 0)
    (let* ((first-unit (car (last (nle::history-turns-for-session session))))
           (first-messages first-unit.messages))
      (is (not first-unit.terminal-p) "the turn is still open")
      (is (nle::history-turn-clean-p first-unit) "its fold ended at a cut point")
      (open-turn-round turn 1)
      (let ((afters '()))
        (with-stubbed-fdefinition (nlk:page-events (&rest args)
                                   (push (getf args :after) afters)
                                   (apply original args))
          (let* ((unit (car (last (nle::history-turns-for-session session))))
                 (messages (nle::history-turn-messages unit)))
            (is (every #'plusp afters) "the round pages only the facts it landed")
            (is (equal (nle::history-turn-turn-id first-unit)
                       (nle::history-turn-turn-id unit)))
            (is (= (+ 2 (length first-messages)) (length messages)))
            (is (every #'eq first-messages (subseq messages 0 (length first-messages))))
            ;; What the extension answers is what a fold from zero answers.
            (nle::clear-hydration-cache)
            (let ((cold (car (last (nle::history-turns-for-session session)))))
              (is (equal (mapcar #'nlk:encode-json-object messages)
                         (mapcar #'nlk:encode-json-object
                                 (nle::history-turn-messages cold))))
              (is (= (nle::history-turn-tokens unit) (nle::history-turn-tokens cold)))
              (is (equal (nle::history-turn-round-starts unit)
                         (nle::history-turn-round-starts cold))))))))))

(deftest a-late-fact-of-a-settled-turn-leaves-the-pin-at-the-end (with-temp-store ())
  ;; A backgrounded call's exit lands its result after its turn completed
  ;; and the next turn began, and the fold splits the next turn around it.
  ;; The pin used to stop at the first unit without a terminal fact, so every
  ;; later round refolded everything behind such a result from the log.
  (nle::clear-hydration-cache)
  (nlk:create-session :id "hcl")
  (let ((session (nle::make-session :id "hcl"))
        (early (nlk:admit-turn "hcl" "hcl-cmd-0" "input 0")))
    (open-turn-round early 0)
    (nlk:complete-turn early nil)
    (let ((next (nlk:admit-turn "hcl" "hcl-cmd-1" "input 1")))
      (open-turn-round next 1)
      (nlk::with-store-transaction (handle)
        (nlk::append-turn-event-row handle early nlk::+kind-tool-result+
                                    (list :call-id "open-0" :tool-name "eval" :result "late")
                                    :owner 'nlk::record-tool-result))
      (open-turn-round next 2)
      (nlk:complete-turn next nil))
    (let ((cold (nle::history-turns-for-session session)))
      (is (= 4 (length cold)) "the late result splits the next turn")
      (is (= (nle::history-turn-end-sequence (car (last cold)))
             (first (gethash "hcl" nle::*hydration-cache*))) "the pin reaches the last unit")
      (let ((afters '()))
        (with-stubbed-fdefinition (nlk:page-events (&rest args)
                                   (push (getf args :after) afters)
                                   (apply original args))
          (let ((warm (nle::history-turns-for-session session)))
            (is (every #'eq cold warm) "a warm round refolds nothing")
            (is (every (lambda (after)
                         (>= after (nle::history-turn-end-sequence (car (last cold)))))
                       afters)))))
      ;; What the pin answers is what a fold from zero answers.
      (nle::clear-hydration-cache)
      (is (equal (mapcar (lambda (unit) (mapcar #'nlk:encode-json-object unit.messages)) cold)
                 (mapcar (lambda (unit) (mapcar #'nlk:encode-json-object unit.messages))
                         (nle::history-turns-for-session session)))))))

(deftest a-fold-that-ends-on-an-open-call-is-no-cut-point (with-temp-store ())
  ;; The call's result records later. A prefix pinned with the fold's own
  ;; stand-in answer would keep answering for it, and the real result would
  ;; fold as an orphan and be dropped.
  (nle::clear-hydration-cache)
  (nlk:create-session :id "hcu")
  (let* ((session (nle::make-session :id "hcu"))
         (turn (nlk:admit-turn "hcu" "hcu-cmd" "ask")))
    (nlk:record-assistant-message
     turn (wire-message "assistant" :content ""
                        :tool-calls (vector (wire-tool-call "hcu-1" "eval" "{}"))))
    (nlk:record-tool-call turn "hcu-1" "eval" "{}")
    (let ((unit (car (last (nle::history-turns-for-session session)))))
      (is (not unit.clean-p) "the open call is not a cut point")
      (is (member nle::+orphan-tool-result+ (unit-contents unit) :test #'equal)))
    (nlk:record-tool-result turn "hcu-1" "eval" "the recorded result")
    (let ((contents (unit-contents (car (last (nle::history-turns-for-session session))))))
      ;; the recorded result answers the call.
      (is (find-if (lambda (content) (eql 0 (search "the recorded result" content))) contents))
      (is (not (member nle::+orphan-tool-result+ contents :test #'equal))))))

(deftest hydration-cache-drops-on-coverage-mismatch (with-temp-store ())
  (nle::clear-hydration-cache)
  (nlk:create-session :id "hcm")
  (let ((session (nle::make-session :id "hcm")))
    (let ((turn (nlk:admit-turn "hcm" "hcm-cmd" "go")))
      (nlk:record-assistant-message
       turn (wire-message "assistant" :content "done"))
      (nlk:complete-turn turn nil))
    (is (= 1 (length (nle::history-turns-for-session session))))
    ;; Corrupt the pin: the covered-prefix recheck must refuse it.
    (let ((entry (gethash "hcm" nle::*hydration-cache*)))
      (is entry "the fold pinned a cache entry")
      (setf (gethash "hcm" nle::*hydration-cache*)
            (list (first entry) (1+ (second entry)) (third entry))))
    (let ((afters '()))
      (with-stubbed-fdefinition (nlk:page-events (&rest args)
                                 (push (getf args :after) afters)
                                 (apply original args))
        (is (= 1 (length (nle::history-turns-for-session session))))
        (is (member 0 afters))))))

(deftest hydration-cache-keeps-the-running-sessions-and-the-warm-idle-ones ()
  (with-saved-globals (nle::*warm-sessions*)
    (setf nle::*warm-sessions* 1)
    (with-temp-store ()
      (nle::clear-hydration-cache)
      (flet ((seeded (id)
               (nlk:create-session :id id)
               (let ((turn (nlk:admit-turn id (format nil "~a-cmd" id) "go")))
                 (nlk:record-assistant-message turn (wire-message "assistant" :content "done"))
                 (nlk:complete-turn turn nil))
               (nle::make-session :id id))
             (cached-p (id) (nth-value 1 (gethash id nle::*hydration-cache*))))
        (let ((a (seeded "warm-a")) (b (seeded "warm-b")) (c (seeded "warm-c")))
          (nle::history-turns-for-session a)
          (is (cached-p "warm-a"))
          (nle::history-turns-for-session b)
          (is (cached-p "warm-b") "the idle session hydrated last stays warm")
          (is (not (cached-p "warm-a")) "the one before it is let go")
          (setf (nle::session-in-flight-p c) t)
          (nle::history-turns-for-session c)
          (nle::history-turns-for-session a)
          (is (cached-p "warm-c") "a session with a turn in flight is never let go")
          (is (cached-p "warm-a"))
          (is (not (cached-p "warm-b")))
          (is (= 1 (length (nle::history-turns-for-session b))))
          (setf (nle::session-in-flight-p c) nil))))))

(deftest turn-token-estimates-memoize-per-unit ()
  (let ((turn (nle::make-history-turn
               :messages (list (nle::message "user" "hello")))))
    (is (null (nle::history-turn-token-estimate turn)))
    (let ((first (nle::history-turn-tokens turn)))
      (is (integerp first))
      (is (= first (nle::history-turn-token-estimate turn)))
      (is (= first (nle::history-turn-tokens turn))))))

(deftest resumed-turn-does-not-duplicate-user-message-capture (with-temp-store ())
  ;; Boot resume re-enters RUN with the pre-admitted turn; the pre-crash run
  ;; already stamped the user_message capture. A second stamp would leave
  ;; duplicate captures whose predecessor resolution is ambiguous for undo.
  (let ((workspace (temp-directory "capdup")))
   (with-workspace-captures (t)
    (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree workspace :validate t)))
      (nlk:create-session :id "capdup" :cwd (namestring workspace))
      (let* ((turn (nlk:admit-turn "capdup" "capdup-cmd" "hello")) (turn-id turn.turn-id))
        ;; The pre-crash run's capture.
        (nlk:checkpoint-capture-boundary
         turn :boundary-kind "user_message"
         :anchor-position (nlk:session-log-cursor "capdup"))
        (is (nlk:turn-boundary-captured-p "capdup" turn-id
                                          "user_message"))
        (with-stub-provider ((wire-message "assistant"
                                           :content "resumed"))
          (nle:run "hello" :session (nle:intern-session "capdup")
                   :command-id "capdup-cmd" :admitted-turn turn))
        (let ((captures (nlk:session-workspace-captures "capdup")))
          (is (= 1 (count-if
                    (lambda (capture)
                      (and (equal (getf capture :turn-id) turn-id)
                           (equal (getf capture :boundary-kind)
                                  "user_message")))
                    captures)))
          (is (= 1 (count-if
                    (lambda (capture)
                      (and (equal (getf capture :turn-id) turn-id)
                           (equal (getf capture :boundary-kind)
                                  "assistant_message")))
                    captures)))))))))

(deftest tool-call-bracket-ships-workspace-diff-metadata (with-temp-store ())
  ;; The tool call's workspace bracket: a write landing between the call's
  ;; pre capture and its reply rides the result fact as workspace_diff
  ;; display metadata, a non-writing call ships none, and the tool_result
  ;; capture stamps the bracket's pre tree so the window stays durable.
  (let ((workspace (temp-directory "wsdiff")))
    (with-workspace-captures (t)
      (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree workspace
                                                                    :validate t)))
        (nlk:create-session :id "wsdiff" :cwd (namestring workspace))
        (with-open-file (out (merge-pathnames "notes.txt" workspace)
                             :direction :output)
          (write-string "one
two
three
" out))
        (let* ((turn (nlk:admit-turn "wsdiff" "wsdiff-cmd" "go"))
               (nle::*current-durable-turn* turn)
               (nle::*tools* nle::*tools*)
               (parameters (make-hash-table :test #'equal)))
          (nle:register-tool
           "mutate_probe" (list :description "d" :parameters parameters)
           (lambda (args)
             (declare (ignore args))
             (with-open-file (out (merge-pathnames "notes.txt"
                                                   workspace)
                                  :direction :output
                                  :if-exists :supersede)
               (write-string "one
TWO
three
" out))
             "wrote"))
          (nle:register-tool
           "noop_probe" (list :description "d" :parameters parameters)
           (lambda (args) (declare (ignore args)) "did nothing"))
          (execute-wire-call "call-m" "mutate_probe" "{}")
          (execute-wire-call "call-n" "noop_probe" "{}")
          (let* ((facts (remove-if-not
                         (lambda (event)
                           (typep event 'nlk:tool-result-event))
                         (nle::collect-visible-session-events "wsdiff")))
                 (mutating (find "call-m" facts
                                 :key #'nlk:tool-result-event-call-id
                                 :test #'equal))
                 (noop (find "call-n" facts
                             :key #'nlk:tool-result-event-call-id
                             :test #'equal))
                 (metadata (nlk::tool-result-event-metadata mutating))
                 (document (and (hash-table-p metadata)
                                (gethash "workspace_diff" metadata))))
            (if (nlk:git-available-p)
                (progn
                  (is (hash-table-p document))
                  (let* ((files (and document
                                     (gethash "files" document)))
                         (file (and (vectorp files)
                                    (plusp (length files))
                                    (aref files 0))))
                    (is (equal "notes.txt"
                               (and file (gethash "path" file))))
                    (is (eql 1 (and file (gethash "added" file))))
                    (is (eql 1 (and file (gethash "removed" file))))
                    (is (search "+TWO"
                                (or (and file (gethash "patch" file))
                                    ""))))
                  (let ((capture
                          (find "call-m"
                                (nlk:session-workspace-captures
                                 "wsdiff")
                                :key (lambda (entry)
                                       (getf entry :tool-call-id))
                                :test #'equal)))
                    (is capture "the tool_result capture stamped")
                    (is (= 40 (length (or (getf capture :pre-tree-sha)
                                          ""))))
                    (is (not (equal (getf capture :pre-tree-sha)
                                    (getf capture :tree-sha))))))
                (is (null document)))
            (is (null (and (hash-table-p
                            (nlk::tool-result-event-metadata noop))
                           (gethash "workspace_diff"
                                    (nlk::tool-result-event-metadata
                                     noop)))))))))))

(deftest a-write-between-two-calls-rides-neither-result (with-temp-store ())
  ;; A write between two calls of one turn belongs to neither: a reading
  ;; call ships no diff, and a writing call ships its own write alone.
  (let ((workspace (temp-directory "wsforeign")))
    (with-workspace-captures (t)
      (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree workspace :validate t)))
        (flet ((put (name content)
                 (write-temp-file (merge-pathnames name workspace) content))
               (diff-paths (call-id)
                 (let* ((fact (find call-id
                                    (remove-if-not
                                     (lambda (event) (typep event 'nlk:tool-result-event))
                                     (nle::collect-visible-session-events "wsforeign"))
                                    :key #'nlk:tool-result-event-call-id
                                    :test #'equal))
                        (metadata (and fact (nlk::tool-result-event-metadata fact)))
                        (document (and (hash-table-p metadata)
                                       (gethash "workspace_diff" metadata))))
                   (and document
                        (map 'list (lambda (file) (gethash "path" file))
                             (gethash "files" document))))))
          (nlk:create-session :id "wsforeign" :cwd (namestring workspace))
          (put "notes.txt" "one")
          (let* ((turn (nlk:admit-turn "wsforeign" "wsforeign-cmd" "go"))
                 (nle::*current-durable-turn* turn)
                 (nle::*tools* nle::*tools*)
                 (parameters (make-hash-table :test #'equal)))
            (nle:register-tool
             "write_probe" (list :description "d" :parameters parameters)
             (lambda (args)
               (put (gethash "name" args) "mine")
               "wrote"))
            (nle:register-tool
             "read_probe" (list :description "d" :parameters parameters)
             (lambda (args) (declare (ignore args)) "read"))
            (execute-wire-call "call-1" "write_probe" "{\"name\": \"notes.txt\"}")
            ;; the provider round: a peer edits the checkout
            (put "peer-a.txt" "theirs")
            (execute-wire-call "call-2" "read_probe" "{}")
            (put "peer-b.txt" "theirs")
            (execute-wire-call "call-3" "write_probe" "{\"name\": \"mine.txt\"}")
            (cond ((nlk:git-available-p)
                   (is (equal '("notes.txt") (diff-paths "call-1")))
                   (is (null (diff-paths "call-2")))
                   (is (equal '("mine.txt") (diff-paths "call-3")))
                   (let ((capture (find "call-2" (nlk:session-workspace-captures "wsforeign")
                                        :key (lambda (entry) (getf entry :tool-call-id))
                                        :test #'equal)))
                     (is (and capture
                              (equal (getf capture :pre-tree-sha)
                                     (getf capture :tree-sha))))))
                  (t (is (every #'null (mapcar #'diff-paths
                                               '("call-1" "call-2" "call-3"))))))))))))

(deftest capture-guard-failure-fails-turn-durably (with-temp-store (store))
  ;; From the moment a turn is durably STARTED, any signal on the way to the
  ;; agent loop must reach FAIL-TURN: a stranded open turn is re-billed by
  ;; boot resume. The capture guard touches the store and can signal
  ;; (SQLITE_BUSY, torn payload) — prove that lands as turn.failed.
  (ensure-durable-session "capguard")
  (with-workspace-captures (t)
    (let ((turn (nlk:admit-turn "capguard" "capguard-cmd" "hello")))
      (with-stubbed-fdefinition (nlk:turn-boundary-captured-p (&rest args)
                                 (error "synthetic store failure"))
        (nlk:with-handlers ((error () nil))
          (nle:run "hello" :session (nle:intern-session "capguard")
                   :command-id "capguard-cmd" :admitted-turn turn))
        (is (nlk::turn-terminal-p (nlk:durable-turn-turn-id turn)))))))

(defun %turn-kind-count (turn-id kind)
  (nlk::with-store-handle (handle)
    (or (sqlite:execute-single
         handle
         "select count(*) from events where turn_id = ? and kind = ?"
         turn-id kind)
        0)))

(deftest cancel-after-stream-return-keeps-durable-closure (with-session-store ("cancel-close"))
  ;; A cancel that lands while the provider streams must not discard a
  ;; round that returned complete: the text and tokens are paid for, so
  ;; the assistant closure and usage commit FIRST and the cancel is
  ;; honored after them.
  (let* ((turn (nlk:admit-turn "cancel-close" "cc-cmd" "hello")) (turn-id turn.turn-id))
    (with-provider-stub (nil
                         (nlk:request-cancel-turn "cancel-close" turn-id "user")
                         (wire-message "assistant" :content "kept answer"))
      (nlk:with-handlers ((nlk:turn-cancelled-condition () nil))
        (nle:run "hello" :session (nle:intern-session "cancel-close")
                 :command-id "cc-cmd" :admitted-turn turn))
      (is (= 1 (%turn-kind-count turn-id
                                 nlk::+kind-assistant-completed+)))
      (is (= 1 (%turn-kind-count turn-id nlk::+kind-turn-cancelled+)))
      (is (= 1 (nlk:turn-assistant-round-count turn-id))))))

(deftest cancel-mid-stream-records-estimated-usage (with-session-store ("cancel-usage"))
  ;; A cancel that cuts the stream leaves the server's usage chunk unsent;
  ;; the resting divider must still read like a finished turn's. The lane's
  ;; handler carries the round's identity and an estimate out on the
  ;; condition, and RUN records them as an ESTIMATED usage fact: identity
  ;; from the config and the stream's own metadata, the prompt from the
  ;; calibrated context estimate, the output from what streamed (chars/4).
  (let* ((turn (nlk:admit-turn "cancel-usage" "cu-cmd" "hello")) (turn-id turn.turn-id))
    (with-streaming-provider-stub
        (context on-part
         (nle::emit-stream-part on-part :stream-start)
         (nle::emit-stream-part on-part :response-metadata :model "served-id")
         (nle::emit-stream-part on-part :reasoning-delta
                                :text (make-string 40 :initial-element #\t))
         (nle::emit-stream-part on-part :text-delta
                                :text (make-string 284 :initial-element #\a))
         (error (make-condition 'nlk:turn-cancelled-condition
                                :turn-id turn-id :reason "esc")))
      (nlk:with-handlers ((nlk:turn-cancelled-condition () nil))
        (nle:run "hello" :session (nle:intern-session "cancel-usage")
                 :command-id "cu-cmd" :admitted-turn turn)))
    (is (= 1 (%turn-kind-count turn-id nlk::+kind-assistant-completed+)))
    (is (= 1 (%turn-kind-count turn-id nlk::+kind-turn-cancelled+)))
    (let ((facts (remove-if-not (lambda (event)
                                  (typep event 'nlk:turn-usage-event))
                                (nle::collect-session-events "cancel-usage"))))
      (is (= 1 (length facts)) "the cut round left exactly one usage fact")
      (is-present (fact (first facts)) "the usage fact exists"
          (is (nlk::turn-usage-event-estimated fact))
          (is (plusp (length (nlk:turn-usage-event-model fact))))
          (is (plusp (length (nlk:turn-usage-event-provider fact))))
          (is (equal "served-id" (nlk:turn-usage-event-response-model fact)))
          (is (= 81 (nlk:turn-usage-event-output-tokens fact)))
          (is (= 10 (nlk:turn-usage-event-reasoning-tokens fact)))
          (is (and (integerp (nlk:turn-usage-event-input-tokens fact))
                   (plusp (nlk:turn-usage-event-input-tokens fact))))))))

(deftest cancel-before-the-stream-opens-records-no-counts (with-session-store ("cancel-unserved"))
  ;; No part arrived: nothing proves the server processed the request, so
  ;; the fact names the identity only — never a count the provider did not
  ;; say.
  (let* ((turn (nlk:admit-turn "cancel-unserved" "cun-cmd" "hello")) (turn-id turn.turn-id))
    (with-streaming-provider-stub
        (context on-part
         (error (make-condition 'nlk:turn-cancelled-condition
                                :turn-id turn-id :reason "esc")))
      (nlk:with-handlers ((nlk:turn-cancelled-condition () nil))
        (nle:run "hello" :session (nle:intern-session "cancel-unserved")
                 :command-id "cun-cmd" :admitted-turn turn)))
    (is (zerop (%turn-kind-count turn-id nlk::+kind-assistant-completed+)))
    (let ((facts (remove-if-not (lambda (event)
                                  (typep event 'nlk:turn-usage-event))
                                (nle::collect-session-events "cancel-unserved"))))
      (is (= 1 (length facts)) "the identity still lands as a usage fact")
      (is-present (fact (first facts)) "the usage fact exists"
          (is (nlk::turn-usage-event-estimated fact) "marked estimated")
          (is (null (nlk:turn-usage-event-input-tokens fact)))
          (is (null (nlk:turn-usage-event-output-tokens fact)))))))

(deftest cancel-mid-stream-records-partial-text (with-session-store ("cancel-partial"))
  ;; A cancel that unwinds the lane mid-stream carries the streamed text on
  ;; the condition; RUN's handler durably closes the round with it (the Zig
  ;; appendOpenAssistantMessageCompletionsLocked backstop) before recording
  ;; the cancel.
  (let* ((turn (nlk:admit-turn "cancel-partial" "cp-cmd" "hello")) (turn-id turn.turn-id))
    (with-provider-stub (nil
                         (let ((condition (make-condition
                                           'nlk:turn-cancelled-condition
                                           :turn-id turn-id :reason "user")))
                           (setf (nlk:turn-cancelled-condition-partial-text
                                  condition)
                                 "partial answer")
                           (error condition)))
      (nlk:with-handlers ((nlk:turn-cancelled-condition () nil))
        (nle:run "hello" :session (nle:intern-session "cancel-partial")
                 :command-id "cp-cmd" :admitted-turn turn))
      (is (= 1 (%turn-kind-count turn-id
                                 nlk::+kind-assistant-completed+)))
      (is (= 1 (%turn-kind-count turn-id nlk::+kind-turn-cancelled+))))))

;;; =========================================================================
;;; Reasoning effort: request wiring, RUN binding, and the /models backing
;;; =========================================================================

(deftest chat-request-omits-reasoning-effort-when-off (let* ((nle::*reasoning-effort* "off")
                                                             (body (nle::request-body
                                                                    (user-context "hi")))))
  (is (null (nth-value 1 (gethash "reasoning_effort" body)))))

(deftest responses-request-spells-off-as-no-reasoning ()
  (let* ((nle::*provider* "openai-responses")
         (nle::*reasoning-effort* "off")
         (body (nle::responses-request-body
                (user-context "hi"))))
    (is (equal "none" (nlk:json-value body :string "reasoning" "effort")))
    (is (null (nth-value 1 (gethash "include" body))))))

(deftest google-request-off-suppresses-thoughts ()
  (let* ((nle::*provider* "google")
         (nle::*reasoning-effort* "off")
         (body (nle::google-request-body
                (user-context "hi")))
         (thinking (gethash "thinkingConfig" (gethash "generationConfig" body))))
    (is (hash-table-p thinking) "off still crosses the wire explicitly")
    (when (hash-table-p thinking)
      (is (eq :false (gethash "includeThoughts" thinking)))
      (is (equal "off" (gethash "thinkingLevel" thinking))))))

(deftest catalog-effort-levels-reads-the-declared-ladder ()
  ;; The models.dev reasoning_options union, projected onto one ladder.
  (flet ((entry (json) (shasht:read-json json)))
    (is (equal '("off" "low" "medium" "high" "xhigh" "max")
               (nle::catalog-effort-levels
                (entry "{\"reasoning_options\":[{\"type\":\"effort\",\"values\":
                         [\"none\",\"low\",\"medium\",\"high\",\"xhigh\",\"max\"]}]}"))))
    (is (equal '("minimal" "low" "medium" "high")
               (nle::catalog-effort-levels
                (entry "{\"reasoning_options\":[{\"type\":\"effort\",\"values\":
                         [\"high\",\"minimal\",\"medium\",\"low\"]}]}"))))
    (is (equal '("low" "high" "max")
               (nle::catalog-effort-levels
                (entry "{\"reasoning_options\":[{\"type\":\"toggle\"},
                         {\"type\":\"effort\",\"values\":[\"low\",\"high\",\"max\"]},
                         {\"type\":\"budget_tokens\",\"min\":1024}]}"))))
    (is (null (nle::catalog-effort-levels
               (entry "{\"reasoning_options\":[{\"type\":\"budget_tokens\",
                        \"min\":1024}]}"))))
    (is (null (nle::catalog-effort-levels (entry "{}"))))
    (is (equal '("low" "high")
               (nle::catalog-effort-levels
                (entry "{\"reasoning_options\":[{\"type\":\"effort\",\"values\":
                         [\"low\",\"default\",null,\"ultra\",\"high\"]}]}")))))
  (is (equal "high" (nle::clamp-effort "max" "high")))
  (is (equal "low" (nle::clamp-effort "low" "high")))
  (is (equal "ultra" (nle::clamp-effort "ultra" "high"))))

(deftest the-declared-ladder-comes-from-the-models-own-namespace (with-catalog-fixture)
  ;; A model id is not unique in models.dev: every reseller republishes the
  ;; row, and their rows disagree about the reasoning vocabulary. Picking
  ;; whichever namespace hashed first cost claude-opus-5 its top two rungs.
  (path "{\"zzz-reseller\":{\"models\":{\"m1\":{\"reasoning\":true,
                  \"reasoning_options\":[{\"type\":\"effort\",
                    \"values\":[\"low\",\"high\"]}]}}},
                \"vendor\":{\"models\":{\"m1\":{\"reasoning\":true,
                  \"reasoning_options\":[{\"type\":\"effort\",
                    \"values\":[\"low\",\"medium\",\"high\",\"xhigh\",\"max\"]}]},
                  \"m2\":{\"reasoning\":true,
                  \"reasoning_options\":[{\"type\":\"budget_tokens\",
                    \"min\":1024}]}}},
                \"aaa-reseller\":{\"models\":{\"m1\":{\"reasoning\":true,
                  \"reasoning_options\":[{\"type\":\"effort\",
                    \"values\":[\"low\",\"medium\",\"high\",\"xhigh\",\"max\"]}]},
                  \"m2\":{\"reasoning\":true,
                  \"reasoning_options\":[{\"type\":\"effort\",
                    \"values\":[\"low\",\"high\",\"max\"]}]}}}}")
(is (equal '("low" "medium" "high" "xhigh" "max")
           (nle::declared-effort-levels "m1" "vendor")))
(is (equal '("low" "high")
           (nle::declared-effort-levels "m1" "zzz-reseller")))
(is (equal '("low" "medium" "high" "xhigh" "max")
           (nle::declared-effort-levels "m1" "some-private-router")))
;; The vendor says m2 spends a budget, not an effort. That
;; silence is a fact: a reseller's ladder must not fill it.
(is (null (nle::declared-effort-levels "m2" "vendor")))
(is (equal '("low" "high" "max")
           (nle::declared-effort-levels "m2" "unknown-router")))
(is (eq t (nle::model-capability-reasoning-p
           (nle::resolve-model-capability "m1" "vendor"))))
(is (nle::anthropic-adaptive-p
     (nle::model-capability-effort-levels
      (nle::resolve-model-capability "m1" "vendor"))))
(is (null (nle::anthropic-adaptive-p
           (nle::model-capability-effort-levels
            (nle::resolve-model-capability "m2" "vendor"))))))

(defparameter *priced-catalog-json*
  "{\"vendor\":{\"models\":{\"m1\":{\"cost\":{\"input\":2,\"output\":10,\"cache_read\":0.2,\"cache_write\":2.5}}}},
    \"aaa-reseller\":{\"models\":{\"m1\":{\"cost\":{\"input\":2,\"output\":10}}}},
    \"bbb-reseller\":{\"models\":{\"m1\":{\"cost\":{\"input\":2,\"output\":10,\"cache_read\":0.2,\"cache_write\":2.5}}}},
    \"zero-plan\":{\"models\":{\"m1\":{\"cost\":{\"input\":0,\"output\":0}}}},
    \"free\":{\"models\":{\"m2\":{\"name\":\"no price\"}}}}"
  "A catalog where m1's rows disagree about its price and m2 has none.")

(deftest a-price-is-the-providers-own-row-else-the-rows-consensus
    (with-catalog-fixture (path *priced-catalog-json*))
  (is (equal '(2 10 0.2d0 2.5d0) (nle::model-price "m1" "vendor")))
  ;; A provider's own row wins even when it leaves a price out.
  (is (equal '(2 10 nil nil) (nle::model-price "m1" "aaa-reseller")))
  ;; aaa-reseller sorts first and forgot its cache price; the consensus
  ;; passes over it, and over the plan row that says zero.
  (is (equal '(2 10 0.2d0 2.5d0) (nle::model-price "m1" "private-relay")))
  (is (equal '(2 10 0.2d0 2.5d0) (nle::model-price "m1" "zero-plan")))
  (is (null (nle::model-price "m2" "free")))
  (is (null (nle::model-price "nowhere" "vendor"))))

(deftest a-rounds-cost-bills-each-count-at-its-own-price ()
  (is (= 0.027d0 (nle::round-cost '(2 10 0.2d0 2.5d0)
                                  :input-tokens 1000 :cached-input-tokens 100000
                                  :output-tokens 500 :cache-write-tokens 0)))
  ;; An unpublished cache price bills at the input price.
  (is (= 2d0 (nle::round-cost '(2 10 nil nil) :cached-input-tokens 1000000)))
  (is (null (nle::round-cost nil :input-tokens 5)))
  ;; No count at all is no cost, not a zero.
  (is (null (nle::round-cost '(2 10 nil nil)))))

(defparameter *tiered-catalog-json*
  "{\"vendor\":{\"models\":{\"m3\":{\"cost\":{\"input\":2,\"output\":10,\"cache_read\":0.2,
      \"tiers\":[{\"input\":6,\"output\":20,\"tier\":{\"type\":\"context\",\"size\":272000}},
                 {\"input\":4,\"output\":15,\"cache_read\":0.4,\"tier\":{\"type\":\"context\",\"size\":200000}}],
      \"context_over_200k\":{\"input\":99,\"output\":99}}}}}}"
  "A catalog where m3 costs more past 200k tokens and more again past 272k,
and a context_over_200k no reader may trust: it names no size.")

(deftest a-long-prompt-bills-at-the-tier-it-is-past
    (with-catalog-fixture (path *tiered-catalog-json*))
  (let ((price (nle::model-price "m3" "vendor")))
    ;; Each tier as its size and prices, smallest first, whatever order the
    ;; row lists them in; context_over_200k is not one of them.
    (is (equal '(2 10 0.2d0 nil (200000 4 15 0.4d0 nil) (272000 6 20 nil nil)) price))
    (flet ((near (expected actual) (< (abs (- expected actual)) 1d-9)))
      (is (near 0.3d0 (nle::round-cost price :input-tokens 150000)))
      ;; At the size, not past it: the base price.
      (is (near 0.4d0 (nle::round-cost price :input-tokens 200000)))
      ;; The prompt is fresh, read and written together: 250k is past 200k,
      ;; and the whole round bills at that tier, output included.
      (is (near 0.475d0 (nle::round-cost price :input-tokens 100000 :cached-input-tokens 150000
                                               :output-tokens 1000)))
      ;; Past 272k, where the tier publishes no cache price: its input price.
      (is (near 1.8d0 (nle::round-cost price :input-tokens 100000 :cached-input-tokens 200000))))))

(deftest what-the-cache-saved-is-net-of-what-writing-it-cost ()
  (flet ((near (expected actual) (< (abs (- expected actual)) 1d-9)))
    ;; 100k read at 0.2 instead of 2.
    (is (near 0.18d0 (nle::round-cache-saving '(2 10 0.2d0 2.5d0)
                                              :input-tokens 1000 :cached-input-tokens 100000
                                              :output-tokens 500)))
    ;; A round that only wrote the cache paid its premium and saved nothing.
    (is (near -0.05d0 (nle::round-cache-saving '(2 10 0.2d0 2.5d0) :cache-write-tokens 100000)))
    ;; Past a tier, the saving is that tier's.
    (is (near 0.9d0 (nle::round-cache-saving '(2 10 0.2d0 nil (200000 4 15 0.4d0 nil))
                                             :cached-input-tokens 250000)))
    ;; Unpublished cache prices bill at the input price: no saving, none invented.
    (is (near 0d0 (nle::round-cache-saving '(2 10 nil nil) :cached-input-tokens 100000)))
    (is (null (nle::round-cache-saving nil :cached-input-tokens 100000)))))

(deftest a-rounds-usage-fact-carries-its-price
    (with-session-store ("priced-usage"))
  (with-catalog-fixture (path *priced-catalog-json*)
   (nlk:bind ((nle::*model* "m1"))
    (with-streaming-provider-stub
        (context on-part
         (values (wire-message "assistant" :content "counted")
                 (nle::make-provider-usage :input-tokens 1000 :output-tokens 500
                                           :cached-input-tokens 100000 :total-tokens 101500)
                 "stop"))
      (nle:run "count me" :session (nle:intern-session "priced-usage")
               :command-id "priced-usage-cmd"))))
  (let ((fact (find-if (lambda (event) (typep event 'nlk:turn-usage-event))
                       (nle::collect-session-events "priced-usage"))))
    (is (eq t (nlk:turn-usage-event-cost-known fact)))
    (is (< (abs (- 0.027d0 (nlk:turn-usage-event-cost-usd fact))) 1d-6))))

(deftest an-unpriced-rounds-cost-stays-unknown
    (with-session-store ("unpriced-usage"))
  (with-catalog-fixture (path *priced-catalog-json*)
   (nlk:bind ((nle::*model* "m2"))
    (with-streaming-provider-stub
        (context on-part
         (values (wire-message "assistant" :content "counted")
                 (nle::make-provider-usage :input-tokens 10 :output-tokens 5)
                 "stop"))
      (nle:run "count me" :session (nle:intern-session "unpriced-usage")
               :command-id "unpriced-usage-cmd"))))
  (let ((fact (find-if (lambda (event) (typep event 'nlk:turn-usage-event))
                       (nle::collect-session-events "unpriced-usage"))))
    (is (null (nlk:turn-usage-event-cost-known fact)))))

(deftest an-undeclared-model-offers-every-rung (let ((nle::*models-catalog-path* nil)))
  ;; Unknown is not "no", and it is not "less" either: guessing a narrower
  ;; ladder would hide rungs that work, while a rung the endpoint cannot take
  ;; fails loudly at the provider.
  (flet ((rungs (provider model) (nle::effort-ladder provider model)))
    (is (equal '("off" "minimal" "low" "medium" "high" "xhigh" "max")
               (rungs "openai-responses" "gpt-never-catalogued")))
    (let ((nle::*model-capability-overrides*
            '(("private-lane" . (:effort-levels ("low" "high"))))))
      (is (equal '("off" "low" "high") (rungs "openai-completions" "private-lane")))
      (is (null (nle::admitted-effort "xhigh" "openai-completions" "private-lane")) "a rung off the ladder is no pick")
      (let ((nle::*reasoning-effort* "low"))
        (is (equal "low" (nle::effective-effort "xhigh" "openai-completions" "private-lane")) "so the turn runs at the default"))
      (is (nth-value 1 (ignore-errors (nle::check-effort "openai-completions" "private-lane" "xhigh"))) "and picking it refuses"))))

(deftest anthropic-thinks-adaptively-when-the-ladder-reaches-past-high ()
  ;; A model whose declared ladder names xhigh or max thinks adaptively: it
  ;; decides how much to spend, and the request carries only the effort name.
  (nlk:bind ((nle::*models-catalog-path* nil) (nle::*model* "claude-adaptive-test")
             (nle::*model-capability-overrides*
               '(("claude-adaptive-test"
                  . (:effort-levels ("low" "medium" "high" "xhigh" "max")))))
             (nle::*max-completion-tokens* 8192) (nle::*temperature* 0.7)
             (nle::*reasoning-effort* "xhigh")
             (body (nle::anthropic-request-body (anthropic-user-context)))
             (thinking (gethash "thinking" body)) (output-config (gethash "output_config" body)))
    (is (equal "adaptive" (gethash "type" thinking)))
    (is (equal "summarized" (gethash "display" thinking)))
    (is (null (nth-value 1 (gethash "budget_tokens" thinking))))
    (is (equal "xhigh" (gethash "effort" output-config)))
    (is (null (nth-value 1 (gethash "temperature" body))))))

(deftest anthropic-adaptive-off-disables-thinking ()
  (nlk:bind ((nle::*models-catalog-path* nil) (nle::*model* "claude-adaptive-test")
             (nle::*model-capability-overrides*
               '(("claude-adaptive-test"
                  . (:effort-levels ("low" "medium" "high" "xhigh" "max")))))
             (nle::*temperature* 0.7) (nle::*reasoning-effort* "off")
             (body (nle::anthropic-request-body (anthropic-user-context)))
             (thinking (gethash "thinking" body)))
    (is (equal "disabled" (gethash "type" thinking)))
    (is (null (nth-value 1 (gethash "output_config" body))))
    (is (= 0.7 (gethash "temperature" body)))))

(defun adaptive-body-declaring (declared effort)
  "An adaptive model's request at EFFORT, its endpoint having declared the
thinking types DECLARED (a list, :UNDECLARED, or NIL for never asked)."
  (nlk:bind ((nle::*models-catalog-path* nil) (nle::*model* "claude-adaptive-test")
             (nle::*model-capability-overrides*
               '(("claude-adaptive-test"
                  . (:effort-levels ("low" "medium" "high" "xhigh" "max")))))
             (nle::*temperature* 0.7) (nle::*reasoning-effort* effort)
             (nle::*anthropic-thinking-types* (make-hash-table :test #'equal))
             (context (anthropic-user-context))
             (config (nle::compiled-turn-context-provider-config context)))
    (when declared
      (setf (gethash (cons (nle::anthropic-models-url (nle::effective-provider-config-endpoint config))
                           "claude-adaptive-test")
                     nle::*anthropic-thinking-types*)
            declared))
    (nle::anthropic-request-body context)))

(deftest anthropic-off-is-the-spelling-the-model-declares ()
  ;; Off is no one spelling: Sonnet 5.5 refuses "disabled" and takes
  ;; "between_tools", Opus 5.5 refuses both because it always thinks, and
  ;; either refusal is a 400 that fails the turn. What the endpoint's Models
  ;; API declares picks; an endpoint that declares nothing keeps "disabled".
  (flet ((off-type (declared effort)
           (gethash "type" (gethash "thinking" (adaptive-body-declaring declared effort)))))
    (let* ((body (adaptive-body-declaring '("adaptive" "between_tools") "off"))
           (thinking (gethash "thinking" body)))
      (is (equal "between_tools" (gethash "type" thinking)) "sonnet 5.5's off")
      (is (null (nth-value 1 (gethash "display" thinking))) "between_tools refuses a display")
      (is (null (nth-value 1 (gethash "output_config" body)))))
    (is (equal "between_tools" (off-type '("adaptive" "between_tools") nil)) "an effort never set is off too")
    (is (equal "disabled" (off-type '("adaptive" "between_tools" "disabled") "off")))
    (is (equal "disabled" (off-type :undeclared "off")) "an endpoint that declares nothing keeps disabled")
    (is (equal "disabled" (off-type nil "off")) "and so does one never asked")
    (is (equal "adaptive" (off-type '("adaptive" "between_tools") "high")) "a rung thinks, whatever off is")
    (let ((body (adaptive-body-declaring '("adaptive") "off")))
      (is (equal "adaptive" (gethash "type" (gethash "thinking" body))) "a model that always thinks")
      (is (equal "low" (gethash "effort" (gethash "output_config" body))) "spends its weakest rung")
      (is (null (nth-value 1 (gethash "temperature" body))) "and samples as a thinking request does"))))

(defun models-api-config (url)
  "A config whose Messages endpoint sits beside URL's fixture server."
  (nle::make-effective-provider-config
   :provider "anthropic" :lane "anthropic" :model "claude-x" :api-key "k"
   :endpoint (ppcre:regex-replace "/api\\.json$" url "/v1/messages")))

(deftest anthropic-thinking-types-are-asked-once-of-the-models-api ()
  ;; The fixture answers one request and closes, so a second read that
  ;; dialed would hear nothing: the kept answer is what it reads.
  (multiple-value-bind (url server)
      (serve-catalog-once "{\"id\": \"claude-x\", \"capabilities\": {\"thinking\": {\"supported\": true, \"types\": {\"adaptive\": {\"supported\": true}, \"between_tools\": {\"supported\": true}, \"disabled\": {\"supported\": false}, \"enabled\": {\"supported\": false}}}}}")
    (nlk:bind ((nle::*anthropic-thinking-types* (make-hash-table :test #'equal))
               (nle::*provider-models-cache-path* "listing.json")
               (config (models-api-config url)))
      (flet ((declared () (sort (copy-list (nle::anthropic-thinking-types config :fetch t)) #'string<)))
        (is (equal '("adaptive" "between_tools") (declared)))
        (bt2:join-thread server)
        (is (equal '("adaptive" "between_tools") (declared)) "kept, not asked again")
        (is (equal "between_tools" (nle::anthropic-thinking-off config)))))))

(deftest anthropic-thinking-types-keep-a-relay-answer-and-not-a-silence ()
  ;; A relay serving no Models API answers with something else: that is its
  ;; answer, kept, and off stays "disabled". A server nobody hears is no
  ;; answer, and the next request asks again.
  (multiple-value-bind (url server) (serve-catalog-once "<html>a relay's web page</html>")
    (nlk:bind ((nle::*anthropic-thinking-types* (make-hash-table :test #'equal))
               (nle::*provider-models-cache-path* "listing.json")
               (config (models-api-config url)))
      (is (null (nle::anthropic-thinking-types config :fetch t)))
      (bt2:join-thread server)
      (is (eq :undeclared (gethash (cons (nle::anthropic-models-url (nle::effective-provider-config-endpoint config)) "claude-x")
                                   nle::*anthropic-thinking-types*)))
      (is (equal "disabled" (nle::anthropic-thinking-off config)))
      (setf nle::*anthropic-thinking-types* (make-hash-table :test #'equal))
      (is (null (nle::anthropic-thinking-types config :fetch t)) "the closed port answers nothing")
      (is (zerop (hash-table-count nle::*anthropic-thinking-types*)) "and nothing is kept"))))

(deftest a-hermetic-anthropic-lane-asks-no-models-api ()
  (nlk:bind ((nle::*anthropic-thinking-types* (make-hash-table :test #'equal))
             (nle::*provider-models-cache-path* nil)
             (config (models-api-config "http://127.0.0.1:9/api.json")))
    (is (null (nle::anthropic-thinking-types config :fetch t)))
    (is (zerop (hash-table-count nle::*anthropic-thinking-types*)))))

(deftest anthropic-budget-thinking-survives-a-ladder-that-stops-at-high ()
  (nlk:bind ((nle::*models-catalog-path* nil) (nle::*model* "claude-budget-test")
             (nle::*model-capability-overrides*
               '(("claude-budget-test" . (:effort-levels ("low" "medium" "high")))))
             (nle::*max-completion-tokens* 32000) (nle::*reasoning-effort* "medium")
             (body (nle::anthropic-request-body (anthropic-user-context)))
             (thinking (gethash "thinking" body)))
    (is (equal "enabled" (gethash "type" thinking)))
    (is (= 8000 (gethash "budget_tokens" thinking)))
    (is (null (nth-value 1 (gethash "output_config" body))))))

(deftest anthropic-budget-thinking-clamps-an-extended-rung ()
  ;; The rung above `high` is an adaptive vocabulary, not a bigger number: a
  ;; budget model asked for it buys its deepest budget rather than silently
  ;; sending no thinking block at all.
  (nlk:bind ((nle::*models-catalog-path* nil) (nle::*model* "claude-budget-test")
             (nle::*model-capability-overrides*
               '(("claude-budget-test" . (:effort-levels ("low" "medium" "high")))))
             (nle::*max-completion-tokens* 32000) (nle::*reasoning-effort* "max")
             (body (nle::anthropic-request-body (anthropic-user-context)))
             (thinking (gethash "thinking" body)))
    (is (equal "enabled" (gethash "type" thinking)))
    (is (= 16000 (gethash "budget_tokens" thinking))))
  (is (= 1024 (nle::anthropic-thinking-budget "minimal")))
  (is (null (nle::anthropic-thinking-budget "off"))))

(deftest google-request-clamps-an-extended-rung ()
  (let* ((nle::*provider* "google")
         (nle::*reasoning-effort* "max")
         (body (nle::google-request-body (user-context "hi")))
         (thinking (gethash "thinkingConfig" (gethash "generationConfig" body))))
    (is (equal "high" (gethash "thinkingLevel" thinking)))
    (is (eq t (gethash "includeThoughts" thinking)))))

(deftest responses-request-carries-an-extended-rung-verbatim ()
  (let* ((nle::*provider* "openai-responses")
         (nle::*reasoning-effort* "xhigh")
         (body (nle::responses-request-body (user-context "hi")))
         (reasoning (gethash "reasoning" body)))
    (is (equal "xhigh" (gethash "effort" reasoning)))
    (is (equal "auto" (gethash "summary" reasoning))))
  (let* ((nle::*provider* "openai-completions")
         (nle::*reasoning-effort* "max")
         (body (nle::request-body (user-context "hi"))))
    (is (equal "max" (gethash "reasoning_effort" body)))))

(deftest effort-ladder-follows-capability ()
  ;; Unknown is not "no": an uncataloged model keeps the reasoning ladder, and
  ;; a catalog-declared non-reasoning model has no rung to pick.
  (is (nle::effort-ladder "anthropic" "claude-not-released-yet") "unknown model keeps the effort ladder")
  (is (nle::effort-ladder "openai-responses" "gpt-brand-new"))
  (let ((nle::*model-capability-overrides* '(("chatty-model" . (:reasoning-p nil)))))
    (is (null (nle::effort-ladder "openai-responses" "chatty-model")))
    (let ((refusal (nth-value 1 (ignore-errors (nle::check-effort "openai-responses" "chatty-model" "high")))))
      (is (search "does not reason" (princ-to-string refusal)) "a pick on it refuses, saying why"))))

(deftest run-binds-the-sessions-own-effort-pick (with-session-store ("ctl-run"))
  ;; The RUN dynamic-binding parity: a session's /think pick rides into the
  ;; frozen turn config exactly like its model pin does, over the default
  ;; effort as the pin is over the default model. A session that picked none
  ;; runs at the default.
  (nlk:record-session-model-selection "ctl-run" :effort "high")
  (ensure-durable-session "ctl-other")
  (let ((seen-effort :unset))
    (flet ((run-once (session-id)
             (setf seen-effort :unset)
             (nle:run "hello" :session (nle:intern-session session-id))
             seen-effort))
      (with-provider-stub
          (context
           (setf seen-effort
                 (nle::effective-provider-config-reasoning-effort context.provider-config))
           (wire-message "assistant" :content "ok"))
        (let ((nle::*reasoning-effort* "low"))
          (is (equal "high" (run-once "ctl-run")) "the pick outranks the default")
          (is (equal "low" (run-once "ctl-other")) "a session that picked none runs at the default"))
        (let ((nle::*reasoning-effort* nil))
          (is (equal "high" (run-once "ctl-run"))))))))

(deftest a-session-keeps-the-model-its-first-turn-ran (with-session-store ("begun"))
  ;; A pick in one session is the default for new sessions, and a session
  ;; already begun keeps its own. The first turn pins the pair it ran and
  ;; the effort it ran at — the session's own pick, else the default's,
  ;; which an effort pick moves too — so a later move of the default reaches
  ;; only a session that has not run yet.
  (nlk:record-session-model-selection "begun" :effort "high")
  (ensure-durable-session "plain")
  (ensure-durable-session "later")
  (let ((seen nil))
    (flet ((run-once (session-id)
             (setf seen nil)
             (nle:run "hello" :session (nle:intern-session session-id))
             seen))
      (with-provider-stub
          (context
           (setf seen (list (nle::effective-provider-config-model context.provider-config)
                            (nle::effective-provider-config-reasoning-effort context.provider-config)))
           (wire-message "assistant" :content "ok"))
        (let ((nle::*provider* "openai-completions") (nle::*model* "first-default")
              (nle::*reasoning-effort* "medium"))
          (is (equal '("first-default" "high") (run-once "begun")))
          (is (equal '("first-default" "medium") (run-once "plain")) "a session that picked none runs at the default"))
        (is (equal '("openai-completions" "first-default")
                   (multiple-value-list (nlk:session-model-selection "begun"))) "the first turn pinned the pair it ran")
        (is (equal "high" (nlk:session-model-effort "begun")) "with the session's own reasoning pick")
        (is (equal "medium" (nlk:session-model-effort "plain")) "or the default effort it ran at")
        (let ((nle::*provider* "openai-completions") (nle::*model* "moved-default")
              (nle::*reasoning-effort* "max"))
          (is (equal '("first-default" "high") (run-once "begun")) "the default moved: the begun session keeps its model")
          (is (equal '("first-default" "medium") (run-once "plain")) "and its effort")
          (is (equal '("moved-default" "max") (run-once "later")) "a session not yet begun starts on the new one"))))))

(deftest provider-auth-state-and-api-key-save (with-temp-file (auth-path))
  ;; The /models backing: an entered key saves into the SHARED auth.json in
  ;; the Zig writeApiKeyAuthFile shape, the state listing reads it back, and
  ;; the ordinary credential chain resolves it.
  (let ((nle::*api-key* nil))
    (is (equal "anthropic"
               (nle::save-provider-api-key
                "  Anthropic " "sk-test-123" :auth-path auth-path)))
    (let* ((parsed (shasht:read-json (uiop:read-file-string auth-path)))
           (entry (gethash "anthropic" (gethash "api_keys" parsed))))
      (is (equal "anthropic" (gethash "provider" entry)))
      (is (equal "sk-test-123" (gethash "key" entry))))
    (is (eq :api-key (nle::provider-auth-state
                      "anthropic" :auth-path auth-path)))
    (is (equal "sk-test-123"
               (nle::credential-key (nle::resolve-provider-credential
                                     "anthropic" :auth-path auth-path))))
    ;; A second save must not clobber the first entry.
    (nle::save-provider-api-key
     "google" "g-key" :auth-path auth-path)
    (let ((parsed (shasht:read-json (uiop:read-file-string auth-path))))
      (is (gethash "anthropic" (gethash "api_keys" parsed)))
      (is (equal "g-key" (gethash "key"
                                  (gethash "google"
                                           (gethash "api_keys" parsed))))))
    (is (eq :api-key (nle::provider-auth-state
                      "google" :auth-path auth-path)))))

;;; --- the live turn's image attachments -------------------------------------

(deftest turn-image-registry-appends-and-forgets ()
  (nle::forget-turn-images "registry-turn")
  (nle::attach-turn-images "registry-turn" '(("image/png" . "QQ==")))
  (nle::attach-turn-images "registry-turn" '(("image/jpeg" . "Qg==")))
  (is (equal '((nil ("image/png" . "QQ==") ("image/jpeg" . "Qg==")))
             (nle::turn-images "registry-turn")))
  ;; What a tool attaches lands under its call id, its own group behind the
  ;; inputs'; a second attachment from the same call joins that group.
  (nle::attach-turn-images "registry-turn" '(("image/png" . "Qw=="))
                           :anchor "call-look")
  (nle::attach-turn-images "registry-turn" '(("image/gif" . "RA=="))
                           :anchor "call-look")
  (is (equal '((nil ("image/png" . "QQ==") ("image/jpeg" . "Qg=="))
               ("call-look" ("image/png" . "Qw==") ("image/gif" . "RA==")))
             (nle::turn-images "registry-turn")))
  (nle::forget-turn-images "registry-turn")
  (is (null (nle::turn-images "registry-turn"))))

(deftest run-attaches-images-to-the-turn-it-started (with-session-store ("turn-images"))
  (let ((seen '())
        (session (nle:intern-session "turn-images")))
    (with-provider-stub (context
                         (push (coerce
                                context.messages
                                'list)
                               seen)
                         (wire-message "assistant" :content "ok"))
      (nle:run "[Image #1] what is this?" :session session
               :command-id "turn-images-1"
               :images '(("image/png" . "QUJD")))
      (nle:run "and now?" :session session :command-id "turn-images-2"))
    (let* ((rounds (reverse seen))
           (first-round (first rounds))
           (second-round (second rounds)))
      (let ((parts (nle::message-content-parts
                    (gethash "content" (car (last first-round))))))
        (is (= 2 (length parts)))
        (is (equal "[Image #1] what is this?" (gethash "text" (first parts))))
        (is (equal "data:image/png;base64,QUJD"
                   (gethash "url" (gethash "image_url" (second parts))))))
      (is (equal "[Image #1] what is this?"
                 (gethash "content" (first second-round)))))))

(deftest chat-request-carries-the-visible-turn-images ()
  ;; The chat lane sends typed parts verbatim — the OpenAI-compatible
  ;; multipart shape is the same one the engine compiles — so an attachment
  ;; reaches the default lane without a lane-specific conversion.
  (let* ((context (local-context
                   (list (nle::message "user" "[Image #1] what is this?"))
                   :visible-images '((nil ("image/png" . "QUJD")))))
         (body (nle::request-body context))
         (messages (gethash "messages" body))
         (content (gethash "content" (aref messages 1))))
    (is (= 2 (length content)) "the user message rides as two typed parts")
    (is (equal "text" (gethash "type" (aref content 0))))
    (is (equal "image_url" (gethash "type" (aref content 1))))
    (is (equal "data:image/png;base64,QUJD"
               (gethash "url" (gethash "image_url" (aref content 1)))))))

(deftest call-provider-withholds-the-images-a-provider-refused (with-session-store ("withhold"))
  ;; The round is re-sent without its images once, at once, a note where
  ;; each stood. One failure is no verdict on them -- an outage fails a
  ;; request with images as a refusal does -- so the turn's staging keeps
  ;; them, the next round carries them, and LOOK still shows the next one.
  (let ((prompt "[Image #1] what is this?"))
    (with-durable-turn (turn "withhold" :command-id "withhold-cmd" :input prompt
                             :current t :retry-delay 0)
      (let* ((turn-id turn.turn-id)
             (calls 0)
             (seen '()))
        (nle::forget-turn-images turn-id)
        (nle::attach-turn-images turn-id '(("image/png" . "QUJD")))
        (nlk:with-cleanup ((nle::forget-turn-images turn-id))
          (with-streaming-provider-stub
              (context on-part
               (incf calls)
               (push (coerce context.messages 'list)
                     seen)
               (if (= calls 1)
                   (error 'nle::provider-error
                          :status 503 :detail "upstream unavailable"
                          :scope :request)
                   (wire-message "assistant" :content "read it another way")))
            ;; A sentinel model on the request's own config: the note names
            ;; the identity the failing request targeted.
            (let ((reply (nle::call-provider
                          (local-context (list (nle::message "user" prompt))
                                         :visible-images
                                         '((nil ("image/png" . "QUJD")))
                                         :provider-config
                                         (test-provider-config :model "text-only-alias")))))
              (is (equal "read it another way" (gethash "content" reply))))
            (is (not (nle::turn-images-withheld-p turn-id)) "one failure does not blind the turn")
            (is (equal '(nil ("image/png" . "QUJD")) (first (nle::turn-images turn-id))) "the staging keeps the image")
            (let* ((next (nle::compiled-turn-context-messages
                          (compiled-context (list (nle::message "user" prompt)))))
                   (parts (nle::message-content-parts
                           (gethash "content" (aref next 0)))))
              (is (equal '("text" "image_url") (mapcar #'nle::content-part-type parts)) "the next round carries it again"))))
        (is (= 2 calls) "one refusal, one re-send, no ladder")
        (let* ((rounds (reverse seen))
               (first-parts (nle::message-content-parts
                             (gethash "content" (car (last (first rounds))))))
               (again-parts (nle::message-content-parts
                             (gethash "content" (car (last (second rounds)))))))
          (is (equal '("text" "image_url")
                     (mapcar #'nle::content-part-type first-parts)))
          (is (equal '("text" "text") (mapcar #'nle::content-part-type again-parts)))
          (is (equal prompt (gethash "text" (first again-parts))))
          (is-carrying (note (gethash "text" (second again-parts)))
            ("1 image withheld" note)
            (is (search "openai-completions text-only-alias did not accept" note))
            ("status 503" "and the status")
            ("upstream unavailable" "and the provider's detail")
            ("carries it again" "and that the next request tries it again")))
        (let ((retries (remove-if-not (lambda (event)
                                        (typep event 'nlk:provider-retry-event))
                                      (nle::collect-session-events "withhold"))))
          (is (= 1 (length retries)) "the withholding is one durable retry fact")
          (is (= 503 (nlk:provider-retry-event-status (first retries))))
          (is (search "1 image withheld after: upstream unavailable"
                      (nlk::provider-retry-event-detail (first retries))))
          (is (zerop (nlk::provider-retry-event-delay-seconds (first retries)))))))))

(deftest a-second-failed-request-with-images-withholds-them-for-the-turn (with-session-store ("withhold-twice"))
  ;; A provider that fails every request carrying an image costs one extra
  ;; failed request, never a blind turn after one outage: the first round
  ;; withholds the image from itself alone, the second marks the staging,
  ;; and from then on no round carries the bytes and LOOK says why.
  (let ((prompt "[Image #1] what is this?"))
    (with-durable-turn (turn "withhold-twice" :command-id "withhold-twice-cmd" :input prompt
                             :current t :retry-delay 0)
      (let ((turn-id turn.turn-id)
            (calls 0)
            (with-images 0)
            (last-note nil))
        (nle::forget-turn-images turn-id)
        (nle::attach-turn-images turn-id '(("image/png" . "QUJD")))
        (nlk:with-cleanup ((nle::forget-turn-images turn-id))
          (with-streaming-provider-stub
              (context on-part
               (incf calls)
               (let ((parts (nle::message-content-parts
                             (gethash "content" (aref context.messages 0)))))
                 (cond ((find "image_url" parts :key #'nle::content-part-type :test #'equal)
                        (incf with-images)
                        (error 'nle::provider-error :status 503 :detail "no images here"
                                                    :scope :request))
                       (t (setf last-note (gethash "text" (second parts)))
                          (wire-message "assistant" :content "ok")))))
            (flet ((round-now ()
                     (nle::call-provider
                      (local-context (list (nle::message "user" prompt))
                                     :visible-images (nle::turn-images turn-id)))))
              (round-now)
              (is (not (nle::turn-images-withheld-p turn-id)) "after one failure the image stays")
              (is (search "carries it again" last-note) last-note)
              (round-now)
              (is (nle::turn-images-withheld-p turn-id) "after the second the turn withholds it")
              (is (eq :withheld (car (second (first (nle::turn-images turn-id))))))
              (is-carrying (note last-note)
                ("the second this turn" "the note says why now")
                ("select-model" "and the way out"))
              (round-now))))
        (is (= 2 with-images) "the image went out twice, never a third time")
        (is (= 5 calls) "two refused, two re-sent, one round with no image at all")))))

(deftest a-refused-connection-keeps-the-images (with-session-store ("withhold-refused"))
  ;; A port nothing listens on is not the images' fault: the round stands with
  ;; them, and no later round is told they were refused.
  (let ((prompt "[Image #1] what is this?"))
    (with-durable-turn (turn "withhold-refused" :command-id "withhold-refused-cmd" :input prompt
                             :current t :retry-delay 0)
      (let ((turn-id turn.turn-id) (calls 0))
        (nle::forget-turn-images turn-id)
        (nle::attach-turn-images turn-id '(("image/png" . "QUJD")))
        (nlk:with-cleanup ((nle::forget-turn-images turn-id))
          (with-streaming-provider-stub
              (nil nil
               (incf calls)
               (error 'nle::provider-connection-refused
                      :detail "nothing listens at 127.0.0.1:1 — is the server running?"))
            (is (signals-error nle::provider-connection-refused
                  (nle::call-provider (local-context (list (nle::message "user" prompt))
                                                     :visible-images '((nil ("image/png" . "QUJD")))))))
            (is (not (nle::turn-images-withheld-p turn-id)) "the images stay")))
        (is (= 1 calls) "no re-send without them")))))

(deftest call-provider-withholds-images-once-then-the-verdict-is-the-lanes ()
  ;; The second failure of the same round is the lane's own: the flat test
  ;; config has no request budget, so it stands. One request with the
  ;; images, one without, never a third carrying them.
  (with-session-store ("withhold-once")
    (with-durable-turn (turn "withhold-once" :command-id "withhold-once-cmd" :input "[Image #1] x" :current t :retry-delay 0)
      (let* ((calls 0)
             (with-images 0))
        (with-streaming-provider-stub
            (context on-part
             (incf calls)
             (when (find "image_url"
                         (nle::message-content-parts (gethash "content" (aref context.messages 0)))
                         :key #'nle::content-part-type :test #'equal)
               (incf with-images))
             (error 'nle::provider-error :status 503 :detail "down"
                                         :scope :request))
          (multiple-value-bind (value condition)
              (ignore-errors
               (nle::call-provider
                (local-context (list (nle::message "user" "[Image #1] x"))
                               :visible-images '((nil ("image/png" . "QUJD"))))))
            (is (null value) "the second refusal is not absorbed")
            (is (typep condition 'nle::provider-error))))
        (is (= 1 with-images) "the images went out once")
        (is (= 2 calls) "refused with images, refused without, then it stands")))))

(deftest call-provider-does-not-withhold-from-a-request-without-images ()
  ;; A round with nothing to withhold takes the ladder as it always did:
  ;; the flat test config has no request budget, so the first failure stands.
  (with-session-store ("withhold-none")
    (with-durable-turn (turn "withhold-none" :command-id "withhold-none-cmd" :input "x" :current t :retry-delay 0)
      (let* ((calls 0))
        (with-streaming-provider-stub
            (context on-part
             (incf calls)
             (error 'nle::provider-error :status 503 :detail "down"
                                         :scope :request))
          (is (typep (nth-value 1 (ignore-errors
                                   (nle::call-provider
                                    (local-context (list (nle::message "user" "x"))))))
                     'nle::provider-error)))
        (is (= 1 calls) "no image, no re-send")))))

;;; --- the scribe's residency contract --------------------------

(deftest help-teaches-the-layer-lifecycle ()
  ;; defining is keeping. The layer topic names the layer, the
  ;; one blind spot (a definition evaled inside another form), advice over
  ;; override, the undo, the report, and the reads — read on demand now,
  ;; where the prompt used to carry it at every round.
  (is-carrying (text (nle:help :layer))
    ("Defining is keeping" "the topic states the implicit keep")
    ("cells/<your-name>/ under your home"
     "the topic names the folder, under whichever home this is")
    ("eval the defining form itself" "the topic teaches the constructed-redefinition blind spot")
    ("(define-hook name \"key\"" "the topic teaches advice over override, in its kept form")
    ("live-only, unkept" "and names the hook call as the unkept one")
    ("(rollback n)" "the topic names the undo: a generation")
    ("(cells)" "the topic names the report")
    ("definition-location" "the topic names the reads")
    ("HELD" "the topic teaches the hold")
    ("backslash-n" "the topic warns about the missing \\n escape")))

(deftest help-teaches-the-persist-flow ()
  ;; The persist topic carries the lifecycle whole: the
  ;; side-channel, the file verbs, the package caveat, the proof
  ;; discipline, the one-form retire.
  (is-carrying (text (nle:help :persist))
    ("($ \"name\")" "the topic teaches the strings side-channel")
    ("(edit path old new)" "the topic names the edit verb")
    ("(write-file path text)" "the topic names the whole-file writer")
    ("shipped source is home" "the topic states where a definition settles")
    ("unless the snippet said otherwise - while shipped files declare their own"
     "the topic carries the package caveat")
    ("the full suite only when" "the operator's proof discipline rides the recipe")
    ("one toplevel form" "the topic teaches the bulk retire")))

(deftest eval-tool-names-the-escape-the-reader-does-not-have ()
  ;; A "\n" inside a Lisp string reads as a plain n. Run as read, a file
  ;; would lose its line breaks and a grep its pattern, and answer as if it
  ;; worked. The snippet is refused with the escape named and nothing runs.
  (is-eval "(progn (setf cl-user::*escape-ran* t) (length \"a\\nb\"))"
    (is (eql 0 (search "ERROR: nothing ran. [reader: \"\\n\" at char 48 is inside a string literal" result))
        result)
    ("($ \"name\")" "the strings channel is the way out")
    (is (not (boundp 'cl-user::*escape-ran*)) "and nothing in the form ran"))
  (is (null (nle::string-escape-report "(sh \"printf 'a\\\\nb'\")")))
  (is (null (nle::string-escape-report "(list #\\n \"x\")")))
  (is (null (nle::string-escape-report "(list 1) ; \"\\n\"")))
  (is (search "\"\\t\" at char" (nle::string-escape-report "(list \"a\\tb\")"))))

;; Without this, escapes the reader drops would be lost, greps would come back
;; silently empty, and errors with a \n would not lead with the note: any
;; escape the reader does not keep is named, with the doubled repair, and an
;; error result leads with it.
(deftest the-escape-note-names-any-escape-and-leads-an-error ()
  (is-carrying (note (nle::string-escape-report "(sh \"grep 'a\\|b' f\")"))
    "\"\\|\" at char" "written twice")
  (is (null (nle::string-escape-report "(sh \"grep 'a\\\\|b' f\")")) "a doubled backslash is kept")
  (is-eval "(error \"x\\ny\")"
    (is (eql 0 (search "ERROR: nothing ran. [reader: \"\\n\"" result)) result)
    (is (not (search "xny" result)) "the form never ran to signal")))

;; 39 DeepSeek calls named sh, look, write-file, edit or plan as a tool of
;; their own (2026-09): the refusal names the call they meant.
(deftest a-snippet-verb-called-as-a-tool-is-told-the-call-it-meant ()
  (let ((nle::*tools* (list (list "eval") (list "websearch"))))
    (is-carrying (refusal (nth-value 1 (nle::tool-call-preflight "sh" "{}" nil)))
      "sh is a function inside eval, not a tool" "(sh ...)")
    (is-carrying (refusal (nth-value 1 (nle::tool-call-preflight "no_such_tool" "{}" nil)))
      "choose a registered tool; available tools: eval, websearch")))

(deftest openai-prompt-cache-routing-is-stable-and-prefix-scoped ()
  (flet ((context (config session system tools messages)
           (nle::make-compiled-turn-context
            :provider-config config :session-id session
            :system-prompt system :base-system-prompt system
            :tools tools :messages (coerce messages 'vector))))
    (let* ((config (test-provider-config))
           (other-model (test-provider-config :model "other-model"))
           (tool (vector (nlk:json-object "type" "function"
                                          "function"
                                          (nlk:json-object "name" "eval"))))
           (other-tool (vector (nlk:json-object "type" "function"
                                                "function"
                                                (nlk:json-object "name" "recall"))))
           (first (context config "one" "SYSTEM" tool
                           (list (nle::message "user" "first"))))
           (continued (context config "one" "SYSTEM" tool
                               (list (nle::message "user" "first")
                                     (nle::message "assistant" "second")
                                     (nle::message "user" "third"))))
           (other-session (context config "two" "SYSTEM" tool
                                   (list (nle::message "user" "first"))))
           (changed-system (context config "one" "OTHER SYSTEM" tool
                                    (list (nle::message "user" "first"))))
           (changed-model (context other-model "one" "SYSTEM" tool
                                   (list (nle::message "user" "first"))))
           (changed-tools (context config "one" "SYSTEM" other-tool
                                   (list (nle::message "user" "first"))))
           (key (nle::openai-prompt-cache-key first)))
      (is (= 64 (length key)) "routing key is a SHA-256 hex digest")
      (is (string= key (nle::openai-prompt-cache-key continued)) "a session's growing history keeps its key")
      (is (string= key (nle::openai-prompt-cache-key other-session)) "a fork or another session on the same prompt shares it")
      (is (not (string= key (nle::openai-prompt-cache-key changed-system))))
      (is (not (string= key (nle::openai-prompt-cache-key changed-model))))
      (is (not (string= key (nle::openai-prompt-cache-key changed-tools))))
      (is (string= key (gethash "prompt_cache_key"
                                (nle::request-body first))))
      (is (string= key (gethash "prompt_cache_key"
                                (nle::responses-request-body first)))))))

;;; --- the snippet package switch -------------------------------------------

(deftest eval-snippet-reads-the-rest-of-itself-in-the-package-it-names ()
  (is-eval "(in-package :cl-user) (package-name (symbol-package 'snippet-switch-probe))"
    ("=> \"COMMON-LISP-USER\"" "a symbol after the switch reads in the named package"))
  (is-eval "(package-name (symbol-package 'snippet-switch-probe))"
    (:= "\"NODECODE.EVOLVED\"" "and the next snippet reads in the evolved package again")))

(deftest eval-snippet-a-missing-package-runs-nothing ()
  (is-eval "(in-package :no-such-package-zz) (defparameter *never-defined-zz* 1)"
    (is (and (uiop:string-prefix-p "ERROR" result) (search "no package named" result)) result)
    ("NO-SUCH-PACKAGE-ZZ" "the verdict names the package")
    (is (null (find-symbol "*NEVER-DEFINED-ZZ*" :nodecode.evolved)))))

;;; --- the compiler's warnings lead the result ---------------------------------

(deftest eval-snippet-surfaces-the-compilers-warnings-before-the-value ()
  ;; Without this, a wrong argument count, an undefined name or a misused
  ;; literal that SBCL flags at compile time would be muffled by the
  ;; snippet.
  (is-eval "(defun snippet-warn-1 () (car 42))"
    ("[warning: Constant 42 conflicts with its asserted type LIST" result)
    (is (< (search "[warning" result) (search "SNIPPET-WARN-1" result)))
    (:absent "See also" "without the manual reference"))
  (is-eval "(defun snippet-warn-2 () (car 1 2))"
    ("[warning: The function CAR is called with two arguments, but wants exactly one.]"
     "a wrong argument count"))
  (is-eval "(defun snippet-warn-3 () *snippet-warn-novar*)"
    ("[warning: undefined variable: NODECODE.EVOLVED::*SNIPPET-WARN-NOVAR*]" "an undefined variable"))
  (is-eval "(defun snippet-warn-4 () (snippet-warn-no-such 1))"
    ("[warning: undefined function: NODECODE.EVOLVED::SNIPPET-WARN-NO-SUCH]" result)
    (:absent "caught STYLE-WARNING" "as one line, not the compiler's dump"))
  (nle:eval-string "(defun snippet-warn-5 (x) x)")
  (is-eval "(defun snippet-warn-5 (x) (list x))"
    (:absent "[warning" "a redefinition is not a warning the model needs"))
  (is-eval "(progn (warn \"by hand\") :done)"
    ("[warning: by hand]" "a warning the form itself raises")))

(deftest eval-snippet-a-failing-form-takes-its-hooks-with-it ()
  ;; A form that signals registers nothing: hooks registered during its
  ;; evaluation are removed with its error. Without this, a hook registered
  ;; before the error would outlive the form and fire on publishes after it.
  (nle:eval-string "(defun snippet-hook-probe (x) (* x 2))")
  (is-eval "(progn (hook 'snippet-hook-probe \"gone\" (lambda (next &rest args) (apply next args))) (error \"boom\"))"
    ("SIMPLE-ERROR: boom" result)
    ("[hooks this form registered were removed with its error: snippet-hook-probe \"gone\"]"
     "the result names what the error took")
    (is (null (assoc "gone" (cdr (assoc (nlk::evolved-symbol "SNIPPET-HOOK-PROBE") nlk::*hooks* :test #'equal))
                     :test #'string=))))
  (is-eval "(snippet-hook-probe 3)" (:= "6" "the function is plain again"))
  (nle:eval-string "(hook 'snippet-hook-probe \"kept\" (lambda (next &rest args) (1+ (apply next args))))")
  (let ((kept (nle:eval-string "(snippet-hook-probe 3)")))
    (is (equal "7" (subseq kept (1- (length kept)))) "a hook whose form finished stays"))
  (nle:eval-string "(unhook 'snippet-hook-probe \"kept\")"))

(deftest eval-snippet-an-arity-error-names-the-callee ()
  ;; SBCL says "invalid number of arguments: 4" and nothing else; the
  ;; frame that refused the call is read before the stack unwinds.
  (is-eval "(funcall (lambda (next op) (list next op)) 1 2 3 4)"
    ("invalid number of arguments: 4" result)
    (", calling (LAMBDA (NEXT OP)" "the lambda that refused four arguments is named"))
  (nle:eval-string "(defun snippet-arity-probe (a) a)")
  (is-eval "(snippet-arity-probe 1 2)"
    (", calling SNIPPET-ARITY-PROBE" result))
  (is-eval "(error \"plain\")"
    (:absent "calling" "an error that is not an arity error carries no callee")))

(deftest eval-value-a-multi-line-string-prints-as-text ()
  ;; A file, a listing, a command's output: read as text, not as a literal
  ;; with every quote escaped. One line keeps the REPL's quotes.
  (is-eval "(format nil \"(defun a () \\\"x\\\")~%  ; c:\\\\d\")"
    ("[text: 2 lines, " result)
    ("(defun a () \"x\")" "quotes arrive as quotes")
    ("; c:\\d" "backslashes single")
    (:absent "\\\"" "no escapes anywhere"))
  (is-eval "\"one line\"" (:= "\"one line\"" "a one-line string keeps its quotes"))
  (is-eval "(with-output-to-string (s) (dotimes (i 4000) (format s \"line ~d of the text~%\" i)))"
    ("[value elided: a text of " result)
    ("lines, the first " "the cut names the line count")
    ("(subseq * " "and the form that reads on")
    (is (< (length result) nle::*eval-result-limit*) "within the limit")))

(deftest streaming-routes-an-inline-thinking-tag-to-reasoning ()
  ;; An upstream that renders its thinking as <thinking>...</thinking> in
  ;; content, with reasoning_content empty: the tag and its inside are
  ;; reasoning, what follows the close is the content, and an unclosed tag is
  ;; reasoning to the end.
  (let ((message (streamed-message (chunk-json "<thin")
                                   (chunk-json "king>**Plan")
                                   (chunk-json "ning**</think")
                                   (chunk-json (format nil "ing>~%The answer."))
                                   (chunk-json "" :finish-reason "stop"))))
    (is (equal "The answer." (gethash "content" message)) (gethash "content" message))
    (is (equal "**Planning**" (gethash "reasoning_content" message))))
  (let ((message (streamed-message (chunk-json "<thinking>**Verifying live")
                                   (chunk-json " image**")
                                   (chunk-json "" :finish-reason "tool_calls"))))
    (is (eq :null (gethash "content" message)) "an unclosed tag leaves no content")
    (is (equal "**Verifying live image**" (gethash "reasoning_content" message))))
  (let ((message (streamed-message (chunk-json "plain text, then <thinking>")
                                   (chunk-json "" :finish-reason "stop"))))
    (is (equal "plain text, then <thinking>" (gethash "content" message))))
  (let ((message (streamed-message (chunk-json "<th")
                                   (chunk-json "e end")
                                   (chunk-json "" :finish-reason "stop"))))
    (is (equal "<the end" (gethash "content" message)))))


(deftest responses-request-omits-the-reasoning-summary-when-the-provider-says-off ()
  ;; A summary is output the model is billed for and a headless run never
  ;; reads. providers.<id>.reasoning_summary "off" drops the key; an entry
  ;; that says nothing keeps the auto summary the TUI shows.
  (with-temp-shared-config ("{\"providers\": {\"quiet\": {
    \"sdk\": \"openai-responses\", \"base_url\": \"https://quiet.example\",
    \"api_key\": \"k\", \"reasoning_summary\": \"off\"},
    \"loud\": {\"sdk\": \"openai-responses\", \"base_url\": \"https://loud.example\",
    \"api_key\": \"k\"}}}")
    (let ((nle::*reasoning-effort* "medium")
          (nle::*model* "gpt-5-mini")
          (nle::*endpoint* nil)
          (nle::*api-key* nil))
      (nlk:bind ((nle::*provider* "quiet") (context (user-context "hi"))
                 (config context.provider-config)
                 (reasoning (gethash "reasoning" (nle::responses-request-body context))))
        (is (null config.reasoning-summary))
        (is-present reasoning "the reasoning object still rides"
          (is (equal "medium" (gethash "effort" reasoning)) "with its effort")
          (is (null (nth-value 1 (gethash "summary" reasoning))))))
      (let ((nle::*provider* "loud"))
        (let ((reasoning (gethash "reasoning"
                                  (nle::responses-request-body (user-context "hi")))))
          (is-present reasoning "the loud entry's reasoning object"
            (is (equal "auto" (gethash "summary" reasoning)))))))))

;;; --- an open tool call never reaches the wire open ------------------------
;;; A call fact with no result fact — an unhandled Esc, or a process death —
;;; would fold as an assistant tool_calls message with no tool reply, which
;;; every provider rejects on every later round. Three seams each hold the
;;; line: the fold pairs every call it emits, the dispatch closes the call
;;; on any unwind, and the terminal failure fact keeps what the provider
;;; answered.

(deftest orphan-tool-call-always-pairs-in-provider-context (with-session-store ("wire-open-call"))
  (let* ((turn (nlk:admit-turn "wire-open-call" "open-command" "go"))
         (call (wire-tool-call "call-cut" "eval" "{\"form\":\"(sleep 30)\"}"))
         (assistant (wire-message "assistant" :content ""
                                  :tool-calls (vector call))))
    (nlk:record-assistant-message turn assistant)
    (nlk:record-tool-call turn "call-cut" "eval" "{\"form\":\"(sleep 30)\"}")
    ;; No result fact follows: the process died, or Esc landed in a
    ;; window no handler covered.
    (multiple-value-bind (messages clean-p)
        (nle::wire-messages-from-events
         (session-events "wire-open-call" :created nil))
      (is (equal '("user" "assistant" "tool")
                 (mapcar (lambda (message) (gethash "role" message)) messages)))
      (is (null clean-p)))
    (nlk:cancel-turn turn "esc")
    (let ((next (nlk:admit-turn "wire-open-call" "next-command" "again")))
      (nlk:complete-turn next nil))
    (let* ((messages (wire-fold "wire-open-call"))
           (answer (third messages)))
      (is (equal '("user" "assistant" "tool" "user")
                 (mapcar (lambda (message) (gethash "role" message)) messages)))
      (is (equal "call-cut" (gethash "tool_call_id" answer)))
      (is (equal "eval" (gethash "name" answer)) "and its tool")
      (is (equal nle::+orphan-tool-result+ (gethash "content" answer))))))

(deftest synthesized-open-tool-call-pairs-too (with-session-store ("wire-open-synth"))
  ;; No stored assistant message: the fold synthesizes the tool_calls
  ;; message from the call fact, and the same pairing rule answers it.
  (let ((turn (nlk:admit-turn "wire-open-synth" "synth-open" "go")))
    (nlk:record-tool-call turn "call-open" "eval" "{\"form\":\"1\"}")
    (nlk:cancel-turn turn "esc")
    (let ((next (nlk:admit-turn "wire-open-synth" "synth-next" "again")))
      (nlk:complete-turn next nil))
    (let ((messages (wire-fold "wire-open-synth")))
      (is (equal '("user" "assistant" "tool" "user")
                 (mapcar (lambda (message) (gethash "role" message)) messages)))
      (is (equal "call-open" (gethash "tool_call_id" (third messages)))))))

(deftest cancel-inside-the-baseline-walk-still-closes-the-call ()
  ;; Esc lands between the call fact and the tool body — inside the
  ;; workspace baseline capture, which is not a cancellable-wait region and
  ;; ran 10 s over $HOME on one box. The call fact is durable; without the
  ;; closure the next round's wire carries a call with no result.
  (with-session-store ("cancel-baseline")
    (let* ((turn (nlk:admit-turn "cancel-baseline" "cb-cmd" "go"))
           (context (compiled-context '()))
           (outcome nil))
      (with-stubbed-fdefinition (nlk:open-workspace-bracket (turn &key label)
                                 (nlk:request-cancel-turn
                                  "cancel-baseline"
                                  (nlk:durable-turn-turn-id turn) "esc")
                                 (values nil nil))
        (setf outcome
              (nlk:with-handlers ((nlk:turn-cancelled-condition () :cancelled))
                (let ((nle::*current-durable-turn* turn))
                  (execute-wire-call "call-walk" "eval" "{\"form\": \"1\"}" context)))))
      (is (eq :cancelled outcome))
      (let ((results (session-events "cancel-baseline"
                                     :type 'nlk:tool-result-event)))
        (is (= 1 (length results)) "the call closed durably")
        (when results
          (is (equal "call-walk" (nlk:tool-result-event-call-id (first results))))
          (is (equal "interrupted by user"
                     (nlk:tool-result-event-result (first results)))))))))

(deftest cancel-flagged-before-dispatch-skips-the-baseline-walk ()
  ;; A cancel already flagged when the call fact records is not worth a
  ;; walk: the call closes at once, before any capture spends on it.
  (with-session-store ("cancel-flagged")
    (let* ((turn (nlk:admit-turn "cancel-flagged" "cf-cmd" "go"))
           (context (compiled-context '()))
           (walks 0)
           (outcome nil))
      (with-stubbed-fdefinition (nlk:record-tool-call (turn &rest args)
                                 (prog1 (apply original turn args)
                                   (nlk:request-cancel-turn
                                    "cancel-flagged"
                                    (nlk:durable-turn-turn-id turn) "esc")))
        (with-stubbed-fdefinition (nlk:open-workspace-bracket (&rest args)
                                   (incf walks)
                                   (values nil nil))
          (setf outcome
                (nlk:with-handlers ((nlk:turn-cancelled-condition () :cancelled))
                  (let ((nle::*current-durable-turn* turn))
                    (execute-wire-call "call-flagged" "eval" "{\"form\": \"1\"}" context))))))
      (is (eq :cancelled outcome) "the dispatch unwound as the cancel")
      (is (zerop walks) "and never walked the workspace")
      (is (= 1 (length (session-events "cancel-flagged"
                                       :type 'nlk:tool-result-event)))))))

(deftest failed-turn-keeps-the-provider-failure-evidence (with-session-store ("fail-evidence"))
  ;; Twelve failed turns on one box kept 400 characters of `<response body
  ;; unreadable>' between them and no request identity: the terminal fact
  ;; now carries the status, the content type, the request digest and the
  ;; head of the body — what a diagnosis reads when the process is gone.
  (let ((turn (nlk:admit-turn "fail-evidence" "fe-cmd" "hello"))
        (request "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}")
        (body "{\"error\":{\"message\":\"An assistant message with 'tool_calls' must be followed by tool messages responding to each 'tool_call_id'.\",\"type\":\"invalid_request_error\"}}"))
    (with-provider-stub (nil
                         (error 'nle::provider-error
                                :status 400 :scope :request
                                :detail (subseq body 0 40)
                                :evidence-body body
                                :evidence-content-type "application/octet-stream"
                                :evidence-request-body request))
      (nlk:with-handlers ((error () nil))
        (nle:run "hello" :session (nle:intern-session "fail-evidence")
                 :command-id "fe-cmd" :admitted-turn turn)))
    (let ((failed (session-event "fail-evidence" :type 'nlk:turn-failed)))
      (is-present failed "the turn failed durably"
        (is (eql 400 (nlk:turn-failed-status failed)) "with the HTTP status")
        (is (stringp (nlk:turn-failed-model failed)) "and the model it targeted")
        (is (equal "application/octet-stream"
                   (nlk:turn-failed-content-type failed)))
        (is (equal (nlk::sha256-text request)
                   (nlk:turn-failed-request-sha256 failed)))
        (is (equal body (nlk:turn-failed-response-body failed)))))))

(deftest a-home-with-no-model-chosen-fails-its-turn-saying-so (with-session-store ("no-model"))
  ;; The target an image is built with answers nobody now -- OpenCode's free
  ;; tier serves only OpenCode -- so a home that chose no model
  ;; is told what to do, and nothing is dialed.
  (let ((turn (nlk:admit-turn "no-model" "nm-cmd" "hello"))
        (dialed nil))
    (with-temp-shared-config ("{}")
      (with-provider-stub (nil (setf dialed t) (wire-message "assistant" :content "no"))
        (nlk:with-handlers ((error () nil))
          (nle:run "hello" :session (nle:intern-session "no-model")
                   :command-id "nm-cmd" :admitted-turn turn))))
    (is (not dialed) "no provider was dialed")
    (is-present (failed (session-event "no-model" :type 'nlk:turn-failed)) "the turn failed durably"
      (is (search "no model is chosen yet: /setup" (nlk:turn-failed-detail failed))))))

(deftest failure-body-drains-a-binary-keep-alive-stream ()
  ;; Every DeepSeek 400 answers application/octet-stream through CloudFront;
  ;; dexador leaves that body a binary KEEP-ALIVE-STREAM with no line
  ;; reader, and READ-LINE on it rendered `<response body unreadable>' for
  ;; twelve turns while the provider's one-line explanation sat in the
  ;; socket. Both readers now drain octets and decode them.
  (let* ((json "{\"error\":{\"message\":\"An assistant message with 'tool_calls' must be followed by tool messages responding to each 'tool_call_id'.\",\"type\":\"invalid_request_error\"}}")
         (octets (sb-ext:string-to-octets json :external-format :utf-8)))
    (with-temp-directory (dir "octet-body")
      (let ((path (merge-pathnames "body.bin" dir)))
        (with-open-file (out path :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
          (write-sequence octets out))
        (with-open-file (raw path :element-type '(unsigned-byte 8))
          (let ((body (dexador.keep-alive-stream:make-keep-alive-stream
                       raw :end (length octets))))
            (is (equal json (nlk:body-text body)))))
        (with-open-file (raw path :element-type '(unsigned-byte 8))
          (let ((body (dexador.keep-alive-stream:make-keep-alive-stream
                       raw :end (length octets))))
            (is (equal (subseq json 0 32) (nlk:body-text body 32)))))))
    (is (equal "plain" (nlk:body-text (make-string-input-stream "plain"))))))

(deftest a-tool-round-walks-its-bracket-and-its-checkpoint-anchors-that-tree (with-temp-store ())
  ;; Three walks per tool call: a baseline, the reply's recapture, and a
  ;; deferred checkpoint reading the same unchanged tree a third time. The
  ;; checkpoint anchors the reply's tree: its fact carries that sha and no
  ;; walk of its own. The baseline stays a walk at dispatch; otherwise a
  ;; bracket opened on an earlier tree would charge a peer's writes during
  ;; the provider round to the call. The turn-start capture stays deferred,
  ;; overlapping the first request.
  (let ((workspace (temp-directory "onewalk")))
    (if (not (nlk:git-available-p))
        (is t "git unavailable: no walk to count")
        (with-workspace-captures (t)
          (nlk:create-session :id "onewalk" :cwd (namestring workspace))
          (let ((worker (bt2:current-thread))
                (walks '())
                (rounds 0))
            (with-stubbed-fdefinition (nlk:capture-workspace-tree (store)
                                       (prog1 (funcall original store)
                                         (push (bt2:current-thread) walks)))
              (with-provider-stub
                  (context
                   (incf rounds)
                   (case rounds
                     (1 (wire-message
                         "assistant"
                         :tool-calls (vector (wire-tool-call "c1" "eval" "{\"form\": \"1\"}"))))
                     (t (wire-message "assistant" :content "done"))))
                (nle:run "go" :session (nle:intern-session "onewalk")
                              :command-id "onewalk-command")))
            (is (= 2 rounds) "two rounds ran")
            (is (= 1 (count-if-not (lambda (thread) (eq thread worker)) walks)))
            (is (= 3 (count worker walks)))
            (let* ((captures (nlk:session-workspace-captures "onewalk"))
                   (start (find "user_message" captures
                                :key (lambda (capture) (getf capture :boundary-kind))
                                :test #'equal))
                   (result (find "c1" captures
                                 :key (lambda (capture) (getf capture :tool-call-id))
                                 :test #'equal)))
              (is (= 3 (length captures)) "start, tool round and end captures recorded")
              (is (and start result))
              (when (and start result)
                (is (equal (getf start :tree-sha) (getf result :pre-tree-sha)))
                (is (equal (getf result :pre-tree-sha) (getf result :tree-sha)))
                (is (null (gethash (getf start :turn-id) nlk::*deferred-turn-facts*)))))
            (let* ((kinds (mapcar #'nlk:event-kind (session-events "onewalk" :created nil)))
                   (captures (loop for kind in kinds for i from 0
                                   when (equal kind "session.workspace_captured") collect i))
                   (assistants (loop for kind in kinds for i from 0
                                     when (equal kind "turn.assistant_message_completed") collect i))
                   (result (position "turn.tool_result" kinds :test #'equal)))
              (when (and (= 3 (length captures)) (= 2 (length assistants)) result)
                (is (< (first captures) (first assistants)))
                (is (< result (second captures) (second assistants))))))))))

;;; --- the request fact's prefix diagnosis -----------------------------------
;;; A round whose cached input fell short of the previous prompt is either a
;;; harness leak (the body's prefix moved) or the provider's own miss (it did
;;; not); the fact carries the bytes shared with the turn's previous request
;;; and where the first differing byte fell, so history.db can tell the two
;;; apart without the bodies.

(deftest json-path-at-names-the-item-under-an-offset ()
  ;; ASCII text, so a character offset in TEXT is the byte offset in BODY;
  ;; the wide key is what proves the bytes of one never read as structure.
  (let* ((text "{\"model\":\"m\",\"tools\":[{\"n\":\"a, b\"}],\"input\":[{\"role\":\"sys\"},{\"role\":\"user\",\"x\":\"[,]\"},{\"t\":1}]}")
         (body (sb-ext:string-to-octets text)))
    (is (equal "" (nle::json-path-at body 0)) "before the first key")
    (is (equal "model" (nle::json-path-at body (search "\"m\"" text))) "a scalar value")
    (is (equal "tools[0]" (nle::json-path-at body (search "a, b" text))))
    (is (equal "input[0]" (nle::json-path-at body (search "sys" text))))
    (is (equal "input[1]" (nle::json-path-at body (search "[,]" text))))
    (is (equal "input[2]" (nle::json-path-at body (search "{\"t\"" text))))
    (is (equal "input" (nle::json-path-at body (1- (length body)))))
    (let ((wide (sb-ext:string-to-octets "{\"clé\":\"[\",\"x\":[1]}" :external-format :utf-8)))
      (is (equal "clé" (nle::json-path-at wide 8)) "a wide key reads whole")
      (is (equal "x[0]" (nle::json-path-at wide (- (length wide) 3)))))))

(deftest request-fact-carries-the-prefix-shared-with-the-previous-round ()
  (with-session-store ("prefix-fact")
    (let* ((body-0 "{\"model\":\"m\",\"input\":[{\"role\":\"system\",\"content\":\"S\"},{\"role\":\"user\",\"content\":\"go\"}]}")
           (body-1 "{\"model\":\"m\",\"input\":[{\"role\":\"system\",\"content\":\"S\"},{\"role\":\"user\",\"content\":\"go\"},{\"type\":\"function_call\"}]}")
           (rounds 0))
      (with-streaming-provider-stub
          (context on-part
           (declare (ignorable context on-part))
           (incf rounds)
           (if (= rounds 1)
               (values (wire-message "assistant" :content ""
                                     :tool-calls (vector (wire-tool-call "c1" "eval" "{\"form\": \"1\"}")))
                       nil "tool_calls" (sb-ext:string-to-octets body-0))
               (values (wire-message "assistant" :content "done") nil "stop"
                       (sb-ext:string-to-octets body-1))))
        (nle:run "go" :session (nle:intern-session "prefix-fact")
                 :command-id "prefix-fact-cmd"))
      (let ((facts (remove-if-not (lambda (event)
                                    (typep event 'nlk:provider-request-event))
                                  (nle::collect-session-events "prefix-fact"))))
        (is (= 2 (length facts)) "two rounds left two request facts")
        (is (null (nlk:provider-request-event-prefix-shared-bytes (first facts))))
        (is (null (nlk:provider-request-event-prefix-diverged-in (first facts))))
        (is (= (mismatch body-0 body-1)
               (nlk:provider-request-event-prefix-shared-bytes (second facts))))
        (is (equal "input[2]" (nlk:provider-request-event-prefix-diverged-in (second facts))))))))

(deftest a-turns-first-request-fact-carries-the-prefix-shared-with-the-turn-before ()
  ;; Round zero read nothing, so a prefix lost at a turn boundary had
  ;; no reading in the log. It reads the session's last kept body.
  (with-session-store ("prefix-turns")
    (let* ((bodies (list "{\"model\":\"m\",\"input\":[{\"role\":\"user\",\"content\":\"one\"}]}"
                         "{\"model\":\"m\",\"input\":[{\"role\":\"user\",\"content\":\"one\"},{\"role\":\"assistant\",\"content\":\"ok\"},{\"role\":\"user\",\"content\":\"two\"}]}"))
           (sent bodies))
      (with-streaming-provider-stub
          (context on-part
           (declare (ignorable context on-part))
           (values (wire-message "assistant" :content "ok") nil "stop"
                   (sb-ext:string-to-octets (pop sent))))
        (nle:run "one" :session (nle:intern-session "prefix-turns") :command-id "prefix-turns-1")
        (nle:run "two" :session (nle:intern-session "prefix-turns") :command-id "prefix-turns-2"))
      (let ((facts (remove-if-not (lambda (event)
                                    (typep event 'nlk:provider-request-event))
                                  (nle::collect-session-events "prefix-turns"))))
        (is (= 2 (length facts)) "one request a turn")
        ;; a session's first request has nothing before it
        (is (null (nlk:provider-request-event-prefix-shared-bytes (first facts))))
        (is (= (mismatch (first bodies) (second bodies))
               (nlk:provider-request-event-prefix-shared-bytes (second facts))))
        (is (equal "input[1]" (nlk:provider-request-event-prefix-diverged-in (second facts))))))))

;;; --- the turn's memo through the chat lane ---------------------------------
;;; Two real chat-lane rounds over canned SSE streams, the loop in between:
;;; on round one every message the provider has already been sent comes back
;;; from the memo as a verbatim — the closed turn's, and the open turn's own,
;;; which the hydration cache now extends instead of refolding — while the
;;; round's new items do not, and the two request facts agree the bodies
;;; share every byte up to the round's first new item.

(deftest later-rounds-carry-the-closed-history-as-verbatims (with-session-store ("memo-rounds"))
  (seed-completed-turn "memo-rounds" "earlier ask" "earlier answer")
  (let ((nle::*api-key* "k")
        (streams (list (make-sse-stream (tool-call-chunk-json 0 "c1" "eval" "{\"form\": \"1\"}"))
                       (make-sse-stream (chunk-json "done" :finish-reason "stop"))))
        (shapes '()))
    (with-stubbed-fdefinitions ((dex:post (&rest args) (values (pop streams) 200))
                                (nlk:encode-json-body (body key memo)
                                 (push (map 'list #'nlk:json-verbatim-p (gethash key body))
                                       shapes)
                                 (funcall original body key memo)))
      (nle:run "go" :session (nle:intern-session "memo-rounds")
               :command-id "memo-rounds-cmd"))
    (is (= 2 (length shapes)) "two rounds crossed the serialize seam")
    (is (notany #'identity (second shapes)) "round zero encodes every message fresh")
    (is (equal '(nil t t t nil nil) (first shapes)))
    (let ((facts (remove-if-not (lambda (event)
                                  (typep event 'nlk:provider-request-event))
                                (nle::collect-session-events "memo-rounds"))))
      (is (= 2 (length facts)))
      (is (equal "messages[4]"
                 (nlk:provider-request-event-prefix-diverged-in (second facts)))))))

;;; --- a failed call is a recorded fact ---------------------------------------

(deftest a-failed-call-lands-failed-on-its-fact (with-session-store ("call-failed"))
  ;; Without the dispatch seam, a failed call would draw as a success and
  ;; the fact would carry no failure. The dispatch seam records it. A body
  ;; that signalled, a body's own verdict (NLE:FAILURE) and a hook's veto
  ;; that says so are failed; a call refused before it ran is failed; a body
  ;; that answered is not.
  (with-saved-globals (nle::*tools* nle:*hooks*)
    (nle:register-tool "probe" (probe-schema)
                       (lambda (args &aux (how (gethash "how" args)))
                         (cond ((equal how "signal") (error "the probe broke"))
                               ((equal how "verdict") (nle:failure "ERROR: the probe says no"))
                               (t "fine"))))
    (nle:hook :tool "veto"
              (lambda (op next)
                (if (equal "veto" (gethash "how" (getf op :arguments)))
                    (nle:failure "ERROR: refused: probe")
                    (funcall next op))))
    (with-durable-turn (turn "call-failed" :command-id "call-failed-cmd" :input "go" :current t)
      (nle::execute-tool-calls
       (map 'vector (lambda (id how) (wire-tool-call id "probe" (format nil "{\"how\": ~s}" how)))
            '("c1" "c2" "c3" "c4") '("signal" "verdict" "veto" "fine"))
       (compiled-context '()))
      (execute-wire-call "c5" "probe" "{\"how\": \"signal\"}")
      (execute-wire-call "c6" "ghost" "{}")
      (is (equal '(("c1" . t) ("c2" . t) ("c3" . t) ("c4" . nil) ("c5" . t) ("c6" . t))
                 (mapcar (lambda (fact)
                           (cons (nlk:tool-result-event-call-id fact)
                                 (nlk:tool-result-event-failed fact)))
                         (session-events "call-failed" :type 'nlk:tool-result-event)))))))


;;; --- a cut-off read runs again ----------------------------------------------

(defun replay-args (form)
  "An eval call's arguments JSON carrying FORM."
  (nlk:encode-json-object (nlk:make-json-object "form" form)))

(deftest a-snippet-replays-only-when-every-form-only-reads ()
  ;; Pi Durable reruns a tool a crash cut off when the tool says a rerun is
  ;; safe; here the snippet's forms say it. Reads of the image's own and
  ;; COMMON-LISP's pure operators replay; a verb that acts, an assignment, a
  ;; definition, an indirect call, a file opened, or a reader macro never does.
  (dolist (form '("(+ 40 2)" "(recall-log :match \"x\")" "(help :eval) (index \"x\")"
                  "(let ((a 1)) (list a '(sh)))" "(mapcar (lambda (p) (getf p :n)) '((:n 1)))"
                  "(loop for x in '(1 2) collect (* x x))" "(cond ((= 1 1) :a) (t :b))"
                  "(handler-case (car 1) (error (e) e))"))
    (is (eq t (nle::replay-safe-snippet-p (replay-args form))) (format nil "~a only reads" form)))
  (dolist (form '("(sh \"true\")" "(setf x 1)" "(defun f () 1)" "(funcall #'car '(1))"
                  "(with-open-file (s \"/tmp/x\"))" "#.(+ 1 2)" "(edit \"a\" \"b\" \"c\")"
                  "(case 1 (1 :a) (t (sh \"x\")))" "(list (unknown-reader))"))
    (is (null (nle::replay-safe-snippet-p (replay-args form))) (format nil "~a may act" form)))
  (is (null (nle::replay-safe-snippet-p "not json")))
  (is (null (nle::replay-safe-snippet-p (nlk:encode-json-object (nlk:make-json-object "cmd" "(+ 1 2)"))))))

(deftest a-read-the-restart-cut-off-runs-again-before-the-provider-is-asked
    (with-restartable-store (path "replay"))
  ;; The dead process had a round of two calls in flight: a read and a verb
  ;; that acts. Recovery closes both as interrupted; the resumed turn runs the
  ;; read again in a round of the harness's own, before its provider is asked,
  ;; and leaves the verb to the model, which may know it half-acted.
  (with-saved-globals (nlk::*recovery-boot-id*
                       (nle::*resumed-turns* (make-hash-table :test #'equal :synchronized t))
                       (nle::*replays-claimed* (make-hash-table :test #'equal :synchronized t)))
    (let ((reading (replay-args "(+ 40 2)"))
          (acting (replay-args "(sh \"true\")"))
          (asked '()))
      (let ((turn (nlk:admit-turn "replay" "replay-cmd" "hello")))
        (nlk:record-assistant-message
         turn (wire-message "assistant" :tool-calls (vector (wire-tool-call "c-read" "eval" reading)
                                                            (wire-tool-call "c-act" "eval" acting))))
        (nlk:record-tool-call turn "c-read" "eval" reading)
        (nlk:record-tool-call turn "c-act" "eval" acting))
      (reopen-store path)
      (destructuring-bind ((turn input)) (nlk::recoverable-interrupted-turns)
        (setf (gethash (nlk:durable-turn-turn-id turn) nle::*resumed-turns*) "back")
        (is (nlk::claim-turn-resumption turn))
        (with-provider-stub (context (push (nle::compiled-turn-context-messages context) asked)
                                     (wire-message "assistant" :content "done"))
          (nle:run input :session (nle:intern-session "replay") :admitted-turn turn)))
      (is (= 1 (length asked)) "the replay round asked no provider")
      (let* ((messages (coerce (first asked) 'list))
             (tools (remove "tool" messages :key (lambda (m) (gethash "role" m)) :test-not #'equal)))
        (is (= 3 (length tools)) "two interrupted answers and the replayed read")
        (is (= 2 (count-if (lambda (m) (search nlk::+restart-tool-result+ (nle::content-text (gethash "content" m))))
                           tools)))
        ;; the provider is asked with the read's value in hand
        (is (search "42" (nle::content-text (gethash "content" (car (last tools)))))))
      (let ((calls (session-events "replay" :type 'nlk:tool-call-event)))
        ;; the read runs again, the verb does not
        (is (equal (list reading acting reading) (mapcar #'nlk:tool-call-event-arguments calls)))
        ;; the replay is a call of its own
        (is (not (member (nlk:tool-call-event-call-id (third calls)) '("c-read" "c-act") :test #'equal))))
      (is (= 3 (length (session-events "replay" :type 'nlk:tool-result-event))))
      (is (null (nle::claim-turn-replays)) "claimed once"))))
