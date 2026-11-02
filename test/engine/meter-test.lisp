;;;; meter-test.lisp --- the running turn as a value, its spend line, its budget.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the meter's contract: TURN answers NIL outside a
;;;; turn and the running turn's identity and spend inside one; a budgeted
;;;; turn's spend line reads the clock, the rounds, the calls, the context the
;;;; newest round filled and the wall clock, rides a result's metadata and is
;;;; rendered into the model's copy alone, the history fold rendering the same
;;;; bytes, and an unbudgeted turn carries none; TURN-BUDGET is advice read once as the turn opens, a failing
;;;; advice leaving the turn unbudgeted; a call past the budget is refused
;;;; and the turn still completes on the model's answer, while a round that
;;;; calls tools after the refusal ends the turn; and no park outlasts what
;;;; the budget has left.

(in-package #:nodecode.test)

(defmacro with-turn-budget ((budget &optional turn-id) &body body)
  "Run BODY with every opening turn given BUDGET through advice on
NLE:TURN-BUDGET, unhooked however BODY leaves."
  ;; TURN-ID, when given, opens a live turn of that id for BODY.
  `(nlk:with-cleanup ((nle:unhook 'nle:turn-budget "meter-test"))
     (nle:hook 'nle:turn-budget "meter-test"
               (nlk:lambda-ignoring (next turn)
                 ,budget))
     (let (,@(when turn-id `((nle::*live-turn* (nle::open-live-turn "s-meter" ,turn-id)))))
       ,@body)))

(deftest the-turn-is-nil-outside-a-turn-and-the-running-turn-inside-one ()
  (is (null (nle:turn)) "no turn, no value")
  (let ((nle::*live-turn* (nle::open-live-turn "s-meter" "t1")))
    (is-present (turn (nle:turn)) "the running turn"
      (is-shape turn (:session-id "s-meter") (:turn-id "t1")
        (:parent null "no store, no parent read") (:rounds eql 0) (:calls eql 0)
        (:budget null "no advice: no cap") (:context-tokens null "no round yet, no context")))
    (nle::note-round nil (nle::make-provider-usage :input-tokens 100 :cached-input-tokens 900
                                                   :cache-write-tokens 500
                                                   :output-tokens 40 :reasoning-tokens 30)
                     1000000)
    (nle::note-round t nil)
    (is-present (turn (nle:turn)) "the running turn, two rounds on"
      (is-shape turn (:rounds eql 2 "two rounds folded") (:input-tokens eql 100)
        (:cached-input-tokens eql 900) (:output-tokens eql 40)
        (:reasoning-tokens eql 30 "a round without usage adds nothing") (:context-tokens eql 1500)
        (:context-window eql 1000000 "and a round without a window leaves that")
        (:quiet-seconds eql 0 "the second round said something")))
    (is (not (eq (nle:turn) (nle:turn))) "a fresh plist on every call")))

(deftest only-a-budgeted-turn-carries-the-spend-line ()
  (let ((nle::*live-turn* (nle::open-live-turn "s-meter" "t1")))
    (is (null (nle::spend-metadata nil)) "a turn without a budget adds nothing")
    (let ((receipts (nlk:json-object "receipts" #())))
      (is (eq receipts (nle::spend-metadata receipts)) "and keeps the result's own metadata")))
  (let* ((nle::*turn-budget* '(:seconds 1800))
         (nle::*live-turn* (nle::open-live-turn "s-meter" "t2")))
    (let* ((metadata (nle::spend-metadata nil))
           (spend (gethash "spend" metadata)))
      (is (eql 0 (search "[turn 0s of 30m · 0 rounds · 0 calls · " spend)) spend)
      (is (ppcre:scan "· (Mon|Tue|Wed|Thu|Fri|Sat|Sun) \\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2} [+-]\\d{2}:\\d{2}\\]$"
                      spend))
      (is (equal (format nil "ok~%~a" spend) (nle::tool-result-text "ok" metadata))))
    (nle::note-round nil (nle::make-provider-usage :input-tokens 2000 :cached-input-tokens 85000)
                     1000000)
    (is (search "· 1 round · 0 calls · context 87k of 1M · " (gethash "spend" (nle::spend-metadata nil)))))
  (is (null (nle::spend-metadata nil)) "outside a turn nothing is added")
  (is (equal "ok" (nle::tool-result-text "ok" nil)) "and nothing is rendered"))

(deftest a-tool-result-ends-with-its-seq ()
  (is (equal (format nil "ok~%[seq 57]") (nle::tool-result-text "ok" nil :seq 57)))
  (is (equal "ok" (nle::without-seq-line (nle::tool-result-text "ok" nil :seq 57))) "the file reads it without the line")
  (is (equal "" (nle::without-seq-line (nle::tool-result-text "" nil :seq 57))) "an empty result too")
  (is (equal "ok" (nle::without-seq-line "ok")) "and a result without one is itself")
  (let* ((nle::*turn-budget* '(:seconds 1800))
         (nle::*live-turn* (nle::open-live-turn "s-meter" "t3"))
         (text (nle::tool-result-text "ok" (nle::spend-metadata nil) :seq 57)))
    (is (eql 0 (search (format nil "ok~%[seq 57 · turn 0s of 30m · 0 rounds · ") text)) text)
    (is (equal "ok" (nle::without-seq-line text)) "one bracket, one line to take off")))

(deftest past-the-fill-threshold-every-result-says-how-full ()
  (let ((nle::*live-turn* (nle::open-live-turn "s-meter" "t4")))
    (nle::note-round nil (nle::make-provider-usage :input-tokens 1000 :cached-input-tokens 49000) 180000)
    (is (null (nle::spend-metadata nil)) "under the floor a turn without a budget adds nothing")
    (nle::note-round nil (nle::make-provider-usage :input-tokens 1000 :cached-input-tokens 94000) 180000)
    (is (equal "[context 95k of 180k]" (gethash "spend" (nle::spend-metadata nil))))
    (is (equal (format nil "ok~%[seq 57 · context 95k of 180k]")
               (nle::tool-result-text "ok" (nle::spend-metadata nil) :seq 57)) "one bracket with the seq")))

(deftest the-fill-line-is-one-rule-for-every-session ()
  ;; The smaller of the floor and the window's share, for every session: the
  ;; coin that drew an arm per session was A/B'd and came back flat (T-294).
  (flet ((fill-line (id window tokens)
           (let ((nle::*live-turn* (nle::open-live-turn id "t5")))
             (nle::note-round nil (nle::make-provider-usage :input-tokens 1000
                                                            :cached-input-tokens (- tokens 1000))
                              window)
             (let ((metadata (nle::spend-metadata nil)))
               (and metadata (gethash "spend" metadata))))))
    (is (null (fill-line "s-meterA" 1000000 40000)) "under the floor a 1M window says nothing")
    (is (equal "[context 65k of 1M]" (fill-line "s-meterA" 1000000 65000)) "a 1M window's session reads its fill at the floor")
    (is (equal "[context 65k of 1M]" (fill-line "s-meterB" 1000000 65000)) "and every other session by the same rule")
    (is (equal "[context 70k of 180k]" (fill-line "s-meterB" 180000 70000)) "a small window reads it at the floor too, before its half")
    (is (equal "[context 50k of 90k]" (fill-line "s-meterA" 90000 50000)) "and at its half when that share is the smaller bound")
    (is (null (fill-line "s-meterA" 90000 40000)) "under whichever bound is smaller, nothing")
    (let ((nle::*context-line-floor-tokens* nil))
      (is (null (fill-line "s-meterA" 1000000 65000)) "no floor leaves the ratio alone")
      (is (equal "[context 500k of 1M]" (fill-line "s-meterA" 1000000 500000)) "which a 1M window still reaches at its half"))))

(deftest the-wall-clock-names-the-weekday-date-minute-and-offset ()
  ;; 2026-09-16 16:37:05 UTC, a Wednesday.
  (let ((time (encode-universal-time 5 37 16 16 9 2026 0)))
    (is (equal "Wed 2026-09-16 16:37 +00:00" (nle::clock-text time 0)))
    (is (equal "Wed 2026-09-16 18:37 +02:00" (nle::clock-text time -2)) "east of Greenwich")
    (is (equal "Wed 2026-09-16 12:37 -04:00" (nle::clock-text time 4)) "west of it")
    (is (equal "Wed 2026-09-16 22:07 +05:30" (nle::clock-text time -11/2)) "a half-hour zone")
    (is (equal "Thu 2026-09-17 01:37 +09:00" (nle::clock-text time -9)) "across midnight"))
  (is (ppcre:scan "^(Mon|Tue|Wed|Thu|Fri|Sat|Sun) \\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2} [+-]\\d{2}:\\d{2}$"
                  (nle::clock-text))))

(deftest a-turn-budget-is-advice-read-once-as-the-turn-opens ()
  (with-turn-budget ('(:seconds 90 :calls 3) "t1")
    (is (equal '(:seconds 90 :calls 3) (getf (nle:turn) :budget)))
    (is (search "[turn 0s of 1m · 0 rounds · 0 of 3 calls · "
                (gethash "spend" (nle::spend-metadata nil)))))
  (with-turn-budget ('(:seconds -1 :calls 2.5 :other 4))
    (is (null (nle::live-turn-budget (nle::open-live-turn "s-meter" "t2")))))
  (with-turn-budget ((error "the advice broke"))
    (let ((warned nil))
      (handler-bind ((warning (lambda (condition)
                                (setf warned (princ-to-string condition))
                                (muffle-warning condition))))
        (is (null (nle::live-turn-budget (nle::open-live-turn "s-meter" "t3")))))
      (is (search "the advice broke" warned) "and says why"))))

(deftest no-wait-outlasts-what-the-budget-has-left ()
  (is (= 600000 (nle::clamp-exec-yield 900000 10000)) "no turn: the ten-minute ceiling")
  (let ((nle::*live-turn* (nle::open-live-turn "s-meter" "t0")))
    (is (= 600000 (nle::clamp-exec-yield 900000 10000)) "an uncapped turn: the same ceiling"))
  (is (= 10000 (nle::clamp-exec-yield nil 10000)) "no turn: the caller's default")
  (with-turn-budget ('(:seconds 2) "t1")
    (is (<= 50 (nle::clamp-exec-yield 600000 10000) 2000) "the window is what is left")
    (is (= 100 (nle::clamp-exec-yield 100 10000)) "a shorter ask stands"))
  (with-turn-budget ('(:calls 5) "t1")
    (is (= 240000 (nle::clamp-exec-yield 240000 10000)) "a budget of calls leaves waits alone")))

(deftest a-budget-of-tokens-counts-what-bills-and-refuses-past-it ()
  ;; Billable tokens are input, output and reasoning of the turn's rounds,
  ;; never cache reads; past the budget the next call is refused exactly as
  ;; past the calls.
  (with-turn-budget ('(:tokens 100) "t1")
    (is (equal '(:tokens 100) (getf (nle:turn) :budget)))
    (is (search "· 0 calls · 0 of 100 tokens · " (gethash "spend" (nle::spend-metadata nil))))
    (is (null (nle::meter-call)) "nothing spent: the call runs")
    (nle::note-round nil (nle::make-provider-usage :input-tokens 40 :cached-input-tokens 900
                                                   :output-tokens 10 :reasoning-tokens 5))
    ;; cache reads do not bill
    (is (search "· 1 round · 1 call · 55 of 100 tokens · " (gethash "spend" (nle::spend-metadata nil))))
    (is (null (nle::meter-call)))
    (nle::note-round nil (nle::make-provider-usage :input-tokens 30 :cached-input-tokens 500
                                                   :output-tokens 10 :reasoning-tokens 5))
    (is-carrying (line (gethash "spend" (nle::spend-metadata nil)))
      ("· 100 of 100 tokens · " "the second round's spend counts")
      ("the budget is spent" "and the line says so"))
    (is-carrying (refusal (nle::meter-call))
      ("ERROR: refused: this turn has spent its budget (100 of 100 tokens)")
      ("answer now with what is done and what is left"))
    (is (nle::turn-closing-p)))
  (with-turn-budget ('(:tokens 2.5 :calls 1))
    (is (equal '(:calls 1) (nle::live-turn-budget (nle::open-live-turn "s-meter" "t2")))))
  (with-turn-budget ('(:tokens 0))
    (is (null (nle::live-turn-budget (nle::open-live-turn "s-meter" "t3"))))))

(defun budget-turn (session-id replies &aux (nle::*tools* nle::*tools*)
                                            (queue (copy-list replies)))
  "Run one turn of SESSION-ID with a `probe' tool answering `ran', the
provider answering REPLIES in order and signalling past them: a reply is
a message, or (MESSAGE . USAGE) for a round that reports its usage."
  ;; => :COMPLETED
  ;; or :CANCELLED.
  (nle:register-tool "probe" (probe-schema) (lambda (args) (declare (ignore args)) "ran"))
  (with-provider-stub (nil (let ((reply (if queue
                                            (pop queue)
                                            (error "stub provider: the loop asked for a round past the script"))))
                             (if (consp reply)
                                 (values (car reply) (cdr reply))
                                 reply)))
    (nlk:with-handlers ((nlk:turn-cancelled-condition () :cancelled))
      (nle:run "go" :session (nle:intern-session session-id)
                    :command-id (format nil "~a-cmd" session-id))
      :completed)))

(defun probe-calls (&rest ids)
  "An assistant round calling the probe tool once per id in IDS."
  (wire-message "assistant"
                :tool-calls (map 'vector (lambda (id) (wire-tool-call id "probe" "{}")) ids)))

(defmacro with-budget-turn ((results session-id budget outcome &rest replies) &body body)
  "BODY with RESULTS SESSION-ID's tool-result events, once one turn of a fresh
SESSION-ID under BUDGET -- BUDGET-TURN over REPLIES -- is asserted to end as
OUTCOME."
  `(with-session-store (,session-id)
     (with-turn-budget (,budget)
       (is (eq ,outcome (budget-turn ,session-id (list ,@replies)))))
     (let ((,results (session-events ,session-id :type 'nlk:tool-result-event)))
       ,@body)))

(deftest a-call-past-the-budget-is-refused-and-the-model-answers ()
  (with-budget-turn (results "budget-answers" '(:calls 2) :completed
                     (probe-calls "c1" "c2") (probe-calls "c3")
                     (wire-message "assistant" :content "Two probes ran; the third is left."))
    (is (equal '("ran" "ran") (mapcar #'nlk:tool-result-event-result (subseq results 0 2))))
    (is (search "2 of 2 calls" (gethash "spend" (nlk::tool-result-event-metadata (first results)))))
    (is-carrying (refusal (nlk:tool-result-event-result (third results)))
      ("ERROR: refused: this turn has spent its budget (2 of 2 calls)" "the call past it is refused")
      ("answer now with what is done and what is left" "and says what to do instead"))
    (multiple-value-bind (messages clean-p)
        (nle::wire-messages-from-events (session-events "budget-answers" :created nil))
      (is clean-p)
      (let ((first-tool (find "tool" messages :key (lambda (m) (gethash "role" m)) :test #'equal)))
        (is-carrying (content (gethash "content" first-tool))
          (is (eql 0 (search (format nil "ran~%[seq ") content)) "the replayed result keeps its footer")
          (" · turn " "which still carries the turn's spend"))))))

(deftest a-budget-of-no-calls-refuses-the-first-call ()
  (is (equal '(:calls 0) (nle::budget-limits '(:calls 0))))
  (is (null (nle::budget-limits '(:calls -1))))
  (with-budget-turn (results "budget-none" '(:calls 0) :completed
                     (probe-calls "c1") (wire-message "assistant" :content "Nothing ran."))
    (is (= 1 (length results)))
    (is (search "ERROR: refused: this turn has spent its budget (0 of 0 calls)"
                (nlk:tool-result-event-result (first results))))))

(deftest a-call-past-the-token-budget-is-refused-and-the-model-answers ()
  ;; The round's usage folds before its calls are metered, so a round that
  ;; overspends the tokens has its own calls refused and answers next.
  (with-budget-turn (results "budget-tokens" '(:tokens 50) :completed
                     (cons (probe-calls "c1")
                           (nle::make-provider-usage :input-tokens 40 :cached-input-tokens 2000
                                                     :output-tokens 20))
                     (wire-message "assistant" :content "The probe was refused."))
    (is (= 1 (length results)))
    (is-carrying (refusal (nlk:tool-result-event-result (first results)))
      ("ERROR: refused: this turn has spent its budget (60 of 50 tokens)")
      ("answer now with what is done and what is left"))
    (is (session-event "budget-tokens" :kind "turn.completed") "the turn completed")))

(deftest a-round-that-calls-tools-after-the-refusal-ends-the-turn ()
  (with-budget-turn (results "budget-ends" '(:calls 1) :cancelled
                     (probe-calls "c1") (probe-calls "c2") (probe-calls "c3"))
    (is (equal '((:call . "c1") (:result . "c1") (:call . "c2") (:result . "c2")
                 (:call . "c3") (:result . "c3"))
               (tool-fact-order "budget-ends")))
    (is (search "refused: this turn has spent its budget"
                (nlk:tool-result-event-result (car (last results)))))
    (is-present (cancelled (session-event "budget-ends" :kind "turn.cancelled")) "the turn ended cancelled"
      (is (search "budget spent" (nlk:turn-cancelled-reason cancelled))))))
