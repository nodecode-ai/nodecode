;;;; observe-test.lisp --- where the time and money went: a session's
;;;; timeline and the usage across sessions (gateway/observe.lisp).
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun reflection-turn (session-id command input)
  "SESSION-ID's turn under the command id COMMAND admitted as the experience
cell's own request admits it: a reflection, no ask of the operator's."
  (nlk:admit-turn session-id command input
                  :invocation (nlk:default-invocation "turn.start" command :source "experience")))

(defun record-call (turn model round hash request usage)
  "One model call of TURN on vendor's MODEL: its request fact, ROUND and the body
whose SHA-256 is HASH's, with REQUEST's keys, then its usage fact with USAGE's."
  (apply #'nlk:record-turn-provider-request turn :provider "vendor" :model model
         :provider-round round :body-sha256 (nlk::sha256-text hash) request)
  (apply #'nlk:record-turn-usage turn :provider "vendor" :model model usage))

(defun seed-timeline-session (session-id)
  "Two completed turns on SESSION-ID. The first: a call that waits 1.2 s
and reuses nothing (the session's first, so no miss), a 900 ms step, a
retry, and a call that waits 40 s and re-reads 20,000 tokens at full price
because message 5 changed, with no price known. The second: one priced call
that waits 1.5 s."
  (ensure-durable-session session-id)
  (with-durable-turn (turn session-id :input (format nil "look  at~% the routes") :complete t)
    (record-call turn "m1" 0 "a" '(:ttft-ms 1200 :stream-duration-ms 3000
                                   :estimated-context-tokens 12000 :provider-message-count 3)
                 '(:input-tokens 12000 :output-tokens 300 :cost-known t :cost-usd 0.027d0
                   :finish-reason "tool_calls"))
    (record-tool-use turn "call-1" "eval" "{\"form\":\"(+ 1 2)\"}" "3" :duration-ms 900)
    (nlk:record-provider-retry turn :attempt 1 :status 529 :detail "overloaded"
                                    :delay-seconds 2 :provider "vendor" :model "m1")
    (record-call turn "m1" 1 "b" '(:ttft-ms 40000 :stream-duration-ms 2000
                                   :estimated-context-tokens 21000 :provider-message-count 5
                                   :prefix-shared-bytes 800 :prefix-diverged-in "input[5]")
                 '(:input-tokens 20000 :cached-input-tokens 1000 :output-tokens 200
                   :finish-reason "stop"))
    (nlk:record-assistant-message turn (nlk:json-object "role" "assistant" "content" "done")))
  (with-durable-turn (turn session-id :input "again" :complete t)
    (record-call turn "m1" 0 "c" '(:ttft-ms 1500 :stream-duration-ms 1000)
                 '(:input-tokens 100 :cached-input-tokens 21000 :output-tokens 50
                   :cost-known t :cost-usd 0.005d0 :finish-reason "stop"))
    (nlk:record-assistant-message turn (nlk:json-object "role" "assistant" "content" "ok"))))

(deftest a-sessions-timeline-says-where-its-time-and-money-went (with-temp-gateway (port))
  (seed-timeline-session "timeline")
  (with-gateway-http (port :get "/api/gateway/sessions/timeline/timeline" :status 200)
    (let* ((turns (gethash "turns" body))
           (turn (aref turns 0))
           (calls (gethash "calls" turn))
           (steps (gethash "steps" turn))
           (totals (gethash "totals" body)))
      (is (= 2 (length turns)))
      (is-shape turn ("prompt" "look at the routes") ("status" "completed"))
      (is (= 2 (length calls)))
      (is (= 1200 (gethash "wait_ms" (aref calls 0))))
      ;; The session's first call has nothing to reuse: no miss.
      (is (equalp #() (gethash "flags" (aref calls 0))))
      ;; 40 s against a usual 1.5 s, and 20,000 of 21,000 tokens at full price.
      (is-shape (aref calls 1) ("flags" equalp #("slow_start" "cache_miss")) ("diverged_at" = 5))
      ;; An unpriced call's cost is absent, and the turn says its sum is short.
      (is (null (nth-value 1 (gethash "cost_usd" (aref calls 1)))))
      (is (member (gethash "cost_complete" turn) '(nil :false)))
      (is (< (abs (- 0.027d0 (gethash "cost_usd" turn))) 1d-6))
      (is (= 1 (length steps)))
      (is-shape (aref steps 0) ("ms" = 900) ("tool" "eval"))
      (is (= 529 (gethash "status" (aref (gethash "retries" turn) 0))))
      (is (equalp #("slow_start" "cache_miss" "retried") (gethash "flags" turn)))
      (is-shape totals ("calls" = 3) ("steps" = 1) ("usual_wait_ms" = 1500) ("peak_prompt" = 21100))
      (is (< (abs (- 0.032d0 (gethash "cost_usd" totals))) 1d-6))))
  (is-route (port :get "/api/gateway/sessions/nowhere/timeline") 404))

(deftest a-sessions-timeline-lists-what-ran-and-counts-what-was-asked (with-temp-gateway (port))
  ;; A reflection's recap ran nothing in the session it is recorded in: it is no
  ;; row, and no time of the session's. An exit's wake ran a model round of its
  ;; own, so it stays a row -- named by its origin, and numbered by no one.
  (seed-unasked-turns "unasked")
  (with-gateway-http (port :get "/api/gateway/sessions/unasked/timeline" :status 200)
    (let ((totals (gethash "totals" body)))
      (flet ((each (key) (map 'list (lambda (turn) (gethash key turn)) (gethash "turns" body))))
        (is (equal '("first ask" "second ask" "imported ask" "") (each "prompt")) "the recap is not a row")
        (is (equal '(1 2 3 nil) (each "number")) "the asks are numbered in a row, the wake is not")
        (is (equal '(nil nil nil "wake") (each "origin")) "the wake names what woke it")
        (is (equal '(1 2 3 4) (each "n")) "and n still places every row, the address of its turn"))
      (is (= 3 (gethash "turns" totals)) "the totals count the asks")
      (is (= 1 (gethash "calls" totals)) "and still the wake's own round"))))

(deftest a-calls-request-body-reads-back-byte-for-byte (with-temp-gateway (port))
  ;; Three rounds of one turn through the real recording path -- the lane's
  ;; bytes are CALL-PROVIDER-STREAMING's fourth value -- the third changing
  ;; the head the first two shared, as a new system prompt would.
  (ensure-durable-session "sent")
  (let* ((head "{\"model\":\"m\",\"input\":[{\"role\":\"system\",\"content\":\"Café — ~a, the fixed head every round repeats\"},{\"role\":\"user\",\"content\":\"go\"}")
         (step ",{\"type\":\"function_call_output\",\"call_id\":\"c~d\",\"output\":\"round ~:*~d read a file of forty lines\"}")
         (bodies (loop for (system steps) in '(("S" 0) ("S" 1) ("T" 2))
                       collect (nlk:utf-8-octets
                                (format nil "~?~{~?~}],\"tools\":[{\"name\":\"eval\"}]}"
                                        head (list system)
                                        (loop for n from 1 to steps append (list step (list n)))))))
         (served 0))
    (with-streaming-provider-stub (nil nil
                                   (let ((body (nth served bodies)))
                                     (incf served)
                                     (if (< served 3)
                                         (values (wire-message "assistant"
                                                               :tool-calls (vector (wire-tool-call (format nil "c~d" served)
                                                                                                   "eval" "{\"form\": \"1\"}")))
                                                 nil "tool_calls" body)
                                         (values (wire-message "assistant" :content "done") nil "stop" body))))
      (nle:run "go" :session (nle:intern-session "sent") :command-id "sent-cmd"))
    (let ((facts (nlk:events :session-id "sent" :kind nlk::+kind-provider-request+)))
      (is (= 3 (length facts)) "three rounds, three request facts")
      (loop for fact in facts
            for (before this) on (cons nil bodies)
            do (multiple-value-bind (octets status headers)
                   (nlk:http :get (format nil "http://127.0.0.1:~d/api/gateway/sessions/sent/request?turn=~a&round=~d"
                                          port (nlk:event-turn-id fact)
                                          (nlk:provider-request-event-provider-round fact))
                             :headers '(("authorization" . "Bearer test-token")) :binary t)
                 (is (= 200 status))
                 (is (equalp this octets) "the body, byte for byte")
                 ;; Its SHA-256 is the one the request fact recorded, and the
                 ;; bytes it shares with the round before are said beside it.
                 (is (equal (nlk:provider-request-event-body-sha256 fact) (nlk:sha256-text octets)))
                 (is (search "application/json" (gethash "content-type" headers)))
                 (when before
                   (is (= (mismatch before this) (parse-integer (gethash "x-shared-bytes" headers)))))))
      (let ((turn (nlk:event-turn-id (first facts))))
        (is (equal "input[0]" (nlk:provider-request-event-prefix-diverged-in (third facts))))
        (is-route (port :get (format nil "/api/gateway/sessions/sent/request?turn=~a&round=9" turn))
                  404 "a round never kept" "not_kept")))
    (is-route (port :get "/api/gateway/sessions/nowhere/request?turn=t&round=0") 404)))

(deftest usage-sums-every-sessions-rounds-over-a-range (with-temp-gateway (port))
  (seed-timeline-session "spent")
  (seed-completed-turn "idle" "hello" "hi")
  (with-gateway-http (port :get "/api/gateway/usage?range=today" :status 200)
    (let ((totals (gethash "totals" body)))
      (is-shape body ("range" "today"))
      (is-shape totals ("calls" = 3) ("turns" = 3) ("cached" = 22000)
        ;; One of the three rounds had no price: counted, not summed as zero.
        ("unpriced" = 1))
      (is (< (abs (- 0.032d0 (gethash "cost_usd" totals))) 1d-6))
      (is (= 1 (length (gethash "days" body))))
      (is-shape (aref (gethash "models" body) 0) ("model" "m1"))
      (is-shape (aref (gethash "sessions" body) 0) ("id" "spent"))))
  (with-gateway-http (port :get "/api/gateway/usage")
    (is (equal "7d" (gethash "range" body)))
    (is (= 7 (length (gethash "days" body))))))

(deftest usage-says-account-rounds-apart-from-unpriced-ones (with-temp-gateway (port))
  ;; A round nodecode.ai's account paid -- its plan's requests or its credit -- has no API price
  ;; here: counted and said, never `no price' (a buyer's /usage said `1 call has no price',
  ;; checkout audit 2026-10-22).
  (seed-timeline-session "spent")
  (ensure-durable-session "acct")
  (with-durable-turn (turn "acct" :input "hi" :complete t)
    (nlk:record-turn-usage turn :provider "nodecode" :model "cyber-v1" :input-tokens 100 :output-tokens 10))
  (with-gateway-http (port :get "/api/gateway/usage?range=today" :status 200)
    (is-shape (gethash "totals" body) ("calls" = 4) ("unpriced" = 1) ("account_calls" = 1)))
  (is-carrying (text (nle:slash "/usage today"))
    ("1 call ran on your nodecode.ai account, paid in its plan's requests or its credit." "the account's call, said as such")
    ("1 call has no price, so the spend is a floor." "the vendor's unpriced call, still said")))

(deftest the-dashboard-counts-no-reflection-as-the-operators-work (with-temp-gateway (port))
  ;; Without this, a reflection's own session, and the recap it records
  ;; behind the turn, would count as the operator's work. The reflection's
  ;; call is real spend, but it belongs to the session it reflected on.
  (seed-unasked-turns "asked")
  (nlk:create-session :id "asked-note" :cwd (fixture-workspace) :parent "asked")
  (let ((turn (reflection-turn "asked-note" "note-cmd" "[experience] reflect")))
    (nlk:record-turn-usage turn :provider "vendor" :model "m1" :input-tokens 500 :output-tokens 20)
    (nlk:complete-turn turn nil))
  (with-gateway-http (port :get "/api/gateway/usage?range=today")
    (let ((totals (gethash "totals" body)))
      (is (= 1 (gethash "sessions" totals)) "the note's call is its parent's")
      (is (= 2 (gethash "calls" totals)) "and still counted: the wake's and the note's")
      (is (= 4 (gethash "turns" totals)) "two asks, the import and the wake: no recap, no note")))
  (with-gateway-http (port :get "/api/gateway/activity?range=today")
    (is (= 4 (reduce #'+ (map 'list (lambda (day) (reduce #'+ day)) (gethash "hours" body)))) "the hours count the same turns"))
  (is (= 1 (nle::session-count)) "and the store's count leaves the note out"))

(deftest usage-answers-in-words-on-a-shell-and-without-one (with-temp-gateway (port))
  (seed-timeline-session "spent")
  ;; Without a shell (a channel, the web page's `/'), in the gateway's image.
  (is-carrying (text (nle:slash "/usage today"))
    ("Usage · today" "the range leads")
    ("Today, $0.03 across 1 session." "the sentence the Usage view leads with")
    ("3 model calls." "the calls")
    ("1 call has no price, so the spend is a floor." "an unpriced call is said, not summed as zero")
    ("0 of 2 turns failed.")
    ("m1" "the model table")
    ("costliest sessions" "the costliest session"))
  (is (equal "usage: no range by that name; /usage alone offers today, 7 days, 30 days and all"
             (nle:slash "/usage fortnight")))
  ;; On a shell, a dialog: a tab per range, each filled from the gateway's
  ;; read (in order here: a test drives no loop), the asked range showing.
  (let ((model (make-frame-model 100 40 :session-id "other" :http-port port :http-token "test-token")))
    (is (null (nle::execute-slash-command model "usage" "today")))
    (let* ((dialog (nle::chat-model-dialog model))
           (shown (nle::picker-visible-rows dialog))
           (labels (mapcar #'nle::picker-row-label shown)))
      (is (eq :usage (nle::picker-dialog-kind dialog)))
      (is (equal '("Today" "7 days" "30 days" "All")
                 (mapcar (lambda (tab) (getf tab :label)) (nle::picker-dialog-tabs dialog))))
      (is (= 0 (nle::picker-dialog-tab dialog)))
      (is (equal "Today, $0.03 across 1 session." (first labels)))
      (is (find "1 call has no price, so the spend is a floor." labels :test #'equal))
      ;; Every range was read; the 7-day tab holds its own sentence.
      (is (member "This week, $0.03 across 1 session." (row-labels dialog) :test #'equal))
      ;; Enter on the costliest session opens it.
      (is (equal '(:session "spent")
                 (nle::picker-row-value (find-if (lambda (row) (consp (nle::picker-row-value row)))
                                                 shown)))))
    (is (equal "usage: no range by that name; /usage alone offers today, 7 days, 30 days and all"
               (nle::execute-slash-command model "usage" "fortnight"))))
  ;; Typed alone without a shell, the four ranges are its rows.
  (multiple-value-bind (text dialog) (nle:slash "/usage")
    (is (search "The last 7 days" text))
    (is (equal '("/usage today" "/usage 7d" "/usage 30d" "/usage all")
               (map 'list (lambda (row) (nlk:json-value row :text "run" "enter")) (nlk:json-array dialog "rows")))))
  (is (null (nle:slash-completions "usage" "7")) "nothing completes words after a name"))

(deftest activity-says-what-the-agent-did-and-what-went-wrong (with-temp-gateway (port))
  (seed-timeline-session "spent")
  ;; A turn whose command failed, then the turn itself.
  (ensure-durable-session "broke")
  (let ((turn (nlk:admit-turn "broke" (nlk::make-durable-id "command") "run the tests")))
    (record-tool-use turn "call-9" "eval" "{\"form\":\"(sh \\\"just test\\\")\"}" "ERROR: exit 2"
                     :duration-ms 4000)
    (nlk:fail-turn turn (make-condition 'simple-error :format-control "provider gave up")))
  (with-gateway-http (port :get "/api/gateway/activity?range=today" :status 200)
    (let ((failed (gethash "failed_turns" body))
          (families (map 'list (lambda (family) (gethash "family" family)) (gethash "families" body)))
          (calls (gethash "calls" body)))
      (is-shape (gethash "steps" body) ("count" = 2) ("failed" = 1) ("ms" = 4900))
      ;; Each step sorted as its card titles it: a snippet, a command.
      (is (subsetp '("eval" "command") families :test #'equal))
      ;; The slowest step first, in words, with its session.
      (is-shape (aref (gethash "slowest" body) 0) ("ms" = 4000) ("session_id" "broke"))
      ;; Every turn started lands in an hour of the week.
      (is (= 3 (reduce #'+ (gethash "hours" body) :key (lambda (day) (reduce #'+ day)))))
      (is (= 529 (gethash "status" (aref (gethash "retries" body) 0))))
      (is (= 1 (gethash "count" failed)))
      (is (equal "broke" (gethash "session_id" (aref (gethash "latest" failed) 0))))
      (is (search "provider gave up" (gethash "detail" (aref (gethash "latest" failed) 0))))
      ;; The newest call first, its wait from its own request.
      (is (= 3 (length calls)))
      (is (= 1500 (gethash "wait_ms" (aref calls 0))))
      (is (= 40000 (gethash "wait_ms" (aref calls 1))))
      (is (< (abs (- 0.005d0 (gethash "cost_usd" (aref calls 0)))) 1d-6))))
  ;; The usage read's models carry their typical wait and their speed.
  (with-gateway-http (port :get "/api/gateway/usage?range=today")
    (let ((model (aref (gethash "models" body) 0)))
      (is (= 1500 (gethash "wait_p50_ms" model)))
      ;; 550 tokens written over 6 s of writing.
      (is (= 92 (gethash "speed" model))))))

(defun seed-failing-folders ()
  "Two folders. In alpha: a turn whose $0.01 round asked for a command and a
snippet, whose attempt failed voided ($0.001) and then 529, and whose last
$0.004 round asked for nothing; a turn that met 429 twice on m2 and failed
naming it; and a turn whose m1 request stands before a 500 its failure fact
does not name a model for. In beta: one turn, a $0.002 round on m3 and an
unpriced one, nothing failed."
  (nlk:create-session :id "split" :cwd (format nil "~aalpha" (fixture-workspace)))
  (nlk:create-session :id "calm" :cwd (format nil "~abeta" (fixture-workspace)))
  (flet ((round-of (turn model &optional cost &rest more)
           (record-call turn model 0 model nil
                        `(:input-tokens 100 :output-tokens 10
                          ,@(and cost (list :cost-known t :cost-usd cost)) ,@more)))
         (provider-failure (status model &aux (nle::*provider* "vendor") (nle::*model* model))
           (make-condition 'nle::provider-error :status status :detail "no")))
    (with-durable-turn (turn "split" :input "split the cost" :complete t)
      (round-of turn "m1" 0.01d0)
      (nlk:record-tool-call turn "c1" "eval" "{\"form\":\"(sh \\\"just test\\\")\"}")
      (nlk:record-tool-call turn "c2" "eval" "{\"form\":\"(+ 1 2)\"}")
      (nlk:record-tool-result turn "c1" "eval" "ok" :duration-ms 10)
      (nlk:record-tool-result turn "c2" "eval" "3" :duration-ms 10)
      (nlk:record-turn-usage turn :provider "vendor" :model "m1" :voided t :cost-known t :cost-usd 0.001d0)
      (nlk:record-provider-retry turn :attempt 1 :status 529 :detail "overloaded" :provider "vendor" :model "m1")
      (round-of turn "m1" 0.004d0))
    (let ((turn (nlk:admit-turn "split" (nlk::make-durable-id "command") "busy")))
      (nlk:record-provider-retry turn :attempt 1 :status 429 :detail "slow down" :provider "vendor" :model "m2")
      (let ((failure (provider-failure 429 "m2")))
        (nlk:fail-turn turn failure :evidence (nle::provider-failure-evidence failure))))
    (let ((turn (nlk:admit-turn "split" (nlk::make-durable-id "command") "older")))
      (nlk:record-turn-provider-request turn :provider "vendor" :model "m1" :provider-round 0
                                             :body-sha256 (nlk::sha256-text "older"))
      (nlk:fail-turn turn (provider-failure 500 "m1") :evidence (list :status 500)))
    (with-durable-turn (turn "calm" :input "hello" :complete t)
      (round-of turn "m3" 0.002d0)
      (round-of turn "m3"))))

(deftest the-dashboard-says-what-went-wrong-per-model-and-folder-and-where-the-money-went
    (with-temp-gateway (port))
  (seed-failing-folders)
  (let ((total nil))
    (flet ((row (rows key value) (find-if (lambda (row) (search value (gethash key row))) rows)))
      (with-gateway-http (port :get "/api/gateway/usage?range=today")
        (setf total (gethash "cost_usd" (gethash "totals" body)))
        ;; m1: two rounds, a retry and the failure its last request stood before.
        (let ((m1 (row (gethash "models" body) "model" "m1")))
          (is-shape m1 ("attempts" = 4) ("failed" = 2) ("error_rate" = 0.5d0))
          (is (equal '((529 . 1) (500 . 1))
                     (sort (map 'list (lambda (s) (cons (gethash "status" s) (gethash "count" s)))
                                (gethash "statuses" m1))
                           #'> :key #'car))))
        ;; m2 never answered: a row of its own, every attempt failed.
        (let ((m2 (row (gethash "models" body) "model" "m2")))
          (is-shape m2 ("calls" = 0) ("attempts" = 2) ("error_rate" = 1d0))
          (is-shape (aref (gethash "statuses" m2) 0) ("status" = 429) ("count" = 2)))
        ;; A model that never failed reads a zero rate, not an absence.
        (is (= 0d0 (gethash "error_rate" (row (gethash "models" body) "model" "m3"))))
        (let ((alpha (row (gethash "folders" body) "folder" "alpha"))
              (beta (row (gethash "folders" body) "folder" "beta")))
          (is-shape alpha ("turns" = 3) ("failed_turns" = 2))
          (is (< (abs (- 2/3 (gethash "error_rate" alpha))) 1d-6))
          (is-shape beta ("failed_turns" = 0) ("error_rate" = 0d0))))
      (with-gateway-http (port :get "/api/gateway/activity?range=today")
        (let* ((spend (gethash "spend" body))
               (parts (gethash "parts" spend)))
          (is (= 4 (gethash "rounds" spend)))
          ;; Three of four rounds priced: the parts are weighed by the money.
          (is (equal "cost" (gethash "by" spend)))
          ;; The unpriced round is counted, never summed as zero.
          (is (= 1 (gethash "unpriced" spend)))
          (is (< (abs (- 0.017d0 (gethash "cost_usd" spend))) 1d-6))
          ;; The Spend tile and this section say the same total, and the
          ;; parts sum to it.
          (is (< (abs (- total (gethash "cost_usd" spend))) 1d-6))
          (is (< (abs (- total (reduce #'+ parts :key (lambda (part) (gethash "cost_usd" part 0))))) 1d-6))
          ;; The $0.01 round split over its command and its snippet.
          (let ((command (row parts "family" "command")) (snippet (row parts "family" "eval")))
            (is (< (abs (- 0.005d0 (gethash "cost_usd" command))) 1d-6))
            (is (equal "Ran" (gethash "verb" command)))
            (is (< (abs (- 0.005d0 (gethash "cost_usd" snippet))) 1d-6))
            (is (= 0.5d0 (gethash "rounds" snippet))))
          ;; Three rounds asked for nothing, two of them priced; the voided
          ;; attempt's spend stands beside them.
          (let ((reply (find-if (lambda (part) (gethash "reply" part)) parts)))
            (is (< (abs (- 0.006d0 (gethash "cost_usd" reply))) 1d-6))
            (is (= 3 (gethash "calls" reply))))
          (is (< (abs (- 0.001d0 (gethash "cost_usd" (find-if (lambda (part) (gethash "voided" part)) parts))))
                 1d-6)))))))

(defun ranked-models (rows)
  "ROWS, a frequent-models read, as (PROVIDER MODEL TURNS) lists in rank order."
  (map 'list (lambda (row) (mapcar (lambda (key) (gethash key row)) '("provider" "model" "turns")))
       rows))

(defun frequent-models (port)
  "What GET models/frequent answers on PORT, as RANKED-MODELS reads it."
  (with-gateway-http (port :get "/api/gateway/models/frequent" :status 200)
    (ranked-models (gethash "models" body))))

(deftest frequent-models-rank-distinct-asked-turns-and-recent-ties (with-temp-gateway (port))
  (is (null (frequent-models port)))
  (ensure-durable-session "model-history")
  (flet ((used (provider model &key (rounds 1) voided (session "model-history") invocation)
           (let ((turn (funcall (if invocation #'reflection-turn #'nlk:admit-turn)
                                session (nlk::make-durable-id "command") "ask")))
             (dotimes (i rounds)
               (nlk:record-turn-usage turn :provider provider :model model
                                          :input-tokens 10 :output-tokens 1 :voided voided))
             (nlk:complete-turn turn nil))))
    (used "vendor" "often")
    (used "vendor" "often")
    (used "vendor" "older" :rounds 8)
    (used "other" "often")
    (used "vendor" "newer")
    (used "vendor" "void" :voided t)
    (used "vendor" "")
    (nlk:create-session :id "model-note" :cwd (fixture-workspace) :parent "model-history")
    (used "vendor" "reflection" :session "model-note"
          :invocation t))
  (is (equal '(("vendor" "often" 2) ("vendor" "newer" 1)
               ("other" "often" 1) ("vendor" "older" 1))
             (frequent-models port))))

(deftest frequent-models-rebuild-from-usage-and-reflection-completions (with-temp-gateway (port))
  (ensure-durable-session "rank-history")
  (ensure-durable-session "rank-other-history")
  (let ((first (nlk:admit-turn "rank-history" "rank-first" "ask"))
        (second (nlk:admit-turn "rank-other-history" "rank-second" "ask")))
    ;; A large loop contributes one distinct turn; two providers of one
    ;; model remain separate, and voided/missing model facts never rank.
    (dotimes (i 100)
      (nlk:record-turn-usage first :provider "vendor" :model "loop"))
    (nlk:record-turn-usage second :provider "other" :model "loop")
    (nlk:record-turn-usage second :provider "vendor" :model "void" :voided t)
    (nlk:record-turn-usage second :provider "" :model "missing-provider")
    (nlk:record-turn-usage second :provider "vendor" :model "")
    (is (equal '(("other" "loop" 1) ("vendor" "loop" 1)) (frequent-models port)))
    ;; No cache may retain the second turn after its reflection completion.
    (nlk:complete-turn first nil)
    (nlk::record-turn-terminal second nlk::+kind-turn-completed+
                               '(:recorder "reflection" :turn-number 1 :message-count 0))
    (is (equal '(("vendor" "loop" 1)) (frequent-models port)))
    (with-durable-turn (third "rank-history" :command-id "rank-third" :input "ask" :complete t)
      (nlk:record-turn-usage third :provider "vendor" :model "loop"))
    (is (= 2 (nth 2 (first (frequent-models port)))))))

(deftest frequent-models-read-the-distinct-turn-projection (with-session-store ("rank-plan"))
  (let* ((statements (statements-read-by #'nle::frequent-models-json))
         (statement (first statements))
         (plan (format nil "~{~a~^; ~}"
                       (mapcar #'fourth
                               (sqlite:execute-to-list
                                (nlk::require-open-store)
                                (concatenate 'string "explain query plan " statement))))))
    (is (= 1 (length statements)))
    (is (search "GROUP BY" (string-upcase statement)))
    (is (search "SCAN model_history" plan) plan)
    (is (not (search "events" plan)))
    (is (not (search "json_extract" statement)))
    (is (not (sqlite:execute-single (nlk::require-open-store)
                                   "select sql from sqlite_master where name = 'events_usage_by_model'")))))

(deftest frequent-models-projection-tracks-late-facts-and-rollback (with-session-store ("rank-late"))
  (labels ((rows () (ranked-models (nle::frequent-models-json))))
    (ensure-durable-session "rank-late-other")
    (let ((a (nlk:admit-turn "rank-late" "late-a" "ask"))
          (b (nlk:admit-turn "rank-late-other" "late-b" "ask")))
      (nlk:record-turn-usage a :provider "vendor" :model "a")
      (nlk:record-turn-usage b :provider "vendor" :model "b")
      (is (equal '(("vendor" "b" 1) ("vendor" "a" 1)) (rows)))
      ;; More rounds change recency, not the number of distinct turns.
      (nlk:record-turn-usage a :provider "vendor" :model "a")
      (is (equal '(("vendor" "a" 1) ("vendor" "b" 1)) (rows)))
      (nlk:record-turn-usage b :provider "vendor" :model "b" :voided t)
      (is (equal '(("vendor" "a" 1) ("vendor" "b" 1)) (rows)))
      (nlk::record-turn-terminal b nlk::+kind-turn-completed+
                                 '(:recorder "reflection" :turn-number 1 :message-count 0))
      ;; A delayed replay append after the terminal reflection may not re-enter it.
      (nlk::append-session-fact "rank-late-other" nlk::+kind-turn-usage+
                                '(:provider "vendor" :model "b")
                                :turn-id (nlk:durable-turn-turn-id b))
      (is (equal '(("vendor" "a" 1)) (rows)))
      (nlk:complete-turn a nil))
    ;; A session's first source can arrive after imported usage facts.
    (ensure-durable-session "rank-late-source")
    (nlk::append-session-fact "rank-late-source" nlk::+kind-turn-usage+
                              '(:provider "vendor" :model "imported") :turn-id "late-import")
    (is (equal '(("vendor" "imported" 1) ("vendor" "a" 1)) (rows)))
    (let ((note (reflection-turn "rank-late-source" "late-source" "recap")))
      (nlk:record-turn-usage note :provider "vendor" :model "note")
      (is (equal '(("vendor" "a" 1)) (rows)))
      (nlk:complete-turn note nil))
    ;; Both the event and its projection disappear on rollback; facts stay
    ;; authoritative even when the append is made by another writer.
    (is (signals-error error
          (nlk::with-store-transaction (handle)
            (let ((nlk::*event-write-owner* (nlk::event-kind-owner nlk::+kind-turn-usage+)))
              (nlk::append-event-row handle "rank-late" nlk::+kind-turn-usage+
                                    '(:provider "vendor" :model "rolled-back")
                                    :turn-id "rollback-turn"))
            (is (equal '(("vendor" "rolled-back" 1) ("vendor" "a" 1)) (rows)))
            (error "roll back"))))
    (is (equal '(("vendor" "a" 1)) (rows)))))

(deftest frequent-models-projection-rebuilds-a-stale-shape ()
  (with-restartable-store (path "rank-replay" :cwd (fixture-workspace))
    (with-durable-turn (turn "rank-replay" :command-id "replay-a" :input "ask" :complete t)
      (dotimes (i 30) (nlk:record-turn-usage turn :provider "vendor" :model "a")))
    (let ((expected (nlk:encode-json-object (nlk:json-object "models" (nle::frequent-models-json)))))
      (nlk:close-store)
      (sqlite:with-open-database (db path)
        ;; A stale table must be replaced, not blessed by IF NOT EXISTS.
        (dolist (name '("model_history_follows_usage" "model_history_follows_completion"
                        "model_history_follows_source"))
          (sqlite:execute-non-query db (format nil "drop trigger if exists ~a" name)))
        (sqlite:execute-non-query db "drop table if exists model_history")
        (sqlite:execute-non-query db "create table model_history (obsolete text)"))
      (nlk:open-store :path path)
      (is (string= expected (nlk:encode-json-object (nlk:json-object "models" (nle::frequent-models-json)))))
      (is (= 1 (sqlite:execute-single (nlk::require-open-store) "select count(*) from model_history")))
      (is (search "log_position" (sqlite:execute-single (nlk::require-open-store)
                                                        "select sql from sqlite_master where name = 'model_history'")))
      ;; An unchanged reopen preserves the derived rows and every field/order.
      (reopen-store path)
      (is (string= expected (nlk:encode-json-object (nlk:json-object "models" (nle::frequent-models-json)))))
      ;; A table with the right shape but an obsolete trigger is stale too.
      (nlk::with-store-handle (db)
        (sqlite:execute-non-query db "drop trigger model_history_follows_usage")
        (sqlite:execute-non-query db "create trigger model_history_follows_usage
                                      after insert on events when new.kind = 'obsolete'
                                      begin select 1; end"))
      (reopen-store path)
      (is (search "turn.usage" (sqlite:execute-single (nlk::require-open-store)
                                                      "select sql from sqlite_master where name = 'model_history_follows_usage'")))
      (with-durable-turn (turn "rank-replay" :command-id "replay-b" :input "ask" :complete t)
        (nlk:record-turn-usage turn :provider "vendor" :model "a"))
      (is (= 2 (gethash "turns" (aref (nle::frequent-models-json) 0)))))))
