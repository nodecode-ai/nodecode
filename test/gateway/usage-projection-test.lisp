;;;; usage-projection-test.lisp --- the usage projection: what /usage and the
;;;; Dashboard add up, summed as the log lands and read as SQL sums
;;;; (gateway/observe.lisp).
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defparameter +tiered-price+ '(1 4 0.1d0 1.25d0 (1000 2 8 0.2d0 2.5d0))
  "A million tokens' price whose prompts over 1,000 tokens bill at twice the rate.")

(deftest usage-saves-each-round-at-the-tier-its-own-prompt-bills (with-temp-gateway (port))
  ;; Two rounds of one session, day and model: one prompt under the tier, one
  ;; over it. Summed first and billed after, both would bill at the upper tier.
  ;; The rollup was derived when no model named the tier, so the read grows it.
  (with-stubbed-fdefinitions ((nle::model-price (model provider)
                                (declare (ignore provider))
                                (and (equal model "tiered") +tiered-price+)))
    (ensure-durable-session "tiers")
    (with-durable-turn (turn "tiers" :input "ask" :complete t)
      (record-call turn "tiered" 0 "a" '(:ttft-ms 300 :stream-duration-ms 1000)
                   '(:input-tokens 100 :cached-input-tokens 500 :output-tokens 10))
      (record-call turn "tiered" 1 "b" '(:ttft-ms 100 :stream-duration-ms 1000)
                   '(:input-tokens 100 :cached-input-tokens 5000 :cache-write-tokens 200
                     :output-tokens 30 :cost-known t :cost-usd 0.25d0))
      (record-call turn "tiered" 2 "c" '(:ttft-ms 200)
                   '(:input-tokens 100 :cached-input-tokens 5000 :output-tokens 10
                     :cost-known t :cost-usd 0.5d0 :voided t))
      (record-call turn "other" 0 "d" '(:ttft-ms 50) '(:input-tokens 7 :output-tokens 1)))
    ;; A session that only ran a turn is listed after the one that spent.
    (seed-completed-turn "idle" "hello" "hi")
    (let ((saved (+ (nle::round-cache-saving +tiered-price+ :input-tokens 100 :cached-input-tokens 500)
                    (nle::round-cache-saving +tiered-price+ :input-tokens 100 :cached-input-tokens 5000
                                                            :cache-write-tokens 200))))
      (with-gateway-http (port :get "/api/gateway/usage?range=today" :status 200)
        (let ((totals (gethash "totals" body))
              (tiered (find "tiered" (gethash "models" body)
                            :key (lambda (m) (gethash "model" m)) :test #'equal)))
          (is (< (abs (- saved (gethash "saved_usd" totals))) 1d-9) "each round's saving at its own tier, the voided one none")
          (is-shape totals ("calls" = 3) ("unpriced" = 2) ("input" = 207) ("cached" = 5500)
            ("cache_write" = 200) ("output" = 41) ("sessions" = 1) ("turns" = 2))
          (is (< (abs (- 0.75d0 (gethash "cost_usd" totals))) 1d-9) "the voided attempt's spend still counted")
          (is-shape tiered ("calls" = 2) ("sessions" = 1) ("wait_p50_ms" = 200) ("speed" = 20))
          (is-shape body ("wait_p50_ms" = 200) ("wait_p90_ms" = 300))
          (is (equal '("tiers" "idle") (map 'list (lambda (s) (gethash "id" s)) (gethash "sessions" body))))
          (is-shape (aref (gethash "sessions" body) 1) ("calls" = 0) ("sessions" = 0)))))
    (is (equal '(1000) (nle::usage-tier-sizes)))))

(deftest usage-projection-follows-the-log-and-rebuilds-a-stale-shape ()
  (with-restartable-store (path "usage-replay" :cwd (fixture-workspace))
    (with-durable-turn (turn "usage-replay" :command-id "usage-a" :input "ask" :complete t)
      (dotimes (i 3)
        (nlk:record-turn-usage turn :provider "vendor" :model "a" :input-tokens 10 :output-tokens 1)))
    (flet ((rounds () (sqlite:execute-single (nlk::require-open-store)
                                             "select coalesce(sum(rounds), 0) from usage_rollup"))
           (spans () (sqlite:execute-single (nlk::require-open-store) "select count(*) from turn_spans"))
           (totals () (gethash "totals" (nle::usage-json "all"))))
      (is (= 3 (rounds)))
      (is (= 1 (spans)))
      (is-shape (totals) ("calls" = 3) ("turns" = 1))
      ;; A table of another shape, its trigger gone, is replaced and derived again.
      (nlk:close-store)
      (sqlite:with-open-database (db path)
        (sqlite:execute-non-query db "drop trigger if exists usage_rollup_follows_the_log")
        (sqlite:execute-non-query db "drop table usage_rollup")
        (sqlite:execute-non-query db "create table usage_rollup (obsolete text)"))
      (nlk:open-store :path path)
      (is (= 3 (rounds)))
      (is (= 1 (spans)))
      (is-shape (totals) ("calls" = 3) ("turns" = 1))
      ;; Its triggers back, a new round and turn land inside the fact's own write.
      (with-durable-turn (turn "usage-replay" :command-id "usage-b" :input "ask" :complete t)
        (nlk:record-turn-usage turn :provider "vendor" :model "a" :input-tokens 10))
      (is (= 4 (rounds)))
      (is (= 2 (spans)))
      (is-shape (totals) ("calls" = 4) ("input" = 40) ("turns" = 2)))))
