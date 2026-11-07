;;;; page-test.lisp --- the page is counts only; the cursor, the nonce, the schema.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the page's own contract: it folds the durable
;;;; facts after the cursor into counts per provider and model, per tool and
;;;; per failure class, and copies no text from any of them; a refused send
;;;; keeps the nonce and the cursor so the retry resends the same page; the
;;;; first look begins at the consent moment and sends nothing; never,
;;;; unanswered and the environment switch ship nothing; and every key the
;;;; page carries is one the checked-in schema declares.

(in-package #:nodecode.test)

(defun page-now (&optional (nonce "n"))
  "The page as it would go now, from the cursor to the log's head."
  (nodecode-qa::page (nodecode-qa::cursor) (nodecode-qa::log-head) nonce))

(deftest qa-cell-the-page-is-counts-only (with-session-store ("s1"))
  (with-qa (:share :never)
    (nodecode-qa::begin-cursor)
    (record-a-week (nlk:admit-turn "s1" "c1" "please read /home/me/private.txt"))
    (nlk:complete-turn (nlk:admit-turn "s1" "c2" "again") (nle:message "assistant" "done"))
    (nodecode-qa::keep-note "eval" "timeout" "the snippet (sh \"sleep 100\") never came back")
    (let* ((page (page-now "abc"))
           (body (nlk:encode-json-object page)))
      (is-shape page ("report" "nodecode/weekly/1") ("nonce" "abc")
        ("platform" (nle::release-platform)) ("version" (nle::effective-version)))
      (let ((rounds (gethash "rounds" page)))
        (is (= 2 (length rounds)) "one lane per provider and model")
        (is-present (opus (wire-row "claude-opus-5" rounds "model")) "the opus lane"
          (is-shape opus ("requests" = 2) ("retries" = 1) ("fallbacks" = 1)
            ("input_tokens" = 4000 "3500 to the nearest thousand") ("output_tokens" = 1000)
            ("cached_tokens" = 12000) ("ttft_ms" = 1200 "the upper median of 800 and 1200")))
        (is-present (gpt (wire-row "gpt-5" rounds "model")) "the gpt lane"
          (is-shape gpt ("requests" = 1) ("input_tokens" = 0 "300 rounds to nothing"))
          (is (null (nth-value 1 (gethash "ttft_ms" gpt))) "no first-token time, no key")))
      (let ((turns (gethash "turns" page)))
        (is-shape turns ("completed" = 1) ("cancelled" = 0))
        (is (= 1 (gethash "simple-error" (gethash "failed" turns))) "the failure class, package dropped"))
      (let ((tools (gethash "tools" page)))
        (is (= 2 (length tools)))
        (is-present (tally (wire-row "eval" tools "tool")) "the eval tally"
          (is-shape tally ("calls" = 2) ("errors" = 1 "the ERROR: result counted")
            ("duration_ms" = 10000))))
      (let ((issues (gethash "issues" page)))
        (is (= 1 (length issues)))
        (is (equal "eval" (gethash "tool" (aref issues 0))))
        (is (equal "timeout" (gethash "symptom" (aref issues 0))))
        (is (= 1 (gethash "count" (aref issues 0)))))
      (is (hash-table-p (gethash "crashes" page)))
      ;; Nothing any fact said rides along.
      (is (not (search "private" body)) "no prompt, no path, no result text, no detail")
      (is (not (search "/home/" body)) "no path")
      (is (not (search "sleep 100" body)) "the note's text never rides the page")
      (is (not (search "overloaded" body)) "no retry detail")
      (is (not (search "\"s1\"" body)) "no session id")
      (let ((host (machine-instance)))
        (when (> (length host) 3)
          (is (not (search host body)) "no hostname"))))))

(deftest qa-cell-a-negative-first-token-time-stays-off-the-page (with-session-store ("s1"))
  ;; A negative wait is a broken measurement (a failover once timed the
  ;; candidate from the refused attempt's first token), and the collector
  ;; refuses a page that carries one: the page leaves it out.
  (with-qa (:share :never)
    (nodecode-qa::begin-cursor)
    (let ((turn (nlk:admit-turn "s1" "c1" "hi")))
      (nlk:record-turn-provider-request turn :provider "anthropic" :model "claude-opus-5" :ttft-ms -7863)
      (nlk:record-turn-provider-request turn :provider "anthropic" :model "claude-opus-5" :ttft-ms 900)
      (nlk:record-turn-usage turn :provider "openai" :model "gpt-5"
                                  :input-tokens 300 :output-tokens 100 :finish-reason "stop")
      (nlk:record-turn-provider-request turn :provider "openai" :model "gpt-5" :ttft-ms -24))
    (let ((rounds (gethash "rounds" (page-now))))
      (is-present (opus (wire-row "claude-opus-5" rounds "model")) "the opus lane"
        (is-shape opus ("ttft_ms" = 900 "the one real wait")))
      (is-present (gpt (wire-row "gpt-5" rounds "model")) "the gpt lane"
        (is (null (nth-value 1 (gethash "ttft_ms" gpt))) "no real wait, no key")))))

(deftest qa-cell-a-refused-page-keeps-its-nonce-and-cursor (with-session-store ("s1"))
  (with-stubbed-collector
    (with-qa (:share :weekly)
      (nodecode-qa::ensure-cursor)
      (let ((before (nlk:json-value (nodecode-qa::cursor) :integer "position")))
        (record-a-week (nlk:admit-turn "s1" "c1" "hi"))
        (setf *qa-answer* "HTTP 503")
        (is (search "did not take the page: HTTP 503" (refusal-text qa:qa-error (qa:send))))
        (is (equal "HTTP 503" nodecode-qa::*last-error*))
        (is (= 1 (length *qa-posts*)))
        (let ((cursor (nodecode-qa::cursor)))
          (is (nlk:json-value cursor :text "nonce") "the nonce is kept for the retry")
          (is (= before (nlk:json-value cursor :integer "position")) "the cursor did not move"))
        (is (search "last refused: HTTP 503" (nodecode-qa::status-line)))
        (setf *qa-answer* t)
        (is (search "sent the page" (qa:send)))
        (is (= 2 (length *qa-posts*)))
        (is (equal "https://collector.test/api/qa" (car (first *qa-posts*))))
        (is (equal (gethash "nonce" (qa-post-body 0))
                   (gethash "nonce" (qa-post-body 1))))
        (is (= 2 (gethash "requests" (wire-row "claude-opus-5" (gethash "rounds" (qa-post-body 0))
                                               "model"))))
        (let ((cursor (nodecode-qa::cursor)))
          (is-shape cursor ((:text "nonce") null "the nonce is spent")
            ((:integer "position") = (nodecode-qa::log-head))
            ((:text "since") (gethash "until" (qa-post-body 0)))))
        (is (null nodecode-qa::*last-error*))
        (is (= 0 (length (gethash "rounds" (page-now)))) "the next page is empty")
        (is (a-week-away-p (nodecode-qa::status-line)) "the next page is a week away")))))

(deftest qa-cell-the-first-look-begins-at-consent-and-sends-nothing (with-session-store ("s1"))
  (with-stubbed-collector
    (record-a-week (nlk:admit-turn "s1" "c0" "before consent"))
    (with-qa (:share :weekly)
      (nodecode-qa::look)
      (is (null *qa-posts*) "nothing goes at the first look")
      (let ((cursor (nodecode-qa::cursor)))
        (is (= (nodecode-qa::log-head) (nlk:json-value cursor :integer "position")))
        (is (< 0 (nodecode-qa::seconds-until-due) (1+ nodecode-qa::+period-seconds+))))
      (is (= 0 (length (gethash "rounds" (page-now)))))
      (is (= 0 (length (gethash "tools" (page-now)))))
      ;; The week passes: the look sends.
      (let ((cursor (nodecode-qa::cursor)))
        (setf (gethash "last_sent" cursor) "2020-01-01T00:00:00.000Z")
        (nodecode-qa::write-row nodecode-qa::+cursor-key+ cursor))
      (is (zerop (nodecode-qa::seconds-until-due)))
      (nodecode-qa::look)
      (is (= 1 (length *qa-posts*)) "the page went")
      (is (= 32 (length (gethash "nonce" (qa-post-body)))))
      (is (< 0 (nodecode-qa::seconds-until-due)) "and the next is a period away"))))

(deftest qa-cell-never-unanswered-and-the-environment-ship-nothing (with-session-store ("s1"))
  (with-stubbed-collector
    (record-a-week (nlk:admit-turn "s1" "c1" "hi"))
    (with-qa (:share :never)
      (is (search "not sharing" (refusal-text qa:qa-error (qa:send))))
      (nodecode-qa::look)
      (is (search "not sharing" (nodecode-qa::status-line))))
    (with-qa (:share nil)
      (with-stubbed-fdefinitions ((nodecode-qa::written-share () nil))
        (is (search "not answered" (refusal-text qa:qa-error (qa:send))))
        (is (search "not answered" (nodecode-qa::status-line)))
        ;; The walk stopped asking cell sections; /settings is what asks now.
        (is (search "/settings" (nodecode-qa::status-line)) "names the surface that asks")
        (is (not (search "/setup" (nodecode-qa::status-line))) "never the walk that does not")))
    (with-qa (:share :weekly :off-by-env t)
      (is (search "NODECODE_QA" (refusal-text qa:qa-error (qa:send))))
      (is (search "off by NODECODE_QA" (nodecode-qa::status-line)))
      (let ((nodecode-qa::*operator* t))
        (nodecode-qa::keep-note "eval" "timeout" "x")
        (is (search "NODECODE_QA" (refusal-text qa:qa-error (qa:push-notes))))))
    (is (null *qa-posts*))
    (is-table (value expected) (eq expected (and (nodecode-qa::env-off-p value) t))
      ("0" t) ("off" t) ("never" t) ("FALSE" t) ("" nil) (nil nil) ("1" nil) ("weekly" nil))))

(deftest qa-cell-an-answer-written-since-the-start-is-named-and-ships-nothing
    (with-session-store ("s1"))
  ;; /settings saves qa.share and leaves the restart to the operator: until
  ;; it, /qa says the answer waits instead of `not answered', and send
  ;; refuses whichever way the answer went.
  (with-stubbed-collector
    (record-a-week (nlk:admit-turn "s1" "c1" "hi"))
    (with-qa (:share nil)
      (with-stubbed-fdefinitions ((nodecode-qa::written-share () "weekly"))
        (is (search "qa.share is weekly in config.jsonc, but this cell started unanswered"
                    (nodecode-qa::status-line)))
        (is (not (search "not answered" (nodecode-qa::status-line))))
        (is (search "restart" (refusal-text qa:qa-error (qa:send))))))
    (with-qa (:share :weekly)
      (with-stubbed-fdefinitions ((nodecode-qa::written-share () "never"))
        (is (search "qa.share is never in config.jsonc, but this cell started on weekly"
                    (refusal-text qa:qa-error (qa:send))) "a later never wins")))
    (with-qa (:share :weekly)
      (with-stubbed-fdefinitions ((nodecode-qa::written-share () "weekly"))
        (is (null (nodecode-qa::pending-answer)) "the same answer waits on nothing")))
    (is (null *qa-posts*))))

(deftest qa-cell-the-page-matches-the-checked-in-schema (with-session-store ("s1"))
  (with-qa (:share :never)
    (nodecode-qa::begin-cursor)
    (record-a-week (nlk:admit-turn "s1" "c1" "hi"))
    (nodecode-qa::keep-note "eval" "timeout" "x")
    (let ((schema (nlk:decode-json
                   (uiop:read-file-string
                    (asdf:system-relative-pathname "nodecode-qa" "report.schema.json"))))
          (page (page-now)))
      (labels ((keys (object)
                 (sort (loop for key being the hash-keys of object collect key) #'string<))
               (check (node object path)
                 (let ((declared (keys (gethash "properties" node)))
                       (present (keys object)))
                   (is (null (set-difference present declared :test #'equal))
                       (format nil "~a: every key the page carries is declared: ~a"
                               path present))
                   (is (null (set-difference (coerce (gethash "required" node) 'list) present
                                             :test #'equal))
                       (format nil "~a: every required key is present" path)))
                 (loop for key being the hash-keys of object using (hash-value value)
                       for sub = (gethash key (gethash "properties" node))
                       when sub
                         do (cond ((and (hash-table-p value) (gethash "properties" sub))
                                   (check sub value (format nil "~a.~a" path key)))
                                  ((and (vectorp value) (not (stringp value))
                                        (plusp (length value)) (gethash "items" sub))
                                   (check (gethash "items" sub) (aref value 0)
                                          (format nil "~a.~a[]" path key)))))))
        (check schema page "page"))
      (is (equal (gethash "title" schema) (gethash "report" page))))))
