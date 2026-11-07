;;;; support.lisp --- qa test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; QA tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a
;;;; QA-CELL- name prefix; RUN-QA-TESTS runs exactly that
;;;; slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-QA::*POST*, the
;;;; function the collector is asked with; WITH-STUBBED-COLLECTOR captures
;;;; every post and answers what the test names, so nothing ever dials. The
;;;; sender thread is never started here: WITH-QA stubs START-SENDER out and
;;;; a test runs NODECODE-QA::LOOK by hand, so nothing races the
;;;; assertions; one lifecycle test starts the whole cell.

(in-package #:nodecode.test)

(define-test-slice "qa" "QA-CELL-" :start nodecode-qa:start-cell)

(defvar *qa-posts* '()
  "(URL . BODY) the stubbed collector received, newest first.")

(defvar *qa-answer* t
  "What the stubbed collector answers: T, or the reason it refuses with.")

(defmacro with-stubbed-collector (&body body)
  "Run BODY with the collector stubbed: every post lands on
*QA-POSTS*, answered as *QA-ANSWER* says."
  `(with-saved-globals (nodecode-qa::*post* nodecode-qa::*last-error* nle:*tools*)
     (setf *qa-posts* '()
           *qa-answer* t
           nodecode-qa::*last-error* nil
           nodecode-qa::*post*
           (lambda (url body)
             (push (cons url body) *qa-posts*)
             (if (eq *qa-answer* t)
                 t
                 (values nil *qa-answer*))))
     ,@body))

(defmacro with-qa ((&key share off-by-env) &body body)
  "Run BODY with the cell started against a test collector and no sender
thread; stopped on unwind."
  ;; SHARE is :weekly, :never or NIL (unanswered) and goes into the `qa'
  ;; section the way the operator writes it; OFF-BY-ENV stands in for
  ;; NODECODE_QA=0, which is read off the environment rather than the config.
  ;; START-SENDER is stubbed out: the tests run LOOK by hand, so nothing races
  ;; the assertions. The tool registry is saved around it, so a stop that
  ;; unregisters leaves the image as it was.
  `(with-saved-globals (nle:*tools*)
     (with-stubbed-fdefinitions
         ((nodecode-qa::env-off-p (&optional value) ,(and off-by-env t))
          (nodecode-qa::start-sender () nil))
       (with-cell-stop ((qa-start "url" "https://collector.test"
                                   ,@(when share (list "share" (string-downcase share)))))
         ,@body))))

(defun qa-post-body (&optional (index 0))
  "The INDEXth newest body the collector received, decoded."
  (nlk:decode-json (cdr (nth index *qa-posts*))))

(defun a-week-away-p (line)
  "Whether LINE says the next page is a week away: 7d 0h in the second the
last one went, 6d 23h from the next second on."
  (or (search "next page in 7d 0h" line)
      (search "next page in 6d 23h" line)))

(defun qa-slash (args)
  "/qa ARGS in session s1 => its answer."
  (nodecode-qa::run-slash args "s1"))

(defun record-a-week (turn)
  "A turn's worth of facts on TURN, every one carrying text a page must not
copy: three rounds on two models, two first-token times, a retry, a
failover, three tool results one of which failed, and the turn's failure."
  (nlk:record-turn-usage turn :provider "anthropic" :model "claude-opus-5"
                              :input-tokens 1400 :output-tokens 600 :cached-input-tokens 12000
                              :finish-reason "stop")
  (nlk:record-turn-usage turn :provider "anthropic" :model "claude-opus-5"
                              :input-tokens 2100 :output-tokens 400 :finish-reason "tool_calls")
  (nlk:record-turn-usage turn :provider "openai" :model "gpt-5"
                              :input-tokens 300 :output-tokens 100 :finish-reason "stop")
  (nlk:record-turn-provider-request turn :provider "anthropic" :model "claude-opus-5" :ttft-ms 800)
  (nlk:record-turn-provider-request turn :provider "anthropic" :model "claude-opus-5" :ttft-ms 1200)
  (nlk:record-provider-retry turn :provider "anthropic" :model "claude-opus-5"
                                  :status 529 :detail "overloaded at /home/me/private")
  (nlk:record-provider-fallback turn :from-provider "anthropic" :from-model "claude-opus-5"
                                     :to-provider "openai" :to-model "gpt-5" :reason "overloaded")
  (nlk:record-tool-result turn "c1" "eval" "42" :duration-ms 12)
  (nlk:record-tool-result turn "c2" "eval" "ERROR: EVAL-TIMEOUT: /home/me/private.txt took too long"
                          :duration-ms 10000)
  (nlk:record-tool-result turn "c3" "look" "attached /home/me/private.png" :duration-ms 30)
  (nlk:fail-turn turn (make-condition 'simple-error
                                      :format-control "the provider answered 500 for /home/me/private")))
