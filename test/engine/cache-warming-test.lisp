;;;; cache-warming-test.lisp --- a waiting session's prompt cache, kept alive.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: a refresh repeats the round's own request with
;;;; max_tokens 1 and every other byte as it went, through the round's own
;;;; lane; a request with no breakpoint, or thinking on a budget, arms
;;;; nothing; a refresh comes at nine tenths of the shortest marker's
;;;; lifetime; and the verdict -- warm, wait or stop -- follows the phase, the
;;;; price, the mode, a round on the wire, the warmer's age and the session's
;;;; model.

(in-package #:nodecode.test)

(defun octets-of (text)
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun text-of (octets)
  (sb-ext:octets-to-string octets :external-format :utf-8))

(defparameter +opus-price+ '(5 25 0.5 6.25)
  "A Claude Opus price, dollars per million tokens: input, output, cache read, cache write.")

(deftest a-refresh-sets-max-tokens-to-one-and-keeps-every-other-byte ()
  (let ((body (octets-of "{\"model\":\"m\",\"max_tokens\":64000,\"tools\":[{\"name\":\"t\",\"input_schema\":{\"max_tokens\":7}}],\"messages\":[{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"say \\\"max_tokens\\\":9\",\"cache_control\":{\"type\":\"ephemeral\"}}]}]}")))
    (is (equal "{\"model\":\"m\",\"max_tokens\":1,\"tools\":[{\"name\":\"t\",\"input_schema\":{\"max_tokens\":7}}],\"messages\":[{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"say \\\"max_tokens\\\":9\",\"cache_control\":{\"type\":\"ephemeral\"}}]}]}"
               (text-of (nle::warming-request body))) "the object's own max_tokens, never a nested one or one inside a string")))

(deftest a-request-a-refresh-cannot-repeat-arms-nothing ()
  (let ((bare (octets-of "{\"max_tokens\":10,\"messages\":[]}"))
        (budget (octets-of "{\"max_tokens\":10,\"thinking\":{\"type\":\"enabled\",\"budget_tokens\":4000},\"system\":[{\"type\":\"text\",\"text\":\"s\",\"cache_control\":{\"type\":\"ephemeral\"}}]}"))
        (adaptive (octets-of "{\"max_tokens\":10,\"thinking\":{\"type\":\"adaptive\"},\"system\":[{\"type\":\"text\",\"text\":\"s\",\"cache_control\":{\"type\":\"ephemeral\"}}]}")))
    (is (null (nle::warming-request bare)) "no breakpoint: nothing cached to keep")
    (is (null (nle::warming-request budget)) "thinking on a budget: the budget must stay under max_tokens, and the cache keys on it")
    (is (nle::warming-request adaptive) "adaptive thinking repeats")))

(deftest a-refresh-comes-at-nine-tenths-of-the-shortest-lifetime ()
  (let ((markers (octets-of "{\"system\":[{\"cache_control\":{\"type\":\"ephemeral\"}},{\"cache_control\":{\"type\":\"ephemeral\",\"ttl\":\"1h\"}}]}")))
    (is (equal '(300 3600) (nle::cache-control-ttls markers)) "five minutes unless the marker says an hour"))
  (is (eql 270 (nle::cache-warming-delay 300)))
  (is (eql 3240 (nle::cache-warming-delay 3600)))
  (is (eql 10 (nle::cache-warming-delay 20)) "ten seconds of margin at least")
  (is (null (nle::cache-warming-delay 10)) "too short to refresh"))

(deftest a-refresh-pays-for-itself-or-is-not-sent ()
  ;; 20k tokens on Opus: the write a miss costs is $0.125, the refresh $0.010
  (is (< 0.10 (nle::cache-warming-saving +opus-price+ 20000 :streaming) 0.11) "a turn still open will send its next round")
  (is (< (nle::cache-warming-saving +opus-price+ 20000 :idle) 0.05) "an idle session may never come back: a small prompt is not worth it")
  (is (< 0.05 (nle::cache-warming-saving +opus-price+ 200000 :idle)) "a large one is")
  (is (null (nle::cache-warming-saving nil 20000 :idle)) "no price, no verdict"))

(defmacro with-warmer ((warmer &key (prompt 200000) (open nil) (age 0) selection (mode "idle")) &body body)
  "Run BODY with WARMER, a warmer of session s-warm armed AGE seconds ago on a
PROMPT-token round, its turn OPEN or settled, the session's selection
SELECTION ((PROVIDER MODEL) or NIL) and cache_warming.mode MODE."
  `(with-stubbed-fdefinitions ((nle::model-price (model provider) (declare (ignore model provider)) +opus-price+)
                               (nle::turn-still-open-p (turn) (declare (ignore turn)) ,open)
                               (nle::cache-warming-mode () ,mode)
                               (nlk:session-model-selection (id) (declare (ignore id)) (values-list ',selection)))
     (let ((,warmer (nle::make-cache-warmer :session-id "s-warm" :provider "anthropic" :model "claude-opus-4-5"
                                            :ttl 300 :prompt-tokens ,prompt
                                            :armed-at (- (get-universal-time) ,age))))
       ,@body)))

(deftest the-verdict-follows-phase-price-mode-wire-age-and-model ()
  (with-warmer (w)
    (is (eq :warm (nle::cache-warming-verdict w)) "a large idle prompt"))
  (with-warmer (w :prompt 20000)
    (is (eq :stop (nle::cache-warming-verdict w)) "a small idle prompt"))
  (with-warmer (w :prompt 20000 :open t)
    (multiple-value-bind (action phase) (nle::cache-warming-verdict w)
      (is (eq :warm action) "a small prompt whose turn is still open")
      (is (eq :streaming phase))))
  (with-warmer (w :mode "off")
    (is (eq :stop (nle::cache-warming-verdict w)) "switched off"))
  (with-warmer (w :mode "streaming")
    (is (eq :stop (nle::cache-warming-verdict w)) "streaming only, and the turn settled"))
  (with-warmer (w :age 1900)
    (is (eq :stop (nle::cache-warming-verdict w)) "idle past 30 minutes"))
  (with-warmer (w :age 1900 :open t)
    (is (eq :warm (nle::cache-warming-verdict w)) "an open turn warms for an hour"))
  (with-warmer (w :selection ("anthropic" "claude-sonnet-5"))
    (is (eq :stop (nle::cache-warming-verdict w)) "the session moved to another model"))
  (with-warmer (w :selection ("anthropic" "claude-opus-4-5"))
    (is (eq :warm (nle::cache-warming-verdict w)) "its own model, picked"))
  (with-warmer (w)
    (nle::with-round-in-flight ("s-warm")
      (is (eq :wait (nle::cache-warming-verdict w)) "a round on the wire reads the prefix itself"))
    (is (eq :warm (nle::cache-warming-verdict w)) "and once it landed, the warmer goes on")))

(defmacro capturing-posts ((bodies) &body body)
  "Run BODY with every provider POST answered by the canned Messages stream,
the octets each one sent pushed onto BODIES, newest first."
  `(with-stubbed-fdefinition (dex:post (&rest args)
                               (push (getf (rest args) :content) ,bodies)
                               (values (make-anthropic-stream) 200))
     ,@body))

(deftest a-refresh-repeats-the-round-on-its-own-lane ()
  (let ((sent '())
        (context (anthropic-user-context "go")))
    (capturing-posts (sent)
      (nle::call-anthropic-streaming context)
      (let ((round (first sent)))
        (is (search "\"cache_control\"" (text-of round)) "the round placed its breakpoints")
        (unwind-protect
             (let ((warmer (nle::arm-cache-warming
                            :session-id "s-warm" :lane 'nle::call-anthropic-streaming :context context
                            :request-json round :provider "anthropic" :model "claude-opus-4-5"
                            :usage (nle::make-provider-usage :input-tokens 10 :cached-input-tokens 150000
                                                             :cache-write-tokens 4000))))
               (is-present (armed warmer) "the round armed its session"
                 (is (eq armed (gethash "s-warm" nle::*cache-warmers*)))
                 (is (eql 300 (nle::warmer-ttl armed)))
                 (is (eql 154010 (nle::warmer-prompt-tokens armed)))
                 (nle::refresh-prompt-cache armed)
                 (let ((sent-round (nlk::decode-json round))
                       (refresh (nlk::decode-json (first sent))))
                   (is (eql 2 (length sent)) "one refresh went out")
                   (is (eql 1 (gethash "max_tokens" refresh)))
                   (is (eql t (gethash "stream" refresh)) "streamed, as the lane reads it")
                   (remhash "max_tokens" refresh)
                   (remhash "max_tokens" sent-round)
                   (is (equalp sent-round refresh) "every other member as the round sent it"))))
          (nle::disarm-cache-warmer "s-warm"))
        (is (null (gethash "s-warm" nle::*cache-warmers*)) "disarmed")))))

(deftest a-round-without-breakpoints-leaves-its-session-unarmed ()
  (let ((nle::*anthropic-cache-breakpoints* nil)
        (sent '())
        (context (anthropic-user-context "go")))
    (capturing-posts (sent)
      (nle::call-anthropic-streaming context)
      (is (null (nle::arm-cache-warming
                 :session-id "s-warm" :lane 'nle::call-anthropic-streaming :context context
                 :request-json (first sent) :provider "anthropic" :model "claude-opus-4-5"
                 :usage (nle::make-provider-usage :input-tokens 10 :cached-input-tokens 150000))))
      (is (null (gethash "s-warm" nle::*cache-warmers*))))))
