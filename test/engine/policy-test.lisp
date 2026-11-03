;;;; policy-test.lisp --- the agent's own policy store, and what a turn reads from it.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun policy-request-text (context &aux (head (nle::compiled-turn-context-head-message context)))
  "Everything CONTEXT, one compiled request, sends: the system message, the
head message and every message after it -- where a conversation already
pinned is told what changed since -- as one text."
  (format nil "~a~%~a~%~{~a~^~%~}" (nle::compiled-turn-context-system-prompt context)
          (if (hash-table-p head) (nle::content-text (gethash "content" head)) "")
          (map 'list (lambda (message) (nle::content-text (gethash "content" message)))
               (nle::compiled-turn-context-messages context))))

(deftest policy-store-binds-only-what-adopted-names ()
  ;; The store is a git repository whose `adopted' ref is the one authority:
  ;; a draft on disk binds nothing, every change is a commit that keeps the
  ;; rest of the tree, and a writer that raced another is refused.
  (write-temp-file (nlk:home "SOUL.md") "I am the operator's voice.")
  (let ((start (nle::policy-ensure)))
    (is (= 40 (length start)) "the store starts with a commit on adopted")
    (is (equal "I am the operator's voice." (nle::policy-text "SOUL.md")) "seeded from the home's SOUL.md")
    (is (probe-file (merge-pathnames "SOUL.md" (nle::policy-directory))) "and the draft starts as it")
    (is (null (nle::policy-text "AGENTS.md")) "no rules of its own yet")
    (is (equal start (nle::policy-ensure)) "made once")
    (write-temp-file (merge-pathnames "AGENTS.md" (nle::policy-directory)) "Draft rule.")
    (is (null (nle::policy-text "AGENTS.md")) "a draft on disk binds nothing")
    (let ((one (nle::policy-adopt '(("AGENTS.md" . "Rule one.")) "policy: rule one")))
      (is (equal "Rule one." (nle::policy-text "AGENTS.md")) "an adopted text reads back")
      (is (equal "I am the operator's voice." (nle::policy-text "SOUL.md")) "the rest of the tree is kept")
      (is (equal one (nle::policy-adopt '(("AGENTS.md" . "Rule one.")) "policy: the same")) "no change, no commit")
      (is (signals-error nle::policy-error (nle::policy-commit nle::+policy-adopted-ref+ '(("AGENTS.md" . "Raced.")) "policy: stale" :expect start)) "a writer that raced another is refused")
      (is (equal "Rule one." (nle::policy-text "AGENTS.md")) "and moves nothing")
      (is (signals-error nle::policy-error (nle::policy-adopt '(("../SOUL.md" . "Out.")) "policy: escape")) "only plain names at the store's top")
      (nle::policy-adopt '(("AGENTS.md")) "policy: no rules")
      (is (null (nle::policy-text "AGENTS.md")) "NIL removes a file")
      (is (equal "Rule one." (nle::policy-text "AGENTS.md" :ref one)) "every adopted text stays in the record"))))

(deftest an-adopted-policy-edit-binds-the-next-turn-and-a-draft-never
    (with-session-store ("policy-turn"))
  ;; The mode end to end, through real turns: off, the operator's rules and
  ;; the kit's soul row ride as before; on, the agent's adopted rules ride
  ;; behind the operator's and labelled its own, its adopted identity
  ;; replaces the kit's row in every session, a draft never reaches a
  ;; request, an adoption reaches the next turn of a conversation already
  ;; under way, and a pinned session keeps the text it was pinned to. A
  ;; conversation keeps the prompt it was first given and is told each change
  ;; since, so what must be ABSENT is read off a conversation's first turn.
  (with-temp-shared-config ((pinned-default "deepseek" "deepseek-chat") config)
    (let ((asked '()))
      (labels ((turn (session-id text)
                 (with-provider-stub (context
                                      (push (policy-request-text context) asked)
                                      (wire-message "assistant" :content "ok"))
                   (nle:run text :session (nle:intern-session session-id)))
                 (first asked))
               (fresh (session-id text)
                 ;; a new conversation, carrying the row the channel kit
                 ;; writes from SOUL.md on disk
                 (ensure-durable-session session-id)
                 (nlk:set-harness-section session-id "soul" "Kit voice read from disk.")
                 (turn session-id text)))
        (write-temp-file (nlk:home "SOUL.md") "Operator-written voice.")
        (write-temp-file (nlk:home "AGENTS.md") "Operator rule: use just.")
        (let ((off (fresh "policy-turn" "one")))
          (is (search "Operator rule: use just." off) "off: the operator's rules ride")
          (is (search "Kit voice read from disk." off) "off: the kit's soul row rides")
          (is (not (search "agent-rules" off)) "off: no rules of the agent's own")
          (is (not (probe-file (nle::policy-directory))) "off: no store is made"))
        (nle::config-set '("policy" "self") t)
        (is (search "\"self\": true" (uiop:read-file-string config)) "the switch is one line of the config")
        (let ((on (fresh "policy-on" "two")))
          (is (search "Operator-written voice." on) "on: the identity starts from the operator's")
          (is (not (search "Kit voice read from disk." on)) "on: it replaces the kit's row")
          (is (search "Operator rule: use just." on) "on: the operator's rules still ride")
          (is (not (search "agent-rules" on)) "on: no rules of its own until it adopts some"))
        (write-temp-file (merge-pathnames "AGENTS.md" (nle::policy-directory)) "Draft: never ask.")
        (is (not (search "Draft: never ask." (turn "policy-on" "three"))) "a draft on disk binds nothing")
        (let* ((before (nle::policy-ref-sha nle::+policy-adopted-ref+))
               (after (nle::policy-adopt '(("AGENTS.md" . "Own rule: name the file you changed.")
                                           ("SOUL.md" . "Own voice."))
                                         "policy: own rules and voice")))
          (let ((later (turn "policy-on" "four")))
            (is (search "Own rule: name the file you changed." later) "the adopted edit binds the next turn of a conversation under way")
            (is (search "Own voice." later) "its adopted identity too"))
          (let ((new (fresh "policy-new" "five")))
            (is (search (format nil "Your own rules: you wrote them, and they bind as adopted at ~a" (subseq after 0 7)) new) "labelled the agent's own, at its commit")
            (is (search "The operator's rules outrank them." new) "and below the operator's")
            (is (< (search "<harness key=\"operator-rules\">" new) (or (search "<harness key=\"agent-rules\">" new) 0)) "the operator's rules ride ahead")
            (is (search "Own voice." new) "its adopted identity rides")
            (is (not (search "Operator-written voice." new)) "in place of the seed")
            (is (not (search "Kit voice read from disk." new)) "and of the kit's row"))
          (ensure-durable-session "policy-pinned")
          (nle::policy-pin "policy-pinned" before)
          (let ((pinned (turn "policy-pinned" "six")))
            (is (not (search "Own rule: name the file you changed." pinned)) "a pinned session reads the text it was pinned to")
            (is (search "Operator-written voice." pinned) "identity included"))
          (nle::policy-pin "policy-pinned" nil)
          (is (search "Own voice." (turn "policy-pinned" "seven")) "unpinned, it reads adopted again"))
        (nle::config-set '("policy" "self") :false)
        (let ((off (fresh "policy-off" "eight")))
          (is (not (search "agent-rules" off)) "switched off, the agent's rules leave")
          (is (search "Kit voice read from disk." off) "and the kit's soul row is back"))))))
