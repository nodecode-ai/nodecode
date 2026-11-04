;;;; policy-test.lisp --- the agent's own policy store, and what a turn reads from it.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun policy-test-git (dir &rest args)
  "Git ARGS in DIR with a test identity, its trimmed stdout; signals on a failure."
  (string-trim '(#\Space #\Newline)
               (uiop:run-program (list* "git" "-C" (uiop:native-namestring dir)
                                        "-c" "user.name=Policy Test" "-c" "user.email=policy@test.invalid"
                                        args)
                                 :output :string :error-output :string)))

(defun policy-request-text (context &aux (head (nle::compiled-turn-context-head-message context)))
  "Everything CONTEXT, one compiled request, sends: the system message, the
head message and every message after it -- where a conversation already
pinned is told what changed since -- as one text."
  (format nil "~a~%~a~%~{~a~^~%~}" (nle::compiled-turn-context-system-prompt context)
          (if (hash-table-p head) (nle::content-text (gethash "content" head)) "")
          (map 'list (lambda (message) (nle::content-text (gethash "content" message)))
               (nle::compiled-turn-context-messages context))))

(deftest policy-store-commits-every-edit-and-binds-it ()
  ;; The store is plain files on a git branch: an edit on disk is committed
  ;; by the next read and binds, each edit is a commit of its own that keeps
  ;; the rest of the tree, git revert undoes one, and a writer that raced
  ;; another is refused.
  (write-temp-file (nlk:home "SOUL.md") "I am the operator's voice.")
  (let ((start (nle::policy-ensure))
        (dir (nle::policy-directory)))
    (is (= 40 (length start)) "the store starts with a commit on adopted")
    (is (equal "I am the operator's voice." (nle::policy-text "SOUL.md")) "seeded from the home's SOUL.md")
    (is (probe-file (merge-pathnames "SOUL.md" dir)) "as a file the agent edits")
    (is (null (nle::policy-text "AGENTS.md")) "no rules of its own yet")
    (is (equal start (nle::policy-ensure)) "made once")
    (is (equal start (nle::policy-sync)) "nothing changed, no commit")
    (write-temp-file (merge-pathnames "AGENTS.md" dir) "Rule one.")
    (let ((one (nle::policy-sync)))
      (is (not (equal start one)) "an edit on disk is committed by the next read")
      (is (equal "Rule one." (nle::policy-text "AGENTS.md")) "and binds")
      (is (equal "policy: AGENTS.md" (policy-test-git dir "log" "-1" "--format=%s")) "the commit names the file")
      (is (equal "" (policy-test-git dir "status" "--porcelain")) "the store reads clean after it")
      (is (equal "I am the operator's voice." (nle::policy-text "SOUL.md")) "the rest of the tree is kept")
      (let ((two (nle::policy-adopt '(("AGENTS.md" . "Rule two.")) "policy: ask before a delete")))
        (is (equal "Rule two." (uiop:read-file-string (merge-pathnames "AGENTS.md" dir))) "policy-adopt writes the file")
        (is (equal "policy: ask before a delete" (policy-test-git dir "log" "-1" "--format=%s")) "with a reason of its own")
        (is (equal two (nle::policy-adopt '(("AGENTS.md" . "Rule two.")) "policy: the same")) "no change, no commit")
        (is (signals-error nle::policy-error (nle::policy-commit nle::+policy-adopted-ref+ '(("AGENTS.md" . "Raced.")) "policy: stale" :expect start)) "a writer that raced another is refused")
        (is (equal two (nle::policy-sync)) "and moves nothing"))
      (policy-test-git dir "revert" "--no-edit" "HEAD")
      (is (equal "Rule one." (nle::policy-text "AGENTS.md" :ref (nle::policy-sync))) "git revert in the store undoes an edit")
      (is (signals-error nle::policy-error (nle::policy-adopt '(("../SOUL.md" . "Out.")) "policy: escape")) "only plain names at the store's top")
      (delete-file (merge-pathnames "AGENTS.md" dir))
      (is (null (nle::policy-text "AGENTS.md" :ref (nle::policy-sync))) "a file removed on disk leaves the policy")
      (is (equal "Rule one." (nle::policy-text "AGENTS.md" :ref one)) "every edit stays in the record"))
    (write-temp-file (merge-pathnames "SYSTEM.md" dir) "Own prompt.")
    (nle::policy-sync)
    (is (equal nle::*system-prompt* (nle::policy-text "SYSTEM.shipped")) "a prompt edit keeps the shipped prompt it was written against")
    (is (equal nle::*system-prompt* (uiop:read-file-string (merge-pathnames "SYSTEM.shipped" dir))) "on disk too, so the next read keeps it")))

(deftest an-edit-of-the-agents-own-policy-binds-the-next-turn
    (with-session-store ("policy-turn"))
  ;; The mode end to end, through real turns: off, the operator's rules and
  ;; the kit's soul row ride as before; on, the agent is told its policy is
  ;; its own to write and where, its rules ride behind the operator's and
  ;; labelled its own, its identity replaces the kit's row in every session,
  ;; and a plain edit of a file reaches the next turn of a conversation
  ;; already under way, committed. A conversation keeps the prompt it was
  ;; first given and is told each change since, so what must be ABSENT is
  ;; read off a conversation's first turn.
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
          (is (not (search "agent-rules" off)) "off: no policy of the agent's own")
          (is (not (probe-file (nle::policy-directory))) "off: no store is made"))
        (nle::config-set '("policy" "self") t)
        (is (search "\"self\": true" (uiop:read-file-string config)) "the switch is one line of the config")
        (let ((on (fresh "policy-on" "two")))
          (is (search "Operator-written voice." on) "on: the identity starts from the operator's")
          (is (not (search "Kit voice read from disk." on)) "on: it replaces the kit's row")
          (is (search "Operator rule: use just." on) "on: the operator's rules still ride")
          (is (search (format nil "Your own policy is yours to write, in ~a" (namestring (nle::policy-directory))) on) "on: the agent is told its policy is its own, and where")
          (is (not (search "Your rules:" on)) "no rules of its own until it writes some"))
        (let ((dir (nle::policy-directory)))
          (write-temp-file (merge-pathnames "AGENTS.md" dir) "Own rule: name the file you changed.")
          (write-temp-file (merge-pathnames "SOUL.md" dir) "Own voice.")
          (let ((later (turn "policy-on" "three"))
                (sha (policy-test-git dir "rev-parse" "adopted")))
            (is (search "Own rule: name the file you changed." later) "a plain edit binds the next turn of a conversation under way")
            (is (search "Own voice." later) "its identity too")
            (is (equal "policy: AGENTS.md, SOUL.md" (policy-test-git dir "log" "-1" "--format=%s")) "committed on the way")
            (let ((new (fresh "policy-new" "four")))
              (is (search (format nil "(now at ~a)" (subseq sha 0 7)) new) "the section names the commit it read")
              (is (search (format nil "Your rules:~%Own rule: name the file you changed.") new) "its rules ride, labelled its own")
              (is (search "The operator's rules outrank yours." new) "and below the operator's")
              (is (< (search "<harness key=\"operator-rules\">" new) (or (search "<harness key=\"agent-rules\">" new) 0)) "the operator's rules ride ahead")
              (is (search "Own voice." new) "its identity rides")
              (is (not (search "Operator-written voice." new)) "in place of the seed")
              (is (not (search "Kit voice read from disk." new)) "and of the kit's row")))
          (write-temp-file (merge-pathnames "SYSTEM.md" dir) "Own system prompt.")
          (nle::policy-sync)
          (let ((prompted (fresh "policy-prompt" "five")))
            (is (search "Own system prompt." prompted) "its own system prompt is the system message")
            (is (not (search nle::*system-prompt* prompted)) "in place of the shipped one"))
          (is (not (assoc "policy-prompt" (nlk:standing-notices) :test #'string=)) "written against this release, no notice")
          (nle::policy-adopt '(("SYSTEM.shipped" . "An older shipped prompt.")) "test: a release since")
          (fresh "policy-drift" "six")
          (is (assoc "policy-prompt" (nlk:standing-notices) :test #'string=) "a release that ships another prompt stands a notice"))
        (nle::config-set '("policy" "self") :false)
        (let ((off (fresh "policy-off" "seven")))
          (is (not (search "agent-rules" off)) "switched off, the agent's policy leaves")
          (is (not (search "Own system prompt." off)) "its prompt too")
          (is (search "Kit voice read from disk." off) "and the kit's soul row is back"))
        (is (not (assoc "policy-prompt" (nlk:standing-notices) :test #'string=)) "off, the prompt notice comes down")))))
