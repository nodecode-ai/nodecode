;;;; loop-test.lisp --- the observer, the decision, the fork, the recap, the fence.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the loop's contract on the seams the gateway
;;;; hands the cell: the :FRAME observer queues an operator turn's end
;;;; (newest per origin, never a recap's own completion) and folds a child's
;;;; answer and terminal fact; the decision follows the settings; the fork
;;;; is anchored at the reflected turn and carries the origin's sections,
;;;; model pin and floor, and submits one sourced request; the recap lands
;;;; in an idle origin at the same head as one recorded exchange, published
;;;; once, and is dropped when the origin moved on or is busy; the fence
;;;; holds a reflection to its verbs, its keeps and its edits; install and
;;;; stop, START-CELL, and /experience.

(in-package #:nodecode.test)

(nlk:access (reflection nodecode-experience::reflection))

(defun experience-input-payload (turn)
  "The payload of TURN's committed input in origin s-o."
  (nlk:events :session-id "s-o" :turn-id turn :kind nlk::+kind-input-committed+ :as :payload))

(defun experience-recorded-answer (turn)
  "The assistant answer TURN of origin s-o recorded."
  (nle:message-content
   (gethash "message" (nlk:events :session-id "s-o" :turn-id turn
                                  :kind nlk::+kind-assistant-completed+ :as :payload))))

(deftest experience-cell-the-observer-queues-operator-turns-and-folds-its-children ()
  (with-experience-runtime ()
    (is (eq :next (experience-publish "s-o" "turn.completed" "t1")))
    (is (equal '(("s-o" "t1" "turn.completed")) nodecode-experience::*pending*) "an operator turn is queued")
    (experience-publish "s-o" "turn.failed" "t2")
    (is (equal '(("s-o" "t2" "turn.failed")) nodecode-experience::*pending*))
    (experience-publish "s-p" "turn.cancelled" "t3")
    (is (= 2 (length nodecode-experience::*pending*)) "another origin queues beside it")
    (setf (gethash "t9" nodecode-experience::*recorded*) t)
    (experience-publish "s-o" "turn.completed" "t9")
    (is (equal '("s-o" "t2" "turn.failed")
               (find "s-o" nodecode-experience::*pending* :key #'first :test #'string=)))
    (experience-publish "s-o" "turn.assistant_message_completed" "t2")
    (is (= 2 (length nodecode-experience::*pending*)) "a non-terminal frame queues nothing")
    (let ((reflection (experience-reflection :turn "t2" :child "experience-s-o-t2")))
      (push reflection nodecode-experience::*reflections*)
      (experience-publish "experience-s-o-t2" "turn.assistant_message_completed" "ct"
                          (nlk:json-object "role" "assistant"
                                           "message" (nlk:json-object "role" "assistant"
                                                                      "content" "the recap")))
      (is-shape reflection (.answer "the recap" "the child's answer folds") (.child-turn "ct"))
      (experience-publish "experience-s-o-t2" "turn.completed" "ct")
      (is (equal "turn.completed" reflection.status))
      (is (= 2 (length nodecode-experience::*pending*)) "a child's completion is not an operator turn"))))

(deftest experience-cell-the-decision-follows-the-settings ()
  (let* ((sources '("gateway" "in_process"))
         (tools (list :reflect "tools" :sources sources))
         (every (list :reflect "every" :sources sources))
         (off (list :reflect "off" :sources sources)))
    (is-each (nodecode-experience::reflect-decision)
      (tools "turn.completed" "gateway" "started" 0 :quiet "a completed turn without a tool is quiet")
      (tools "turn.completed" "gateway" "started" 2 :reflect nil)
      (tools "turn.failed" "gateway" "started" 0 :reflect "a failed turn is a loss signal")
      (tools "turn.cancelled" "in_process" nil 0 :reflect "so is a stop")
      (tools "turn.completed" "cron" "started" 3 :source "a cron turn is not an operator's")
      (tools "turn.completed" "in_process" "recorded" 0 :by-hand t :recorded
       "a recap is never reflected on, not even by hand")
      (off "turn.failed" "gateway" "started" 5 :off nil)
      (off "turn.completed" "cron" "started" 0 :by-hand t :reflect
       "by hand overrides everything but recorded")
      (every "turn.completed" "gateway" "started" 0 :reflect nil)
      (tools "turn.completed" "gateway" "started" 2 :limited t :limited
       "a turn the provider refused for a limit: its reflection would be refused too")
      (tools "turn.failed" "gateway" "started" 0 :limited t :limited nil)
      (tools "turn.completed" "cron" "started" 2 :limited t :source "not the operator's first")
      (tools "turn.failed" "gateway" "started" 0 :limited t :by-hand t :reflect "by hand still reflects"))))

(deftest experience-cell-a-turn-refused-for-a-limit-is-not-reflected-on
    (with-experience-store ())
  ;; Without this, a turn whose own requests met a 429 would see its
  ;; reflection refused as surely.
  (flet ((turn (finish &aux (command (nlk::make-durable-id "command")))
           ;; One turn at a time: admitted, FINISH run on it, its id.
           (let ((turn (nlk:admit-turn "s-o" command "work"
                                       :invocation (test-invocation
                                                    :command-id command :correlation-id command
                                                    :source (nlk:make-invocation-source "gateway")))))
             (funcall finish turn)
             (nlk:durable-turn-turn-id turn))))
    (nlk:create-session :id "s-o" :cwd "/tmp")
    (let ((retried (turn (lambda (turn)
                           (nlk:record-provider-retry turn :attempt 1 :status 429 :detail "quota")
                           (nlk:complete-turn turn nil))))
          (overloaded (turn (lambda (turn)
                              (nlk:record-provider-retry turn :attempt 1 :status 529 :detail "overloaded")
                              (nlk:complete-turn turn nil))))
          (refused (turn (lambda (turn)
                           (nlk:fail-turn turn (make-condition 'simple-error :format-control "429")
                                          :evidence (list :status 429))))))
      (is (nodecode-experience::turn-limited-p "s-o" retried) "a 429 retry")
      (is (nodecode-experience::turn-limited-p "s-o" refused) "a 429 failure")
      (is (not (nodecode-experience::turn-limited-p "s-o" overloaded)) "a 529 is no limit of the key's")
      (with-submit-log (log)
        (is (eq :limited (nodecode-experience::reflect-turn "s-o" retried "turn.completed")))
        (is (eq :limited (nodecode-experience::reflect-turn "s-o" refused "turn.failed")))
        (is (null log) "nothing submitted")))))

(deftest experience-cell-a-reflection-that-failed-settles-without-a-word ()
  ;; Its line on *ERROR-OUTPUT* was a notice in the transcript of a shell
  ;; hosting the organism: the first a fresh box read after its first task.
  (with-experience-runtime ()
    (let ((*error-output* (make-string-output-stream)))
      (is (null (nodecode-experience::settle-reflection
                 (experience-reflection :turn "t" :child "experience-s-o-t" :status "turn.failed"))))
      ;; Completed, but answered nothing.
      (is (null (nodecode-experience::settle-reflection
                 (experience-reflection :turn "t" :child "experience-s-o-t-2" :status "turn.completed"))))
      (is (equal "" (get-output-stream-string *error-output*)) "nothing on the notice lane"))))

(deftest experience-cell-a-wake-is-judged-by-whoever-began-the-session (with-experience-store ())
  ;; An exit wake carries the kernel's default provenance, in_process, and
  ;; `in_process' is a reflected source: a team node's wakes forked eight
  ;; reflections on the first team run. A wake continues its session's work,
  ;; so it is judged by the source that began it.
  (flet ((turn (session source &aux (command (nlk::make-durable-id "command")))
           (nlk:admit-turn session command "work"
                           :invocation (and source
                                            (test-invocation
                                             :command-id command :correlation-id command
                                             :source (nlk:make-invocation-source source))))))
    (dolist (id '("s-op" "team-s-op-node-0" "s-bare"))
      (nlk:create-session :id id :cwd "/tmp"))
    (nlk:complete-turn (turn "s-op" "gateway") nil)
    (nlk:complete-turn (turn "team-s-op-node-0" "team") nil)
    (nlk:complete-turn (turn "s-bare" nil) nil)
    (is (equal "gateway" (nodecode-experience::attributed-source "s-op" "in_process")) "the operator's session: its wake is the operator's")
    (is (equal "team" (nodecode-experience::attributed-source "team-s-op-node-0" "in_process")) "a node's wake is the team's")
    (is (equal "gateway" (nodecode-experience::attributed-source "team-s-op-node-0" "gateway")) "an input with a source of its own keeps it")
    (is (equal "in_process" (nodecode-experience::attributed-source "s-bare" "in_process")) "a session begun in-process stays in-process")
    ;; the gate itself: the node's wake fails, and is no operator's turn
    (let ((wake (turn "team-s-op-node-0" nil)))
      (nlk:fail-turn wake (make-condition 'simple-error :format-control "the wake's round failed"))
      (is (eq :source (nodecode-experience::reflect-turn
                       "team-s-op-node-0" (nlk:durable-turn-turn-id wake) "turn.failed")) "never reflected on"))))

(deftest experience-cell-the-fork-carries-the-origins-standing-context (with-temp-store ())
  (with-temp-workspace (root ".git/HEAD")
    (with-experience-runtime ()
      (nlk:create-session :id "s-o" :cwd (namestring root))
      (let ((turn (nlk:record-exchange-turn "s-o" "hello" "hi")))
        (nlk:set-harness-section "s-o" "index" "kn-quiet — the operator wants quiet output")
        (nlk:set-harness-section "s-o" "soul" "be kind")
        (nlk:record-session-model-selection "s-o" :provider "p" :model "m")
        (nlk:advance-history-floor "s-o" 2)
        (with-submit-log (log)
          (let ((child (nodecode-experience::fork-reflection "s-o" turn)))
            (is (equal (format nil "experience-s-o-~a" turn) child) "one child per turn")
            (is-present (session (nlk:find-session child)) "the child session"
              (is (equal "s-o" (nlk:durable-session-parent session)))
              (is (equal turn (nlk:durable-session-anchor-turn-id session)) "anchored at the reflected turn")
              (is (equal (namestring root) (nlk:durable-session-cwd session)) "the origin's directory"))
            (is (null (nlk:list-harness-sections child)))
            (is (equal '(("index" . "kn-quiet — the operator wants quiet output") ("soul" . "be kind"))
                       (nlk:standing-harness-sections child)))
            (is (equal '("p" "m") (multiple-value-list (nlk:session-model-selection child))))
            (is (= 2 (nlk:retained-history-floor child)) "the floor copied")
            (is (equal (cons "s-o" turn) (gethash child nodecode-experience::*children*)))
            (is (= 1 (length nodecode-experience::*reflections*)))
            (is-present (entry (first log)) "one submit"
              (is-shape entry (first child) (third (format nil "experience:~a:reflect" child)))
              (is (equal "experience" (fourth entry)) "sourced experience")
              (is (search "[experience] The turn above just ended" (second entry)))
              (is (search (format nil ":project ~s" (string-right-trim "/" (namestring root))) (second entry)))
              (is (search "one line - at most 1 line" (second entry)))
              ;; credit only for a shown moment; a keep carries its when, evidence and confidence
              (is-carrying (request (second entry))
                ("confounded (it was in play") ("no-effect (you read or followed it")
                ("A turn that went well is not a moment") ("when (a skill's when-to-use line)")
                ("evidence - the moment in the turn above") ("confidence - high or medium")
                ("narrow it to what the turn does show, or keep nothing")
                ("never as a general rule"))))
          (is (equal (format nil "experience-s-o-~a-2" turn) (nodecode-experience::fork-reflection "s-o" turn)))
          (is (= 2 (length log)))
          ;; A configured reflection model replaces the origin's pin.
          (setf (getf nodecode-experience::*experience* :provider) "x"
                (getf nodecode-experience::*experience* :model) "y")
          (let ((third (nodecode-experience::fork-reflection "s-o" turn)))
            (is (equal '("x" "y") (multiple-value-list (nlk:session-model-selection third))))))
        (multiple-value-bind (source disposition tools) (nodecode-experience::turn-facts "s-o" turn)
          (is (equal "in_process" source))
          (is (equal "recorded" disposition) "the exchange this test recorded")
          (is (= 0 tools)))
        (with-submit-log (log)
          (is (eq :recorded (nodecode-experience::reflect-turn "s-o" turn "turn.completed" :by-hand t)))
          (is (null log) "nothing submitted"))))))

(deftest experience-cell-the-recap-lands-in-an-idle-origin-unless-the-operator-moved-on ()
  (with-experience-store ("recap_lines" 3)
    (nlk:create-session :id "s-o" :cwd "/tmp")
    (let* ((turn (nlk:record-exchange-turn "s-o" "hello there, organism" "hi"))
           (published '())
           (settings nodecode-experience::*experience*)
           (reflection (experience-reflection
                        :turn turn :child "experience-s-o-x" :status "turn.completed"
                        :answer (format nil "asked: a thing~%~%done: the thing~%open: nothing~%carry: this line is cut"))))
      (with-stubbed-fdefinition (nle::publish-turn-facts (session cursor)
                                  (push (list session cursor) published)
                                  t)
        (is (eq t (nodecode-experience::settle-reflection reflection)) "settled with a recap")
        (let ((head (nlk:session-head-turn-id "s-o")))
          (is (not (equal turn head)) "the origin has a new head")
          (is (gethash head nodecode-experience::*recorded*) "remembered as ours")
          (is (equal "recorded" (nth-value 1 (nodecode-experience::turn-facts "s-o" head))))
          (is (equal nodecode-experience::*recap-input*
                     (nlk:json-value (experience-input-payload head) :string "message")))
          (is (null (nth-value 1 (nlk:json-value (experience-input-payload head) :boolean "announce"))))
          (is (equal (format nil "asked: a thing~%done: the thing~%open: nothing")
                     (experience-recorded-answer head))))
        (is (= 1 (length published)) "published once to the attached shells")
        (is (equal "s-o" (first (first published))))
        ;; Turns nobody asked for came between - the recap just recorded
        ;; is one - so a recap of the same turn still lands, and its user
        ;; half names the turn it belongs to.
        (let ((since (nodecode-experience::turns-since "s-o" turn)))
          (is (= 1 (length since)))
          (is (equal "recorded" (third (first since))) "the recap turn is not the operator's"))
        (is-present (again (nodecode-experience::record-recap
                            (experience-reflection
                             :turn turn :child "experience-s-o-x-2" :command-id "c2"
                             :answer (nodecode-experience::reflection-answer reflection)
                             :status "turn.completed")
                            settings))
          "a second reflection of the same turn lands behind a non-operator turn"
          (is (equal (format nil "[experience] recap of turn 1, \"hello there, organism\", by a reflection ~
                                  of this session; 1 turn since, none the operator's")
                     (nlk:json-value (experience-input-payload again) :string "message"))))
        (is (= 2 (length published)))
        ;; The operator moved on: dropped, without a word.
        (with-stubbed-fdefinition (nodecode-experience::turns-since (origin turn)
                                    (list (list "t-op" "gateway" "started")))
          (is (null (nodecode-experience::record-recap reflection settings)) "an operator turn since: dropped")
          (is (null (nodecode-experience::settle-reflection reflection)) "settled silently"))
        (is (= 2 (length published)))
        (let ((again (experience-reflection
                      :turn (nlk:session-head-turn-id "s-o") :child "experience-s-o-y"
                      :answer "recap" :status "turn.completed")))
          (with-stubbed-fdefinition (nlk:active-turn-p (session) t)
            (is (null (nodecode-experience::record-recap again settings)) "busy: dropped"))
          (setf (getf settings :recap) nil)
          (is (null (nodecode-experience::settle-reflection again)) "recap off: nothing written")
          (is (= 2 (length published)))))
      ;; The facts the recap's completion carries: the child's turn as the
      ;; meter sums it, and what the fold reads back.
      (is (equal '(:provider "a6api" :model "glm-5.3-flash"
                   :input-tokens 3562 :cached-input-tokens 6528 :output-tokens 205 :reasoning-tokens 58)
                 (nodecode-experience::usage-facts
                  (list (nlk:json-object "input-tokens" 2831 "output-tokens" 37 "cached-input-tokens" 2176
                                         "reasoning-tokens" 20 "provider" "a6api" "model" "glm-5.3-flash")
                        (nlk:json-object "input-tokens" 9999 "output-tokens" 9 "voided" t)
                        (nlk:json-object "input-tokens" 731 "output-tokens" 168 "cached-input-tokens" 4352
                                         "reasoning-tokens" 38 "provider" "a6api" "model" "glm-5.3-flash")))))
      (is (null (nodecode-experience::usage-facts '())) "no rows, no counts")
      (nlk:create-session :id "experience-s-o-z" :cwd "/tmp" :parent "s-o")
      (let ((child-turn (nlk:record-exchange-turn "experience-s-o-z" "reflect" "a recap")))
        (is-present (facts (nodecode-experience::reflection-facts
                            (experience-reflection
                             :turn turn :child "experience-s-o-z" :child-turn child-turn
                             :answer "a recap" :status "turn.completed")))
          "a child with a turn and no usage"
          (is (equal "reflection" (getf facts :recorder)))
          (is (integerp (getf facts :elapsed-ms)) "its clock from start to end")
          (is (null (getf facts :provider)) "no usage row, no provider"))
        (with-stubbed-fdefinition (nodecode-experience::reflection-facts (reflection)
                                    '(:recorder "reflection" :provider "a6api" :model "glm-5.3-flash"
                                      :elapsed-ms 18400 :input-tokens 3562 :cached-input-tokens 6528
                                      :output-tokens 205 :reasoning-tokens 58))
          (is-present (landed (nodecode-experience::record-recap
                               (experience-reflection
                                :turn turn :child "experience-s-o-z" :child-turn child-turn
                                :answer "a recap" :status "turn.completed")
                               settings))
            "the recap lands with the facts"
            (let ((event (nlk:events :session-id "s-o" :turn-id landed
                                     :kind nlk::+kind-turn-completed+ :as :instance)))
              (is (equal "reflection" (nlk:turn-completed-recorder event)))
              (is (= 18400 (nlk:turn-completed-elapsed-ms event)))
              (is (= 6528 (nlk:turn-completed-cached-input-tokens event)))))))
      (is (nodecode-experience::operator-turn-p "gateway" "started" settings))
      (is (nodecode-experience::operator-turn-p "in_process" nil settings) "a plain in-process request is the operator's")
      (is (null (nodecode-experience::operator-turn-p "cron" "started" settings)) "a cron delivery is not")
      (is (null (nodecode-experience::operator-turn-p "in_process" "recorded" settings)) "nor a recorded exchange"))))

(deftest experience-cell-attention-marks-the-recap-and-the-recorded-fact-carries-it ()
  ;; The recap is silent by default; the reflection marks the one that must
  ;; reach the operator, and the recorded exchange carries the mark so the
  ;; room can post it as a note.
  (with-experience-store ()
    (nlk:create-session :id "s-o" :cwd "/tmp")
    (let* ((turn (nlk:record-exchange-turn "s-o" "hello" "hi"))
           (settings nodecode-experience::*experience*)
           (published '())
           (reflection (experience-reflection
                        :turn turn :child "experience-s-o-att"
                        :answer "the disk filled up; open: nothing")))
      (push reflection nodecode-experience::*reflections*)
      (nlk:with-cleanup ((setf nodecode-experience::*reflections*
                               (remove reflection nodecode-experience::*reflections*))
                         (remhash "experience-s-o-att" nodecode-experience::*announced*))
        (signals-error experience:experience-error
          (let ((nlk:*scribe-session-id* "not-a-reflection"))
            (experience:attention)))
        (let ((nlk:*scribe-session-id* "experience-s-o-att"))
          (is (search "the recap will reach the operator"
                      (experience:attention)))
          (is (eq t (gethash "experience-s-o-att"
                             nodecode-experience::*announced*))))
        (with-stubbed-fdefinition (nle::publish-turn-facts (session cursor)
                                    (push (list session cursor) published)
                                    t)
          (is-present (loud (nodecode-experience::record-recap reflection settings))
            "the marked recap lands"
            (is (eq t (nlk:json-value (experience-input-payload loud) :boolean "announce")))))))))

(define-cell-lifecycle-tests "experience"
  (:hooks :frame :tool 'nle:turn-budget 'nle:name-session)
  (:help :experience)
  (:command "experience")
  (:running (is nodecode-experience::*worker* "the reflector runs"))
  (:stopped (is (null nodecode-experience::*worker*) "and stopped"))
  (:refused ("reflect" "maybe")))

(deftest experience-cell-the-manual-the-settings-and-the-slash-surface (with-temp-store ())
  (with-experience-runtime ()
    (is (search "(experience:sight" (nle:help :experience)) "(help :experience) is the manual")
    ;; and every request's line says whose a recap is
    (is (search "never the operator's words" (cell-section "s" "help")))
    (nlk:create-session :id "s-op" :cwd "/tmp")
    (let ((summary (cell-entry "nodecode-experience" "experience" "" "s-op")))
      (is (uiop:string-prefix-p "experience: 0 sightings (helped 0 · harm 0 · confounded 0 · no-effect 0 · call 0 · checks 0) · reflect tools · " summary))
      (is (uiop:string-suffix-p summary "usage.jsonl") "the ledger last"))
    (is (search "unknown subcommand nope" (cell-entry "nodecode-experience" "experience" "nope" "s-op")))
    (is (search "has no completed turn yet" (cell-entry "nodecode-experience" "experience" "reflect" "s-op"))))
  (with-knowledge-fixture ()
    (with-cell-stop ((experience-start "reflect" "every" "recap_lines" 2 "sources" (vector "gateway")))
      (is-shape nodecode-experience::*experience* (:reflect "every") (:recap-lines = 2)
        (:sources '("gateway"))))))

(deftest experience-cell-the-reflection-names-what-the-turn-kept (with-experience-store ())
  (nlk:create-session :id "s-k" :cwd "/tmp")
  (experience-seed-definition "s-k" "EARLIER" "(defun earlier () 1)")
  (let* ((turn (nlk:admit-turn "s-k" "cmd-k" "make a file writer")) (turn-id turn.turn-id))
    (experience-seed-definition "s-k" "WRITE-TEXT-FILE" "(defun write-text-file (path text) path)")
    (experience-seed-definition "s-k" "WRITE-TEXT-FILE" "(defun write-text-file (path text) (list path text))")
    (nlk:cancel-turn turn "done")
    (experience-seed-definition "s-k" "LATER" "(defun later () 2)")
    (is (equal '("write-text-file") (nodecode-experience::turn-definitions "s-k" turn-id)))
    (with-submit-log (log)
      (nodecode-experience::fork-reflection "s-k" turn-id)
      (is-present (entry (first log)) "the reflection's request"
        (is (search "one you kept this turn: write-text-file, or one an earlier session kept"
                    (second entry)))))))

(deftest experience-cell-a-recap-is-one-line-by-default (with-experience-store ())
  (nlk:create-session :id "s-o" :cwd "/tmp")
  (let* ((turn (nlk:record-exchange-turn "s-o" "hello there" "hi"))
         (reflection (experience-reflection
                      :turn turn :child "experience-s-o-x" :status "turn.completed"
                      :answer (format nil "asked: a thing~%done: the thing~%open: nothing"))))
    (with-stubbed-fdefinition (nle::publish-turn-facts (session cursor)
                                t)
      (is (eq t (nodecode-experience::settle-reflection reflection)) "settled with a one-line recap"))
    (is (equal "asked: a thing" (experience-recorded-answer (nlk:session-head-turn-id "s-o"))))))



(deftest experience-cell-a-reflection-runs-its-verbs-and-keeps-definitions-unbudgeted ()
  ;; A session this cell made calls its verbs and keeps definitions, and
  ;; runs nothing else; no budget stops it by default, and it sees how long
  ;; it has run.
  (with-experience-runtime ()
    (flet ((snippet (form)
             (nodecode-experience::fence-refusal "eval" (nlk:json-object "form" form))))
      (is (null (snippet "(experience:sight \"helped\" \"x\" :quote ($ \"q\"))")))
      (is (null (snippet "(list (experience:sight \"helped\" \"a\" :quote \"b\") (help 'a) (index \"text\"))")))
      (is (null (snippet "(experience:sight \"call\" 'unquoted-name :quote \"q\")")) "a quoted datum")
      (is (null (snippet "(defun sweep-once (guild) (discord-sweep guild))")))
      (is (null (snippet "(define-memory quiet-output \"The operator wants quiet output.\" :type :feedback
  ;; said twice
  )")) "a memory is kept, not run")
      (is (null (snippet "(define-skill red-baseline \"Split red tests from the baseline.\")")))
      (is (null (snippet "(edit \"/x/quiet-output.lisp\" ($ \"old\") ($ \"new\"))")) "a file it read, edited")
      (is (null (snippet "(unintern 'quiet-output)")) "or forgotten")
      (is (search "SH is neither" (snippet "(edit (sh \"ls\") \"a\" \"b\")")) "over values alone")
      (is (null (snippet "(experience:attention")) "a paren the eval snippet closes reads the same")
      (is (null (snippet ")")) "a snippet that does not read passes: the eval tool refuses it and runs nothing")
      (is-carrying (refusal (snippet "(discord-suppress-embeds-sweep \"1541872608385966080\")"))
        ("refused by nodecode-experience" "running a definition is refused")
        ("DISCORD-SUPPRESS-EMBEDS-SWEEP is neither" "and the refusal names it"))
      (is (search "SH is neither" (snippet "(progn (experience:attention) (sh \"curl -X PATCH x\"))")))
      (is (search "WRITE-FILE is neither" (snippet "(write-file \"~/.nodecode/x.lisp\" ($ \"body\"))")))
      (is (search "DEFPARAMETER is neither" (snippet "(defparameter *lab* (sh \"ls\"))")))
      (is (search "SH is neither" (snippet "(experience:sight \"call\" \"x\" :note (sh \"ls\"))"))))
    (is (search "works through eval alone"
                (nodecode-experience::fence-refusal "websearch" (nlk:json-object "query" "x"))))
    (let ((hook (cell-hook "nodecode-experience" :tool))
          (ran nil))
      (flet ((call (session form)
               (setf ran nil)
               (let ((nle::*live-turn* (nle::%make-live-turn :session-id session :turn-id "t")))
                 (funcall hook (list :name "eval" :arguments (nlk:json-object "form" form)
                                     :call-id "c")
                          (lambda (op) (declare (ignore op)) (setf ran t) "ran")))))
        (is (equal "ran" (call "s-op" "(sh \"ls\")")) "an operator's call runs")
        (is (search "refused" (call "experience-s-op-t1" "(sh \"ls\")")) "a reflection's shell call does not")
        (is (not ran) "and never reaches the tool")
        (is (equal "ran" (call "experience-s-op-t1" "(experience:attention)")) "its verbs do")))
    (is (null (nle::live-turn-budget (nle::open-live-turn "experience-s-op-t1" "t"))))
    (let ((nodecode-experience::*budget* '(:calls 40)))
      (is (equal '(:calls 40) (nle::live-turn-budget (nle::open-live-turn "experience-s-op-t1" "t")))))
    (is (null (nle::live-turn-budget (nle::open-live-turn "s-op" "t"))))
    (let ((nle::*turn-budget* '(:seconds 1800)))
      (is (null (nle::live-turn-budget (nle::open-live-turn "experience-s-op-t1" "t")))))
    (flet ((next (session-id text) (declare (ignore session-id)) text))
      ;; a reflection is never named
      (is (null (nodecode-experience::naming-hook #'next "experience-s-op-t1" "[reflect] the turn")))
      (is (equal "fix it" (nodecode-experience::naming-hook #'next "s-op" "fix it")))))
  (is (null (nle::live-turn-budget (nle::open-live-turn "experience-s-op-t1" "t")))))
