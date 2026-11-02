;;;; background-steer-test.lisp --- a background exit reaches the running turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A backgrounded evaluation that exits while its session's turn still runs
;;;; is read in that turn, at its next round boundary: after the tool batch it
;;;; finished during, or where a terminal answer would have ended the turn --
;;;; one (eval-status ID) call of the harness's own, never a prompt, once per
;;;; exit. A turn that completed before passes its exits to the running turn
;;;; the same way; a cancelled or failed spawner's are never read. The gated
;;;; form exits when the test opens the gate, so no wait here is a guess.

(in-package #:nodecode.test)

(defvar *steer-gate* nil
  "The semaphore a gated background form waits on. SETF, never LET: the form
runs on its own evaluation thread.")

(defun steer-gate ()
  "A gated background form's body: wait for the gate, then answer 4242."
  (bt2:wait-on-semaphore *steer-gate* :timeout 10)
  4242)

(defun steer-process (call-id &aux (registry (nle::exec-registry-instance)))
  "The registry entry the eval call CALL-ID backgrounded, or NIL."
  (find call-id (bt2:with-lock-held ((nle::exec-registry-lock registry))
                  (copy-list (nle::exec-registry-processes registry)))
        :key #'nle::exec-process-tool-call-id :test #'equal))

(defun open-steer-gate (call-id &aux (process (steer-process call-id)))
  "Let CALL-ID's gated form exit, and return once it has: the exit lands
wherever the caller stands in its turn. => its status, :EXITED."
  (bt2:signal-semaphore *steer-gate*)
  (nle::exec-park-until (nle::exec-registry-instance) (nle::exec-process-id process) 5000)
  (nle::exec-process-status process))

(defun gated-eval-call (call-id)
  "A model's eval call whose form waits on the gate past its 50 ms window, so
the eval tool hands it to the background."
  (wire-tool-call call-id "eval"
                  (nlk:encode-json-object
                   (nlk:make-json-object "form" "(nodecode.test::steer-gate)" "yield_time_ms" 50))))

(defun steer-background (registry session-id turn-id call-id &key (value "NIL") gated)
  "Background an evaluation TURN-ID of SESSION-ID ran as CALL-ID, answering
VALUE: exited before this returns, or, GATED, once the gate opens. => its id."
  (let ((id (spawn-test-eval registry :form "(steer-fixture)"
                                      :value (lambda () (when gated (steer-gate)) value)
                                      :session-id session-id :turn-id turn-id
                                      :tool-call-id call-id)))
    (nle::mark-exec-backgrounded registry id)
    (unless gated (nle::exec-park-until registry id 5000))
    id))

(defun steer-reads (messages value)
  "How many tool results of MESSAGES, one provider request, carry an exit
that answered VALUE."
  (count-if (lambda (message)
              (and (equal "tool" (gethash "role" message))
                   (let ((text (nle::content-text (gethash "content" message))))
                     (and (search ":EXITED" text)
                          (search (format nil ":VALUE ~s" value) text)))))
            (coerce messages 'list)))

(defun steer-log-reads (session-id value)
  "The turn id of each tool result in SESSION-ID's log that read an exit
answering VALUE, oldest first."
  (loop for event in (session-events session-id :type 'nlk:tool-result-event)
        for text = (nlk:tool-result-event-result event)
        when (and (stringp text) (search ":EXITED" text)
                  (search (format nil ":VALUE ~s" value) text))
          collect (nlk:event-turn-id event)))

(defun steer-turn-ids (session-id)
  "SESSION-ID's turns, oldest first, by the id their start fact carries."
  (mapcar #'nlk:event-turn-id (session-events session-id :type 'nlk:turn-started)))

(defun steer-completion (session-id)
  "SESSION-ID's newest completion fact, as its payload."
  (first (nlk:events :session-id session-id :kind nlk::+kind-turn-completed+
                     :order :newest :as :payloads)))

(defun steer-due-ids (registry session-id)
  "The ids of SESSION-ID's exits that still owe a read."
  (mapcar (lambda (entry) (getf entry :id)) (nle::unobserved-exited-evals registry session-id)))

(defmacro with-steered-turn ((session-id asked &rest rounds) &body body
                             &aux (context (gensym "CONTEXT")))
  "Run BODY in a store holding SESSION-ID, the steer gate shut and the provider
stubbed: ASKED gathers each request's messages, newest first, and request N
answers the Nth of ROUNDS, each a body of forms -- the last answers every
request past it."
  `(with-session-store (,session-id)
     (with-saved-globals ((*steer-gate* (bt2:make-semaphore :name "steer-gate")))
       (let ((,asked '()))
         (with-provider-stub (,context
                              (push (nle::compiled-turn-context-messages ,context) ,asked)
                              (case (length ,asked)
                                ,@(loop for (round . more) on rounds for n from 1
                                        collect `(,(if more n t) ,@round))))
           ,@body)))))

(deftest bg-exit-during-tools-is-read-before-the-next-provider-round (with-exec-registry (registry))
  ;; The operator's complaint: an evaluation the turn backgrounded exited
  ;; while the turn still had rounds to run, and its value waited for the
  ;; turn to end and a wake turn to read it. Here it exits during the next
  ;; round's tools, and the very next provider request carries its read --
  ;; a call and its result in this turn, read once.
  (with-steered-turn ("bg-steer-tools" asked
                      ((wire-message "assistant" :tool-calls (vector (gated-eval-call "c-bg"))))
                      ((wire-message "assistant" :tool-calls
                                     (vector (wire-tool-call "c-open" "eval"
                                                             (eval-args "(nodecode.test::open-steer-gate \"c-bg\")")))))
                      ((wire-message "assistant" :content "done")))
    (nle:run "go" :session (nle:intern-session "bg-steer-tools") :command-id "bg-steer-tools-command")
    (is (= 3 (length asked)) "the turn ran on past the exit")
    (is (zerop (steer-reads (second asked) "4242")) "no read before the exit")
    (is (= 1 (steer-reads (first asked) "4242")) "the round after the tools reads the exit, once")
    (let ((turns (steer-turn-ids "bg-steer-tools")))
      (is (= 1 (length turns)) "no wake turn")
      ;; The read is a result of the turn that ran.
      (is (equal (list (first turns)) (steer-log-reads "bg-steer-tools" "4242"))))
    (is (null (steer-due-ids registry "bg-steer-tools")) "the exit owes nothing more")
    (is (null (gethash "background-pending" (steer-completion "bg-steer-tools"))))))

(deftest bg-exit-during-a-terminal-answer-continues-the-turn (with-exec-registry (registry))
  ;; The model said its word while the work ran -- status, not the answer --
  ;; and the work exited before that word ended the turn: the turn reads the
  ;; exit instead of ending, and the model answers in the same turn.
  (with-steered-turn ("bg-steer-final" asked
                      ((wire-message "assistant" :tool-calls (vector (gated-eval-call "c-bg"))))
                      ((open-steer-gate "c-bg")
                       (wire-message "assistant" :content "waiting on it"))
                      ((wire-message "assistant" :content "the answer is 4242")))
    ;; The turn's answer is the one after the read.
    (is (equal "the answer is 4242"
               (nle:run "go" :session (nle:intern-session "bg-steer-final")
                             :command-id "bg-steer-final-command")))
    (is (= 3 (length asked)) "the terminal word did not end the turn")
    (is (= 1 (steer-reads (first asked) "4242")) "the next request reads the exit")
    (is (= 1 (length (steer-turn-ids "bg-steer-final"))) "no wake turn")
    (is (= 1 (length (steer-log-reads "bg-steer-final" "4242"))) "read once")
    (is (null (steer-due-ids registry "bg-steer-final")))
    ;; The work the word stood on was read: nothing is pending.
    (is (null (gethash "background-pending" (steer-completion "bg-steer-final"))))))

(deftest bg-exit-of-a-completed-turn-steers-the-running-turn (with-exec-registry (registry))
  ;; Work a completed turn handed off belongs to the session, and a turn
  ;; running when it exits is where it is read: one that exited before the
  ;; turn began is read ahead of its first provider request, one that exits
  ;; during it at its next boundary.
  (with-steered-turn ("bg-steer-prior" asked
                      ((open-steer-gate "c-late")
                       (wire-message "assistant" :tool-calls
                                     (vector (wire-tool-call "c-sum" "eval" (eval-args "(+ 1 2)")))))
                      ((wire-message "assistant" :content "ok")))
    (let ((earlier (nlk:admit-turn "bg-steer-prior" "bg-steer-prior-a" "earlier")))
      (steer-background registry "bg-steer-prior" (nlk:durable-turn-turn-id earlier) "c-early"
                        :value "1111")
      (steer-background registry "bg-steer-prior" (nlk:durable-turn-turn-id earlier) "c-late"
                        :value "2222" :gated t)
      (nlk:complete-turn earlier nil))
    (nle:run "next" :session (nle:intern-session "bg-steer-prior") :command-id "bg-steer-prior-b")
    (is (= 2 (length asked)))
    (is (= 1 (steer-reads (second asked) "1111")) "the earlier exit is read before the first request")
    (is (zerop (steer-reads (second asked) "2222")))
    (is (= 1 (steer-reads (first asked) "2222")) "the exit during the turn at its next boundary")
    (let ((turns (steer-turn-ids "bg-steer-prior")))
      (is (= 2 (length turns)) "the earlier turn and this one: no wake")
      (is (equal (list (second turns)) (steer-log-reads "bg-steer-prior" "1111")))
      (is (equal (list (second turns)) (steer-log-reads "bg-steer-prior" "2222"))))
    (is (null (steer-due-ids registry "bg-steer-prior")))))

(deftest bg-exit-of-a-cancelled-or-failed-turn-is-never-read (with-exec-registry (registry))
  ;; A turn the operator stopped, or one that failed, does not come back
  ;; through the work it left: the running turn reads neither exit, and both
  ;; stay unclaimed, exactly as the idle wake leaves them.
  (with-steered-turn ("bg-steer-dead" asked ((wire-message "assistant" :content "ok")))
    (let ((cancelled (nlk:admit-turn "bg-steer-dead" "bg-steer-dead-a" "stopped")))
      (steer-background registry "bg-steer-dead" (nlk:durable-turn-turn-id cancelled) "c-cut" :value "3333")
      (nlk:cancel-turn cancelled "stopped by the operator"))
    (let ((failed (nlk:admit-turn "bg-steer-dead" "bg-steer-dead-b" "failed")))
      (steer-background registry "bg-steer-dead" (nlk:durable-turn-turn-id failed) "c-fail" :value "4444")
      (nlk:fail-turn failed (make-condition 'simple-error :format-control "boom")))
    (nle:run "next" :session (nle:intern-session "bg-steer-dead") :command-id "bg-steer-dead-c")
    (is (= 1 (length asked)))
    (is (zerop (+ (steer-reads (first asked) "3333") (steer-reads (first asked) "4444"))))
    (is (= 2 (length (steer-due-ids registry "bg-steer-dead"))) "both exits left as they were")))

(deftest bg-exit-the-model-read-is-never-read-again (with-exec-registry (registry))
  ;; The model looked at the exit itself: the harness has nothing to add.
  (with-steered-turn ("bg-steer-manual" asked
                      ((wire-message "assistant" :tool-calls (vector (gated-eval-call "c-bg"))))
                      ((let ((id (nle::exec-process-id (steer-process "c-bg"))))
                         (open-steer-gate "c-bg")
                         (wire-message "assistant" :tool-calls
                                       (vector (wire-tool-call "c-look" "eval"
                                                               (eval-args (format nil "(eval-status ~d)" id)))))))
                      ((wire-message "assistant" :content "done")))
    (nle:run "go" :session (nle:intern-session "bg-steer-manual") :command-id "bg-steer-manual-command")
    (is (= 3 (length asked)))
    (is (= 1 (steer-reads (first asked) "4242")) "the model's own read, and no second")
    (is (= 1 (length (steer-log-reads "bg-steer-manual" "4242"))))
    (is (null (steer-due-ids registry "bg-steer-manual")))))

(deftest bg-exit-claimed-by-a-turn-cut-before-its-read-is-given-back (with-exec-registry (registry))
  ;; The turn claims the exit and is cancelled before the read runs: the
  ;; claim goes back, so the exit is still owed -- the idle wake carries it
  ;; once the session idles -- and nothing read it.
  (with-session-store ("bg-steer-cut")
    (let ((earlier (nlk:admit-turn "bg-steer-cut" "bg-steer-cut-a" "earlier"))
          (cut 0)
          (asked 0))
      (let ((id (steer-background registry "bg-steer-cut" (nlk:durable-turn-turn-id earlier) "c-early"
                                  :value "5555")))
        (nlk:complete-turn earlier nil)
        (with-stubbed-fdefinition (nle::exit-wake-message (ids)
                                   (incf cut)
                                   (nlk:request-cancel-turn "bg-steer-cut"
                                                            (nlk:durable-turn-turn-id nle::*current-durable-turn*)
                                                            "cut before the read")
                                   (funcall original ids))
          (with-provider-stub (nil (incf asked) (wire-message "assistant" :content "ok"))
            (signals-error nlk:turn-cancelled-condition
              (nle:run "next" :session (nle:intern-session "bg-steer-cut")
                              :command-id "bg-steer-cut-b"))))
        (is (= 1 cut) "the turn claimed the exit")
        (is (zerop asked) "and was cut before any provider round")
        (is (equal (list id) (steer-due-ids registry "bg-steer-cut")) "the claim went back")
        ;; Nothing read it.
        (is (null (nle::exec-process-exit-observed-by-model (nle::find-background-eval registry id))))))))

(deftest bg-exit-under-a-parked-steer-is-read-by-the-steers-turn (with-exec-registry (registry))
  ;; The operator's steer outranks the exit: a steer parked while the
  ;; terminal answer streamed ends the turn there, as it would with no exit,
  ;; and the steer's turn reads the exit ahead of its first provider request.
  (with-steered-turn ("bg-steer-parked" asked
                      ((wire-message "assistant" :tool-calls (vector (gated-eval-call "c-bg"))))
                      ((nlk:admit-active-turn-input
                        "bg-steer-parked" "bg-steer-parked-steer" "also do this"
                        :disposition :steer
                        :expected-turn-id (nlk:durable-turn-turn-id nle::*current-durable-turn*))
                       (open-steer-gate "c-bg")
                       (wire-message "assistant" :content "first answer"))
                      ((wire-message "assistant" :content "steered answer")))
    (is (equal "first answer"
               (nle:run "go" :session (nle:intern-session "bg-steer-parked")
                             :command-id "bg-steer-parked-command")))
    (is (= 2 (length asked)) "the steer ended the turn: no read round")
    (is (= 1 (length (steer-due-ids registry "bg-steer-parked"))) "the exit waits for the steer's turn")
    (let ((next (nlk::dequeue-next-turn "bg-steer-parked")))
      (is (equal "also do this" (second next)))
      (nle:run (second next) :session (nle:intern-session "bg-steer-parked")
                             :command-id "bg-steer-parked-steer" :admitted-turn (first next)))
    (is (= 3 (length asked)))
    (is (= 1 (steer-reads (first asked) "4242")) "the steer's turn reads the exit before its first request")
    (let ((turns (steer-turn-ids "bg-steer-parked")))
      (is (= 2 (length turns)))
      (is (equal (list (second turns)) (steer-log-reads "bg-steer-parked" "4242"))))
    (is (null (steer-due-ids registry "bg-steer-parked")))))

(deftest bg-exit-in-a-turn-that-spent-its-budget-waits-for-the-wake (with-exec-registry (registry))
  ;; A read is a call, and a turn past its budget has every call refused:
  ;; the exit is not claimed there -- a refusal the model did not cause would
  ;; cost it a round -- and stays owed for the idle wake.
  (with-steered-turn ("bg-steer-spent" asked
                      ((wire-message "assistant" :tool-calls (vector (gated-eval-call "c-bg"))))
                      ((open-steer-gate "c-bg")
                       (wire-message "assistant" :content "waiting on it"))
                      ((wire-message "assistant" :content "unreachable")))
    (with-turn-budget ('(:calls 1))
      (is (equal "waiting on it"
                 (nle:run "go" :session (nle:intern-session "bg-steer-spent")
                               :command-id "bg-steer-spent-command"))))
    (is (= 2 (length asked)) "the spent turn ends on its word")
    (is (zerop (length (steer-log-reads "bg-steer-spent" "4242"))) "nothing read it")
    (is (= 1 (length (steer-due-ids registry "bg-steer-spent"))) "the exit is still owed")))
