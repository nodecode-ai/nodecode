;;;; lifecycle-test.lisp --- durable turn lifecycle invariants.

(in-package #:nodecode.test)

(nlk:access (cancelled nlk::durable-turn) (completed nlk::durable-turn) (input nlk::event)
            (promoted nlk::durable-turn) (queued nlk::active-input-admission) (started nlk::event))

(defparameter +admitted-kinds+ '("session.created" "turn.input_committed" "turn.started")
  "The facts a fresh session's first admitted turn has written, in log order.")

(deftest durable-admission-records-input-and-start-atomically ()
  (with-session-store ("lifecycle-admit")
    (let* ((turn (nlk:admit-turn "lifecycle-admit" "command-1" "hello"
                                 :definition-revision "agent-loop@test"))
           (events (nlk:page-events :session-id "lifecycle-admit"))
           (turn-events (remove-if-not (lambda (event) (equal turn.turn-id event.turn-id)) events)))
      (is (equal +admitted-kinds+ (mapcar #'nlk:event-kind events)))
      (is (equal "lifecycle-admit" turn.session-id) "the turn names its session")
      (is (equal '(1 2 3) (mapcar #'nlk:event-aggregate-sequence events)))
      (is (every (lambda (event) (equal "agent-loop@test" event.definition-revision)) turn-events)))))

(deftest duplicate-command-is-not-admitted-twice (with-session-store ("lifecycle-dedup"))
  (nlk:admit-turn "lifecycle-dedup" "same-command" "first")
  (is (signals-error nlk:duplicate-turn-command
        (nlk:admit-turn "lifecycle-dedup" "same-command" "second")))
  (is (= 3 (nlk:event-count :session-id "lifecycle-dedup"))))

(deftest one-turn-accepts-exactly-one-terminal-event ()
  (with-admitted-turn (turn "lifecycle-terminal" "command-1" "hello")
    (nlk:complete-turn turn (wire-message "assistant" :content "hi"))
    (is (signals-error nlk:turn-already-terminal
          (nlk:fail-turn turn (make-condition 'simple-error
                                              :format-control "late failure"))))
    (is (equal (append +admitted-kinds+ '("turn.assistant_message_completed" "turn.completed"))
               (event-kinds-for "lifecycle-terminal")))))

(deftest a-failed-turns-detail-is-the-message-without-the-printers-references ()
  ;; SBCL's printer appends a `See also:' block of manual references to some
  ;; conditions; a card showing every line of a failure's detail would print
  ;; it as if it were the error. The detail is the message, every line of it.
  (with-admitted-turn (turn "lifecycle-references" "command-1" "hello")
    (nlk:fail-turn turn (handler-case (subseq (string (gensym)) 0 99) (error (e) e)))
    (let ((detail (nlk::turn-failed-detail
                   (first (nlk:events :session-id "lifecycle-references" :kind nlk::+kind-turn-failed+)))))
      (is-carrying detail
        ("The bounding indices 0 and 99 are bad" "the message stands")
        (:absent "See also" "the printer's references are not the error")))))

(deftest a-settled-turn-releases-its-cancellation-registration ()
  ;; D2 reclamation: the process-local cancellation registry must not grow by
  ;; one entry per turn for the life of the image. The terminal fact is the
  ;; release point; a still-active turn keeps its registration.
  (with-admitted-turn (turn "lifecycle-reclaim" "command-1" "hello"
                       (live-key (nlk::cancellation-key "lifecycle-reclaim" turn.turn-id)))
    (is (nth-value 1 (gethash live-key nlk::*turn-cancellation-signals*)))
    (nlk:complete-turn turn (wire-message "assistant" :content "hi"))
    (is (not (nth-value 1 (gethash live-key nlk::*turn-cancellation-signals*))))))

(deftest cancel-signal-carries-retract-intent ()
  ;; The take-it-back intent rides the kernel cancellation signal the
  ;; worker already holds: a retracting cancel sets it, and a later plain
  ;; cancel of the same turn does not withdraw it (set-only).
  (with-admitted-turn (turn "lifecycle-retract" "command-1" "hello" (turn-id turn.turn-id))
    (is (not (nlk:turn-retract-requested-p turn)))
    (nlk:request-cancel-turn "lifecycle-retract" turn-id "esc")
    (is-shape turn (nlk:turn-cancellation-requested-p is) (nlk:turn-retract-requested-p not))
    (nlk:request-cancel-turn "lifecycle-retract" turn-id "esc" t)
    (is (nlk:turn-retract-requested-p turn))
    (nlk:request-cancel-turn "lifecycle-retract" turn-id "again")
    (is (nlk:turn-retract-requested-p turn))
    (nlk:cancel-turn turn "esc")))

(deftest turn-terminal-kind-reads-the-durable-terminal ()
  ;; The durable answer to how a turn ended — NIL while open, then exactly
  ;; the terminal fact's kind.
  (with-admitted-turn (cancelled "lifecycle-terminal-kind" "c-1" "one"
                       (cancelled-id cancelled.turn-id))
    (is (null (nlk:turn-terminal-kind cancelled-id)))
    (nlk:cancel-turn cancelled "esc")
    (is (equal "turn.cancelled" (nlk:turn-terminal-kind cancelled-id)))
    (let ((completed (nlk:admit-turn "lifecycle-terminal-kind" "c-2" "two")))
      (nlk:complete-turn completed (wire-message "assistant" :content "hi"))
      (is (equal "turn.completed" (nlk:turn-terminal-kind completed.turn-id))))))

(deftest run-does-not-call-provider-when-durable-admission-fails ()
  (let ((session (nle::make-session :id "lifecycle-no-store"))
        (provider-called nil))
    (ignore-errors (nlk:close-store))
    (with-provider-stub (nil (setf provider-called t)
                             (wire-message "assistant" :content "should not run"))
      (is (signals-error nlk::store-not-open
            (nle:run "hello" :session session :command-id "command-1")))
      (is (not provider-called) "provider policy did not execute"))))

(deftest run-records-failure-without-recording-completion (with-session-store ("lifecycle-failure"))
  (let ((session (nle:intern-session "lifecycle-failure")))
    (with-provider-stub (nil (error "synthetic provider failure"))
      (ignore-errors (nle:run "hello" :session session :command-id "command-1")))
    ;; The round's own facts (the model it took, the prompt it sent) may sit
    ;; between admission and the end; the end is a failure and nothing completes.
    (let ((kinds (event-kinds-for "lifecycle-failure")))
      (is (equal +admitted-kinds+ (subseq kinds 0 (length +admitted-kinds+))))
      (is (equal "turn.failed" (car (last kinds))))
      (is (not (member "turn.completed" kinds :test #'equal))))
    (is (not session.in-flight-p))))

(deftest reopening-store-classifies-an-interrupted-turn ()
  (with-restartable-store (path "lifecycle-recovery")
    (nlk:admit-turn "lifecycle-recovery" "command-1" "hello")
    (is (nlk:active-turn-p "lifecycle-recovery"))
    (reopen-store path)
    (is (nlk:active-turn-p "lifecycle-recovery") "it stays the active turn")
    (is (equal (append +admitted-kinds+ '("turn.suspended"))
               (event-kinds-for "lifecycle-recovery")))
    (reopen-store path)
    (is (= 1 (count-session-events "lifecycle-recovery" "turn.suspended")))))

(deftest admission-materializes-started-and-links-causation ()
  (with-admitted-turn (turn "lifecycle-cause" "command-cause" "hello"
                       (events (session-events "lifecycle-cause" :created nil))
                       (input (first events))
                       (started (second events)))
    (is (typep started 'nlk:turn-started) "turn.started is a registered class")
    (is (equal "hello" (nlk:turn-started-input started)) "start carries input")
    (is-shape input (.event-id started.causation-id) (.correlation-id "command-cause"))
    (is-shape started (.correlation-id "command-cause") (.event-id turn.started-event-id))))

(deftest tool-journal-requires-an-open-turn ()
  (with-admitted-turn (turn "lifecycle-tools" "command-tools" "eval")
    (record-tool-use turn "call-1" "eval" "(+ 1 2)" "3")
    (nlk:complete-turn turn "done")
    (is-table (record)
      (signals-error nlk:turn-already-terminal (record turn "call-2" "eval" "1"))
      (nlk:record-tool-call "a completed turn cannot grow a tool call")
      (nlk:record-tool-result "a completed turn cannot grow a tool result"))
    (is (= 1 (count-session-events "lifecycle-tools" "turn.tool_call_started")))))

(deftest runtime-policy-redefinition-applies-to-the-next-turn ()
  (with-session-store ("lifecycle-mutation")
    (with-stubbed-fdefinition (nodecode.evolved:agent-loop (session &key &allow-other-keys)
                               (wire-message "assistant" :content "mutated policy"))
      (is (equal "mutated policy"
                 (nle:run "hello"
                          :session (nle:intern-session "lifecycle-mutation")
                          :command-id "command-1"))))))

(deftest canonical-command-envelope-round-trips-admission-metadata ()
  (with-session-store ("lifecycle-command")
    (let* ((command (nlk:make-command
                     :id "command-envelope"
                     :kind "turn.start"
                     :session-id "lifecycle-command"
                     :input "hello"
                     :invocation (test-invocation :command-id "command-envelope"
                                                  :correlation-id "correlation-envelope")
                     :definition-revision "agent-loop@test"))
           (turn (nlk:admit-command command))
           (events (nlk:page-events :session-id "lifecycle-command")))
      (is (equal "command-envelope" turn.command-id))
      (is (every (lambda (event) (equal "agent-loop@test" event.definition-revision)) (cdr events)))
      (is (= 3 (nlk:event-count :session-id "lifecycle-command"))))))

(deftest cancellation-is-the-only-terminal-fact ()
  (with-admitted-turn (turn "lifecycle-cancel" "cancel-command" "hello")
    (nlk:cancel-turn turn "operator requested")
    (is (equal (append +admitted-kinds+ '("turn.cancelled"))
               (event-kinds-for "lifecycle-cancel")))
    (is (signals-error nlk:turn-already-terminal
          (nle::complete-turn turn "late completion")))))

(defun %cancel-parked-wait (session-id command-id park)
  "Admit a turn on SESSION-ID under COMMAND-ID, park a worker inside its
WITH-CANCELLABLE-WAIT running PARK — a function of the semaphore it signals
once parked — and cancel the turn once it is."
  ;; Answers the worker's outcome, :CANCELLED or :SLEPT-THROUGH, and the
  ;; seconds from the worker's start to its join.
  (let* ((turn (nlk:admit-turn session-id command-id "hello"))
         (entered (sb-thread:make-semaphore :name "wait-entered"))
         (outcome nil)
         (worker (nlk:spawn "wait-cancel-worker"
                   (setf outcome
                         (nlk:with-handlers ((nlk:turn-cancelled-condition () :cancelled))
                           (nlk:with-cancellable-wait (turn)
                             (funcall park entered)
                             :slept-through)))))
         (start (get-internal-real-time)))
    (sb-thread:wait-on-semaphore entered)
    (nlk:request-cancel-turn session-id turn.turn-id "esc")
    (bt2:join-thread worker)
    (values outcome (/ (- (get-internal-real-time) start)
                       internal-time-units-per-second))))

(deftest cancel-interrupts-a-parked-cancellable-wait (with-session-store ("lifecycle-wait-cancel"))
  ;; Esc must not wait for the next byte off the provider socket: a thread
  ;; parked inside WITH-CANCELLABLE-WAIT is interrupted by the cancel request
  ;; and unwinds as the ordinary cancel condition, immediately.
  (is-values (outcome elapsed)
      (%cancel-parked-wait "lifecycle-wait-cancel" "wait-command"
                           (lambda (entered)
                             (sb-thread:signal-semaphore entered)
                             (sleep 30)))
    (outcome eq :cancelled) ((< elapsed 5) is)))

(deftest a-prior-cancel-signals-at-wait-entry ()
  ;; A cancel that lands before the wait opens must refuse to block at all.
  (with-admitted-turn (turn "lifecycle-wait-precancel" "pre-command" "hello")
    (nlk:request-cancel-turn "lifecycle-wait-precancel"
                             turn.turn-id "esc")
    (is (signals-error nlk:turn-cancelled-condition
          (nlk:with-cancellable-wait (turn) (sleep 30))))))

(deftest cancel-unwinds-past-foreign-error-handlers ()
  ;; The dexador WITH-RETRYING shape: a (HANDLER-BIND ((ERROR ...))) inside
  ;; the parked BODY that answers any error by resuming the wait (the
  ;; reused-pooled-connection retry). A cancel that unwound as a signalled
  ;; condition was eaten there, and the resumed request re-parked INSIDE the
  ;; interruption with interrupts disabled — SBCL's poll(2)-without-a-timeout
  ;; wedge that needed a second Esc. The cancel must cross BODY as a THROW no
  ;; handler-bind can see, and re-signal only at the region boundary.
  (with-session-store ("lifecycle-foreign-handler")
    (is-let* ((retries 0)
              (outcome (%cancel-parked-wait "lifecycle-foreign-handler" "fh-command"
                                            (lambda (entered)
                                              (tagbody
                                                 retry
                                                 (handler-bind ((error
                                                                  (nlk:lambda-ignoring (e)
                                                                    (incf retries)
                                                                    (go retry))))
                                                   (sb-thread:signal-semaphore entered)
                                                   (sleep 30)))))))
      (outcome eq :cancelled) ((zerop retries) is))))

(deftest a-stale-wait-interruption-is-a-no-op ()
  ;; Delivery can race region exit: the interruption function outside any
  ;; cancellable wait must do nothing rather than unwind foreign code.
  (is (null (nlk::cancel-wait-interruption))))


(deftest durable-admission-refuses-a-second-active-turn (with-session-store ("one-active"))
  (nlk:admit-turn "one-active" "first-command" "first")
  (signals-error nlk:turn-already-active
    (nlk:admit-turn "one-active" "second-command" "second"))
  (is (equal "active" (nle::replay-admission-state "one-active"))))

(defun %admitted-active-turn (session-id command-id input)
  "The turn ADMIT-ACTIVE-TURN-INPUT starts for INPUT on idle SESSION-ID."
  (nlk:active-input-admission-turn
   (nlk:admit-active-turn-input session-id command-id input)))

(defun %asked (session-id)
  "The user turns SESSION-ID's fold hands a provider, oldest first."
  (loop for message in (wire-fold session-id)
        when (equal "user" (gethash "role" message))
          collect (gethash "content" message)))

(deftest queued-follow-up-folds-exactly-once-after-promotion (with-session-store ("queue-fold"))
  (let* ((turn (%admitted-active-turn "queue-fold" "fold-cmd-a" "first"))
         (queued (nlk:admit-active-turn-input "queue-fold" "fold-cmd-b" "second")))
    (is (eq :queued queued.disposition))
    (is (equal '("first") (%asked "queue-fold")))
    (nlk:complete-turn turn nil)
    (let ((next (nlk::dequeue-next-turn "queue-fold")))
      (is next "pending prompt promotes after the terminal fact")
      (is (equal "second" (second next)) "promotion carries the queued input"))
    (is (equal '("first" "second") (%asked "queue-fold")))))

(deftest resume-claim-is-per-boot-and-exhausts-into-failure ()
  (with-restartable-store (path "lifecycle-reclaim")
    (with-saved-globals (nlk::*recovery-boot-id*)
      (nlk:admit-turn "lifecycle-reclaim" "reclaim-command" "hello")
      (reopen-store path)
      (flet ((claimed-p (new-boot)
               (when new-boot (setf nlk::*recovery-boot-id* nil))
               (and (nlk::claim-turn-resumption (caar (nlk::recoverable-interrupted-turns))) t)))
        (is-table (new-boot expected) (eq expected (claimed-p new-boot))
          (t t "first boot claims") (nil nil) (t t) (t t "third boot claims") (t nil))
        (is (not (nlk:active-turn-p "lifecycle-reclaim")))
        (is (= 1 (count-session-events "lifecycle-reclaim" "turn.failed")))))))

(deftest boot-resumed-turns-adopt-the-cancellation-registry ()
  (with-restartable-store (path "resume-cancel")
    (with-saved-globals (nlk::*recovery-boot-id*)
      (nlk:with-cleanup ((clrhash nlk::*turn-cancellation-signals*))
        (nlk:admit-turn "resume-cancel" "resume-cancel-cmd" "hello")
        ;; A restart empties the process-local registry: the admission
        ;; registration is gone, only the durable suspension survives.
        (reopen-store path)
        (clrhash nlk::*turn-cancellation-signals*)
        (setf nlk::*recovery-boot-id* nil)
        (let* ((turn (first (first (nlk::recoverable-interrupted-turns))))
               (turn-id (and turn (nlk:durable-turn-turn-id turn))))
          (is-present turn "the interrupted turn is listed for resume"
            ;; The struct's signal IS the registry entry, so a cancel
            ;; reaches the resumed worker instead of flagging an orphan
            ;; while replying "accepted".
            (nlk:request-cancel-turn "resume-cancel" turn-id)
            (is (nlk::turn-cancellation-requested-p turn))
            ;; Opposite race: a cancel landing BEFORE the recovery pass
            ;; lists the turn is inherited by adoption, never overwritten.
            (clrhash nlk::*turn-cancellation-signals*)
            (nlk:request-cancel-turn "resume-cancel" turn-id)
            (let ((relisted (first (first (nlk::recoverable-interrupted-turns)))))
              (is (and relisted
                       (nlk::turn-cancellation-requested-p relisted))))))))))

(deftest history-floor-no-op-leaves-the-store-transaction-closed (with-session-store ("floor-txn"))
  (is (= 5 (nlk:advance-history-floor "floor-txn" 5)) "floor advances")
  (is (= 5 (nlk:advance-history-floor "floor-txn" 3)))
  (nlk:admit-turn "floor-txn" "floor-command" "hello")
  (is (nlk:active-turn-p "floor-txn")))

(deftest pending-prompt-census-sees-only-non-empty-latest-snapshots ()
  (with-session-store ("census-pending")
    (nlk:create-session :id "census-empty")
    (let ((turn (%admitted-active-turn "census-pending" "census-cmd-a" "first")))
      (nlk:admit-active-turn-input "census-pending" "census-cmd-b" "second")
      (is (equal '("census-pending") (nlk::sessions-with-pending-prompts)))
      (nlk:complete-turn turn nil)
      (nlk::dequeue-next-turn "census-pending")
      (is (null (nlk::sessions-with-pending-prompts))))))

(deftest dequeue-refuses-promotion-while-a-turn-is-active (with-session-store ("dq-guard"))
  (let ((turn (%admitted-active-turn "dq-guard" "dq-cmd-1" "one")))
    (nlk:admit-active-turn-input "dq-guard" "dq-cmd-2" "two")
    (nlk:admit-active-turn-input "dq-guard" "dq-cmd-3" "three")
    ;; The same in-transaction authority ADMIT-TURN consults: a promoter
    ;; that races a live turn must yield, not double-start.
    (is (null (nlk::dequeue-next-turn "dq-guard")))
    (nlk:complete-turn turn nil)
    (let ((promoted (nlk::dequeue-next-turn "dq-guard")))
      (is promoted "the oldest pending prompt promotes after the terminal fact")
      (is (equal "two" (second promoted)) "promotion is FIFO")
      (is (null (nlk::dequeue-next-turn "dq-guard")))
      (is (equal '("dq-guard") (nlk::sessions-with-pending-prompts))))))

(deftest history-floor-fact-carries-divider-evidence (with-session-store ("floor-wide"))
  (nlk:complete-turn (nlk:admit-turn "floor-wide" "floor-wide-cmd" "hello") nil)
  (nlk:advance-history-floor "floor-wide" 4
                             :reason "operator_requested"
                             :evicted-turn-count 1
                             :retained-turn-count 2
                             :evicted-message-count 3
                             :retained-message-count 5
                             :requested-percent 50
                             :realized-percent 41
                             :evicted-estimated-tokens 120
                             :retained-estimated-tokens 170)
  (let ((fact (session-event "floor-wide" :type 'nlk:context-evicted-event)))
    (is fact "the eviction fact is durable")
    (is (equal "operator_requested" (nlk:context-evicted-reason fact)))
    (is (plusp (nlk:context-evicted-source-watermark fact)))
    (is (> (nlk:event-log-position fact)
           (nlk:context-evicted-source-watermark fact)))
    (is-table (expected reader) (= expected (reader fact))
      (2 nlk:context-evicted-retained-turn-count "retained turn count is recorded")
      (3 nlk:context-evicted-message-count "evicted message count is recorded")
      (5 nlk:context-evicted-retained-message-count "retained message count is recorded")
      (50 nlk:context-evicted-requested-percent "the operator percent contract is recorded")
      (41 nlk:context-evicted-realized-percent "the realized share is recorded")
      (120 nlk:context-evicted-estimated-tokens "the evicted token estimate is recorded")
      (170 nlk:context-evicted-retained-estimated-tokens "the retained token estimate is recorded")))
  (is (= 4 (nlk:advance-history-floor "floor-wide" 2 :reason "operator_requested"))))

(deftest a-queued-prompt-promotes-with-the-images-it-was-staged-with ()
  ;; A queued follow-up waits in the durable snapshot, possibly across a
  ;; restart, so its attachments wait there too: promotion must hand the
  ;; turn the images the user staged with that prompt, not none.
  (with-session-store ("queue-images")
    (let ((busy-turn (%admitted-active-turn "queue-images" "qi-busy" "busy prompt")))
      (let ((queued (nlk:admit-active-turn-input
                     "queue-images" "qi-queued" "[Image #1] and this?"
                     :images '(("image/png" . "QUJD")))))
        (is (eq :queued queued.disposition)))
      (nlk:complete-turn busy-turn nil)
      (let ((next (nlk::dequeue-next-turn "queue-images")))
        (is-present next "completing the active turn frees the promotion"
          (is-shape next (second "[Image #1] and this?") (third '(("image/png" . "QUJD")))))))))

;;; --- DEFINE-TURN-FACT ----------------------------------------------------
;;; The durable turn-fact declaration. Two things are locked: the eight-field
;;; turn envelope every fact carries (which a hand-written recorder could omit
;;; a key of and still compile), and the payload rule that an unknown number is
;;; omitted rather than written as zero.

(deftest turn-facts-carry-the-whole-turn-envelope (with-session-store ("envelope"))
  (let ((turn (nlk:admit-turn "envelope" "env-cmd" "hi"
                              :definition-revision "rev-9")))
    (nlk:record-tool-call turn "c1" "run" "{}")
    (let ((row (session-event "envelope" :kind nlk::+kind-tool-call-started+)))
      (is row "the fact was appended")
      (is-table (expected reader) (equal expected (reader row))
        (turn.turn-id nlk::event-turn-id "turn id is stamped")
        ("env-cmd" nlk::event-command-id "command id is stamped")
        ("env-cmd" nlk::event-correlation-id "correlation is the command that admitted the turn")
        ((nlk:durable-turn-started-event-id turn) nlk::event-causation-id
         "causation is the turn's own start fact")
        ("rev-9" nlk::event-definition-revision "the turn's definition revision rides along")
        ("envelope" nlk::event-aggregate-id "the session is the aggregate")))))

(deftest turn-fact-payloads-omit-unknown-numbers (with-admitted-turn (turn "omit" "omit-cmd" "hi"))
  (nlk:record-turn-usage turn :input-tokens 10 :output-tokens nil
                              :model "m" :finish-reason nil)
  (let* ((event (session-event "omit" :kind nlk::+kind-turn-usage+))
         ;; The durable JSON, not the hydrated slot: a hydrated reader
         ;; answers its declared default for an absent key, which is
         ;; exactly the distinction under test.
         (payload (nlk:events :event-id event.event-id :as :payload)))
    (is-shape event (nlk:turn-usage-event-input-tokens = 10) (nlk:turn-usage-event-model "m"))
    (is (null (nth-value 1 (gethash "output-tokens" payload))))
    (is (null (nth-value 1 (gethash "finish-reason" payload))))))

(deftest record-exchange-turn-writes-a-whole-turn (with-session-store ("room"))
  ;; The write-back seam: a caller that produced an answer elsewhere folds
  ;; the pair in as history. Nothing runs, nothing stays active, and the
  ;; session is forkable at the exchange afterwards.
  (let ((turn-id (nlk:record-exchange-turn
                  "room" "alice: fix auth" "run pnpm test:auth")))
    (is (stringp turn-id))
    (is (equal '("turn.input_committed" "turn.started"
                 "turn.assistant_message_completed" "turn.completed")
               (mapcar #'nlk:event-kind
                       (session-events "room" :created nil))))
    (is (not (nlk:active-turn-p "room")))
    (is (equal turn-id (nlk:session-head-turn-id "room")))
    (is-present (input (session-event "room" :type 'nlk::user-message)) "the user side"
      (is (equal "alice: fix auth" (nlk::message-event-content input))))
    (is-present (answer (session-event "room"
                                       :type 'nlk::assistant-message))
      "the assistant side, in wire shape the context fold reads"
      (is (equal "run pnpm test:auth"
                 (nlk::message-event-content answer))))
    ;; Forkable at the exchange: CREATE-SESSION validates the anchor
    ;; against a real TURN.STARTED, which a synthesized turn must have.
    (let ((child (nlk:create-session :id "lane" :parent "room")))
      (is (equal turn-id child.anchor-turn-id))))
  ;; A second exchange chains onto the first rather than branching.
  (let ((second (nlk:record-exchange-turn "room" "bob: and signup?" "same")))
    (is (equal second (nlk:session-head-turn-id "room")))
    (is (= 2 (count-session-events "room" "turn.started")))))

(deftest record-exchange-turn-carries-the-facts-of-the-work-behind-it (with-session-store ("room"))
  ;; A recorder that knows what produced the answer - a reflection's turn,
  ;; its model, its clock, its tokens - says so on the completion, and the
  ;; transcript rests the pair's divider on it.
  (let ((plain (nlk:record-exchange-turn "room" "q" "a"))
        (told (nlk:record-exchange-turn
               "room" "q2" "a2"
               :facts '(:recorder "reflection" :provider "a6api" :model "glm-5.3-flash"
                        :elapsed-ms 18400 :input-tokens 3562 :cached-input-tokens 6528
                        :output-tokens 205 :reasoning-tokens 58))))
    (flet ((completed (turn)
             (nlk:events :session-id "room" :turn-id turn
                         :kind nlk::+kind-turn-completed+ :as :instance)))
      (is (null (nlk:turn-completed-recorder (completed plain))) "no facts, no recorder")
      (is-present (event (completed told)) "the completion with facts"
        (is-shape event (nlk:turn-completed-recorder "reflection")
          (nlk:turn-completed-provider "a6api") (nlk:turn-completed-model "glm-5.3-flash")
          (nlk:turn-completed-elapsed-ms = 18400) (nlk:turn-completed-input-tokens = 3562)
          (nlk:turn-completed-cached-input-tokens = 6528) (nlk:turn-completed-output-tokens = 205)
          (nlk:turn-completed-reasoning-tokens = 58)
          (nlk:turn-completed-turn-number = 2 "the pair still chains"))))
    (is-table (input answer facts)
      (signals-error type-error (nlk:record-exchange-turn "room" input answer :facts facts))
      ("q3" "a3" '(:elapsed-ms "soon"))
      ("q4" "a4" '(:cost-usd 1)))
    (is (= 2 (count-session-events "room" "turn.completed")))))

(deftest turn-origin-names-the-turns-no-one-asked ()
  ;; The one rule the listings that number and name turns read: an exit's wake
  ;; and a reflection's recap are not asks; a pair another recorder wrote is.
  (is-table (expected wake recorder)
    (equal expected (nlk:turn-origin wake recorder))
    (nil nil nil "a turn someone typed")
    ("wake" (vector (nlk:json-object "eval_id" 1003 "shell_exit" 2)) nil "a turn an exit woke")
    (nil #() nil "a wake that names no exit is none")
    ("reflection" nil "reflection" "the recap a reflection recorded")
    (nil nil "claude-code" "a pair an import recorded is an ask")))

;;; --- steers: parked at admission, committed at the drain --------------------
;;; Zig drainSteerIntoHistory: the accept path only queues; the running
;;; turn's round boundary commits the steer's INPUT_COMMITTED, so the fact
;;; sits in the log behind the round it interrupted and every fold — the
;;; provider request and the transcript — reads the steer where the model
;;; read it. Before this, admission wrote the fact at once and a steer typed
;;; during round 0 folded ahead of round 0's answer on both surfaces.

(defun %input-facts (session-id)
  "The session's INPUT_COMMITTED facts as (DISPOSITION MESSAGE STEER), oldest
first."
  (loop for event in (nlk:events :session-id session-id :kind nlk::+kind-input-committed+)
        for payload = (nlk::message-event-payload event)
        collect (list (gethash "disposition" payload)
                      (nlk::message-event-content event)
                      (nlk:json-value payload :boolean "steer"))))

(defun %steer (session-id command-id input expected-turn-id &rest keys)
  "The disposition ADMIT-ACTIVE-TURN-INPUT answers for INPUT sent under
COMMAND-ID as a steer of EXPECTED-TURN-ID; KEYS reach the admission."
  (nlk:active-input-admission-disposition
   (apply #'nlk:admit-active-turn-input session-id command-id input
          :disposition :steer :expected-turn-id expected-turn-id keys)))

(deftest a-steer-parks-until-promotion-starts-it-as-the-next-turn ()
  ;; A steer is queue state alone until it promotes: the running turn reads
  ;; it parked as its cue to end at the next round boundary, and dequeue
  ;; then starts it as a turn of its own, parented on the turn it cut, with
  ;; the images it was sent with. No steer input fact is ever written.
  (with-session-store ("steer-park")
    (let* ((turn (%admitted-active-turn "steer-park" "sp-start" "first")) (turn-id turn.turn-id))
      (is (not (nlk:steer-parked-p turn)) "nothing is parked yet")
      (is (eq :steered (%steer "steer-park" "sp-steer" "and this" turn-id
                               :images '(("image/png" . "QUJD")))))
      (is (equal '(("started" "first"))
                 (mapcar #'butlast (%input-facts "steer-park"))))
      (is-let* ((parked (nlk::queue-prompts "steer-park" turn-id)))
        ((length parked) = 1 "the steer waits in the queue snapshot")
        ((nlk::steer-prompt-p (aref parked 0)) is) ((zerop (nlk::follow-up-count parked)) is))
      (is (nlk:steer-parked-p turn))
      (is (eq :duplicate (%steer "steer-park" "sp-steer" "and this" turn-id)))
      (nlk:record-assistant-message
       turn (nlk:json-object "role" "assistant" "content" "first answer"))
      (nlk:complete-turn turn nil :steered t)
      (is (nlk:turn-completed-steered
           (nlk:events :session-id "steer-park" :kind nlk::+kind-turn-completed+
                       :as :instance)))
      (is (equal '(("started" "first"))
                 (mapcar #'butlast (%input-facts "steer-park"))))
      (let ((next (nlk::dequeue-next-turn "steer-park")))
        (is-present next "promotion starts the steer as the next turn"
            (destructuring-bind (promoted input images) next
              (is (equal "and this" input) "with its text")
              (is (equal '(("image/png" . "QUJD")) images))
              (is (equal turn-id
                         (nlk:turn-started-parent-turn-id
                          (nlk:events :turn-id promoted.turn-id
                                      :kind nlk::+kind-turn-started+
                                      :as :instance))))
              (is (equal '(("started" "first") ("started" "and this"))
                         (mapcar #'butlast (%input-facts "steer-park"))))
              (is (zerop (length (nlk::queue-prompts "steer-park" promoted.turn-id))))
              (is (eq :duplicate
                      (%steer "steer-park" "sp-steer" "and this" promoted.turn-id)))))))))

(deftest a-steer-outranks-the-follow-ups-parked-before-it (with-session-store ("steer-end"))
  ;; A follow-up waits for the turn to finish; a steer ends it at the next
  ;; boundary and runs next — so at promotion the steer goes first even when
  ;; a follow-up was parked before it, and the follow-up keeps its place
  ;; behind. A turn that ends with a steer parked commits no steer fact.
  (let* ((turn (%admitted-active-turn "steer-end" "se-start" "first")) (turn-id turn.turn-id))
    (nlk:admit-active-turn-input "steer-end" "se-follow" "next")
    (%steer "steer-end" "se-steer" "late" turn-id)
    (is (= 1 (nlk::follow-up-count (nlk::queue-prompts "steer-end" turn-id))))
    (nlk:complete-turn turn nil :steered t)
    (is (equal '("started" "follow-up")
               (nlk:distinct (mapcar #'first (%input-facts "steer-end")))))
    (is-present (next (nlk::dequeue-next-turn "steer-end")) "the steer promotes first"
        (is (equal "late" (second next)) "the steer, not the older follow-up")
        (let ((rest (nlk::queue-prompts
                     "steer-end" (nlk:durable-turn-turn-id (first next)))))
          (is (= 1 (length rest)) "the follow-up still waits")
          (is (equal "next" (gethash "content" (aref rest 0)))))
        (nlk:complete-turn (first next) nil)
        (let ((after (nlk::dequeue-next-turn "steer-end")))
          (is (equal "next" (and after (second after))))))))

(deftest a-promoted-steer-opens-a-turn-marked-as-the-steers (with-session-store ("steer-mark"))
  ;; The started input a steer's promotion writes says it was one, and the
  ;; transcript fold marks the user row that opens its turn; a follow-up's
  ;; promotion, and a first prompt, carry no mark.
  (let* ((turn (%admitted-active-turn "steer-mark" "sm-start" "first")) (turn-id turn.turn-id))
    (nlk:admit-active-turn-input "steer-mark" "sm-follow" "after")
    (%steer "steer-mark" "sm-steer" "cut in" turn-id)
    (nlk:complete-turn turn nil :steered t)
    (nlk:complete-turn (first (nlk::dequeue-next-turn "steer-mark")) nil)
    (nlk:complete-turn (first (nlk::dequeue-next-turn "steer-mark")) nil)
    (is (equal '(("first" nil) ("cut in" t) ("after" nil))
               (loop for (disposition . started) in (%input-facts "steer-mark")
                     when (equal "started" disposition) collect started)))
    (is (equal '(("first" nil) ("cut in" t) ("after" nil))
               (loop for row in (nle::session-transcript "steer-mark")
                     when (and (eq :text (nle::transcript-row-kind row))
                               (equal "user" (nle::transcript-row-role row)))
                       collect (list (nle::transcript-row-text row)
                                     (nle::transcript-row-steer-p row)))))))

(deftest record-exchange-turn-keeps-a-stated-time (with-session-store ("room"))
  ;; A recorder folding in an exchange that happened elsewhere, earlier,
  ;; states when: all four facts carry it, so the log reads in the order
  ;; things happened. Unstated, the facts are stamped now, as every live
  ;; write is; a clock in any other shape is refused before a row is written.
  (let* ((then "2026-06-23T15:05:03.250Z")
         (turn-id (nlk:record-exchange-turn "room" "q" "a" :occurred-at then))
         (stated (nlk:events :session-id "room" :turn-id turn-id)))
    (is (= 4 (length stated)) "one exchange, four facts")
    (is (every (lambda (event) (equal then event.occurred-at)) stated))
    (let* ((later (nlk:record-exchange-turn "room" "q2" "a2"))
           (now (nlk:events :session-id "room" :turn-id later)))
      (is (every (lambda (event) (string> event.occurred-at then)) now)))
    (is (signals-error type-error
          (nlk:record-exchange-turn "room" "q3" "a3" :occurred-at "yesterday")))
    (is (= 8 (length (session-events "room" :created nil))))))

(deftest boot-recovery-closes-the-open-tool-call (with-restartable-store (path "open-call"))
  ;; Without recovery, a turn resumed after a call fact is recorded but
  ;; before its result fact would put a tool_calls message with no tool reply
  ;; on the wire and fail. Recovery closes the open call before the resume.
  (with-saved-globals (nlk::*recovery-boot-id*)
    (let ((turn (nlk:admit-turn "open-call" "open-call-cmd" "hello")))
      (nlk:record-tool-call turn "call-died" "eval" "{\"form\":\"1\"}")
      (record-tool-use turn "call-done" "eval" "{\"form\":\"2\"}" "2"))
    (reopen-store path)
    (let* ((results (session-events "open-call" :type 'nlk:tool-result-event))
           (closed (find "call-died" results
                         :key #'nlk:tool-result-event-call-id :test #'equal)))
      (is (= 2 (length results)))
      (is-present closed "the closure names the open call"
        (is-shape closed (nlk:tool-result-event-result nlk::+restart-tool-result+)
          (nlk:tool-result-event-tool-name "eval"))))
    (is (nlk::recoverable-interrupted-turns))
    (reopen-store path)
    (is (= 2 (length (session-events "open-call" :type 'nlk:tool-result-event))))))

(deftest turn-answer-reads-what-a-turn-said-last (with-temp-store ())
  ;; The newest assistant message's text, of one turn or of the session: an
  ;; earlier round's words are not the answer once a later round spoke, and
  ;; a turn whose last round only called tools answered nothing.
  (ensure-durable-session "said")
  (let ((quiet (nlk:admit-turn "said" (nlk::make-durable-id "command") "one")))
    (nlk:record-assistant-message quiet (nlk:json-object "role" "assistant" "content" :null))
    (nlk:complete-turn quiet nil)
    (let ((worked (nlk:admit-turn "said" (nlk::make-durable-id "command") "two")))
      (nlk:record-assistant-message worked (nlk:json-object "role" "assistant" "content" "looking first"))
      (nlk:record-assistant-message worked (nlk:json-object "role" "assistant" "content" "found it"))
      (nlk:complete-turn worked nil)
      (seed-completed-turn "said" "three" "and this")
      (is (null (nlk:turn-answer "said" (nlk::durable-turn-turn-id quiet))) "a turn whose last round only called tools answered nothing")
      (is (equal "found it" (nlk:turn-answer "said" (nlk::durable-turn-turn-id worked))) "a turn's answer is its last round's words, not an earlier round's")
      (is (equal "and this" (nlk:turn-answer "said")) "with no turn named, the session's newest answer")
      (is (null (nlk:turn-answer "never")) "a session with no answer at all"))))
