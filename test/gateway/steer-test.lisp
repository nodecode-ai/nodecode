;;;; steer-test.lisp --- U4 gates: the steer client leg.
;;;;
;;;; Claims:
;;;;   1. A steer_turn command round-trips the wire: prompt normalizes
;;;;      through the Zig content-part shape, expected_turn_id survives.
;;;;   2. Submitting while a turn runs takes the steer path: history noted,
;;;;      composer reset, turn stays in flight, and a delivery failure lands
;;;;      in the notice lane instead of signalling.
;;;;   3. With a turn in flight but no admitted turn id yet, Enter holds the
;;;;      text — nothing fires against an unknown turn.
;;;;   4. Against a live gateway, the client's steer command traverses the
;;;;      real decode path and comes back as a correlated command result
;;;;      (rejected here: the session has no active turn to steer).
;;;;
;;;; Loads after gateway-test: reuses WITH-GATEWAY-SESSION, FRAMES-WHILE,
;;;; FRAME-CARRYING, FEED-WIRE, and the press/type-string/composer-value helpers.

(in-package #:nodecode.test)

;;; --- claim 1: wire round-trip ----------------------------------------------

(deftest steer-command-round-trips-the-wire ()
  (let* ((command (nlk:make-gateway-command
                   :kind :steer-turn
                   :command-id "cmd-steer-1"
                   :correlation-id "corr-steer-1"
                   :request (nlk:json-object "session_id" "s-steer"
                                             "expected_turn_id" "turn-9"
                                             "prompt" "go left instead")))
         (frame (nlk:decode-sync-message
                 (nlk::encode-client-command command)))
         (decoded frame.payload))
    (is (eq :client-command frame.type))
    (is (eq :steer-turn decoded.kind))
    (is-shape decoded.request ("expected_turn_id" "turn-9") ("prompt" "go left instead"))))

;;; --- claims 2-3: the client dispatch ----------------------------------------

(defun make-steer-model (session-id &aux (model (detached-shell session-id)))
  "A gateway chat model initialized through the real init path, no socket."
  (nlt:init model)
  (nle:update model (nlt:make-window-size-msg :width 80 :height 24))
  model)

(defmacro with-running-turn-model ((model session-id &key (turn-id "turn-live"))
                                   &body body &aux (link (gensym "LINK")))
  "Bind MODEL to a fresh gateway shell mid-turn over a temp store: a turn is
in flight and TURN-ID names the admitted turn."
  ;; Both slots are load-bearing — without them Enter and Tab take the idle
  ;; path and the gesture under test never fires at all. :TURN-ID NIL is the
  ;; not-yet-admitted case. A running turn came over a link, and this one
  ;; keeps what it carries (RECORDING-LINK).
  `(with-temp-store ()
     (with-recording-link (,link)
       (let ((,model (make-steer-model ,session-id)))
         (setf (nle::chat-model-turn-in-flight ,model) t
               (nle::gateway-model-active-turn-id ,model) ,turn-id
               (nle::gateway-model-ws ,model) ,link)
         ,@body))))

(deftest steer-leg-fires-while-a-turn-runs (with-running-turn-model (model "steer-dispatch"))
  (type-string model "go left" :enter)
  (is-shape model (composer-value "") (nle::chat-model-turn-in-flight is) (nle::status-said null))
  (is (equal '("steer_turn") (link-commands model.ws)) "the steer left over the link")
  (is (equal "go left" (first model.prompt-history))))

(deftest steer-waits-for-an-admitted-turn-id ()
  (with-running-turn-model (model "steer-pending" :turn-id nil)
    (type-string model "too early" :enter)
    (is (equal "too early" (composer-value model)))))

;;; --- the Tab queue-submit gesture -------------------------------------------
;;; The Zig submit_turn_queue split: Enter steers the running turn, Tab
;;; enqueues an ordinary start_turn the gateway parks behind it. No
;;; dedicated queue command exists on the wire in either runtime.

(deftests tab (session-id typed key checks)
  (with-running-turn-model (model session-id)
    (when typed (type-string model typed))
    key
    (is (equal "" (composer-value model)))
    (is (equal typed (first model.prompt-history)))
    checks)
  (queue-submit-fires-while-a-turn-runs
   "queue-dispatch" "queued follow-up" (press model :tab)
   (progn (is (nle::chat-model-turn-in-flight model))
          (is (null model.pending-command-id))
          (is (equal '("start_turn") (link-commands model.ws)))))
  ;; HT with the reader's C0 ctrl bit — what a real Tab press delivers.
  (queue-submit-fires-on-the-byte-the-terminal-sends
   "queue-c0" "queued follow-up" (press-wire model 9) nil)
  (empty-composer-is-a-consumed-no-op-mid-turn
   "queue-noop" nil (press model :tab)
   (is (null (nle::status-said model))))
  (mid-turn-outranks-the-slash-dropdown
   "queue-dropdown" "/con" (press model :tab) nil))

(deftest tab-idle-keeps-suggestion-accept (with-temp-store ())
  (let ((model (make-steer-model "queue-idle")))
    (type-string model "/models" :tab)
    (is-shape model (composer-value "/models ") (.turn-in-flight not))))

(deftest queued-command-result-sets-the-composer-status ()
  (with-running-turn-model (model "queue-result")
    (nle::gateway-command-result
     model (nlk:json-object "status" "queued"
                            "command" "start_turn"
                            "command_id" "q-cmd"
                            "response" (nlk:json-object "turn_id" "turn-live"
                                                        "queue_depth" 2)))
    (is-shape model (nle::status-said "Queued (2 waiting)")
      (nle::chat-model-turn-in-flight is) (.active-turn-id "turn-live"))))

(deftest queue-submit-enqueues-over-the-wire-and-drains-in-order ()
  ;; The full admission seam: two start_turn commands while a turn is
  ;; active come back "queued" with depths 1 and 2 against the active
  ;; turn's id, and dequeue promotion drains them strictly FIFO.
  (with-gateway-session (port "queue-wire")
    (let* ((admission (nlk:admit-active-turn-input
                       "queue-wire" "qw-busy" "busy prompt"))
           (busy-turn-id admission.turn-id)
           (frames (frames-while (ws port :subscribe '("queue-wire"))
                     (nle::send-start-turn ws "queue-wire" "queued one"
                                           :command-id "qw-cmd-1")
                     (nle::send-start-turn ws "queue-wire" "queued two"
                                           :command-id "qw-cmd-2"))))
      (flet ((result-for (command-id)
               (nlk:json-value (rpc-result frames command-id) :object "payload" "result")))
        (is-present (first-result (result-for "qw-cmd-1")) "the first enqueue came back as a result"
          (is-shape first-result ("status" "queued") ("message" "turn input queued")
            ((:any "response" "turn_id") busy-turn-id) ((:any "response" "queue_depth") eql 1)))
        (is-present (second-result (result-for "qw-cmd-2"))
            "the second enqueue came back as a result"
            (is-shape second-result ("status" "queued") ((:any "response" "queue_depth") eql 2))))
      ;; Drain order: promotion is strictly FIFO, one turn at a time.
      (nlk:complete-turn admission.turn nil)
      (is-present (next (nlk::dequeue-next-turn "queue-wire"))
          "completing the active turn frees the first promotion"
          (is (equal "queued one" (second next)))
          (nlk:complete-turn (first next) nil))
      (is-present (next (nlk::dequeue-next-turn "queue-wire")) "the second promotion follows"
          (is (equal "queued two" (second next)))
          (nlk:complete-turn (first next) nil))
      (is (null (nlk::dequeue-next-turn "queue-wire"))))))

;;; --- retraction: Esc before the turn produced anything ----------------------
;;; The cancel_turn strategy input "retract" and the client's half of it: a
;;; composer stash captured at submit, Esc sending (or deferring) a
;;; retracting cancel for the turn this composer started, and the gateway's
;;; retract confirmation (session_checkpoint_undo_applied reason "retract")
;;; handing the prompt back exactly as typed.

(deftest cancel-turn-retract-flag-round-trips-the-wire ()
  (flet ((sent-request (&rest args &aux (captured nil))
           ;; The request SEND-CANCEL-TURN serializes, after a wire round-trip.
           (with-stubbed-fdefinition (wsd:send (ws text) (setf captured text))
             (apply #'nle::send-cancel-turn :no-socket "s-retract" "turn-9" args))
           (nlk:gateway-command-request
            (nlk:sync-message-payload (nlk:decode-sync-message captured)))))
    (let ((plain (sent-request))
          (retracting (sent-request :retract t)))
      (is (equal "turn-9" (gethash "turn_id" plain)))
      (is (null (nth-value 1 (gethash "retract" plain))))
      (is-shape retracting ("retract" eq t) ("turn_id" "turn-9")))))

(defmacro with-steer-shell ((model starts cancels &key submit accepted) &body body)
  "Run BODY over a temp store with MODEL a fresh shell on session \"s\" whose
outgoing commands are recorded, not sent: STARTS collects (prompt command-id),
CANCELS collects (turn-id retract)."
  ;; SUBMIT, when given, is typed into the composer and entered first, and
  ;; ACCEPTED, when given, is the turn id the gateway then admits that start as.
  ;; The shell has a link that could carry them, or Enter would be refused
  ;; before any send.
  (let ((link (gensym "LINK")))
    `(with-temp-store ()
       (let ((,starts '()) (,cancels '()))
         (with-stubbed-fdefinitions
             ((nle::send-start-turn (ws session-id prompt &key command-id
                                        correlation-id images target)
                (push (list prompt command-id target) ,starts))
              (nle::send-cancel-turn (ws session-id turn-id &key retract)
                (push (list turn-id retract) ,cancels))
              (nle::send-subscribe (ws session-ids) nil))
           (with-recording-link (,link)
             (let ((,model (make-steer-model "s")))
               (setf (nle::gateway-model-ws ,model) ,link)
               ,@(when submit `((type-string ,model ,submit) (press ,model :enter)))
               ,@(when accepted
                   `((feed-wire ,model (%accepted-start (second (first ,starts)) ,accepted))))
               ,@body)))))))

(defun %start-result (command-id status &rest members)
  "The command_result the gateway answers COMMAND-ID's start_turn with:
STATUS, and the MEMBERS that ride it — the code, the message, the response."
  (nlk:encode-command-result
   1 (apply #'nlk:make-json-object
            "command_id" command-id "correlation_id" command-id
            "session_id" "s" "command" "start_turn"
            "status" status members)
   :session-id "s"))

(defun %accepted-start (command-id turn-id)
  (%start-result command-id "accepted" "message" "turn admitted"
                 "response" (nlk:json-object "turn_id" turn-id
                                             "queue_depth" 0)))

(defun %undo-applied (reason previous-turn-id &key (session-id "s"))
  (nlk:encode-projection-event
   session-id 0 "session_checkpoint_undo_applied"
   (nlk:json-object "session_id" session-id
                    "target_turn_id" ""
                    "previous_turn_id" previous-turn-id
                    "reason" reason
                    "boundary" "user_message")))

(defun %turn-fact (kind turn-id &rest keys &key (seq 5) (payload (nlk:json-object "reason" "esc"))
                                             &allow-other-keys)
  "One durable fact of TURN-ID on the live stream: FACT-FRAME's, KEYS and all."
  ;; COMMAND-ID, when given, rides the metadata the way the gateway stamps the
  ;; command that produced the fact.
  (nlk:encode-projection-event
   "s" seq "session_stream_event" (apply #'fact-frame kind turn-id :payload payload keys)
   :cursor seq :durable-p t))

(defun %user-rows (model)
  "The user :TEXT rows of MODEL's transcript, in display order."
  (remove-if-not (lambda (row) (and (eq :text row.kind) (equal "user" row.role)))
                 (nle::transcript-rows model.state)))

(defun %input-fact (command-id text &key (seq 2) (turn-id "t1"))
  "The durable turn.input_committed a start_turn COMMAND-ID committed."
  (%turn-fact "turn.input_committed" turn-id :seq seq :command-id command-id
              :payload (nlk:json-object "role" "user" "message" text
                                        "disposition" "started")))

(defun %iso-ms (ms)
  "The ISO-NOW shape for MS milliseconds since the CL epoch."
  (nlk:iso-time (- (/ ms 1000) nlk:+unix-epoch+)))

(defun %bar-elapsed-ms (model)
  (truncate (* 1000 (- (get-internal-real-time)
                       model.turn-started-at))
            internal-time-units-per-second))

(deftest turn-started-frame-anchors-the-bar-clock-on-the-durable-start ()
  ;; The finish divider's elapsed is terminal.occurred_at minus
  ;; turn.started.occurred_at. The bar's clock starts at submit, then moves
  ;; onto that same durable start the moment the fact lands, so the number
  ;; the operator watched is the number the divider rests with.
  (with-steer-shell (model starts cancels)
    (let ((now-ms (nle::iso-8601-ms (nlk::iso-now))))
      (type-string model "hello there" :enter)
      (is (< (%bar-elapsed-ms model) 1000))
      (feed-wire model (%turn-fact "turn.started" "t1" :seq 2 :payload (nlk:json-object "input" "hi")
                                   :occurred-at (%iso-ms (- now-ms 3000))))
      (is (<= 3000 (%bar-elapsed-ms model) 4000))
      (is (equal 3000 (- now-ms (gethash "turn_started_at_ms"
                                         model.state))))
      ;; A start stamped in the future (skew across hosts) clamps: the
      ;; bar never runs a negative clock.
      (feed-wire model (%turn-fact "turn.started" "t2" :seq 3 :payload (nlk:json-object "input" "hi")
                                   :occurred-at (%iso-ms (+ now-ms 60000))))
      (is (<= 0 (%bar-elapsed-ms model) 1000))
      ;; A replay onto an active turn re-derives the anchor from the same
      ;; fact rather than restarting at zero (the replay-final arm calls
      ;; ANCHOR-TURN-CLOCK after folding the chunk).
      (setf (gethash "turn_started_at_ms" model.state)
            (- now-ms 30000))
      (nle::anchor-turn-clock model)
      (is (<= 30000 (%bar-elapsed-ms model) 31000)))))

(deftest esc-before-admission-defers-a-retracting-cancel ()
  ;; Esc in the window between Enter and the start_turn ack: the turn id is
  ;; unknown, so nothing is sent yet and the tree gesture is NOT armed; the
  ;; retracting cancel goes out the moment the ack names the turn.
  (with-steer-shell (model starts cancels :submit "hello there")
    (is (= 1 (length starts)) "Enter sent the start")
    (is (nle::chat-model-turn-in-flight model) "the turn is in flight")
    (is (null model.active-turn-id))
    (let ((stash model.retract-stash))
      (is-shape stash (:text "hello there") (:command-id (second (first starts)))))
    (press model :escape)
    (is (null cancels) "no cancel can be sent without a turn id")
    (is-shape model (.retract-requested is) (.last-escape-at null) (nle::status-said "Cancelling"))
    (feed-wire model (%accepted-start (second (first starts)) "turn-1"))
    (is (equal '(("turn-1" t)) cancels))
    (is (not model.retract-requested))
    (is (equal "turn-1" (getf model.retract-stash :turn-id)))
    (is (equal "" (composer-value model)))))

(deftest esc-after-admission-sends-a-retracting-cancel-for-an-owned-turn ()
  (with-steer-shell (model starts cancels :submit "mine" :accepted "turn-own")
    (press model :escape)
    (is (equal '(("turn-own" t)) cancels))
    ;; An adopted turn — learned from a replay, no stash — is a plain
    ;; cancel: the prompt is not this composer's to take back.
    (setf cancels '()
          model.retract-stash nil
          model.active-turn-id "turn-adopted"
          model.turn-in-flight t)
    (press model :escape)
    (is (equal '(("turn-adopted" nil)) cancels))))

(deftest retract-undo-applied-refills-the-composer-from-the-stash ()
  ;; The gateway's retract confirmation hands the prompt home whole: text
  ;; with its paste markers, the paste store, the staged images. The head
  ;; move notices stay quiet for this client; an unrelated head move keeps
  ;; the ordinary notice.
  (with-steer-shell (model starts cancels)
    (setf model.pastes '((1 . "pasted body")) model.pending-images '(("image/png" . "AAAA")))
    (type-string model "[Paste #1] and more" :enter)
    (is (equal "" (composer-value model)) "submit cleared the composer")
    (is (null model.pastes) "submit consumed the pastes")
    (feed-wire model (%accepted-start (second (first starts)) "turn-1"))
    (press model :escape)
    (feed-wire model (%turn-fact "turn.cancelled" "turn-1"))
    (is-shape model (.turn-in-flight not) (.retract-stash is))
    (feed-wire model (%undo-applied "retract" "turn-1"))
    (is-shape model (composer-value "[Paste #1] and more") (.pastes '((1 . "pasted body")))
      (.pending-images '(("image/png" . "AAAA")))
      (nle::status-said "Retracted") (.retract-stash null "the stash is spent"))
    (feed-wire model (nlk:encode-projection-event
                      "s" 0 "session_resync_required" (nlk:json-object "session_id" "s")))
    ;; the retraction's own resync is quiet: it says nothing over `Retracted'
    (is-shape model (nle::status-said "Retracted") (.sync-lifecycle eq :reconnecting))
    ;; An unrelated head move: composer untouched, ordinary notice.
    (setf model.reconnect-required nil model.quiet-resync nil)
    (type-string model "!")
    (feed-wire model (%undo-applied "rewind" "turn-other"))
    (is (equal "[Paste #1] and more!" (composer-value model)))
    (is (equal "History rewind: the head moved" (nle::status-said model)))))

(deftest reconnect-ready-drops-a-retracted-pending-start ()
  ;; Esc asked for the prompt back before any ack, then the connection
  ;; replayed with no admission for the command: the start is dropped, not
  ;; re-sent, and the prompt goes home -- to the composer, and out of the
  ;; transcript the row Enter painted stood in until then.
  (with-steer-shell (model starts cancels :submit "never admitted")
    (press model :escape)
    (is (= 1 (length (%user-rows model))) "still painted while the ack is awaited")
    (feed-wire model (resumed-frame '()))
    (is (= 1 (length starts)) "the retracted start was not re-sent")
    (is (null cancels) "nothing to cancel: the turn never existed")
    (is-shape model (composer-value "never admitted") (.turn-in-flight not "nothing is in flight")
      (.pending-command-id null) (.retract-requested not)
      (%user-rows null "the painted row went home with it"))))

(deftest completed-terminal-clears-the-stash ()
  ;; A turn that completed produced its history: a later retract head move
  ;; naming it (another peer's doing) must not refill this composer.
  (with-steer-shell (model starts cancels :submit "done deal" :accepted "turn-1")
    (feed-wire model (%turn-fact "turn.completed" "turn-1"))
    (is (null model.retract-stash))
    (type-string model "next")
    (feed-wire model (%undo-applied "retract" "turn-1"))
    (is (equal "next" (composer-value model)))))

;;; --- the steered handoff: the flight crosses to the promoted turn ----------
;;; A completion a parked steer cut short is a handoff, not an end: the
;;; promoted text starts the next turn, and the shell keeps its flight armed
;;; across the gap. Without this, an Enter in the gap would leave as a fresh
;;; start_turn, so a chained steer would become a queued follow-up -- one
;;; steer at a time.

(deftest a-steered-completion-keeps-the-flight-until-the-promoted-turn-starts ()
  (with-steer-shell (model starts cancels :submit "hello there" :accepted "turn-1")
    (feed-wire model (%turn-fact "turn.completed" "turn-1"
                                 :payload (nlk:json-object "steered" t)))
    (is-shape model (.turn-in-flight is "the flight crosses the handoff")
      (.active-turn-id null "the dead turn's id is gone")
      (nle::chat-model-turn-started-at is "the clock keeps running"))
    (feed-wire model (%turn-fact "turn.started" "turn-2" :seq 6))
    (is-shape model (.turn-in-flight is)
      (.active-turn-id "turn-2" "the promoted turn takes it over"))))

(deftest an-enter-in-the-steered-handoff-waits-instead-of-starting-a-turn ()
  (with-steer-shell (model starts cancels :submit "hello there" :accepted "turn-1")
    (type-string model "and also this" :enter)
    (feed-wire model (%turn-fact "turn.completed" "turn-1"
                                 :payload (nlk:json-object "steered" t)))
    (type-string model "third ask" :enter)
    (is (= 1 (length starts)) "no second start_turn left the shell")
    (is (equal "third ask" (composer-value model)) "the text waits for the promoted turn")
    (feed-wire model (%turn-fact "turn.started" "turn-2" :seq 6))
    (press model :enter)
    (let ((sent (link-commands model.ws)))
      (is (equal '("steer_turn" "steer_turn") sent) "the waiting ask lands as a steer of the promoted turn"))))

(deftest a-plain-completion-still-settles-the-flight ()
  (with-steer-shell (model starts cancels :submit "hello there" :accepted "turn-1")
    (feed-wire model (%turn-fact "turn.completed" "turn-1"))
    (is-shape model (.turn-in-flight not) (.active-turn-id null))))

(deftest a-refused-start-hands-its-prompt-back-to-the-composer ()
  ;; A start the gateway refuses never became a turn: the prompt comes home as
  ;; typed, unless the composer holds something new, which it keeps.
  (with-steer-shell (model starts cancels :submit "run this")
    (feed-wire model (%start-result (second (first starts)) "rejected" "code" "unknown_provider" "message" "no x"))
    (is (equal "run this" (nle::editor-value model.editor)) "the prompt is back in the composer")
    (is (search "Turn rejected" (nle::status-said model)) "and the notice says why")
    (press model :enter)
    (type-string model "newer")
    (feed-wire model (%start-result (second (first starts)) "rejected" "code" "unknown_provider" "message" "no x"))
    (is (equal "newer" (nle::editor-value model.editor)) "what was typed since stays")))

(deftest a-launch-prompt-goes-out-on-its-target-once-the-session-stands ()
  ;; The launch's prompt waits in the composer, then goes through Enter's
  ;; start leg with the held target; the admission spends the target.
  (with-steer-shell (model starts cancels)
    (nle::seat-launch model "s" '(:model "openai-codex/gpt-6.1-sol:high" :prompt "fix the bug"))
    (is (equal "fix the bug" (nle::editor-value model.editor)) "the prompt waits in the composer")
    (nle::send-launch-prompt model)
    (is (equal "fix the bug" (first (first starts))) "then goes out as typed")
    (is (equal "openai-codex/gpt-6.1-sol:high" (getf (third (first starts)) :model)) "with the target")
    (feed-wire model (%accepted-start (second (first starts)) "turn-1"))
    (is (null (nle::held-launch-target model)) "which the admission spends")
    (nle::send-launch-prompt model)
    (is (= 1 (length starts)) "and it goes once")))

(deftest a-launch-prompt-seated-before-the-composer-waits-for-init ()
  ;; The shell seats its launch before the program's INIT makes the composer.
  (let ((model (make-instance 'nle::gateway-chat-model :session-id "s" :ws nil)))
    (nle::seat-launch model "s" '(:model "m" :prompt "fix the bug"))
    (is (null (nle::chat-model-editor model)) "no composer yet")
    (nlt:init model)
    (is (equal "fix the bug" (nle::editor-value (nle::chat-model-editor model))) "INIT seats it in the composer")
    (is (equal "fix the bug" (getf (nle::gateway-model-launch-prompt model) :composer)) "to be sent from there")))

(deftest a-launch-prompt-waits-when-edited-or-refused-and-goes-out-exact ()
  (with-steer-shell (model starts cancels)
    (nle::seat-launch model "s" '(:prompt "fix"))
    (type-string model " it")
    (nle::send-launch-prompt model)
    (is (null starts) "an edited prompt is the operator's: it waits"))
  (with-steer-shell (model starts cancels)
    (nle::seat-launch model "s" '(:model "nope" :prompt "fix"))
    (setf (nle::gateway-model-launch-refusal model) (cons "unknown_provider" "no provider \"nope\""))
    (nle::send-launch-prompt model)
    (is (null starts) "a refused target sends nothing")
    (is (search "refused: no provider" (nle::status-said model)) "and says why")
    (is (equal "fix" (nle::editor-value model.editor)) "the prompt waiting in the composer"))
  (with-steer-shell (model starts cancels)
    (let ((exact (format nil "  a~cb~%" #\Tab)))
      (nle::seat-launch model "s" (list :prompt exact))
      (is (equal "[Paste #1]" (nle::editor-value model.editor)) "a prompt a paste would change shows as one")
      (nle::send-launch-prompt model)
      (is (equal exact (first (first starts))) "and goes out byte for byte"))))

(deftest rejected-start-turn-clears-stash-and-marker ()
  ;; The rejection takes back the prompt row Enter painted, too, and says why.
  (with-steer-shell (model starts cancels :submit "refused")
    (is (= 1 (length (%user-rows model))))
    (press model :escape)
    (feed-wire model (%start-result (second (first starts)) "rejected"
                                    "code" "unknown_session"
                                    "message" "session does not exist"))
    (is-shape model (.retract-requested not "no marker")
      (.retract-stash null "no stash") (.turn-in-flight not "nothing in flight")
      (%user-rows null "a rejected start leaves no prompt row"))
    (is (null cancels) "a rejected start has no turn to cancel")
    (is (search "Turn rejected" (nle::status-said model)))))

(deftest session-switch-drops-the-retract-stash ()
  (with-steer-shell (model starts cancels :submit "left behind")
    (press model :escape)
    (nle::switch-gateway-session model "s-2")
    (is (null model.retract-stash))
    (is (not model.retract-requested) "no marker")
    (is (not model.quiet-resync) "no quiet flag")))

(deftest retracting-cancel-over-the-wire-erases-the-prompt-from-replay ()
  ;; The whole seam through a real socket: the cancel_turn{retract:true}
  ;; decodes, the gateway retracts the parked turn, the subscribed peer
  ;; sees the retract head move and the resync, and a fresh subscribe
  ;; replay no longer carries the prompt.
  (with-gateway-session (port "retract-wire")
    (with-provider-stub (nil (park-until-cancelled))
      (let* ((turn (nlk:active-input-admission-turn
                    (nle:submit "retract-wire" "wire me back"
                                :command-id "rw-cmd")))
             (turn-id turn.turn-id))
        (is (%wait-for-parked-worker turn) "the worker parked in its wait")
        (let* ((frames (frames-while (ws port :subscribe '("retract-wire"))
                         (nle::send-cancel-turn ws "retract-wire" turn-id
                                                :retract t)))
               (types (mapcar #'frame-type frames)))
          (is (member "session_checkpoint_undo_applied" types :test #'equal))
          (is (member "session_resync_required" types :test #'equal))
          (is-present (undo (find-frame frames "session_checkpoint_undo_applied"))
            "the head-move frame is present"
            (is-shape (gethash "payload" undo) ("reason" "retract") ("previous_turn_id" turn-id))))
        (is (%wait-for-worker-exit "retract-wire") "the worker settled")
        (is (null (nlk:session-head-turn-id "retract-wire")))
        (is-present (resumed (find-frame (subscribed-frames port "retract-wire")
                                         "session_stream_resumed"))
          "a fresh subscribe replays"
          (is (not (search "wire me back"
                           (shasht:write-json (gethash "payload" resumed) nil)))))))))

;;; --- claim 4: live gateway round-trip ---------------------------------------

(deftest steer-command-reaches-a-live-gateway (with-gateway-session (port "steer-live"))
  (let* ((frames (frames-while (ws port :subscribe '("steer-live"))
                   (nle::send-steer-turn ws "steer-live" "turn-absent"
                                         "steer into the void"
                                         :command-id "steer-cmd-live")))
         (result (rpc-result frames "steer-cmd-live")))
    (is-present result "the steer command came back as a correlated result"
        (is (equal "rejected" (gethash "status" (gethash "result" (gethash "payload" result))))))))

;;; --- claim 5: a steer's images join the turn it steered ---------------------

;;; --- claim 6: chained steers ------------------------------------------------
;;; The operator's "chained queued steers": a second steer parks on the same
;;; running turn while the first waits -- nothing refuses it, nothing demotes
;;; it to a follow-up -- and the two promote in order as the turn ends at its
;;; boundary on each of them.

(deftest two-steers-chain-through-their-turn-boundaries (with-session-store ("steer-chain"))
  (let ((turn (%admitted-active-turn "steer-chain" "chain-cmd-1" "first")))
    (loop for (command prompt) in '(("chain-a" "A") ("chain-b" "B"))
          do (is (eq :steered (nlk:active-input-admission-disposition
                               (nlk:admit-active-turn-input "steer-chain" command prompt
                                                            :disposition :steer
                                                            :expected-turn-id turn.turn-id)))))
    (let ((queued (mapcar (lambda (prompt)
                            (cons (gethash "content" prompt)
                                  (gethash "disposition" prompt)))
                          (coerce (nlk::queue-prompts "steer-chain" turn.turn-id) 'list))))
      (is (equal '(("A" . "steer") ("B" . "steer")) queued) "both steers wait, chained, in the queue"))
    (nlk:complete-turn turn nil :steered t)
    (let ((first-steer (nlk::dequeue-next-turn "steer-chain")))
      (is-present first-steer "the first steer promotes as the next turn"
        (is (equal "A" (second first-steer)))
        (is (nlk:steer-parked-p (first first-steer)) "the second waits parked on it"))
      (nlk:complete-turn (first first-steer) nil :steered t)
      (let ((second-steer (nlk::dequeue-next-turn "steer-chain")))
        (is-present second-steer "the second steer follows it"
          (is (equal "B" (second second-steer))))))))

(deftest steered-images-promote-with-the-steer (with-gateway-session (port "steer-images"))
  ;; A steer is delivered as the next turn, so its attachments belong to
  ;; that turn — not to the turn it cut short, and not nowhere. They wait in
  ;; the queue snapshot beside the steer and promote with it, so a restart
  ;; between the two loses neither.
  (let ((admission (nlk:admit-active-turn-input
                    "steer-images" "si-busy" "busy prompt")))
    (frames-while (ws port :subscribe '("steer-images"))
      (nle::send-steer-turn ws "steer-images" admission.turn-id
                            "[Image #1] and this?"
                            :command-id "si-steer-1"
                            :images '(("image/png" . "QUJD"))))
    (is (null (nle::turn-images admission.turn-id)))
    (is (nlk:steer-parked-p admission.turn) "the steer waits, parked on the running turn")
    (nlk:complete-turn admission.turn nil :steered t)
    (let ((next (nlk::dequeue-next-turn "steer-images")))
      (is-present next "promotion starts the steer as the next turn"
          (is (equal "[Image #1] and this?" (second next)) "with its text")
          (is (equal '(("image/png" . "QUJD")) (third next)))))))

;;; --- the prompt row Enter paints ---------------------------------------------
;;; The typed prompt is on screen the frame after Enter, before the loopback
;;; round trip: a live-only user row at the tail keyed by the start's command
;;; id. The durable input fact hydrates it in place; a rejection, a refusal, a
;;; queued start, or a retraction takes it back; a replay never rebuilds it.

(deftest enter-paints-the-prompt-before-any-frame ()
  (with-steer-shell (model starts cancels :submit "hello there")
    (let ((rows (%user-rows model))
          (command-id (second (first starts))))
      (is (= 1 (length rows)) "painted before any frame")
      (is-shape (first rows) (nle::transcript-row-text "hello there")
        (nle::transcript-row-prompt-id command-id) (nle::transcript-row-retention eq :live-only)
        (nle::transcript-row-hydrated not))
      (feed-wire model (%input-fact command-id "hello there"))
      (let ((after (%user-rows model)))
        (is (= 1 (length after)) "the fact hydrated the painted row, never a second one")
        (is (eq (first rows) (first after)) "the same row")
        (is-shape (first after) (nle::transcript-row-hydrated is) (nle::transcript-row-turn-id "t1")
          (nle::transcript-row-retention eq :persisted-history))))))

(deftest the-painted-prompt-sits-after-every-history-row (with-steer-shell (model starts cancels))
  (fold-model model
    (exchange "earlier" "answered" :turn-id "t0"))
  (nle::settle-tail-rows model.state)
  (type-string model "and now this" :enter)
  (let ((rows (nle::transcript-rows model.state)))
    (is (= 3 (length rows)))
    (is (equal "and now this" (nle::transcript-row-text (car (last rows)))))))

(deftest queued-start-turn-yields-the-painted-prompt-to-the-band ()
  (with-steer-shell (model starts cancels :submit "parked")
    (feed-wire model (%start-result (second (first starts)) "queued"
                                    "message" "turn queued"
                                    "response" (nlk:json-object "queue_depth" 1)))
    (is (null (%user-rows model)) "the follow-up band row is the queue's one projection")))

(deftest transport-refusal-drops-the-painted-prompt (with-temp-store ())
  ;; A link that fails in transit: the send signals, the notice says delivery
  ;; is uncertain in plain words -- the system's own when the failure has
  ;; them, never the condition's report -- and the row Enter painted no longer
  ;; stands for a prompt nobody holds.
  (with-recording-link (link)
    (loop for (fail words) in (list (list (lambda () (error "There is no applicable method for #<SEND>"))
                                          nil)
                                    (list (lambda () (sb-posix:mkdir "/no-such-dir-here/x" #o700))
                                          "(no such file or directory)"))
          do (with-stubbed-fdefinition (nle::gateway-link-send (ws text) (funcall fail))
               (let ((model (make-steer-model "refusal")))
                 (setf model.ws link)
                 (type-string model "into the void" :enter)
                 (is-carrying (said (nle::status-said model))
                   "delivery uncertain" "the link to the gateway failed while sending"
                   (:absent "applicable method") (:absent "#<"))
                 (when words (is (search words (nle::status-said model)) "the system's words ride along"))
                 (is (null (%user-rows model))))))))

(deftest replay-leaves-exactly-one-prompt-row ()
  ;; A reconnect rebuilds the transcript from durable frames: an admitted
  ;; start shows once, through its fact, whatever this shell painted before
  ;; the connection dropped.
  (with-steer-shell (model starts cancels :submit "resumed prompt")
    (let ((command-id (second (first starts))))
      (feed-wire model (resumed-frame
                        (list (fact-frame "turn.input_committed" "t1" :seq 1 :command-id command-id
                                          :payload (nlk:json-object "role" "user"
                                                                    "message" "resumed prompt"
                                                                    "disposition" "started")))
                        :sequence 7 :cursor 1 :admission "active"))
      (let ((rows (%user-rows model)))
        (is (= 1 (length rows)) "the durable fact is the one row")
        (is (nle::transcript-row-hydrated (first rows)))
        (is (equal "t1" model.active-turn-id))))))

(deftest replay-without-admission-leaves-no-prompt-row ()
  ;; The connection dropped before the gateway admitted the start: the
  ;; replay carries no fact for it, the painted row does not survive the
  ;; rebuild, and the re-sent start's own fact appends the row when it lands.
  (with-steer-shell (model starts cancels :submit "never landed")
    (feed-wire model (resumed-frame '()))
    (is (= 2 (length starts)) "the start was re-sent")
    (is (null (%user-rows model)))
    (feed-wire model (%input-fact (second (first starts)) "never landed" :seq 4))
    (is (= 1 (length (%user-rows model))))))

;;; --- the frame bound at the composer ---------------------------------------
;;; Without this, a paste over the gateway's frame bound would leave the
;;; bar running on a turn that never began. The shell measures before it
;;; writes, and a gateway refusal of the pending start brings the prompt home.

(defun oversized-image ()
  "One staged image whose base64 alone fills the sync frame bound."
  (list (cons "image/png" (make-string nlk:+max-sync-frame-bytes+
                                       :initial-element #\A
                                       :element-type 'base-char))))

(defun is-prompt-home (model text)
  "Assert TEXT is back in MODEL's composer, with no flight, pending start, stash or row for it."
  (is-shape model (composer-value text "the text comes back")
    (.turn-in-flight not "nothing is in flight") (%user-rows null "the optimistic row is gone")
    (.pending-command-id null "nothing is pending") (.retract-stash null "no stash to hand back")))

(deftest an-oversized-start-never-leaves-the-composer (with-recording-link (link))
  (with-temp-store ()
    (let ((model (make-steer-model "s")))
      (setf model.ws link
            model.pending-images (oversized-image))
      (type-string model "what is this" :enter)
      (is (= 1 (length model.pending-images)) "the image stays staged")
      (is-prompt-home model "what is this")
      (is-carrying (said (nle::status-said model)) "Prompt not sent" "exceeds the" "1 image staged" (:absent "not connected")))))

(deftest a-start-over-no-link-never-leaves-the-composer (with-temp-store ())
  ;; A shell has no link until its first attach, a dead boot leaves it none,
  ;; and a dropped one carries nothing it is handed: the start is refused in
  ;; words and comes home whole, with nothing armed for Esc or Ctrl+C to wait
  ;; on. Handed to the socket's send, it would read `There is no applicable
  ;; method ... (NIL ...)' over a spinner both keys left on `Cancelling', and
  ;; a dropped socket's send would take it in silence under the same spinner.
  (is (equal "this shell is not connected to its gateway"
             (refusal-text nle::gateway-not-connected (nle::send-start-turn nil "s" "hello"))))
  (let ((dropped (recording-link)))
    (nle::stop-gw-peer dropped)
    (dolist (link (list nil dropped))
      (let ((model (make-steer-model "s")))
        (setf model.ws link)
        (type-string model "hello there" :enter)
        (is-prompt-home model "hello there")
        (is-carrying (said (nle::status-said model)) "Prompt not sent" "not connected to its gateway" (:absent "NIL"))
        (is (null (nle::interrupt-turn model)) "Esc and Ctrl+C find no turn to stop")))
    (is (null (link-commands dropped)) "nothing went over the dropped link")))

(deftest an-oversized-steer-keeps-the-turn-and-the-composer (with-running-turn-model (model "s"))
  (setf model.pending-images (oversized-image))
  (type-string model "look here instead" :enter)
  (is (equal "look here instead" (composer-value model)))
  (is (nle::chat-model-turn-in-flight model) "the running turn is untouched")
  (is (null (find :pending-input (nle::transcript-rows model.state) :key #'nle::transcript-row-kind)))
  (is (search "Prompt not sent" (nle::status-said model))))

(deftest a-gateway-refusal-of-the-pending-start-brings-the-prompt-home ()
  (with-steer-shell (model starts cancels :submit "a paste the gateway refuses")
    (is-shape model (nle::chat-model-turn-in-flight is) (.pending-command-id is))
    ;; The tombstoned peer's diagnostic is not ours to act on: notice only.
    (feed-wire model (nlk::encode-sync-protocol-error
                      "peer_writer_failed" "peer writer failed" :permanent t))
    (is model.pending-command-id)
    (is (search "Gateway protocol error" (nle::status-said model)))
    (feed-wire model (nlk::encode-sync-protocol-error
                      "frame_too_large"
                      "a 17.0 MiB frame exceeds the 16 MiB sync frame bound"))
    (is-prompt-home model "a paste the gateway refuses")
    (is-carrying (said (nle::status-said model)) "gateway refused the prompt" "16 MiB")
    (is (null cancels) "nothing to cancel: the turn never existed")))
