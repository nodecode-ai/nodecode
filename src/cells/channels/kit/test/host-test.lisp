;;;; host-test.lisp --- the digest flow through the host, over a test platform.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The delivery half of the host without threads-of-uncertainty: folds run
;;;; on the test thread (as the :FRAME hook would), the status flush is
;;;; driven directly, and only the terminal finish rides the real worker.
;;;; Platform-neutral by construction — the plans are the test platform's.

(in-package #:nodecode.test)

(defmacro with-digest-host ((host lane &key responses (workers 1) (name "digest-flow")
                             (session "chat-123-m1") (trigger "m1") host-keys)
                            &body body)
  "A host with one interned lane for SESSION — LANE NIL interns none — and a
live delivery pool, torn down after BODY."
  ;; EXECUTOR is bound around it as the
  ;; recording executor, HOST-KEYS ride into TEST-HOST, and BODY gets the
  ;; moves every digest test makes: (FACT KIND PAYLOAD [TURN-ID]), one fact
  ;; folded into SESSION's turn, (SAID TEXT [TURN-ID]), the round that says TEXT,
  ;; (CALLED CALL-ID [ARGUMENTS]), the turn's eval call CALL-ID starting,
  ;; (EARN-LINE [TURN-ID]), a started turn's eval call and its status line flushed,
  ;; (PLANS), the plans the executor has recorded so far, (LABELLED LABEL), those
  ;; under the audit LABEL, (POSTED N), whether it has recorded N of them within ten
  ;; seconds, (SAY TEXT USER MESSAGE), USER typing TEXT as MESSAGE in room 123,
  ;; (PRESS ID VERB USER), USER pressing SESSION's VERB control as interaction ID,
  ;; and (RECAP SAID [:ANNOUNCE] [:TURN-ID]), an experience recap folded back as a
  ;; recorded exchange, ANNOUNCE or not, whose round says SAID.
  `(let* ((executor (nck:make-recording-executor :responses ,responses))
          (,host (test-host :executor executor :name ,name :workers ,workers ,@host-keys)))
     (nlk:with-cleanup ((nck:stop-delivery-worker (nck:host-worker ,host)))
       (let (,@(when lane
                 `((,lane (nck:intern-lane (nck:host-lanes ,host) ,session
                                           :target '(:channel-id "123")
                                           :trigger-message-id ,trigger)))))
         ,@(when lane `((declare (ignorable ,lane))))
         (labels ((fact (kind payload &optional (turn-id "t1"))
                    (fold-fact ,host ,session kind payload turn-id))
                  (said (text &optional (turn-id "t1"))
                    (fact "turn.assistant_message_completed"
                          (nlk:json-object "message" (nlk:json-object "content" text)) turn-id))
                  (called (call-id &optional arguments)
                    (fact "turn.tool_call_started"
                          (nlk:json-object "tool-name" "eval" "call-id" call-id
                                           :opt "arguments" arguments)))
                  (earn-line (&optional (turn-id "t1"))
                    (fact "turn.started" (nlk:json-object) turn-id)
                    (fact "turn.tool_call_started" (nlk:json-object "tool-name" "eval") turn-id)
                    (nck:flush-now ,host ,lane))
                  (plans () (nck:recording-executor-plans executor))
                  (labelled (label) (plans-matching (plans) :label label))
                  (posted (n)
                    (await (:timeout 10) (>= (length (plans)) n)))
                  (say (text user message)
                    (nck:handle-candidate ,host (test-candidate :channel "123" :message message
                                                                :text text :user user
                                                                :user-name user)))
                  (press (id verb user)
                    (nck:control-pressed ,host (list :id id :user-id user :message-id "s1"
                                                     :data (format nil "nck:~a:~a" verb ,session)
                                                     :channel-id "123")))
                  (recap (text &key announce (turn-id "t1"))
                    (let ((input (format nil "[experience] recap of the turn above, ~
                                              by a reflection of this session")))
                      (fact "turn.input_committed"
                            (nlk:json-object "role" "user" "message" input "disposition" "recorded"
                                             :when announce "announce" t)
                            turn-id)
                      (fact "turn.started" (nlk:json-object "input" input) turn-id)
                      (said text turn-id))))
           (declare (ignorable #'fact #'said #'called #'earn-line #'plans #'labelled #'posted
                               #'say #'press #'recap))
           ,@body)))))

(deftest channel-host-digest-flow (with-digest-host (host lane :responses (replies "s1" "s1" "a1" "s1")))
  (fact "turn.started" (nlk:json-object))
  (called "c1" "{\"form\": \"(sh \\\"just lint\\\")\"}")
  (said "probing the registry")
  (is (null (plans)) "hook-thread folds perform no I/O")
  (nck:flush-now host lane)
  (is-present (post (first (plans))) "the first flush posts the card"
    (is-plan post :label "send_message" :path "/rooms/123/messages" "reply_to" "m1" "ping" nil)
    (is-shape (gethash "card" (nck:request-plan-body post))
      (:state :working) (:headline "Running just lint"))
    (is-carrying (content (plan-content post))
      "1 step" (:absent "round" "the card counts steps, never rounds")
      (:absent "probing the registry" "a round's words leave as their own message")))
  ;; The thought that chose the work lands on the card, as its headline.
  (fold-delta host "chat-123-m1" "Weighing the lint output. Then")
  (nck:flush-now host lane)
  (is-present (edit (second (plans))) "the second flush edits the card in place"
    (is-plan edit :label "edit_message" :method "PATCH" :path "/rooms/123/messages/s1")
    (is (search "thinking · Weighing the lint output." (plan-content edit))))
  (said "the final answer")
  (fact "turn.completed" (nlk:json-object))
  (is (posted 4) "the finish job posts the answer and settles the card")
  (let ((all (plans)))
    (is-present (final (third all)) "the answer is a fresh message, not an edit of the card"
      (is-plan final :label "send_message" "reply_to" "m1" "ping" t)
      (is (equal "the final answer" (plan-content final)) "its words alone: the card says the work"))
    (is-present (settle (fourth all))
      "and the card settles above it as the turn's record — never deleted"
      (is-plan settle :label "edit_message" :method "PATCH" :path "/rooms/123/messages/s1")
      (is-shape (gethash "card" (nck:request-plan-body settle)) (:state :done))
      (is (uiop:string-prefix-p "Done in " (plan-content settle))))
    (is (= 4 (length all)))
    (is (eq lane (nck:lane-for-address (nck:host-lanes host) "a1")))))

(deftest channel-host-a-waiting-turn-keeps-the-ask-open ()
  ;; The turn ended with its own background evaluation still in flight: the
  ;; model's word was status, not the ask's answer. The card the ask watches
  ;; stays up saying what the wait is, the word on it, nothing is delivered
  ;; as the answer — and the exit wake's turn takes the same card over and
  ;; answers for real.
  (with-digest-host (host lane :responses (replies "s1" "a1" "a2"))
    (fact "turn.started" (nlk:json-object "input" "link me the report"))
    (called "c1")
    (nck:flush-now host lane)
    (is-present (line (first (plans))) "the ask's line is up while the turn works"
      (is (equal "send_message" (nck:request-plan-audit-label line))))
    (said "On it — digging through it.")
    (fact "turn.completed" (nlk:json-object "background-pending" 1))
    ;; Its word rides the card it watches, never a message of its own.
    (is (await (:timeout 10) (equal "On it — digging through it."
                                    (getf (nck:digest-card (nck:lane-digest lane) 9999999999) :said))))
    (is (= 1 (length (labelled "send_message"))))
    (is (null (plan-matching (plans) :label "delete_message")))
    (is (eql 1 (nck:turn-digest-background-pending (nck:lane-digest lane))))
    (is (search "Waiting on a background evaluation"
                (nck:card-text (nck:digest-card (nck:lane-digest lane) 9999999999))))
    ;; The exit wake's turn: same lane, the same ask's line.
    (fact "turn.started"
          (nlk:json-object "input" "[background eval 7 exited 0] done")
          "t2")
    (is (equal "t2" (nck:turn-digest-turn-id (nck:lane-digest lane))))
    (is (null (nck:turn-digest-background-pending (nck:lane-digest lane))))
    (is (equal "link me the report" (nck:lane-prompt lane)))
    (said "Here it is — the report." "t2")
    (fact "turn.completed" (nlk:json-object) "t2")
    (is-present (answer (await-plan executor :ping t)) "the answer posts, alone"
      (is-plan answer "reply_to" "m1" "content" "Here it is — the report."))
    ;; The card the ask watched settles as the turn's record.
    (is (await-plan executor :label "edit_message" :content "Done in"))))

(deftest channel-host-an-exchange-records-what-was-said-while-it-waited ()
  ;; The room's record of an ask names what the model said while the turn
  ;; waited on its own background work, then the answer — the turn that ends
  ;; the work would otherwise record an exchange missing everything spoken
  ;; before the result arrived.
  (let ((digest (nck::make-turn-digest :turn-id "t1" :started-at-ms 0)))
    (nck::digest-note-waiting-words digest "On it — digging.")
    (is (equal (format nil "On it — digging.~%~%Here it is.")
               (nck:recorded-answer-text digest "Here it is.")))
    (is (null (nck::turn-digest-waiting-words digest)))))

(deftest channel-host-what-a-turn-says-stays-on-its-card ()
  ;; A round that spoke and kept working: its words show on the card, and
  ;; never as a message of their own; the answer that ends the turn is the
  ;; one message, with nothing above it to be set apart from.
  (with-digest-host (host lane :responses (replies "s1" "a1"))
    (fact "turn.started" (nlk:json-object))
    (fact "turn.assistant_message_completed"
          (nlk:json-object
           "message" (nlk:json-object
                      "content" "the registry answers first"
                      "tool_calls" (vector (nlk:json-object "id" "call-1")))))
    (fact "turn.tool_call_started" (nlk:json-object "tool-name" "eval"))
    (nck:flush-now host lane)
    (is-present (card (await-plan executor :label "send_message")) "the card posts, the words on it"
      (is (equal "the registry answers first" (getf (plan-field card "card") :said))))
    (said "the registry is fine")
    (fact "turn.completed" (nlk:json-object))
    (is-present (answer (await-plan executor :ping t)) "the answer posts"
      (is (equal "the registry is fine" (plan-content answer))))
    (is (= 2 (length (labelled "send_message"))) "the card and the answer, nothing else")))

(defmacro with-streaming-host ((host lane &key responses) &body body)
  "WITH-DIGEST-HOST whose section streams (channels.<id>.stream), its turn
started nine seconds ago, past the eight a writing turn waits to earn its
card; BODY also gets (WRITES TEXT), one text delta."
  `(with-digest-host (,host ,lane :responses ,responses :name "streaming")
     (setf (nck::host-stream-p ,host) t)
     (fact "turn.started" (nlk:json-object))
     (setf (nck::turn-digest-started-at-ms (nck:lane-digest ,lane)) (- (nck:now-ms) 9000))
     (flet ((writes (text) (fold-delta ,host "chat-123-m1" text :type "text")))
       ,@body)))

(deftest channel-host-a-streamed-answer-posts-fresh ()
  ;; channels.<id>.stream, on unless a section says otherwise: the words a
  ;; round writes show on its card as they come, the cursor at their end; the
  ;; answer then posts fresh, pinging its asker, and leaves the card. No
  ;; message carries words on their way. A turn that fails says so on its card.
  (is (nck::host-stream-p (nck:make-host-from-section (nlk:make-json-object) (test-platform))))
  (is (not (nck::host-stream-p (nck:make-host-from-section (nlk:make-json-object "stream" nil)
                                                           (test-platform)))))
  (with-streaming-host (host lane :responses (replies "s1" "a1"))
    (writes "The registry answers on port 80, ")
    (nck::schedule-flush host lane)
    (is-present (card (await-plan executor :label "send_message")) "a writing turn earns its card"
      (is-plan card "reply_to" "m1" "ping" nil)
      (is (equal (writing-cursor "The registry answers on port 80,") (getf (plan-field card "card") :said))))
    (said "The registry answers on port 80, and on 443.")
    (fact "turn.completed" (nlk:json-object))
    (is-present (answer (await-plan executor :ping t)) "the answer posts fresh"
      (is-plan answer "reply_to" "m1" "content" "The registry answers on port 80, and on 443."))
    (is-present (settle (await-plan executor :label "edit_message" :content "Done in")) "the card settles"
      (is (null (getf (plan-field settle "card") :said)) "the answer's words left it"))
    (is (null (labelled "delete_message")) "no message carried words on their way"))
  (with-streaming-host (host lane :responses (replies "s1"))
    (writes "The registry answers on port 80, and then")
    (nck::schedule-flush host lane)
    (is (await-plan executor :label "send_message"))
    (fact "turn.failed" (nlk:json-object "detail" "provider 502"))
    (is (await-plan executor :content "> provider 502"))
    (is (null (labelled "delete_message")))))

(deftest channel-host-an-answer-is-said-in-a-voice-message-as-the-room-asks ()
  ;; channels.<id>.voice_replies: off, the default, an answer is words alone;
  ;; on, an ask said in a voice message is answered in one too; tts, every ask
  ;; is. An ask said in a voice channel is answered out loud there, not twice.
  ;; The voice message follows the answer, and a voice that cannot speak costs
  ;; one line saying why, never the answer.
  (is-table (mode voice due) (eq due (nck::voice-reply-due-p mode voice))
    ("off" :note nil) ("on" nil nil) ("on" :note t) ("tts" nil t) ("tts" :channel nil))
  (is (equal "off" (nck::host-voice-replies
                    (nck:make-host-from-section (nlk:make-json-object) (test-platform)))))
  (let ((speech nck:*speech*))
    (nlk:with-cleanup ((setf nck:*speech* speech))
      ;; SETF, not LET: the voice message is made on the delivery pool's thread.
      (setf nck:*speech* (nlk:make-json-object "enabled" nil))
      (with-digest-host (host lane :name "voice-replies"
                                   :host-keys (:platform (test-platform :files t)))
        (setf (nck::host-voice-replies host) "tts")
        (fact "turn.started" (nlk:json-object))
        (said "The registry answers on port 80.")
        (fact "turn.completed" (nlk:json-object))
        (let ((notice (await-plan executor :content "no voice message (speech is turned off")))
          (is-present notice "the room hears why there is no voice message"
            (is-plan notice "ping" nil "reply_to" nil)))
        (is (< (position-if (lambda (plan) (plan-matching-p plan :content "port 80")) (plans))
               (position-if (lambda (plan) (plan-matching-p plan :content "no voice message"))
                            (plans))))))))

(deftest channel-host-streamed-words-a-round-said-stay-on-its-card ()
  ;; A round that wrote its words and called a tool: they stay on the card as
  ;; what it said, the cursor gone, and the answer posts alone.
  (with-streaming-host (host lane :responses (replies "s1" "a1"))
    (writes "Looking at the registry before I answer.")
    (nck::schedule-flush host lane)
    (is (await-plan executor :label "send_message"))
    (fact "turn.assistant_message_completed"
          (nlk:json-object
           "message" (nlk:json-object "content" "Looking at the registry before I answer."
                                      "tool_calls" (vector (nlk:json-object "id" "call-1")))))
    (is (await (:timeout 10)
          (progn (nck::schedule-flush host lane)
                 (find "Looking at the registry before I answer." (labelled "edit_message")
                       :key (lambda (plan) (getf (plan-field plan "card") :said)) :test #'equal))))
    (said "the registry is fine")
    (fact "turn.completed" (nlk:json-object))
    (is-present (answer (await-plan executor :ping t)) "the answer posts"
      (is (equal "the registry is fine" (plan-content answer))))
    (is (= 2 (length (labelled "send_message"))) "the card and the answer")))

(deftest channel-host-a-call-is-a-step-not-a-message ()
  ;; A call posts no message of its own: it is a step on the card, and the
  ;; room carries the turn's own words. The room-wide trail this replaced
  ;; posted every call for everyone.
  (with-digest-host (host lane :responses (replies "s1" "s1" "s1"))
    (fact "turn.started" (nlk:json-object))
    (called "c1" "{\"form\": \"(sh \\\"just lint\\\")\"}")
    (nck::schedule-flush host lane)
    (is (posted 1) "the card goes out")
    (sleep 0.3)
    (is (= 1 (length (plans))) "and no call message follows")
    (is-carrying (content (plan-content (first (plans))))
      (:= (format nil "Running just lint · 0s~%› Running just lint · 0s~%1 step")))))

(deftest channel-host-a-recorded-exchange-is-silent-unless-announced ()
  ;; A recap folded back by the experience cell is a RECORDED exchange: a
  ;; settled turn nobody asked. The lane answers asks, so it plans no chrome
  ;; and posts nothing — while an ANNOUNCED one ends in a single silent note.
  ;; The room write-back needs a store and is the experience suite's; here the
  ;; fold's whole visible contract is the silence and that one note.
  (with-digest-host (host lane :responses (replies "n1"))
    (recap "asked: a thing")
    (nck:flush-now host lane)
    (is (null (plans)) "a recorded exchange plans no chrome")
    (is (null (nck:lane-digest lane)) "and never opens a digest")
    (is (null (nck:lane-active-turn-id lane)) "nor counts as a running turn")
    (fact "turn.completed" (nlk:json-object))
    (nck:flush-now host lane)
    (is (null (plans)) "and settles with nothing posted")
    (is (null (gethash lane nck::*recorded-turns*)) "the fold's entry is gone")
    ;; Announced: the same flow ends in one note that @s the operator.
    (recap "it broke" :announce t :turn-id "t2")
    (fact "turn.completed" (nlk:json-object) "t2")
    (is (posted 1) "the announced recap posts its one note")
    (is-present (note (first (plans))) "one message"
      (is-plan note :label "send_message" "ping" nil "reply_to" nil)
      (is (equalp #("mike") (gethash "mentions" (nck:request-plan-body note))))
      (is (equal (format nil "~a~%~%~a" nck::+note-head+ "it broke")
                 (plan-content note))))
    (is (= 1 (length (plans))) "nothing beside the note")
    (is (eq lane (nck:lane-for-address (nck:host-lanes host) "n1")))))

(deftest channel-host-an-undeclared-room-notes-without-an-address ()
  ;; A room that declares no operator: the note posts, and mentions nobody.
  (with-digest-host (host lane :host-keys (:owners nil) :responses (replies "n1"))
    (recap "it broke" :announce t :turn-id "t2")
    (fact "turn.completed" (nlk:json-object) "t2")
    (is (posted 1) "the announced recap posts its one note")
    (is (null (gethash "mentions" (nck:request-plan-body (first (plans))))))))

(deftest channel-host-parked-input-shows-on-the-lane-line ()
  ;; A steer parked on the lane's session while its turn runs is a ⌎ row
  ;; under the running status line — the same queue snapshot the TUI folds
  ;; into its pending band — and its promotion clears the row. A snapshot
  ;; for a turn this lane is not running is dropped.
  (with-digest-host (host lane :responses (replies "s1" "s1"))
    (flet ((parked (turn-id &rest prompts)
             (fact "session_input_queue_snapshot_updated"
                   (nlk:json-object "snapshot" (nlk:json-object "pending_prompts"
                                                                (coerce prompts 'vector)))
                   turn-id)))
      (fact "turn.started" (nlk:json-object))
      (nck:flush-now host lane)
      (is (null (plans)) "a quiet turn has no line yet")
      (parked "t1" (nlk:json-object "prompt_id" "p1"
                                    "content" "mila [m2 u8]: also run the tests"
                                    "disposition" "steer"))
      (is (null (plans)) "the fold performs no I/O")
      (nck:flush-now host lane)
      (is-present (post (first (plans))) "the parked steer opens the line at once"
        (is (equal "send_message" (nck:request-plan-audit-label post)))
        (let ((content (plan-content post)))
          (is (search (format nil "~%⌎ also run the tests — after this round")
                      content))
          (is (uiop:string-prefix-p "Working · " content))))
      (parked "t-other" (nlk:json-object "prompt_id" "p9"
                                         "content" "someone else's"
                                         "disposition" "steer"))
      (is (equal 1 (length (nck:turn-digest-pending (nck:lane-digest lane)))))
      (parked "t1")
      (nck:flush-now host lane)
      (is-present (edit (second (plans)))
        "the promotion's empty snapshot takes the row off the line"
        (is (equal "edit_message" (nck:request-plan-audit-label edit)))
        (is-carrying (content (plan-content edit)) (:absent "⌎" "no row is left")
          (is (uiop:string-prefix-p "Working · " content)))))))

(defun room-1-ask (message)
  "The ask MESSAGE from u8 opens in room 1, as its lane chat-1-MESSAGE."
  (nck:make-ask :room "chat-1" :lane (format nil "chat-1-~a" message) :owner-id "u8"
                :message-id message :target '(:channel-id "1") :prompt "hi"))

(deftest channel-host-reactions-follow-the-ask ()
  ;; Opt-in: the ask carries the eye on the message itself from admission
  ;; through the working turn, and the check once its answer has landed.
  ;; Off, nothing is ever planned.
  (with-digest-host (host nil :responses (replies "s1" "ok" "ok" "a1" "ok" "ok")
                     :name "reactions" :session "chat-1-m7"
                     :host-keys (:max-concurrent 0 :reactions t))
    ;; No slot at all: the ask queues, and is marked seen at once.
    (nck::gate-ask host (room-1-ask "m7"))
    (is (posted 2) "the queued line and the reaction both go out")
    (is-present (seen (first (labelled "reaction"))) "the ask is marked seen"
      (is-plan seen :method "PUT" :path "/rooms/1/messages/m7/reaction"
                    "emoji" nck:+reaction-seen+))
    (is-present (queued (first (labelled "send_message")))
      "the queued line is a silent reply to the ask"
      (is-plan queued "reply_to" "m7" "ping" nil "content" "Queued"))
    ;; The gate's book still lists the ask as waiting; this test
    ;; drives the turn's facts itself (the kernel is not here), so
    ;; the entry is taken off by hand — else the finish job's
    ;; promotion would try to admit it for real.
    (nck:release-slot host "u8")
    (let ((lane (nck:find-lane (nck:host-lanes host) "chat-1-m7")))
      (fact "turn.started" (nlk:json-object))
      (is (not (await (:timeout 0.3) (>= (length (labelled "reaction")) 2))))
      (is (equal "m7" (nck:turn-digest-ask-id (nck:lane-digest lane))))
      (said "done")
      (fact "turn.completed" (nlk:json-object))
      (is (await (:timeout 10) (>= (length (labelled "reaction")) 3)))
      (is-plan (second (labelled "reaction")) :method "DELETE" "emoji" nck:+reaction-seen+)
      (is-plan (third (labelled "reaction")) :method "PUT" "emoji" nck:+reaction-done+)
      (is (null (nck:lane-reactions lane)) "the outcome is no turn's to clear")))
  ;; Off by default: the same admission plans no reaction.
  (with-digest-host (host nil :responses (replies "s1") :name "no-reactions"
                     :host-keys (:max-concurrent 0))
    (nck::gate-ask host (room-1-ask "m8"))
    (is (posted 1))
    (sleep 0.2)
    (is (every (lambda (plan)
                 (equal "send_message" (nck:request-plan-audit-label plan)))
               (plans)))))

(defun test-reaction (&key (channel "123") (on "a1") (user "u9") (user-name "alice")
                           (emoji "❤️") (action "add"))
  "One reaction candidate as an adapter's route normalizes it: the emoji as
its text, the message it sits on as its reply gesture, no message id of its
own — a reaction is not a message — and addressed, having been left on a
message of ours."
  (test-candidate :channel channel :message nil :reply-to on :user user :user-name user-name
                  :text emoji :reaction action :addressed t))

(deftest channel-host-a-reaction-on-our-message-notices-never-answers ()
  ;; A reaction is a line the room said without typing one — and a notice,
  ;; never an actionable trigger: it opens no turn and steers none, whoever
  ;; left it and whether it was added or taken back. Turns start from
  ;; words, never from a gesture. Every accepted one becomes one line of
  ;; chatter the room's next ask carries. The emoji itself is never read
  ;; here: what a room means by one is the model's to read, not a table's
  ;; to decide. Only ever on a message the kit posted and still holds an
  ;; address for.
  (with-digest-host (host lane :name "reaction-inbound" :session "chat-123-m1"
                     :host-keys (:max-concurrent 0 :owners '("mike")))
    (setf (nck:host-policy host) (channel-policy :allowed-channels '("123"))
          (nck:lane-owner-id lane) "u8")
    ;; a1 is a message this lane posted: the address book is what says "mine".
    (nck:bind-lane-address (nck:host-lanes host) "a1" "chat-123-m1")
    (is-each (nck:reaction-noticed host)
      ;; An operator's reaction is chatter like anyone's: no turn, no queue.
      ((test-reaction :user "mike" :user-name "mike" :emoji "✅") :observe "the operator's")
      ;; The lane's own asker's reaction is chatter too.
      ((test-reaction :user "u8" :user-name "owner" :emoji "🎉") :observe "the asker's")
      ;; Anybody else's is chatter: no turn, no post, no ping.
      ((test-reaction) :observe "anybody else's")
      ;; So is one the operator takes back — a turn already run cannot be unrun.
      ((test-reaction :user "mike" :user-name "mike" :emoji "✅" :action "remove") :observe
       "the operator's, taken back"))
    (is (null (nck:release-slot host "mike")) "no ask was queued")
    (is (null (nck:release-slot host "u8")) "no ask was queued for the asker either")
    (let ((ambient (nck:drain-ambient host "chat-123")))
      (is-shape ambient (length = 4 "one line each, and nothing was posted")
        (first "mike (operator) [umike ra1]: reacted ✅") (second "owner [uu8 ra1]: reacted 🎉")
        (third "alice [uu9 ra1]: reacted ❤️") (fourth "mike (operator) [umike ra1]: took back ✅")))
    (is (null (plans)) "no gesture ever posted a message")
    ;; A reaction on a message no lane of ours holds is not ours to read.
    (is (null (nck:reaction-noticed host (test-reaction :on "someone-elses"))))
    ;; And the room's own read gates still answer first.
    (setf (nck:host-policy host) (channel-policy :allowed-channels '("999")))
    (is (eq :reject (nck:reaction-noticed host (test-reaction))))
    (is (equal "channel_not_allowed"
               (getf (nck:channel-status "test") :last-rejection)))))

(deftest channel-host-terminal-clears-the-completed-turn-reactions ()
  ;; A steer changes the lane's current ask before the old turn's terminal
  ;; delivery runs. Clear the old turn's marker while preserving the queued
  ;; follow-up's fresh marker for its own turn.
  (with-digest-host (host lane :name "reaction-generations" :host-keys (:reactions t))
    (fact "turn.started" (nlk:json-object))
    (is (await (:timeout 10) (= 1 (length (plans)))))
    (setf (nck:lane-trigger-message-id lane) "m2")
    (nck::schedule-reaction host lane "m2" nck:+reaction-seen+)
    (is (await (:timeout 10) (= 2 (length (plans)))))
    (fact "turn.completed" (nlk:json-object))
    (is (await (:timeout 10) (= 3 (length (plans)))))
    (is-plan (third (plans)) :method "DELETE" :path "/rooms/123/messages/m1/reaction")
    (is (equal '(("m2" . "👀")) (nck:lane-reactions lane)))))

(deftest channel-host-reactions-say-the-outcome ()
  ;; A failed turn leaves the cross; a stopped one leaves the message bare.
  (loop for (kind payload outcome) in `(("turn.failed" ,(nlk:json-object "detail" "provider 502")
                                                       ,nck:+reaction-failed+)
                                        ("turn.cancelled" ,(nlk:json-object "reason" "stop") nil))
        do (with-digest-host (host lane :name "reaction-outcome" :host-keys (:reactions t))
             (fact "turn.started" (nlk:json-object))
             (is (await (:timeout 10) (labelled "reaction")))
             (fact kind payload)
             (is (await (:timeout 10) (>= (length (labelled "reaction")) (if outcome 3 2))))
             (sleep 0.1)
             (is (equal (if outcome
                            (list "PUT" "DELETE" "PUT")
                            (list "PUT" "DELETE"))
                        (mapcar #'nck:request-plan-method (labelled "reaction")))
                 kind)
             (when outcome
               (is-plan (third (labelled "reaction")) :path "/rooms/123/messages/m1/reaction"
                                                      "emoji" outcome)))))

(deftest channel-host-an-answer-the-room-refused-says-so ()
  ;; Without this, a refused answer leaves the turn's working line up with
  ;; its stop under a check on the ask, and the asker never learns the
  ;; answer was lost. The card settles failed into the refusal and loses
  ;; its stop — no delete, it is the one word the room has, and its steps
  ;; stay with Details on it — and the ask takes the cross.
  (with-digest-host (host lane :name "answer-refused"
                     :responses (list (reply 200 "id" "s1") (reply 400 "message" "Invalid Form Body"))
                     :host-keys (:platform (test-platform :choices t)))
    (earn-line)
    (said "the final answer")
    (fact "turn.completed" (nlk:json-object))
    (is-present (settle (await-plan executor :label "edit_message"
                                             :content "The answer could not be posted"))
      "the card says the answer did not post"
      (is-plan settle :path "/rooms/123/messages/s1")
      (is (eq :failed (getf (plan-field settle "card") :state)) "a failed card")
      (is (search "status 400" (getf (plan-field settle "card") :note)) "and why")
      (is (equal '("Details") (mapcar #'first (plan-field settle "controls"))) "its stop goes, Details stays"))
    (is (null (labelled "delete_message")))
    (is (null (nck::turn-digest-status-id (nck:lane-digest lane))) "the lane holds no line"))
  ;; A turn that earned no line says it in one, a silent reply to the ask.
  (with-digest-host (host lane :name "answer-refused-bare" :host-keys (:reactions t)
                     :responses (list (reply 200) (reply 400 "message" "Invalid Form Body")))
    (fact "turn.started" (nlk:json-object))
    (is (await (:timeout 10) (labelled "reaction")))
    (said "the final answer")
    (fact "turn.completed" (nlk:json-object))
    (is (await (:timeout 10) (= 3 (length (labelled "reaction")))))
    (is-present (notice (plan-matching (labelled "send_message")
                                       :content "the answer could not be posted"))
      "one line says the answer did not post"
      (is-plan notice "reply_to" "m1" "ping" nil))
    (is-plan (third (labelled "reaction")) :method "PUT" "emoji" nck:+reaction-failed+)))

(deftest channel-host-an-answer-whose-post-signals-says-so ()
  ;; 2026-10-19: an answer carrying a new choice died inside its post (the
  ;; choice's press could not be kept past the session state quota) and only
  ;; the worker's log said so: the card read working, the ask kept its eye,
  ;; the room heard nothing. A post that signals says so as a refusal does.
  (with-digest-host (host lane :name "answer-signals"
                     :responses (list (reply 200 "id" "s1") (reply 200))
                     :host-keys (:platform (test-platform :choices t)))
    (with-stubbed-fdefinition (nck::answer-controls (host lane choices)
                                (declare (ignore host lane choices))
                                (error "session state: session state quota exceeded"))
      (earn-line)
      (said "the final answer")
      (fact "turn.completed" (nlk:json-object))
      (is-present (settle (await-plan executor :label "edit_message"
                                               :content "The answer could not be posted"))
        "the card says the answer did not post"
        (is (eq :failed (getf (plan-field settle "card") :state)) "a failed card")
        (is (search "quota exceeded" (getf (plan-field settle "card") :note)) "and why")))))

(deftest channel-host-a-steered-turn-hands-its-marks-on ()
  ;; A steer cuts the turn short and the turn it opens answers both lines:
  ;; the first ask's eye holds through the handover, and the answer's check
  ;; lands on both messages.
  (with-digest-host (host lane :name "reaction-steer" :host-keys (:reactions t))
    (fact "turn.started" (nlk:json-object))
    (is (await (:timeout 10) (= 1 (length (labelled "reaction")))))
    (setf (nck:lane-trigger-message-id lane) "m2")
    (nck::schedule-reaction host lane "m2" nck:+reaction-seen+)
    (is (await (:timeout 10) (= 2 (length (labelled "reaction")))))
    (fact "turn.completed" (nlk:json-object "steered" t))
    ;; The cut turn touches neither mark.
    (is (not (await (:timeout 0.5) (> (length (labelled "reaction")) 2))))
    (fact "turn.started" (nlk:json-object) "t2")
    (said "both, answered" "t2")
    (fact "turn.completed" (nlk:json-object) "t2")
    (is (await (:timeout 10) (= 6 (length (labelled "reaction")))))
    (is (equal '("m1" "m2")
               (sort (loop for plan in (nthcdr 2 (labelled "reaction"))
                           when (equal nck:+reaction-done+ (plan-field plan "emoji"))
                             collect (ppcre:scan-to-strings "m[0-9]+" (nck:request-plan-path plan)))
                     #'string<)))
    (is (null (nck:lane-reactions lane)))))

(deftest channel-host-failure-settles-into-the-chrome ()
  ;; A failed turn is the one case where the line IS the message: nothing is
  ;; posted beside it — and its ask stays in the room's record with the notice.
  (with-digest-host (host lane :responses (replies "s1"))
    (fact "turn.started" (nlk:json-object))
    (fact "turn.failed" (nlk:json-object "detail" "provider 502"))
    (is (await (:timeout 10) (plans)))
    (is (= 1 (length (plans))))
    (is-present (notice (first (plans))) "the notice posts"
      (is (equal "send_message" (nck:request-plan-audit-label notice)))
      (is-carrying (content (plan-content notice)) "Failed at" "> provider 502"))))

(deftest channel-host-a-cancelled-turn-keeps-its-ask-in-the-room (with-temp-store ())
  ;; A failed or cancelled turn is its status line — and the ask it was given
  ;; stays in the room's record with the notice as the assistant half: an ask
  ;; that was stopped is still something the room said, so the next lane
  ;; forked above sees it.
  (with-digest-host (host lane :responses (replies "s1") :name "cancel-record")
    (setf (nck:lane-parent-session-id lane) "chat-123")
    (nlk:create-session :id "chat-123")
    (nlk:record-exchange-turn "chat-123" "bob [m3 u2]: is the deploy up" "yes")
    (fact "turn.started" (nlk:json-object "input" "alice [m1 u2]: yo"))
    (fact "turn.cancelled" (nlk:json-object "reason" "stopped from the room"))
    (is (await (:timeout 10) (>= (count-session-events "chat-123" "turn.completed") 2)))
    (is (= 2 (count-session-events "chat-123" "turn.started")))))

(deftest channel-host-reconciles-a-missed-terminal-frame (with-temp-store ())
  ;; A durable terminal fact can land after the live frame is missed. The tick
  ;; must close the running digest and let the normal terminal delivery settle
  ;; its captured chrome instead of leaving a permanent working line.
  (with-digest-host (host lane :responses (replies "s1"))
    (let* ((session-id "chat-123-m1")
           (turn (progn
                   (ensure-durable-session session-id)
                   (nlk:admit-turn session-id "command-terminal-recovery"
                                  "the question")))
           (turn-id turn.turn-id))
      ;; Fold only the opening and the status-worthy tool fact into the
      ;; channel. Deliberately omit the terminal frame below.
      (earn-line turn-id)
      (nlk:complete-turn turn nil)
      (is (eq :running (nck:turn-digest-phase (nck:lane-digest lane))))
      (is (nck::reconcile-terminal-digest host lane))
      (is (await () (>= (length (plans)) 2)))
      (is (equal turn-id (nck:turn-digest-turn-id (nck:lane-digest lane))))
      (is (eq :completed (nck:turn-digest-phase (nck:lane-digest lane))))
      (is (equal "delete_message" (nck:request-plan-audit-label (second (plans))))))))

(deftests channel-host (spellings)
    (dolist (spelling spellings)
      (with-digest-host (host lane :responses (replies "s1"))
            (earn-line)
        (when spelling (said (format nil " ~%~a~% " spelling)))
        (fact "turn.completed" (nlk:json-object))
        (is (posted 2) "a completion with no answer to post deletes its existing line")
        (is (= 1 (length (labelled "send_message"))) (format nil "~s posts no answer" spelling))
        (is-plan (second (plans))
                 :label "delete_message" :method "DELETE" :path "/rooms/123/messages/s1")
        (is (= 2 (length (plans))))
        (is-shape (nck:lane-digest lane) (nck:turn-digest-status-id null)
          (nck:turn-digest-answer null))))
  ;; A completed turn whose live digest missed every assistant fact must
  ;; remove its temporary working line without leaving a failure tombstone.
  (completed-without-answer-settles-chrome '(nil))
  ;; SILENT is a control value, not a Discord reply: a turn that already
  ;; earned chrome deletes that line and emits no answer message. The no-post
  ;; sentinel is not one spelling: SILENT, [SILENT], NO_REPLY and NO REPLY all
  ;; settle the turn quietly — trimmed and case-insensitive — with no answer
  ;; post.
  (silence-spellings-all-settle-quietly '("SILENT" "[SILENT]" "[silent]" "No_Reply" "no reply")))

(deftest channel-host-completed-without-answer-is-silent-without-chrome ()
  ;; A quick no-response turn never earns chrome and must remain completely
  ;; absent from the platform surface.
  (with-digest-host (host lane :responses nil)
    (fact "turn.started" (nlk:json-object))
    (fact "turn.completed" (nlk:json-object))
    (is (await (:timeout 10) (eq :completed
                                  (nck:turn-digest-phase (nck:lane-digest lane)))))
    (is (null (plans)))))

(deftest channel-host-says-when-the-model-moved ()
  ;; A durable turn.provider_fallback folds onto the card as one line naming
  ;; the model the turn moved to and why — the room's half of the engine's
  ;; failover fact, never a second post.
  (with-digest-host (host lane :responses (replies "s1"))
    (fact "turn.started" (nlk:json-object))
    (fact "turn.provider_fallback"
          (nlk:json-object "from-provider" "deepseek"
                           "from-model" "deepseek-flash"
                           "to-provider" "a6api"
                           "to-model" "deepseek-v4-flash-free"
                           "reason" "overloaded"
                           "status" 503))
    (nck:flush-now host lane)
    (is-present (post (first (plans))) "the failover opens the line"
      (is (search "fell back to deepseek-v4-flash-free (overloaded)"
                  (plan-content post))))))

(deftest channel-host-a-sentence-carrying-the-silence-word-is-spoken ()
  ;; Only the sentinel itself is silent: the same word inside a sentence is
  ;; the answer, said out loud.
  (with-digest-host (host lane :responses (replies "s1"))
    (said "I will stay NO_REPLY until Friday")
    (fact "turn.completed" (nlk:json-object))
    (let ((send (await-plan executor :ping t)))
      (is-present send "the answer posts"
        (is (search "NO_REPLY until Friday" (plan-content send)))))))

(deftest channel-host-a-stop-press-is-acted-on-where-it-arrives (with-digest-host (host lane))
  ;; The room's one escape from a running turn does not queue behind the
  ;; delivery pool, and does not wait out the acknowledgement's round trip
  ;; to the platform: cancelling is not a platform call. The press is acted
  ;; on inside CONTROL-PRESSED, on the thread it arrived on, so a handler
  ;; around the press sees what the stop said — a worker's warning would be
  ;; raised on another thread and never reach here.
  (let ((said (warnings-of (press "i1" "stop" "mike"))))
    (is (search "no active turn" (format nil "~{~a~}" said))))
  ;; Details is a worker's: its answer is platform work and belongs off the
  ;; transport thread.
  (let ((said (warnings-of (press "i2" "details" "stranger"))))
    (is (null said) "nothing a Details press says is raised on the pressing thread")))

(deftest channel-host-a-status-post-landing-after-settle-retires-itself ()
  ;; Without the landing retiring it, a settle can race the status post:
  ;; the finish job retires the chrome while the line is still in flight —
  ;; it reads no id — and the line lands after, a working line no turn
  ;; watches.
  (with-digest-host (host lane :responses (replies "s1"))
    (fact "turn.started" (nlk:json-object))
    (said "NO_REPLY")
    (fact "turn.completed" (nlk:json-object))
    ;; The silent finish retires nothing: no line was ever recorded.
    (await (:timeout 10) (null (nck:turn-digest-answer (nck:lane-digest lane))))
    (is (null (labelled "send_message")))
    ;; The in-flight live plan lands now, with the flags it captured then.
    (nck::perform-flush host lane :post '(:state :working :elapsed "8s" :headline "Working")
                        nil nil :reply-to "m1")
    (is (= 1 (length (labelled "send_message"))) "the late line posts")
    (is (= 1 (length (labelled "delete_message"))) "and retires itself on landing")
    (is-plan (first (labelled "delete_message")) :path "/rooms/123/messages/s1")
    (is (null (nck::turn-digest-status-id (nck:lane-digest lane))))))

(deftests channel-host-a-completion-the-fold-missed (messages said answer card)
  (with-temp-store ()
    (with-digest-host (host lane
                       :responses (replies "s1" "a1")
                       :name "store-fallback" :session "chat-123-m-store"
                       :trigger "m-store")
      (let ((turn (progn (ensure-durable-session "chat-123-m-store")
                         (nlk:admit-turn "chat-123-m-store" "command-store-answer"
                                         "the question"))))
        (dolist (message messages)
          (nlk:record-assistant-message turn message))
        (nlk:complete-turn turn nil)
        (earn-line turn.turn-id)
        ;; Deliberately omit turn.assistant_message_completed from the
        ;; live fold; only turn.completed reaches the host.
        (fact "turn.completed" (nlk:json-object) turn.turn-id)
        (is (posted 3) "the store's word posts beside the card")
        (let ((plans (plans)))
          (is-plan (second plans) :label "send_message")
          (is (search said (plan-content (second plans))))
          (is-plan (third plans) :label card)
          (is (equal answer (nck:turn-digest-answer (nck:lane-digest lane))))))))
  ;; The live frame may be missed, but the durable assistant fact remains the
  ;; answer authority. The worker recovers it and settles the captured card.
  (falls-back-to-store
   (list (nlk:json-object "role" "assistant" "content" "partial answer")
         (nlk:json-object "role" "assistant" "content" "durable answer"))
   "durable answer" "durable answer" "edit_message")
  ;; a DeepSeek round leaked its tool call into text — null
  ;; content, 10,462 chars of reasoning — and the turn completed in silence:
  ;; the engine accepted a reasoning-only final as an answer, and the room
  ;; batched "no text at all" with the SILENT sentinel, so a failure read as
  ;; intent. The engine now retries that final; this is the room's backstop —
  ;; a completion whose only words the room cannot show says so in one line,
  ;; where a turn that never spoke at all keeps the pinned absence.
  (says-a-wordless-one-has-no-answer
   (list (nlk:json-object "role" "assistant" "reasoning_content" "leaked tool call text"))
   "without an answer" nil "delete_message"))

(deftest channel-host-thinking-status (with-digest-host (host lane :responses (replies "s1")))
  ;; A round that speaks only in reasoning: once it has thought for the
  ;; earning window the card opens, saying the turn is thinking and naming
  ;; the thought by its newest whole sentence — never the tail it is still
  ;; writing — without this, the raw tail would read as the answer — and a
  ;; content-null round's committed reasoning settles into it.
  (fact "turn.started" (nlk:json-object))
  (fold-delta host "chat-123-m1" "Looking up the registry. the registry lookup")
  (fold-delta host "chat-123-m1" " NOT THIS TURN" :turn-id "other-turn")
  (is (null (plans)) "delta folds perform no I/O")
  (setf (nck::turn-digest-started-at-ms (nck:lane-digest lane)) (- (nck:now-ms) 9000))
  (nck:flush-now host lane)
  (is-present (post (first (plans))) "a thinking round opens the card"
    (is-carrying (content (plan-content post))
      "Thinking · " "thinking · Looking up the registry."
      (:absent "the registry lookup" "the sentence still being written stays off the card")))
  (fact "turn.assistant_message_completed"
        (nlk:json-object
         "message" (nlk:json-object "content" :null
                                    "reasoning_content"
                                    "Settled on eval.")))
  (nck:flush-now host lane)
  (is-present (edit (second (plans))) "a content-null round settles into the same card"
    (is (equal "PATCH" (nck:request-plan-method edit)))
    (is-carrying (content (plan-content edit))
      "thinking · Settled on eval."
      (:absent "NOT THIS TURN" "another turn's deltas never fold into this digest")))
  (is (= 2 (length (plans)))))

(deftest channel-host-a-quick-turn-says-its-answer-once ()
  ;; A round that says the answer and calls a tool anyway is followed by the
  ;; final round saying it again. The room reads it once, in the answer: words
  ;; on the way never post, whatever the answer says.
  (flet ((run (said answer)
           (with-digest-host (host lane :responses (replies "c1" "a1" "a2") :name "quick-answer")
                      (fold-fact host "chat-123-m1" "turn.started" (nlk:json-object))
             (fold-fact host "chat-123-m1" "turn.assistant_message_completed"
                        (nlk:json-object "message" (nlk:json-object
                                                    "content" said
                                                    "tool_calls" (vector (nlk:json-object "id" "c")))))
             (nck::schedule-flush host lane)
             (fold-fact host "chat-123-m1" "turn.assistant_message_completed"
                        (nlk:json-object "message" (nlk:json-object "content" answer)))
             (fold-fact host "chat-123-m1" "turn.completed" (nlk:json-object))
             (await (:timeout 10) (plans-matching (nck:recording-executor-plans executor)
                                                  :content answer))
             (sleep 0.3)
             (mapcar #'plan-content (plans-matching (nck:recording-executor-plans executor)
                                                    :label "send_message")))))
    (is (equal '("12 squared is **144**.") (run "144." "12 squared is **144**.")))
    (is (equal '("They cover Q3 hiring.") (run "Reading the notes first." "They cover Q3 hiring.")))))

(deftest channel-host-one-flush-in-flight-per-lane ()
  ;; With a pool of drainers, two flushes planned from one digest would race
  ;; to create two status messages for one ask.
  (with-digest-host (host lane :responses (replies "s1") :workers 4)
    (fact "turn.tool_call_started" (nlk:json-object "tool-name" "eval"))
    (dotimes (index 8) (nck:schedule-flush host lane))
    (is (await (:timeout 10) (plans)))
    (sleep 0.3)
    (is (= 1 (length (plans))))))

(deftest channel-host-laneless-failure-notice (with-digest-host (host nil :name "laneless"))
  (fold-fact host "chat-999-t555-m4" "turn.failed"
             (nlk:json-object "detail" "turn was not resumed after 3 boots") "t9")
  (is (await (:timeout 10) (plans)))
  (is-present (plan (first (plans))) "notice plan recorded"
    (is-plan plan :path "/rooms/555/messages")
    (is (search "turn was not resumed" (plan-content plan))))
  (fold-fact host "web-abc" "turn.failed" (nlk:json-object "detail" "x") "t10")
  (sleep 0.2)
  (is (= 1 (length (plans)))))

(defun typing-beats (host executor)
  "Run one typing tick on HOST; the beats EXECUTOR has recorded so far."
  (nck::typing-tick host)
  (length (nck:recording-executor-plans executor)))

(defun queued-lane-digest (host id)
  "Open a queued digest on HOST's lane ID in room 1, the live turn a typing beat follows."
  (let ((lane (nck:intern-lane (nck:host-lanes host) id :target '(:channel-id "1"))))
    (bt2:with-lock-held ((nck:lane-lock lane))
      (nck:digest-note-queued (nck:lane-open-digest lane (nck:now-ms)) 0))))

(deftest channel-host-typing-follows-the-platform-cadence ()
  ;; One indicator per surface, re-asserted on the platform's own refresh —
  ;; Telegram's beat dies in five seconds where Discord's lives ten.
  (let* ((executor (nck:make-recording-executor))
         (host (test-host :executor executor :name "typing")))
    (dolist (id '("chat-1-m1" "chat-1-m2"))
      (queued-lane-digest host id))
    (is (= 1 (typing-beats host executor)) "two live lanes in one surface drive one beat")
    (is-plan (first (nck:recording-executor-plans executor)) :path "/rooms/1/typing")
    (is (= 1 (typing-beats host executor)) "inside the refresh window nothing is re-sent")
    (sleep 0.15)
    (is (= 2 (typing-beats host executor)))))

(deftest channel-host-typing-cap-measures-the-turn-silence ()
  ;; The cap is the turn's silence, not the indicator's age: a surface whose
  ;; turn keeps showing something keeps its beat past the cap, and one that
  ;; has shown nothing for the cap stops — the wedged-turn guard, kept.
  (nlk:bind ((executor (nck:make-recording-executor))
             (host (test-host :executor executor :name "typing-silence")) (nck::+typing-cap-ms+ 300)
             (digest (queued-lane-digest host "chat-1-m1")))
    (is (= 1 (typing-beats host executor)) "a live surface gets its first beat")
    (sleep 0.25)
    ;; The turn shows something — a part, a call, a round — and the
    ;; indicator is already older than the cap when the next tick looks.
    (nck:digest-note-visible digest (nck:now-ms))
    (sleep 0.12)
    (is (= 2 (typing-beats host executor)) "a turn still producing keeps its beat past the cap")
    (sleep 0.35)
    (is (= 2 (typing-beats host executor)) "silence past the cap ends the beat")))

;;; --- commands ---------------------------------------------------------------

(deftest channel-host-slash-line-is-a-command-answered-in-the-room (with-temp-store ())
  ;; A slash line never becomes a lane: it runs through NLE:SLASH against
  ;; the room on a delivery thread and answers as one reply that pings its
  ;; asker. With operators declared, /help is everyone's and the rest are
  ;; theirs; a failure is an answer, never a warning nobody reads.
  (with-digest-host (host nil :name "command-test" :host-keys (:owners '("mike")))
    (setf (nck:host-policy host) (channel-policy :allowed-channels '("123")))
    (flet ((answered (text user message n)
             (say text user message)
             (is (posted n))
             (plan-content (nth (1- n) (plans)))))
      (is (eq :answer (say "/help" "mike" "m1")))
      (is (posted 1) "the answer lands")
      (is-present (post (first (plans))) "the operator's /help answers as a reply to the ask"
        (is-plan post :label "send_message" :path "/rooms/123/messages" "reply_to" "m1" "ping" t)
        (is (search "/models" (plan-content post)) "with the catalog"))
      (is (nlk:session-exists-p "chat-123"))
      (is (zerop (nck:lane-count (nck:host-lanes host))))
      (is (equal "🔒 Only this room's operators can run /models. Press Ask for access to send them a request, or send /help to see what you can use."
                 (answered "/models" "alice" "m2" 2)))
      (is (search "/models" (answered "/help" "alice" "m3" 3)))
      (is (equal "nothing to undo" (answered "<@999> /undo" "mike" "m4" 4)))
      (is (equal "unknown command /nope; /help lists them" (answered "/nope" "mike" "m5" 5)))
      (is (uiop:string-prefix-p "/think: " (answered "/think bogus" "mike" "m6" 6)))
      (is (= 6 (length (plans))) "one message per command, nothing else"))))

(deftest channel-host-models-aux-in-a-room-answers-the-auxiliary-model (with-temp-store ())
  ;; /models aux in a room is the organism's, as on a shell: it answers the
  ;; auxiliary model with how to change it — never the room's own pick nor
  ;; the session summary the bare /models line prints — and `default' takes
  ;; the organism's auxiliary model out, not the room's pick.
  (with-saved-globals (nle::*auxiliary-model*)
    (setf nle::*auxiliary-model* '(:provider "zeta" :model "z-model" :effort "low"))
    (with-digest-host (host nil :name "aux-command" :host-keys (:owners '("mike")))
      (setf (nck:host-policy host) (channel-policy :allowed-channels '("123")))
      (say "/models aux" "mike" "m1")
      (is (posted 1) "the command answers once")
      (is-carrying (content (plan-content (first (plans))))
        (is (uiop:string-prefix-p "auxiliary model: zeta/z-model (low)" content))
        "/models aux <provider> <model> changes it" (:absent "provider: "))
      (say "/models aux default" "mike" "m2")
      (is (posted 2) "the clear answers too")
      (is (uiop:string-prefix-p "auxiliary model: same as main"
                                (plan-content (second (plans)))))
      (is (null nle::*auxiliary-model*)))))

(deftest channel-host-completion-follows-the-commands-own-rule ()
  ;; Completion is the commands' own rule: with operators declared, only an
  ;; operator sees a command's argument choices — /help is everyone's, as
  ;; every command but /help is the operator's.
  (let ((host (test-host :name "completions")))
    (is (nck::completion-allowed-p host "models" "mike"))
    (is (not (nck::completion-allowed-p host "models" "sam")))
    (is (nck::completion-allowed-p host "help" "sam")))
  (is (nck::completion-allowed-p
       (test-host :name "completions-open" :owners '())
       "models" "sam")))

(deftest channel-host-publishes-the-menu-once-per-catalog-shape ()
  ;; The tick publishes the catalog to the platform when its shape differs
  ;; from the one last published: once at start, again when a cell
  ;; registers a command later, never while a publish is in flight, and
  ;; not at all for a platform without a menu or one not ready to take it.
  (with-saved-globals (nle::*registered-commands*)
    (let ((platform (test-platform))
          (ready (list t))
          (seen '()))
      (setf (nck:platform-plan-commands platform)
            (lambda (entries &key timeout-seconds)
              (push entries seen)
              (and (car ready)
                   (list (nck:make-request-plan
                          :method "PUT" :path "/commands"
                          :body (nlk:json-object "count" (length entries))
                          :timeout-seconds timeout-seconds
                          :audit-label "set_commands")))))
      (with-digest-host (host nil :name "menu-test" :host-keys (:platform platform))
        (setf (car ready) nil)
        (nck:sync-commands host)
        (is (not (await (:timeout 0.3) (plans))))
        (is (null (nck:host-commands-published host)))
        (setf (car ready) t)
        (nck:sync-commands host)
        (is (await (:timeout 5) (= 1 (length (plans)))))
        (is-present (plan (first (plans))) "as the platform's plans"
          (is-plan plan :label "set_commands" :timeout 5))
        (is (equal "help" (getf (first (first seen)) :name)))
        (is-present (new (find "new" (first seen) :key (lambda (entry) (getf entry :name))
                                                   :test #'string=))
          "/clear's alias is a command of its own in the menu"
          (is (equal "Start a fresh session" (getf new :description))))
        (is (stringp (nck:host-commands-published host)))
        (nck:sync-commands host)
        (nck:sync-commands host)
        (is (not (await (:timeout 0.3) (> (length (plans)) 1))))
        (nle:register-command "late-pack" "late" (constantly nil) :description "Registered later")
        (nck:sync-commands host)
        (is (await (:timeout 5) (= 2 (length (plans)))))
        (is (find "late" (first seen)
                  :key (lambda (entry) (getf entry :name)) :test #'string=))
        (is (null (nck:sync-commands (test-host))))))))

(deftest channel-host-the-menu-names-every-alias ()
  ;; A platform menu has no aliases, so each name a command answers to is a
  ;; command of its own there, after every command. A name already in the
  ;; menu stays the one command it names.
  (is (equal '(("a" "first") ("b" "second") ("c" "first"))
             (mapcar (lambda (entry)
                       (list (getf entry :name) (getf entry :description)))
                     (nck::menu-entries '((:name "a" :description "first" :aliases ("b" "c"))
                                          (:name "b" :description "second" :aliases ("c")))))))
  (let ((names (mapcar (lambda (entry) (getf entry :name)) (nck::menu-entries (nle:slash-catalog)))))
    (is (member "new" names :test #'string=))
    (is (equal names (remove-duplicates names :test #'string-equal :from-end t)) "no name twice")))

(deftest channel-ask-prompt-reads-attachments-in-at-their-anchors (let ((nck:*transcription* nil)
                                                                        (read '())))
  ;; What the lane reads when people attached things: at the end of each line
  ;; that brought a file, the marker that pairs with the image part the turn
  ;; carries, a recording's transcript, or the one note saying why neither.
  ;; The fetch and the engine are stubbed: the network and the model are the
  ;; live proof's business, the composition is this test's.
  (with-stubbed-fdefinitions
      ((nck::read-attachment (attachment)
        (let ((name (gethash "filename" attachment)))
          (push name read)
          (cond ((equal name "good.png")
                 (values :image (cons "image/png" "QUJD") nil))
                ((member name '("note.ogg" "broken.ogg") :test #'equal)
                 (values :audio (cons (octets-of 1) "ogg") nil))
                (t (values nil nil "its bytes open no image (PNG, JPEG, GIF, WebP) and no recording")))))
       (nck:transcribe-audio (octets container &key seconds)
        (if (eql seconds 3)
            (error "the local transcriber failed: out of memory")
            (values "ship it" 5.52))))
    (flet ((file (name type &optional seconds)
             (nlk:json-object "url" (format nil "https://cdn/~a" name)
                              "filename" name "media_type" type "seconds" seconds)))
      (nlk:bind (((prompt anchors) (nck::compose-prompt
                                    (list "bob [m1 u2]: morning"
                                          (cons "v1se [m2 u3]:" (list (file "note.ogg" "audio/ogg" 5.52))))
                                    "kim [m3 u4]: what does this say?"
                                    :anchors (list (cons (length "kim [m3 u4]: what does this say?")
                                                         (list (file "good.png" "image/png")
                                                               (file "bad.png" "image/png")
                                                               (file "broken.ogg" "audio/ogg" 3)
                                                               (file "long.ogg" "audio/ogg" 900))))))
                 (ask (nck:make-ask :room "discord-1" :lane "discord-1-m3"
                                    :prompt prompt :attachments anchors))
                 ((prompt images) (nck::ask-prompt-and-attachments ask)))
        (is (equal '(("image/png" . "QUJD")) images))
        (is (equal "bob [m1 u2]: morning
v1se [m2 u3]: [Audio #1, 5.5s, transcribed] \"ship it\"
kim [m3 u4]: what does this say? [Image #1] [the attachment \"bad.png\" could not be read: its bytes open no image (PNG, JPEG, GIF, WebP) and no recording] [the recording \"broken.ogg\" could not be transcribed: the local transcriber failed: out of memory] [the recording \"long.ogg\" could not be transcribed: 15:00 long, over the 5:00 cap (transcription.max_seconds)]"
                   prompt))
        (is (not (member "long.ogg" read :test #'equal)))))))

(deftest channel-read-attachment-sniffs-what-it-fetched ()
  ;; The bytes decide, never the declared type: a recording declared as an
  ;; image is still a recording, a file that opens as nothing is saved for the
  ;; lane's tools and named, and a size declared past the ceiling a file may
  ;; take is refused before the fetch runs.
  (flet ((entry (octets &rest keys)
           (apply #'nlk:make-json-object "fetch" (lambda () octets) keys)))
    (multiple-value-bind (kind data) (nck::read-attachment
                                      (entry (fixture-recording) "media_type" "image/png"))
      (is (eq :audio kind))
      (is (equal "ogg" (cdr data))))
    (is-values (kind data) (nck::read-attachment (entry (test-png-octets) "media_type" "audio/ogg"))
      (kind eq :image)
      ((car data) "image/png"))
    (let ((nck::*attachment-directory* (temp-path "channel-files" nil))
          (clip (octets-of 1 2 3 4 5 6 7 8 9 10 11 12)))
      (unwind-protect
           (is-values (kind data) (nck::read-attachment (entry clip "id" "a7" "filename" "clip 1.mp4"
                                                               "media_type" "video/mp4"))
             (kind eq :file)
             ((cdr data) = 12)
             ((file-namestring (car data)) "a7-clip_1.mp4")
             ((alexandria:read-file-into-byte-vector (car data)) equalp clip))
        (uiop:delete-directory-tree nck::*attachment-directory* :validate t :if-does-not-exist :ignore))
      ;; The prompt names it, its type and size, and where it is.
      (let ((ask (nck::make-ask :prompt "mike [m1 u1]: what happens in this"
                                :attachments (list (cons 34 (list (entry clip "id" "a8" "filename" "c.mp4"
                                                                         "media_type" "video/mp4")))))))
        (unwind-protect
             (is (search (format nil "what happens in this [File \"c.mp4\" (video/mp4, 12 bytes) saved at ~a"
                                 (uiop:native-namestring (merge-pathnames "a8-c.mp4" nck::*attachment-directory*)))
                         (nck::ask-prompt-and-attachments ask)))
          (uiop:delete-directory-tree nck::*attachment-directory* :validate t :if-does-not-exist :ignore))))
    ;; A video's container holds sound, but a video file is opened, not
    ;; heard; a round video note that declares its length is a recording.
    (let ((nck::*attachment-directory* (temp-path "channel-files" nil))
          (mp4 (octets-of 0 0 0 24 #x66 #x74 #x79 #x70 #x69 #x73 #x6F #x6D)))
      (unwind-protect
           (progn
             (is (eq :file (nck::read-attachment (entry mp4 "media_type" "video/mp4"
                                                        "filename" "clip.mp4"))))
             (is (eq :audio (nck::read-attachment (entry mp4 "media_type" "video/mp4"
                                                         "seconds" 12)))))
        (uiop:delete-directory-tree nck::*attachment-directory* :validate t :if-does-not-exist :ignore)))
    (multiple-value-bind (kind data note)
        (nck::read-attachment (nlk:json-object "fetch" (lambda () (error "fetched"))
                                               "media_type" "video/mp4"
                                               "size" (1+ nck::+file-max-bytes+)))
      (is (null kind))
      (is (null data))
      (is (search "ceiling" note))
      (is (not (search "fetched" note)) "nothing was fetched"))))

(deftest channel-read-attachment-reads-a-text-file-whole ()
  ;; A file declared as text is read into the prompt whole, fenced under its
  ;; name — the fence longer than any run of backticks inside, so the file's
  ;; own fences stay its own — up to the ceiling Hermes reads; bytes that are
  ;; not UTF-8 text, and a text file past that ceiling, are files on disk the
  ;; lane opens with its tools.
  (flet ((entry (octets &rest keys)
           (apply #'nlk:make-json-object "fetch" (lambda () octets) keys)))
    (let ((notes (format nil "# Q3~%- ship /link (owner: mike)~%```lisp~%(+ 1 2)~%```~%")))
      (is-values (kind data) (nck::read-attachment
                              (entry (sb-ext:string-to-octets notes :external-format :utf-8)
                                     "media_type" "text/markdown" "filename" "notes.md"))
        (kind eq :text)
        (data notes))
      (let ((text (nck::document-prompt-text (nlk:json-object "filename" "notes.md") notes)))
        (is (uiop:string-prefix-p (format nil "[File \"notes.md\"]~%````~%# Q3") text))
        (is (uiop:string-suffix-p text (format nil "```~%````")))))
    ;; Bytes that are not UTF-8 text are a file the lane opens with its tools.
    (let ((nck::*attachment-directory* (temp-path "channel-files" nil)))
      (unwind-protect
           (is (eq :file (nck::read-attachment (entry (octets-of 104 105 255 254) "filename" "odd.txt"))))
        (uiop:delete-directory-tree nck::*attachment-directory* :validate t :if-does-not-exist :ignore)))
    ;; A text file past the ceiling one is read whole at is a file on disk.
    (let ((nck::*attachment-directory* (temp-path "channel-files" nil)))
      (unwind-protect
           (is (eq :file (nck::read-attachment
                          (entry (make-array (1+ nck::+document-max-bytes+)
                                             :element-type '(unsigned-byte 8) :initial-element 97)
                                 "filename" "huge.log"))))
        (uiop:delete-directory-tree nck::*attachment-directory* :validate t :if-does-not-exist :ignore)))
    ;; The prompt carries it where the asker's line ends.
    (let ((ask (nck::make-ask :prompt "mike [m1 u1]: summarize this"
                              :attachments (list (cons 28 (list (entry (sb-ext:string-to-octets
                                                                        "- a" :external-format :utf-8)
                                                                       "filename" "notes.md")))))))
      (is (search (format nil "summarize this [File \"notes.md\"]~%```~%- a~%```")
                  (nck::ask-prompt-and-attachments ask))))))

(defmacro with-recordings-heard ((transcript &key (seconds 'seconds) on-read) &body body)
  "BODY with every attachment read as a recording, ON-READ run at each read, that
transcribes to TRANSCRIPT and lasts SECONDS — by default the length it declares."
  `(with-stubbed-fdefinitions
       ((nck::read-attachment (attachment) ,on-read (values :audio (cons (octets-of 1) "ogg") nil))
        (nck:transcribe-audio (octets container &key seconds) (values ,transcript ,seconds)))
     ,@body))

(deftest channel-voice-note-rides-the-chatter-to-the-next-ask (let ((nck:*transcription* nil)
                                                                    (host (test-host))))
  ;; v1se's voice note in a room that wants a mention: nobody addressed the
  ;; bot, so it is chatter — and chatter a voice note is: its line rides with
  ;; the recording, and the ask that asks about it reads the transcript in
  ;; right after the person who spoke it, not after the asker.
  (nck:observe-candidate
   host (test-candidate :text "" :user-name "v1se" :message "7"
                        :attachments (vector (nlk:json-object "url" "https://cdn/voice-message.ogg"
                                                              "media_type" "audio/ogg"
                                                              "filename" "voice-message.ogg"
                                                              "size" 22399 "seconds" 5.52))))
  (nck:observe-candidate host (test-candidate :text "did anyone hear that?"
                                              :user-name "bob" :message "8"))
  ;; An image nobody addressed stays out of the chatter, as it always has.
  (nck:observe-candidate
   host (test-candidate :text "" :user-name "carol" :message "8b"
                        :attachments (vector (nlk:json-object "url" "https://cdn/x.png"
                                                              "media_type" "image/png"))))
  (let ((ask (nck:build-ask host (test-candidate :text "<@999> do you see the voice msg above"
                                                 :message "9" :user-name "mike")
                            "chat-100-m9" nil)))
    (is (equal "v1se [m7 uu1]:
bob [m8 uu1]: did anyone hear that?
mike [m9 uu1]: do you see the voice msg above"
               (nck:ask-prompt ask)))
    (with-recordings-heard ("can you check why the build is red")
      (multiple-value-bind (prompt images) (nck::ask-prompt-and-attachments ask)
        (is (null images))
        (is (equal "v1se [m7 uu1]: [Audio #1, 5.5s, transcribed] \"can you check why the build is red\"
bob [m8 uu1]: did anyone hear that?
mike [m9 uu1]: do you see the voice msg above"
                   prompt)))))
  ;; A voice note that is itself the ask, replying to a message: the
  ;; transcript follows the speaker's line, above the reply it answers.
  (let ((candidate (test-candidate :text "" :user-name "v1se" :message "10"
                                   :reply "the deploy is done" :reply-to "5"
                                   :reply-name "bob"
                                   :attachments (vector (nlk:json-object
                                                         "url" "https://cdn/voice-message.ogg"
                                                         "media_type" "audio/ogg" "seconds" 2)))))
    (let ((ask (nck:build-ask host candidate "chat-100-m10" nil)))
      (with-recordings-heard ("" :seconds 2.0)
        (is (equal "v1se [m10 uu1 r5]: [Audio #1, 2.0s, transcribed: no speech heard]
↩ bob [m5]: the deploy is done"
                   (nck::ask-prompt-and-attachments ask)))))))

(deftest channel-reply-to-a-voice-note-reads-what-it-says ()
  ;; "do u hear this", typed as a reply to the operator's own voice note: the
  ;; ask carries the note it answers, so its transcript is read in under the
  ;; reply's line whether or not the note is still in the chatter — and when
  ;; it is, it is read once, not twice. An edit that adds the mention to the
  ;; voice note makes the note its own ask the same way.
  (let ((nck:*transcription* nil)
        (host (test-host))
        (read 0)
        (note (nlk:json-object "id" "att-1" "url" "https://cdn/qb8rzbq.ogg"
                               "media_type" "audio/ogg" "filename" "qb8rzbq.ogg"
                               "seconds" 5.52)))
    (nck:observe-candidate host (test-candidate :text "" :user-name "peas" :message "7"
                                                :attachments (vector note)))
    (let ((ask (test-candidate :text "<@999> do u hear this" :message "9"
                               :user-name "peas" :reply "" :reply-to "7"
                               :reply-name "peas")))
      ;; The same file, as the platform ships it again inside the reply.
      (setf (gethash "attachments" (gethash "reply" ask))
            (vector (nlk:json-object "id" "att-1" "url" "https://cdn/qb8rzbq.ogg?ex=2"
                                     "media_type" "audio/ogg" "seconds" 5.52)))
      (with-recordings-heard ("is the bot hearing this" :on-read (incf read))
        (is (equal "peas [m7 uu1]:
peas [m9 uu1 r7]: do u hear this
↩ peas [m7] [Audio #1, 5.5s, transcribed] \"is the bot hearing this\""
                   (nck::ask-prompt-and-attachments
                    (nck:build-ask host ask "chat-100-m9" nil))))
        (is (= 1 read) "the note is fetched and transcribed once")))
    ;; The voice note edited to mention the bot: its own ask, its own file.
    (setf read 0)
    (nck:observe-candidate host (test-candidate :text "" :user-name "peas" :message "7"
                                                :attachments (vector note)))
    (let ((edited (test-candidate :text "<@999>" :user-name "peas" :message "7"
                                  :attachments (vector note))))
      (with-recordings-heard ("is the bot hearing this" :on-read (incf read))
        (is (equal "peas [m7 uu1]:
peas [m7 uu1]: [Audio #1, 5.5s, transcribed] \"is the bot hearing this\""
                   (nck::ask-prompt-and-attachments
                    (nck:build-ask host edited "chat-100-m7" nil))))
        (is (= 1 read) "the chatter's copy is not read again")))))

;;; --- a file into the room --------------------------------------------------

(defun test-png-octets (&optional (size 64))
  "PNG magic bytes and filler: an image the sniffer opens, without a real
picture's weight."
  (replace (make-array size :element-type '(unsigned-byte 8) :initial-element 0)
           #(137 80 78 71 13 10 26 10)))

(defun write-pea-fixture ()
  "Write /tmp/pea.png — the picture the file tests hand around — so a
 delivery that checks the file exists has one to join."
  ;; The fixture is the test's own: an ambient /tmp/pea.png would be a silent
  ;; dependency, and a test that reads it would go red whenever /tmp is cleaned.
  (alexandria:write-byte-vector-into-file (test-png-octets) #p"/tmp/pea.png" :if-exists :supersede)
  #p"/tmp/pea.png")

(defmacro with-file-host ((host name &key responses (platform '(test-platform))) &body body)
  "BODY with HOST, WITH-DIGEST-HOST's host named NAME over PLATFORM and its lane
LANE, as the one running channel."
  ;; Writes the file fixture first: a file test that depends on ambient /tmp
  ;; is a test that fails the day /tmp is cleaned.
  `(with-digest-host (,host lane :name ,name :responses ,responses :host-keys (:platform ,platform))
     (let ((nck::*hosts* (list ,host)))
       (write-pea-fixture)
       ,@body)))

(deftest channel-post-file-carries-a-file ()
  ;; One file posts as its own message: the kit plans the platform's own
  ;; upload, executes it on the caller's thread, and answers what the
  ;; platform created — the id a read-back would name.
  (with-file-host (host "post-file" :responses (replies "f1") :platform (test-platform :files t))
    (is (equal '(:status 200 :body (:id "f1"))
               (nck:post-file #p"/tmp/pea.png" :platform "test"
                              :channel "123" :thread "t9"
                              :content "the picture" :reply-to "m1")))
    (let ((plan (first (nck:recording-executor-plans executor))))
      (is-plan plan :path "/rooms/t9/files" "caption" "the picture")
      (is (equal "m1" (plan-field plan "reply_to")) "and the message it answers rides with it"))))

(deftest channel-post-file-says-when-the-platform-carries-none (with-file-host (host "no-files"))
  ;; A platform that cannot carry a file answers why and sends nothing: the
  ;; caller reads the refusal instead of a caption naming a picture nobody
  ;; can see.
  (let ((answer (nck:post-file #p"/tmp/pea.png" :platform "test"
                               :channel "123")))
    (is (zerop (getf answer :status)))
    (is (search "carries no files" (getf answer :error)))
    (is (null (nck:recording-executor-plans executor)))))

(deftest channel-post-file-refuses-a-platform-that-is-not-running (let ((nck::*hosts* '())))
  (is (search "no nowhere channel is running"
              (refusal-text error
                (nck:post-file #p"/tmp/pea.png" :platform "nowhere" :channel "1")))))

(deftest channel-answer-file-notes-the-running-turn (with-file-host (host "answer-file"))
  ;; A file handed to the answer is recorded on the running turn — read
  ;; back as the turn's own file, never posted now.
  (setf (nck:lane-active-turn-id lane) "t9")
  (is (equal (list :session-id "chat-123-m1" :turn-id "t9"
                   :files (list #p"/tmp/pea.png"))
             (nck:answer-file #p"/tmp/pea.png" :platform "test"
                              :channel "123")))
  (is (null (nck:recording-executor-plans executor)))
  (is (equal (list #p"/tmp/pea.png")
             (nck::take-answer-files lane "t9")))
  (is (null (nck::take-answer-files lane "t9"))))

(deftest channel-answer-file-needs-a-running-turn (with-file-host (host "no-turn"))
  ;; No turn, no answer, no place for the file: the call says which of the
  ;; two is missing rather than posting beside an answer that never comes.
  (is-table (needle channel)
    (search needle (refusal-text error
                     (nck:answer-file #p"/tmp/pea.png" :platform "test" :channel channel)))
    ("no turn is running" "123")
    ("no lane answers" "999")))

(deftest channel-answer-carries-its-files ()
  ;; The answer's own message carries the picture: the plan the terminal
  ;; delivery posts names the file the turn handed over, on its first chunk.
  (with-file-host (host "answer-carries" :responses (replies "a1"))
    (let ((digest (nck:make-turn-digest :turn-id "t9" :phase :completed :answer "the answer")))
      (setf (nck:lane-active-turn-id lane) "t9")
      (nck:answer-file #p"/tmp/pea.png" :platform "test" :channel "123")
      (nck::deliver-answer host lane digest)
      (let ((plan (first (nck:recording-executor-plans executor))))
        (is (search "the answer" (plan-content plan)))
        (is (equalp #("pea.png") (plan-field plan "files")))))))

;;; --- choices: a clarifying question's answers on it ---------------------------------

(deftest channel-answer-choices-ride-the-answer ()
  ;; Choices handed to the answer ride its message as one row of buttons,
  ;; each saying its own label, under the words they answer: on the last
  ;; chunk of a long answer. A second set replaces the first.
  (with-file-host (host "answer-choices" :responses (replies "a1" "a2")
                                         :platform (test-platform :choices t))
    (setf (nck:lane-active-turn-id lane) "t9")
    (nck:answer-choices '("Postgres" "SQLite") :platform "test" :channel "123")
    (is (equal '(:session-id "chat-123-m1" :turn-id "t9"
                 :choices ("Zero setup" "Relational queries" "Realtime sync"))
               (nck:answer-choices '("Zero setup" " Relational queries " "Realtime sync")
                                   :platform "test" :channel "123")))
    (is (null (nck:recording-executor-plans executor)) "nothing posts before the answer")
    (nck::deliver-answer host lane (nck:make-turn-digest
                                    :turn-id "t9" :phase :completed
                                    :answer (format nil "~a~%~%What matters most?"
                                                    (make-string 2100 :initial-element #\x))))
    (let* ((plans (nck:recording-executor-plans executor))
           (last-plan (first (last plans))))
      (is (< 1 (length plans)) "a long answer is several messages")
      ;; None but the last carries controls.
      (is (every (lambda (plan) (null (plan-field plan "controls"))) (butlast plans)))
      (is (search "What matters most?" (plan-content last-plan)))
      ;; The last carries one row of buttons, each saying its label.
      (is (equal '((("Zero setup" "Zero setup" :primary nil)
                    ("Relational queries" "Relational queries" :primary nil)
                    ("Realtime sync" "Realtime sync" :primary nil)))
                 (press-lines (plan-field last-plan "controls")))))
    (is (null (nck::take-answer-choices lane "t9")) "taken once")))

(deftest channel-answer-choices-refuses-what-it-cannot-show (with-file-host (host "no-choices"))
  ;; Labels a row of buttons cannot show refuse, and so does a platform whose
  ;; messages carry no choices: the model hears why instead of a question
  ;; posted without its answers.
  (setf (nck:lane-active-turn-id lane) "t9")
  (is-table (needle labels)
    (search needle (refusal-text error (nck:answer-choices labels :platform "test" :channel "123")))
    ("2 to 5 distinct labels" '("only one"))
    ("2 to 5 distinct labels" '("same" "same"))
    ("2 to 5 distinct labels" '("a" "b" "c" "d" "e" "f"))
    ("2 to 5 distinct labels" (list "short" (make-string 81 :initial-element #\x)))
    ("Test messages carry no choices" '("Postgres" "SQLite"))))

(deftest channel-answer-waits-marks-the-posted-answer (with-temp-store ())
  ;; Without a written mark, the thread board's Needs you would be a guess from
  ;; a question mark. A turn marks its answer waiting with ANSWER-WAITS --
  ;; choices mark it too -- and the mark is written down beside the messages
  ;; the answer posted as, so a reader of the room's newest message knows
  ;; whether it waits. An unmarked answer waits on nobody, whatever it ends on.
  ;; The mark is the session's own, in its log, so a listing of sessions reads
  ;; it as the room does.
  (with-file-host (host "answer-waits" :responses (replies "a1" "a2" "a3")
                                       :platform (test-platform :choices t))
    (nlk:create-session :id "chat-123-m1" :cwd ".")
    (setf (nck:lane-active-turn-id lane) "t9")
    (let ((nle::*snippet-touched* nil))
      (is (equal '(:session-id "chat-123-m1" :turn-id "t9" :waits "Push the pin?")
                 (nck:answer-waits :question " Push the pin? " :platform "test" :channel "123")))
      (is (eq t nle::*snippet-touched*) "marking is an act"))
    (is (null (nck:recording-executor-plans executor)) "nothing posts before the answer")
    (is (equal '("Push the pin?" "t9") (multiple-value-list (nlk:session-waits "chat-123-m1"))) "the session carries the mark")
    (nck::deliver-answer host lane (nck:make-turn-digest :turn-id "t9" :phase :completed
                                                         :answer "Landed. Push the pin?"))
    (is (equal "Push the pin?" (nck:answer-waiting host "123" "a1")) "the posted message waits")
    (is (null (nck::answer-mark lane "t10")) "a later turn's answer is not marked by an earlier one's")
    (setf (nck:lane-active-turn-id lane) "t10")
    (nck::deliver-answer host lane (nck:make-turn-digest :turn-id "t10" :phase :completed
                                                         :answer "Want that?"))
    (is (null (nck:answer-waiting host "123" "a2")) "an unmarked question waits on nobody")
    (setf (nck:lane-active-turn-id lane) "t11")
    (nck:answer-choices '("Postgres" "SQLite") :platform "test" :channel "123")
    (nck::deliver-answer host lane (nck:make-turn-digest :turn-id "t11" :phase :completed
                                                         :answer "Which store?"))
    (is (equal "" (nck:answer-waiting host "123" "a3")) "choices wait, on the answer's own line")
    (is (equal '("" "t11") (multiple-value-list (nlk:session-waits "chat-123-m1"))) "and the session with them")
    (is-table (needle question)
      (search needle (refusal-text error (nck:answer-waits :question question :platform "test" :channel "123")))
      ("1 to 200 characters" "  ")
      ("1 to 200 characters" (make-string 201 :initial-element #\x)))))

(deftest channel-answer-verbs-act-so-no-watch-repeats-them
    (with-file-host (host "answer-acts" :platform (test-platform :choices t)))
  ;; (nck:answer-choices ...) closing a snippet read to the eval tool as a read
  ;; -- it reaches nothing outside the image -- so it would be watched, run
  ;; again after every later snippet, and put an earlier question's buttons on
  ;; later answers. Each answer verb stamps its call an act before it acts:
  ;; the snippet is never watched, and a run again is refused with nothing
  ;; handed.
  (setf (nck:lane-active-turn-id lane) "t9")
  (let ((nle::*snippet-touched* nil))
    (nck:answer-choices '("Postgres" "SQLite") :platform "test" :channel "123")
    (is (eq t nle::*snippet-touched*) "handing choices is an act"))
  (let ((nle::*snippet-touched* nil))
    (nck:answer-file #p"/tmp/pea.png" :platform "test" :channel "123")
    (is (eq t nle::*snippet-touched*) "handing a file is an act"))
  (is (equal '("Postgres" "SQLite") (nck::take-answer-choices lane "t9")))
  (nck::take-answer-files lane "t9")
  (let ((nle::*watch-running* t))
    (is (eq :refused (handler-case (nck:answer-choices '("Old" "Stale") :platform "test" :channel "123") (error () :refused))))
    (is (eq :refused (handler-case (nck:answer-file #p"/tmp/old.png" :platform "test" :channel "123") (error () :refused)))))
  (is (null (nck::take-answer-choices lane "t9")) "a refused run again hands no choices")
  (is (null (nck::take-answer-files lane "t9")) "nor a file"))

(defun test-press (text card &key (card-text "What matters most?") (user "u1"))
  "USER pressing the choice that says TEXT on the card message CARD in room
123, as interaction i1 — no message of its own."
  (let ((press (test-candidate :channel "123" :message nil :text text :user user
                               :reply-to card :addressed t)))
    (setf (gethash "pressed" (gethash "source" press)) t
          (gethash "interaction_id" (gethash "source" press)) "i1"
          (gethash "card_text" (gethash "source" press)) card-text)
    press))

(deftest channel-a-press-answers-the-card-it-sits-on ()
  ;; A press on a choice is the presser's reply to the card: the lane that
  ;; posted it takes the label as its next ask, and the card is answered in
  ;; place — its words kept, its buttons gone, a line naming the answer.
  (with-digest-host (host lane :name "press" :host-keys (:platform (test-platform :choices t)))
    (setf (nck:host-policy host) (channel-policy))
    (nck:bind-lane-address (nck:host-lanes host) "c9" (nck:lane-session-id lane))
    (let ((continued '()))
      (with-stubbed-fdefinition (nck:continue-lane (host lane ask)
                                  (push (list lane ask) continued))
        (is (eq :answer (nck:handle-candidate host (test-press "Zero setup" "c9"))))
        (is (= 1 (length continued)))
        (is (eq lane (first (first continued))) "the card's own lane")
        (is (search "Zero setup" (nck:ask-prompt (second (first continued)))))
        (is (posted 2))
        (is-plan (first (labelled "interaction_defer")) :path "/interactions/i1"
                 "private" nil "pressed" t)
        (let ((answer (first (labelled "interaction_response"))))
          (is (search "What matters most?" (plan-content answer)) "the question stays")
          (is (search "Answered: Zero setup" (plan-content answer)))
          (is (eq :clear (plan-field answer "controls")) "its buttons go"))))))

(deftest channel-each-press-is-its-own-submission ()
  ;; A press is no message, so what makes it its own submission is the card
  ;; it answered and the line it said: a press on a second card in the same
  ;; lane is a new ask, never the first press arriving twice (the engine
  ;; dedupes on the key, and a shared one left every press after a lane's
  ;; first with no turn); the same press delivered twice is one ask.
  (with-digest-host (host lane :name "presses" :host-keys (:platform (test-platform :choices t)))
    (setf (nck:host-policy host) (channel-policy))
    (nck:bind-lane-address (nck:host-lanes host) "c1" (nck:lane-session-id lane))
    (nck:bind-lane-address (nck:host-lanes host) "c2" (nck:lane-session-id lane))
    (let ((asks '()))
      (with-stubbed-fdefinition (nck:continue-lane (host lane ask)
                                  (push ask asks))
        (nck:handle-candidate host (test-press "Run it" "c1"))
        (nck:handle-candidate host (test-press "Run it" "c2"))
        (nck:handle-candidate host (test-press "Run it" "c2")))
      (is (= 3 (length asks)))
      (destructuring-bind (again second first) (mapcar #'nck::ask-identity asks)
        (is (stringp first) "a press has a key of its own")
        (is (not (equal first second)) "another card's press is another ask")
        (is (equal second again) "the same press twice is one ask")))))

(deftest channel-a-press-on-a-card-no-lane-opens-one ()
  ;; A card whose lane is gone -- reaped after its idle window, or lost to a
  ;; restart -- still takes its press, as a typed reply to it would: the card
  ;; is answered in place, its buttons gone, and the label opens a new lane at
  ;; the room's head, anchored on the card. The press used to tell the presser
  ;; alone "this question is no longer open" and leave every button on.
  (with-digest-host (host lane :name "stale-press" :host-keys (:platform (test-platform :choices t)))
    (setf (nck:host-policy host) (channel-policy))
    (let ((gated '()))
      (with-stubbed-fdefinitions ((nck:continue-lane (host lane ask)
                                    (error "a stale press continued a lane"))
                                  (nck::gate-ask (host ask)
                                    (push ask gated)))
        (is (eq :answer (nck:handle-candidate host (test-press "Zero setup" "c404")))))
      (is (= 1 (length gated)) "one new lane")
      (let ((ask (first gated)))
        (is (search "Zero setup" (nck:ask-prompt ask)))
        (is (equal "c404" (nck::ask-message-id ask)) "anchored on the card: its answer replies there")
        (is (search "-mc404" (nck::ask-lane ask)) "the lane is named for the card")
        (is (not (nck::ask-continue-p ask)) "a fork at the room's head"))
      (is (posted 2))
      (is-plan (first (labelled "interaction_defer")) "private" nil "pressed" t)
      (let ((answer (first (labelled "interaction_response"))))
        (is (search "What matters most?" (plan-content answer)) "the question stays")
        (is (search "Answered: Zero setup" (plan-content answer)))
        (is (eq :clear (plan-field answer "controls")) "its buttons go")
        (is (null (search "no longer open" (plan-content answer))))))))

(deftest channel-a-press-is-a-short-id-the-store-keeps ()
  ;; A choice's data is a press: one short id for one line and the view its
  ;; card opens on, the same on every card, inside every platform's cap
  ;; (Telegram's callback_data is 64 bytes, a Discord custom_id 100), read
  ;; back as its line -- from the store once the image has forgotten it, so a
  ;; card posted before a restart still says its line.
  (with-temp-store ()
    (let* ((line (format nil "/models cline ~a" (make-string 120 :initial-element #\m)))
           (data (nck:press-token line :tab 1 :page 2 :back "/cron")))
      (is (equal data (nck:press-token line :tab 1 :page 2 :back "/cron")) "the same press is the same id")
      (is (not (equal data (nck:press-token line :tab 2 :page 2 :back "/cron"))) "another view is another press")
      (is (<= (length data) 64))
      (is (equal line (nck:said-line data)))
      (let ((action (nck:press-action data)))
        (is (eql 1 (nlk:json-value action :integer "tab")))
        (is (eql 2 (nlk:json-value action :integer "page")))
        (is (equal "/cron" (nlk:json-value action :text "back"))))
      (let ((nck::*presses* (make-hash-table :test #'equal :synchronized t)))
        (is (equal line (nck:said-line data)) "read back from the store")))
    (is (null (nck:said-line "nck:p:000000000000")) "a press this home never made says nothing")
    (is (null (nck:said-line "Zero setup")) "and data that is no press says nothing")))

(deftest channel-presses-stay-under-the-state-quota ()
  ;; The store keeps presses on a ring of slots under the session state's
  ;; per-namespace key quota: minting more distinct presses than the quota
  ;; fails no card (it once answered /voice with "session state quota
  ;; exceeded"), the newest reads back once the image forgets it, and a
  ;; press a later one displaced says nothing, never that one's line.
  (with-temp-store ()
    (let* ((count (+ 50 nlk::+max-state-keys-per-ws+))
           (lines (loop for i below count collect (format nil "/voice join ~a" i)))
           (datas (let ((nck::*presses* (make-hash-table :test #'equal :synchronized t)))
                    (mapcar #'nck:press-token lines))))
      (is (eql count (length datas)) "every press was minted")
      (let ((nck::*presses* (make-hash-table :test #'equal :synchronized t)))
        (is (equal (car (last lines)) (nck:said-line (car (last datas))))
            "the newest press reads back from the store")
        (is (loop for line in lines for data in datas
                  always (member (nck:said-line data) (list nil line) :test #'equal))
            "a displaced press says nothing, never another's line")))))

(deftest channel-a-command-answers-a-room-its-dialog-as-a-card ()
  ;; A command answers a shell with a dialog beside its words (NLE:SLASH's
  ;; second value, NLE:LIST-DIALOG: /cron's jobs, /link's browsers). Where a
  ;; platform's messages carry choices the room gets it as a card: its title
  ;; and context, a menu of the rows that run something -- each saying the
  ;; line the shell's Enter runs, a row's RUN, or a VALUE draft a shell hands
  ;; the composer, which the card asks about before it runs -- the rows that
  ;; run nothing in its words, and Refresh. A press's answer that is words
  ;; alone is said over the card pressed, read again. Where messages carry
  ;; no choices, the words alone.
  (with-temp-store ()
    (let ((ran '()))
      (with-saved-globals (nle::*registered-commands*)
        (nle:register-command
         "card-test" "jobs"
         (lambda (args session-id)
           (declare (ignore session-id))
           (push args ran)
           (if (plusp (length args))
               (format nil "did ~a" args)
               (values "jobs: 2"
                       (nle:list-dialog
                        :title "Jobs (2)" :context "1 active"
                        :rows (list (list :label "nightly" :detail "every day" :mark "●"
                                          :run (list :enter "/jobs show nightly" :space "/jobs pause nightly"))
                                    (list :label "draft" :detail "done" :value "/jobs remove draft")
                                    (list :label "Paused for the week" :detail "a note")
                                    (list :label "" :detail "that carries on")
                                    (list :label "$name" :value "$name "))))))
         :session nil)
        (let* ((host (test-host :name "command-dialog" :platform (test-platform :choices t)))
               (plain (test-host :name "command-words"))
               (channel (test-candidate :channel "100")))
          (flet ((press (data)
                   (let ((press (test-press (nck:said-line data) "c1")))
                     (setf (gethash "press_data" (gethash "source" press)) data)
                     (multiple-value-list (nck::run-command host press (nck:said-line data))))))
            (destructuring-bind (text controls &rest _) (multiple-value-list (nck::run-command host channel "/jobs"))
              (declare (ignore _))
              (is (search (format nil "## Jobs (2)~%1 active") text))
              (is (search (format nil "- Paused for the week · a note~%  that carries on") text) "a row that runs nothing is said, one with no label carrying on the row above")
              (is (search "- $name" text) "a value that is no line is no press")
              ;; Few rows: each is a line in its order, a button on those that run one.
              (is (equal '((:row "●" "nightly" "every day" ("Open" "/jobs show nightly" :secondary nil))
                           (:row nil "draft" "done" ("Open…" "/jobs remove draft" :secondary nil))
                           (:row nil "Paused for the week" "a note" nil)
                           (:row nil "" "that carries on" nil)
                           (:row nil "$name" nil nil)
                           (("Refresh" "/jobs" :secondary nil)))
                         (press-lines controls)))
              (destructuring-bind (nightly draft) (list (second (fifth (first controls))) (second (fifth (second controls))))
                (is (equal "/jobs" (nlk:json-value (nck:press-action nightly) :text "back")))
                (is (null (nlk:json-value (nck:press-action nightly) :boolean "confirm")))
                ;; A run row runs; its words are said over the card it was on.
                (destructuring-bind (text controls &rest _) (press nightly)
                  (declare (ignore _))
                  (is (search (format nil "did show nightly~%~%## Jobs (2)") text))
                  (is (equal "/jobs show nightly" (nck:said-line (second (fifth (first controls)))))))
                ;; A draft asks first, and runs nothing until Run.
                (setf ran '())
                (destructuring-bind (text controls &rest _) (press draft)
                  (declare (ignore _))
                  (is (null ran) "the draft did not run")
                  (is (search (format nil "## Run this?~%/jobs remove draft") text))
                  (is (equal '((("Run" "/jobs remove draft" :primary nil) ("Back" "/jobs" :secondary nil)))
                             (press-lines controls)))
                  (destructuring-bind (text &rest _) (press (second (first (first controls))))
                    (declare (ignore _))
                    (is (equal '("remove draft" "") (reverse ran)) "Run runs it, and the card is read again")
                    (is (search (format nil "did remove draft~%~%## Jobs (2)") text))))))
            (is (equal '("jobs: 2" nil nil)
                       (multiple-value-list (nck::run-command plain channel "/jobs"))) "no choices, no card")))))))

(deftest channel-a-dialog-card-is-a-panel-of-lines ()
  ;; A dialog of few rows is a panel (variant C of /context's room card):
  ;; its tone the panel's, its picture one row of squares, each pressable
  ;; row a line in its tone's square with the button its hint names, a
  ;; person's handle bracket gone from a label; past six pressable rows
  ;; they go in a menu that says what Enter does.
  (let* ((square (nck::tone-square "context-prompt"))
         (blank (cdr (assoc :black nck::+tone-squares+)))
         (dialog (nle::dialog-json
                  (nle:list-dialog
                   :title "Context" :context "2k of 4k · 50%" :summary "evicts past 85%" :tone :success
                   :hint "enter open  esc close"
                   :picture (list :rows '("0011") :palette '("" "context-prompt") :strip "0011")
                   :rows (list (list :label "standing prompt" :detail "400    10%" :mark "██" :mark-tone :context-prompt
                                     :run (list :enter "/context prompt"))
                               (list :label "peas (operator) [m15 u93]: hello" :run (list :enter "/context history"))
                               (list :label "free" :detail "2k"))))))
    (is square "the palette paints the part's tone")
    (multiple-value-bind (text controls panel) (nck::dialog-card dialog "/context")
      (is (search "## Context" text) "the words stand for a platform with no panel")
      (is (equal (format nil "~a~a~a~a" blank blank square square) (getf panel :gauge)) "the strip, a square a cell")
      (is (equal '("Context" "2k of 4k · 50%" :done "evicts past 85%")
                 (list (getf panel :title) (getf panel :context) (getf panel :tone) (getf panel :text))) "its rows are lines, not the panel's words")
      (is (equal `((:row ,square "standing prompt" "400 · 10%" ("Open" "/context prompt" :secondary nil))
                   (:row nil "peas (operator): hello" nil ("Open" "/context history" :secondary nil))
                   (:row nil "free" "2k" nil)
                   (("Refresh" "/context" :secondary nil)))
                 (press-lines controls))))
    (let ((many (nle::dialog-json
                 (nle:list-dialog :hint "enter evict through  esc back"
                                  :rows (loop for i below 7
                                              collect (list :label (format nil "t~d" i)
                                                            :run (list :enter (format nil "/evict through t~d" i))))))))
      (is (equal "Evict through…" (second (first (nth-value 1 (nck::dialog-card many "/context history"))))) "seven rows keep a menu that says what a pick does"))
    (let ((own (nlk:json-object "rows" (vector (nlk:json-object "label" "Sitting in" "detail" "#General" "verb" "leave"
                                                                "run" (nlk:json-object "enter" "/voice leave"))))))
      (is (equal '((:row nil "Sitting in" "#General" ("Leave" "/voice leave" :secondary nil))
                   (("Refresh" "/voice" :secondary nil)))
                 (press-lines (nth-value 1 (nck::dialog-card own "/voice"))))
          "a row that names its own press is pressed by that word, over the hint's"))))

(deftest channel-a-dialog-card-draws-a-rows-options-as-its-menu ()
  ;; A row that offers picks (NLE:LIST-DIALOG's OPTIONS) is a menu in its
  ;; place, each option saying the row it is a pick of, the one in use the
  ;; one shown picked, a pick saying its own line; the words still say it.
  (let ((dialog (flet ((row (label detail mark enter)
                         (nlk:json-object "label" label "detail" detail "mark" mark
                                          "run" (nlk:json-object "enter" enter))))
                  (let ((sitting (row "Sitting in" "#General" "" "/voice leave"))
                        (speaks (row "Speaks with" "kokoro bf_isabella" "" "/voice speaks")))
                    (setf (gethash "verb" sitting) "leave"
                          (gethash "options" speaks) (vector (row "bf_isabella" "kokoro" "●" "/voice speaks bf_isabella")
                                                             (row "af_heart" "kokoro" "" "/voice speaks af_heart")))
                    (nlk:json-object "rows" (vector sitting speaks))))))
    (multiple-value-bind (text controls) (nck::dialog-card dialog "/voice")
      (is (search "- Speaks with · kokoro bf_isabella" text) "the words say the row")
      (is (equal '((:row nil "Sitting in" "#General" ("Leave" "/voice leave" :secondary nil))
                   (:menu "Speaks with…" (("Speaks with · bf_isabella" "/voice speaks bf_isabella" "kokoro" t)
                                         ("Speaks with · af_heart" "/voice speaks af_heart" "kokoro" nil)))
                   (("Refresh" "/voice" :secondary nil)))
                 (press-lines controls))))))

(deftest channel-a-dialog-card-pages-its-rows-and-tabs ()
  ;; A tabbed dialog's card shows a row of its tabs, the one open primary,
  ;; each saying the card's own line on that tab; a menu holds 25, so a
  ;; longer list pages, Prev and Next each the card on the page they go to;
  ;; a card opened from another carries Back to it, on the view it had.
  (with-temp-store ()
    (let ((dialog (nlk:json-object
                   "title" "Usage"
                   "tabs" (vector (nlk:json-object "label" "All") (nlk:json-object "label" "Odd" "value" "odd" "count" 15))
                   "rows" (coerce (loop for i below 30
                                        collect (nlk:json-object "label" (format nil "row ~d" i)
                                                                 "key" (if (oddp i) "odd" "")
                                                                 "value" (format nil "/usage ~d" i)))
                                  'vector))))
      (multiple-value-bind (text controls) (nck::dialog-card dialog "/usage" :page 2 :back "/status" :back-page 3)
        (is (search "## Usage" text))
        (is (equal '(("All" "/usage" :primary nil) ("Odd (15)" "/usage" :secondary nil))
                   (press-lines (first controls))))
        (is (equal '("row 25" "row 26" "row 27" "row 28" "row 29")
                   (mapcar #'first (third (second controls)))) "the second page of All")
        (is (equal '(("Prev" "/usage" :secondary nil) ("Page 2/2" "nck:page" :secondary t) ("Next" "/usage" :secondary t))
                   (press-lines (third controls))))
        (is (eql 1 (nlk:json-value (nck:press-action (second (first (third controls)))) :integer "page")))
        (is (equal '(("Back" "/status" :secondary nil) ("Refresh" "/usage" :secondary nil))
                   (press-lines (fourth controls))))
        (is (eql 3 (nlk:json-value (nck:press-action (second (first (fourth controls)))) :integer "page")) "Back opens the card it came from on its page")
        (is (equal "/usage" (nlk:json-value (nck:press-action (second (first (third (second controls))))) :text "back")) "a row's way back is this card, not the one it came from"))
      (multiple-value-bind (text controls) (nck::dialog-card dialog "/usage" :tab 1)
        (declare (ignore text))
        (is (equal '(("All" "/usage" :secondary nil) ("Odd (15)" "/usage" :primary nil))
                   (press-lines (first controls))))
        (is (equal 15 (length (third (second controls)))) "the Odd tab's rows alone, one page")
        (is (equal '((("Refresh" "/usage" :secondary nil))) (press-lines (last controls))))))))

(deftest channel-a-command-in-a-room-may-answer-with-a-card ()
  ;; A command run in a room may offer a card (NCK:OFFER-CARD): controls,
  ;; whose presses say their lines, and a panel the platform draws in place of
  ;; its words. Run anywhere else it offers nothing, and nothing is left for
  ;; the next one. Answered through a held interaction, the panel rides the
  ;; answer, which is one message.
  (with-temp-store ()
    (let ((card (list (list (nck:choice "Join" "/voice join"))))
          (panel '(:title "Voice" :text "Not in a voice channel." :tone :stopped)))
      (with-stubbed-fdefinition (nle:slash (line &key session-id)
                                  (nck:offer-card :controls card :panel panel)
                                  (values (format nil "ran ~a" line) nil))
        (let* ((executor (nck:make-recording-executor))
               (host (test-host :name "command-card" :platform (test-platform :choices t)
                                :executor executor)))
          (is (equal (list "ran /voice" card panel)
                     (multiple-value-list
                      (nck::run-command host (test-candidate :channel "100") "/voice"))))
          (is (equal '("ran /help" nil nil)
                     (multiple-value-list
                      (with-stubbed-fdefinition (nle:slash (line &key session-id)
                                                  (values (format nil "ran ~a" line) nil))
                        (nck::run-command host (test-candidate :channel "100") "/help")))))
          (nck::respond host (test-press "Join" "c1")
                        (lambda () (values (format nil "words~%~a" (make-string 3000 :initial-element #\x))
                                          card panel)))
          (flet ((labelled (label)
                   (find label (nck:recording-executor-plans executor)
                         :key #'nck:request-plan-audit-label :test #'equal)))
            (is-present (answer (labelled "interaction_response")) "the press is answered in place"
              (is-plan answer "panel" panel))
            ;; A panel's words past the first chunk are not posted after it.
            (is (null (labelled "send_message")))))))
    (is (equal '(x) (nck:offer-card :controls '(x))))
    (is (null nck::*command-card*))))

(deftest channel-fetch-image-brings-a-picture-down ()
  ;; The borrow half: an image URL becomes a file a lane can post — its
  ;; bytes sniffed, named from the URL, and written where files to post
  ;; live. The network is stubbed; the composition is this test's, a real
  ;; fetch is the live proof's.
  (let ((nck::*image-cache-directory* (temp-path "fetched-images" nil))
        (octets (lambda () (test-png-octets))))
    (with-stubbed-fdefinition (nck::url-octets (url &key timeout) (funcall octets))
      (is-values (path media-type)
          (nck:fetch-image "https://example.test/pea.png")
        (media-type "image/png")
        ((pathname-type path) "png")
        ((search "pea-" (file-namestring path)) is)
        ((probe-file path) is "the file is there to post")
        ((plusp (nlk:file-bytes path)) is)
        ((file-namestring (nck:fetch-image "https://example.test/pea.png"))
         (file-namestring path)))
      (is-values (path media-type)
          (nck:fetch-image "https://example.test/anything"
                           :filename "poster.png")
        ((file-namestring path) "poster.png")
        (media-type "image/png"))
      (setf octets (lambda ()
                     (sb-ext:string-to-octets "<html>not a picture</html>")))
      (is (search "did not answer with an image"
                  (refusal-text error (nck:fetch-image "https://example.test/page"))))
      (setf octets (lambda () (test-png-octets (1+ nle:*image-max-bytes*))))
      (is (search "over the" (refusal-text error (nck:fetch-image "https://example.test/huge.png")))))))

(deftest channel-host-the-card-carries-stop-and-details (with-digest-host (host lane))
  ;; A running card carries Stop beside Details on one row; a settled one
  ;; carries Details alone, the card staying as the turn's record; a queued
  ;; ask's carries Raise limit, which lets the line through.
  (fact "turn.started" (nlk:json-object))
  (let ((digest (nck:lane-digest lane)))
    (is (equal (list (list (list "Stop" "nck:stop:chat-123-m1" :danger)
                           (list "Details" "nck:details:chat-123-m1" :secondary)))
               (nck::line-controls lane digest)))
    (nck:digest-note-terminal digest :completed nil (nck:now-ms))
    (is (equal (list (list "Details" "nck:details:chat-123-m1" :secondary))
               (nck::line-controls lane digest)))
    (nck:digest-note-queued digest 1)
    (is (equal (list (list (list "Raise limit" "nck:lift" :primary)))
               (nck::line-controls lane digest)))))

(deftest channel-host-raise-limit-is-the-operators-and-lets-the-line-through ()
  ;; A queued card carries Raise limit. A room operator's press doubles the
  ;; turns that run at once, live and in the config so a restart keeps it, and
  ;; lets the line through at once; anyone else's is answered, to them alone,
  ;; with whose it is, and moves nothing.
  (handler-bind ((warning #'muffle-warning))
    (with-temp-file (nodecode.evolved::*shared-config-path*
                     :type "jsonc" :contents "{\"channels\": {\"test\": {}}}" :setf t)
      (with-digest-host (host nil :responses (replies "s1" "ok" "ok" "ok" "ok" "ok")
                         :name "lift" :session "chat-1-m7"
                         :host-keys (:max-concurrent 1 :platform (test-platform :choices t)))
        ;; The one slot is taken; the ask waits behind it.
        (is (nck:claim-slot host (room-1-ask "m6") 1))
        (nck::gate-ask host (room-1-ask "m7"))
        (is-present (queued (await-plan executor :label "send_message"))
          "the queued line carries Raise limit"
          (is (equal (list (list (list "Raise limit" "nck:lift" :primary)))
                     (plan-field queued "controls"))))
        (flet ((lift (id user)
                 (nck:control-pressed host (list :id id :user-id user :message-id "s1" :channel-id "1"
                                                 :data "nck:lift"))
                 (plan-field (await-plan executor :path (format nil "/presses/~a" id)) "text"))
               (waiting ()
                 (nck::with-room-book (book host) (length (nck::room-book-pending book)))))
          (is (equal "Only the room's operators can raise the limit (1 turn at once): ask one of them."
                     (lift "i1" "alice")))
          (is (= 1 (nck::host-max-concurrent-turns host)) "anyone else's press moves nothing")
          (is (= 1 (waiting)) "and the ask still waits")
          (is (equal "Raised to 2 turns at once (was 1); 1 waiting ask started. Saved as channels.test.max_concurrent_turns."
                     (lift "i2" "mike")))
          (is (= 2 (nck::host-max-concurrent-turns host)))
          (is (zerop (waiting)) "the line went through")
          (let ((config (uiop:read-file-string nodecode.evolved::*shared-config-path*)))
            (is (ppcre:scan "\"max_concurrent_turns\":\\s*2" config) "the config keeps it")))))))

(deftest channel-host-ask-for-access-reaches-the-operators-and-lets-them-in ()
  ;; A refused command carries Ask for access. The refused person's press
  ;; posts one request into the room, addressed to its operators and carrying
  ;; Make operator, at most once in ten minutes a person; an operator's press
  ;; of that makes them one, live and in the config so a restart keeps it, and
  ;; tells them in the room. Anyone else's press moves nothing; every press is
  ;; answered to the presser alone.
  (handler-bind ((warning #'muffle-warning))
    (with-temp-file (nodecode.evolved::*shared-config-path*
                     :type "jsonc" :contents "{\"channels\": {\"test\": {}}}" :setf t)
      (with-digest-host (host nil :name "access"
                         :host-keys (:owners '("mike") :platform (test-platform :choices t)))
        (setf (nck:host-policy host) (channel-policy :allowed-channels '("123")))
        (clrhash nck::*access-asked*)
        (say "/models" "alice" "m1")
        (is (posted 1))
        (is (equal (list (list (list "Ask for access" "nck:access:models" :primary)))
                   (plan-field (first (plans)) "controls")) "the refusal carries Ask for access")
        (flet ((tap (id user data)
                 (nck:control-pressed host (list :id id :user-id user :user-name user :message-id "s1"
                                                 :channel-id "123" :data data))
                 (plan-field (await-plan executor :path (format nil "/presses/~a" id)) "text"))
               (requests () (plans-matching (plans) :label "send_message" :content "asks to run /models")))
          (is (equal "Asked: the room's operators got your request, and can let you in with one press."
                     (tap "i1" "alice" "nck:access:models")))
          (is-present (request (first (requests))) "one request reaches the room, to its operators"
            (is-plan request :path "/rooms/123/messages")
            (is (equalp (vector "mike") (plan-field request "mentions")))
            (is (equal (list (list (list "Make alice an operator" "nck:grant:alice" :primary)))
                       (plan-field request "controls"))))
          (is (equal "Already asked: the room's operators have your request."
                     (tap "i2" "alice" "nck:access:models")))
          (is (= 1 (length (requests))) "a second press within ten minutes posts nothing")
          (is (equal "Only the room's operators can make someone an operator."
                     (tap "i3" "alice" "nck:grant:alice")))
          (is (equal '("mike") (nck::host-owners host)) "anyone else's press moves nothing")
          (is (equal "Made an operator (2 now). Saved as channels.test.owner."
                     (tap "i4" "mike" "nck:grant:alice")))
          (is (equal '("mike" "alice") (nck::host-owners host)) "live at once")
          (is-present (told (await-plan executor :label "send_message" :content "you're now one of this room's operators"))
            "the person is told in the room"
            (is (equalp (vector "alice") (plan-field told "mentions"))))
          (let ((config (uiop:read-file-string nodecode.evolved::*shared-config-path*)))
            (is (ppcre:scan "\"owner\":\\s*\\[\\s*\"mike\",\\s*\"alice\"\\s*\\]" config) "the config keeps it"))
          (is (equal "Already one of this room's operators." (tap "i5" "mike" "nck:grant:alice")))
          (is (equal "You're already one of this room's operators: run the command again."
                     (tap "i6" "alice" "nck:access:models"))))))))

(deftest channel-host-a-usage-fact-meters-the-card (with-digest-host (host lane))
  ;; A round's usage fact, in the keys the engine writes it under, reaches the
  ;; card's footer as the TUI reads it: what the round ran on, the meter with
  ;; its cache, write and reasoning marks, the prompt in the window, the price.
  (fact "turn.started" (nlk:json-object))
  (fact "turn.usage" (nlk:json-object "input-tokens" 1200 "output-tokens" 40 "cached-input-tokens" 8800
                                      "cache-write-tokens" 300 "reasoning-tokens" 12
                                      "cost_known" t "cost_usd" 0.5 "provider" "qa"
                                      "model" "qa-model" "reasoning-effort" "high"))
  ;; The model id leads with its provider, so the TUI's words name it once.
  (is (equal (format nil "qa-model (high)~%↑1.2k c88.0% w300 ↓40 r12 · ctx 10.3k · $0.500")
             (getf (nck:digest-card (nck:lane-digest lane) (nck:now-ms)) :meta))))

(deftest channel-host-details-answer-the-presser-alone ()
  ;; A Details press answers the person who pressed, and no one else, with
  ;; every step and the newest thought: the running turn's from its digest,
  ;; a settled card's from what it kept when it settled. Stop's rule says who
  ;; may: the asker and the room's operators.
  (with-digest-host (host lane :responses (replies "s1" "s1" "a1" "s1")
                               :host-keys (:platform (test-platform :choices t)))
    (fact "turn.started" (nlk:json-object))
    (called "c1" "{\"form\": \"(sh \\\"just lint\\\")\"}")
    (fact "turn.tool_result" (nlk:json-object "call-id" "c1" "result" "lint: clean"))
    (nck:flush-now host lane)
    (flet ((details (id user &optional (card "s1"))
             (nck:control-pressed host (list :id id :user-id user :message-id card :channel-id "123"
                                             :data "nck:details:chat-123-m1"))
             (plan-field (await-plan executor :path (format nil "/presses/~a" id)) "text")))
      (is-carrying (text (details "i1" "mike"))
        "**Steps** · 1" "1. ✓ Ran just lint" "lint: clean")
      (is (equal "Details are for the person who asked and the room's operators."
                 (details "i2" "alice")))
      (said "all clean")
      (fact "turn.completed" (nlk:json-object))
      (is (await-plan executor :label "edit_message" :content "Done in"))
      (is-carrying (text (details "i3" "mike")) "1. ✓ Ran just lint"
        (is (search "lint: clean" text) "a settled card's details are what it kept"))
      (is (equal "This card's steps are no longer held." (details "i4" "mike" "gone"))))))

(deftest channel-host-a-steps-output-answers-the-presser-alone ()
  ;; A step's Output press answers the person who pressed, and no one else,
  ;; with the end of what that step answered: the running turn's from its
  ;; digest, a settled card's from what it kept. Details' rule says who may.
  (with-digest-host (host lane :responses (replies "s1" "s1" "a1" "s1")
                               :host-keys (:platform (test-platform :choices t)))
    (fact "turn.started" (nlk:json-object))
    (called "c1" "{\"form\": \"(sh \\\"just lint\\\")\"}")
    (fact "turn.tool_result" (nlk:json-object "call-id" "c1" "result" (format nil "checking~%lint: clean")))
    (nck:flush-now host lane)
    (is-present (card (await-plan executor :label "send_message"))
      "the card posts its step with its press"
      (is (equal "nck:step:chat-123-m1:1" (fourth (first (getf (plan-field card "card") :steps))))))
    (flet ((output (id user &optional (press "nck:step:chat-123-m1:1"))
             (nck:control-pressed host (list :id id :user-id user :message-id "s1" :channel-id "123"
                                             :data press))
             (plan-field (await-plan executor :path (format nil "/presses/~a" id)) "text")))
      (is-carrying (text (output "i1" "mike"))
        "**1. ✓ Ran just lint" (format nil "```~%checking~%lint: clean~%```"))
      (is (equal "Details are for the person who asked and the room's operators." (output "i2" "alice")))
      (said "all clean")
      (fact "turn.completed" (nlk:json-object))
      (is (await-plan executor :label "edit_message" :content "Done in"))
      ;; A settled card's step is what it kept.
      (is (await (:timeout 10) (gethash "s1" nck::*card-details*)))
      (is-carrying (text (output "i3" "mike")) (format nil "checking~%lint: clean"))
      (is (equal "This step is no longer held." (output "i4" "mike" "nck:step:chat-123-m1:7"))))))

;;; --- a stop in the middle of a turn ----------------------------------------------

(defun room-123-ask (message &key (owner "u9") queued-at)
  "The ask MESSAGE from OWNER opens in room 123, as its lane chat-123-MESSAGE,
first waiting at QUEUED-AT."
  (let ((ask (nck:make-ask :room "chat-123" :lane (format nil "chat-123-~a" message) :owner-id owner
                           :message-id message :target '(:channel-id "123") :prompt "and this"
                           :contract "the room's contract" :said "and this")))
    (when queued-at
      (setf (nck::queued-since ask) queued-at))
    ask))

(deftest channel-host-a-stop-hands-its-running-lanes-on (with-temp-store ())
  ;; A host stopping under a running turn says so on the turn's line and
  ;; writes the lane down; the next start takes it back on the same line, and
  ;; the turn — resumed by the boot, or still running — answers there. A
  ;; queued ask waits on: its line says so, it is written down as it queued,
  ;; and the next start puts it back at the gate on that line.
  (let* ((first-executor (nck:make-recording-executor :responses (replies "s1" "s2")))
         (host (test-host :executor first-executor :name "handoff" :max-concurrent 0))
         (lane (nck:intern-lane (nck:host-lanes host) "chat-123-m1"
                                :target '(:channel-id "123") :trigger-message-id "m1"
                                :owner-id "u8")))
    (nck:start-host host)
    (fold-fact host "chat-123-m1" "turn.started" (nlk:json-object))
    (fold-fact host "chat-123-m1" "turn.tool_call_started" (nlk:json-object "tool-name" "eval"))
    (nck:flush-now host lane)
    (nck::gate-ask host (room-123-ask "m2"))
    (is (nlk:session-state-get "channel-test" nck::+queued-key+) "written down as it queued")
    (nck:flush-now host (nck:find-lane (nck:host-lanes host) "chat-123-m2"))
    (nck:stop-host host)
    (let ((plans (recorded-plans first-executor)))
      (is-present (paused (plan-matching plans :method "PATCH" :content "Paused at"))
        "the running line says the turn is paused, and picks up"
        (is (search nck::+paused-detail+ (plan-field paused "content")))
        (is (search "/s1" (nck:request-plan-path paused))))
      ;; The queued line says it waits on.
      (is-present (held (plan-matching plans :method "PATCH" :content nck::+held-detail+))
        (is (search "/s2" (nck:request-plan-path held)))))
    (let ((held (nlk:decode-json (nlk:session-state-get "channel-test" nck::+queued-key+))))
      (is (= 1 (length held)))
      (is (equal "s2" (nlk:json-value (aref held 0) :string "status")) "with the line it shows")))
  ;; The next start: the same line, the same turn, and its answer lands; the
  ;; waiting ask waits again on its own line, behind the lane's slot.
  (let* ((executor (nck:make-recording-executor))
         (host (test-host :executor executor :name "handoff-2" :max-concurrent 1)))
    (nck:start-host host)
    (unwind-protect
         (is-present (lane (nck:find-lane (nck:host-lanes host) "chat-123-m1"))
           "the running lane is taken back"
           (is (equal "t1" (nck:lane-active-turn-id lane)))
           (let ((digest (nck:lane-digest lane)))
             (is (equal "s1" (nck:turn-digest-status-id digest)))
             (is (eq :running (nck:turn-digest-phase digest)))
             (is (= 1 (nck:turn-digest-tool-calls digest))))
           (is (equal '(:channel-id "123") (nck:lane-target lane)))
           (is-present (waiting (nck:find-lane (nck:host-lanes host) "chat-123-m2"))
             "the waiting ask is back"
             (let ((digest (nck:lane-digest waiting)))
               (is (equal "s2" (nck:turn-digest-status-id digest)) "on the line it had")
               (is (eq :queued (nck:turn-digest-phase digest)))))
           (is-present (ask (first (nck::with-room-book (book host) (copy-list (nck::room-book-pending book)))))
             "at the gate, whole"
             (is (equal '("chat-123-m2" "m2" "and this" "the room's contract" (:channel-id "123"))
                        (list (nck::ask-lane ask) (nck::ask-message-id ask) (nck::ask-prompt ask)
                              (nck::ask-contract ask) (nck::ask-target ask))))
             (is (integerp (nck::queued-since ask))))
           (fold-fact host "chat-123-m1" "turn.assistant_message_completed"
                      (nlk:json-object "message" (nlk:json-object "content" "done at last")))
           (fold-fact host "chat-123-m1" "turn.completed" (nlk:json-object))
           (is (await-plan executor :content "done at last") "the answer lands")
           ;; What was written down is taken.
           (is (null (nlk:session-state-get "channel-test" nck::+handoff-key+))))
      (nck::release-slot host "u9")
      (nck:stop-host host))))

(defun crash-host (host)
  "What a power cut leaves of HOST: its threads and hooks gone, and nothing
it would have done on the way out (STOP-HOST's hand-off)."
  (setf nck::*hosts* (remove host nck::*hosts*))
  (dolist (point (list :frame 'nle:turn-budget 'nle:name-session))
    (nle:unhook point (format nil "channel-~a" (nck:host-id host))))
  (nck::stop-delivery-worker (nck::host-worker host)))

(deftest channel-host-a-crash-hands-its-running-lanes-on (with-temp-store ())
  ;; The process dies under a running turn — the cord pulled, a kill -9 —
  ;; and no stop runs: the lane was written down while it ran, so the next
  ;; start takes it back on the same line as a stop's hand-off would, and the
  ;; turn the boot resumes answers there.
  (let* ((first-executor (nck:make-recording-executor :responses (replies "s1")))
         (host (test-host :executor first-executor :name "crash"))
         (lane (nck:intern-lane (nck:host-lanes host) "chat-123-m1"
                                :target '(:channel-id "123") :trigger-message-id "m1"
                                :owner-id "u8")))
    (nck:start-host host)
    (fold-fact host "chat-123-m1" "turn.started" (nlk:json-object))
    (fold-fact host "chat-123-m1" "turn.tool_call_started" (nlk:json-object "tool-name" "eval"))
    (bt2:with-lock-held ((nck::lane-lock lane))
      (setf (nck::turn-digest-task (nck:lane-digest lane)) "Checkout failure"))
    (nck:flush-now host lane)
    (is (loop repeat 40
              thereis (let ((json (nlk:session-state-get "channel-test" nck::+handoff-key+)))
                        (and json (search "\"s1\"" json) (search "Checkout failure" json)))
              do (sleep 0.1)) "written down, with its line, while it runs")
    (crash-host host)
    (is (null (plan-matching (recorded-plans first-executor) :method "PATCH" :content "Paused at")) "a dead process says nothing on the way out"))
  (let* ((executor (nck:make-recording-executor))
         (host (test-host :executor executor :name "crash-2")))
    (nck:start-host host)
    (unwind-protect
         (is-present (lane (nck:find-lane (nck:host-lanes host) "chat-123-m1"))
           "the running lane is taken back"
           (is (equal "t1" (nck:lane-active-turn-id lane)))
           (let ((digest (nck:lane-digest lane)))
             (is (equal "s1" (nck:turn-digest-status-id digest)) "on the line it had")
             (is (equal "Checkout failure" (nck::turn-digest-task digest)) "wearing the title it had")
             (is (eq :running (nck:turn-digest-phase digest)))
             (is (= 1 (nck:turn-digest-tool-calls digest))))
           (fold-fact host "chat-123-m1" "turn.assistant_message_completed"
                      (nlk:json-object "message" (nlk:json-object "content" "back where it was")))
           (fold-fact host "chat-123-m1" "turn.completed" (nlk:json-object))
           (is (await-plan executor :content "back where it was") "the answer lands")
           (is (loop repeat 40
                     thereis (null (nlk:session-state-get "channel-test" nck::+handoff-key+))
                     do (sleep 0.1)) "an answered turn is no longer written down"))
      (nck:stop-host host))
    (is (null (nlk:session-state-get "channel-test" nck::+handoff-key+)) "a stop with nothing running leaves nothing to take back")))

(deftest channel-host-an-ask-that-waited-too-long-is-dropped-on-its-line (with-temp-store ())
  ;; A downtime past the horizon must not resurrect yesterday's backlog: the
  ;; ask is not run, and its line says to ask again.
  (nlk:session-state-put "channel-test" nck::+queued-key+
                         (nlk:encode-json-object
                          (vector (nck::ask-handoff (room-123-ask "m3" :queued-at (- (get-universal-time) 7200))
                                                    "s3"))))
  (let* ((executor (nck:make-recording-executor :responses (replies "s3")))
         (host (test-host :executor executor :name "stale")))
    (nck:start-host host)
    (unwind-protect
         (progn
           (is (null (nck::with-room-book (book host) (nck::room-book-pending book))) "nothing waits")
           (is-present (dropped (await-plan executor :method "PATCH" :content nck::+stale-detail+))
             (is (search "/s3" (nck:request-plan-path dropped))))
           (is (null (nlk:session-state-get "channel-test" nck::+queued-key+))))
      (nck:stop-host host))))

(deftest channel-reply-to-a-picture-reads-it-in ()
  ;; "what's wrong here", typed as a reply to someone's screenshot: the ask
  ;; carries the picture it answers, read in under the reply's line, as it
  ;; carries a voice note's words.
  (let* ((host (test-host))
         (ask (test-candidate :text "<@999> what's wrong here" :message "9" :user-name "kim"
                              :reply "" :reply-to "7" :reply-name "bob")))
    (setf (gethash "attachments" (gethash "reply" ask))
          (vector (nlk:json-object "id" "shot" "fetch" (lambda () (test-png-octets))
                                   "media_type" "image/png" "filename" "ci.png")))
    (is-values (prompt images) (nck::ask-prompt-and-attachments
                                (nck:build-ask host ask "chat-100-m9" nil))
      ((search "↩ bob [m7] [Image #1]" prompt) is)
      ((length images) = 1))))

(deftest channel-host-a-line-someone-replied-to-is-settled-not-deleted ()
  ;; A status line goes when the answer lands, unless someone replied to it:
  ;; then it is settled in place, and their reply still points at something.
  (with-digest-host (host lane :responses (replies "ok" "ok"))
    (bt2:with-lock-held ((nck::lane-lock lane))
      (setf (nck::turn-digest-status-id (nck::lane-open-digest lane 0)) "s1"))
    (nck::note-replied-line lane "s1")
    (nck::note-replied-line lane "s9")
    (nck::retire-chrome host lane "s1")
    (is-plan (first (plans)) :method "PATCH" :path "/rooms/123/messages/s1" "content" "answered below")
    ;; A card's message stays a card.
    (is (equal "answered below" (getf (plan-field (first (plans)) "card") :headline)))
    (nck::retire-chrome host lane "s2")
    (is-plan (second (plans)) :method "DELETE" :path "/rooms/123/messages/s2")
    (is (null (gethash "s9" nck::*replied-lines*)) "a reply to anything but the line marks nothing")))

(deftest channel-host-the-card-wears-its-asks-title ()
  ;; A lane's session is named from its ask's own words, never the room's
  ;; envelope its prompt carries, and the card opened under the ask's first
  ;; words wears the title it is given. Every other session is the kernel's.
  (let ((asked '())
        (said "lint is red on main, find out why and fix it if it's small"))
    (flet ((next (session-id text)
             (push (list session-id text) asked)
             "Fix the red lint on main"))
      (with-digest-host (host lane :responses (replies "s1" "s1"))
        (setf (nck::lane-said lane) said
              (nck::host-status-update-ms host) 0)
        (earn-line)
        (is-present (post (first (plans))) "the card opens under the ask's first words"
          (is (equal "lint is red on main" (getf (gethash "card" (nck:request-plan-body post)) :task)))
          (is (uiop:string-prefix-p (format nil "lint is red on main~%") (plan-content post))))
        (is (equal "Fix the red lint on main"
                   (funcall (nck::naming-advice host) #'next "chat-123-m1" "[room] lint is red...")))
        (is (equal (list (list "chat-123-m1" said)) asked) "named from the ask's own words")
        (nck:flush-now host lane)
        (is-present (edit (second (plans))) "the card takes the title"
          (is-plan edit :label "edit_message")
          (is (equal "Fix the red lint on main" (getf (gethash "card" (nck:request-plan-body edit)) :task))))
        (funcall (nck::naming-advice host) #'next "chat-9-m2" "a shell's own prompt")
        (is (equal '("chat-9-m2" "a shell's own prompt") (first asked)) "no lane's, the kernel's")))))

(deftest channel-host-a-late-title-retitles-the-settled-card ()
  ;; Naming never holds the turn: the turn answers meanwhile and its card
  ;; settles under the ask's first words; the title, landing after, retitles
  ;; the settled card in place — the whole card, its Details and the pictures
  ;; its message holds restated.
  (let ((said "lint is red on main, find out why and fix it if it's small"))
    (with-digest-host (host lane :responses (make-list 8 :initial-element
                                                       (reply 200 "id" "s1" "media" '(("step-1.png" . "900"))))
                                 :host-keys (:platform (test-platform :choices t)))
      (setf (nck::lane-said lane) said)
      (earn-line)
      (said "all clean")
      (fact "turn.completed" (nlk:json-object))
      (is (await-plan executor :content "all clean") "the answer did not wait on the title")
      (is-present (settle (await-plan executor :label "edit_message" :content "Done in"))
        "the card settled under the ask's first words"
        (is (equal "lint is red on main" (getf (gethash "card" (nck:request-plan-body settle)) :task)))
        (is (equal '(("step-1.png" . "900")) (plan-field settle "media")) "keeping what its message holds"))
      (funcall (nck::naming-advice host)
               (lambda (session-id text)
                 (declare (ignore session-id text))
                 "Fix the red lint on main")
               "chat-123-m1" said)
      (is-present (retitle (await-plan executor :label "edit_message" :content "Fix the red lint on main"))
        "the late title retitled the settled card"
        (is-plan retitle :path "/rooms/123/messages/s1")
        (is-shape (gethash "card" (nck:request-plan-body retitle))
          (:task "Fix the red lint on main") (:state :done))
        (is (equal '("Details") (mapcar #'first (plan-field retitle "controls"))))
        (is (equal '(("step-1.png" . "900")) (plan-field retitle "media")))))))


(deftest channel-host-failed-tool-results-are-not-successful-steps ()
  ;; Fold the durable flag, not words that happen to mention an error.
  (with-digest-host (host lane)
    (fact "turn.started" (nlk:json-object))
    (called "failed" "{\"form\": \"(error \\\"boom\\\")\"}")
    (fact "turn.tool_result" (nlk:json-object "call-id" "failed" "result" "boom" "failed" t))
    (called "ok")
    (fact "turn.tool_result" (nlk:json-object "call-id" "ok" "result" "error count: 0" "failed" nil))
    (called "stopped")
    (fact "turn.tool_result"
          (nlk:json-object "call-id" "stopped" "result" "interrupted by user" "failed" nil
                           "metadata" (nlk:json-object "stopped" (nlk:json-object "by" "user"))))
    (called "open")
    (let* ((digest (nck:lane-digest lane))
           (card (nck:digest-card digest (nck:now-ms)))
           (details (nck:digest-details digest (nck:now-ms))))
      (is (equal '(:failed :done :stopped :running) (mapcar #'first (getf card :steps))))
      (is (equal '(:failed :done :stopped :running) (mapcar #'first (getf details :steps))))
      (is (search "❌" (nck:card-text card)))
      (is (search "1. ❌" (nck:details-text details 2000)))
      (is (search "❌" (nck::step-text (first (getf details :steps)) 2000)))
      ;; Settling the turn changes only its still-open call, not past outcomes.
      (nck:digest-note-terminal digest :completed nil (nck:now-ms))
      (is (equal '(:failed :done :stopped :stopped)
                 (mapcar #'first (getf (nck:digest-card digest (nck:now-ms)) :steps)))))))

(deftest channel-answer-in-a-thread-carries-close ()
  ;; An answer in a thread the platform can close carries Close under its
  ;; words, naming who asked; a flat answer, or one on a platform that cannot
  ;; close a thread, carries none.
  (flet ((controls (platform target)
           (with-digest-host (host nil :name "close-row" :responses (replies "a1")
                                       :host-keys (:platform platform))
             (let ((lane (nck:intern-lane (nck:host-lanes host) "chat-123-t42-m5"
                                          :target target :trigger-message-id "5")))
               (setf (nck:lane-owner-id lane) "alice")
               (nck::deliver-answer host lane (nck:make-turn-digest :turn-id "t9" :phase :completed
                                                                    :answer "Landed."))
               (plan-field (plan-matching (nck:recording-executor-plans executor) :content "Landed.")
                           "controls")))))
    (is (equal '((("Close thread" "nck:close:alice" :secondary)))
               (controls (test-platform :choices t :close t) '(:channel-id "123" :thread-id "42"))))
    (is (null (controls (test-platform :choices t :close t) '(:channel-id "123"))) "a flat answer has no thread to close")
    (is (null (controls (test-platform :choices t) '(:channel-id "123" :thread-id "42"))) "nor has a platform that cannot close one")))

(deftest channel-close-press-is-the-askers-or-an-operators ()
  ;; Close closes the thread the press came from when the person who asked
  ;; or a room operator presses it; anyone else's press, and one while a turn
  ;; still runs there, closes nothing. Every press is answered to the presser
  ;; alone.
  (handler-bind ((warning #'muffle-warning))
    (with-digest-host (host nil :name "close-press"
                                :responses (replies "ok" "ok" "ok" "ok" "ok" "ok" "ok" "ok")
                                :host-keys (:owners '("mike") :platform (test-platform :choices t :close t)))
      (let ((lane (nck:intern-lane (nck:host-lanes host) "chat-123-t42-m5"
                                   :target '(:channel-id "123" :thread-id "42")
                                   :trigger-message-id "5")))
        (setf (nck:lane-owner-id lane) "alice")
        (flet ((tap (id user)
                 (nck:control-pressed host (list :id id :user-id user :user-name user :message-id "a1"
                                                 :channel-id "42" :data "nck:close:alice"))
                 (plan-field (await-plan executor :path (format nil "/presses/~a" id)) "text"))
               (closes ()
                 (length (plans-matching (nck:recording-executor-plans executor) :label "close_thread"))))
          (is (equal "Only the person who asked or the room's operators can close this thread."
                     (tap "i1" "bob")))
          (setf (nck:lane-active-turn-id lane) "t2")
          (is (equal "A turn still runs here: close the thread once it has answered, or stop it first."
                     (tap "i2" "alice")))
          (setf (nck:lane-active-turn-id lane) nil)
          (is (equal "Closed. Write in it to open it again." (tap "i3" "alice")))
          (is (await-plan executor :label "close_thread" :path "/rooms/42" :method "PATCH") "the asker's press closes the thread it came from")
          (is (= 1 (closes)) "and only that press did")
          (is (equal "Closed. Write in it to open it again." (tap "i4" "mike")))
          (is (await (:timeout 10) (= 2 (closes))) "so does an operator's"))))))

(deftest channel-host-a-silent-wait-offers-switch-model
    (with-digest-host (host lane :host-keys (:platform (test-platform :choices t))))
  ;; Past the stall window, while the engine says the attempt still waits
  ;; with a failover candidate behind it, the running card carries Switch
  ;; model between Stop and Details. The owner's press cuts the wait to that
  ;; candidate and says where the round goes; a stranger's moves nothing.
  (fact "turn.started" (nlk:json-object))
  (setf (nck:lane-owner-id lane) "alice")
  (let ((digest (nck:lane-digest lane))
        (skipped '()))
    (setf (nck::turn-digest-started-at-ms digest) (- (nck:now-ms) 30000)
          (nck::turn-digest-visible-at-ms digest) (- (nck:now-ms) 30000))
    (with-stubbed-fdefinitions
        ((nle:provider-attempt-next (turn-id)
           (and (equal "t1" turn-id) (null skipped) '(:provider "deepseek" :model "deepseek-flash")))
         (nle:skip-provider-attempt (turn-id &optional reason)
           (push (list turn-id reason) skipped)
           '(:provider "deepseek" :model "deepseek-flash")))
      (is (equal (list (list (list "Stop" "nck:stop:chat-123-m1" :danger)
                             (list "Switch model" "nck:skip:chat-123-m1" :secondary)
                             (list "Details" "nck:details:chat-123-m1" :secondary)))
                 (nck::line-controls lane digest)))
      (flet ((switch (id user)
               (nck:control-pressed host (list :id id :user-id user :user-name user :message-id "s1"
                                               :channel-id "123" :data "nck:skip:chat-123-m1"))
               (plan-field (await-plan executor :path (format nil "/presses/~a" id)) "text")))
        (is (equal "Only whoever asked, or a room operator, can switch the model." (switch "i1" "stranger")))
        (is (null skipped) "a stranger's press moves nothing")
        (is (equal "Switching to deepseek/deepseek-flash." (switch "i2" "alice")))
        (is (equal '(("t1" "switched by alice")) skipped)))
      (is (equal (list (list (list "Stop" "nck:stop:chat-123-m1" :danger)
                             (list "Details" "nck:details:chat-123-m1" :secondary)))
                 (nck::line-controls lane digest)) "once switched, no second offer"))))
