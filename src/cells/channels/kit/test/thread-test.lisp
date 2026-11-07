;;;; thread-test.lisp --- an ask that opens a thread of its own.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The thread-per-ask path over a platform that can open threads: the
;;;; thread is created off the ask's own message, the lane runs inside it,
;;;; and it answers there alone — its first answer naming the asker. A
;;;; thread the bot takes part in is one conversation: a line typed there
;;;; talks to its lane. Every way the thread can fail to appear lands the
;;;; ask flat. The
;;;; platform here is the room test one
;;;; plus the three thread calls, so nothing about it is Discord's but the
;;;; shape.

(in-package #:nodecode.test)

(nlk:access (asked nck::channel-lane) (room nlk::durable-session))

(defun thread-ask-host (&key executor (threads t) platform (name "thread-ask"))
  "A started test host for the thread tests: THREADS is the section's
channels.test.threads, PLATFORM the capability (the
room test platform is threadless, so THREADS with no PLATFORM is the
platform that cannot open one)."
  ;; One delivery worker, and the policy admits channel 100.
  (let ((host (test-host :executor executor
                         :workers 1
                         :platform (or platform (test-platform :threads t))
                         :threads threads
                         :name name)))
    (setf (nck:host-policy host) (channel-policy :allowed-channels '("100")))
    (nck:start-host host)
    host))

(defun thread-ask (host text &rest keys)
  "Hand HOST the ask TEXT, typed as message 5 in channel 100; KEYS override
the candidate's."
  (nck:handle-candidate host (apply #'test-candidate
                                    (append keys (list :channel "100" :message "5" :text text)))))

(defun origin-lane (host &rest origin)
  "HOST's lane for the ask m5 that opened thread 42 in channel 100, with its
origin noted — ORIGIN the keys past the three that place it."
  (let ((lane (nck:intern-lane (nck:host-lanes host) "chat-100-t42-m5"
                               :target '(:channel-id "100" :thread-id "42")
                               :trigger-message-id "5")))
    (setf (nck:lane-origin lane)
          (list* :channel-id "100" :message-id "5" :thread-id "42" origin))
    lane))

;;; --- the name ------------------------------------------------------------------

(deftest channel-thread-title-names-the-ask ()
  (let ((host (test-host :name "thread-title"
                         :platform (test-platform :threads t))))
    (flet ((title (text)
             (nck:thread-title host (test-candidate :text text
                                                    :channel "100"
                                                    :message "5"))))
      (is (equal "fix the auth bug" (title "<@999> fix the auth bug")))
      (is (equal "can you make a thread"
                 (title "<@999> can you make a thread real quick please")))
      (is (equal "a question" (title "<@999>")))
      (is (= 80 (length (title (format nil "~{~a~^ ~}"
                                       (loop repeat 30
                                             collect "supercalifragilisticexpialidocious")))))))))

;;; --- which asks open one ------------------------------------------------------------

(deftest channel-thread-ask-p-answers-where-a-thread-belongs ()
  (let* ((platform (test-platform :threads t))
         (host (test-host :name "thread-ask-p" :platform platform :threads t)))
    (flet ((ask-p (host candidate) (and (nck:thread-ask-p host candidate) t)))
      (is-each (ask-p)
        (host (test-candidate :channel "100" :message "5") t
         "a channel message with an id opens a thread")
        (host (test-candidate :channel "200" :message "5" :kind "group") t "a group message too")
        (host (test-candidate :channel "100" :message "5" :kind "direct_message") nil
         "a DM has no threads to open")
        (host (test-candidate :channel "t9" :thread "t9" :parent "100" :message "5") nil
         "a message already in a thread is one")
        ((test-host :name "thread-flat" :platform platform :threads t :flat-channels '("100"))
         (test-candidate :channel "100" :message "5") nil
         "a channel the section keeps flat opens nothing")
        ((test-host :name "thread-off" :platform platform :threads nil)
         (test-candidate :channel "100" :message "5") nil
         "threads off is the flat surface it always was")))))

;;; How the room reaches the bot — every line, or only a mentioned one — is
;;; the contract's to say, once per room (LANE-CONTRACT), never a mark on the
;;; line: an ask that reached the lane is one the room meant for it, in a room
;;; that takes no mention and in one gated on it alike. (a room
;;; that took no mention marked every unmentioned line "(not addressed to
;;; you)", and a voice note — which can never carry a mention — went NO_REPLY.)
(deftests channel-thread (require-mention text asked note)
    (with-ask-host (host thread-ask-host :answers ("ok")
                    :responses (replies "42" "p1") :threads t)
      (setf (nck:host-policy host)
            (channel-policy :allowed-channels '("100") :require-mention require-mention
                            :mention-target "999" :mention-test #'nck:discord-mention-p))
      (thread-ask host text)
      (is-present (lane (await-lane host "chat-100-t42-m5")) note
        (is (search asked (nck:lane-prompt lane)))
        (is (null (search "(not addressed" (nck:lane-prompt lane))))))
  (an-ask-line-carries-no-addressing-mark nil "local whisper is a big no?"
   "local whisper is a big no?" "the ask's lane opened in a room that takes no mention")
  (a-mentioned-ask-in-a-gated-room-carries-no-mark t "<@999> fix the auth bug"
   "fix the auth bug" "the ask's lane opened in a room that takes a mention"))

;;; --- the ask runs in the thread it opened ---------------------------------------------

(deftest channel-thread-ask-opens-a-thread-and-answers-inside-it ()
  ;; The whole acceptance: a question typed in the channel opens a thread named
  ;; after it, the lane runs inside the thread, and the answer lands there
  ;; alone — the ask's own message already shows the thread, so a copy in the
  ;; channel would be the same answer twice. The thread's answer carries no
  ;; reference — its ask is in the channel — and names the asker once.
  (with-ask-host (host thread-ask-host :answers ("the auth bug is in session.lisp")
                  :responses (replies "42" "p1") :threads t)
    ;; The channel said something the ask is about: it rides in
    ;; with the ask, because the thread's record starts there.
    (nck:note-ambient host "chat-100"
                      "bob [m4 u2]: the login flow is broken")
    (thread-ask host "<@999> fix the auth bug")
    (is-present (create (await-plan executor :label "create_thread"))
      "off the ask's own message, named after it"
      (is-plan create :path "/rooms/100/messages/5/threads" "name" "fix the auth bug"))
    (is-present (lane (await-lane host "chat-100-t42-m5"))
      "the lane is the thread's, not the channel's"
      (is (equal '(:channel-id "100" :thread-id "42" :message-id "5")
                 (nck:lane-target lane)))
      ;; The conversation the question is born out of reaches the lane by the
      ;; fork, so the ask carries no read-back: its prompt is the chatter that
      ;; was never a turn, and its own line.
      (is-carrying (prompt (nck:lane-prompt lane))
        "bob [m4 u2]: the login flow is broken" "fix the auth bug"))
    (is (nlk:session-exists-p "chat-100-t42"))
    (is-present (answer (await-plan executor :path "/rooms/42/messages"
                                             :content "the auth bug is in session.lisp"))
      "the answer is the lane's own text, naming the asker"
      (is-plan answer "reply_to" nil "ping" nil)
      (is (equalp #("u1") (plan-field answer "mentions"))))
    (is (null (recorded-plan executor "read_messages")) "nothing reads the channel back")
    ;; The origin is spent once the answer has landed: from here the
    ;; lane is an ordinary thread lane, and its next answer is the
    ;; thread's own business.
    (is (await (:timeout 10) (null (nck:lane-thread-origin
                                    (nck:find-lane (nck:host-lanes host) "chat-100-t42-m5")))))
    ;; The channel keeps no copy.
    (is (null (plan-matching (recorded-plans executor) :path "/rooms/100/messages")))
    ;; The thread's first exchange settles in the room the ask was typed in
    ;; too (DELIVER-ANSWER) — the channel's record is the channel's
    ;; timeline whether or not its asks open threads, and the next ask typed
    ;; there forks above every answer the channel has read. Without this a
    ;; channel whose asks all open threads would freeze at its last flat
    ;; exchange: the channel would record none of its answered asks, and
    ;; every new thread would fork from a stale head. The thread's own
    ;; record, as before:
    (is (await (:timeout 15) (plusp (count-session-events "chat-100-t42" "turn.completed"))))
    ;; And the channel's: the same exchange, once.
    (is (await (:timeout 15) (plusp (count-session-events "chat-100" "turn.completed"))))
    (is (= 1 (count-session-events "chat-100" "turn.started")))
    (is (= 1 (count-session-events "chat-100-t42" "turn.started")))
    (is-present (input (first (nlk:events :session-id "chat-100"
                                          :kind "turn.input_committed" :as :payloads)))
      "the channel records the ask as the lane read it, chatter and all"
      (is-carrying (message (nlk:json-value input :string "message")) "the login flow is broken"
        "fix the auth bug"))
    ;; The thread's room was cut below the exchange and holds it once, its
    ;; own; a thread opened after it forks above the exchange.
    (nlk:create-session :id "chat-100-t43" :parent "chat-100")
    (is-present (segment (first (nlk:session-lineage "chat-100-t43")))
      "the next thread's room composes the channel through the exchange"
      (is-shape segment (first "chat-100") (second (nlk:session-head-turn-id "chat-100"))))))

;;; --- a silence takes its empty thread with it ----------------------------------------

(deftest channel-thread-a-silent-answer-takes-the-empty-thread-with-it ()
  ;; A turn that answers NO_REPLY posts nothing — no answer, no ping — so the
  ;; thread the kit opened to work in would stand in the room empty, named
  ;; after a question nobody was told the fate of. It goes with the silence,
  ;; and the working line inside it goes with the thread, so the room is left
  ;; exactly as it was. The lane goes too: its room no longer exists, and a
  ;; later reply to the ask must open one of its own.
  (with-ask-host (host thread-ask-host :answers ("NO_REPLY") :responses (replies "42" "p1"))
    (thread-ask host "<@999> is this worth answering")
    (is (await-plan executor :label "create_thread") "the ask opened a thread")
    (is (await-plan executor :label "delete_thread") "and the silence removed it")
    (is-present (gone (recorded-plan executor "delete_thread"))
      "the thread is removed by its own id"
      (is-plan gone :path "/rooms/42" :method "DELETE"))
    (let ((plans (recorded-plans executor)))
      (is (null (plan-matching plans :label "send_message" :content "NO_REPLY")))
      (is (null (plan-matching plans :path "/rooms/100/messages"
                                     :content "NO_REPLY")))
      (is (null (plan-matching plans :label "delete_message"))))
    (is (await (:timeout 10) (null (nck:find-lane (nck:host-lanes host) "chat-100-t42-m5"))))))

(deftest channel-thread-a-silence-leaves-a-thread-that-holds-something ()
  ;; The guard: only an EMPTY thread the kit opened for this ask goes. A
  ;; thread carrying a word the turn spoke, a row of its trail, or a line
  ;; somebody typed is the room's — deleting it would take away what the room
  ;; already read — and a thread whose answer has landed is spent.
  (let* ((host (test-host :name "empty-thread-lanes" :platform (test-platform :threads t)))
         (lane (origin-lane host)))
    (is-present (origin (nck::lane-empty-thread lane))
      "a thread nothing has gone into is the one a silence retires"
      (is (equal "42" (getf origin :thread-id))))
    ;; Somebody typed in the thread, or the turn spoke there.
    (nck::note-thread-spoken "42")
    (is (null (nck::lane-empty-thread lane)) "a thread that holds something stays")
    (is-shape (nck:lane-origin lane) (:channel-id "100") (:thread-id "42")))
  ;; A word said in some other thread marks nothing here.
  (let* ((host (test-host :name "other-thread-lanes" :platform (test-platform :threads t)))
         (lane (origin-lane host)))
    (nck::note-thread-spoken "99")
    (is (nck::lane-empty-thread lane) "another thread's line is not this thread's")
    (nck::note-thread-spoken nil)
    (is (nck::lane-empty-thread lane) "and no thread at all marks nothing"))
  ;; A lane the kit opened no thread for has none to retire, and a lane whose
  ;; answer has landed is spent.
  (let ((lane (nck:intern-lane (nck:make-lane-table "flat-lanes") "chat-100-m5")))
    (is (null (nck::lane-empty-thread lane)) "a flat lane has no thread to take")
    (setf (nck:lane-origin lane)
          (list :channel-id "100" :message-id "5" :thread-id "42" :landed t))
    (is (null (nck::lane-empty-thread lane)) "and a landed answer spends the origin")))

(deftest channel-thread-a-line-typed-in-the-thread-keeps-it ()
  ;; The ingress half of the same guard, through the real seam: a line typed
  ;; inside a thread the kit opened marks that thread as holding something,
  ;; so a turn still running there can settle in silence and the thread still
  ;; stands. Keyed on the thread, because a line typed in one opens a lane of
  ;; its own — the lane that owns the thread never sees it.
  (with-ask-host (host thread-ask-host :responses (replies "p1") :threads t
                  :name "typed-in-thread")
    ;; A thread is admitted by its parent, the way every platform with
    ;; threads admits one.
    (setf (nck:host-policy host)
          (channel-policy :allowed-channels '("100") :channel-match :discord))
    (let ((lane (origin-lane host)))
      (is (nck::lane-empty-thread lane) "nothing has gone into the thread yet")
      (thread-ask host "and this?" :thread "42" :parent "100" :message "6")
      (is (await (:timeout 10) (null (nck::lane-empty-thread lane))))))
  ;; A line the policy refuses is not a line in the room: it marks nothing.
  (with-ask-host (host thread-ask-host :responses (replies "p1") :threads t
                  :name "refused-in-thread")
    (setf (nck:host-policy host) (channel-policy :allowed-channels '("100")
                                                 :allowed-users '("nobody")))
    (let ((lane (origin-lane host)))
      (thread-ask host "refused" :thread "42" :parent "100" :message "7")
      (is (nck::lane-empty-thread lane) "a refused line reaches no thread"))))

(deftest channel-thread-a-stopped-host-drops-the-claims-its-queue-held ()
  ;; The line above went to the worker queue, and the test was done before the
  ;; worker ran it: the claim it took waited on a release that never ran, and
  ;; message 6 asked again, on the next host, was dropped. The stop drops its
  ;; own host's claims and no other host's.
  (let ((host (thread-ask-host :name "claims")))
    (setf (gethash (cons "test" "6") nck::*ask-claims*) t
          (gethash (cons "other" "6") nck::*ask-claims*) t)
    (nck:stop-host host)
    (is (null (gethash (cons "test" "6") nck::*ask-claims*)))
    (is (remhash (cons "other" "6") nck::*ask-claims*))))

(deftest channel-thread-a-platform-that-cannot-remove-a-thread-keeps-it ()
  ;; The capability the base platform does not carry: a thread the platform
  ;; cannot remove is one the silence leaves alone, and the working line is
  ;; retired on its own as it always was.
  (is (null (funcall (nck:platform-plan-delete-thread (test-platform)) "42"))))

;;; --- the ask carries its own line; the fork carries the conversation ------------------

(deftest channel-thread-ask-inside-a-thread-carries-its-own-line ()
  ;; An ask typed INSIDE a thread runs on the thread room's fork — the
  ;; channel as it stood when the thread opened, and the thread's own
  ;; exchange — and reads nothing back either: the ask's own line is the
  ;; whole of what it adds.
  (with-ask-host (host thread-ask-host
                  :answers ("the auth bug is in session.lisp")
                  :responses (replies "p1")
                  :name "thread-inside-own-line")
    ;; The Discord match admits a parent channel's threads by their parent id.
    (setf (nck:host-policy host)
          (channel-policy :allowed-channels '("100") :channel-match :discord))
    (thread-ask host "<@999> what about the logs" :thread "42" :parent "100" :message "6")
    (is (await-plan executor :label "send_message"))
    (is-present (lane (await-lane host "chat-100-t42-m6")) "the ask rides with its own line alone"
      (is-carrying (prompt (nck:lane-prompt lane)) "what about the logs"))
    (is (null (recorded-plan executor "read_messages")))))

;;; --- a reply talks to the lane where it runs -------------------------------------------

(deftest channel-thread-reply-in-the-channel-opens-its-own-thread ()
  ;; A reply names the message it was aimed at, and that is all it decides on a
  ;; surface that opens threads: the question gets a lane of its own, forked at
  ;; the room's head, and a thread of its own hung off its own message. It never
  ;; joins the lane of the message it replied to — that lane's thread holds
  ;; another question — and its answer lands in its own thread, while the old
  ;; thread and its lane are untouched.
  (with-ask-host (host thread-ask-host :answers ("the answer")
                  :responses (replies "77" "p2") :threads t :name "thread-reply")
    (let* ((lanes (nck:host-lanes host))
           (asked (nck:intern-lane lanes "chat-100-t42-m5"
                                   :target '(:channel-id "100" :thread-id "42"
                                             :message-id "5")
                                   :trigger-message-id "5"
                                   :parent-session-id "chat-100"
                                   :owner-id "u1")))
      ;; The message the reply is aimed at is a live address of a
      ;; thread lane: the old path would have continued it.
      (setf (nck:lane-origin asked)
            '(:channel-id "100" :message-id "5" :thread-id "42" :landed t))
      (nck:bind-lane-address lanes "5" "chat-100-t42-m5")
      (is (eq asked (nck:lane-for-address lanes "5")))
      (thread-ask host "<@999> and deploy it"
                  :message "9" :reply-to "5" :reply "the answered message")
      (is-present (create (await-plan executor :path "/rooms/100/messages/9/threads"))
        "the reply opens a thread of its own"
        (is-plan create "name" "and deploy it"))
      (is (null asked.active-turn-id))
      (is (null (plan-matching (recorded-plans executor) :path "/rooms/42/messages")))
      (is-present (lane (await-lane host "chat-100-t77-m9")) "the reply runs in a lane of its own"
        (is-shape lane (nck:lane-parent-session-id "chat-100-t77")
          (nck:lane-target '(:channel-id "100" :thread-id "77" :message-id "9")))
        (is-carrying (prompt (nck:lane-prompt lane)) "and deploy it" "the answered message" "↩"))
      (is-present (answer (await-plan executor :path "/rooms/77/messages"))
        "the answer lands in the reply's own thread, and there alone"
        (is (equal "the answer" (plan-content answer))))
      (is (await (:timeout 10) (null (nck:lane-thread-origin
                                      (nck:find-lane (nck:host-lanes host) "chat-100-t77-m9")))))
      (is (null (plan-matching (recorded-plans executor) :path "/rooms/100/messages"))))))

(deftest channel-thread-lane-keeps-its-thread-against-a-reply-surface ()
  ;; The one refresh rule both ways a reply arrives share: the settled
  ;; continuation (INTERN-LANE) and the reply that steers a running turn
  ;; (ANSWER-MESSAGE) both go through LANE-DELIVERY-TARGET.
  (with-ask-host (host test-host :store nil :name "thread-target")
    (let ((threaded (nck:intern-lane (nck:host-lanes host) "chat-100-t42-m5"
                                     :target '(:channel-id "100" :thread-id "42"
                                               :message-id "5")))
          (flat (nck:intern-lane (nck:host-lanes host) "chat-100-m6"
                                 :target '(:channel-id "100" :message-id "6"))))
      (is (equal '(:channel-id "100" :thread-id "42" :message-id "5")
                 (nck::lane-delivery-target threaded
                                            '(:channel-id "100" :message-id "9"))))
      (is (equal '(:channel-id "100" :message-id "9")
                 (nck::lane-delivery-target flat
                                            '(:channel-id "100" :message-id "9")))))))

;;; --- every way the thread can fail to appear -------------------------------------------

(deftest channel-threadless-platform-runs-the-ask-flat ()
  ;; Threads on, and a platform that cannot open one (Telegram's own case):
  ;; the ask runs in the channel it was typed in, exactly as before — no
  ;; thread call, and the lane keyed by the message.
  (with-ask-host (host thread-ask-host :answers ("the answer") :threads t
                  :platform (test-platform))
    (thread-ask host "<@999> fix the auth bug")
    (is (await-lane host "chat-100-m5"))
    (is (null (recorded-plan executor "create_thread")))))

(deftest channel-thread-refused-falls-back-to-the-channel ()
  ;; The call goes out and comes back a refusal: the ask still runs, in the
  ;; channel, and the failure is one warning rather than a lost question.
  (with-ask-host (host thread-ask-host :answers ("the answer")
                  :responses (list (reply 500 "message" "no")) :threads t)
    (thread-ask host "<@999> fix the auth bug")
    (is (await-lane host "chat-100-m5"))
    (is (null (nck:find-lane (nck:host-lanes host) "chat-100-t42-m5")))))

(deftest channel-thread-redelivery-opens-one-lane ()
  ;; The gateway can hand the same ask over twice — a replay after a resume,
  ;; a reconnect whose first events overlap the last lap's. The thread call
  ;; is platform I/O, and while it is out nothing owns the ask: the second
  ;; delivery finds no lane and no address, takes the thread path too, is
  ;; refused as a duplicate thread, and its fallback opens a flat lane for
  ;; words already being answered inside the thread. One ask is one lane.
  (with-ask-host (host thread-ask-host :answers ("the answer")
                  :responses (list (reply 200 "id" "42")
                                   (reply 400 "message"
                                          "a thread has already been created for this message"))
                  :threads t :name "thread-redelivery")
    (thread-ask host "<@999> fix the auth bug")
    (thread-ask host "<@999> fix the auth bug")
    (is (await-lane host "chat-100-t42-m5"))
    (is (null (nck:find-lane (nck:host-lanes host) "chat-100-m5")))
    (is (= 1 (length (plans-matching (recorded-plans executor) :label "create_thread"))))))

(deftest channel-an-ask-admitted-again-runs-nothing-and-settles ()
  ;; 2026-10-20: a thread deleted under an answered ask made Discord restate
  ;; the ask after its lane was reaped, and the kit admitted it again under
  ;; its spent command id. The engine answered :DUPLICATE and nothing read
  ;; it: the line stood "Waiting on the provider" for five hours with no turn
  ;; behind it, Stop found nothing to stop, and the lane never reaped. A
  ;; second admission of one ask runs nothing and leaves nothing live.
  (with-ask-host (host thread-ask-host :answers ("the answer") :threads nil
                  :name "ask-admitted-again")
    (flet ((lane () (nck:find-lane (nck:host-lanes host) "chat-100-m5"))
           (started () (length (nlk:events :session-id "chat-100-m5" :kind "turn.started"
                                           :as :payloads))))
      (thread-ask host "<@999> fix the auth bug")
      (is (await-plan executor :content "the answer") "the ask was answered")
      (is (await (:timeout 10) (and (lane) (not (nck::lane-live-p (lane))))))
      ;; The reaper lets the lane go; the platform restates the ask.
      (nck:remove-lane (nck:host-lanes host) "chat-100-m5")
      (thread-ask host "<@999> fix the auth bug")
      (is (await (:timeout 10) (and (lane) (not (nck::lane-live-p (lane)))))
          "the second admission settled: no line left waiting on a turn")
      (is (= 1 (started)) "the ask ran once")
      (is (= 1 (length (plans-matching (recorded-plans executor) :content "the answer")))
          "and was answered once"))))

(deftest channel-a-line-holds-its-turn-without-its-first-frame ()
  ;; A line learned its turn only from TURN.STARTED's frame, and one the fold
  ;; missed was left running with no turn, which nothing ends. It holds the
  ;; turn its admission names, so the ending is found in the log and the
  ;; answer still lands with no frame folded at all.
  (with-ask-host (host thread-ask-host :answers ("the answer") :threads nil
                  :name "no-first-frame")
    (nle:unhook :frame (format nil "channel-~a" (nck::host-id host)))
    (thread-ask host "<@999> fix the auth bug")
    (is (await-plan executor :content "the answer" :timeout 20)
        "the answer landed though no frame was folded")))

(deftest channel-stop-settles-a-line-with-no-turn ()
  ;; Stop cancelled by turn id alone: on a line with none it warned and left
  ;; the card working (2026-10-20, pressed on the 'are you here?' card).
  (with-ask-host (host thread-ask-host :threads nil :name "stop-turnless")
    (let ((lane (nck:intern-lane (nck:host-lanes host) "chat-100-m9"
                                 :target '(:channel-id "100") :trigger-message-id "9")))
      (setf (nck:lane-owner-id lane) "u1")
      (bt2:with-lock-held ((nck::lane-lock lane))
        (nck:lane-open-digest lane (nck::now-ms)))
      (nck::stop-pressed host "u1" "chat-100-m9" "kim")
      (is (eq :cancelled (nck:turn-digest-phase (nck:lane-digest lane)))
          "the line settled")
      (is (equal "stopped by kim" (nck::turn-digest-detail (nck:lane-digest lane))))
      (is (await (:timeout 10) (not (nck::lane-live-p lane)))))))

;;; --- the ask's message stays out of the thread --------------------------------------

(deftest channel-thread-lane-answers-without-a-reference ()
  ;; The ask a thread lane answers is not in the thread: it is in the parent
  ;; channel. A platform refuses the whole post over a reference it cannot
  ;; resolve, so a thread lane never carries one, while a lane that runs where
  ;; its ask was typed still answers the message it answers.
  (with-ask-host (host test-host :store nil :name "thread-reference")
    (let ((threaded (origin-lane host :landed nil))
          (flat (nck:intern-lane (nck:host-lanes host) "chat-100-m6"
                                 :target '(:channel-id "100")
                                 :trigger-message-id "6")))
      (is (null (nck:lane-reference threaded "5")))
      (is (equal "6" (nck:lane-reference flat "6")))
      (nck:forget-thread-origin threaded)
      (is (null (nck:lane-thread-origin threaded)) "the answer landed: the origin is spent")
      (is (null (nck:lane-reference threaded "5")))
      (is (equal '(:channel-id "100") (nck:message-target threaded "5"))))))

(deftest channel-thread-first-answer-names-the-asker-once ()
  ;; A thread the kit opened hangs off a message its asker typed in the
  ;; channel, and the asker has not joined it: the thread's first answer names
  ;; them — the one ping the ask earns — and spends the origin, so every later
  ;; answer there is the thread's own business and names nobody. The first
  ;; answer hands back the room the ask was typed in, the record its exchange
  ;; settles into as well.
  (with-digest-host (host lane :name "thread-first-answer" :session "chat-100-t42-m5"
                     :host-keys (:platform (test-platform :threads t)))
    (setf (nck:lane-owner-id lane) "u1"
          (nck:lane-target lane) '(:channel-id "100" :thread-id "42")
          (nck:lane-origin lane) '(:channel-id "100" :message-id "5" :thread-id "42"
                                   :room "chat-100"))
    (flet ((answer (text turn-id)
             (fact "turn.started" (nlk:json-object) turn-id)
             (said text turn-id)
             (fact "turn.completed" (nlk:json-object) turn-id)))
      (answer "the first answer" "t1")
      (is-present (first-answer (await-plan executor :content "the first answer"))
        "the first answer names its asker, in the thread"
        (is-plan first-answer :path "/rooms/42/messages" "reply_to" nil)
        (is (equalp #("u1") (plan-field first-answer "mentions"))))
      (is (await (:timeout 10) (null (nck:lane-thread-origin lane))))
      (answer "the second answer" "t2")
      (is-present (second-answer (await-plan executor :content "the second answer"))
        "a later answer names nobody"
        (is-plan second-answer :path "/rooms/42/messages" "mentions" nil))
      (is (null (plan-matching (plans) :path "/rooms/100/messages")) "the channel keeps no copy"))))

;;; --- a thread the bot takes part in is one conversation ------------------------------

(deftest channel-thread-joined-marks-a-thread-the-bot-is-in (with-temp-store ())
  ;; Taking part is the thread's room existing: a person's line typed in such
  ;; a thread is marked joined, and so reaches the bot without a mention; a
  ;; thread the bot never spoke in, a bot's line and a channel line are left
  ;; alone. The candidate the kit moves into the thread it opened is joined
  ;; from the start.
  (let ((host (thread-ask-host :name "thread-joined")))
    (unwind-protect
         (flet ((joined-p (&rest keys)
                  (nck:candidate-joined-p
                   (nck:mark-joined-thread host (apply #'test-candidate :thread "42" :parent "100"
                                                       keys)))))
           (is (not (joined-p)) "a thread the bot has no room in")
           (nlk:create-session :id "chat-100-t42")
           (is (joined-p) "a thread whose room exists")
           (is (not (joined-p :is-bot t)) "never a bot's line")
           ;; A channel line is no thread's.
           (is (not (nck:candidate-joined-p (nck:mark-joined-thread host (test-candidate :channel "100")))))
           (is (null (nck:mark-joined-thread host nil)))
           ;; The kit's own thread is joined from its first ask.
           (is (nck:candidate-joined-p (nck::candidate-in-thread (test-candidate :channel "100")
                                                                 "42" "100"))))
      (nck:stop-host host))))

(deftest channel-thread-joined-speaks-freely ()
  ;; A joined thread reaches the bot without a mention in a room that takes
  ;; one — and a line that opens by naming somebody else is still theirs.
  (let ((policy (channel-policy :allowed-channels '("100") :channel-match :discord
                                :require-mention t :mention-target "999"
                                :mention-test #'nck:discord-mention-p))
        (joined (lambda (text)
                  (let ((candidate (test-candidate :thread "42" :parent "100" :text text)))
                    (setf (gethash "joined" (gethash "source" candidate)) t)
                    candidate))))
    (is-inbound policy (test-candidate :thread "42" :parent "100" :text "and the logs?")
                :observe "mention_required" "an unmarked thread line needs its mention")
    (is-inbound policy (funcall joined "and the logs?") :answer nil
                "a joined thread's line reaches the bot")
    (is-inbound policy (funcall joined "<@555> can you check?") :observe "addressed_elsewhere"
                "a line for somebody else is still theirs")))

(deftest channel-thread-a-line-in-a-joined-thread-talks-to-its-lane ()
  ;; The thread is one conversation: a line typed in a joined thread talks to
  ;; the lane that runs there — the one running a turn, which the line steers
  ;; — and a line in another thread, or one not marked joined, talks to none
  ;; and opens a lane of its own. A line its lane already took, redelivered,
  ;; is not taken twice.
  (with-ask-host (host thread-ask-host :responses (replies "p1") :name "thread-lane" :store nil)
    (setf (nck:host-policy host)
          (channel-policy :allowed-channels '("100") :channel-match :discord))
    (let ((lane (origin-lane host))
          (settled (nck:intern-lane (nck:host-lanes host) "chat-100-t42-m3"
                                    :target '(:channel-id "100" :thread-id "42"))))
      (setf (nck:lane-active-turn-id lane) "t-running")
      (flet ((line (thread message &key (joined t))
               (let ((candidate (test-candidate :thread thread :parent "100" :message message
                                                :text "and the logs?")))
                 (setf (gethash "joined" (gethash "source" candidate)) joined)
                 candidate)))
        ;; The lane running a turn in the thread, over one that settled.
        (is (eq lane (nck::conversation-lane host (line "42" "6"))))
        (setf (nck:lane-active-turn-id lane) nil
              (nck:lane-last-active-ms settled) (+ (nck:lane-last-active-ms lane) 1000))
        ;; With none running, the one active last.
        (is (eq settled (nck::conversation-lane host (line "42" "6"))))
        (is (null (nck::conversation-lane host (line "43" "6"))) "another thread's line")
        ;; A line the adapter did not mark joined.
        (is (null (nck::conversation-lane host (line "42" "6" :joined nil))))
        (nck:bind-lane-address (nck:host-lanes host) "6" (nck:lane-session-id settled))
        ;; A line its lane already took is not taken twice.
        (is (null (nck::answer-message host (line "42" "6"))))
        (is (null (plan-matching (recorded-plans executor) :label "send_message")))))))

(deftest channel-thread-a-follow-up-in-the-thread-is-its-lanes-next-turn ()
  ;; End to end: the ask opens its thread and answers there; the asker types a
  ;; follow-up in the thread with no mention, and it runs as the same lane's
  ;; next turn — the thread's conversation goes on with its trace — rather
  ;; than as a lane of its own, and its answer lands in the thread.
  (with-ask-host (host thread-ask-host :answers ("canberra" "wellington")
                  :responses (replies "42") :threads t :name "thread-follow-up")
    (setf (nck:host-policy host)
          (channel-policy :allowed-channels '("100") :channel-match :discord
                          :require-mention t :mention-target "999"
                          :mention-test #'nck:discord-mention-p))
    (thread-ask host "<@999> capital of australia?")
    (is (await-plan executor :path "/rooms/42/messages" :content "canberra"))
    (is (await (:timeout 10) (null (nck:lane-active-turn-id
                                    (nck:find-lane (nck:host-lanes host) "chat-100-t42-m5")))))
    (let ((follow-up (nck:mark-joined-thread
                      host (test-candidate :thread "42" :parent "100" :message "6"
                                           :text "and new zealand?"))))
      (is (nck:candidate-joined-p follow-up) "the bot is in the thread it opened")
      (is (eq :answer (nck:handle-candidate host follow-up)) "no mention needed"))
    (is-present (answer (await-plan executor :path "/rooms/42/messages" :content "wellington"))
      "the follow-up's answer lands in the thread"
      (is-plan answer "mentions" nil))
    (is (null (nck:find-lane (nck:host-lanes host) "chat-100-t42-m6")) "no lane of its own")
    (is (await (:timeout 15) (= 2 (count-session-events "chat-100-t42-m5" "turn.started"))))
    (is (null (plan-matching (recorded-plans executor) :path "/rooms/100/messages")))))

(deftest channel-thread-new-in-the-thread-starts-the-next-line-fresh ()
  ;; End to end: /new typed in a thread clears the thread's room, and the next
  ;; line opens a lane of its own over the cleared record — not the next turn of
  ;; the lane that answered before it, which still held everything /new said
  ;; was gone.
  (with-ask-host (host thread-ask-host :answers ("canberra" "hello")
                  :responses (replies "42") :threads t :name "thread-new")
    (setf (nck:host-policy host)
          (channel-policy :allowed-channels '("100") :channel-match :discord
                          :require-mention t :mention-target "999"
                          :mention-test #'nck:discord-mention-p))
    (nlk:create-session :id "chat-100")
    (nlk:record-exchange-turn "chat-100" "bob [m4 u2]: the deploy is green" "noted")
    (thread-ask host "<@999> capital of australia?")
    (is (await-plan executor :path "/rooms/42/messages" :content "canberra"))
    (is (await (:timeout 10) (null (nck:lane-active-turn-id
                                    (nck:find-lane (nck:host-lanes host) "chat-100-t42-m5")))))
    (flet ((line (message text)
             (nck:handle-candidate
              host (nck:mark-joined-thread
                    host (test-candidate :thread "42" :parent "100" :message message
                                         :user "mike" :text text)))))
      (line "6" "/new")
      (is (await-plan executor :path "/rooms/42/messages" :content "cleared"))
      (is (null (nlk:session-head-turn-id "chat-100-t42")) "the thread's room is at its start")
      (is (null (nck:find-lane (nck:host-lanes host) "chat-100-t42-m5")) "its lane is retired")
      (is (eq :answer (line "7" "hi"))))
    (is (await-plan executor :path "/rooms/42/messages" :content "hello"))
    (is (await-lane host "chat-100-t42-m7") "the line opened a lane of its own")
    (is (null (nlk:session-lineage "chat-100-t42-m7")) "over nothing: the room was cleared")
    (is (= 1 (count-session-events "chat-100-t42-m5" "turn.started")) "the old lane took no turn")))

;;; --- a thread's room forks the room it hangs in ---------------------------------------

(deftest channel-thread-room-forks-the-channel-room ()
  ;; A thread the kit opens is a room of its own that FORKS the room it hangs
  ;; in: the channel's record, composed by reference and frozen at the
  ;; channel's head when the thread's room was made. Without the fork a
  ;; thread's lane starts from its own ask and nothing else — the room the
  ;; thread was opened in is invisible to the work done in it.
  (with-ask-host (host thread-ask-host :answers ("the answer") :responses (replies "42")
                  :threads t :name "thread-fork")
    ;; The channel has a record before the thread exists: it is
    ;; what the thread's room has to start from.
    (nlk:create-session :id "chat-100")
    (nlk:record-exchange-turn "chat-100"
                              "bob [m4 u2]: the deploy is green"
                              "noted")
    (is (equal "chat-100"
               (nck::room-parent-session-id
                host '(:channel-id "100" :thread-id "42"))))
    (is (null (nck::room-parent-session-id host '(:channel-id "100"))))
    (let ((head (nlk:session-head-turn-id "chat-100")))
      (is head "the channel room has a head to fork at")
      (thread-ask host "<@999> why is auth failing")
      (is (await (:timeout 10) (nlk:session-exists-p "chat-100-t42")))
      (let ((room (nlk:find-session "chat-100-t42")))
        (is-shape room (.parent "chat-100") (.anchor-turn-id head)))
      (is-present (segment (first (nlk:session-lineage "chat-100-t42")))
        "and the thread's room composes the channel's record"
        (is-shape segment (first "chat-100") (second head))
        (is (plusp (third segment))))
      ;; The lane's own fork: the room it runs in has no turns yet,
      ;; so the head it forks at is the room's anchor — the channel
      ;; turn above. Without the composed-tree check the create was
      ;; refused and the ask died as a laneless failure notice.
      (is (await (:timeout 10) (nlk:session-exists-p "chat-100-t42-m5")))
      (is-present (lane (nlk:find-session "chat-100-t42-m5"))
        "the lane is a session of the thread's own"
        (is (equal "chat-100-t42" (nlk:durable-session-parent lane)))
        (is (equal head (nlk:durable-session-anchor-turn-id lane)))
        (is (nlk:session-known-turn-p "chat-100-t42-m5" head)))
      (is (await-plan executor :label "send_message" :path "/rooms/42/messages"
                               :content "the answer")))))

(deftest channel-thread-a-model-names-the-thread ()
  ;; The thread opens under the ask's first words; the kernel names the
  ;; lane's session from its first prompt (NLE:NAME-SESSION) while the turn
  ;; runs, and the thread takes that title. An ask of five words or fewer is
  ;; its own name already, and costs no call.
  (let ((asked '()))
    (with-saved-globals ((nle::*session-naming* :inline))
      (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                  (declare (ignore system session-id max-tokens fallback-p))
                                  (push user asked)
                                  "Title: \"Capital of Australia vs Sydney.\"")
        (with-ask-host (host thread-ask-host :answers ("Canberra") :responses (replies "42" "p1")
                        :threads t)
          (thread-ask host "<@999> what is the capital of Australia, is it Sydney")
          (is-present (rename (await-plan executor :label "rename_thread"))
            "the thread takes the title the model wrote"
            (is-plan rename :method "PATCH" :path "/rooms/42"
                            "name" "Capital of Australia vs Sydney"))
          (is (await-plan executor :content "Canberra"))
          ;; The call reads the ask, our mention stripped.
          (is (equal '("what is the capital of Australia, is it Sydney") asked)))
        (setf asked '())
        (with-ask-host (host thread-ask-host :answers ("fixed") :responses (replies "42" "p1")
                        :threads t)
          (thread-ask host "<@999> fix the auth bug")
          (is (await-plan executor :content "fixed"))
          (is (null (recorded-plan executor "rename_thread")))
          (is (null asked)))))))
