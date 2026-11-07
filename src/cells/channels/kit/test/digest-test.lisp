;;;; digest-test.lisp --- the per-turn delivery digest. Pure, table-driven.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The digest is pure planning: every test drives folds and asserts
;;;; DIGEST-STATUS-PLAN decisions and the cards they carry at explicit clocks
;;;; — no threads, no executors, no sleeps.

(in-package #:nodecode.test)

(nlk:access (digest nck::turn-digest) (second-digest nck::turn-digest))

(defun status-kind (digest at)
  "The kind of delivery DIGEST-STATUS-PLAN answers for DIGEST at clock AT."
  (nth-value 0 (nck:digest-status-plan digest at)))

(defun status-delivered (digest at card &optional message-id)
  "One card delivery attempted at AT and landed as CARD (MESSAGE-ID the post's)."
  (nck:digest-status-attempted digest at)
  (nck:digest-status-delivered digest card message-id))

(defun card-at (digest at)
  "DIGEST's card at AT, as its plain lines."
  (nck:card-text (nck:digest-card digest at)))

(defmacro with-digest ((&key (turn-id "t1") (started 0) (evals 0)) &body body)
  "Run BODY with DIGEST a turn digest of TURN-ID started at STARTED that has
counted EVALS eval calls."
  `(let ((digest (nck:make-turn-digest :turn-id ,turn-id :started-at-ms ,started)))
     ,@(loop repeat evals collect '(nck:digest-note-tool-call digest))
     ,@body))

(defmacro is-planned ((form expected &optional description) &body checks)
  "FORM's values bound as a digest plan's KIND and TEXT — a card planned is read
as its CARD-TEXT, the card itself bound as CARD — then CALL-ID and MESSAGE-ID:
KIND is EXPECTED under DESCRIPTION, and TEXT carries CHECKS (IS-CARRYING's),
all in scope."
  `(multiple-value-bind (kind card call-id message-id) ,form
     (declare (ignorable call-id message-id))
     (let ((text (if (listp card) (and card (nck:card-text card)) card)))
       (is (eq ,expected kind) ,@(when description (list description)))
       (is-carrying text ,@checks))))

(defun note-eval (digest call-id form at)
  "Fold an eval call CALL-ID starting at AT into DIGEST, its arguments the
JSON {\"form\": FORM} a model's call carries."
  (nck:digest-note-call-start digest call-id "eval" (format nil "{\"form\": ~s}" form) at))

(deftest channel-digest-quiet-until-tools (with-digest (:started 1000))
  ;; A quick turn that called no tool earns no card: the typing indicator
  ;; covers it, and its answer is the one message.
  (is (eq :skip (status-kind digest 2000)))
  (nck:digest-note-round digest "short answer")
  (is (eq :skip (status-kind digest 3000)))
  (nck:digest-note-terminal digest :completed nil 4000)
  (is (eq :skip (status-kind digest 4000)))
  (is-shape digest (nck:digest-final-text "short answer") (.status-id null)))

(deftest channel-digest-status-lifecycle (with-digest (:evals 1))
  (nck:digest-note-round digest nil)
  (is-planned ((nck:digest-status-plan digest 5000) :post
               "the first card delivery creates the message")
    (:= (format nil "Working · 5s~%1 step")) (:absent "eval")
    (is (eq :working (getf card :state))))
  ;; The throttle base is the ATTEMPT: a failing post must not hot-loop
  ;; the worker tick, and it retries once the window passes.
  (nck:digest-status-attempted digest 5000)
  (is (eq :skip (status-kind digest 6000)))
  (is (eq :post (status-kind digest 7000)))
  (status-delivered digest 7000 (nck:digest-card digest 7000) "m1")
  (nck:digest-note-round digest (format nil "working on~%it now"))
  (is (eq :skip (status-kind digest 8000)) "inside the 2 s window")
  (is-planned ((nck:digest-status-plan digest 9000) :edit "later deliveries edit in place")
    "Working · 9s"
    (status-delivered digest 9000 card))
  (is (eq :skip (status-kind digest 9000)))
  ;; Completion skips here: the answer posts, and the card settles above it
  ;; (the host's SETTLE-CARD) to the work it took.
  (nck:digest-note-terminal digest :completed nil 30000)
  (is (eq :skip (status-kind digest 30500)))
  (is (equal "working on
it now" (nck:digest-final-text digest)))
  (is-carrying (text (card-at digest 30500)) (:= (format nil "Done in 30s~%1 step"))))

(deftest channel-digest-thinking-opens-the-card (with-digest ())
  ;; The audit's qwen-class round: everything in reasoning, content JSON
  ;; null. A turn that is thinking is not a turn that is dead — but a card the
  ;; room keeps over every quick answer is noise, so it earns one late. The
  ;; card names the thought by its newest whole sentence, never by the tail
  ;; it is still writing.
  (nck:digest-note-thinking digest "checking the model registry. then the")
  (is (eq :skip (status-kind digest 3000)))
  (is (eq :skip (status-kind digest 7900)) "a thinking turn earns its card at 8 s")
  (is-planned ((nck:digest-status-plan digest 9000) :post)
    (:= (format nil "Thinking · 9s~%thinking · checking the model registry."))
    (status-delivered digest 9000 card "m1"))
  (nck:digest-note-thinking digest
                            (make-string (* 2 nck:+digest-thinking-cap+)
                                         :initial-element #\y))
  (is-planned ((nck:digest-status-plan digest 20000) :edit)
    (:= "Thinking · 20s") (:absent "yyy" "a tail cut mid-word is never shown")))

(deftest channel-digest-thought-headline ()
  ;; A reasoning summary's newest bold title on a line of its own is the
  ;; headline; raw reasoning's newest whole sentence is; a sentence still
  ;; being written, a head the cap cut and a bold word inside a sentence
  ;; never are.
  (is-table (text headline) (equal headline (nck:thought-headline text))
    ((format nil "**Reading the registry**~%~%I open it first.~%**Checking ports**~%~%Port 80 is")
     "Checking ports")
    ("The user wants the port. Let me read the **format** call first. Then I"
     "Let me read the **format** call first.")
    ("Let me read the file." "Let me read the file.")
    ("Still writing this one" nil)
    (nil nil)
    ((format nil "A line that ended~%and one that has not") "A line that ended"))
  (let ((cut (concatenate 'string "rest of a cut sentence. "
                          (make-string (- nck:+digest-thinking-cap+ 24) :initial-element #\z))))
    (is (null (nck:thought-headline cut)) "the head the cap cut is not a sentence"))
  (let ((long (format nil "~{~a~^ ~}." (loop repeat 40 collect "word"))))
    (is-carrying (text (nck:thought-headline long))
      (is (<= (length text) 101)) (is (char= #\… (char text (1- (length text)))))
      (:absent "wor…" "it cuts at a word"))))

(deftest channel-digest-round-reasoning (with-digest (:evals 1))
  (nck:digest-note-thinking digest "streamed tail")
  (nck:digest-note-round digest nil "the round's committed thought")
  ;; A committed thought ends its line, so the next one starts a sentence.
  (is (equal (format nil "the round's committed thought~%") digest.thinking))
  (nck:digest-note-round digest nil nil)
  (is (equal (format nil "the round's committed thought~%") digest.thinking))
  (is (equal "the round's committed thought" (getf (nck:digest-card digest 1000) :thought)))
  (nck:digest-note-round digest "the answer" "more thinking")
  (is (null digest.thinking))
  (is (null (getf (nck:digest-card digest 1000) :thought)))
  (nck:digest-note-terminal digest :completed nil 5000)
  (is (equal "the answer" (nck:digest-final-text digest))))

(deftest channel-digest-thinking-is-never-the-answer (with-digest ())
  ;; Retention: thinking is live-only. It rides the card, which the terminal
  ;; settle overwrites, and leaves no message behind.
  (nck:digest-note-thinking digest "a whole round of silent thought")
  (nck:digest-note-round digest nil "and its committed trace")
  (nck:digest-note-terminal digest :completed nil 5000)
  (is (null (nck:digest-final-text digest)))
  (is (null (getf (nck:digest-card digest 5000) :thought)) "a settled card names no thought"))

(deftest channel-digest-failure-phases (with-digest (:evals 1))
  (nck:digest-note-round digest "partial narration")
  (status-delivered digest 1000 (nck:digest-card digest 1000) "m1")
  (nck:digest-note-terminal digest :failed "provider 502" 60000)
  (is-planned ((nck:digest-status-plan digest 60100) :edit)
    (:= (format nil "Failed at 60s~%> provider 502~%1 step"))
    (is (eq :failed (getf card :state))))
  (is-shape digest (nck:digest-final-text null) (.detail "provider 502"))
  (nck:digest-note-terminal digest :cancelled "stopped by mike" 61000)
  (is-carrying (text (card-at digest 61000)) "Stopped at 61s" "> stopped by mike"))

(deftest channel-digest-a-failure-shows-every-line-of-its-detail (with-digest (:evals 1))
  ;; A walked failover chain fails with a headline over a model a line, each
  ;; with what its provider said: the card showed the headline alone.
  (nck:digest-note-round digest "partial narration")
  (nck:digest-note-terminal digest :failed
                            (format nil "every model failed · /models picks another model~%1. claude-code/opus: the provider refused to answer (finish reason refusal)~%~%2. oc/dsf · HTTP 400: Upstream request failed: Model is unavailable.")
                            30000)
  (is-carrying (text (card-at digest 30000))
    "> every model failed · /models picks another model"
    "> 1. claude-code/opus: the provider refused to answer (finish reason refusal)"
    ("> 2. oc/dsf · HTTP 400: Upstream request failed: Model is unavailable." "a blank line is skipped, not the end"))
  (is (equal (list "a" "b" "c" "d" "e" "f" "… 3 more lines in the turn's record")
             (nck:digest-note-lines (format nil "a~%b~%c~%d~%e~%f~%g~%h~%i"))) "past the row cap, the card says how many more the record keeps"))

(deftest channel-digest-failure-without-a-card (with-digest ())
  ;; The fast failure: a rejected admission dies before earning a card. It
  ;; still has to say so, so the terminal plan CREATES the card.
  (nck:digest-note-terminal digest :failed "model not found" 900)
  (is-planned ((nck:digest-status-plan digest 900) :post "a failure with no open card posts one")
    "> model not found"))

(deftest channel-digest-a-refused-card-is-not-posted-again (with-digest (:evals 1))
  ;; A post the platform refused outright would be refused again: the card
  ;; is not posted while it keeps its state, past every refresh window, and
  ;; one whose state moves earns one more try, so the failure still says so.
  (is-planned ((nck:digest-status-plan digest 5000) :post "the card's first post")
    (nck:digest-status-attempted digest 5000)
    (nck:digest-status-refused digest card))
  (is (eq :skip (status-kind digest 7000)) "a working card refused is not posted again")
  (is (eq :skip (status-kind digest 60000)) "nor in any later window")
  (nck:digest-note-terminal digest :failed "provider 502" 61000)
  (is-planned ((nck:digest-status-plan digest 61000) :post "the failed card is another card")
    "> provider 502"
    (nck:digest-status-attempted digest 61000)
    (nck:digest-status-refused digest card))
  (is (eq :skip (status-kind digest 62000)) "refused too, it is not posted again"))

(deftest channel-digest-queued-phase (let ((digest (nck:make-turn-digest :started-at-ms 0))))
  ;; Waiting at the concurrency gate is visible immediately: the failure this
  ;; replaces was silence behind a typing indicator that expired at 5 minutes.
  (nck:digest-note-queued digest 2)
  (is-planned ((nck:digest-status-plan digest 10) :post "a queued ask earns its card at once")
    (:= "Queued · 2 ahead") (is (eq :queued (getf card :state)))
    (status-delivered digest 10 card "m1"))
  (nck:digest-note-queued digest 0)
  (is-planned ((nck:digest-status-plan digest 20000) :edit "the card counts down in place")
    (:= "Queued"))
  (nck:digest-note-admitted digest 30000)
  (is (eq :running digest.phase))
  (is (equal "Working · 1s" (card-at digest 31000)))
  (is (equal "m1" digest.status-id)))

(deftest channel-digest-parked-input-rides-the-running-card (with-digest ())
  ;; The TUI's pending band in channel form: a steer or follow-up parked
  ;; behind the running turn is a ⌎ row on its card, the row that runs next
  ;; saying when, and a parked message opens the card at once and is
  ;; acknowledged past the refresh throttle — once.
  (is (eq :skip (status-kind digest 1000)) "a quiet turn has no card yet")
  (nck:digest-note-pending
   digest (list (list :prompt-id "p1"
                      :text (format nil "ambient chatter~%mila [m12 u8]: and deploy it")
                      :steer-p nil)))
  (is-planned ((nck:digest-status-plan digest 1000) :post "parked input earns the card at once")
    (:= (format nil "Working · 1s~%⌎ and deploy it — after this turn"))
    (is (equal '("⌎ and deploy it — after this turn") (getf card :pending))))
  (status-delivered digest 1000 nil "m1")
  (nck:digest-note-pending
   digest (list (list :prompt-id "p1" :text "mila [m12 u8]: and deploy it"
                      :steer-p nil)
                (list :prompt-id "p2" :text "no wait, fix the tests first"
                      :steer-p t)))
  (is-planned ((nck:digest-status-plan digest 1500) :edit
               "a change in the parked rows bypasses the throttle")
    (is (search (format nil "~%⌎ no wait, fix the tests first — after this round~%⌎ and deploy it")
                text)))
  (status-delivered digest 1500 nil)
  (is (eq :skip (status-kind digest 1600)) "the bypass was spent by the attempt")
  (nck:digest-note-pending
   digest (list (list :prompt-id "p1" :text "and deploy it" :steer-p nil)))
  (is-planned ((nck:digest-status-plan digest 3500) :edit "the promoted steer's row goes at once")
    (:= (format nil "Working · 3s~%⌎ and deploy it — after this turn")))
  (nck:digest-status-attempted digest 3500)
  (nck:digest-note-pending
   digest (loop for index from 1 to 7
                collect (list :prompt-id (format nil "p~d" index)
                              :text (format nil "prompt ~d" index)
                              :steer-p nil)))
  (is-carrying (text (card-at digest 4000)) (:absent "prompt 6")
    (is (search (format nil "⌎ prompt 5~%⌎ 2 more waiting") text)))
  (nck:digest-note-pending digest '())
  (is (equal "Working · 4s" (card-at digest 4000))))

(defun writing-cursor (text)
  (concatenate 'string text nck::+digest-writing-cursor+))

(deftest channel-digest-what-a-turn-says-rides-its-card (with-digest (:evals 1))
  ;; A round that spoke and called a tool said its piece while the turn went
  ;; on: its words stay on the card, the newest of them, never a message of
  ;; their own, and the card's Details holds them all. A settled card shows
  ;; them only when no answer stands below it to say more.
  (nck:digest-note-written digest "first I look" t)
  (nck:digest-note-written digest (format nil "then~%the next thing") t)
  (is (equal (format nil "then~%the next thing") (getf (nck:digest-card digest 1000) :said)))
  (is-carrying (text (card-at digest 1000)) (format nil "then~%the next thing") (:absent "first I look"))
  (is (equal (list "first I look" (format nil "then~%the next thing"))
             (getf (nck:digest-details digest 1000) :said)))
  (is (search (format nil "**Said**~%first I look~%~%then~%the next thing")
              (nck:details-text (nck:digest-details digest 1000) 2000)))
  ;; The round that ends the turn spoke the answer, not words on its way.
  (nck:digest-note-written digest "the answer" nil)
  (is (= 2 (length (nck:turn-digest-said digest))))
  (nck:digest-note-terminal digest :completed nil 4000)
  (is (null (getf (nck:digest-card digest 4000) :said)) "a done card leaves them to the answer")
  (with-digest (:evals 1)
    (nck:digest-note-written digest "found the bug, fixing it" t)
    (nck:digest-note-terminal digest :cancelled "stopped by Mike" 2000)
    (is (equal "found the bug, fixing it" (getf (nck:digest-card digest 2000) :said)) "a stopped one keeps them")))

(deftest channel-digest-a-card-shows-the-words-a-round-writes (with-digest ())
  ;; channels.<id>.stream: the words a round writes show on its card as they
  ;; come, the cursor at their end, the card saying it writes — their end
  ;; when they are long. A round that calls a tool keeps them as what it
  ;; said; the round that ends the turn takes them off the card for its
  ;; answer; a retried round's are void. A turn that only writes earns its
  ;; card as one that only thinks does.
  (nck:digest-note-writing digest "Looking at the registry")
  (is (eq :skip (status-kind digest 7000)))
  (is (eq :post (status-kind digest 8000)) "a writing turn earns its card at 8 s")
  (is-shape (nck:digest-card digest 8000) (:headline "Writing") (:said (writing-cursor "Looking at the registry")))
  (nck:digest-note-writing digest " before I answer.")
  (nck:digest-note-written digest "Looking at the registry before I answer." t)
  (is-shape (nck:digest-card digest 9000)
    (:headline "Working") (:said "Looking at the registry before I answer."))
  (nck:digest-note-writing digest "The registry answers on 80.")
  (nck:digest-note-written digest "The registry answers on 80." nil)
  (is (equal "Looking at the registry before I answer." (getf (nck:digest-card digest 9500) :said)))
  (nck:digest-note-writing digest "A first attempt the provider cut")
  (nck:digest-note-written digest nil nil)
  (is (null (nck:turn-digest-writing digest)) "a retried round's words are void")
  (nck:digest-note-writing digest (format nil "~{line ~d~%~}" (loop for n from 1 to 400 collect n)))
  (let ((shown (getf (nck:digest-card digest 9600) :said)))
    (is (<= (length shown) (+ 4 nck::+card-said-cap+)))
    (is (eql 0 (search (format nil "…~%line ") shown)) "their end, from a line's start")
    (is (uiop:string-suffix-p shown (writing-cursor "line 400")))))

(deftest channel-digest-a-cards-note-is-capped (with-digest (:turn-id "t" :evals 1))
  ;; A note — a failover's, a settled turn's detail — is one line of at most
  ;; +DIGEST-NOTE-CAP+ characters.
  (setf (nck:turn-digest-fallback-text digest) (make-string 500 :initial-element #\x))
  (let* ((text (card-at digest 1000))
         (note (subseq text (+ 2 (search "> " text)) (position #\Newline text :start (search "> " text)))))
    (is (uiop:string-suffix-p note "..."))
    (is (= (+ 3 nck::+digest-note-cap+) (length note)) "the cap's characters, then the cut's mark")))

(deftest channel-digest-lane-turn-identity ()
  (let ((lane (nck:intern-lane (nck:make-lane-table "digest-test")
                               "discord-1")))
    (let ((first-digest (nck:lane-turn-digest lane "t1" 100)))
      (is (eq first-digest (nck:lane-turn-digest lane "t1" 200)))
      (nck:digest-note-tool-call first-digest)
      (let ((second-digest (nck:lane-turn-digest lane "t2" 300)))
        (is (not (eq first-digest second-digest)))
        (is (zerop second-digest.tool-calls))
        (is (eq second-digest (nck:lane-digest lane)))))))

(deftest channel-digest-queued-digest-adopts-its-turn ()
  ;; A digest opened before its turn existed must ADOPT the first turn id it
  ;; sees, not be replaced by it: replacing orphans the card the queued ask
  ;; already posted, and nothing would ever settle it.
  (let* ((lane (nck:intern-lane (nck:make-lane-table "adopt-test")
                                "discord-1-m9"))
         (queued (nck:lane-open-digest lane 100)))
    (nck:digest-note-queued queued 1)
    (nck:digest-status-delivered queued (nck:digest-card queued 100) "chrome-1")
    (is (null (nck:turn-digest-turn-id queued)))
    (let ((running (nck:lane-turn-digest lane "t1" 500)))
      (is (eq queued running) "the queued digest becomes the turn's digest")
      (is-shape running (nck:turn-digest-turn-id "t1") (nck:turn-digest-status-id "chrome-1")))))

(deftest channel-digest-says-what-the-turn-is-doing (with-digest (:evals 1))
  ;; The card reads the same classifier the TUI's cards read: a call in
  ;; flight is the card's headline and its live step, in the present tense;
  ;; once it lands it is a step done, in the past tense, and the card says
  ;; what the turn does next.
  (note-eval digest "c1" "(uiop:read-file-lines \"src/a.lisp\")" 1000)
  (is-shape (nck:digest-card digest 2000)
    (:headline "Reading src/a.lisp")
    (:steps '((:running "Reading src/a.lisp" "1s" nil))))
  (nck:digest-note-call-result digest "c1" 2500)
  (is-shape (nck:digest-card digest 3000)
    (:headline "Working")
    (:steps '((:done "Read src/a.lisp" "1s" nil))))
  ;; A snippet that reads several files says the count, like the cards do.
  (nck:digest-note-tool-call digest)
  (note-eval digest "c2"
             "(list (uiop:read-file-lines \"src/b.lisp\") (uiop:read-file-lines \"src/c.lisp\"))" 3500)
  (is-carrying (text (card-at digest 4000))
    (:= (format nil "Reading 2 files · 4s~%✓ Read src/a.lisp · 1s~%› Reading 2 files · 0s~%2 steps"))))

(deftest channel-digest-steps-take-the-results-words (with-digest (:evals 1))
  ;; A receipt on the result fact names what a snippet of free Lisp did; its
  ;; step takes those words the moment the call lands, and keeps the end of
  ;; what it answered: whole for its Output press, its last lines that say
  ;; anything for the details.
  (let ((metadata (nlk:json-object "calls" (vector (nlk:json-object "verb" "web:search"
                                                                    "family" "search"
                                                                    "source" "latest news")))))
    (note-eval digest "c1" "(web:search \"latest news\")" 1000)
    (nck:digest-note-call-result digest "c1" 2000 :metadata metadata
                                 :output (format nil "one~%~%two~%three~%four~%"))
    (let ((call (first (nck:turn-digest-steps digest))))
      (is-shape call
        (nck::digest-step-settled "Searched 'latest news'")
        (nck::digest-step-number 1)
        (nck::digest-step-output (format nil "one~%~%two~%three~%four") "the end of what it answered")))
    ;; The details show its last lines that say anything.
    (is (search (format nil "```~%two~%three~%four~%```")
                (nck:details-text (nck:digest-details digest 3000) 2000)))))

(deftest channel-digest-a-step-with-something-to-show-carries-its-press (with-digest (:evals 2))
  ;; Each step is numbered in its turn. A card's step carries the data of its
  ;; Output press once it landed with something to show — its card's room
  ;; and its number — and none while it runs or when it answered nothing.
  (setf (nck::turn-digest-session-id digest) "chat-1-m1")
  (note-eval digest "c1" "(sh \"just lint\")" 1000)
  (is (equal '(nil) (mapcar #'fourth (getf (nck:digest-card digest 1500) :steps))) "running: none")
  (nck:digest-note-call-result digest "c1" 2000 :output (format nil "lint: clean~%"))
  (note-eval digest "c2" "(sh \"true\")" 2500)
  (nck:digest-note-call-result digest "c2" 3000 :output "")
  (is (equal '("nck:step:chat-1-m1:1" nil) (mapcar #'fourth (getf (nck:digest-card digest 3000) :steps))))
  (is (equal '(1 2) (mapcar #'nck::digest-step-number (nck:turn-digest-steps digest))))
  ;; The details carry each step's output and number.
  (is (equal '(("lint: clean" 1) (nil 2))
             (mapcar (lambda (step) (last step 2)) (getf (nck:digest-details digest 3000) :steps)))))

(deftest channel-digest-the-newest-steps-keep-what-they-answered (with-digest ())
  ;; The newest steps keep the end of what they answered for their Output
  ;; press, cut from the front at a line's start, `…' where it was cut; a
  ;; step pushed past the newest keeps the details' last lines alone.
  (let ((long (format nil "~{line ~d~%~}" (loop for n from 1 to 400 collect n))))
    (note-eval digest "c0" "(sh \"just test\")" 0)
    (nck:digest-note-call-result digest "c0" 10 :output long)
    (let ((kept (nck::digest-step-output (first (nck:turn-digest-steps digest)))))
      (is (<= (length kept) (+ 2 nck::+step-output-chars+)))
      (is (eql 0 (search (format nil "…~%line ") kept)) "cut at a line's start")
      (is (search "line 400" kept)))
    (loop for n from 1 to nck::+steps-with-output+
          do (note-eval digest (format nil "c~d" n) "(sh \"true\")" (* 100 n)))
    ;; Pushed past the newest, it keeps the details' lines.
    (is (equal (format nil "line 398~%line 399~%line 400")
               (nck::digest-step-output (first (nck:turn-digest-steps digest)))))))

(deftest channel-digest-a-card-shows-the-pictures-its-turn-looked-at (with-digest ())
  ;; A call that looked at a picture (LOOK's image fact) gives its step that
  ;; picture, named by the step; the card carries the newest four, oldest
  ;; first. A picture an answer named is the answer's, not the card's.
  (flet ((looked (id n &optional (source "look"))
           (note-eval digest id "(look \"shot.png\")" (* 100 n))
           (nck:digest-note-call-result
            digest id (1+ (* 100 n))
            :metadata (nlk:json-object "image" (nlk:json-object "source" source
                                                                "path" (format nil "/work/shot-~d.png" n)
                                                                "media_type" "image/png")))))
    (loop for n from 1 to 5 do (looked (format nil "c~d" n) n))
    (looked "c6" 6 "answer")
    (is (equal '("step-1.png" "/work/shot-1.png" "shot-1.png")
               (nck::digest-step-image (first (nck:turn-digest-steps digest)))))
    (is (equal '("step-2.png" "step-3.png" "step-4.png" "step-5.png")
               (mapcar #'first (getf (nck:digest-card digest 1000) :images))))))

(deftest channel-digest-a-steps-press-shows-what-it-answered ()
  ;; A step's Output press answers with the step and the end of what it
  ;; answered in a block, its fences kept whole and its front cut when it is
  ;; long; a step that answered nothing says so.
  (is (equal (format nil "**3. ✓ Ran just lint · 4s**~%```~%lint: clean~%```")
             (nck::step-text '(:done "Ran just lint" "4s" "lint: `clean`" 3) 2000)))
  (let ((text (nck::step-text (list :done "Ran just test" "9s" (make-string 3000 :initial-element #\x) 4) 200)))
    (is (= 200 (length text)))
    (is (search (format nil "```~%…x") text)))
  (is (equal (format nil "**✓ Ran true · 1s**~%It answered nothing to show.")
             (nck::step-text '(:done "Ran true" "1s") 2000))))

(deftest channel-digest-a-settled-card-keeps-its-last-steps (with-digest ())
  ;; The card stays as the turn's record: settled, it says how long the work
  ;; took over its newest steps, the older ones counted; a call a failure cut
  ;; short is marked so.
  (loop for (id form) in '(("c1" "(sh \"just lint\")") ("c2" "(uiop:read-file-lines \"a.lisp\")")
                           ("c3" "(uiop:read-file-lines \"b.lisp\")") ("c4" "(sh \"just build\")")
                           ("c5" "(sh \"just test\")"))
        for at from 1000 by 2000
        do (nck:digest-note-tool-call digest)
           (note-eval digest id form at)
           (nck:digest-note-call-result digest id (+ at 1000)))
  (nck:digest-note-terminal digest :completed nil 30000)
  (is-carrying (text (card-at digest 30000))
    (:= (format nil "Done in 30s~%+1 earlier~%✓ Read a.lisp · 1s~%✓ Read b.lisp · 1s~%✓ Ran just build · 1s~%✓ Ran just test · 1s~%5 steps")))
  (with-digest (:evals 1)
    (note-eval digest "c1" "(sh \"just lint\")" 1000)
    (nck:digest-note-terminal digest :failed "provider 502" 3000)
    (is-carrying (text (card-at digest 3000))
      (:= (format nil "Failed at 3s~%× Running just lint · 2s~%> provider 502~%1 step")))))

(deftest channel-digest-counts-steps-not-rounds (with-digest ())
  ;; The card counts one step per tool call — the TUI bar's own total — and
  ;; carries no count at all before the first (the operator struck the round
  ;; index).
  (is (equal "Working · 3s" (card-at digest 3000)))
  (nck:digest-note-tool-call digest)
  (is (search "1 step" (card-at digest 3000)))
  (nck:digest-note-tool-call digest)
  (is (search "2 steps" (card-at digest 3000))))

(deftest channel-digest-stall-says-so ()
  ;; A provider step that shows nothing for the stall window stops reading as
  ;; dead air: the card says what the silence is — and the silence itself
  ;; earns the card when the turn never showed one.
  (is (eq :skip (status-kind (nck:make-turn-digest :turn-id "t0" :started-at-ms 0) 19000)))
  (is (eq :post (status-kind (nck:make-turn-digest :turn-id "t2" :started-at-ms 0) 21000)))
  (with-digest (:evals 1)
    (nck:digest-note-visible digest 0)
    (is (not (search "Waiting" (card-at digest 19000))))
    (is (search "Waiting on the provider" (card-at digest 21000)))
    (note-eval digest "c1" "(uiop:read-file-lines \"a.lisp\")" 21000)
    (is (not (search "Waiting" (card-at digest 60000))))
    (is (search "Reading a.lisp" (card-at digest 60000)))))

(deftest channel-digest-says-when-the-model-moved (with-digest ())
  ;; A failover fact folds the move onto the card, with its reason; the move
  ;; opens the card by itself — the operator is owed the fact that the turn
  ;; is no longer on the model they picked.
  (is (eq :skip (status-kind digest 2000)))
  (setf (nck:turn-digest-fallback-text digest) "fell back to alt-model (overloaded)")
  (is (eq :post (status-kind digest 2000)))
  (is (search "> fell back to alt-model (overloaded)" (card-at digest 2000))))

(deftest channel-digest-meters-the-turn (with-digest (:evals 1))
  ;; The footer's second line is the TUI's run line: the provider's counts
  ;; summed across rounds in the finish divider's meter, the prompt the newest
  ;; round put in the window, the price once priced, and ~ once any count was
  ;; the organism's arithmetic.
  (nck:digest-note-usage digest '(:input 1000 :output 1200 :cached 9000 :model "qa-model"))
  (nck:digest-note-usage digest '(:input 500 :output 800 :cached 9500 :reasoning 300
                                  :cost-known t :cost 0.25))
  (is (equal (format nil "qa-model · 1 step~%↑1.5k c92.5% ↓2k r300 · ctx 10k · $0.250")
             (getf (nck:digest-card digest 5000) :meta)))
  ;; A window the model is known by reads as its share; the window holds one
  ;; prompt at a time, the newest round's, not the turn's sum.
  (let ((nle::*context-tokens-env-override* 40000))
    (is (search "· ctx 25% ·" (card-at digest 5000))))
  (nck:digest-note-usage digest '(:output 300 :estimated t))
  (is (search "↓~2.3k" (card-at digest 6000)))
  (is (not (search "↓" (card-at (nck:make-turn-digest :turn-id "t3" :started-at-ms 0) 6000)))))

(deftest channel-digest-says-a-finish-that-was-not-clean (with-digest (:evals 1))
  ;; Once the turn ended, an answer the provider cut short says so, as the
  ;; finish divider does; a clean stop says nothing, and nor does a running
  ;; round's.
  (nck:digest-note-usage digest '(:input 100 :output 10 :finish-reason "length"))
  (is (not (search "finish" (card-at digest 1000))))
  (nck:digest-note-terminal digest :completed nil 2000)
  (is (search "· finish length" (getf (nck:digest-card digest 2000) :meta)))
  (nck:digest-note-usage digest '(:finish-reason "stop"))
  (is (not (search "finish" (getf (nck:digest-card digest 2000) :meta)))))

(deftest channel-digest-a-card-is-titled-its-task (with-digest (:evals 1))
  ;; The ask's task, once the card has one, opens its lines; the headline
  ;; says what the turn does under it.
  (setf (nck::turn-digest-task digest) "Fix the red lint on main")
  (is (equal (format nil "Fix the red lint on main~%Working · 2s~%1 step") (card-at digest 2000)))
  (is (equal "Fix the red lint on main" (getf (nck:digest-card digest 2000) :task))))

(deftest channel-digest-names-what-the-turn-ran-on (with-digest (:evals 1))
  ;; The footer opens with what the newest round ran on, in the finish
  ;; divider's words: provider/model, the model a relay served instead, and
  ;; the effort; a round silent about any of it keeps the last round's. A
  ;; provider the model id already leads with is not said twice (the TUI's
  ;; STATUS-IDENTITY-LABEL).
  (nck:digest-note-usage digest '(:provider "deepseek" :model "deepseek-flash" :effort "high"))
  (is (equal "deepseek-flash (high) · 1 step" (getf (nck:digest-card digest 1000) :meta)))
  (nck:digest-note-usage digest '(:provider "relay" :model "asked" :response-model "served"))
  (is (search "relay/asked→served (high) · 1 step" (card-at digest 1000)))
  (nck:digest-note-usage digest '(:provider "x" :model "" :effort "low"))
  (is (search "x/asked→served (low)" (card-at digest 1000)) "a round that names no model keeps the model"))

(deftest channel-digest-a-controls-move-is-its-own-delivery (with-digest (:evals 1))
  ;; The card can stand still while the buttons under it move, and the moved
  ;; buttons are the delivery: a press waits for no refresh window, and a set
  ;; already attempted is not replanned every tick.
  (nck:digest-note-round digest nil)
  (is-planned ((nck:digest-status-plan digest 5000) :post)
    (nck:digest-status-attempted digest 5000)
    (nck:digest-status-delivered digest card "m1"))
  (is (eq :skip (status-kind digest 6000)) "inside the window nothing is owed")
  (let ((shown (list (list (list "Stop" "nck:stop:chat-123-m1" :danger)
                           (list "Details" "nck:details:chat-123-m1" :secondary)))))
    (is (eq :edit (nth-value 0 (nck:digest-status-plan digest 6000 :controls shown))))
    (nck:digest-status-attempted digest 6000 :controls shown)
    (is (eq :skip (nth-value 0 (nck:digest-status-plan digest 6000 :controls shown))))
    (is (eq :edit (nth-value 0 (nck:digest-status-plan digest 6000 :controls :clear))))))

(deftest channel-digest-details-show-what-the-card-leaves-out (with-digest ())
  ;; A Details press shows every step kept, each with the end of what it
  ;; answered, and the newest thought in whole sentences; a message too long
  ;; for the platform drops the outputs first, then the oldest steps.
  (nck:digest-note-tool-call digest)
  (note-eval digest "c1" "(sh \"just lint\")" 1000)
  (nck:digest-note-call-result digest "c1" 4000 :output "kit/digest.lisp:876:3: unbalanced )")
  (nck:digest-note-tool-call digest)
  (note-eval digest "c2" "(uiop:read-file-lines \"kit/digest.lisp\")" 5000)
  (nck:digest-note-thinking digest "The paren closes format early. Read the lines around it. Then")
  (let ((details (nck:digest-details digest 6000)))
    (is-carrying (text (nck:details-text details 2000))
      (:= (format nil "**Steps** · 2~%1. ✓ Ran just lint · 3s~%```~%kit/digest.lisp:876:3: unbalanced )~%```~%2. › Reading kit/digest.lisp · 1s~%**Thinking**~%> The paren closes format early. Read the lines around it.")))
    (is-carrying (text (nck:details-text details 120))
      (:absent "```" "the outputs go first") "1. ✓ Ran just lint" "2. › Reading"
      "> The paren closes format early…" (is (<= (length text) 120)))
    (is-carrying (text (nck:details-text details 60))
      (:absent "1. ✓" "then the oldest steps") "… 1 earlier" "2. › Reading" (is (<= (length text) 60))))
  (is (equal "**Steps** · none yet" (nck:details-text (nck:digest-details (nck:make-turn-digest) 0) 2000))))


(deftest channel-digest-failure-outcomes-belong-to-the-call (with-digest ())
  (note-eval digest "c1" "(error \"boom\")" 1000)
  (nck:digest-note-call-result digest "c1" 2000 :failed t)
  (is (eq :failed (first (first (getf (nck:digest-card digest 2000) :steps)))))
  ;; A new step reusing the id cannot inherit the earlier step's failure.
  (note-eval digest "c1" "(+ 1 1)" 3000)
  (is (eq :running (first (first (getf (nck:digest-card digest 3000) :steps)))))
  (nck:digest-note-call-result digest "c1" 4000 :output "error count: 0" :failed nil)
  (is (eq :done (first (first (getf (nck:digest-card digest 4000) :steps)))))
  ;; As in the TUI, structured interruption is distinct even if failed is set.
  (note-eval digest "c2" "(sleep 10)" 5000)
  (nck:digest-note-call-result digest "c2" 6000 :failed t
                               :metadata (nlk:json-object "stopped" (nlk:json-object "by" "user")))
  (is (eq :stopped (first (second (getf (nck:digest-card digest 6000) :steps))))))


(deftest channel-digest-a-note-is-its-first-line ()
  ;; A settled failure's detail is whatever the condition prints, and a Lisp
  ;; condition prints its message then a `See also: The ANSI Standard…' block.
  ;; The card shows the message alone: a note is a glance, and the turn's
  ;; record keeps the rendering whole. (A failure card carried the traceback.)
  (is (equal "The bounding indices 0 and 300 are bad for a sequence of length 61."
             (nck:digest-note-line
              (format nil "The bounding indices 0 and 300 are bad for a sequence of length 61.~%See also:~%  The ANSI Standard, Glossary entry for \"bounding index designator\"")
              200)))
  ;; What is left is still one line, cut to the cap CLIP promises: the first
  ;; CAP characters and an ellipsis.
  (let* ((long (format nil "~{~a~}" (make-list 60 :initial-element "boom")))
         (line (nck:digest-note-line long 60)))
    (is (equal (concatenate 'string (subseq long 0 60) "...") line))
    (is (null (find #\Newline line))))
  ;; A single-line note is untouched.
  (is (equal "generated \"ok\"" (nck:digest-note-line "generated \"ok\"" 200))))

(deftest channel-digest-names-the-wait-and-counts-the-stream (with-digest ())
  ;; A silent round says whom it waits on and how long nothing has come; a
  ;; streaming one counts what it streamed, four characters a token, and how
  ;; fast; a round over counts from nothing, whom it waits on kept.
  (nck:digest-note-visible digest 0)
  (is (search "Waiting on the provider · no reply for 21s" (card-at digest 21000)) "nobody named yet")
  (nck::digest-note-provider digest "xiaomi")
  (is (search "Waiting on xiaomi · no reply for 80s" (card-at digest 80000)))
  (nck::digest-note-thinking digest "Checking the hosts.")
  (nck:digest-note-visible digest 100000)
  (nck::digest-note-streamed digest 4000 100000)
  (nck:digest-note-visible digest 104000)
  (nck::digest-note-streamed digest 3200 104000)
  (is (search "Thinking · ~1.8k tokens · 450 tok/s" (card-at digest 104000)))
  (nck::digest-note-call-streamed digest 0 200 104000)
  (nck::digest-note-call-streamed digest 0 400 104000)
  (is (search "~1.9k tokens" (card-at digest 104000)) "a call's arguments count once, whole so far")
  (nck::digest-note-round-over digest)
  (is (not (search "tokens" (card-at digest 105000))) "the next round counts from nothing")
  (is (search "Waiting on xiaomi · no reply for 26s" (card-at digest 130000)) "whom it waits on stays"))
