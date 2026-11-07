;;;; digest.lisp --- the per-turn delivery digest. Pure planning.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One turn = one channel answer. Chat channels are message surfaces, not
;;;; terminal transcripts: per-round passthrough drips narration fragments
;;;; into the channel while tool-only rounds read as dead air. The digest
;;;; folds a turn's stream facts into ONE live status line — posted when the
;;;; turn first calls a tool, then edited in place on the delivery worker's
;;;; tick — and holds the newest non-empty assistant text as the turn's
;;;; final answer, delivered once on turn.completed.
;;;;
;;;; The status line is a CARD (DIGEST-CARD): what the turn is doing now, in
;;;; the words the TUI's cards read — `Running just lint' — the thought that
;;;; chose it, the newest of its steps as a checklist, the pictures it looked
;;;; at, and its numbers. The card is data, and each platform draws it:
;;;; Discord a container of components whose accent is the phase, each step a
;;;; row whose Output button shows what it answered, Telegram HTML whose dot
;;;; is the phase, its steps in a quote (CARD-HTML). Every step of the turn
;;;; stays on the digest, and the card's Details control answers whoever
;;;; pressed it, privately, with all of them (DIGEST-DETAILS).
;;;;
;;;; The card is the turn's RECORD once it has one: a completed turn posts its
;;;; answer below it and the card settles to `Done in 27s' over its last
;;;; steps, so the room keeps what the work was beside what it said. A turn
;;;; that never earns a card — quick, no tools, no long thinking — is covered
;;;; by the typing indicator and posts nothing but the answer. A failed or
;;;; stopped turn settles its card to the notice, its detail included.
;;;;
;;;; A lane can also be admitted late, behind a concurrency gate. That is a
;;;; :QUEUED phase, and it earns a line at once: waiting invisibly behind an
;;;; unbounded typing indicator is the failure this replaces.
;;;;
;;;; Input parked BEHIND a running turn — a steer that ends it at its next
;;;; round boundary, a follow-up that waits for it — is the other wait, and
;;;; it rides the running turn's own line as ⌎ rows, the TUI's pending band
;;;; in channel form: the line the operator is already watching says what
;;;; is waiting and when it runs, and stays the newest message in the chat,
;;;; so the wait is visible from the chat list and from wherever they scroll
;;;; back to. A pin was considered for this and rejected: a
;;;; pin leaves a permanent service row per ask, is invisible from the chat
;;;; list, and exists on two platforms. The rows come whole from every
;;;; kernel queue snapshot, so a promotion or a retraction clears its row.
;;;;
;;;; The status line posts as a silent reply to the ask it stands in for:
;;;; in a room running several lanes at once, the anchor is what says which
;;;; ask a line belongs to, and the answer that replaces it replies to the
;;;; same message.
;;;;
;;;; Reasoning folds in the same way, from the live reasoning deltas and
;;;; from each round's committed reasoning_content: a round that puts
;;;; everything in its thinking and answers with JSON null is not a silent
;;;; round, and the card says so with the thought's headline
;;;; (THOUGHT-HEADLINE) — its newest bold title, else its newest whole
;;;; sentence, never a tail cut mid-word. Thinking is display
;;;; only — a live trace with no retention past the card and its details —
;;;; and never becomes the channel's answer.
;;;;
;;;; Fold functions mutate the digest and perform NO I/O: adapters call them
;;;; under the lane lock on the sync reader thread (the telegram delta
;;;; accumulator precedent — classify + fold only, never a REST call).
;;;; DIGEST-STATUS-PLAN is the pure decision seam the delivery worker
;;;; drives; the worker performs at most one platform call per plan and
;;;; records the outcome back through DIGEST-STATUS-ATTEMPTED /
;;;; DIGEST-STATUS-DELIVERED.

(in-package #:nodecode-channel-kit)

;;; Inside Discord's five edits per five seconds a channel: at 8 s the card
;;; read frozen, and it carries the words a round writes. Terminal settles
;;; bypass it.
(defparameter +digest-status-refresh-ms+ 2000
  "Minimum interval between card deliveries for a running turn.")

;;; Every edit of a card the room keeps is cheap; a card above every quick
;;; answer is not, so a turn that only thinks earns one late.
(defparameter +digest-earn-ms+ 8000
  "How long a turn that has called no tool thinks or writes before it earns a
card.")

;;; The turn's own words are not folded in — a round that spoke leaves as its
;;; own message, and its answer as the answer.
(defparameter +digest-note-cap+ 300
  "Characters a line of a card's notice may take: a terminal turn's detail, a
failover's note.")

(defparameter +digest-note-rows+ 6
  "Lines of a card's notice: a walked failover chain's failure is a headline
and a model a line.")

;;; The card shows the thought's headline and the details its newest whole
;;; sentences, so the digest keeps a few of those and drops from the front —
;;; a thinking block runs to thousands of tokens, and the transcript, not the
;;; channel, is where the whole of it is kept.
(defparameter +digest-thinking-cap+ 1200
  "Reasoning retained for the card and its details.")

(nlk:define-record (turn-digest (:copier nil) (:export :constructor turn-id tool-calls
                                                       answer thinking phase
                                                       queue-position detail status-id posted-card
                                                       ask-id pending pending-changed-p
                                                       fallback-text said writing
                                                       background-pending steps))
  "Delivery state for one turn on one lane. Guarded by the lane lock."
  (turn-id nil :type (or null string))
  ;; The lane's session id: the room a step's Output press names (STEP-PRESS).
  (session-id nil :type (or null string))
  (started-at-ms 0 :type integer)
  (tool-calls 0 :type integer)
  ;; Newest non-empty assistant text — the final-answer candidate.
  (answer nil :type (or null string))
  ;; Newest reasoning: the streamed tail while a round is in flight, the
  ;; round's committed reasoning_content once it lands. Display only.
  (thinking nil :type (or null string))
  ;; :queued | :running | :completed | :steered | :failed | :cancelled |
  ;; :paused — :STEERED a completion a parked steer cut short: no answer of
  ;; its own; :PAUSED a turn the host stopped under, which the next boot
  ;; resumes (HAND-OFF-LANES).
  (phase :running :type keyword)
  ;; Asks ahead of this one at the concurrency gate, :QUEUED only.
  (queue-position 0 :type integer)
  (detail nil :type (or null string))
  (ended-at-ms 0 :type integer)
  ;; Platform card-message identity plus the last card actually delivered
  ;; (no-op edit dedupe) and the last delivery ATTEMPT (throttle base — a
  ;; failing post must not hot-loop the worker tick).
  (status-id nil :type (or null string))
  (posted-card nil :type list)
  (posted-at-ms 0 :type integer)
  ;; The card :STATE whose post the platform refused outright (a 4xx, not a
  ;; rate limit): the same request would be refused again, so no post is
  ;; planned until the card's state moves on (DIGEST-STATUS-REFUSED).
  (refused-state nil :type (or null keyword))
  ;; What the delivered card's message holds of its pictures, in the
  ;; platform's own words (its MEDIA-OF): the next edit keeps them by these
  ;; rather than sending them again.
  (posted-media '() :type list)
  ;; The platform message id of the ask this digest answers: the status
  ;; line replies to it, the answer replies to it, and the reaction, when
  ;; the host reacts, sits on it (host.lisp).
  (ask-id nil :type (or null string))
  ;; Input parked on the lane's session while this turn runs: one plist
  ;; (:prompt-id ID :text TEXT :steer-p BOOL) per prompt, in the order the
  ;; kernel snapshot lists them. Replaced wholesale by every snapshot.
  (pending '() :type list)
  ;; The pending rows changed since the last delivery attempt: the next
  ;; plan bypasses the refresh throttle once, so a parked message is
  ;; acknowledged on the next tick rather than in the next window.
  (pending-changed-p nil :type boolean)
  ;; What the card knows about the turn's work, in the words the TUI's cards
  ;; read: STEPS, one DIGEST-STEP per call, OLDEST first, the open one
  ;; without an end; VISIBLE-AT-MS is the newest moment the turn showed
  ;; anything.
  (steps '() :type list)
  (visible-at-ms 0 :type integer)
  ;; What the turn ran on and spent, as the TUI's bar and finish divider
  ;; read it (DIGEST-NOTE-USAGE): USAGE the turn's rounds summed, in the
  ;; TUI accumulator's keys, its identity the newest round's — from the
  ;; usage facts, not the room's pick a later /models would already have
  ;; moved; CONTEXT the prompt the newest round put in the window.
  (usage '() :type list)
  (context '() :type list)
  ;; What the ask is, the card's title: its first words until the title its
  ;; session is named lands (WEAR-TITLE, host.lisp).
  (task nil :type (or null string))
  ;; The newest failover note (`fell back to alt-model (overloaded)'), shown
  ;; until the turn settles.
  (fallback-text nil :type (or null string))
  ;; The controls the card was last delivered with, or :CLEAR once a settle
  ;; retired them: the card may stand still while a button moves, and a moved
  ;; button is its own reason to edit.
  (controls nil)
  ;; What the turn said on its way, its rounds' words OLDEST first, and —
  ;; while the section streams — the words the round in flight is writing.
  (said '() :type list)
  (writing nil :type (or null string))
  ;; How many of the turn's background evaluations are still in flight, and
  ;; the words the model spoke while it waited on them, oldest first: what the
  ;; room was told before the answer.
  (background-pending nil)
  (waiting-words '() :type list))

(nlk:access (digest turn-digest) (fresh turn-digest))

;;; --- what the turn is doing, in words ----------------------------------------
;;; The card says what the turn is doing in the same words the TUI's cards
;;; read: the classifier over each call's arguments (engine/activity.lisp —
;;; one module, two surfaces) and the output-token flow the provider
;;; reported.

;;; The card's copy of the TUI bar's staleness rule, longer because a channel
;;; edit is a delivery, not a repaint.
(defparameter +digest-stall-ms+ 20000
  "How long a running turn may show nothing at all — no streamed part, no
tool call, no committed round — before the card stops pretending and says it
is waiting on the provider.")

(defparameter +digest-step-cap+ 80
  "Characters one step's words may take on a card before they cut.")

(defun digest-forget-activity (digest)
  "Drop what DIGEST knew of its turn's work: its turn settled, and a turn
that takes the digest over starts from nothing."
  (when digest
    (setf digest.steps '() digest.visible-at-ms 0 digest.usage '() digest.context '()
          digest.fallback-text nil digest.said '() digest.writing nil digest.controls nil))
  digest)

(defun step-words (activity &key running)
  "ACTIVITY's collapsed line, the TUI card's title — present tense with
RUNNING — or NIL for a call this module has no words for."
  ;; The `…' a running title may end on is the card's marker's to say.
  (let ((title (and activity (nle:activity-title activity :running running))))
    (when (and title (plusp (length title)))
      (string-right-trim "…" title))))

;;; --- the turn's steps: one per call, as the TUI shows one row ----------------
;;; The transcript walks rows: a call is one collapsed line — `Read
;;; src/tui/ui.lisp', `Ran just lint' — its output one level down. The card
;;; keeps the same rows: each call is a STEP, in the words the classifier
;;; gives the transcript (activity.lisp), present tense while it runs and past
;;; tense once it lands. The card shows the newest few, each with a press
;;; that shows what it answered; its details show them all, each with the end
;;; of that.

(defparameter +digest-steps-kept+ 240
  "Steps one digest keeps; past it the oldest drop, and the card's count
still names them.")

(defparameter +digest-output-lines+ 3
  "Lines of a call's answer the details show under its step: the last ones.")

;;; A step's Output press shows the end of what it answered, a private
;;; message's worth. The newest steps keep that much and the older ones the
;;; details' lines alone, so a long turn's record stays small.
(defparameter +step-output-chars+ 1500
  "Characters of a call's answer a step keeps for its Output press.")

(defparameter +steps-with-output+ 8
  "The newest steps that keep +STEP-OUTPUT-CHARS+; a card shows half as many.")

(defstruct (digest-step (:copier nil) (:predicate nil)
                        (:constructor make-digest-step))
  "One call of the turn."
  ;; NAME and ARGUMENTS classify it again when its result lands; RUNNING is
  ;; what it says while it runs and SETTLED once it landed (the words its
  ;; result's receipts added included); ENDED-MS is 0 while it runs; NUMBER
  ;; its place in the turn, from 1; OUTPUT the end of what it answered
  ;; (OUTPUT-END, the details' lines once newer steps pushed it back); IMAGE
  ;; the picture it looked at, (NAME PATH DESCRIPTION), NAME the file a
  ;; platform calls it.
  (call-id nil :type (or null string))
  (name nil :type (or null string))
  (arguments nil :type (or null string))
  (running "" :type string)
  (settled nil :type (or null string))
  (started-ms 0 :type integer)
  (ended-ms 0 :type integer)
  (number 0 :type integer)
  (output nil :type (or null string))
  (image nil :type list))

(nlk:access (call digest-step))

;; Live-only derived state, weakly held for exactly the lifetime of its step.
;; Keep it beside the step rather than redefining structures a running image
;; already holds. The durable tool-result fact remains the status authority.
(nlk:define-side-table step-outcome (:test #'eq :weakness :key :synchronized t)
  "A landed step's outcome, :done / :failed / :stopped, from its result fact.")

(defun digest-open-step (digest)
  "The newest of DIGEST's calls still running, or NIL."
  (find 0 digest.steps :key #'digest-step-ended-ms :from-end t))

;;; --- what the turn says on its way: its card's, as its thought is ------------
;;; A round that spoke and then called a tool said its piece while the turn
;;; went on. Those words are the turn's own, not its answer, and they stay
;;; with its work: the card shows the newest of them under the thought, and
;;; its Details all of them; a turn's messages do not leak out of its
;;; card. The round that ENDS the turn carried no calls; its text is the
;;; answer and leaves through DELIVER-ANSWER, the one message a turn posts.
;;;
;;; channels.<id>.stream: the words the round in flight writes show on the
;;; card as the model writes them, the cursor at their end; the round decides
;;; what they were once it ends — words said on the way, or the answer, which
;;; posts fresh and leaves the card. A turn that only writes earns its card
;;; as one that only thinks does.

(defparameter +digest-said-kept+ 40
  "Rounds' words one digest keeps for its card's Details; past it the oldest go.")

(defparameter +card-said-cap+ 1200
  "Characters of the newest words a card shows: their end.")

(defparameter +digest-writing-cursor+ " ▉"
  "What ends, on a card, the words a round still writes.")

(defun digest-note-said (digest text)
  "One round spoke TEXT and kept working: its words join what the turn said."
  (when (and digest (stringp text) (plusp (length (nlk:trimmed text))))
    (setf digest.said (last (append digest.said (list text)) +digest-said-kept+)))
  digest)

(defun digest-note-writing (digest text)
  "One text delta streamed in: the words the round in flight writes grow by
TEXT."
  (when (and digest (stringp text) (plusp (length text)))
    (setf digest.writing (concatenate 'string (or digest.writing "") text)))
  digest)

(defun digest-note-written (digest text called-p)
  "The round in flight committed TEXT, CALLED-P when it called a tool and said
TEXT on its way: what it was writing leaves the card, and those words join
what the turn said. The provider retrying a round (TEXT NIL) voids what its
failed attempt wrote the same way."
  (setf digest.writing nil)
  (when called-p (digest-note-said digest text))
  digest)

;;; --- the round in flight, as the card counts it -------------------------------
;;; Whom a round waits on, and how much it has streamed and how fast, from the
;;; deltas the card already folds: a token is four characters, the engine's
;;; own estimate for a round cut short. The card shows a number that moves --
;;; `Thinking · ~1.8k tokens · 40 tok/s' -- and names a silence: `Waiting on
;;; xiaomi · no reply for 1m20s' (2026-10-22: a card that said only `Waiting
;;; on the provider' over a 100 s cold start read as nodecode being down). A
;;; table beside the digest, never slots of it: a running image keeps its
;;; digests.

(nlk:define-side-table round-stream (:test #'eq :weakness :key :synchronized t)
  "DIGEST -> its round in flight as the card counts it, a plist: :PROVIDER
whom its rounds wait on and :ASKED once the host looked; :FIRST-MS when the
round's first part streamed, :CHARS the characters of its thinking and words,
:CALLS each streaming call's index and the length of its arguments so far.")

(defun digest-note-streamed (digest chars now)
  "One delta of DIGEST's round in flight streamed CHARS characters at NOW."
  (let ((round (round-stream digest)))
    (setf (round-stream digest)
          (list* :first-ms (or (getf round :first-ms) now)
                 :chars (+ (max 0 chars) (or (getf round :chars) 0))
                 (alexandria:remove-from-plist round :first-ms :chars))))
  digest)

(defun digest-note-call-streamed (digest index length now)
  "A call of DIGEST's round in flight, its stream INDEX, has streamed
arguments LENGTH characters long by NOW: a call's delta carries its
arguments whole so far, so it replaces that call's count."
  (let* ((round (round-stream digest))
         (calls (getf round :calls)))
    (setf (round-stream digest)
          (list* :first-ms (or (getf round :first-ms) now)
                 :calls (acons index (max 0 length) (remove index calls :key #'car))
                 (alexandria:remove-from-plist round :first-ms :calls))))
  digest)

(defun digest-note-round-over (digest)
  "DIGEST's round in flight ended or was voided: the next counts from nothing."
  (setf (round-stream digest)
        (alexandria:remove-from-plist (round-stream digest) :first-ms :chars :calls))
  digest)

(defun digest-note-provider (digest provider)
  "DIGEST's rounds wait on PROVIDER from now on; NIL says the host looked and
found no name."
  (setf (round-stream digest)
        (list* :provider provider :asked t
               (alexandria:remove-from-plist (round-stream digest) :provider :asked)))
  digest)

(defun round-provider (digest)
  "Whom DIGEST's round waits on: the newest the host or a failover named,
else what its last round ran on, else NIL."
  (or (getf (round-stream digest) :provider)
      (getf digest.usage :provider)))

(defun round-flow (digest now)
  "What DIGEST's round in flight has streamed by NOW -- `~1.8k tokens · 40
tok/s', the rate once a second has passed -- or NIL before anything streamed."
  (let* ((round (round-stream digest))
         (first (getf round :first-ms))
         (chars (+ (or (getf round :chars) 0)
                   (reduce #'+ (getf round :calls) :key #'cdr))))
    (when (and first (plusp chars))
      (let ((tokens (ceiling chars 4))
            (seconds (/ (- now first) 1000)))
        (format nil "~~~a token~p~@[ · ~d tok/s~]"
                (nle:format-token-count tokens) tokens
                (and (>= seconds 1) (round tokens seconds)))))))

(defun waiting-headline (digest now)
  "A silent round's line: whom it waits on, and how long nothing has come."
  (format nil "Waiting on ~a · no reply for ~a"
          (or (round-provider digest) "the provider")
          (elapsed-label (- now (digest-seen-ms digest)))))

(defun flowing-headline (verb digest now)
  "VERB, and what the round in flight streamed once it streamed anything."
  (let ((flow (round-flow digest now)))
    (if flow (format nil "~a · ~a" verb flow) verb)))

;;; --- a turn that ended with its own work still in flight ---------------------
;;; A turn can end on the model's word while a backgrounded evaluation it
;;; spawned is still running: the engine says so on the completion fact
;;; (turn.lisp hands the count to COMPLETE-TURN), and the word was spoken
;;; standing on a handoff it has no result for yet — status, not the ask's
;;; answer. The ask stays OPEN: the line the ask is watching keeps saying what
;;; the wait is, its working mark stays, no answer and no write-back land, and
;;; the exit wake's turn takes the same line over and answers for real. The
;;; count a turn left in flight: the ask is unanswered while it is positive,
;;; and forgotten once a later turn on the lane carries the work.

(defun digest-note-background-pending (digest count)
  "The turn ended with COUNT of its own background evaluations in flight."
  (when (and digest (integerp count) (plusp count))
    (setf digest.background-pending count))
  digest)

;; Forgotten once recorded.

(defun digest-note-waiting-words (digest text)
  "One word a waiting turn spoke."
  ;; Kept for the room's record, so the exchange written back names the ask,
  ;; what was said on the way, and the answer.
  (when (and digest (stringp text) (plusp (length text)))
    (setf digest.waiting-words (append digest.waiting-words (list text))))
  digest)

(defun lane-turn-digest (lane turn-id now &aux (digest lane.digest))
  "LANE's digest for TURN-ID, created on first sight; a different turn id
replaces the previous digest (the gateway serializes turns per session, so
newest wins is chronology, not a race)."
  ;; Caller holds the lane lock.
  ;;
  ;; A digest opened before its turn existed — an ask holding a queued status
  ;; line at a concurrency gate — has no turn id yet and ADOPTS the first one
  ;; it sees, rather than being replaced by it. Replacing would orphan the
  ;; message the ask already posted.
  (cond
    ((and digest (equal turn-id (turn-digest-turn-id digest))) digest)
    ((and digest (null (turn-digest-turn-id digest))) (setf digest.turn-id turn-id) digest)
    (t (let ((fresh (lane-open-digest lane now)))
         (setf fresh.turn-id turn-id)
         fresh))))

(defun lane-open-digest (lane now)
  "Open a fresh digest for an ask whose turn does not exist yet — the queued
state. Caller holds the lane lock."
  (setf lane.digest (make-turn-digest :started-at-ms now :session-id lane.session-id)))

(defun digest-seen-ms (digest)
  "The newest moment DIGEST's turn was seen: its start, or the newest part,
call or round it showed (DIGEST-NOTE-VISIBLE) — the same sighting the stall
notice reads. 0 for a digest that never opened."
  (max digest.started-at-ms digest.visible-at-ms))

;;; --- folds (lane lock held, no I/O) ----------------------------------------

(defun digest-note-tool-call (digest)
  (incf digest.tool-calls)
  digest)

(defun digest-thinking-tail (text &aux (size (length text)))
  "TEXT bounded to the newest +DIGEST-THINKING-CAP+ characters."
  (if (> size +digest-thinking-cap+)
      (subseq text (- size +digest-thinking-cap+))
      text))

(defun digest-note-thinking (digest text)
  "One reasoning delta streamed in."
  ;; The tail is what the card reads: where the thought currently is, not
  ;; where it started.
  (when (and (stringp text) (plusp (length text)))
    (setf digest.thinking (digest-thinking-tail (concatenate 'string (or digest.thinking "") text))))
  digest)

(defun digest-note-round (digest text &optional reasoning)
  "One assistant round committed."
  ;; TEXT is the round's deliverable text or NIL (a tool-only round),
  ;; REASONING its committed thinking.
  ;;
  ;; Text out loud supersedes thinking — it is newer and it is deliverable: as
  ;; the room's own message when the round called tools
  ;; (DIGEST-NOTE-COMMENTARY), as the answer when it ended the turn. A round
  ;; that only thought keeps thinking as the card's thought, replacing
  ;; whatever the deltas had streamed with the round's own committed trace,
  ;; ended by a line so the next round's thought starts a sentence of its own;
  ;; a round that did neither leaves the thought alone, so the thought that
  ;; chose the tool stays on the card while the tool runs.
  (cond
    ((and (stringp text) (plusp (length text))) (setf digest.answer text digest.thinking nil))
    ((and (stringp reasoning) (plusp (length reasoning)))
     (setf digest.thinking (digest-thinking-tail (format nil "~a~%" (string-right-trim '(#\Newline) reasoning))))))
  digest)

(defun digest-note-terminal (digest phase detail now)
  "The turn ended: PHASE is :completed / :steered / :failed / :cancelled."
  ;; Elapsed freezes here so the terminal card is stable across replans.
  (setf digest.phase phase digest.detail detail digest.ended-at-ms now)
  digest)

(defun digest-note-queued (digest position)
  "The ask is waiting at a concurrency gate with POSITION asks ahead of it."
  ;; Re-folded on every promotion so the card counts down.
  (setf digest.phase :queued
        digest.queue-position (max 0 position))
  digest)

(defun digest-note-admitted (digest now)
  "The gate let the ask through."
  ;; Elapsed restarts here: the card times the turn, and the wait was already
  ;; its own visible phase.
  (setf digest.phase :running digest.queue-position 0 digest.started-at-ms now)
  digest)

(defun digest-note-pending (digest prompts)
  "The session's parked input as the kernel's queue snapshot lists it:
PROMPTS is a list of (:prompt-id ID :text TEXT :steer-p BOOL), replacing
whatever was pending — every snapshot is the whole queue, so a promotion
or a retraction simply lists one row fewer."
  ;; Marks the change when the rows differ, so the next plan bypasses the
  ;; refresh throttle once.
  (unless (equal prompts digest.pending) (setf digest.pending prompts digest.pending-changed-p t))
  digest)

;;; --- the turn's work, in words (folds) ---------------------------------------

(defun digest-note-visible (digest now)
  "The turn showed something at NOW — a streamed part, a call starting or
landing, a committed round. The stall notice reads the newest of these."
  (setf digest.visible-at-ms now)
  digest)

(defun sentence-case (text)
  "TEXT with its first character upper case."
  (if (plusp (length text))
      (concatenate 'string (string (char-upcase (char text 0))) (subseq text 1))
      text))

(defun digest-note-call-start (digest call-id name arguments now)
  "One call started: a step of the turn, its words already classified, and
the turn stamped seen."
  ;; The words are the classifier's the TUI's cards read — `Reading
  ;; src/tui/ui.lisp' while it runs — or the verb phrase its bar falls back
  ;; on for a call it has no title for.
  (when (and (stringp call-id) (plusp (length call-id)))
    (let ((steps (append (remove call-id digest.steps :key #'digest-step-call-id :test #'equal)
                         (list (make-digest-step
                                :call-id call-id :name name :arguments arguments
                                :running (or (step-words (nle:tool-arguments-activity
                                                          name (or arguments ""))
                                                         :running t)
                                             (sentence-case (or (nle:tool-activity-verb name arguments)
                                                                name ""))
                                             "")
                                :started-ms now
                                :number (1+ (let ((newest (car (last digest.steps))))
                                              (if newest (digest-step-number newest) 0))))))))
      ;; The step this one pushed past the newest keeps its details' lines alone.
      (when (> (length steps) +steps-with-output+)
        (let ((older (nth (- (length steps) 1 +steps-with-output+) steps)))
          (setf (digest-step-output older) (output-tail (digest-step-output older)))))
      (setf digest.steps (last steps +digest-steps-kept+)
            digest.visible-at-ms now)))
  digest)

(defun output-tail (text)
  "The last +DIGEST-OUTPUT-LINES+ lines of TEXT that say anything, each cut
short, or NIL."
  (let ((lines (and (stringp text)
                    (remove-if (lambda (line) (zerop (length (nlk:trimmed line))))
                               (uiop:split-string text :separator '(#\Newline))))))
    (when lines
      (format nil "~{~a~^~%~}"
              (mapcar (lambda (line)
                        (nlk:clip (string-right-trim '(#\Return #\Space #\Tab) line) 160 :ellipsis "…"))
                      (last lines +digest-output-lines+))))))

(defun text-end (text cap)
  "The end of TEXT, at most CAP characters from the start of a line, `…' where
it was cut, or NIL when TEXT says nothing: what a step's Output press shows of
its answer (+STEP-OUTPUT-CHARS+), and a card of the words a turn says."
  (let* ((text (and (stringp text) (string-right-trim '(#\Newline #\Return #\Space #\Tab) text)))
         (start (and text (max 0 (- (length text) cap))))
         (line (and start (plusp start) (position #\Newline text :start start))))
    (when (and text (plusp (length (nlk:trimmed text))))
      (if (zerop start)
          text
          (format nil "…~%~a" (subseq text (if line (1+ line) start)))))))

(defun step-image (metadata number)
  "The picture a call numbered NUMBER looked at, by its result's METADATA —
(NAME PATH DESCRIPTION), NAME the file a platform calls it — or NIL."
  ;; LOOK's image fact (exec.lisp IMAGE-FACT): a file on this machine, read
  ;; by the platform when the card first carries it.
  (let* ((image (and (hash-table-p metadata) (gethash "image" metadata)))
         (path (and (hash-table-p image) (equal "look" (gethash "source" image))
                    (gethash "path" image)))
         (type (and path (gethash "media_type" image))))
    (when (and (stringp path) (stringp type) (eql 0 (search "image/" type)))
      (list (format nil "step-~d.~a" number (subseq type (length "image/")))
            path
            (subseq path (1+ (or (position #\/ path :from-end t) -1)))))))

(defun digest-note-call-result (digest call-id now &key metadata output failed)
  "One call landed: its step settles into the past tense, taking whatever
words the result added — the receipts on METADATA name what a snippet of free
Lisp did (activity.lisp), read as the transcript's row reads them at the same
moment — and keeps the end of OUTPUT, what the call answered, and the picture
it looked at. FAILED is the result fact's verdict, never inferred from OUTPUT;
structured stop metadata distinguishes interruption from success."
  (nlk:when-let (call (find call-id digest.steps :key #'digest-step-call-id :test #'equal))
    (setf (step-outcome call) (cond ((nlk:json-value metadata :object "stopped") :stopped)
                                   (failed :failed)
                                   (t :done))
          call.settled (or (and call.name
                                (step-words (nle:tool-arguments-activity
                                             call.name (or call.arguments "") :metadata metadata)))
                           call.running)
          call.ended-ms (max now 1)
          call.output (if (member call (last digest.steps +steps-with-output+))
                          (text-end output +step-output-chars+)
                          (output-tail output))
          call.image (step-image metadata call.number)))
  (setf digest.visible-at-ms now)
  digest)

(defun digest-note-usage (digest round)
  "One provider round's usage fact landed. ROUND is a plist in the TUI
accumulator's keys: the counts :INPUT :OUTPUT :CACHED :CACHE-WRITE
:REASONING, :COST-KNOWN and :COST, :ESTIMATED, and what it ran on,
:PROVIDER :MODEL :RESPONSE-MODEL :EFFORT :FINISH-REASON — any NIL when unsaid."
  ;; The TUI's rule (INGEST-TURN-USAGE): counts sum over the turn's rounds
  ;; and NIL is unknown, never zero; one estimated round (a cut stream) marks
  ;; the sum ~ for the rest of the turn; the cost joins only when priced; a
  ;; round silent about what it ran on keeps the previous round's words. The
  ;; window holds one prompt at a time, so CONTEXT is the newest round alone.
  (flet ((said (key) (let ((text (getf round key))) (and (stringp text) (plusp (length text)) text))))
    (let ((turn digest.usage))
      (setf digest.usage
            (append (nle:usage-plus turn (loop for key in '(:input :output :cached :cache-write :reasoning)
                                               collect key collect (getf round key)))
                    (list :estimated (or (getf turn :estimated) (and (getf round :estimated) t))
                          :cost-known (or (getf turn :cost-known) (and (getf round :cost-known) t))
                          :cost (let ((cost (getf round :cost)))
                                  (if (and (getf round :cost-known) (numberp cost))
                                      (+ (or (getf turn :cost) 0) cost)
                                      (getf turn :cost))))
                    (loop for key in '(:provider :model :response-model :effort :finish-reason)
                          collect key collect (or (said key) (getf turn key))))
            digest.context (list :input (getf round :input) :cached (getf round :cached)
                                 :cache-write (getf round :cache-write)
                                 :model (or (said :model) (getf digest.context :model))))))
  digest)

;;; --- the thought, in whole words ----------------------------------------------
;;; The card names what the turn is thinking in one line the room can read: a
;;; reasoning summary opens each step on a bold title of its own line, and
;;; that title is the headline; raw reasoning has none, and its newest whole
;;; sentence is. Never the stream's tail: cut mid-word it reads as scratch, and
;;; the room may take it for the answer.

(defparameter +thought-headline-cap+ 100
  "Characters a thought's headline takes before it cuts at a word.")

(defun clip-at-word (text cap)
  "TEXT whole when it fits CAP, else cut at the last word that does, `…' after."
  (if (<= (length text) cap)
      text
      (let ((space (position #\Space text :end cap :from-end t)))
        (concatenate 'string (string-right-trim " ,;:." (subseq text 0 (or space cap))) "…"))))

(defun whole-sentences (text)
  "The sentences TEXT holds whole, oldest first, each on one line: the one it
is still writing left out, and its first when the thinking cap cut its head."
  ;; A sentence ends at . ! or ? before a space or the end, or at a line's end.
  (when (stringp text)
    (let* ((pieces (remove-if (lambda (piece) (< (length piece) 3))
                              (mapcar #'nlk:trimmed
                                      (ppcre:split "(?<=[.!?])[ \\t]+|\\n+" text))))
           (ended (ppcre:scan "(?:[.!?]|\\n)\\s*\\z" text))
           (whole (if ended pieces (butlast pieces))))
      (if (>= (length text) +digest-thinking-cap+) (rest whole) whole))))

(defun thought-headline (text)
  "The headline of the thought TEXT holds: its newest bold title on a line of
its own, else its newest whole sentence, cut at a word past
+THOUGHT-HEADLINE-CAP+; NIL while it holds neither."
  (let ((title nil))
    (when (stringp text)
      (ppcre:do-register-groups (inner) ("(?m)^[ \\t]*\\*\\*([^*\\n]{2,160})\\*\\*[ \\t]*$" text)
        (setf title inner)))
    (nlk:when-let (line (or title
                            (car (last (remove-if (lambda (sentence)
                                                    (ppcre:scan "^\\*\\*.*\\*\\*$" sentence))
                                                  (whole-sentences text))))))
      (clip-at-word (nlk:one-line line) +thought-headline-cap+))))

;;; --- the card -------------------------------------------------------------------
;;; DIGEST-CARD is what a platform draws: a plist of the card's parts, each in
;;; the words the room reads —
;;;
;;;   :STATE     :queued, :working, :done, :stopped, :failed or :paused
;;;   :ELAPSED   how long the turn has run, `12s'
;;;   :TASK      what the ask is, its title, or NIL before it has one
;;;   :HEADLINE  what the turn is doing now, or how it ended
;;;   :THOUGHT   the headline of the thought that chose it, or NIL
;;;   :STEPS     the newest steps, oldest first, each (MARK WORDS TIME PRESS):
;;;              MARK :done, :failed, :running or :stopped, TIME how long it ran,
;;;              PRESS the data its Output button carries (STEP-PRESS) once
;;;              it landed with something to show, else NIL
;;;   :EARLIER   how many older steps the card leaves to its details
;;;   :NOTE      a failover's note, a settled turn's detail, or NIL
;;;   :META      its numbers, up to two lines (DIGEST-META), or NIL
;;;   :PENDING   the input parked behind the turn, a line each
;;;   :SAID      the newest words the turn said on its way, or the words its
;;;              round is writing with the cursor at their end — on a running
;;;              card, and on one that settled with no answer — or NIL
;;;   :IMAGES    the newest pictures it looked at, oldest first, each (NAME
;;;              PATH DESCRIPTION): NAME the file a platform calls it
;;;
;;; A platform that draws nothing richer posts its CARD-TEXT.

(defparameter +card-steps+ 4
  "Steps a card shows, the running one included; its details show the rest.")

(defparameter +card-images+ 4
  "Pictures a card shows: the newest the turn looked at.")

(defparameter +step-press-prefix+ "nck:step:"
  "What a step's Output press data begins with; the session id and the
step's number follow.")

(defun step-press (session-id number)
  "The data a step's Output press carries: its card's room and its number."
  (format nil "~a~a:~d" +step-press-prefix+ session-id number))

;;; A glance at what is waiting, not the message: the whole of it runs as its
;;; own turn.
(defparameter +digest-pending-cap+ 80
  "Characters of a parked message shown on its ⌎ row.")

;;; The TUI's band caps at the same five.
(defparameter +digest-pending-rows+ 5
  "Parked rows shown under the card; the rest are one count.")

(defun elapsed-label (ms)
  "MS as the card says a time: `12s', `2m05s'."
  (let ((seconds (max 0 (truncate ms 1000))))
    (if (< seconds 120)
        (format nil "~ds" seconds)
        (format nil "~dm~2,'0ds" (truncate seconds 60) (mod seconds 60)))))

(defun digest-elapsed-label (digest now)
  (elapsed-label (- (if (plusp digest.ended-at-ms) digest.ended-at-ms now)
                    digest.started-at-ms)))

(defun digest-note-line (note cap &aux (line (nlk:one-line (nlk:first-line (or note "")))))
  "NOTE flattened to one line and capped: its first line, for a one-line
notice (a post that failed). A card shows every line (DIGEST-NOTE-LINES)."
  (nlk:clip line cap))

(defun digest-note-lines (note)
  "NOTE as a card's notice: each of its lines, flattened and capped at
+DIGEST-NOTE-CAP+, at most +DIGEST-NOTE-ROWS+, then how many more the
turn's record keeps."
  ;; The card showed the first line alone, and a walked failover chain's
  ;; failure is a headline over a model a line, each with what its provider
  ;; said: every model but the headline was hidden, the one that failed first
  ;; among them.
  (let* ((lines (loop for line in (uiop:split-string (or note "") :separator '(#\Newline))
                      for flat = (nlk:one-line line)
                      when (plusp (length flat)) collect (nlk:clip flat +digest-note-cap+)))
         (shown (subseq lines 0 (min (length lines) +digest-note-rows+)))
         (more (- (length lines) (length shown))))
    (if (plusp more)
        (append shown (list (format nil "… ~d more line~:p in the turn's record" more)))
        shown)))

(defun pending-lines (digest)
  "The ⌎ rows of the input parked behind DIGEST's turn, in promotion order,
steers first; the next to run says when."
  (let* ((pending digest.pending)
         (ordered (append (remove-if-not (lambda (row) (getf row :steer-p)) pending)
                          (remove-if (lambda (row) (getf row :steer-p)) pending)))
         (shown (subseq ordered 0 (min (length ordered) +digest-pending-rows+)))
         (rest (- (length ordered) (length shown))))
    (append
     (loop for row in shown
           for first = t then nil
           ;; The parked text's last line, less its `name [m.. u..]: ' prefix.
           for flat = (string-right-trim '(#\Newline #\Return #\Space) (or (getf row :text) ""))
           for start = (position #\Newline flat :from-end t)
           for line = (if start (subseq flat (1+ start)) flat)
           for colon = (search "]: " line)
           for tag = (and colon (search " [" line :end2 colon))
           collect (format nil "⌎ ~a~@[ — ~a~]"
                           (digest-note-line (if tag (subseq line (+ colon 3)) line)
                                             +digest-pending-cap+)
                           (and first (if (getf row :steer-p) "after this round" "after this turn"))))
     (and (plusp rest) (list (format nil "⌎ ~d more waiting" rest))))))

(defun step-row (call now &optional (end-mark :done))
  "CALL as a card's step, (MARK WORDS TIME): running at NOW while it has not
ended, then its result's outcome; END-MARK stops an unreturned call."
  (let ((open (zerop call.ended-ms)))
    (list (cond ((not open) (or (step-outcome call) :done))
                ((eq end-mark :done) :running)
                (t end-mark))
          (nlk:clip (if open call.running (or call.settled call.running)) +digest-step-cap+
                    :ellipsis "…")
          (elapsed-label (- (if open now call.ended-ms) call.started-ms)))))

(defun digest-meta (digest &aux (usage digest.usage) (context digest.context)
                                (running (eq digest.phase :running)))
  "The card's numbers, the TUI's run line in two: what the turn ran on and
the steps it took, then its token meter, the window its newest round filled,
what it cost and, once it ended, a finish that was not clean."
  ;; Every number is the TUI's own words (NLE:USAGE-METER-TEXT, the finish
  ;; divider's meter: `↑240k c93.9% w800 ↓16.7k r3.2k'); the window says
  ;; `ctx 41%' as the bar's pressure clause does — always, since a room has
  ;; no header to read it from — or the prompt's size, `ctx 87.6k', when the
  ;; model's window is unknown. Each part joins only when its fact exists.
  (let* ((used (nle:context-occupancy-tokens context))
         (window (and (plusp used) (getf context :model)
                      (ignore-errors (nle:model-context-tokens (getf context :model) (getf usage :provider)))))
         (meter (nle:usage-meter-text usage))
         (reason (getf usage :finish-reason)))
    (flet ((line (&rest parts) (format nil "~{~a~^ · ~}" (remove nil parts))))
      (let ((head (line (nle:usage-identity-text usage)
                        (and (plusp digest.tool-calls) (format nil "~d step~:p" digest.tool-calls))))
            (numbers (line (and meter (string-left-trim " ·" meter))
                           (cond ((and window (plusp window))
                                  (format nil "ctx ~d%" (min 999 (floor (* used 100) window))))
                                 ((plusp used) (format nil "ctx ~a" (nle:format-token-count used))))
                           (and (getf usage :cost-known) (format nil "$~,3f" (or (getf usage :cost) 0)))
                           (and (not running) reason
                                (not (member reason '("stop" "tool_calls") :test #'string-equal))
                                (format nil "finish ~a" reason)))))
        (let ((lines (remove "" (list head numbers) :test #'equal)))
          (and lines (format nil "~{~a~^~%~}" lines)))))))

(defun digest-card (digest now)
  "The card for DIGEST at NOW, the plist a platform draws (see above), or NIL
for a turn a steer cut short: the next turn raises its own."
  ;; A running card says what the turn is doing in the classifier's words —
  ;; or that it waits on its own background work, on a stalled provider, or
  ;; writes, or thinks — the thought that chose it, its newest steps and the
  ;; input parked behind it. A settled card says how the turn ended and when,
  ;; over its last steps: a call a stop or a failure cut short is marked so.
  (let* ((phase digest.phase)
         (open (digest-open-step digest))
         (steps (last digest.steps +card-steps+))
         (end-mark (if (eq phase :running) :done :stopped))
         (elapsed (digest-elapsed-label digest now))
         (writing (nlk:trimmed (or digest.writing "")))
         ;; The newest words it said; while it waits on its own background
         ;; work, the word it said before waiting.
         (said (text-end (car (last (or digest.said digest.waiting-words))) +card-said-cap+)))
    (flet ((card (state headline &key thought note pending said)
             (list :state state :elapsed elapsed :task digest.task :headline headline :thought thought
                   :said said
                   ;; A step that landed with something to show carries its press.
                   :steps (mapcar (lambda (call)
                                    (append (step-row call now end-mark)
                                            (list (and (plusp call.ended-ms) call.output digest.session-id
                                                       (step-press digest.session-id call.number)))))
                                  steps)
                   :earlier (- (length digest.steps) (length steps))
                   :note note :meta (digest-meta digest) :pending pending
                   :images (last (remove nil (mapcar #'digest-step-image digest.steps)) +card-images+))))
      (ecase phase
        (:queued (card :queued (format nil "Queued~@[ · ~d ahead~]"
                                       (and (plusp digest.queue-position) digest.queue-position))))
        (:running
         (card :working
               (cond (digest.background-pending "Waiting on a background evaluation")
                     (open (nlk:clip (digest-step-running open) +digest-step-cap+ :ellipsis "…"))
                     ((> (- now (digest-seen-ms digest)) +digest-stall-ms+)
                      (waiting-headline digest now))
                     ((plusp (length writing)) (flowing-headline "Writing" digest now))
                     (digest.thinking (flowing-headline "Thinking" digest now))
                     (t (flowing-headline "Working" digest now)))
               :thought (thought-headline digest.thinking)
               :note digest.fallback-text
               :pending (pending-lines digest)
               :said (if (plusp (length writing))
                         (concatenate 'string (text-end writing +card-said-cap+) +digest-writing-cursor+)
                         said)))
        ;; Settled, a card says what the turn said only when no answer stands
        ;; below it to say more; what a round was still writing goes.
        (:completed (card :done (format nil "Done in ~a" elapsed)))
        (:cancelled (card :stopped (format nil "Stopped at ~a" elapsed) :note digest.detail :said said))
        (:failed (card :failed (format nil "Failed at ~a" elapsed) :note digest.detail :said said))
        (:paused (card :paused (format nil "Paused at ~a" elapsed) :note digest.detail :said said))
        (:steered nil)))))

(defun step-mark (mark)
  "The character a step's MARK reads as in text."
  (ecase mark (:done "✓") (:failed "❌") (:running "›") (:stopped "×")))

(defun card-text (card)
  "CARD as plain lines, for a platform that draws nothing richer — and for the
record a stopped or failed turn leaves in the room. Its pictures are left out."
  (destructuring-bind (&key state elapsed task headline thought steps earlier note meta pending images said)
      card
    (declare (ignore images))
    (with-output-to-string (out)
      (format out "~@[~a~%~]~a~@[ · ~a~]" task headline (and (eq state :working) elapsed))
      (when thought (format out "~%thinking · ~a" thought))
      (when said (format out "~%~a" said))
      (when (and earlier (plusp earlier)) (format out "~%+~d earlier" earlier))
      (loop for (mark words time) in steps
            do (format out "~%~a ~a · ~a" (step-mark mark) words time))
      (dolist (line (and note (digest-note-lines note))) (format out "~%> ~a" line))
      (when meta (format out "~%~a" meta))
      (dolist (line pending) (format out "~%~a" line)))))

;;; --- the details (whoever pressed Details, privately) ------------------------------

(defun digest-details (digest now)
  "Everything DIGEST's card leaves out, the plist its Details press shows:
:STEPS every step kept, each (MARK WORDS TIME OUTPUT NUMBER) — OUTPUT the end
of what it answered, which a step's Output press shows whole and the details
by its last lines — :DROPPED the older steps the digest let go, :THOUGHT the
newest thought's whole sentences, :SAID every word the turn said on its way,
oldest first."
  (let ((end-mark (if (eq digest.phase :running) :done :stopped)))
    (list :steps (mapcar (lambda (call) (append (step-row call now end-mark) (list call.output call.number)))
                         digest.steps)
          :said (append digest.waiting-words digest.said)
          ;; Past the steps kept the oldest drop; the count still names them.
          :dropped (if (>= (length digest.steps) +digest-steps-kept+)
                       (max 0 (- digest.tool-calls (length digest.steps)))
                       0)
          :thought (let ((sentences (whole-sentences digest.thinking)))
                     (and sentences (format nil "~{~a~^ ~}" sentences))))))

(defun details-text (details limit)
  "DETAILS (DIGEST-DETAILS) as one message of at most LIMIT characters: the
numbered steps, the end of each one's answer under it, what the turn said on
its way and the thought. When it is long the outputs go first, then what it
said and the thought take what room is left — the newest words first — then
the oldest steps go."
  (destructuring-bind (&key steps dropped thought said) details
    (labels ((render (steps outputs skipped thought said)
               (with-output-to-string (out)
                 (format out "**Steps**~:[ · none yet~; · ~:*~d~]" (and (or steps (plusp skipped))
                                                                        (+ (length steps) skipped)))
                 (when (plusp skipped) (format out "~%… ~d earlier" skipped))
                 (loop for (mark words time output number) in steps
                       for n from (1+ skipped)
                       do (format out "~%~d. ~a ~a · ~a" (or number n) (step-mark mark) words time)
                          (when (and outputs output)
                            (format out "~%```~%~a~%```" (remove #\` (output-tail output)))))
                 (when said
                   (format out "~%**Said**~%~a" said))
                 (when thought
                   (format out "~%**Thinking**~%> ~a" thought))))
             (fits (text) (and (<= (length text) limit) text)))
      (let ((thought (and thought (clip-at-word (nlk:one-line thought) 900)))
            (said (and said (format nil "~{~a~^~%~%~}" said))))
        (or (fits (render steps t dropped thought said))
            (fits (render steps nil dropped thought said))
            (nlk:when-let (bare (fits (render steps nil dropped nil nil)))
              ;; The room left goes to the words it said, their newest end,
              ;; then to the thought; the Said heading takes 10 characters, the
              ;; Thinking one 16, and an end cut from the front its `…' line.
              (let* ((room (- limit (length bare)))
                     (said (and said (> room 50) (text-end said (- room 12))))
                     (room (- room (if said (+ 10 (length said)) 0) 17))
                     (thought (and thought (> room 20) (clip-at-word thought room))))
                (or (fits (render steps nil dropped thought said)) bare)))
            (loop for keep from (1- (length steps)) downto 0
                  for text = (fits (render (last steps keep) nil (+ dropped (- (length steps) keep)) nil nil))
                  when text return text)
            (nlk:clip (render nil nil (+ dropped (length steps)) nil nil) (1- limit) :ellipsis "…"))))))

(defun step-text (step limit)
  "STEP, a details row (DIGEST-DETAILS), as the one message its Output press
answers, at most LIMIT characters: the step, then the end of what it answered
in a block, cut from the front when it is long."
  (destructuring-bind (mark words time &optional output number) step
    (let ((head (format nil "**~@[~d. ~]~a ~a · ~a**" number (step-mark mark) words time)))
      (if (null output)
          (format nil "~a~%It answered nothing to show." head)
          ;; The block's fences and line breaks take nine characters.
          (let ((body (remove #\` output))
                (room (max 1 (- limit (length head) 9))))
            (format nil "~a~%```~%~a~%```" head
                    (if (> (length body) room)
                        (concatenate 'string "…" (subseq body (- (length body) (1- room))))
                        body)))))))

;;; --- the delivery decision ---------------------------------------------------

(defun digest-status-plan (digest now &key (min-update-ms
                                            +digest-status-refresh-ms+)
                                           controls)
  "(values KIND CARD) — the one card delivery decision."
  ;; KIND is :post (create the card's message), :edit (settle the new card
  ;; into it), or :skip.
  ;;
  ;; :COMPLETED skips here: the answer posts first and the card settles below
  ;; it (SETTLE-CARD, host.lisp); :STEERED has no card. A failed or stopped
  ;; turn settles its notice into the card — creating one when the turn died
  ;; before earning it, so a fast failure is never silent — and bypasses the
  ;; refresh throttle, because nothing follows it. A change in the parked rows
  ;; bypasses it once too: the acknowledgement of a message just sent is not
  ;; a refresh. CONTROLS is the control set the card would carry — the kit's
  ;; buttons, :CLEAR for a settle that retires them — and a set that differs
  ;; from the last attempted delivery is itself a delivery: a press's label
  ;; flip waits for no window.
  (let* ((phase digest.phase)
         (live (member phase '(:queued :running)))
         (status-id digest.status-id))
    (cond
      ((member phase '(:completed :steered)) (values :skip nil))
      ;; A live turn earns its card at once, or after a thinking or stall window.
      ((and live
            (not (or (eq phase :queued)
                     (turn-digest-pending digest)
                     (plusp (turn-digest-tool-calls digest))
                     digest.fallback-text
                     status-id
                     (>= (- now (turn-digest-started-at-ms digest))
                         (if (or (turn-digest-thinking digest) (turn-digest-writing digest))
                             +digest-earn-ms+
                             +digest-stall-ms+)))))
       (values :skip nil))
      (t
       (let ((card (digest-card digest now))
             (moved (not (equal controls digest.controls))))
         (cond
           ((and (equal card (turn-digest-posted-card digest)) (not moved))
            (values :skip nil))
           ((and live (not moved)
                 (not (turn-digest-pending-changed-p digest))
                 (plusp (turn-digest-posted-at-ms digest))
                 (< (- now (turn-digest-posted-at-ms digest)) min-update-ms))
            (values :skip nil))
           ((and (null status-id) (eq (getf card :state) digest.refused-state))
            (values :skip nil))
           ((null status-id) (values :post card))
           (t (values :edit card))))))))

(defun digest-status-attempted (digest now &key controls)
  "Stamp a delivery attempt — success or failure — as the throttle base and
as the controls the attempt carries, so a label that moved is not re-planned
on every tick."
  ;; The attempt consumes a pending change's throttle bypass: a failing edit
  ;; falls back to the refresh window rather than replanning every tick.
  (setf digest.posted-at-ms now
        digest.pending-changed-p nil)
  (when controls (setf digest.controls controls))
  digest)

(defun digest-status-refused (digest card)
  "Record that the platform refused to post CARD: no post is planned again
while the card stays in its state."
  ;; A state that moves — working to failed, say — is a different card, and
  ;; earns one more try, so a turn that fails is still never silent.
  (setf digest.refused-state (getf card :state))
  digest)

(defun digest-status-delivered (digest card &optional message-id (media nil media-p))
  "Record a delivered CARD; MESSAGE-ID on the creating post, MEDIA what its
message now holds of its pictures (the platform's MEDIA-OF)."
  (setf digest.posted-card card)
  (when message-id
    (setf digest.status-id message-id))
  (when media-p
    (setf digest.posted-media media))
  digest)

(defun digest-final-text (digest)
  "The turn's final answer: the newest non-empty assistant text, exposed
once the turn COMPLETED."
  ;; Thinking is never an answer. NIL for failed/cancelled turns — their
  ;; notice carries the terminal detail instead.
  (and (eq digest.phase :completed)
       digest.answer))
