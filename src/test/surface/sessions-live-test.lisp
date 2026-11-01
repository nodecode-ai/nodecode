;;;; sessions-live-test.lisp --- a session followed live from /sessions, and
;;;; the Left that leaves a session running.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Gate claims:
;;;;
;;;;   L1. The preview's follow is a live-only subscription: the followed
;;;;       session's frames arrive with no replay, beside the attached
;;;;       session's own.
;;;;   L2. The /sessions preview runs the highlighted session live once the
;;;;       highlight rests: its turn starting, streaming and ending land in
;;;;       the lane under the session's directory name, never in the live
;;;;       state; Esc drops the lane and the follow.
;;;;   L3. Only the attached and the followed session's frames dispatch.
;;;;   L4. Left on an idle composer -- the home screen included -- goes home
;;;;       on a fresh session with /sessions open on the session it left,
;;;;       marking nothing; with text in the composer Left is the cursor
;;;;       move, and with the picker up it is the picker's fold.
;;;;   L5. The footer advertises the gesture, counting the sessions running.

(in-package #:nodecode.test)

(defun session-row-for (id rows)
  "The row of ROWS -- plists, as NLK:LIST-SESSIONS answers -- whose :ID is ID."
  (find id rows :key (lambda (row) (getf row :id)) :test #'equal))

(defun feed-organism (model kind &rest payload)
  "Hand MODEL one unsequenced KIND frame whose payload holds PAYLOAD's keys and values."
  (feed-wire model (nlk:encode-unsequenced-frame kind (apply #'nlk:make-json-object payload))))

;;; --- L1-L3: the live preview ---------------------------------------------------

(defun preview-fact (model session-id seq turn-id kind payload)
  "One session-scoped durable-fact frame for SESSION-ID, as the wire
carries it, handed to MODEL the way the loop thread hands it."
  (feed-wire model (nlk::encode-projection-event
                    session-id seq "session_stream_event"
                    (nlk:json-object "frame" (fact-frame kind turn-id :session-id session-id
                                                         :seq seq :payload payload
                                                         :occurred-at (nlk::iso-now))))))

(deftest a-live-only-subscription-streams-without-a-replay (with-temp-gateway (port))
  ;; The preview lane's follow at the gateway: the entry joins the
  ;; session's live stream and gets no replay and no band snapshot; the
  ;; attached entry beside it is served as ever. The turn starts once the
  ;; gateway holds the follow: what went out before, no replay brings back.
  (seed-user-inputs "s-att" '("attached history"))
  (seed-user-inputs "ag-follow" '("followed history"))
  (is-let* ((frames (frames-while (ws port :subscribe '("s-att" ("ag-follow" :live-only t))
                                           :await-subscription t)
                      (nlk:admit-turn "ag-follow" "ag-follow-cmd" "live prompt")
                      (nle::publish-turn-facts "ag-follow" 0)))
            (resumed (frames-of-type frames "session_stream_resumed"))
            (live (remove "ag-follow" (frames-of-type frames "session_stream_event")
                          :test-not #'equal :key (lambda (frame) (gethash "session_id" frame)))))
    ((length resumed) = 1 "one replay") ((gethash "session_id" (first resumed)) "s-att")
    (live is "the followed session's live frames arrive")
    ((frame-carrying live "turn.started" "frame" "event" "type") is)))

(deftest the-sessions-preview-runs-the-highlighted-session-live (with-temp-gateway (port))
  ;; Every selection landing folds the highlighted session's bounded history
  ;; into the transcript above the picker; once the highlight rests the
  ;; shell follows the session with a live-only subscription, and from there
  ;; its frames reduce into the lane -- the turn starting, the text
  ;; streaming, the turn ending -- with the previewed turn's bar under the
  ;; dialog, the one attaching to it paints (BAR-LANE); frames for other
  ;; sessions never touch it; Esc drops the lane and the follow.
  (seed-user-inputs "s-here" '("attached prompt"))
  (seed-user-inputs "ag-prev" '("previewed prompt"))
  ;; A socket stands in so the follow sends; SEND-SUBSCRIBE is stubbed.
  (let ((model (gateway-shell port "s-here" :ws :stub))
        (nle::*workspace-directory* (string-right-trim "/" (namestring (fixture-workspace))))
        (subscribed '())
        (history-reads 0))
    (with-stubbed-fdefinitions ((nle::send-subscribe (ws ids) (push ids subscribed))
                                (nle::gateway-http-session-request
                                 (port method path body token &rest options)
                                 (when (search "/history" path) (incf history-reads))
                                 (apply original port method path body token options)))
      (nle::initialize-chat-model-ui model)
      (setf model.state (nle::initial-chat-state))
      (fold-model model (nle::ingest-message-fact "user" "attached prompt"))
      (nle::slash-sessions-op model)
      (is (nle::chat-model-dialog model) "/sessions opened")
      (sessions-dialog-highlight model "ag-prev")
      (nle::sessions-preview-follow-at-rest model model.dialog)
      (is (transcript-carries-p (nle::visible-transcript-state model) "previewed prompt"))
      (is (equal "ag-prev" (nle::gateway-model-preview-session-id model)))
      (is (equal '("s-here" ("ag-prev" :live-only t)) (first subscribed)))
      (is (null (nle::live-status-line model 100)))
      ;; The session's turn starts, streams, and ends: every frame lands in
      ;; the lane, none in the live state, and the bar follows.
      (let ((reads history-reads))
        (preview-fact model "ag-prev" 1 "t-1" "turn.started" (nlk:json-object "input" "live prompt"))
        (is (nth-value 1 (nle::bar-lane model)) "the previewed turn is in flight")
        (is-carrying (bar (nle::live-status-line model 100)) "waiting"
          (:absent "previewed prompt" "the bar wears no name, as the attached one wears none"))
        (is (nle::live-chrome-animating-p model) "and asks for ticks")
        (feed-wire model (nlk::encode-projection-event
                          "ag-prev" 2 "item_delta"
                          (nlk:json-object "turn_id" "t-1"
                                           "delta" (nlk:json-object "type" "text"
                                                                    "text" "streamed answer"))))
        (is (transcript-carries-p (nle::visible-transcript-state model) "streamed answer"))
        (is (equal "streaming" (car (nle::gateway-model-preview-activity model))))
        (is (not (transcript-carries-p model.state "streamed answer")))
        (preview-fact model "s-other" 3 "t-9" "turn.started" (nlk:json-object "input" "elsewhere"))
        (is (not (transcript-carries-p (nle::visible-transcript-state model) "elsewhere")))
        (preview-fact model "ag-prev" 4 "t-1" "turn.completed" (nlk:json-object "turn-number" 1))
        (is (null (nle::live-status-line model 100)) "the turn ended: no bar")
        (is (= reads history-reads) "nothing refetched along the way"))
      (nle::picker-dialog-key model :escape)
      (is (null model.transcript-preview) "Esc drops the preview")
      (is (null (nle::gateway-model-preview-session-id model)) "and the follow")
      (is (equal '("s-here") (first subscribed)) "the attached session alone again")
      (is (transcript-carries-p (nle::visible-transcript-state model) "attached prompt")))))

(deftest the-dispatch-filter-admits-the-followed-session (let* ((model (detached-shell "s-att"))
                                                                (program (nlt:make-program model))))
  (is (frame-dropped-p program model "ag-follow") "an unfollowed session's frames are dropped")
  (setf (nle::gateway-model-preview-session-id model) "ag-follow")
  (is (not (frame-dropped-p program model "ag-follow")) "the followed session's frames dispatch")
  (is (not (frame-dropped-p program model "s-att")) "as the attached session's always did")
  (is (frame-dropped-p program model "s-else") "and nothing else"))

;;; --- L4-L5: Left leaves the session running -----------------------------------

(defmacro with-leaving-shell ((model calls session-id &key listing) &body body)
  "Run BODY with MODEL a 100x30 gateway shell on SESSION-ID whose HTTP calls are
pushed onto CALLS and answered: a new session is s-fresh, a directory read
LISTING (an empty one when none is given). Subscriptions go nowhere, and a
highlighted row's preview reads nothing."
  `(with-frame-model (,model 100 30 :session-id ,session-id)
     (let ((,calls '()))
       (with-stubbed-fdefinitions ((nle::send-subscribe (ws ids) nil)
                                   (nle::sessions-preview-arm (&rest args) nil)
                                   (nle::gateway-model-http-call (m method path body)
                                     (push (list method path body) ,calls)
                                     (if (and (eq method :get) (equal path "/api/gateway/sessions"))
                                         ,(or listing '(vector))
                                         (nlk:json-object "session_id" "s-fresh"))))
         ,@body))))

(defun listing-row (id title updated-at &optional parent)
  "One directory row as GET /api/gateway/sessions answers it, in this shell's folder."
  (nlk::make-json-object "id" id "title" title "parent_id" parent
                         "working_directory" (nle::workspace-directory)
                         "updated_at" updated-at))

(defun footer-row (model)
  "The footer frame row, MODEL's target known: a placeholder
(`setting up…') shows no gesture."
  (setf (nle::gateway-model-model-facts model) (list :provider "acme" :model "m1"))
  (nth (- (nle::chat-model-term-height model) 2) (nle::compose-frame model)))

(deftest footer-advertises-left-on-both-screens ()
  (with-frame-model (model 80 24 :session-id "s-hint")
    (is (search "acme m1 · ← sessions" (footer-row model)) "the home screen")
    (fold-model model (nle::ingest-message-fact "user" "fix the flaky test"))
    (is (search "acme m1 · ← sessions" (footer-row model)) "and a transcript")))

(deftest footer-counts-the-sessions-running (with-frame-model (model 80 24 :session-id "s-hint"))
  (feed-organism model "organism.activity"
                 "sessions" (vector (nlk:make-json-object "id" "s-a" "started_at" "2026-10-03T10:00:00.000Z")
                                    (nlk:make-json-object "id" "s-b" "started_at" "2026-10-03T10:01:00.000Z")))
  (is (search "acme m1 · ← sessions · 2 running" (footer-row model)))
  (feed-organism model "organism.activity" "sessions" (vector))
  (is (search "acme m1 · ← sessions" (footer-row model)) "none running: the plain hint"))

(deftest left-on-the-home-screen-leaves-for-sessions (with-leaving-shell (model calls "s-home"))
  ;; The home screen: no transcript rows, no turn, an idle composer.
  ;; Left still leaves -- the shell holds an attached session
  ;; whatever its transcript says, the gate the footer hint dropped.
  (nle:update model (nlt:make-key-press-msg :code :left))
  (is (equal "s-fresh" model.session-id) "Left goes home on a fresh session")
  (is (notany (lambda (call) (search "/agent" (second call))) calls) "marking nothing")
  (is-present (dialog (nle::chat-model-dialog model)) "with /sessions open"
    (is-shape dialog (nle::picker-dialog-kind eq :sessions))))

(deftest left-on-an-idle-composer-leaves-the-session-running ()
  (with-leaving-shell (model calls "s-work"
                       :listing (vector (listing-row "s-other" "other" "2026-01-02T00:00:00.000Z")
                                        (listing-row "s-work" "fix the flaky test" "2026-01-01T00:00:00.000Z")))
    (fold-model model (nle::ingest-message-fact "user" "fix the flaky test"))
    ;; Text in the composer: Left is the cursor move it always was.
    (nle:update model (nlt:make-key-press-msg :code #\a :text "a"))
    (nle:update model (nlt:make-key-press-msg :code :left))
    (is (null calls) "Left with text in the composer calls nothing")
    (is (equal "s-work" model.session-id) "and leaves nothing")
    ;; The cursor moved before the a; step past it and delete it.
    (nle:update model (nlt:make-key-press-msg :code :right))
    (nle:update model (nlt:make-key-press-msg :code :backspace))
    ;; The composer is idle: Left leaves.
    (nle:update model (nlt:make-key-press-msg :code :left))
    (is (find "/api/gateway/sessions" calls :key #'second :test #'equal))
    (is (equal "s-fresh" model.session-id))
    (is-present (dialog (nle::chat-model-dialog model)) "with /sessions open"
      (is-shape dialog (nle::picker-dialog-kind eq :sessions)
        (picker-selected-value "s-work" "the highlight on the session it left")))))

(deftest left-with-the-sessions-dialog-open-folds-instead-of-leaving ()
  ;; The fold and the leave gesture share one key and never meet: with the
  ;; picker up the modal owns Left -- the fold moves and nothing is posted --
  ;; and with it down Left leaves the session, as it always has.
  (with-leaving-shell (model calls "s-here"
                       :listing (vector (listing-row "s-here" "here" "2026-01-01T00:00:00.000Z")
                                        (listing-row "s-kid" "kid" "2026-01-02T00:00:00.000Z" "s-here")))
    (nle::slash-sessions-op model)
    (let ((dialog model.dialog))
      (is (equal '("here") (session-labels dialog)))
      (nle::picker-dialog-key model :right)
      (is (equal '("here" "╰ kid") (session-labels dialog)))
      (let ((before (length calls)))
        (nle:update model (nlt:make-key-press-msg :code :left))
        (is (= before (length calls)) "Left with the picker up posts nothing")
        (is (equal "s-here" (nle::chat-model-session-id model)))
        (is (eq dialog model.dialog) "the picker stays up")
        (is (equal '("here") (session-labels dialog))))
      (nle::picker-dialog-key model :escape)
      (nle:update model (nlt:make-key-press-msg :code :left))
      (is (equal "s-fresh" (nle::chat-model-session-id model)) "with the picker down, Left leaves")
      (is (eq :sessions (nle::picker-dialog-kind model.dialog)) "onto /sessions"))))
