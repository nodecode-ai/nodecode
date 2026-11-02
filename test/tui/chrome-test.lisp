;;;; chrome-test.lisp --- U5 gates: header ctx/token lane + footer right lane.
;;;;
;;;; Claims:
;;;;   1. Token counts format compactly (Zig appendTokenCount).
;;;;   2. A recorded turn.usage fact folds into state and the header's right
;;;;      lane renders tenth-precision occupancy + token arrows from it,
;;;;      carrying the same cache/reasoning notes the live bar carries.
;;;;   3. The gateway reducer folds live/replayed turn.usage frames the same
;;;;      way (hyphenated durable payload keys).
;;;;   4. A narrow window drops the volatile right lane, never the identity.
;;;;   5. The footer right lane names the gateway endpoint for clients and
;;;;      stays empty for the local shell.
;;;;
;;;; Loads after ui-test: reuses MAKE-FRAME-MODEL.

(in-package #:nodecode.test)

(nlk:access (client nle::gateway-chat-model) (model nle::gateway-chat-model))

(defmacro with-pinned-window ((model-id tokens) &body body)
  "Run BODY with MODEL-ID's context window pinned to TOKENS through the
capability seam."
  ;; An unknown model's window is NIL (no flat default since 76f8f804d), and
  ;; the header lanes under test assert occupancy against a window, so the
  ;; gates declare the one they mean.
  `(let ((nle::*model-capability-overrides*
           '((,model-id . (:context-tokens ,tokens)))))
     ,@body))

(defun fold-usage (model &rest usage)
  "Fold one provider round's USAGE, INGEST-TURN-USAGE's keys, into MODEL."
  (setf model.state (apply #'nle::ingest-turn-usage model.state usage)))

(defun needle-cell (needle rows &aux (index (row-index-of needle rows :required t)))
  "The 1-based cell X, Y where NEEDLE first paints among ROWS."
  (values (1+ (search needle (elt rows index))) (1+ index)))

(defun drag-to (model x y x2 y2 &key (button :left) moves)
  "BUTTON pressed on cell X,Y and released on X2,Y2, the pointer carried there first when MOVES."
  (mouse-update model #'nlt:make-mouse-click-msg x y :button button)
  (when moves (mouse-update model #'nlt:make-mouse-motion-msg x2 y2 :button button))
  (mouse-update model #'nlt:make-mouse-release-msg x2 y2 :button button))

(defmacro with-queued-prompts ((model session-id &rest prompts) &body body)
  "Run BODY with MODEL a 100x24 shell over SESSION-ID whose turn is in flight
and PROMPTS queued behind it, each INGEST-PENDING-INPUT-FACT's arguments."
  `(with-frame-model (,model 100 24 :session-id ,session-id)
     (mark-turn-in-flight ,model)
     (fold-model ,model ,@(loop for prompt in prompts
                                collect `(nle::ingest-pending-input-fact ,@prompt)))
     ,@body))

;;; --- claim 1: count formatting ---------------------------------------------

(deftest token-counts-format-compactly (is-each (nle::format-token-count))
  (999 "999" "below 1000 stays plain")
  (1000 "1k" "1000 is 1k")
  (1234 "1.2k" "tenth precision")
  (12345 "12.3k" "larger tenth")
  (199999 "200k" "rounds to whole k")
  (999949 "999.9k" "the k tier runs to the last value that does not round to 1000k")
  (999950 "1M" "what would round to 1000k is 1M")
  (1234567 "1.2M" "tenth-precision M")
  (nil "0" "absent renders as zero"))

;;; --- claim 2: durable fold + header lane ------------------------------------

(deftest usage-fact-feeds-the-header-lane (with-pinned-window ("chrome-model" 16000))
  (with-session-store ("chrome-usage")
    (with-durable-turn (turn "chrome-usage" :command-id "chrome-cmd" :input "hello" :complete t)
      (nlk:record-turn-usage turn :model "chrome-model"
                                  :input-tokens 7000
                                  :cached-input-tokens 1000
                                  :output-tokens 512))
    (let* ((model (make-frame-model 100 24 :session-id "chrome-usage" :tool-detail :developer))
           (status (nle::header-status-text model)))
      (is-carrying status ("↑ 7k" "the input arrow carries the folded count")
        ("↓ 512" "the output arrow carries the folded count") "50.0%/16k")
      (let ((header (second (nle::compose-frame model))))
        (is (search "↑ 7k" header) "the lane renders in the header row")
        ;; The derived session title (the first prompt) keeps column 3 —
        ;; the raw id yields once the session has a name.
        (is (eql 3 (search "hello" header)))))))

(deftest header-lane-carries-the-sessions-global-bill (with-pinned-window ("chrome-model" 16000))
  ;; The header's right lane is the standing readout: it answers "what has
  ;; this session cost and how full is its context", so a new turn must not
  ;; shrink it back to one turn's numbers.
  (with-frame-model (model 100 24 :session-id "header-global" :tool-detail :developer)
    ;; One turn, two provider rounds.
    (fold-usage model :input 4000 :output 100 :cached 1000 :model "chrome-model")
    (fold-usage model :input 5000 :output 200 :cached 2000 :model "chrome-model")
    ;; The next turn begins: the turn meter retires, this lane does not.
    (fold-model model (nle::begin-turn-usage))
    (is-carrying (status (nle::header-status-text model))
      ("↑ 9k" "the input arrow sums every round of the session")
      ("↓ 300" "so does the output arrow")
      ("43.8%/16k" "occupancy is the newest round's prompt, never the summed prompts")
      (is (search "↑ 9k" (second (nle::compose-frame model)))))))

(deftests header-lane (width usage status header)
  ;; Each case is a window width, one provider round's usage against
  ;; chrome-model's pinned 16k window, the lane's exact text, and what of it
  ;; the header row must carry (NIL: the case asserts the text alone).
  (with-pinned-window ("chrome-model" 16000)
    (with-frame-model (model width 24 :session-id "header-lane" :tool-detail :developer)
      (apply #'fold-usage model usage)
      (is (equal status (nle::header-status-text model)))
      (when header (is (search header (second (nle::compose-frame model)))))))
  ;; The header answers, at session scope, the two questions the live
  ;; transcript bar and the finish divider answer per turn: how much of the
  ;; prompt bill was served from cache, and how much of the completion bill
  ;; was reasoning. Both facts already fold into SESSION_USAGE — the lane
  ;; simply dropped them on the floor.
  (reciprocates-the-live-bars-usage-notes
   120 '(:input 553 :output 2100 :cached 3840 :cache-write 512 :reasoning 1900
         :model "chrome-model")
   "30.7%/16k · ↑ 553 (cache 87.4%) · ↓ 2.1k (reasoning 1.9k)" "(cache 87.4%)")
  ;; A note is a provider fact, never a default: `(cache 0%)` on a lane no
  ;; round ever reported cache reads for would claim evidence that does not
  ;; exist.
  (omits-notes-no-round-reported
   100 '(:input 1500 :output 176 :model "chrome-model") "9.4%/16k · ↑ 1.5k · ↓ 176" nil)
  ;; No config entry and no catalog row means the capability seam answers
  ;; NIL for the window, and a percent of nothing says nothing. The lane
  ;; still has a fact to show: how large the context has grown — labelled,
  ;; because a bare count beside the arrows is just a third number.
  (shows-the-bare-prompt-size-unknown-window
   120 '(:input 80000 :output 176 :cached 7600 :model "uncatalogued-model")
   "ctx 87.6k · ↑ 80k (cache 8.7%) · ↓ 176" "ctx 87.6k"))

(deftest header-lane-warms-toward-the-eviction-line (with-pinned-window ("chrome-model" 16000))
  ;; The lane is the window's one standing meter: from three quarters it
  ;; names the line eviction starts at, past the line it says what the next
  ;; request does, and the occupancy warms with it. Nothing else is drawn.
  (with-frame-model (model 120 24 :session-id "header-warm")
    (loop for (input text tones) in '((6000 "37.5%/16k" (:muted))
                                      (12800 "80.0%/16k · evicts at 13.6k" (:warn :warn))
                                      (14400 "90.0%/16k · evicts next request" (:danger :danger)))
          do (fold-usage model :input input :output 10 :model "chrome-model")
             (is (equal text (nle::header-status-text model)))
             (is (equal tones (mapcar #'cdr (nle::header-status-parts model)))))
    (is (search "evicts next request" (second (nle::compose-frame model))) "the header row paints it")))

(deftest header-lane-keeps-the-percent-under-a-pinned-window
    (with-pinned-window ("uncatalogued-model" 200000))
  ;; The same fact against a pinned window stays a percent — the bare form
  ;; is the unknown-window arm only.
  (with-frame-model (model 120 24 :session-id "header-known-window")
    (fold-usage model :input 80000 :output 176 :cached 7600 :model "uncatalogued-model")
    (is (search "43.8%/200k" (nle::header-status-text model)))))

(deftest header-occupancy-keeps-tenth-precision ()
  ;; Whole-percent occupancy floors every prompt below 2k against a 200k
  ;; window to a flat `0%` — the whole early session reads as empty.
  (is-each (nle::format-occupancy-percent)
    (1500 200000 "0.8%" "a 1.5k prompt in a 200k window is 0.8%, not 0%")
    (8000 16000 "50.0%" "an exact half keeps its tenth")
    (7000 16000 "43.8%" "the tenth rounds half-up")
    (200000 200000 "100.0%" "a full window is 100.0%")
    (2000000 16000 "999.9%" "a mis-sized window cannot widen the lane past 999.9%")))

;;; --- claim 3: gateway reducer fold ------------------------------------------

(deftest gateway-reducer-folds-the-streams-own-metadata ()
  ;; The attached TUI is a fold of these wire keys and nothing else: a fact
  ;; the durable payload carries but this reducer skips can never reach the
  ;; divider, which is how the served model, the finish reason, the cache
  ;; writes and the reasoning tokens all stayed invisible.
  (let ((event (nlk:json-object "type" "turn.usage"
                                "input-tokens" 553
                                "output-tokens" 2100
                                "cached-input-tokens" 3840
                                "reasoning-tokens" 1900
                                "cache-write-tokens" 512
                                "model" "gpt-5.6-sol"
                                "response-model" "gpt-5.6-sol-2026-08-01"
                                "finish-reason" "length"
                                "reasoning-effort" "medium")))
    (let ((usage (gethash "usage" (nle::apply-gateway-event nil event))))
      (is-shape usage (:input = 553 "input folds from the wire key")
        (:output = 2100 "output folds from the wire key")
        (:cached = 3840 "cached input folds from the wire key")
        (:model "gpt-5.6-sol" "the model rides along") (:reasoning = 1900 "reasoning tokens fold")
        (:cache-write = 512 "cache writes fold") (:response-model "gpt-5.6-sol-2026-08-01")
        (:finish-reason "length" "the finish reason folds"))
      (is (equal "• gpt-5.6-sol→gpt-5.6-sol-2026-08-01 (medium) · completed (length)"
                 (nle::turn-finish-divider-text "completed" :usage usage))))))

(deftest gateway-turn-start-retires-the-turn-meter-not-the-session-lane ()
  ;; The attached TUI learns a turn boundary from the turn.started frame.
  ;; That boundary scopes exactly one of the three slots.
  (let ((usage (nlk:json-object "type" "turn.usage"
                                "input-tokens" 4000
                                "output-tokens" 120
                                "model" "wire-model"))
        (started (nlk:json-object "type" "turn.started"))
        (state nil))
    (feed-events state usage usage)
    (is (= 8000 (getf (gethash "usage" state) :input)))
    (feed-events state started)
    (is (null (gethash "usage" state)))
    (is (= 8000 (getf (gethash "session_usage" state) :input)))
    (is (= 4000 (nle::context-occupancy-tokens (gethash "context_usage" state))))))

;;; --- claim 4: narrow windows drop the lane ----------------------------------

(deftest narrow-header-drops-the-volatile-lane (with-frame-model (model 30 24 :session-id "narrow"))
  ;; The identity lane is chat chrome: a shell with no committed prompt
  ;; is the landing screen, which carries none.
  (setf (nle::chat-model-tool-detail model) :developer)
  (fold-model model (nle::ingest-message-fact "user" "n"))
  (fold-usage model :input 7000 :output 512 :cached 1000 :model "chrome-model")
  (is-carrying (header (second (nle::compose-frame model)))
    (is (eql 3 (search "n · narrow" header)) "the identity survives")
    (:absent "↑" "the volatile lane drops instead of squeezing the identity")))

;;; --- claim 5: footer right lane ---------------------------------------------

(deftest footer-right-lane-names-the-gateway ()
  (with-frame-model (local 80 24 :session-id "footer-local")
    (is-carrying (text (nle::segs-text (first (nle::footer-right-candidates local))))
      (is (search (nle::workspace-footer-label) text)) (:absent "cells "))

    (is (not (search "Gateway" (nth 22 (nle::compose-frame local))))))
  (with-frame-model (client 80 24 :session-id "footer-client" :http-port 4321)
    (is-carrying (text (nle::segs-text (first (nle::footer-right-candidates client))))
      "Gateway" (:absent " • connecting"))
    (setf client.sync-lifecycle :connected)
    (is-carrying (text (nle::segs-text (first (nle::footer-right-candidates client))))
      "Gateway" (:absent " • connected") (:absent "cells "))
    (is (search "Gateway"
                (nth 22 (nle::compose-frame client))))))

(deftest every-home-names-itself-in-the-chrome ()
  ;; Identity by absence reads as unknown, not as default. Both renderers
  ;; go through PROFILE-LABEL, which answers for every home.
  (with-frame-model (model 80 24 :session-id "footer-identity")
    (flet ((profile-row ()
             (assoc "/profile" (nle::home-meta-rows 80) :test #'string=))
           (right ()
             (nle::segs-text (first (nle::footer-right-candidates model)))))
      (with-saved-globals ((nodecode.kernel::*home* (nlk:default-home)))
        (is (equal "default" (nlk:profile-label)) "the default home has a name")
        (is (eql 0 (search "default" (right))) "the footer lane leads with it")
        (is (search (nle::workspace-footer-label) (right)) "and still carries the workspace")

        (setf nodecode.kernel::*home* (nlk:profile-home "coder"))
        (is (equal "coder" (nlk:profile-label)) "a named profile is its folder")
        (is (search "coder" (second (profile-row))) "named on the landing screen")
        (is (eql 0 (search "coder" (right))) "and leading the footer lane")
        (is (uiop:string-suffix-p (second (profile-row)) "/coder") "the clipped path keeps the home's own folder")))))

(deftest the-window-wears-the-profile-and-the-workspace ()
  ;; The one identity surface an operator reads without looking inside the
  ;; frame: the title is the footer's right lane, for the window manager.
  (with-frame-model (model 80 24 :session-id "window-title")
    (with-saved-globals ((nodecode.kernel::*home* (nlk:default-home)))
      (let ((title (nle::shell-window-title)))
        (is (eql 0 (search "default" title)) "the profile leads the title")
        (is (search (nle::workspace-footer-label) title) "the workspace follows it")
        (is (notany (lambda (character) (< (char-code character) 32)) title))
        (is (equal title (nlt:view-state-title (nle::shell-view model)))))
      (setf nodecode.kernel::*home* (nlk:profile-home "coder"))
      (is (eql 0 (search "coder" (nle::shell-window-title)))))))


;;; --- input/chrome ladder: live meter, ctx pressure, transient hint ---------

(deftest live-status-carries-meter-and-the-stop-key ()
  (with-frame-model (model 80 24 :session-id "live-meter" :tool-detail :developer)
    (land-model-facts model :provider nle::*provider* :model nle::*model*)
    (mark-turn-in-flight model)
    (fold-usage model :input 7000 :output 512 :cached 1000 :model "meter-model")
    (let ((status (nle::live-status-line model 73)))
      (is-carrying status ("↑7k" "the live input arrow meters the round")
        ("↓512" "the live output arrow meters the round") ("c12.5%" "the cache mark rides along")
        (:absent "ctx"))
      ;; The stop key comes whole or not at all: a narrow bar keeps its own
      ;; words rather than spilling past the edge into a wrap.
      (is (<= (nle::text-display-width status) 73)))
    (is (search " esc  stop" (nle::live-status-line model 130)) "esc stands on the bar it stops")
    (is (not (search "esc" (nle::live-status-hint model))) "and the hover tooltip no longer repeats it")))

(deftest live-meter-follows-the-viewport-without-changing-usage ()
  (with-frame-model (model 180 26 :session-id "viewport-meter")
    (land-model-facts model :provider "p" :model "meter-model")
    (mark-turn-in-flight model)
    (fold-usage model :input 183200 :output 24200 :cached 2970000
                     :reasoning 11300 :estimated t :model "meter-model")
    (let* ((usage (gethash "usage" model.state))
           (meter (nle::usage-meter-text usage)))
      (is (eq :standard (nle::chat-model-tool-detail model)))
      (flet ((check-mode (visible)
               (dolist (width '(80 120 180))
                 (nle:update model (nlt:make-window-size-msg :width width :height 26))
                 (let ((status (nle::live-status-line model width)))
                   (is (eql visible (not (null (search "↑~183.2k" status)))))
                   (is (eql visible (not (null (search "↓~24.2k" status))))))
                 (is (eql visible (rows-carry-p "↑~183.2k" (nle::compose-frame model)))))))
        (check-mode nil)
        (is (null (nle::live-usage-meter-segs model)) "Standard contributes no meter segments below the pressure threshold")
        (press model #\o :mod nlt:+mod-ctrl+)
        (is (eq :developer (nle::chat-model-tool-detail model)))
        (check-mode t)
        (is (equal meter (nle::segs-text (nle::live-usage-meter-segs model))) "Developer retains the full cache, output and reasoning meter")
        (press model #\o :mod nlt:+mod-ctrl+)
        (check-mode nil)
        (is (eq usage (gethash "usage" model.state)) "switching viewports does not mutate the retained usage")
        (is (equal meter (nle::usage-meter-text usage)) "the shared formatter used by /context is unchanged")
        (is (search "↑ 183.2k" (nle::header-status-text model :detail :developer)) "the session header still carries its bill")))))

(deftest context-pressure-remains-visible-in-both-viewports ()
  (with-pinned-window ("pressure-model" 16000)
    (with-frame-model (model 180 26 :session-id "viewport-pressure" :in-flight t)
      (dolist (mode '(:standard :developer))
        (setf (nle::chat-model-tool-detail model) mode)
        (is (null (nle::live-usage-meter-segs model)) "absent usage remains absent")
        (fold-usage model :input 14000 :output 10 :cached 1000 :model "pressure-model")
        (let ((ctx (find "ctx" (nle::live-usage-meter-segs model) :key #'nle::seg-text :test #'search)))
          (is ctx)
          (is (equal "ctx 93%" (nle::seg-text ctx)))
          (is (eq :danger (nle::seg-tone ctx))))
        (setf model.state (nle::begin-turn-usage model.state))))))

(deftest live-status-says-stopping-from-the-esc-that-asked ()
  "Esc on a running turn: the bar reads `stopping…' in the gesture's own frame
and offers Esc no more, until the turn's terminal fact settles the flight.
Without this, the gateway's answer would be the first thing on screen."
  (with-frame-model (model 80 24 :session-id "live-stop")
    (land-model-facts model :provider nle::*provider* :model nle::*model*)
    (setf (nle::gateway-model-active-turn-id (mark-turn-in-flight model)) "turn-live-stop")
    (with-stubbed-fdefinitions ((nle::send-cancel-turn (ws session-id turn-id &key retract) nil))
      (nlt:update model (nlt:make-key-press-msg :code :escape)))
    (let ((bar (nle::live-status-line model 130)))
      (is (search "stopping…" bar) "the bar says so at once")
      (is (not (search " esc " bar)) "and offers Esc no more"))
    (nle::note-turn-flight-settled model)
    (setf (nle::gateway-model-active-turn-id (mark-turn-in-flight model)) "turn-next")
    (is (search " esc  stop" (nle::live-status-line model 130)) "the next turn offers Esc again")))

(deftest a-previewed-turn-wears-the-bar-attaching-would-paint ()
  "The bar under a dialog previewing another session is that session's turn
as attaching to it paints it, character for character: verb, clock, quiet,
meter, resting shape, and no name at its head. Esc's cap stands only where
Esc stops the turn, so no lane wears it under a dialog. Without this, a
preview would draw a bar of its own that leads with the session's name
and loses the rest."
  (with-frame-model (model 120 24 :session-id "bar-attached" :tool-detail :developer)
    (let ((shown (nle::ingest-turn-usage (nle::initial-chat-state)
                                         :input 7000 :output 512 :model "meter-model"))
          (activity (list "thinking" :thinking
                          (- (get-internal-real-time)
                             (floor (* 40050 internal-time-units-per-second) 1000)))))
      ;; One turn eight minutes in, its stream quiet forty seconds.
      (setf (gethash "turn_id" shown) "t-shown"
            (gethash "turn_started_at_ms" shown) (- (nle::iso-8601-ms (nlk::iso-now)) 503500))
      (flet ((bar () (nle::live-status-line model 120))
             (attach (state)
               (setf model.state state
                     (nle::chat-model-turn-in-flight model) (and (gethash "turn_id" state) t)
                     (nle::chat-model-turn-activity model) activity)
               (nle::anchor-turn-clock model)))
        (attach shown)
        (is-carrying (text (bar)) ("waiting on the model · 8m 23s · quiet 40.0s" "the attached turn")
          ("↑7k" "its meter") (" esc  stop" "Esc stops it"))
        (setf (nle::chat-model-dialog model) (nle::make-picker-dialog :title "Sessions"))
        (let ((under (bar)))
          (is (not (search "esc" under)) "under a dialog Esc closes the dialog")
          ;; The same turn previewed from a shell attached elsewhere.
          (attach (nle::initial-chat-state))
          (setf (nle::chat-model-transcript-preview model) (cons "s-shown" shown)
                (nle::gateway-model-preview-activity model) activity)
          (is (equal under (bar)) "the previewed bar is the attached one")
          (is (not (search "s-shown" (bar))) "and wears no name"))
        ;; It fails: the previewed session rests as it would attached.
        (remhash "turn_id" shown)
        (setf (gethash "turn_resting_status" shown) '(:outcome "failed" :elapsed-ms 1234))
        (let ((previewed (bar)))
          (is-carrying previewed ("1.2s failed" "the resting shape"))
          (setf (nle::chat-model-transcript-preview model) nil)
          (attach shown)
          (is (equal previewed (bar)) "as the attached session rests"))))))

(deftest live-status-says-what-the-tool-call-is-doing ()
  ;; The bar read `running eval' for every call, which is
  ;; every call. It reads the snippet's arguments as they stream instead, and
  ;; retains the turn tally as state, not transcript copy — never a count per
  ;; family (`34 reads · 26 commands · 4 listings · …', which the operator
  ;; turned down).
  (with-frame-model (model 120 24 :session-id "live-verb" :in-flight t)
    (fold-model model
      (nle::ingest-message-fact "user" "look around")
      (nle::ingest-tool-call-fact "c1" "eval" "{\"form\":\"(sh \\\"cat a.lisp\\\")\"}")
      (nle::ingest-tool-result-fact "c1" "eval" "(a)")
      (nle::ingest-tool-call-fact "c2" "eval" "{\"form\":\"(sh \\\"rg -n foo src\\\")\"}")
      (nle::ingest-tool-result-fact "c2" "eval" "src/x:1:foo"))
    ;; The streamed fragment folds the way the shell folds its item_delta:
    ;; the row, then the verdict's verb on the bar.
    (multiple-value-bind (state verdict)
        (nle::apply-gateway-event
         model.state (nlk:make-json-object
                      "type" "item_delta"
                      "delta" (nlk:make-json-object
                               "type" "tool_call" "index" 0 "name" "eval"
                               "arguments" "{\"form\":\"(sh \\\"sed -n '1,40p' src/tui/ui.lisp")))
      (setf model.state state)
      (nle::note-turn-activity model (getf verdict :activity) (getf verdict :kind)))
    ;; The bar names the tool, beside the mark of what the call does.
    (is (equal '("evaluating" :read) (subseq model.turn-activity 0 2)))
    (nle::render-state model)
    (is-carrying (status (nle::live-status-line model 120))
      ("evaluating" "the tool's own verb")
      (:absent "reading src/tui/ui.lisp" "the call's own line says what it does, not the bar")
      (:absent "actions" "the tally belongs to state, not the transcript bar")
      (:absent "reads" "one total, not a count per family"))))

(deftest live-status-never-repeats-a-waiting-calls-note ()
  ;; `(progn (sleep 60) (sh ...))' under `;; checking the
  ;; canonical lane's verdict so far' is a wait by its words, and the bar
  ;; took that wait for the model's: the note stood on the bar under the
  ;; call's own line saying it, in and out as its fragments streamed.
  (with-frame-model (model 120 24 :session-id "live-wait-note" :in-flight t)
    (let ((arguments (eval-args (format nil ";; checking the canonical lane's verdict so far~%~
                                          (progn (sleep 60) (sh \"tail -2 lane.log\"))"))))
      (flet ((bar-after (event)
               (multiple-value-bind (state verdict) (nle::apply-gateway-event model.state event)
                 (setf model.state state)
                 (nle::note-turn-activity model (getf verdict :activity) (getf verdict :kind)))
               (nle::render-state model)
               (nle::live-status-line model 120)))
        (loop for end from 12 below (length arguments) by 5
              for bar = (bar-after (nlk:make-json-object
                                    "type" "item_delta"
                                    "delta" (nlk:make-json-object
                                             "type" "tool_call" "index" 0 "name" "eval"
                                             "arguments" (subseq arguments 0 end))))
              do (is-carrying bar
                   ("evaluating" "every fragment names the tool")
                   (:absent "checking the" "and never the note")))
        (is-carrying (status (bar-after (nlk:make-json-object
                                         "type" "turn.tool_call_started" "turn_id" "turn-1"
                                         "call-id" "c1" "tool-name" "eval"
                                         "arguments" arguments)))
          ("evaluating" "the whole call names the tool")
          (:absent "checking the" "its note is its line's"))
        ;; The call's words are a wait, so its mark sways.
        (is (eq :wait (second model.turn-activity)))))))

(deftest live-status-mark-is-the-verbs-own ()
  ;; Every verb the bar names wears its kind's mark (*STATUS-MARKS*): a quiet
  ;; stream sways under `waiting', and a tool working in silence keeps its
  ;; own mark beside its own verb.
  (with-frame-model (model 120 24 :session-id "live-mark")
    ;; The turn began three seconds ago, so a verb stamped at its start is
    ;; past the two-second threshold.
    (let ((began (- (get-internal-real-time) (* 3 internal-time-units-per-second))))
      (mark-turn-in-flight model)
      (setf model.turn-started-at began)
      (with-stubbed-fdefinitions ((nle::status-mark (elapsed-ms &optional (kind :default))
                                    (format nil "<~(~a~)>" kind)))
        (nle::note-turn-activity model "thinking" :thinking)
        (is-carrying (status (nle::live-status-line model 120))
          ("<thinking> " "a fresh verb wears its own mark"))
        (nle::note-turn-activity model "thinking" :thinking began)
        (is-carrying (status (nle::live-status-line model 120))
          ("<wait> " "a quiet stream sways") "waiting" (:absent "thinking"))
        (fold-model model
          (nle::ingest-tool-call-fact "c1" "eval" "{\"form\":\"(sh \\\"just lint\\\")\"}"))
        (nle::note-turn-activity model "evaluating" :command began)
        (is-carrying (status (nle::live-status-line model 120))
          ("<command> " "a silent tool keeps its own mark")
          ("evaluating" "beside its own verb"))))))

(deftest live-status-keeps-lane-and-effort-out-of-the-transcript
    (with-frame-model (model 80 24 :session-id "live-identity"))
  ;; Turn identity remains canonical state; timestamps expose it on hover.
  (land-model-facts model :provider "a6api-oai" :model "gpt-5.6-sol" :effort "high")
  (mark-turn-in-flight model)
  (let ((bar (nle::live-status-line model 90)))
    (is-carrying bar (:absent "a6api-oai") (:absent "gpt-5.6-sol") (:absent "high"))))

(deftest context-pressure-escalates-with-occupancy (with-pinned-window ("pressure-model" 16000))
  ;; The window is pinned through the capability seam: an unknown model's
  ;; window is NIL (no flat default), and NIL window means no ctx clause.
  (with-frame-model (model 80 24 :session-id "ctx-pressure")
    (flet ((ctx-seg ()
             (find "ctx" (nle::live-usage-meter-segs model) :key #'nle::seg-text :test #'search)))
      (setf model.turn-in-flight t)
      ;; 12000/16000 = 75%: the warn threshold admits the clause, muted.
      (fold-usage model :input 11000 :output 10 :cached 1000 :model "pressure-model")
      (is-present (ctx (ctx-seg)) "75% occupancy earns the bar slot"
        (is (equal "ctx 75%" (nle::seg-text ctx)) "the copy names the percent")
        (is (eq :muted (nle::seg-tone ctx)) "warn renders muted"))
      ;; 15000/16000 = 93%: danger escalates the tone.
      (fold-usage model :input 14000 :output 10 :cached 1000 :model "pressure-model")
      (is-present (ctx (ctx-seg)) "danger keeps the clause"
        (is (eq :danger (nle::seg-tone ctx)) "danger escalates the tone"))
      ;; Below the threshold the clause stays footer chrome.
      (fold-usage model :input 2000 :output 10 :cached 0 :model "pressure-model")
      (is (not (ctx-seg)) "low occupancy never claims a bar slot"))))

(deftest queued-steer-lines-render-under-the-status-block ()
  ;; render_plan.zig collectTranscriptStatusLines: a queued follow-up prompt
  ;; is queue state rendered INSIDE the status block — after the live turn
  ;; bar (and the background band), never as a transcript row above it.
  (with-queued-prompts (model "steer-under-bar" ("queue 1" :prompt-id "p-1")
                                                 ("queue 2" :prompt-id "p-2"))
    (is (not (find :pending-input (nle:project-state model.state :width 80)
                   :key #'nle::display-block-kind)))
    (nle::render-state model)
    (let* ((rows (nle::compose-frame model))
           (first-steer (row-index-of "queue 1" rows :required t)))
      (is (< (row-index-of "waiting" rows :required t) first-steer))
      (is (= (1+ first-steer) (row-index-of "queue 2" rows :required t)))
      (is (eql 5 (search "⌎" (nth first-steer rows)))))
    ;; Promotion clears the row and the chrome with it.
    (paint-model model (nle::clear-pending-input "p-1" "queue 1"))
    (is (not (rows-carry-p "queue 1" (nle::compose-frame model))))))

(deftest esc-hint-sits-on-the-queued-prompt-that-dispatches-next ()
  ;; The band tells the operator what Esc does while input is parked: it
  ;; cuts the running turn and the gateway promotes the parked prompt onto
  ;; it at once. The hint sits on exactly the row DEQUEUE-NEXT-TURN would
  ;; promote — the oldest parked steer ahead of every follow-up queued
  ;; before it, else the oldest prompt — adds no row, and shows only while
  ;; a turn runs. The peel's hint closes the block: Up takes a follow-up
  ;; back as it does a steer.
  (with-queued-prompts (model "esc-hint" ("first follow-up" :prompt-id "p-1")
                                          ("second follow-up" :prompt-id "p-2"))
    (let ((plains (nth-value 1 (nle::pending-steer-lines model 100))))
      (is (= 3 (length plains)) "one row per queued prompt, the Esc hint adds none")
      (is (search "⌎ first follow-up  — esc dispatches now" (first plains)))
      (is (not (search "esc dispatches" (second plains))))
      (is (search "up edits the last queued prompt" (third plains)) "the peel's line under them"))
    ;; A steer parked later outranks both follow-ups: the hint moves to it.
    (fold-model model
      (nle::ingest-pending-input-fact "steer" :prompt-id "p-3" :steer t))
    (let ((plains (nth-value 1 (nle::pending-steer-lines model 100))))
      (is (search "⌎ steer  — esc dispatches now" (third plains)))
      (is (not (search "esc dispatches" (first plains)))))
    ;; The snapshot re-naming the steer keeps the mark.
    (fold-model model
      (nle::ingest-pending-input-fact "steer" :prompt-id "p-3"))
    (is (nle::transcript-row-steer-p
         (find "p-3" (nle::pending-steer-rows model.state)
               :key #'nle::transcript-row-prompt-id :test #'equal)))
    ;; The hint is the frame's, painted muted behind the prompt.
    (nle::render-state model)
    (is (rows-carry-p "steer  — esc dispatches now" (nle::compose-frame model)))
    ;; Between the turn's end and the promotion nothing runs to cut.
    (setf model.turn-in-flight nil)
    (is (not (rows-carry-p "esc dispatches" (nth-value 1 (nle::pending-steer-lines model 100)))))))

(deftest queued-prompt-clears-the-spinner-lane ()
  ;; The live turn bar paints its two-cell spinner mark at columns 2-3, the
  ;; gutter, flush against the label lane, and its own text from 5, the label
  ;; lane, a run title's shape. Every row the status block paints BELOW that
  ;; bar must clear the mark's cells: a queued prompt's ⌎ hook hangs at 5,
  ;; under the bar's words, its body at 7 where a run's calls stand. The
  ;; operator sees a queued prompt as paint under the spinner otherwise.
  (with-queued-prompts (model "steer-spinner-lane"
                        ("queued while the turn runs" :prompt-id "p-lane"))
    (nle::render-state model)
    (let* ((rows (nle::compose-frame model))
           (bar (row-index-of "waiting" rows :required t))
           (steer (row-index-of "queued while the turn runs" rows :required t)))
      ;; The cells are the bar's own: the mark holds columns 2-3 there, so a
      ;; blank below it in those columns is a real answer, not a window that
      ;; never painted.
      (is (every (lambda (column) (char/= #\Space (char (nth bar rows) column))) '(2 3)) "the bar's spinner mark holds columns 2-3")
      (is (eql 5 (position-if-not (lambda (ch) (char= ch #\Space)) (nth bar rows) :start 4)) "the bar's words start on the label lane")
      (is (eql 5 (search "⌎" (nth steer rows))) "the queued hook hangs under the bar's words, clear of the mark")
      (is (eql 7 (search "queued while" (nth steer rows))) "and its body two in, where a run's calls stand")
      (loop for row in (subseq rows (1+ bar) (1+ steer))
            for index from (1+ bar)
            do (loop for column from 2 to 3
                     do (is (or (>= column (length row))
                                (char= #\Space (char row column)))
                            (format nil "row ~d column ~d is clear of the spinner"
                                    index column)))))))

(deftest pending-steer-lines-compensate-for-transcript-inset ()
  ;; The status painter runs inside the transcript window. Its two-column
  ;; outer inset brings the hook to 5, under the live line's words, and the
  ;; body to 7.
  (with-queued-prompts (model "steer-inset" ("queue" :prompt-id "p-inset"))
    (let ((plains (nth-value 1 (nle::pending-steer-lines model 100))))
      (is (eql 3 (search "⌎" (first plains))))
      (is (eql 5 (search "queue" (first plains)))))))

(deftest status-block-rests-without-a-terminal-divider ()
  ;; Completed-turn facts remain in projection, but the transcript no longer
  ;; paints the old divider or its padding. User/assistant history stays visible.
  (with-frame-model (model 100 24 :session-id "bar-rest-height"
                     :paint ((exchange "hello there" "hi" :turn-id "t1")
                             (nle::ingest-turn-usage :input 10 :output 2 :model "m" :provider "p")))
    (setf (gethash "turn_started_at_ms" model.state) 0)
    (mark-turn-in-flight model :render t)
    (is (row-index-of "waiting" (nle::compose-frame model)))
    (nle::note-turn-flight-settled model)
    (paint-model model
      (nle::ingest-turn-terminal "completed" :turn-id "t1" :ended-at-ms 1500))
    (let ((rows (nle::compose-frame model)))
      (is (not (row-index-of "completed" rows)))
      (is (row-index-of "hello there" rows))
      (is (row-index-of "hi" rows))
      (is-present (divider (find :finish-divider (nle::transcript-rows model.state)
                                :key #'nle::transcript-row-kind)) "terminal facts remain in projection"
        (is (equal "completed" (nle::transcript-row-outcome divider)))
        (is (null (nle::row-llines divider 100)))
        (is (null (nle::row-block-padding-p divider)))))))

(deftest the-resting-bar-names-the-failed-turn-and-its-clock ()
  ;; A failed turn persists no divider row (the transcriptStatusSummary rule),
  ;; so the live bar at rest is the only place its summary shows -- and it is
  ;; the resting shape: the elapse, the outcome word and the
  ;; wall clock the terminal fact landed at, with no meter the header's right
  ;; lane already carries.
  (with-frame-model (model 100 24 :session-id "bar-failed-rest")
    (land-model-facts model :provider "a6api-oai" :model "gpt-5.6-sol" :effort "high")
    (let ((ended (* 1000 (+ nlk:+unix-epoch+ (encode-universal-time 5 37 16 16 9 2026 0)))))
      (setf (gethash "turn_started_at_ms" model.state) (- ended 24800))
      (fold-model model
        (nle::ingest-message-fact "user" "hello there" :turn-id "t1")
        (nle::ingest-turn-terminal "failed" :turn-id "t1" :detail "provider refused"
                                   :ended-at-ms ended))
      (let ((bar (nle::live-status-line model 130)))
        ;; Elapse/outcome/wall clock remain; identity is available on timestamp hover.
        ;; The mark stands in the gutter and the words on the label lane,
        ;; both counted before the band's two-column inset.
        (is (search (format nil "24.8s failed (~a)"
                            (nle::format-clock-minute ended))
                    bar))
        (is (eql 1 (search "×" bar)) "the mark in the gutter")
        (is (eql 3 (search "24.8s" bar)) "the words on the label lane")
        (is (not (search "↑" bar)) "the failed turn's meter is not repeated")))))

(deftest a-quiet-stream-says-what-it-waits-on-and-for-how-long ()
  ;; Without this, `waiting' alone would come on two seconds into any pause, a
  ;; long think and a stall alike; the bar names the wait and how long a stream
  ;; that spoke has been quiet.
  (with-frame-model (model 100 24 :session-id "bar-quiet" :in-flight t)
    (let ((now (get-internal-real-time)) (second internal-time-units-per-second))
      (setf (nle::chat-model-turn-started-at model) (- now (* 125 second))
            (nle::chat-model-turn-activity model) (list "thinking" :thinking (- now (* 40 second))))
      (let ((bar (nle::live-status-line model 100)))
        (is (search "waiting on the model · 2m 05s · quiet 40.0s" bar))
        (is (eql 3 (search "waiting" bar)) "the verb starts on the label lane, the clock after it"))
      (setf (nle::chat-model-turn-activity model) (list "thinking" :thinking now))
      (let ((bar (nle::live-status-line model 100)))
        (is (search "thinking · 2m 05s" bar) "a stream that speaks names its own verb")
        (is (not (search "quiet" bar))))
      (setf (nle::chat-model-turn-activity model) nil)
      (let ((bar (nle::live-status-line model 100)))
        (is (search "waiting on the model" bar))
        (is (not (search "quiet" bar)) "a stream that never spoke has been quiet all along")))))

(deftest queued-steer-prompt-caps-at-five-lines ()
  ;; render_plan.zig max_pending_steer_prompt_lines: five prompt lines per
  ;; queued steer, the remainder as one counted `[N lines omitted]` clause.
  (with-queued-prompts (model "steer-cap" ((format nil "line 1~%line 2~%line 3~%line 4~%line 5~%~
line 6~%line 7")
                                           :prompt-id "p-cap"))
    (let ((plains (nth-value 1 (nle::pending-steer-lines model 90))))
      (is (= 7 (length plains)) "five prompt lines plus the clause, then the peel's line")
      (is (search "line 5" (nth 4 plains)) "the fifth line survives")
      (is (notany (lambda (p) (search "line 6" p)) plains))
      (is (search "[2 lines omitted]" (nth 5 plains)))
      ;; The rows are composed for the transcript band, whose two-column
      ;; inset brings the hook to column 5 and the body to 7 in the frame.
      (is (eql 3 (search "⌎" (nth 0 plains))))
      (is (eql 5 (search "line 2" (nth 1 plains)))))))

(deftest composer-status-owns-the-placeholder-and-news-expires ()
  (with-frame-model (model 80 24 :session-id "hint-slot")
    (nle::show-status model "Copied selection to clipboard")
    (is (equal "Copied selection to clipboard"
               (nle::status-said model)))
    (is (search "Copied selection" (nth 19 (nle::compose-frame model))))
    ;; Expiry: stamp the hint five-plus seconds into the past.
    (decf (nle::composer-status-born model.composer-status) (* 6 internal-time-units-per-second))
    (is (null (nle::status-said model)))
    (is (search "Connecting to Gateway" (nth 19 (nle::compose-frame model))))
    ;; Composer text outranks the hint.
    (nle::show-status model "noise")
    (type-string model "typing")
    (is (search "typing" (nth 19 (nle::compose-frame model))))))

(deftest composer-status-holds-a-failure-and-an-answer-and-wraps-them-whole ()
  ;; Everything the shell says about what the operator just did or asked
  ;; stands in the composer status: a failure, and
  ;; an answer that wraps, stand until the composer is edited, five seconds
  ;; never take them; a failure paints in the danger tone; the box grows to
  ;; show every line, and the home screen stays.
  (let ((nle::*color-profile* :truecolor))
    (with-frame-model (model 60 30 :session-id "status-held")
      (is (nle::home-screen-p model))
      (nle::show-status model "Gateway error: the link closed while the frame was being written, mid-sentence" :error-p t)
      (is-shape model.composer-status (nle::composer-status-held is) (nle::composer-status-error-p is))
      (decf (nle::composer-status-born model.composer-status) (* 60 internal-time-units-per-second))
      (is (nle::status-said model) "a failure outlives five seconds")
      (is (nle::home-screen-p model) "and keeps home")
      (let* ((rows (nle::compose-frame model))
             (failed (row-carrying "Gateway error" rows)))
        (is (rows-carry-p "mid-sentence" rows) "wrapped, never cut")
        (is (= 2 (nle::composer-visible-rows model)) "the box grows a row for it")
        (nle::show-status model "Gateway error: the link closed while the frame was being written, mid-sentence")
        ;; a failure paints in its own tone
        (is (not (equal failed (row-carrying "Gateway error" (nle::compose-frame model))))))
      (type-string model "x")
      (is (null (nle::status-said model)) "an edit takes it down")
      (nle::editor-reset model.editor)
      (nle::show-status model (format nil "provider: zeta~%model: z-large"))
      (is (nle::composer-status-held model.composer-status) "two lines are an answer, held")
      (is (= 2 (nle::composer-visible-rows model)))
      (nle::show-status model "Changed to Komorebi theme")
      (is (not (nle::composer-status-held model.composer-status)) "one line is news"))))

(deftest composer-status-stands-under-a-draft-when-held ()
  ;; A failure that hands the prompt back -- prompt not sent, refused --
  ;; leaves text in the composer: the held status stands under it, inside the
  ;; box, where the caret's row does not move; news hides under a draft.
  (with-frame-model (model 80 24 :session-id "status-under" :typed "my prompt")
    (nle::show-status model "Prompt not sent: this shell is not connected to its gateway" :error-p t)
    (let ((rows (nle::compose-frame model)))
      (is (rows-carry-p "my prompt" rows))
      (is (rows-carry-p "Prompt not sent" rows) "the failure shows under the draft")
      (is (< (row-index-of "my prompt" rows :required t) (row-index-of "Prompt not sent" rows :required t)))
      (is (= 2 (nle::composer-visible-rows model))))
    (nle::show-status model "Changed to Komorebi theme")
    (is (not (rows-carry-p "Komorebi" (nle::compose-frame model))) "news hides under a draft")
    (is (= 1 (nle::composer-visible-rows model)))))

(deftest composer-status-taller-than-the-composer-opens-as-a-list ()
  ;; An answer taller than the composer can grow -- /models list -- would
  ;; show its first rows and lose the rest: it opens as a list to search.
  (with-frame-model (model 80 24 :session-id "status-list")
    (nle::show-status model (format nil "~{~a~^~%~}" (loop for i below 40 collect (format nil "zeta/model-~d" i)))
                      :title "/models list")
    (is (null model.composer-status))
    (is-shape model.dialog (nle::picker-dialog-title "/models list"))
    (is (= 40 (length (nle::picker-dialog-rows model.dialog))))))

;;; --- home / logo screen ------------------------------------------------------
;;; The Zig landing surface (screens.zig + home.zig): predicate, geometry,
;;; exact copy, and the composer-placeholder swap.

(deftest home-screen-shows-logo-version-tip (with-frame-model (model 80 24 :session-id "home-gate"))
  (let* ((rows (nle::compose-frame model))
         (logo (row-index-of "▄  ▄  ▄▄" rows :required t)))
    (is (nle::home-screen-p model) "an empty idle shell is the home screen")
    ;; Offsets, not absolute rows: the fact block under the tip lifts the
    ;; whole landing screen (HOME-LOGO-START-ROW-LIFTS-FOR-THE-FACT-BLOCK
    ;; is where the anchor itself is proven), and the copy's geometry is
    ;; the spacing between these lines.
    (is-table (needle offset) (search needle (nth (+ logo offset) rows))
      ("█▀▄█" 1 "the second logo line follows") ("▀  ▀  ▀▀" 2 "the third logo line follows")
      ("Tip " 5 "the tip stands under the version and its blank"))
    (is (search (format nil "v~a" (subseq nle::*user-agent-version* 0
                                         (position #\+ nle::*user-agent-version*)))
                (nth (+ logo 3) rows)) "the version tag is directly below the logo")
    ;; A draft is typed on home, as on the launch's screen.
    (type-string model "x")
    (is (nle::home-screen-p model) "composer text keeps home")
    (nle::render-state model)
    (is (rows-carry-p "█▀▄█" (nle::compose-frame model)))
    (nle::editor-reset model.editor)
    ;; A status stands in the composer and keeps home.
    (nle::show-status model "Gateway error: x" :error-p t)
    (is (nle::home-screen-p model) "a status keeps home")
    (nle::show-status model nil)
    ;; A live turn dismisses home.
    (setf model.turn-in-flight t)
    (is (not (nle::home-screen-p model)) "a live turn dismisses home")
    (setf model.turn-in-flight nil)
    (is (nle::home-screen-p model) "home returns once the shell idles")))

(deftest home-screen-comes-back-glinting-when-a-dialog-closes (with-frame-model (model 80 24 :session-id "home-back"))
  ;; Only a compose read the predicate, and closing a dialog composes
  ;; nothing: without a compose after the dialog closes, the band under
  ;; it would be blank.
  (is (rows-carry-p "█▀▄█" (nle::compose-frame model)))
  (is (null (nle::chat-model-mark-glow model)) "a launch without a program plays no glint")
  (open-openai-connect model)
  (nle::render-state model)
  (nle::compose-frame model)
  (nle::dismiss-dialog model nil)
  (is (rows-carry-p "█▀▄█" (nle::compose-frame model)) "the dialog's close gives home back")
  (is (eq :shine (car (nle::chat-model-mark-glow model))) "glinting, as on launch"))

(deftest home-version-tag-is-centered-semver (dolist (version '("0.0.1+abcdef123" "2.3.4")))
  (let* ((nle::*user-agent-version* version)
         (tag (format nil "v~a" (subseq version 0 (position #\+ version)))))
    (dolist (width '(48 80 146))
      (with-frame-model (model width 24 :session-id "home-version")
        (let* ((rows (nth-value 1 (nle::home-band-lines model (nle::projection-width model))))
               (logo (row-index-of "▄  ▄  ▄▄" rows :required t))
               (row (nth (+ logo 3) rows))
               (pad (floor (- (nle::projection-width model) (length tag)) 2)))
          (is (equal row (concatenate 'string (make-string pad :initial-element #\Space) tag)) "a centered semantic tag, never commit metadata")
          (is (equal "" (nth (+ logo 4) rows)) "a blank separates the tag from the tip"))))))

(deftest home-screen-hides-on-cramped-windows ()
  (with-frame-model (narrow 47 24 :session-id "home-narrow")
    (is (not (rows-carry-p "█▀▄█" (nle::compose-frame narrow)))))
  (with-frame-model (short 80 11 :session-id "home-short")
    (is (not (rows-carry-p "█▀▄█" (nle::compose-frame short))))))

(deftest home-mark-shines-on-launch-and-on-click (with-frame-model (model 80 24 :session-id "home-shine"))
  ;; A launch starts the glint (NLT:INIT with a program); tests drive INIT
  ;; without one, so the glow is started by hand here.
  (flet ((quiet-tick-p () (nth-value 2 (nle:update model (nlt:make-tick-msg))))
         (lit (heat) (count-if (lambda (seg) (nle::visual-style-p (nle::seg-tone seg)))
                               (nle::mark-line-segs "▄  ▄  ▄▄▄" 0 heat))))
    (is (null (nle::chat-model-mark-glow model)) "no program, no launch, no glint")
    (is (= 1 (length (nle::mark-line-segs "▄  ▄  ▄▄▄" 0 nil))) "a still mark is one title run")
    (is (= 0 (lit (constantly 0))) "a cold cell paints the mark's own colour")
    (is (= 9 (lit (constantly 1))) "a hot one paints the ramp")
    (nle::render-state model)
    (is (quiet-tick-p) "an idle home tick is quiet")
    (nle::play-mark-glow model :shine)
    (is (not (quiet-tick-p)) "a crossing glint paints on the tick")
    (is (rows-carry-p "█▀▄█" (nle::compose-frame model)) "the mark's cells stay its own")
    (decf (cdr (nle::chat-model-mark-glow model)) (* 2 internal-time-units-per-second))
    (is (not (quiet-tick-p)) "the tick after it crossed paints the still mark")
    (is (eq :done (nle::chat-model-mark-glow model)) "and the glint is spent for this launch")
    (is (quiet-tick-p) "so the ticks after are quiet again")
    (multiple-value-call #'click-at model (needle-cell "█▀▄█" (nle::compose-frame model)))
    (is (eq :shine (car (nle::chat-model-mark-glow model))) "a click on the mark plays the glint again")
    (is (not (quiet-tick-p)) "and the tick carries it")))

(deftest home-mark-burns-on-five-clicks-and-the-konami-code (with-frame-model (model 80 24 :session-id "home-burn"))
  (multiple-value-bind (x y) (needle-cell "█▀▄█" (nle::compose-frame model))
    (dotimes (i 4) (click-at model x y))
    (is (eq :shine (car (nle::chat-model-mark-glow model))) "four quick clicks each shine")
    (click-at model x y)
    (is (eq :burn (car (nle::chat-model-mark-glow model))) "the fifth burns")
    (is (null (nle::chat-model-mark-clicks model)) "and the count starts again"))
  (let ((heat (nle::mark-glow-heat model)))
    (is (every (lambda (x) (<= 0 (funcall heat x 1) 1)) '(0 4 8)) "the burn's heat stays on the ramp"))
  (setf (nle::chat-model-mark-glow model) :done)
  (dolist (key '(:up :up :up :down :down :left :right :left :right #\b))
    (press model key))
  (is (equal "b" (nle::editor-value model.editor)) "the code's b types like any b")
  (press model #\a)
  (is (eq :burn (car (nle::chat-model-mark-glow model))) "the Konami code burns the mark")
  (is (nle::home-screen-p model) "and spends its b and a, so the home screen stands")
  (dolist (key '(:up :up :down :down :left :right :left :right #\b #\x))
    (press model key))
  (is (equal "bx" (nle::editor-value model.editor)) "a code that goes wrong types what it typed"))

(deftest home-screen-carries-no-session-identity ()
  ;; The Zig shell painted the header's session title only when NOT home
  ;; (shell_chrome.zig renderHeader wraps the title in `!isHomeScreen'). A
  ;; shell with no committed prompt has no title to carry, and
  ;; SESSION-DISPLAY-TITLE falls back to the raw durable id there, so the
  ;; landing screen printed `s-XXXX' above the logo and named nothing.
  (with-frame-model (model 80 24 :session-id "home-identity")
    (let ((rows (nle::compose-frame model)))
      (is (nle::home-screen-p model) "an empty idle shell is the home screen")
      (is (not (rows-carry-p "home-identity" rows))))
    ;; Chat keeps the title in both viewports, and the durable id only in Developer.
    (fold-model model (nle::ingest-message-fact "user" "ask"))
    (is-carrying (header (second (nle::compose-frame model)))
      (is (not (nle::home-screen-p model)) "a committed prompt leaves home")
      "ask" (:absent "home-identity" "Standard omits the durable id"))
    (press model #\o :mod nlt:+mod-ctrl+)
    (is (search "ask · home-identity" (second (nle::compose-frame model))))))

(deftest home-screen-keeps-the-header-status-lane ()
  ;; The home gate wraps the identity lane only: a usage fact is not a
  ;; session identity, and the Zig header painted its right lane on both
  ;; screens (`if (layout.status_visible)' outside the home gate).
  (with-frame-model (model 100 24 :session-id "home-status")
    (with-pinned-window ("chrome-model" 200000)
      (fold-usage model :input 7000 :output 512 :cached 1000 :model "chrome-model")
      (let ((header (second (nle::compose-frame model))))
        (is (nle::home-screen-p model))
        (is (search "4.0%/200k" header) "the global context lane still paints in Standard")
        (is (not (search "home-status" header)) "beside no session id")))))

(defun home-fake-cell (name &key (kind :peripheral) (state :loaded)
                                  (defined 0) skipped failures)
  "One CELL record shaped like a boot result."
  ;; The fact block reads the same records DESCRIBE-CELLS prints, so a
  ;; fixture is a record, never a string.
  (nlk::make-cell :name name :kind kind :state state :skipped skipped
                   :failures failures
                   :defined (make-list defined
                                       :initial-element '(:function . x))))

(deftest home-profile-row-is-labelled-by-its-command ()
  ;; `/profiles to browse or switch' trailed the identity; the command is the
  ;; label now, and the row says nothing else.
  (with-saved-globals ((nodecode.kernel::*home* (nlk:default-home)))
    (is (equal '("/profile" "default · ~/.nodecode" :chrome "profile")
               (assoc "/profile" (nle::home-meta-rows 80) :test #'equal)) "the command is the label and the identity the whole value")))

(deftest home-profiles-summary-keeps-long-identities-inside-the-value-lane ()
  (dolist (name '("research-model" "research-model-with-a-sixty-four-character-long-profile-name-here" "研究机"))
    (with-saved-globals ((nodecode.kernel::*home* (nlk:profile-home name)))
      (let ((value (second (assoc "/profile" (nle::home-meta-rows 80) :test #'equal))))
        (is (<= (nle::text-display-width value) (nle::home-meta-value-width 80)))
        (is (search " · " value) "the clipped identity still includes its path")))))

(deftest home-profiles-summary-paints-and-opens-the-existing-picker ()
  (with-saved-globals ((nodecode.kernel::*home* (nlk:default-home)))
    (dolist (home (list (nlk:default-home) (nlk:profile-home "coder")))
      (setf nodecode.kernel::*home* home)
      (dolist (size '((80 20) (80 24) (146 32) (300 32)))
        (with-frame-model (model (first size) (second size) :session-id "home-profile-cta")
          (nlk:bind ((frame (nle::compose-frame model)) ((x y) (needle-cell "/profile  " frame))
                     (row (nth (1- y) frame)))
            (is (search (nlk:profile-label) row) "the active identity is retained")
            (is (or (search " · ~/" row) (search " · /" row)) "the active home's path remains visible")
            (is (every (lambda (line) (<= (nle::text-display-width line) (first size))) frame))
            (click-at model x y)
            (is (eq :profiles (nle::picker-dialog-kind model.dialog)))
            (is (zerop (length (nle::editor-value (nle::chat-model-editor model)))) "the row opens a picker, never submits a turn")))))))

(deftest home-cells-summary-counts-folders-and-always-offers-the-board ()
  (let ((nlk::*cells* (list (home-fake-cell "nodecode-guard")
                           (home-fake-cell "nodecode-chrome" :state :failed)
                           (home-fake-cell "notes" :kind :knowledge)
                           (home-fake-cell "mylayer" :kind :layer))))
    (is (equal '("/cells" "3 installed" :chrome "cells")
               (assoc "/cells" (nle::home-meta-rows 80) :test #'equal)) "failed and knowledge folders count, the layer does not")))

(deftest home-cells-summary-paints-count-and-cta-on-one-row ()
  (let ((nlk::*cells* (loop for i from 1 to 20 collect (home-fake-cell (format nil "cell-~2,'0d" i)))))
    (dolist (size '((80 20) (80 24) (146 32)))
      (with-frame-model (model (first size) (second size) :session-id "home-summary")
        (let ((frame (nle::compose-frame model)))
          (is (row-carrying "/cells        20 installed" frame))
          (is (not (row-carrying "/cells to" frame)) "no tail spells out what the command does")
          (is (not (row-carrying "latest" frame)))
          (is (not (row-carrying "cell-01" frame)) "Home is a summary, /cells is the full catalog"))))))

(deftest home-generations-summary-counts-sealed-generations-and-always-offers-the-board ()
  (dolist (count '(0 1 142))
    (with-stubbed-fdefinition (nlk:manifest-count () count)
      (let ((rows (nle::home-meta-rows 80)))
        (is (equal (list "/generations" (format nil "~d sealed" count) :chrome "generations")
                   (assoc "/generations" rows :test #'equal)))
        (is (null (assoc "layer" rows :test #'equal)) "layer facts belong in /cells, not Home")))))

(deftest home-generations-summary-paints-count-and-cta-on-one-row ()
  (with-stubbed-fdefinition (nlk:manifest-count () 142)
    (dolist (size '((80 20) (80 24) (146 32)))
      (with-frame-model (model (first size) (second size) :session-id "home-generations")
        (let* ((width (nle::projection-width model))
               (plains (nth-value 1 (nle::home-band-lines model width)))
               (row (row-carrying "/generations  142 sealed" plains))
               (cells (row-carrying "/cells        " plains)))
          (is row "the command and the count paint on one row")
          (is cells "the cells row shares the same fact block")
          (when (and row cells)
            (is (= (search "/generations" row) (search "/cells" cells)) "both facts share the label lane"))
          (is (every (lambda (line) (<= (nle::text-display-width line) width)) plains)))))))

(deftest home-cell-diagnostics-stay-in-logs-not-the-landing-screen ()
  (let* ((attended (home-fake-cell "mylayer-attended" :state :failed
                                  :failures '((nil . "attended load condition"))))
         (background (home-fake-cell "mylayer-background" :state :failed
                                    :failures '((nil . "background load condition"))))
         (layer (home-fake-cell "mylayer" :kind :layer :defined 8
                               :skipped '(("old.lisp" . "skipped definition reason"))))
         (nlk::*cells* (list attended background layer)))
    (with-frame-model (model 100 24 :session-id "home-log-only")
      (let ((frame (nle::compose-frame model)))
        (is (row-carrying "/cells        2 installed" frame))
        (is (row-carrying "/generations  " frame))
        (is (not (row-carrying "mylayer" frame)) "the obsolete layer fact is absent")
        (dolist (needle '("failed" "skipped" "mylayer-attended" "mylayer-background" "old.lisp"))
          (is (not (row-carrying needle frame)) needle))))
    (with-stubbed-fdefinition (nlk::note-standing (&rest args))
      (let ((log (with-output-to-string (out) (nlk:describe-cells nlk::*cells* out))))
        (is (search "cell mylayer-attended: FAILED attended load condition" log))
        (is (search "cell mylayer-background: FAILED background load condition" log))
        (is (search "cell mylayer: SKIPPED old.lisp: skipped definition reason" log))))))

(deftest home-row-clicks-dispatch-existing-views-on-release ()
  (let ((nlk::*cells* (list (home-fake-cell "nodecode-guard")
                           (home-fake-cell "mylayer" :kind :layer :defined 3))))
    (dolist (case '(("/profile  " "profile") ("/cells  " "cells") ("/generations  " "generations")))
      (with-frame-model (model 100 24 :session-id "home-row-click")
        (nlk:bind (((x y) (needle-cell (first case) (nle::compose-frame model))) (calls nil))
          (with-stubbed-fdefinition (nle::execute-slash-command (shell name args)
                                      (is (eq shell model)) (is (equal "" args))
                                      (push name calls) nil)
            (nle:update model (nlt:make-mouse-click-msg :x x :y y :button :left))
            (is (null calls) "press only arms")
            (nle:update model (nlt:make-mouse-release-msg :x x :y y :button :left))
            (is (equal (list (second case)) calls))
            (is (zerop (length (nle::editor-value (nle::chat-model-editor model)))) "click never types or submits")))))))

(deftest home-row-clicks-open-profile-and-cells-pickers ()
  (dolist (case '(("/profile  " :profiles) ("/cells  " :cells)))
    (with-frame-model (model 100 24 :session-id "home-real-dialog")
      (multiple-value-call #'click-at model (needle-cell (first case) (nle::compose-frame model)))
      (is (eq (second case) (nle::picker-dialog-kind model.dialog))))))

(deftest home-row-drag-and-release-elsewhere-do-not-dispatch ()
  (with-frame-model (model 100 24 :session-id "home-drag")
    (nlk:bind (((x y) (needle-cell "/cells  " (nle::compose-frame model))) (calls nil))
      (with-stubbed-fdefinition (nle::execute-slash-command (&rest args)
                                  (push args calls))
        (drag-to model x y x (1+ y))
        (is (null calls) "a release on another row cancels the action")
        (drag-to model x y (+ x 6) y :moves t)
        (is (null calls) "drag selects, never activates")))))

(deftest home-row-targets-cover-values-and-hover-without-allowing-command-uris ()
  (dolist (profile '(:none :truecolor))
    (with-frame-model (model 100 24 :session-id "home-hover")
      (let ((nle::*color-profile* profile))
        (nle::render-state model)
        (nle::compose-frame model)
        (nlk:bind (((x y) (needle-cell "/cells  " (nle::frame-plain-rows model))))
          (nle:update model (nlt:make-mouse-motion-msg :x x :y y))
          (is (equal '(:home . "cells") (nle::chat-model-hover-target model)))
          (nle::compose-frame model)
          (multiple-value-bind (line column) (nle::frame-transcript-position model x y)
            (is (equal '(:home . "cells") (nle::home-command-at-position model line column))))))))
  (is (null (nle::openable-target-p "nodecode:/cells")))
  (is (null (nle::openable-target-p "/cells")) "model-authored links cannot run local commands"))

(deftest home-row-health-continuations-open-doctor ()
  (with-stubbed-fdefinition (nle::health-facts ()
                              (list (nle::health-fact :model "model" "模型 pin refused"
                                                      :tone :danger :remedy "choose a model in /setup after checking its provider configuration")))
    (with-frame-model (model 100 26 :session-id "home-health-click")
      (nlk:bind (((x y) (needle-cell "configuration" (nle::compose-frame model))) (calls nil))
        (with-stubbed-fdefinition (nle::execute-slash-command (shell name args)
                                    (push name calls) nil)
          (click-at model x y)
          (is (equal '("doctor") calls) "folded health detail rows retain their view"))))))

(deftest home-logo-start-row-lifts-for-the-fact-block ()
  ;; Six content rows plus five of chrome set the anchor. The fact block
  ;; lifts the landing screen instead of pushing it onto the composer.
  (is-each (nle::home-logo-start-row)
    (24 0 6 "with no fact block the logo and version are centred")
    (24 5 4 "five block rows lift the logo by two")
    (12 5 2 "the cramped arm is unmoved")))

(deftests home-screen (cells width carried absent why)
  ;; Each case is the cell records the fact block reads, the window width,
  ;; the rows the landing screen must carry, and the one it must not.
  (let ((nlk::*cells* cells))
    (with-frame-model (model width 24 :session-id "home-cells")
      (let ((frame (nle::compose-frame model)))
        (dolist (needle carried) (is (row-carrying needle frame) needle))
        (when absent (is (not (row-carrying absent frame)) (or why absent))))))
  (names-the-loaded-cells
   (list (home-fake-cell "nodecode-guard")
         (home-fake-cell "nodecode-cron")
         (home-fake-cell "mylayer" :kind :layer
                                    :defined 37))
   80 '("/cells        2 installed" "/generations  ") "· mylayer" nil)
  (keeps-a-folder-failure-out-of-home
   (list (home-fake-cell "nodecode-guard")
         (home-fake-cell "nodecode-chrome" :state :failed)
         (home-fake-cell "mylayer" :kind :layer
                          :skipped '(("a.lisp" . "names gone, which this image does not define")
                                     ("b.lisp" . "the shipped definition it overrides changed (tui/ui.lisp)"))))
   80 '("/cells        2 installed") "failed" nil)
  (offers-generations-without-the-failed-layer-file
   (list (home-fake-cell "mylayer" :kind :layer :state :failed
                          :defined 8
                          :failures '(("_dm-rev_.lisp" . "the discord channel is not running")
                                      ("_emojis_.lisp" . "boom"))))
   100 '("/cells        0 installed" "/generations  ") "failed" nil)
  (offers-the-first-cell-in-an-empty-home
   '() 80 '("/cells        0 installed" "/profile  ") "latest"
   "unknown recency is absent, not invented"))

(deftest home-screen-folds-a-refused-pins-reason-whole ()
  ;; A pin this box cannot serve says what to do about it at the end of its
  ;; reason, and one row cut at the window's edge ended before it did: `...
  ;; it is not a lane, not a models…' (xm-203). The reason folds down the value
  ;; lane like the cells, whole, a path wider than the lane included.
  (let ((reason (format nil "provider nosuchprov is not one this box knows: nothing under ~
                             providers in ~a names it; pick a model in /setup"
                        "/home/operator/a-rather-long-folder-name/.nodecode/config.jsonc")))
    (with-stubbed-fdefinition (nle::health-facts ()
                                (list (nle::health-fact :model "model" "some-model on nosuchprov"
                                                        :tone :danger :remedy reason)))
      (let ((rows (member "model" (nle::home-meta-rows 120) :key #'first :test #'equal))
            (lane (nle::home-meta-value-width 120)))
        (is (rest rows) "the reason folds onto more rows than one")
        (is (every (lambda (row) (null (first row))) (rest rows)) "under the one label")
        ;; each inside the value lane, and the remedy on the screen
        (is (every (lambda (row) (<= (nle::text-display-width (second row)) lane)) rows))
        (is (search "pick a model in /setup" (format nil "~{~a~^ ~}" (mapcar #'second rows))))))))

(deftest home-screen-takes-a-pins-verdict-from-the-gateway ()
  ;; the operator's landing screen, 2026-10-22: `model claude-opus-5-5 on
  ;; claude-code -- provider claude-code is not one this box knows ...' in red
  ;; while every turn on it ran. The shell asked its own lanes, and a lane a
  ;; cell registers runs in the gateway only. A shell says the gateway's verdict.
  (with-temp-shared-config ((format nil "{~%  \"general\": {\"default_model\": ~
                                     {\"provider\": \"cellprov\", \"model\": \"m-1\"}}~%}~%"))
    (with-saved-globals ((nle::*model-config-refusal* nil) (nle::*serving* nil) (nle::*gateway-models* nil))
      (flet ((answer (providers)
               (setf nle::*gateway-models*
                     (nle::gateway-models-facts
                      (nlk:decode-json (format nil "{\"models\":[],\"unlisted\":[],\"providers\":~a}" providers)))))
             (model-rows () (member "model" (nle::home-meta-rows 120) :key #'first :test #'equal)))
        ;; no lane here: this process's own lanes would refuse it
        (is (with-saved-globals ((nle::*serving* t)) (nle::provider-turn-refusal "cellprov")) "the shell's own lanes refuse it")
        (is (null (model-rows)) "nothing said before the gateway answers")
        (answer "[{\"id\":\"cellprov\",\"connected\":true,\"listable\":false,\"ready\":true}]")
        (is (null (model-rows)) "no row for a pin the gateway serves")
        (answer "[{\"id\":\"cellprov\",\"connected\":false,\"listable\":false,\"ready\":false,\"refusal\":\"provider cellprov is not one this box knows; pick a model in /setup\"}]")
        (is-present (rows (model-rows)) "a pin the gateway refuses, in its words"
          (is (search "m-1 on cellprov" (second (first rows))))
          (is (search "pick a model in /setup" (format nil "~{~a~^ ~}" (mapcar #'second rows))))
          (is (eq :danger (third (first rows)))))))))

(deftest home-screen-lays-out-again-when-the-provider-list-lands ()
  ;; The facts' read and the /models list's land side by side, and the facts
  ;; repaint: a list landing after them left the previous list's verdict
  ;; painted. A cold launch read the pin refused before its cells started and
  ;; kept saying so after (the operator, 2026-10-22).
  (with-temp-shared-config ((format nil "{~%  \"general\": {\"default_model\": ~
                                     {\"provider\": \"cellprov\", \"model\": \"m-1\"}}~%}~%"))
    (with-saved-globals ((nle::*model-config-refusal* nil) (nle::*serving* nil) (nle::*gateway-models* nil))
      (flet ((answer (providers)
               (setf nle::*gateway-models*
                     (nle::gateway-models-facts
                      (nlk:decode-json (format nil "{\"models\":[],\"unlisted\":[],\"providers\":~a}" providers))))))
        (answer "[{\"id\":\"cellprov\",\"connected\":true,\"listable\":false,\"ready\":false,\"refusal\":\"provider cellprov is not one this box knows; pick a model in /setup\"}]")
        (with-frame-model (model 120 40 :session-id "home-list-lands")
          (is (rows-carry-p "pick a model in /setup" (nle::compose-frame model)) "the refused verdict, painted")
          (answer "[{\"id\":\"cellprov\",\"connected\":true,\"listable\":false,\"ready\":true}]")
          (is (null (nle::gateway-models-landed model nle::*gateway-models*)) "a frame is owed")
          (is (not (rows-carry-p "pick a model in /setup" (nle::compose-frame model))) "and it says the new verdict"))))))

(deftest home-band-folds-the-cell-lane-before-a-refused-pins-remedy ()
  ;; A block taller than the band shed rows from its tail, and the tail of a
  ;; refused pin's folded reason is what to do: at 80x24 with a dozen cells
  ;; the landing screen ended mid-reason (xm-203). The cell lane folds to
  ;; one line first.
  (let ((nlk::*cells* (loop for i from 1 to 12
                             collect (home-fake-cell (format nil "nodecode-cell-~2,'0d" i))))
        (reason (format nil "provider nosuchprov is not one this box knows: nothing under ~
                             providers in ~~/.nodecode/config.jsonc names it; pick a model in /setup")))
    (with-stubbed-fdefinition (nle::health-facts ()
                                (list (nle::health-fact :model "model" "some-model on nosuchprov"
                                                        :tone :danger :remedy reason)))
      (with-frame-model (model 80 22 :session-id "home-short-pin")
        (let* ((width (nle::projection-width model))
               (plains (nth-value 1 (nle::home-band-lines model width)))
               (words (format nil "~{~a~^ ~}" (nlk:split-words (format nil "~{~a~^ ~}" plains)))))
          (is (every (lambda (line) (<= (nle::text-display-width line) width)) plains))
          (is (search "pick a model in /setup" words) words)
          (is (search "/cells 12 installed" words) words))))))

(deftest home-fact-lane-is-one-column-at-every-width ()
  ;; The cell lane is the block's widest row, so the lane's fold width is
  ;; the block's width: a lane that re-flowed with the window swept a wide
  ;; terminal instead of reading as a block. One fixed lane, whatever the
  ;; terminal; the band's own room is the only thing that narrows it.
  (is-each (nle::home-meta-value-width)
    (80 nle::+home-meta-value-width+ "an 80-column terminal shows the lane whole")
    (146 nle::+home-meta-value-width+ "a wide terminal folds at the same column")
    (300 nle::+home-meta-value-width+ "and 300 columns do not widen it by one")
    (60 42 "a band too narrow for the lane caps it at its room")))

(deftest home-tip-is-the-first-on-a-new-home-and-held-for-the-shell ()
  ;; One tip a shell: a home that has sealed no generation is on its first
  ;; launch and gets the first tip; any other launch takes one of the tips,
  ;; and keeps it for the shell's life.
  (with-stubbed-fdefinition (nlk:manifest-count () 0)
    (with-frame-model (model 100 24 :session-id "home-tip-new")
      (is (row-carrying "Tip Try \"you can't read PDFs. teach yourself.\"" (nle::compose-frame model)))))
  (with-stubbed-fdefinition (nlk:manifest-count () 12)
    (with-frame-model (model 100 24 :session-id "home-tip-held")
      (let ((tip (nle::home-tip model)))
        (is (member tip nle::+home-tips+ :test #'eq) "a launch past the first takes one of the tips")
        (nle::compose-frame model)
        (is (eq tip (nle::home-tip model)) "and keeps it frame to frame")))))

(deftest home-tips-name-commands-that-exist (dolist (tip nle::+home-tips+))
  ;; A tip that names a command teaches it; one renamed under it would teach
  ;; nothing.
  (let ((typed (second tip)))
    (when (and (> (length typed) 1) (char= #\/ (char typed 0)))
      (is (nle::find-slash-command (subseq typed 1)) typed))))

(deftest gateway-home-placeholder-narrates-the-lifecycle ()
  (with-frame-model (client 80 24 :session-id "home-gw" :http-port 4321)
    (flet ((placeholder () (nth 19 (nle::compose-frame client))))
      (is (nle::home-screen-p client) "an empty gateway attach is home")
      (let* ((rows (nle::compose-frame client))
             (logo (row-index-of "▄  ▄  ▄▄" rows :required t)))
        (is (search "Tip " (nth (+ logo 5) rows)) "the tip stands under the version")
        (is (search "v" (nth (+ logo 3) rows)) "gateway client Home carries the version tag"))
      (is (search "Connecting to Gateway..." (placeholder)))
      (setf client.sync-lifecycle :connected)
      (is (search "Ask anything, or describe a task"
                  (placeholder)))
      (is (not (search "gateway's warm" (placeholder))) "the footer's Gateway says the connection")
      (setf client.sync-lifecycle :failed)
      (is (search "Gateway sync failed - check the endpoint and retry" (placeholder)))
      ;; A pending submit suppresses home on the gateway shell.
      (setf client.sync-lifecycle :connected client.pending-command-id "cmd-1")
      (is (not (nle::home-screen-p client)))
      (is (search "Ready" (placeholder))))))

(deftest slash-clear-returns-to-the-home-screen (with-stubbed-fdefinitions)
  ;; /clear used to leave a blank transcript: SWITCH-GATEWAY-SESSION's
  ;; "[switched to …]" notice dismissed home-screen-p, so the landing
  ;; band never painted. The visual result of a fresh empty session is
  ;; the home screen (Zig create_session + isHomeScreen).
  ((nle::send-subscribe (ws ids))
   (nle::gateway-model-http-call (model method path body)
     (nlk::make-json-object "session_id" "fresh-after-clear")))
(with-frame-model (model 80 24 :session-id "old-session" :http-port 4321)
  (nle::append-tail-row
   model.state
   (nle::make-transcript-row :kind :text :role "user" :text "hello"))
  (nle::render-state model)
  (is (not (nle::home-screen-p model)))
  (type-string model "/clear" :enter)
  (is-shape model (nle::chat-model-session-id "fresh-after-clear")
    (nle::home-screen-p is))
  (let* ((rows (nle::compose-frame model))
         (logo (row-index-of "█▀▄█" rows :required t)))
    (is (search "Tip " (nth (+ logo 4) rows)))
    (is (search "v" (nth (+ logo 2) rows)) "clearing restores the version below the logo")
    (is (eq :shine (car (nle::chat-model-mark-glow model))) "and the launch's glint")
    (is (not (rows-carry-p "switched" rows))))))


;;; --- command palette ---------------------------------------------------------

(deftest ctrl-p-opens-the-command-palette (with-frame-model (model 80 24 :session-id "palette"))
  (press model #\p :mod nlt:+mod-ctrl+)
  (is-shape model (composer-value "/") (nle::slash-active-suggestions is))
  ;; The palette filters exactly as the slash lane does: typing narrows.
  (type-string model "hel")
  (is-present (suggestions (nle::slash-active-suggestions model)) "typing filters the palette"
    (is (member "help" suggestions
                :key (lambda (row)
                       (nle::slash-command-name
                        (nle::composer-suggestion-entry row)))
                :test #'string=)))
  ;; With composer text the key is dropdown navigation, never an
  ;; insert: the text stays put and the dropdown stays open.
  (press model #\p :mod nlt:+mod-ctrl+)
  (is-shape model (composer-value "/hel") (nle::slash-active-suggestions is)))

;;; --- gateway reducer: reasoning outlives its stream --------------------------

(deftest gateway-reducer-keeps-reasoning-past-the-turn ()
  ;; The sync stream's thinking deltas open a live-only row; the durable
  ;; assistant fact that follows carries the same trace as `reasoning_content`
  ;; and hydrates it. Without that hydration the terminal fact's settle drops
  ;; the row and the finished turn shows no reasoning at all.
  (let ((delta-event (nlk:json-object
                      "type" "item_delta"
                      "turn_id" "turn-r"
                      "delta" (nlk:json-object "type" "reasoning"
                                               "text" "weighing options")))
        (fact (nlk:json-object
               "type" "turn.assistant_message_completed"
               "turn_id" "turn-r"
               "message" (nlk:json-object
                          "role" "assistant"
                          "content" "here is the answer"
                          "reasoning_content" "weighing options")))
        (done (nlk:json-object "type" "turn.completed" "turn_id" "turn-r")))
    (let ((state (nle::apply-gateway-event nil delta-event)))
      (feed-events state fact done)
      (let ((row (find :reasoning (nle::transcript-rows state)
                       :key #'nle::transcript-row-kind)))
        (is row "the reasoning row outlives the turn it belongs to")
        (is-shape row (.text "weighing options") (.retention eq :persisted-history))))))

;;; --- gateway reducer: tool_result display facts ------------------------------

(deftest gateway-reducer-folds-tool-display-metadata ()
  ;; The wire tool_result carries the same metadata envelope the durable
  ;; fact stores; the reducer stamps it on the coalesced row so the image
  ;; card renders identically live and replayed.
  (let ((result (nlk:json-object
                 "type" "turn.tool_result"
                 "call-id" "call-img"
                 "tool-name" "gen_image"
                 "result" "https://example.test/i.png"
                 "metadata" (nlk:json-object
                             "image" (nlk:json-object
                                      "payload_kind" "url"
                                      "source" "tool_result")))))
    (let* ((row (first (nle::transcript-rows (nle::apply-gateway-event nil result))))
           (plist (nle::row-image-plist row)))
      (is plist "the image fact marks the row")
      (is-shape plist (:payload-kind "url") (:source "tool_result")))))

;;; --- chrome rows never overflow narrow windows -------------------------------
;;; The geometry contract: every frame row renders inside the frame width.
;;; The two historical offenders were the footer identity lane and the
;;; composer placeholder notice, both of which bypassed wrapping.

(deftest chrome-rows-clip-to-narrow-windows ()
  (let ((nle::*provider* "a-verbose-provider-name")
        (nle::*model* "a-verbose-provider-name/very-long-model-id"))
    (with-frame-model (model 24 24 :session-id "clip-narrow")
      (nle::show-status
       model "A startup notice far wider than the whole window carries")
      (let ((frame (nle::compose-frame model)))
        (is (= 24 (length frame)) "the frame stacks to term-height")
        (loop for row in frame
              do (is (<= (nle::text-display-width row) 24)))
        (is (search "…" (nth 22 frame)))
        (is (rows-carry-p "carries" frame) "the status wraps rather than clips")))))

(deftest header-session-id-follows-viewport (dolist (width '(32 180)))
  (with-frame-model (model width 26 :session-id "s-WUQUK0OV")
    (fold-model model (nle::ingest-message-fact "user" "title · note"))
    (let ((state model.state))
      (is-carrying (header (second (nle::compose-frame model)))
        "title · note" (:absent "s-WUQUK0OV" "Standard keeps the title without its raw id")
        (is (= width (nle::text-display-width header))))
      (press model #\o :mod nlt:+mod-ctrl+)
      (is (search "title · note · s-WUQUK0OV" (second (nle::compose-frame model))) "Developer keeps the original identity")
      (press model #\o :mod nlt:+mod-ctrl+)
      (is (not (search "s-WUQUK0OV" (second (nle::compose-frame model)))))
      (is (equal "s-WUQUK0OV" (nle::chat-model-session-id model)))
      (is (eq state model.state) "viewport changes leave session facts intact"))))

(deftest standard-header-never-falls-back-to-raw-session-id ()
  (with-frame-model (model 100 24 :session-id "s-UNTITLED")
    ;; A live turn leaves Home without creating a committed title.
    (setf model.turn-in-flight t)
    (is (not (nle::home-screen-p model)))
    (is (equal "s-UNTITLED" (nle::session-display-title model)) "shared directory title fallback remains unchanged")
    (is (not (search "s-UNTITLED" (second (nle::compose-frame model)))))
    (press model #\o :mod nlt:+mod-ctrl+)
    (is (search "s-UNTITLED" (second (nle::compose-frame model))))
    (press model #\o :mod nlt:+mod-ctrl+)
    (fold-model model (nle::ingest-message-fact "user" "s-UNTITLED"))
    (is (search "s-UNTITLED" (second (nle::compose-frame model))) "an actual user-authored title is not mistaken for the id fallback")))

(deftest header-usage-follows-viewport-and-retains-global-context ()
  (with-pinned-window ("chrome-model" 16000)
    (with-frame-model (model 180 26 :session-id "header-viewport")
      (fold-usage model :input 553 :output 2100 :cached 3840 :reasoning 1900
                       :model "chrome-model" :cost-known t :cost-usd 0.836)
      (let ((usage (gethash "session_usage" model.state))
            (full "27.5%/16k · ↑ 553 (cache 87.4%) · ↓ 2.1k (reasoning 1.9k) · $0.836"))
        (is (equal "27.5%/16k · cache 87.4%" (nle::header-status-text model)) "Standard retains global context and cache hit rate, not token counts or cost")
        (is (not (search "↑" (second (nle::compose-frame model)))) "Standard's composed header omits usage")
        (press model #\o :mod nlt:+mod-ctrl+)
        (is (equal full (nle::header-status-text model)) "Developer retains every usage note and cost")
        (is (search full (second (nle::compose-frame model))) "Developer paints the full session line")
        (press model #\o :mod nlt:+mod-ctrl+)
        (is (equal full (nle::header-status-text model :detail :developer)) "hover can request the same full formatter in Standard")
        (is (eq usage (gethash "session_usage" model.state)) "mode switches do not mutate usage")
        (fold-model model (nle::begin-turn-usage))
        (is (equal "27.5%/16k · cache 87.4%" (nle::header-status-text model)) "the global context survives a turn boundary")
        (setf (getf (gethash "context_usage" model.state) :model) "uncatalogued-model")
        (is (equal "ctx 4.4k · cache 87.4%" (nle::header-status-text model)) "unknown windows retain absolute context without inventing a percent")
        (setf (getf (gethash "session_usage" model.state) :cached) nil)
        (is (equal "ctx 4.4k" (nle::header-status-text model)) "absent cache facts do not invent a hit rate")))))


(deftest live-status-hint-only-paints-on-verb-hover (dolist (profile '(:none :truecolor)))
  (dolist (width '(40 80 130))
    (with-frame-model (model width 24 :session-id "status-hint-hover")
      (let ((nle::*color-profile* profile))
        (land-model-facts model :provider "p" :model "模型")
        (mark-turn-in-flight model)
        (setf model.active-turn-id "turn"
              (nle::chat-model-turn-started-at model) (get-internal-real-time))
        (nle::render-state model)
        (nle::compose-frame model)
        (let* ((rows (nle::frame-plain-rows model))
               (row (position-if (lambda (text) (search "waiting" text)) rows))
               (text (aref rows row))
               (x (1+ (nle::text-display-width (subseq text 0 (search "waiting" text)))))
               (y (1+ row)))
          (is-carrying text (:absent "type to steer" "the tooltip's own hints stay off the bar")
            (:absent "viewport"))
          (when (= width 130)
            (is (search " esc  stop" text) "esc stands on the bar where it fits"))
          (is (eq :status-hint (nle::frame-target-at-position model x y)))
          (is (null (nle::frame-target-at-position model (1- x) y)) "elapsed and separator are not hover targets")
          (is (null (nle::frame-target-at-position model (+ x (length "waiting on the model")) y)) "the target ends with the verb")
          (is (null (nle::frame-overlay model)))
          (nle:update model (nlt:make-mouse-motion-msg :x x :y y))
          (is (eq :status-hint (nle::chat-model-hover-target model)))
          (let* ((frame (nle::compose-frame model))
                 (buffer (nlt:parse-styled-string frame width 24))
                 (overlay (nle::frame-overlay model)))
            (is overlay "verb hover supplies a real paint overlay")
            (when overlay (funcall overlay buffer))
            (let ((painted (loop for yy below 24 collect (buffer-text buffer yy))))
              (is-table (in) (rows-carry-p in painted) ("ctrl+o viewport"))))
          (with-stubbed-fdefinition (nle::open-target (&rest _) (error "hover-only verb must not open a URI"))
            (click-at model x y))
          (nle:update model (nlt:make-mouse-motion-msg :x (1- x) :y y))
          (nle::compose-frame model)
          (is (null (nle::frame-overlay model)) "leave removes the tooltip")
          (rendering model (setf model.stop-requested "turn"))
          (let ((hint (nle::live-status-hint model)))
            (is-carrying hint (:absent "esc interrupts") (:absent "type to steer")
              "ctrl+o viewport"))
          (setf (nle::chat-model-turn-in-flight model) nil)
          (nle::compose-frame model)
          (is (null (nle::frame-overlay model)) "idle never exposes live controls"))))))
