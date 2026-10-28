;;;; timestamps-test.lisp --- layer-independent transcript timestamp policy.
(in-package #:nodecode.evolved)

(nodecode.test:deftest nodecode.test::message-timestamp-hover ()
  "Regression for timestamp-only turn details, no terminal divider, and truthful missing/live facts."
  (let* ((*transcript-time-facts* (make-hash-table :test 'equal))
         (*transcript-live-times* (make-hash-table :test 'eq :weakness :key))
         (*message-timestamp-hovers* (make-hash-table :test 'eq :weakness :key))
         (*color-profile* :none)
         (state (make-hash-table :test 'equal))
         (model (make-instance 'gateway-chat-model :session-id "timestamp-fixture" :ws nil))
         (user (tail-row state :role "user" :text "question" :turn-id "tt" :log-position 1 :hydrated t))
         (answer (tail-row state :role "assistant" :text "answer with [link](https://example.com/)" :turn-id "tt" :log-position 2 :hydrated t))
         (divider (tail-row state :kind :finish-divider :role "system" :text "• 02:35" :outcome "completed" :turn-id "tt"))
         (eviction (make-transcript-row :kind :finish-divider :role "system" :text "• evicted 1 turn"))
         (unknown (make-transcript-row :role "assistant" :turn-id "absent")))
    (note-transcript-time 1 "2026-10-12T20:04:00.000Z" "turn.started" "tt")
    (note-transcript-time 2 "2026-10-12T20:04:00.100Z" "turn.assistant_message_completed" "tt")
    (note-transcript-time 3 "2026-10-12T20:06:35.304Z" "turn.completed" "tt")
    (note-transcript-usage "tt" '(:provider "deepseek" :model "requested" :response-model "served" :effort "max"))
    (assert (eq t (message-timestamp-row-p user)))
    (assert (eq nil (message-timestamp-row-p divider)))
    ;; A prompt's words start on the prose column as the reply's do, the
    ;; clock beside them at the row's right end and no name riding it.
    (let ((first (first (row-llines user 80))))
      (assert (equal "question" (lline-text first)))
      (assert (equal (transcript-clock user) (seg-text (lline-aside first)))))
    (let ((multiline (make-transcript-row :kind :text :role "user" :log-position 1
                                        :text (format nil "α界~%second line"))))
      (assert (equal '("α界" "second line") (mapcar #'lline-text (row-llines multiline 40))))
      (assert (equal '(:prose :prose) (mapcar #'lline-role (row-llines multiline 40))))
      (assert (lline-aside (first (row-llines multiline 40))))
      (assert (null (lline-aside (second (row-llines multiline 40)))))
      ;; Too narrow to stand beside the words: the words take the row.
      (assert (equal '("α界" "second line") (mapcar #'lline-text (row-llines multiline 20))))
      (assert (notany #'lline-aside (row-llines multiline 20))))
    (assert (null (row-llines divider 80)))
    (assert (null (display-block-lines (row-display-block divider :width 80))))
    (assert (equal "• 02:35" (transcript-row-text divider)))
    (assert (search "evicted" (lline-text (first (row-llines eviction 80)))))
    (assert (equal '("Elapsed: 2m 35s" "Model: deepseek/served" "Effort: max") (message-timestamp-details answer)))
    (assert (equal '("Elapsed: unavailable" "Model: unavailable" "Effort: unavailable") (message-timestamp-details unknown)))
    (note-transcript-time 4 "2026-10-12T20:07:00.000Z" "turn.started" "running")
    (assert (search "still running" (first (message-timestamp-details (make-transcript-row :turn-id "running")))))
    (let* ((line (first (row-llines answer 80)))
           (span (find "https://example.com/" (lline-spans line) :key #'mspan-link :test #'equal)))
      (assert (equal "link" (subseq (lline-text line) (mspan-start span) (mspan-end span)))))
    (setf (chat-model-state model) state
          (chat-model-term-width model) 100 (chat-model-term-height model) 24
          (chat-model-editor model) (make-editor :width 90 :max-height 6)
          (chat-model-band model) (make-band-window :height 16))
    (render-state model)
    (assert (= 3 (search "question" (plain-frame-row (second (compose-frame model))))))
    (flet ((hit (row)
             (loop for y from 1 to 24 thereis
                   (loop for x from 1 to 100
                         when (eq row (message-timestamp-at-position model x y)) return (cons x y))))
           (clock-x (y clock)
             (1+ (search clock (plain-frame-row (nth (1- y) (compose-frame model))) :from-end t))))
      (let* ((clock (transcript-clock user)) (hit (hit user)))
        (assert hit)
        (assert (= (clock-x (cdr hit) clock) (car hit)) () "The hover target starts where the clock does.")
        (assert (> (car hit) 50) () "The clock stands at the row's right end.")
        (assert (message-timestamp-at-position model (+ (car hit) (length clock) -1) (cdr hit)))
        (assert (null (message-timestamp-at-position model (+ (car hit) (length clock)) (cdr hit))))
        (assert (null (message-timestamp-at-position model 6 (cdr hit))) () "The words are not part of the hover target."))
      (let* ((clock (transcript-clock answer)) (hit (hit answer)))
        (assert hit)
        (assert (= (clock-x (cdr hit) clock) (car hit)) () "The hover target starts where the clock does.")
        (assert (not (message-timestamp-at-position model (- (car hit) 2) (cdr hit)))))
      (let ((hit (hit answer)))
        (handle-mouse-motion
         model (nlt:make-mouse-motion-msg :x (car hit) :y (cdr hit) :button nil))
        (assert (eq answer (first (gethash model *message-timestamp-hovers*))))
        (let* ((buffer (nlt::parse-styled-string (compose-frame model) 100 24))
               (overlay (frame-overlay model)))
          (funcall overlay buffer)
          (assert (loop for y below 24 thereis
                        (search "2m 35s" (coerce (loop for x below 100 collect (nlt:cell-char (nlt::screen-buffer-ref buffer x y))) 'string)))))
        (handle-mouse-motion model (nlt:make-mouse-motion-msg :x 1 :y 1 :button nil))
        (assert (null (gethash model *message-timestamp-hovers*)))))
    (dolist (row (list (make-transcript-row :kind :reasoning :text "thinking")
                      (make-transcript-row :kind :tool-call :role "tool" :tool-name "eval" :tool-args "{}" :tool-status "done")
                      (make-transcript-row :kind :system-notice :role "system" :text "notice")))
      (assert (eq nil (message-timestamp-row-p row)))
      (assert (notany #'lline-aside (row-llines row 80))))))

(nodecode.test:deftest nodecode.test::message-elapsed-human-readable ()
  "Whole-second mixed units, subsecond truth and untouched recorded timing."
  (dolist (case '((0 "0s") (1 "<1s") (125 "<1s") (1000 "1s")
                  (59999 "59s") (60000 "1m 0s") (155304 "2m 35s")
                  (490000 "8m 10s") (490100 "8m 10s") (490010 "8m 10s")
                  (3600000 "1h 0s") (3723004 "1h 2m 3s") (86400000 "1d 0s")))
    (assert (equal (second case) (format-message-elapsed (first case)))))
  (let* ((*transcript-time-facts* (make-hash-table :test 'equal))
         (row (make-transcript-row :role "assistant" :turn-id "mixed-unit-fixture")))
    (setf (gethash "mixed-unit-fixture" *transcript-time-facts*) '(:elapsed 490125 :provider "a6api" :model "gpt-6.1-sol" :effort "xhigh"))
    (assert (equal '("Elapsed: 8m 10s" "Model: a6api/gpt-6.1-sol" "Effort: xhigh") (message-timestamp-details row)))
    (assert (= 490125 (getf (gethash "mixed-unit-fixture" *transcript-time-facts*) :elapsed)))))

(nodecode.test:deftest nodecode.test::message-turn-usage ()
  "User timestamp usage sums live/replay rounds, retains old turns, and never invents absent counts."
  (let* ((*transcript-time-facts* (make-hash-table :test 'equal))
         (state (make-hash-table :test 'equal))
         (user (make-transcript-row :role "user" :turn-id "usage-a"))
         (answer (make-transcript-row :role "assistant" :turn-id "usage-a")))
    (flet ((round-fact (turn input output cached reasoning &optional replay)
             (let ((fact
                    (json-object "type" "turn.usage" "input-tokens" input "output-tokens" output
                                 "cached-input-tokens" cached "reasoning-tokens" reasoning
                                 "provider" "p" "model" "m" "reasoning-effort" "high" "estimated" t
                                 "metadata"
                                 (json-object "turn_id" turn "log_position"
                                              (incf (gethash "fixture-event-seq" state 0))
                                              "occurred_at" "2026-10-12T20:00:00.000Z"))))
               (setf state
                       (if replay
                           (apply-event state (gateway-fact-event "turn.usage" fact fact turn))
                           (apply-gateway-event state fact))))))
      (setf (gethash "turn_id" state) "not-the-owner")
      (round-fact "usage-a" 1000 20 9000 300)
      (round-fact "usage-a" 500 12 9500 30 t)
      (assert
       (equal
        '("Elapsed: unavailable" "Model: p/m" "Effort: high" "Usage: ↑~1.5k c92.5% ↓~32 r~330")
        (message-timestamp-details user)))
      (assert
       (equal '("Elapsed: unavailable" "Model: p/m" "Effort: high")
              (message-timestamp-details answer)))
      (begin-turn-usage state)
      (round-fact "usage-b" 8000 1000 0 0)
      (assert (equal "Usage: ↑~1.5k c92.5% ↓~32 r~330" (fourth (message-timestamp-details user))))
      (assert (= 2 (getf (getf (gethash "usage-a" *transcript-time-facts*) :usage) :rounds)))
      (assert (not (gethash "not-the-owner" *transcript-time-facts*))))
    (flet ((usage-line (turn-id)
             (fourth (message-timestamp-details
                      (make-transcript-row :role "user" :turn-id turn-id)))))
      (assert (equal "Usage: unavailable" (usage-line "absent")))
      (note-transcript-usage "zero" '(:input 0 :output 0))
      (assert (equal "Usage: ↑0 ↓0" (usage-line "zero"))))
    (let* ((*color-profile* :none)
           (model (make-instance 'gateway-chat-model :session-id "usage-tooltip" :ws nil)))
      (setf (chat-model-term-width model) 48
            (chat-model-term-height model) 24)
      (let ((rows (message-timestamp-tooltip-rows model user)))
        (assert (some (lambda (row) (search "Usage:" (plain-frame-row row))) rows))
        (assert (every (lambda (row) (<= (text-display-width (plain-frame-row row)) 46)) rows))))))

(nodecode.test:deftest nodecode.test::header-usage-tooltip ()
  "Header usage follows viewport; timestamp hover retains the full labelled session bill."
  (let* ((*color-profile* :none)
         (*transcript-time-facts* (make-hash-table :test 'equal))
         (*model-capability-overrides* '(("tooltip-model" :context-tokens 16000)))
         (state
          (ingest-turn-usage nil :input 553 :output 2100 :cached 3840 :reasoning 1900 :model
                             "tooltip-model" :cost-known t :cost-usd 0.836))
         (model (make-instance 'gateway-chat-model :session-id "tooltip-fixture" :ws nil))
         (row (make-transcript-row :role "assistant" :turn-id "tooltip-turn"))
         (full "27.5%/16k · ↑ 553 (cache 87.4%) · ↓ 2.1k (reasoning 1.9k) · $0.836"))
    (setf (chat-model-state model) state
          (chat-model-term-height model) 26)
    (dolist (mode '(:standard :developer))
      (setf (chat-model-tool-detail model) mode
            (chat-model-term-width model) 180)
      (assert
       (equal
        (if (eq mode :standard)
            "27.5%/16k · cache 87.4%"
            full)
        (header-status-text model)))
      (assert
       (some (lambda (line) (search (format nil "Session: ~a" full) (plain-frame-row line)))
             (message-timestamp-tooltip-rows model row)))
      (setf (chat-model-term-width model) 48)
      (let* ((lines (mapcar #'plain-frame-row (message-timestamp-tooltip-rows model row)))
             (text (format nil "~{~a~^ ~}" (mapcar (lambda (s) (string-trim " " s)) lines))))
        (assert (every (lambda (line) (<= (text-display-width line) 46)) lines))
        (dolist (needle '("Session:" "27.5%/16k" "cache 87.4%" "reasoning 1.9k" "$0.836"))
          (assert (search needle text)))))
    (setf (chat-model-state model) nil)
    (assert
     (notany (lambda (line) (search "Session:" (plain-frame-row line)))
             (message-timestamp-tooltip-rows model row)))))

;; Add a focused status-bar regression proving identity/tally removal while retaining activity, timing, meter and stored facts.
(nodecode.test:deftest nodecode.test::transcript-status-bar-policy ()
  "Transcript-only removal: no model/effort/action tally; other live and failed-bar facts remain."
  (let* ((*color-profile* :none)
         (model (make-instance 'gateway-chat-model :session-id "status-policy-fixture" :ws nil))
         (state (make-hash-table :test 'equal))
         (now (get-internal-real-time)))
    (setf (gethash "usage" state) '(:provider "a6api" :model "gpt-6.1-sol" :effort "xhigh" :input 100 :output 20 :cached 50)
          (chat-model-state model) state
          (chat-model-tool-detail model) :developer
          (chat-model-turn-in-flight model) t
          (chat-model-turn-started-at model) (- now internal-time-units-per-second)
          (chat-model-turn-activity model) (list "thinking" :thinking now)
          (chat-model-turn-tally model) "6 actions"
          (chat-model-live-label-cache model) nil)
    (let ((plain (plain-frame-row (live-status-line model 300))))
      (dolist (gone '("a6api" "gpt-6.1" "xhigh" "actions")) (assert (not (search gone plain))))
      (dolist (kept '("thinking" "1." "↑")) (assert (search kept plain))))
    (assert (equal "6 actions" (slot-value model 'turn-tally)))
    (assert (equal "gpt-6.1-sol" (getf (gethash "usage" state) :model)))
    (assert (equal "a6api/gpt-6.1-sol" (status-identity-label "a6api" "gpt-6.1-sol")))
    (setf (chat-model-turn-in-flight model) nil
          (gethash "turn_resting_status" state) '(:outcome "failed" :elapsed-ms 1234))
    (nodecode.test::is-carrying (plain (plain-frame-row (live-status-line model 300)))
      "failed" "1.2s" (:absent "a6api"))
    (let ((preview (make-hash-table :test 'equal)))
      (setf (gethash "turn_id" preview) "preview-turn"
            (gethash "turn_started_at_ms" preview) (iso-8601-ms (nlk::iso-now))
            (chat-model-transcript-preview model) (cons "preview-session" preview))
      ;; A preview with no dialog over it takes no bar: the transcript shown
      ;; is the attached session's.
      (assert (eq state (bar-lane model))))
    :passed))

(nodecode.test:deftest nodecode.test::message-event-clock-parity ()
  (let* ((*transcript-time-facts* (make-hash-table :test 'equal))
         (*transcript-live-times* (make-hash-table :test 'eq :weakness :key))
         (state (make-hash-table :test 'equal))
         (fact (json-object "type" "turn.started" "turn_id" "clock-turn"
                           "metadata" (json-object "turn_id" "clock-turn" "log_position" 17
                                                   "occurred_at" "2026-10-12T20:04:00.000Z")))
         (row (make-transcript-row :kind :text :role "assistant" :log-position 17 :turn-id "clock-turn")))
    (apply-gateway-event state fact)
    (assert (= 4000824240000 (gethash 17 *transcript-time-facts*)))
    (assert (= 4000824240000 (getf (gethash "clock-turn" *transcript-time-facts*) :started)))
    (let ((clock (transcript-clock row)))
      (clrhash *transcript-time-facts*)
      (apply-event state (gateway-fact-event "turn.started" fact fact "clock-turn"))
      (assert (= 4000824240000 (gethash 17 *transcript-time-facts*)))
      (assert (equal clock (transcript-clock row))))
    ;; A durable position without a clock must never acquire render time.
    (assert (equal "time unknown"
                   (transcript-clock (make-transcript-row :log-position 18))))
    (let ((live (make-transcript-row :role "assistant")))
      (transcript-clock live)
      (assert (integerp (gethash live *transcript-live-times*)))
      (setf (gethash live *transcript-live-times*) 4000824240000)
      (assert (equal (transcript-clock row) (transcript-clock live))))))

(nodecode.test:deftest nodecode.test::message-timestamp-indent ()
  "A prompt's words, wrapped and on, start on the prose column, its clock at
the first row's right end; a first line laid out to the full row stands under
a clock row of its own."
  (let ((*color-profile* :none)
        (*transcript-live-times* (make-hash-table :test 'eq)))
    (nlk:with-cleanup ((nlk:unhook 'transcript-clock "indent-fixture"))
      (nlk:hook 'transcript-clock "indent-fixture" (lambda (next item) (declare (ignore next item)) "10:22 PM"))
      (dolist (width '(24 40 80))
        (let* ((row (make-transcript-row :kind :text :role "user" :hydrated t
                                         :text (format nil "α界 one two three four five six seven eight nine ten~%next line")))
               (plain (nth-value 1 (llines-rows (row-llines row width) width))))
          (assert (= 3 (search "α界" (first plain))))
          (assert (every (lambda (line) (= 3 (position-if-not #'blank-char-p line))) (rest plain)))
          (assert (every (lambda (line) (<= (text-display-width line) width)) plain))
          (if (< width *aside-min-width*)
              (assert (notany (lambda (line) (search "10:22 PM" line)) plain))
              (let ((first (string-right-trim " " (first plain))))
                (assert (= (- width *transcript-right-padding*) (text-display-width first)))
                (assert (string= "10:22 PM" first :start2 (- (length first) 8)))
                (assert (notany (lambda (line) (search "10:22 PM" line)) (rest plain))))))))
    (flet ((stamped (line) (stamp-transcript-llines (list line) "10:22 PM" 80)))
      ;; A code line, a notice, a rule: the clock stands over them, on the
      ;; surface of the line it heads, and the line itself is never written.
      (dolist (body (list (make-lline :text "structured body" :role :code)
                          (make-lline :text "structured body" :role :system :fill :system-block)
                          (make-lline :text "" :rule t)))
        (let ((lines (stamped body)))
          (assert (equal "" (lline-text (first lines))))
          (assert (equal "10:22 PM" (seg-text (lline-aside (first lines)))))
          (assert (eq (lline-fill body) (lline-fill (first lines))))
          (assert (eq body (second lines)))
          (assert (null (lline-aside body)))))
      ;; Prose always shares the row, wrapping short of the clock -- one that
      ;; fits the full row but not beside it too; a table's row, laid out to
      ;; the row, stands under it.
      (dolist (length '(10 70 100))
        (let* ((lines (stamped (make-lline :text (make-string length :initial-element #\x))))
               (plain (nth-value 1 (llines-rows lines 80))))
          (assert (= 1 (length lines)))
          (assert (search "10:22 PM" (first plain)))
          (assert (eq (> length 66) (> (length plain) 1)))))
      (let ((lines (stamped (make-lline :text (make-string 70 :initial-element #\x) :grid t))))
        (assert (= 2 (length lines)))
        (assert (equal "" (lline-text (first lines))))))))

(nodecode.test:deftest nodecode.test::assistant-content-aligns-with-activity-comments ()
  "First, wrapped and subsequent assistant lines start on the prose column,
where an Eval or a Think label stands."
  (let ((*color-profile* :none) (*tool-detail* :standard) (*expanded-tool-call* nil)
        (*transcript-live-times* (make-hash-table :test 'eq)))
    (dolist (clock '("2:57 PM" "10:22 PM" "time unknown"))
      (dolist (width '(24 40 80))
        (let* ((text (format nil "α界 one two three four five six seven eight nine ten~%Next paragraph"))
               (row (make-transcript-row :kind :text :role "assistant" :text text :hydrated t))
               (activity (append-title-detail (make-lline :text "Eval") "Comment"))
               (column (search "Eval" (first (nth-value 1 (lline-rows activity width))))))
          (nlk:with-cleanup ((nlk:unhook 'transcript-clock "alignment-fixture"))
            (nlk:hook 'transcript-clock "alignment-fixture" (lambda (next item) (declare (ignore next item)) clock))
            (let* ((lines (row-llines row width))
                   (plain (nth-value 1 (llines-rows lines width)))
                   (content (remove-if-not (lambda (line) (position-if-not #'blank-char-p line)) plain)))
              (assert (eq (>= width *aside-min-width*)
                          (and (search clock (first content)) t)))
              (assert (= 3 column))
              (assert (= column (search "α界" (first (remove-if-not (lambda (line) (search "α界" line)) plain)))))
              (assert (= column (search "Next" (first (remove-if-not (lambda (line) (search "Next" line)) plain)))))
              (dolist (line content)
                (assert (= column (position-if-not #'blank-char-p line))))
              (assert (every (lambda (line) (<= (text-display-width line) width)) plain)))))))
    ;; Stamping keeps list/quote/code geometry and link spans: only the first
    ;; line is copied, to carry the clock.
    (let* ((text (format nil "- Parent [link](https://example.test) needs words to wrap~%  - Nested item needs words to wrap~%~%> Quoted words need enough words to wrap~%~%```~%code words need enough words to wrap~%```"))
           (body (markdown-llines text :width 31))
           (stamped (stamp-transcript-llines body "2:57 PM" 40))
           (plain (nth-value 1 (llines-rows stamped 40)))
           (link-line (find-if (lambda (line) (search "link" line)) plain))
           (at (search "link" link-line)))
      (assert (not (eq (first body) (first stamped))))
      (assert (equal (lline-text (first body)) (lline-text (first stamped))))
      (assert (every #'eq (rest body) (rest stamped)))
      ;; A bullet is a warm dot (hollow nested) and a quote's bars and a
      ;; fence's edge are the gutter's ▌: the nesting still stands two columns
      ;; in, and the quote and the fence stand on one edge.
      (flet ((at (needle) (or (loop for line in plain thereis (search needle line)) -1)))
        (assert (= 3 (at "• Parent")))
        (assert (search "2:57 PM" (first plain)))
        (assert (= 5 (at "◦ Nested")))
        (assert (= 3 (at "▌ Quoted")))
        (assert (= 3 (at "▌ code"))))
      (assert (equal (list (list at (+ at 4) "https://example.test"))
                     (plain-row-links link-line))))))

(nodecode.test:deftest nodecode.test::status-accessories-dock-below-the-live-bar ()
  (dolist (mode '(:standard :developer))
    (nodecode.test::with-queued-prompts (model "status-accessories" ("Queued follow-up" :prompt-id "queued-fixture"))
      (setf (chat-model-tool-detail model) mode
            (state-background-rows (chat-model-state model))
            (list (nodecode.test::band-row :command "background-fixture" :shell-id 247 :tail "Output" :seen 1)))
      (dolist (gesture '(:home :page-up :end))
        (nodecode.test::press model gesture)
        (render-state model)
        (let* ((rows (mapcar #'plain-frame-row (compose-frame model)))
               (status (position-if (lambda (line) (search "waiting" line)) rows))
               (exec (position-if (lambda (line) (search "background-fixture" line)) rows))
               (queued (position-if (lambda (line) (search "Queued follow-up" line)) rows)))
          ;; The exec is history; the bar and its queued prompts are the
          ;; transcript's tail, one canvas row over the composer.
          (assert (and status exec queued (< exec status queued)))
          (let ((foot (+ 2 (band-window-height (chat-model-band model)))))
            (assert (<= queued foot))
            (assert (zerop (length (string-trim " " (nth (1+ foot) rows))))))
          (let ((index (frame-transcript-position model 8 (1+ exec))))
            (assert (and index (bg-band-row-p (aref (chat-model-line-rows model) index))))))))))

(nodecode.test:deftest nodecode.test::pinned-user-closing-pad-survives-a-body-only-pin ()
  (let* ((*color-profile* :truecolor)
         (row (make-transcript-row :kind :text :role "user" :text (format nil "first~%second~%third")))
         (block (row-display-block row :width 100))
         (window (make-band-window :height 12))
         (lines (append (display-block-lines block) (list "" "answer"))))
    (assert (= 5 (length (display-block-lines block))))
    (setf (band-prompt-spans lines) (list (cons 0 5)))
    (band-window-set-lines window lines nil)
    (band-window-goto-bottom window)
    (let* ((visible (band-window-visible window 100))
           (buffer (nlt:parse-styled-string visible 100 (length visible))))
      (assert (eql 4 (band-window-row-index window 4)))
      (assert (null (band-window-row-index window 5)))
      (assert (equal (color-sgr (visual-style-bg (style-for-tone :user-block)) nil :truecolor)
                     (nlt:cell-bg (nlt::screen-buffer-ref buffer 8 4))))
      (assert (null (nlt:cell-bg (nlt::screen-buffer-ref buffer 8 5)))))))

;;; Every view that folds gateway events notes its clocks here: the shell's own
;;; fold, and the gateway's rows views -- a channel lane's, a late shell's
;;; catch-up -- each on the thread that serves it. They fold at once.

(nodecode.test:deftest nodecode.test::transcript-clocks-hold-while-views-fold-at-once ()
  (let* ((views 8) (each 300) (base 970000000)
         (table *transcript-time-facts*)
         (faults '()) (lock (bt2:make-lock :name "fold-faults")))
    (flet ((turn (k i) (format nil "fold-at-once-~d-~d" k i))
           (position-of (k i) (+ base (* k each) i)))
      (unwind-protect
           (let ((threads
                   (loop for k below views
                         collect (let ((k k))
                                   (bt2:make-thread
                                    (lambda ()
                                      (let ((*transcript-time-facts* table)
                                            (state (make-hash-table :test 'equal)))
                                        (handler-case
                                            (dotimes (i each)
                                              (setf state
                                                    (apply-gateway-event
                                                     state
                                                     (json-object "type" "turn.usage" "input-tokens" 10 "output-tokens" 2
                                                                  "provider" "p" "model" "m"
                                                                  "metadata" (json-object "turn_id" (turn k i)
                                                                                          "log_position" (position-of k i)
                                                                                          "occurred_at" "2026-10-28T19:00:00.000Z")))))
                                          (error (e) (bt2:with-lock-held (lock) (push (princ-to-string e) faults))))))
                                    :name "fold-at-once")))))
             (dolist (thread threads)
               (bt2:join-thread thread))
             (nodecode.test::is (null faults) (format nil "no fold faulted: ~{~a~^; ~}" (subseq faults 0 (min 3 (length faults)))))
             (nodecode.test::is (loop for k below views always (loop for i below each always (gethash (position-of k i) table))) "every fold's event clock is kept")
             (nodecode.test::is (loop for k below views always (loop for i below each always (getf (gethash (turn k i) table) :usage))) "every turn's usage is kept"))
        (dotimes (k views)
          (dotimes (i each)
            (remhash (turn k i) table)
            (remhash (position-of k i) table)))))))
