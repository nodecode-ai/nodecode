;;;; paint.lisp --- rendering, the background-process band, the held prompt, the home screen. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)


(defun note-turn-activity (model verb kind &optional (now (get-internal-real-time)))
  "Stamp VERB, of KIND, as what MODEL's running turn is on — the live bar's
verb and its mark read the stamp, (VERB KIND NOW)."
  ;; A round's wait for its first token is the durable turn.provider_request
  ;; fact's (ttft-ms), read by the usage reports; no bar shows it.
  (setf (chat-model-turn-activity model) (list verb kind now)))

(defun toggle-tool-detail (model &optional (next (if (eq :developer (chat-model-tool-detail model))
                                                     :standard :developer)))
  "Set MODEL's viewport mode, by default the other of Developer and Standard."
  (check-type next (member :developer :standard))
  (setf (chat-model-tool-detail model) next)
  (show-status model (format nil "Switched to ~:(~a~) viewport" next))
  next)

(defun tool-detail-hint (model)
  "The live bar's viewport control — `ctrl+o viewport' — under the
chord MODEL's keymap binds, or NIL when it is unbound."
  (when-let (chord (first (gethash :toggle-viewport (chat-model-keymap model))))
    (format nil "~(~a~) viewport" (key-chord-label chord))))

(defun slash-details-op (model args)
  "The /details op: set Developer/Standard viewport mode, or report it."
  (let ((arg (string-downcase (string-trim '(#\Space #\Tab) (or args "")))))
    (cond ((zerop (length arg))
           (format nil "viewport ~:(~a~)" (chat-model-tool-detail model)))
          ((member arg '("developer" "standard") :test #'string=)
           (toggle-tool-detail model (if (equal arg "developer") :developer :standard))
           nil)
          (t (slash-usage "details")))))

(defun usage-meter-seg (state &aux (usage (and (hash-table-p state) (gethash "usage" state)))
                                   (text (and usage (usage-meter-text usage))))
  "The token-meter segment for the turn in the bar — its rounds so far,
summed — or NIL when no tokens exist."
  ;; The running bar's live meter (the Zig appendTranscriptStatusTokenMeter
  ;; shape), and the only lane a turn's own counts sit in: the resting divider
  ;; names the lane, the outcome and the clock instead of repeating them, and
  ;; the header's right lane carries the session's total, with room to spell
  ;; its long-form notes.
  ;;
  ;; The text is USAGE-METER-TEXT, the one spelling of a turn's counts, so the
  ;; bar and /context can never disagree about the numbers.
  (when text
    (make-seg :text text :tone :muted :dim t)))

(defun live-usage-meter-segs (model)
  "Developer's live token meter, plus context-pressure warnings in either viewport."
  ;; The meter is the turn's own accumulator (its rounds so far, summed); the
  ;; pressure clause is the newest round's occupancy, because the window holds
  ;; one prompt at a time, not the sum of every prompt the loop resent. NIL
  ;; before any usage exists. Standard hides the numeric meter, not the
  ;; actionable pressure warning or the retained usage read by /context. The
  ;; usage is the shown turn's (BAR-LANE), a previewed one's under a dialog.
  (let* ((state (bar-lane model))
         (usage (and (hash-table-p state) (gethash "usage" state)))
         (context (and (hash-table-p state) (gethash "context_usage" state))))
    (when usage
      (let* ((meter (and (eq :developer (chat-model-tool-detail model))
                         (usage-meter-seg state)))
             ;; Context pressure earns a bar slot only once it predicts an
             ;; imminent eviction stall; danger escalates the tone.
             (used (context-occupancy-tokens context))
             (window (and (getf context :model)
                          (ignore-errors
                            (model-context-tokens (getf context :model)))))
             (percent (and window (plusp used)
                           (if (zerop window)
                               100
                               (min 999 (floor (* used 100) window))))))
        (append (and meter (list meter))
                (when (and percent (>= percent 75)) ; Zig ctx warn 75, danger 90
                  (let ((danger (>= percent 90)))
                    (list (make-seg :text " · ")
                          (make-seg :text (format nil "ctx ~d%" percent)
                                    :tone (if danger :danger :muted) :dim (not danger))))))))))

(defun model-names-provider-p (provider model-id &aux (end (length provider)))
  "Whether MODEL-ID starts with PROVIDER's name and a separator --
`deepseek-flash' on deepseek -- so a label that names both says it once."
  (and (plusp end) (< end (length model-id))
       (string-equal provider model-id :end2 end)
       (find (char model-id end) "-/._:")
       t))

(defun status-identity-label (provider model-id)
  "The status bar's provider/model label (Zig
appendTranscriptStatusPlanModelLabel): both absent contributes nothing
rather than the literal word \"unknown\"; a provider that is empty,
\"unknown\", already spelled inside a slashed model id, or leading the model
id (MODEL-NAMES-PROVIDER-P) collapses to the model alone; otherwise
`provider/model'."
  ;; The local is not named TRIMMED: NODECODE.KERNEL exports a TRIMMED of
  ;; its own, and an image whose package locks are on refuses that binding —
  ;; the shipped file compiles under the build's own policy, but a live
  ;; re-evaluation of this definition (an edit made live, a layer override)
  ;; compiled it with an error stored in the function, so every call raised
  ;; "Execution of a form compiled with errors" and the live turn bar lost
  ;; its model label. The name is the fix; the body is unchanged.
  (flet ((trim-field (value)
           (json-value (and (stringp value) (string-trim '(#\Space #\Tab) value))
                       :text)))
    (let ((provider (trim-field provider))
          (model-id (trim-field model-id)))
      (cond ((null model-id) provider)
            ((or (null provider)
                 (string-equal provider "unknown")
                 (find #\/ model-id)
                 (model-names-provider-p provider model-id))
             model-id)
            (t (concatenate 'string provider "/" model-id))))))

(defun status-identity-with-effort (provider model-id effort)
  "STATUS-IDENTITY-LABEL plus the ` (effort)` suffix the turn will bind —
the live/resting bar's identity, resolved through the same
FOOTER-REASONING-EFFORT seam the footer reads, so the two lanes can never
name different efforts for one target."
  (let ((label (status-identity-label provider model-id)))
    (and label (format nil "~a~@[ (~a)~]" label (json-value effort :text)))))

(defun bar-retry (model)
  "The retry MODEL's live bar counts down (RETRY-STATUS), or NIL: the shown
turn's (BAR-LANE), while it runs unstopped."
  (multiple-value-bind (state in-flight started activity own) (bar-lane model)
    (declare (ignore started activity))
    (and in-flight
         (not (and own (turn-stopping-p model)))
         (hash-table-p state)
         (gethash "retry_status" state))))

(defun retry-reason-lines (model width)
  "What the provider said of the retry the live bar counts, whole and with its
own line breaks, as status-block rows on the bar's words' column at WIDTH
=> (values PAINTED-LIST PLAIN-LIST), NIL when no retry stands or it said
nothing."
  ;; Wrapped, never cut: the bar's one row cut the advice off the 503 that
  ;; carried it (always output the error literally).
  (when-let (reason (when-let (retry (bar-retry model)) (nth-value 2 (retry-status-text retry))))
    (llines-rows (mapcar (lambda (line) (make-lline :text line :tone :foreground :role :prose))
                         (split-lines reason))
                 width)))

(defmacro seg (text &optional tone &rest options)
  "A MAKE-SEG of TEXT in TONE, OPTIONS its other keys: the positional spelling
for the many bars and rows that paint one run of text in one tone."
  `(make-seg :text ,text :tone ,tone ,@options))

(defun run-lift (model over &aux (block (find-if (lambda (row) (and (tool-group-p row) (live-run-rows row)))
                                                 (seventh (chat-model-projection-cache model))
                                                 :key #'display-block-row :from-end t)))
  "The canvas rows over the live bar that keep the running turn's run in
MODEL's projection, with the OVER rows between it and the bar, as tall as
they have stood this turn."
  ;; Without this, a round shorter than the one before would leave canvas
  ;; over the bar instead of pulling it up. Counted here, where the canvas
  ;; row over the bar is decided: a transcript ending on canvas takes that
  ;; row's place. A run clicked open, or a call under its shut title, is
  ;; read in full and counts nothing.
  (let ((group (and block (display-block-row block))))
    (if (or (null group) (tool-group-open-p group) (some #'run-member-open-p (tool-group-members group)))
        0
        (let* ((rows (+ over (length (display-block-lines block))))
               (most (max rows (live-run-rows group))))
          (setf (live-run-rows group) most)
          (- most rows)))))

(nlk:define-side-table live-marks (:weakness :key)
  "A model -> (TURN . PLIST): the most its live TURN's block has taken under
each of LIVE-MARK's keys, so the bar and what stands under it hold still.")

(defun live-mark (model turn key value &aux (marks (live-marks model)))
  "The most VALUE has been under KEY in MODEL's live TURN (BAR-LANE's
STARTED), this VALUE included: the rows under the bar, :UNDER."
  (unless (and marks (eql turn (car marks)))
    (setf marks (setf (live-marks model) (list turn))))
  (setf (getf (cdr marks) key) (max value (getf (cdr marks) key 0))))

(defun live-status-line (model width)
  "The live turn bar (Zig transcriptStatusSummaryForKind .started): spinner
mark, the verb -- a tool's own, `evaluating', its line saying what the call does --, the elapsed clock, how long a quiet stream has waited, the
live token meter, the context-pressure clause, then esc on a filled cap: the
key that stops the turn, on the line it stops. The turn is the shown
transcript's (BAR-LANE): a previewed session's under a dialog, painted as
attaching to it would paint it."
  ;; A turn out of flight falls back to the resting interrupted bar when the
  ;; fold left one. Live-only chrome — it never enters the row lanes. Rendered
  ;; through RENDER-ROW so a painting profile carries the canvas to WIDTH.
  (labels ((mark-and-verb (elapsed-ms activity activity-ms tool-activity-p)
             ;; The mark is the verb's own (*STATUS-MARKS*): a stream quiet
             ;; past the threshold — or one that never spoke — reads
             ;; "waiting" under the sway (the Zig
             ;; transcriptStatusActivityStale rule), and every verb the bar
             ;; names wears its kind's mark beside it. A tool in flight
             ;; names itself regardless of stream freshness — tools stream
             ;; nothing while they work, and that silence is exactly when
             ;; the bar should talk. The third value is how long a stream
             ;; that did speak has been quiet: without it, `waiting' alone
             ;; would come on two seconds into any pause, so it could not
             ;; tell a long think from a stall.
             (if (and activity
                      (or tool-activity-p
                          (<= (- elapsed-ms activity-ms) *status-waiting-after-ms*)))
                 (values (status-mark elapsed-ms (second activity)) (first activity) nil)
                 (values (status-mark elapsed-ms :wait) "waiting on the model"
                         (and activity (- elapsed-ms activity-ms)))))
           (quiet-segs (quiet-ms)
             (and quiet-ms
                  (list (make-seg :text (format nil " · quiet ~a" (format-turn-elapsed quiet-ms))
                                  :tone :muted))))
           (elapsed-segs (elapsed-ms)
             ;; After the verb: the bar's first word is what the turn is
             ;; doing, and `11:05' at its head read as a time of day.
             (and elapsed-ms
                  (list (make-seg :text (format nil " · ~a" (format-turn-elapsed elapsed-ms))
                                  :tone :muted))))
           (bar-segs (mark mark-tone head head-tone &rest tails)
             ;; The mark in the gutter where ▸ and ■ stand, set flush against
             ;; the one space before the label lane, since a spinner frame is
             ;; two cells; then in the lane HEAD, then each of TAILS' segs: a run title's shape, so the bar
             ;; reads as the next line of the trail. Its mark sat in the label
             ;; lane and its words on the content rail, nine cells apart.
             (apply #'append
                    (list (make-seg :text (make-string (max 0 (- (nth-value 2 (body-gutter :disclosure))
                                                                 1 (text-display-width mark)))
                                                       :initial-element #\Space))
                          (make-seg :text mark :tone mark-tone)
                          (make-seg :text " "))
                    (list (make-seg :text head :tone head-tone))
                    tails))
           (retry-segs (retry verb-seg elapsed-ms &rest tails)
             ;; A retry in the tool amber, mark and head, then who answered it,
             ;; then the way out; what the provider said stands whole on the
             ;; rows under the bar (RETRY-REASON-LINES). It wore the foreground
             ;; and the plain sway, as any wait does, and a 72-character cut
             ;; left `switch to altern...'.
             (multiple-value-bind (head source) (retry-status-text retry (get-internal-real-time))
               (let ((status (getf retry :status)))
                 (apply #'bar-segs (status-mark elapsed-ms :wait) :tool "" :foreground
                        (list (funcall verb-seg head :tool))
                        (and source (list (make-seg :text " · " :tone :muted)
                                          (make-seg :text source :tone :foreground)))
                        (append tails
                                (list (and status (or (= status 429) (>= status 500))
                                           (list (make-seg :text " · " :tone :muted)
                                                 (hover-seg model "/models to switch"
                                                            (cons :command "models"))))))))))
           (stop-key (segs)
             ;; Esc, the fixed stop key, as a filled cap three columns after
             ;; the bar's own words. Whole or not at all: a bar too narrow
             ;; for it keeps its own words.
             (let ((key (list (make-seg :text "   ")
                              (make-seg :text " esc " :tone :key-chip)
                              (make-seg :text " stop" :tone :muted))))
               (if (<= (+ (segs-display-width segs) (segs-display-width key)) width)
                   (append segs key)
                   segs)))
           (bar-row (segs)
             ;; Attach ranges to the final string, after clipping/narrowing.
             (let ((painted (clip-segs segs width :mark "…")))
               (link-plain-row (nlk:narrow-string (render-row nil painted (canvas-fill-bg) width))
                               (seg-links painted 0)))))
    ;; One bar, whichever session it speaks for: under a dialog previewing
    ;; another session, that session's turn as attaching to it would paint
    ;; it. Without this, the preview would paint a bar of its own, wearing
    ;; the session's name at its head and losing the retry, the meter, the
    ;; resting shape and the tool's silence; the two would drift.
    (multiple-value-bind (state in-flight started activity own) (bar-lane model)
      (if (not in-flight)
          (let ((resting (and (hash-table-p state)
                              (gethash "turn_resting_status" state))))
            (when resting
              (let* (;; The interrupted shape is the resting divider's own: the
                     ;; elapse, the outcome word and the wall clock (in its
                     ;; parentheses) the turn died at, with no meter to repeat
                     ;; what the header's right lane carries.
                     (head (turn-rest-text (or (getf resting :outcome) "failed")
                                           (getf resting :elapsed-ms) (getf resting :ended-at-ms))))
                (bar-row (bar-segs *status-mark-interrupted* :danger head :muted)))))
          (let ((elapsed-ms (if started (since-ms started) 0)))
            (multiple-value-bind (mark verb quiet)
                (mark-and-verb elapsed-ms activity
                               (if (and started activity)
                                   (since-ms started (third activity))
                                   elapsed-ms)
                               ;; A tool call still running as the newest row is the one
                               ;; silence the bar keeps its mark through: a tool streams
                               ;; nothing while it works.
                               (and activity
                                    (hash-table-p state)
                                    (when-let (row (last-tail-row state))
                                      (and (eq :tool-call row.kind)
                                           (equal "running" row.tool-status)))))
              (let* ((stopping (and own (turn-stopping-p model)))
                     (retry (bar-retry model))
                     (words (if stopping "stopping…" verb))
                     (verb-seg (lambda (text tone)
                                 (make-seg :text text :tone tone :link :status-hint
                                           :underline (eq :status-hint (chat-model-hover-target model)))))
                     (elapsed (elapsed-segs (and started elapsed-ms))))
                (bar-row
                 ;; Esc stops the turn only while no dialog holds the keys:
                 ;; under one it closes the dialog, and a previewed turn is
                 ;; never this shell's to stop from there.
                 (funcall (if (or stopping (chat-model-dialog model)) #'identity #'stop-key)
                          (if retry
                              (retry-segs retry verb-seg elapsed-ms elapsed (live-usage-meter-segs model))
                              (bar-segs mark :foreground "" :foreground
                                        (cons (funcall verb-seg words :foreground) elapsed)
                                        (quiet-segs (and (not stopping) quiet))
                                        (live-usage-meter-segs model))))))))))))

;;; --- the background-process band ---------------------------------------------
;;; The status-block band (src/tui/render_plan.zig collectBackgroundProcessLines
;;; + transcript.zig band rows + band_layout.zig): one line per running or
;;; preparing shell, newest first, budgeted by BACKGROUND-BAND-BUDGET, with the
;;; counted overflow row and the frozen-head/marker/live-tail output window.
;;; Geometry (band_layout.zig): lead = 4 spaces, the odometer mark at column 4
;;; (0-based), body text at column 6; the output hangs under the command,
;;; EXEC-OUTPUT-ROWS: the ⌎ hook and the omission's ⋮ on column 6, its lines
;;; two in. Overflow "… ".
;;; Live-only chrome — rows come from STATE-BACKGROUND-ROWS, which replay
;;; never seeds.

(defparameter +band-lead+ "    ")

(defun chrome-row (lead segs bg width)
  "One chrome row behind the LEAD indent => (values PAINTED PLAIN)."
  ;; The plain lane is derived from the very segs that were painted
  ;; (SEGS-TEXT) — clipped to WIDTH first, so both lanes see the same cut —
  ;; instead of being re-concatenated beside them, so it cannot become a
  ;; second authority on what the row says.
  (let ((segs (clip-segs (cons (make-seg :text lead) segs) width
                          :mark "…")))
    (values (render-row '() segs bg width)
            (segs-text segs))))

(defun background-band-mark (row now)
  "The mark a band row's lead carries at NOW (internal time, as every chrome
clock is)."
  ;; Zig's is an activity odometer — it steps when the process speaks — which
  ;; is the honest answer for a row that is still being described, and a
  ;; frozen dot for a command that has said nothing for ten minutes, which is
  ;; exactly the row an operator watches to know the work out of flight is
  ;; still moving. So a RUNNING row's mark is the clock's: the odometer's own
  ;; eight frames, stepped on the turn bar's grid. One column wide either way,
  ;; so the command beside it never shifts. A row that was already live when
  ;; the clock arrived has no stamp: it is born on the frame that first asks,
  ;; so the command an operator is watching starts moving rather than parking.
  (if (eq :running (bg-band-row-status row))
      (let ((born (or (bg-row-born row) (setf (bg-row-born row) now))))
        (aref +background-odometer-frames+
              (mod (floor (since-ms born now) *status-running-frame-ms*)
                   (length +background-odometer-frames+))))
      (background-odometer-mark (bg-band-row-activity-events row))))
(defun band-row-tail-segs (row stop-hovered &optional width)
  "A pending eval's live-only elapsed clock and its preparing/stop state."
  (if (eq :preparing (bg-band-row-status row))
      (list (make-seg :text "  — preparing" :tone :muted :dim t))
      (let* ((now (get-internal-real-time))
             (born (or (bg-row-born row) (setf (bg-row-born row) now))))
        (cons (seg (format nil "~a~a"
                           (cond ((and width (< width 32)) "  ")
                                 ((and width (< width 60)) "  pending · ")
                                 (t "  pending eval · "))
                           (format-turn-elapsed (since-ms born now)))
                     :muted :dim t)
              (cond ((bg-row-stopping row)
                     (list (make-seg :text "  stopping…" :tone :muted :dim t)))
                    ((band-row-stoppable-p row)
                     (list (make-seg :text "  ")
                           (seg "✕" (if stop-hovered :danger :muted) :dim (not stop-hovered)))))))))
(defun band-first-row (row segs room width bg stop-hovered)
  "An exec's first band row: the lead, the mark, SEGS -- the command -- padded
to ROOM columns, then BAND-ROW-TAIL-SEGS. => (values PAINTED PLAIN)."
  (chrome-row +band-lead+
              (append (list (seg (background-band-mark row (get-internal-real-time)) :muted)
                            (make-seg :text " "))
                      segs
                      (list (seg (format nil "~va" (max 0 (- room (segs-display-width segs))) "")))
                      (band-row-tail-segs row stop-hovered width))
              bg width))

(defun band-command-width (row width)
  "The columns a band row's command lines take at WIDTH: all of it but the
lead and mark (6), what ends the first line (BAND-ROW-TAIL-SEGS) and the
last column."
  ;; The ✕'s three columns are kept for a row that has none, so every exec's
  ;; command wraps at one column and a row's ✕ never moves its text.
  (max 4 (- width 6 (max 3 (segs-display-width (band-row-tail-segs row nil width))) 1)))

(defun band-row-expand-key (row)
  "Accordion identity for a band exec: its call's id, the key its card
takes once the exec exits (TOOL-ROW-EXPAND-KEY), so it ends open as it ran;
else the row itself."
  (or (bg-band-row-tool-call-id row) row))

(defun band-exec-row-at (model line &aux (refs (chat-model-line-rows model)))
  "The BG-BAND-ROW whose lines the band paints at transcript LINE, or NIL."
  (let ((row (and (integerp line) (< -1 line (length refs)) (aref refs line))))
    (and (bg-band-row-p row) row)))

(defun band-exec-at (model line)
  "The exec whose lines the band paints at transcript LINE -- its id, what a
click there opens or closes -- or NIL."
  (when-let (row (band-exec-row-at model line))
    (bg-band-row-shell-session-id row)))

(defun band-stop-at (model line column &aux (refs (chat-model-line-rows model)))
  "The exec whose ✕ the band paints at transcript LINE, display COLUMN — its
id — or NIL."
  ;; Every line of an exec's rows refs its BG-BAND-ROW, and the ✕ is its first
  ;; line's last glyph -- never one its command wrote on a line under it; the
  ;; cell either side of it counts, so the press need not be exact.
  (let* ((row (band-exec-row-at model line))
         (plain (and row (band-row-stoppable-p row)
                     (not (and (plusp line) (eq row (aref refs (1- line)))))
                     (aref (chat-model-line-plain model) line)))
         (at (and plain (position #\✕ plain :from-end t))))
    (when (and at (<= (abs (- column (text-display-width (subseq plain 0 at)))) 1))
      (bg-band-row-shell-session-id row))))

(defun exec-output-rows (row width column &key open (expandable t))
  "What exec ROW has written, hung under the words at COLUMN as it streams:
the ⌎ hook on that column and the lines two in, every kept line when OPEN,
else the five-row window, its omission behind ⋮ saying a click opens it
when EXPANDABLE. => (values PAINTED-LIST PLAIN-LIST), NIL before any output."
  ;; Standard hid a running exec's output behind a click;
  ;; the operator wants it streaming right under the command, behind the
  ;; hook a queued prompt hangs under the live line.
  (let ((window (background-output-window row (if open most-positive-fixnum 5)
                                          (if open most-positive-fixnum 5)))
        (bg (canvas-fill-bg))
        (painted '()) (plains '()) (first-p t))
    (flet ((emit (gutter text)
             ;; The gutter undimmed muted -- the zig marker_style -- the body dim.
             (multiple-value-bind (line plain)
                 (chrome-row (make-string (if gutter column (+ column 2)) :initial-element #\Space)
                             (append (and gutter (list (make-seg :text gutter :tone :muted)))
                                     (list (make-seg :text (take-display-prefix text (max 1 (- width column 2)))
                                                     :tone :muted :dim t)))
                             bg width)
               (push line painted)
               (push plain plains))
             (setf first-p nil)))
      (when window
        (let ((omitted (getf window :omitted)) (collapse-p (getf window :collapse-p)))
          (dolist (line (getf window :head)) (emit (and first-p "⌎ ") line))
          ;; A live tail has no head, and its count comes first: the hook
          ;; hangs on it, so it never leaves the command.
          (when (and (plusp omitted) (not collapse-p))
            (emit (if first-p "⌎ " "⋮ ") (cond (open (format nil "+~d line~:p not kept" omitted))
                             (expandable (omission-text omitted))
                             (t (format nil "+~d line~:p" omitted)))))
          (dolist (line (getf window :tail))
            (emit (cond ((and collapse-p (plusp omitted)) "⋮ ") (first-p "⌎ ")) line))))
      (values (nreverse painted) (nreverse plains)))))

(defun background-band-lines (row width)
  "One pending exec's chronological transcript block, with command and retained output.
=> (values PAINTED-LIST PLAIN-LIST REF-LIST). Every line refs ROW, including
its expandable output. No viewport budget may omit a pending call."
  (let* ((open (detail-open-p (band-row-expand-key row)))
         (bg (canvas-fill-bg))
         (stop-hovered (equal *hover-target* (cons :stop (bg-band-row-shell-session-id row))))
         (painted '()) (plains '()) (refs '()))
    (flet ((add (p pl) (push p painted) (push pl plains) (push row refs)))
      ;; The command's lines (COMMAND-ROWS), every one when open, else the
      ;; five-row window: the first behind the lead and the mark, ended by
      ;; BAND-ROW-TAIL-SEGS, the rest under it at the command's column.
      (loop with room = (band-command-width row width)
            for segs in (command-rows (bg-band-row-command row) room :open open)
            for first = t then nil
            do (multiple-value-call #'add (if first
                                              (band-first-row row segs room width bg stop-hovered)
                                              (chrome-row "      " segs bg width))))
      (multiple-value-bind (rows row-plains) (exec-output-rows row width 6 :open open)
        (mapc #'add rows row-plains))
      (values (nreverse painted) (nreverse plains) (nreverse refs)))));;; --- queued steer prompts -----------------------------------------------------
;;; render_plan.zig collectPendingSteerLines: a queued follow-up prompt is
;;; queue state, rendered inside the status block — under the live turn bar
;;; and the background band, never above them as a transcript row. Live-only
;;; chrome: promotion (the same prompt's disposition-"started" fact) clears
;;; the row and the started turn renders it as a real user message.

(defun pending-steer-rows (state)
  "The queued :PENDING-INPUT rows in queue order, or NIL."
  (loop for lane in '("history_rows" "tail_rows")
        append (remove-if-not (lambda (row) (eq :pending-input row.kind))
                              (json-value state :any lane))))

;;; --- the held prompt (the Up peel) -----------------------------------------
;;; Up on a queued prompt takes it back out of the queue and into the
;;; composer: the shell sends the withdrawal, the row leaves the band with
;;; it, and its text sits in the composer for editing. The hold is this
;;; shell's live-only state — one key of the transcript state — and it is
;;; what the withdrawal's answer reads to hand the text back when the
;;; prompt promoted before the withdrawal reached the gateway.

(defun held-prompt (state)
  "This shell's held prompt as (:prompt-id ID :text TEXT), or NIL."
  (and (hash-table-p state) (gethash "held_prompt" state)))

(defun peelable-queued-steer (model)
  "The prompt Up may peel into the composer, or NIL: the newest queued
prompt, a steer or a follow-up, while nothing is held, the turn it waits on
is still running, and the composer is empty — a draft keeps Up's other
meaning (prompt history)."
  (when (and (chat-model-turn-in-flight model)
             (null (held-prompt (chat-model-state model)))
             (zerop (length (editor-value (chat-model-editor model)))))
    (find-if #'transcript-row-prompt-id (last (pending-steer-rows (chat-model-state model))))))

(defun hold-queued-prompt (model row &aux (state (chat-model-state model)) (text row.text))
  "Put ROW's queued prompt into the composer and take the row out of the
band with it — the peel: the prompt is neither queued nor shown once Up
has it."
  ;; Paint first: the composer holding the text and the row gone are what
  ;; the operator sees before the round trip. The withdrawal follows (the
  ;; durable queue loses the seat), and a refusal hands the text back
  ;; (NOTE-WITHDRAW-PROMPT-RESULT).
  (replace-composer-text model text)
  (when (hash-table-p state)
    (setf (gethash "held_prompt" state) (list :prompt-id row.prompt-id
                                              :text text)))
  (clear-pending-input state row.prompt-id text))

(defun release-held-prompt (model)
  "End the hold: the peeled prompt is neither queued nor shown."
  (when (hash-table-p (chat-model-state model)) (remhash "held_prompt" (chat-model-state model))))

(defun prune-pending-steer-rows (state ids)
  "Drop queued rows, steers and follow-ups alike, the newest queue snapshot
no longer names — a withdrawal, usually another shell's."
  ;; A follow-up's admission fact folds as a row a replay rebuilds, so the
  ;; snapshot a subscription is sent after it is what takes a withdrawn one
  ;; back out; a promoted one its started input already cleared.
  (remove-transcript-rows state (lambda (row)
                                  (and (eq :pending-input row.kind)
                                       row.prompt-id
                                       (not (member row.prompt-id ids
                                                    :test #'equal))))))

(defun pending-steer-lines (model width)
  "The queued steer prompts as status-block rows at WIDTH — each prompt's
first line behind the ⌎ hook gutter, continuations at the body column,
capped at five lines with the counted omitted
clause."
  ;; While a turn runs, the row Esc would dispatch carries
  ;; `esc dispatches now' on its first line, and one dim hint line under the
  ;; block names the key that takes the newest prompt back into the composer —
  ;; the pending-input preview's edit hint (codex tui
  ;; bottom_pane/pending_input_preview.rs: `<binding> edit last queued
  ;; message' on its own dim line, never woven into a row). The rows are
  ;; composed inside the transcript band, whose two-column outer inset the
  ;; :STEER-BAND roles account for: the resulting absolute hook/body columns
  ;; are 5/7, under the live line's words.
  ;; => (values PAINTED-LIST PLAIN-LIST), NIL while nothing is queued.
  (let* ((rows (pending-steer-rows (chat-model-state model)))
         ;; Between the turn's end and the promotion there is nothing to cut.
         ;; The row Esc would dispatch, in NLK:DEQUEUE-NEXT-TURN's own
         ;; order: the oldest parked steer when one waits, else the oldest
         ;; prompt.
         (next (and (chat-model-turn-in-flight model)
                    (or (find-if #'transcript-row-steer-p rows) (first rows))))
         (llines
           (append
            (loop for row in rows
                  for all = (or (split-lines (trim-block-text row.text))
                                (list ""))
                  ;; Five lines a prompt at most (Zig max_pending_steer_prompt_lines).
                  for shown = (subseq all 0 (min (length all) 5))
                  for omitted = (- (length all) (length shown))
                  nconc (loop for line in shown
                              for firstp = t then nil
                              for dispatch-p = (and firstp (eq row next))
                              for text = (format nil "~a~:[~;  — esc dispatches now~]"
                                                 line dispatch-p)
                              collect (make-lline
                                       :text text :tone :steer-preview :dim t
                                       :role (if firstp
                                                 :steer-band
                                                 :steer-band-cont)
                                       :gutter-tone :muted
                                       :spans (and dispatch-p
                                                   (list (mspan (+ (length line) 2) (length text)
                                                                :tone :muted :dim t)))))
                  when (plusp omitted)
                    collect (make-lline
                             :text (format nil "[~d lines omitted]" omitted)
                             :tone :muted :dim t :role :steer-band-cont))
            ;; The block's own hint line, last: a dim continuation row, so it
            ;; aligns with the text above it and reads as the block's footer,
            ;; never as another queued prompt. It appears exactly while Up would
            ;; really take the newest prompt back, by PEELABLE-QUEUED-STEER's own
            ;; reading (a draft in the composer keeps Up on prompt history, and
            ;; the hint with it).
            (when (peelable-queued-steer model)
              (list (make-lline :text "up edits the last queued prompt"
                                :tone :steer-preview :dim t
                                :role :steer-band-cont))))))
    (when llines
      (llines-rows llines width))))

;;; --- the home / logo screen -------------------------------------------------
;;; The Zig landing surface (screens.zig + home.zig): an empty idle shell
;;; shows the logo and the tip line in the transcript
;;; band, and the composer placeholder swaps to the gateway-state prompt.
;;; It vanishes the moment any row, notice, turn, dialog, or submit exists —
;;; there is no home flag to fall out of sync, only the predicate, and a
;;; frame whose predicate moved since the band was composed composes it again.

(defparameter +home-meta-label-width+ 14
  "Columns the fact block's label lane owns — the longest label,
`/generations', plus its gutter — so every value in the block starts on one
column.")

;;; The lane is a column, not a fraction of the window: a list of folder names
;;; that re-flowed with the window swept a wide terminal instead of reading as
;;; the block it belongs to. A band too narrow to give 56 gives its room,
;;; floored at 12.
(defparameter +home-meta-value-width+ 56
  "Columns the fact block's value lane owns on every terminal — the
80-column answer the block was drawn around.")

(defun home-logo-start-row (height &optional (extra 0))
  "The absolute terminal row the logo starts at."
  ;; EXTRA is the number of rows the fact block adds under the tip. Six
  ;; content rows plus five of chrome centre the logo, version and tip;
  ;; a growing fact block lifts it rather than crowding the composer.
  (if (<= height 12) 2 (max 2 (floor (- height 11 extra) 2))))

;;; The NC mark glows. Once a launch an ember glint crosses it on the
;;; diagonal (column plus row); a click plays the glint again, and the fifth
;;; click inside two seconds — or the Konami code typed on the home screen —
;;; burns the whole mark, a flicker running through it as it cools. The
;;; colours are the nodecode.ai ramp. The frame clock carries both: the tick
;;; repaints while one plays, and the mark is still otherwise.
(defparameter +home-mark+ '("▄  ▄  ▄▄▄"
                            "█▀▄█ █   "
                            "▀  ▀  ▀▀▀")
  "The NC mark the home screen stands on, one string a row.")

(defparameter +mark-ramp+ '((242 87 15) (250 139 62) (255 171 104) (255 210 171))
  "The glow's colours from coolest to hottest; below the first, the mark's own.")

(defparameter +mark-glow-seconds+ '(:shine 0.55 :burn 1.2)
  "How long each glow the mark plays lasts.")

(defparameter +mark-launch-delay+ 0.15
  "Seconds the mark stands still after launch before the glint sets off.")

(defparameter +mark-shine-half-width+ 3.5
  "Diagonal cells from the glint's centre to where the mark is plain again.")

(defparameter +konami-code+ '(:up :up :down :down :left :right :left :right #\b #\a)
  "The keys that burn the mark from the home screen, in order.")

(defun play-mark-glow (model kind &key launch)
  "Start KIND, :SHINE or :BURN, across the mark: now, or for a LAUNCH, a beat
after it."
  (setf (chat-model-mark-glow model)
        (cons kind (+ (get-internal-real-time)
                      (if launch (round (* +mark-launch-delay+ internal-time-units-per-second)) 0)))))

(defun mark-glow-elapsed (glow)
  "Seconds GLOW has played; negative while it waits."
  (/ (- (get-internal-real-time) (cdr glow)) internal-time-units-per-second))

(defun mark-glowing-p (model &aux (glow (chat-model-mark-glow model)))
  "Whether the mark's glow owes this tick a paint: it is playing on a home
screen that stands, or has just ended and the still mark is owed."
  ;; A launch whose first screen is not home ends it there, and a home that
  ;; comes later plays its own (COMPOSE-TRANSCRIPT-VIEWPORT); a home too
  ;; cramped to paint the mark ends it by the clock.
  (when (consp glow)
    (unless (and (home-screen-p model)
                 (< (mark-glow-elapsed glow) (getf +mark-glow-seconds+ (car glow))))
      (setf (chat-model-mark-glow model) :done))
    t))

(defun mark-glow-heat (model &aux (glow (chat-model-mark-glow model)))
  "The mark's heat this frame as a function of a cell's column and row, 0
its own colour to 1 the ramp's core, or NIL when it is still."
  (when (consp glow)
    (let* ((elapsed (max 0 (mark-glow-elapsed glow)))
           (s (/ elapsed (getf +mark-glow-seconds+ (car glow)))))
      (when (< s 1)
        (ecase (car glow)
          ;; A band crossing the diagonal, eased so it is quickest over the mark.
          (:shine
           (let* ((w +mark-shine-half-width+)
                  (span (+ (length (first +home-mark+)) (length +home-mark+) -2))
                  (centre (+ (- w) (* (/ (- 1 (cos (* pi s))) 2) (+ span w w)))))
             (lambda (x row) (max 0 (- 1 (/ (abs (- (+ x row) centre)) w))))))
          ;; A flare, a hold, a cool; a wave runs down the diagonal throughout.
          (:burn
           (let ((level (cond ((< s 0.1) (/ s 0.1))
                              ((< s 0.4) 1)
                              (t (/ (+ 1 (cos (* pi (/ (- s 0.4) 0.6)))) 2))))
                 (phase (* 6 pi elapsed)))
             (lambda (x row)
               (* level (+ 0.7 (* 0.2 (sin (- phase (/ (+ x row) 1.5))))))))))))))

(defun mark-line-segs (line row heat)
  "LINE, row ROW of the mark, as segs: the title tone, each cell HEAT warms
painted up the ramp."
  (if (null heat)
      (list (make-seg :text line :tone :title))
      (loop with ramp = (cons (visual-style-fg (style-for-tone :title)) +mark-ramp+)
            for ch across line
            for x from 0
            for at = (* (funcall heat x row) (1- (length ramp)))
            collect (make-seg :text (string ch)
                              :tone (if (<= at 0)
                                        :title
                                        (multiple-value-bind (step part) (floor at)
                                          (vstyle :fg (blend-rgb (or (nth (1+ step) ramp) (nth step ramp))
                                                                 (nth step ramp)
                                                                 (round (* 100 part)))
                                                  :bold t)))))))

(defun click-mark (model &aux (now (get-internal-real-time)))
  "A click on the mark: the glint again, or on the fifth inside two seconds,
the burn."
  (let ((clicks (cons now (remove-if (lambda (at) (> (- now at) (* 2 internal-time-units-per-second)))
                                     (chat-model-mark-clicks model)))))
    (setf (chat-model-mark-clicks model) (if (>= (length clicks) 5) nil clicks))
    (play-mark-glow model (if (>= (length clicks) 5) :burn :shine))
    (render-state model)))

(defun konami-key (model key plain &aux (at (chat-model-konami-progress model)))
  "Advance MODEL's run through the Konami code by KEY, PLAIN when no modifier
is held; T when the code spends KEY: its Left and Right on the home screen,
and its last key when that burns the mark."
  ;; Up and Down walk prompt history and back to the empty draft, and the
  ;; `b' types, so those pass through; Left on an empty composer would leave
  ;; the session (LEAVE-SESSION-OP), so the code keeps its own.
  (let ((next (cond ((and plain (eql key (nth at +konami-code+))) (1+ at))
                    ;; Up Up Up still stands two keys in.
                    ((and plain (eq key :up)) (if (= at 2) 2 1))
                    (t 0))))
    (setf (chat-model-konami-progress model) (mod next (length +konami-code+)))
    (cond ((= next (length +konami-code+)) (konami-burn model))
          ((<= 5 next 8) (home-screen-p model)))))

(defun konami-burn (model &aux (editor (chat-model-editor model)))
  "Burn the mark when the code's `b' is all the composer holds and, with it
gone, the home screen stands; T when it did, so the `a' is spent."
  (when (equal (editor-value editor) "b")
    (editor-reset editor)
    (cond ((home-screen-p model)
           (play-mark-glow model :burn)
           (render-state model)
           t)
          (t (editor-insert editor "b") nil))))

;;; --- the fact block -------------------------------------------------------