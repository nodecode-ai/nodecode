;;;; scroll-test.lisp --- U3 gates: transcript scrolling + follow pin.
;;;;
;;;; Claims:
;;;;   1. PgUp scrolls the transcript window up and unpins the follow;
;;;;      re-rendering with the pin off keeps the operator's place.
;;;;   2. Scrolling back to the bottom re-pins; pinned renders snap to the
;;;;      tail.
;;;;   3. The mouse wheel scrolls and manages the pin the same way.
;;;;   4. Submitting re-pins so the reply is visible.
;;;;   5. Scroll keys never leak into the composer.
;;;;   6. The wheel over an open dialog scrolls that dialog's list instead of
;;;;      the transcript behind it, and leaves the highlight on the row the
;;;;      pointer is on.
;;;;
;;;; Loads after ui-test/composer-test: reuses MAKE-FRAME-MODEL and PRESS.

(in-package #:nodecode.test)

(nlk:access (short nle::chat-model) (window nle::band-window))

(defun seed-long-transcript (model rows)
  "A transcript ROWS visual lines tall, then a pinned render."
  ;; One hydrated assistant block carries the body: every block owns a padding
  ;; row above and below it, so the rendered line count is the body plus those
  ;; two.
  (paint-model model
    (nle::ingest-message-fact
     "assistant"
     (format nil "~{message ~d~^~%~}"
             (loop for i below (max 1 (- rows 2)) collect i))))
  model)

(defun transcript-offset (model)
  (nle::band-window-offset model.band))

(defun exchanges (state count)
  "STATE with COUNT numbered question/answer pairs folded in."
  (dotimes (i count state)
    (setf state (exchange state (format nil "question ~d" i) (format nil "answer ~d" i)))))

(defmacro with-long-transcript ((model session-id &optional (rows 40)) &body body)
  "Run BODY with MODEL an 80x24 shell over SESSION-ID whose transcript is
ROWS lines tall and pinned — the preamble every scroll gate opens with."
  `(with-frame-model (,model 80 24 :session-id ,session-id)
     (seed-long-transcript ,model ,rows)
     ,@body))

(defun open-numbered-picker (model count)
  "Open a searchable COUNT-row picker on MODEL, its rows `row-NN' valued by
their index, and answer the dialog."
  (nle::open-picker-dialog
   model "Sessions"
   (loop for i below count
         collect (nle::make-picker-row :label (format nil "row-~2,'0d" i) :value i))
   nil :searchable t)
  model.dialog)

(deftest paging-unpins-and-holds-place (with-long-transcript (model "scroll-page"))
  (is (nle::chat-model-follow-bottom model) "renders start pinned")
  (let ((bottom (transcript-offset model)))
    (is (plusp bottom) "forty rows overflow the band")
    (press model :page-up)
    (is (< (transcript-offset model) bottom) "PgUp moves the window up")
    (is (null model.follow-bottom) "PgUp unpins")
    (let ((held (transcript-offset model)))
      (nle::render-state model)
      (is (= held (transcript-offset model))))
    (loop repeat 10
          until model.follow-bottom
          do (press model :page-down))
    (is (nle::chat-model-follow-bottom model))
    (nle::render-state model)
    (is (= bottom (transcript-offset model)))))

(deftest a-following-window-projects-its-tail-and-reading-up-grows-it
    (with-frame-model (model 80 24 :session-id "scroll-tail" :paint ((exchanges 150))))
  ;; Resuming a long session projected every row before its first frame. A
  ;; window following the tail projects a few windows of it; reading up grows
  ;; the projection under the reader, who keeps their place, and Home
  ;; projects it whole.
  (let* ((window model.band)
         (full (nle::transcript-line-table
                (nle::project-state model.state :width (nle::projection-width model)))))
    (flet ((lead () (- (length full) window.base-count))
           (shown () (+ (nle::band-window-offset window) (- (length full) window.base-count))))
      (is (plusp (lead)) "the following window projects only its tail")
      (is (equal (nthcdr (lead) full) (subseq window.lines 0 window.base-count)) "the tail is the whole table's own")
      (let ((before (shown)))
        (loop repeat 200
              until (zerop (shown))
              do (press model :page-up)
                 (is (= (shown) (max 0 (- before window.height))) "each page moves one window up")
                 (setf before (shown))))
      (is (zerop (lead)) "reading to the top grew the projection whole")
      (press model :end)
      (nle::render-state model)
      (is (plusp (lead)) "back at the tail, the next render projects the tail again")
      (press model :home)
      (is (and (zerop (lead)) (zerop (nle::band-window-offset window))) "Home is the first row of the whole"))))

(deftest a-growing-projection-takes-in-about-what-it-asked-for
    (with-frame-model (model 80 24 :session-id "scroll-grow" :paint ((exchanges 400))))
  ;; Reading up grows the projection about a window at a time, looked for
  ;; back from where it starts. Without this, looking back from the end
  ;; would about double the projection each step, so reaching the top of a
  ;; long session would lay out every line in one notch.
  (let ((window model.band) (steps '()))
    (loop repeat 2000
          for before = window.base-count
          until (and (zerop (nle::band-window-offset window)) (null (cdr model.projection-start)))
          do (press model :page-up)
             (when (> window.base-count before)
               (push (- window.base-count before) steps)))
    (is (> (length steps) 5) "the projection grew a step at a time")
    ;; No step took in more than a few windows of lines.
    (is (every (lambda (grown) (<= grown (* 4 window.height))) steps))))

(deftest a-turn-of-edits-is-not-one-run-to-take-in-whole
    (with-frame-model (model 80 24 :session-id "tail-edits"))
  ;; A call that joins no group closes the run before it as a prompt does, so
  ;; the tail of one long turn of edits or failures is laid out a call at a
  ;; time.
  (fold-model model (nle::ingest-message-fact "user" "fix everything"))
  (dotimes (i 60)
    (let ((id (format nil "c~d" i)))
      (fold-model model
        (nle::ingest-tool-call-fact id "eval" "{\"form\":\"(sh \\\"cat a.lisp\\\")\"}")
        (nle::ingest-tool-result-fact id "eval" "no such file" :error-p t))))
  (nle::with-render-scope (model)
    (let* ((start (nle::tail-projection-start model.state 80 20))
           (lines (reduce #'+ (nle::project-state model.state :width 80 :start start)
                          :key (lambda (block) (length (nle::display-block-lines block))))))
      (is (eq :tool-call (nle::transcript-row-kind start)) "the tail starts at a call")
      ;; About the lines it asked for, not the whole turn.
      (is (< lines 40)))))

(deftest wheel-scrolls-and-manages-the-pin (with-long-transcript (model "scroll-wheel"))
  (let ((bottom (transcript-offset model)))
    (nle:update model (nlt:make-mouse-wheel-msg :x 5 :y 5 :direction :up))
    (is-shape model (transcript-offset = (- bottom 3)) (.follow-bottom null "wheel-up unpins"))
    (mouse-update model #'nlt:make-mouse-wheel-msg 5 5 :direction :down)
    (is (nle::chat-model-follow-bottom model))))

(deftest submit-repins-the-transcript (with-long-transcript (model "scroll-submit"))
  (press model :page-up)
  (is (null model.follow-bottom) "reading history")
  (type-string model "a question" :enter)
  (is (nle::chat-model-follow-bottom model) "submit re-pins")
  (is (nle::band-window-at-bottom-p
       model.band)))

(deftest scroll-keys-stay-out-of-the-composer (with-long-transcript (model "scroll-leak"))
  (type-string model "draft" :page-up)
  (press model :page-down)
  (nle:update model (nlt:make-mouse-wheel-msg :x 5 :y 5 :direction :up))
  (is (equal "draft" (composer-value model))))

;;; --- input/chrome ladder: scrollbar hit-test and drag ----------------------

(deftest an-open-dialog-panel-swallows-transcript-gestures ()
  ;; The docked panel covers the band's tail AND the composer, so the press
  ;; ladder underneath it must stay unreachable — otherwise a click meant for
  ;; a dialog row grabs the transcript scrollbar hidden behind the panel.
  ;; The guard predates this panel but the panel is what makes it load-bearing:
  ;; it grew the covered region from a short band to twenty rows.
  (with-long-transcript (model "dialog-gestures")
    (let ((offset (transcript-offset model)))
      ;; Same press that arms the thumb with no dialog open (see below).
      (open-numbered-picker model 2)
      (nle:update model (nlt:make-mouse-click-msg :x 78 :y 16 :button :left))
      (is-shape model (.scrollbar-drag null) (.pressed-tool-row null)
        (.selection-anchor (cons 15 77)) (nle::chat-model-dialog is "and the dialog stays open"))
      ;; The panel is the shell's, not the transcript's words: a drag begun on
      ;; it scrolls nothing and marks nothing.
      (nle:update model (nlt:make-mouse-motion-msg :x 78 :y 6 :button :left))
      (is-shape model (transcript-offset = offset) (.selection null) (.selection-dragged is))
      (is (equal "" (or (nle::frame-selection-text model) "")))
      ;; The wash is proved by the cells it changes, not by a blend value: the
      ;; palette is whatever the test before this one left engaged, and the
      ;; tint is a palette fact the selection contract does not own.
      (nlk:bind ((nle::*color-profile* :truecolor)
                 (rows (nle::compose-frame model))
                 (marked (washed-frame model rows))
                 (bare (nlt:parse-styled-string rows 80 24)))
        (is (null (loop for row below 24
                        unless (loop for column below 80
                                     always (equal (cell-bg-at marked column row)
                                                   (cell-bg-at bare column row)))
                          collect row))))
      (mouse-update model #'nlt:make-mouse-release-msg 78 6 :button :left)
      (is (nle::chat-model-dialog model)))))

(deftest dialog-wheel-scrolls-the-list-under-the-pointer ()
  ;; The wheel used to reach past an open dialog and scroll the transcript
  ;; hidden behind its panel — a gesture with no visible effect, on a list the
  ;; mouse could otherwise select from. The panel owns the notch now, and the
  ;; contract is the mouse one: the window moves, the highlight stays on the
  ;; row the pointer is on. A 24-row window docks a 19-row panel under the
  ;; frame head, so the list body is 10 rows at frame rows 9..18 and y=13 is
  ;; body row 4.
  (with-long-transcript (model "dialog-wheel")
    (let* ((parked (transcript-offset model))
           (y 13)
           (dialog (open-numbered-picker model 20)))
      (flet ((wheel (direction &optional (count 1))
               (mouse-update model #'nlt:make-mouse-wheel-msg 5 y :direction direction :count count)
               (list dialog.view-offset dialog.selection)))
        (is (equal '(0 4) (list dialog.view-offset (nle::dialog-hover-index model y))))
        (is (equal '(1 5) (wheel :down)))
        (is (= 5 (nle::dialog-hover-index model y)))
        (is (equal '(4 8) (wheel :down 3)))
        (is (equal '(2 6) (wheel :up 2)))
        (is (= parked (transcript-offset model)))
        (is-table (expected direction) (equal expected (wheel direction 99))
          ('(10 14) :down) ('(10 14) :down) ('(0 4) :up))
        ;; A notch on the panel's header is the modal's too: swallowed.
        (mouse-update model #'nlt:make-mouse-wheel-msg 5 4 :direction :down)
        (is (= parked (transcript-offset model)))
        ;; Above the panel the transcript is still the pointer's own surface.
        (mouse-update model #'nlt:make-mouse-wheel-msg 5 2 :direction :up)
        (is (< (transcript-offset model) parked))))))

(deftest dialog-page-keys-scroll-the-list-not-the-transcript (with-long-transcript (model "dialog-page"))
  ;; PgUp/PgDn/Home/End are the transcript's chords; over an open dialog they
  ;; belong to the dialog's list, and the transcript behind the panel holds
  ;; its place. Closing hands them back: the same key pages the transcript
  ;; again — the focus the operator's next gesture expects.
  ;; A 24-row window docks a 19-row panel whose list body is 10 rows, so a
  ;; page is ten rows of a forty-row list.
  (let ((parked (transcript-offset model)))
    (is (plusp parked) "the transcript overflows the band")
    (let ((dialog (open-numbered-picker model 40)))
      (flet ((state () (list dialog.view-offset dialog.selection)))
        (is (equal '(0 0) (state)) "the dialog opens at its top")
        (loop for (key expected why)
                in '((:page-down (10 10) "PgDn moves the window and the highlight a page")
                     (:page-down (20 20) "a second PgDn moves another page")
                     (:page-down (30 30) "the window stops at the last full page")
                     (:page-down (30 39) "a page past the end seats the highlight on the last row")
                     (:page-down (30 39) "and then moves nothing")
                     (:page-up (20 29) "PgUp moves the window and the highlight back a page")
                     (:end (30 39) "End jumps the list to its last row")
                     (:home (0 0) "Home jumps it back to the first")
                     (:page-up (0 0) "a page above the top moves nothing"))
              do (press model key)
                 (is (equal expected (state)) why))
        (is (= parked (transcript-offset model)) "the transcript behind the panel never moved")
        (is (nle::chat-model-follow-bottom model) "and its follow pin stands")
        (press model :escape)
        (is (null model.dialog) "Esc closes the dialog")
        (press model :page-up)
        (is (< (transcript-offset model) parked) "the same key is the transcript's again once the dialog is gone")
        ;; And the way back: Home/End jump the transcript window as before.
        (press model :home)
        (is (= 0 (transcript-offset model)))
        (press model :end)
        (is (= parked (transcript-offset model)))))))

(deftest scrollbar-press-and-drag-scrolls (with-frame-model (model 80 27 :session-id "scroll-thumb"))
  ;; 40 rows over a 17-row band (27 rows less the shell's ten): max scroll
  ;; 23, thumb rows 10-16.
  (seed-long-transcript model 40)
  (is (= 23 (transcript-offset model)) "the pinned render sits at 23")
  ;; Press on the thumb (band row 12 -> mouse y 16) grabs it.
  (nle:update model (nlt:make-mouse-click-msg :x 80 :y 16 :button :left))
  (is (nle::chat-model-scrollbar-drag model) "the thumb press arms a drag")
  ;; Drag so the thumb top lands at the track top.
  (nle:update model (nlt:make-mouse-motion-msg :x 80 :y 6 :button :left))
  (is (= 0 (transcript-offset model)) "dragging to the top scrolls to 0")
  (is (null model.follow-bottom) "dragging unpins")
  ;; Drag back to the bottom re-sticks the follow.
  (nle:update model (nlt:make-mouse-motion-msg :x 80 :y 17 :button :left))
  (is (= 23 (transcript-offset model)) "the bottom drag lands at max")
  (is (nle::chat-model-follow-bottom model) "reaching the bottom re-pins")
  (mouse-update model #'nlt:make-mouse-release-msg 80 17 :button :left)
  (is (null model.scrollbar-drag))
  ;; A track press jumps the thumb there and enters the same drag; the blank
  ;; column beside the rail takes it too.
  (nle:update model (nlt:make-mouse-click-msg :x 79 :y 4 :button :left))
  (is (nle::chat-model-scrollbar-drag model) "the track press arms too")
  (is (= 0 (transcript-offset model)) "the track press jumps the window")
  (nle:update model (nlt:make-mouse-release-msg :x 79 :y 4 :button :left))
  ;; The pointer on the gutter lights the thumb; off it the thumb dims.
  (nle:update model (nlt:make-mouse-motion-msg :x 80 :y 10))
  (is (nle::rail-hot-p model) "the pointer on the gutter lights the thumb")
  (nle:update model (nlt:make-mouse-motion-msg :x 40 :y 10))
  (is (not (nle::rail-hot-p model)) "off it the thumb dims")
  ;; The gutter owns clicks even when nothing can scroll.
  (let ((short (make-frame-model 80 24 :session-id "scroll-short")))
    (seed-long-transcript short 2)
    (mouse-update short #'nlt:make-mouse-click-msg 79 10 :button :left)
    (is (null short.selection-anchor))))

;;; --- the band window itself ------------------------------------------------
;;; The transcript's scroll model, apart from the shell: rows in, a height,
;;; and an offset that can never leave the content.

(deftest band-window-clamps-its-offset-to-the-content ()
  (let ((window (nle::make-band-window :height 3)))
    (nle::band-window-set-lines window (list "a" "b" "c" "d" "e"))
    (is (= 5 window.count) "the count follows the rows")
    (is (= 0 (nle::band-window-offset window)) "a fresh window starts at the top")
    (is (null (nle::band-window-at-bottom-p window)) "and is not at the bottom")
    (setf (nle::band-window-offset window) 40)
    (is (= 2 (nle::band-window-offset window)))
    (is (nle::band-window-at-bottom-p window) "which is the bottom")
    (is (equal '("c" "d" "e") (nle::band-window-visible window)))
    (nle::band-window-set-lines window (list "a" "b"))
    (is (= 0 (nle::band-window-offset window)) "shorter content pulls the offset back")
    (is (equal '("a" "b") (nle::band-window-visible window)) "and shows every row")
    (is (nle::band-window-at-bottom-p window) "short content is at the bottom")
    (setf window.height 1)
    (is (= 0 (nle::band-window-offset window)) "a height change re-clamps on read")
    (is (equal '("a") (nle::band-window-visible window)) "one row shows")))

(deftest band-window-scrolls-by-rows-and-to-the-ends ()
  (let ((window (nle::make-band-window :height 2)))
    (nle::band-window-set-lines window (list "a" "b" "c" "d" "e" "f"))
    (nle::band-window-scroll window 3)
    (is (= 3 (nle::band-window-offset window)) "a positive delta scrolls down")
    (is (equal '("d" "e") (nle::band-window-visible window)) "showing the rows from there")
    (is-table (step offset) (= offset (progn step (nle::band-window-offset window)))
      ((nle::band-window-scroll window -1) 2 "a negative delta scrolls up")
      ((nle::band-window-scroll window -10) 0 "up past the top stops at the top")
      ((nle::band-window-goto-bottom window) 4 "the bottom is the last full window")
      ((nle::band-window-scroll window 10) 4 "down past the bottom stops there")
      ((nle::band-window-goto-top window) 0 "and the top is zero"))))

(deftest paging-moves-the-window-by-its-height (with-long-transcript (model "scroll-page-size" 60))
  (let* ((window model.band) (height window.height) (bottom (nle::band-window-offset window)))
    (is-table (motion offset) (= offset (progn (apply #'nle::scroll-transcript model 'motion)
                                              (nle::band-window-offset window)))
      ((:page-up) (- bottom height)) ((:line-down 2) (+ (- bottom height) 2))
      ((:page-down) bottom))
    (is (nle::chat-model-follow-bottom model))))

;;; --- the pinned prompt band -------------------------------------------------
;;; The operator's question stays at the top at rest, during streaming and
;;; while reading history. The response scrolls beneath it; short content
;;; leaves canvas below, never above. Paint and pointer/copy map the same
;;; pieces, and Home still reaches the answer's lead.

(defun numbered-body (label count)
  "COUNT lines reading `LABEL n', the body every prompt-band gate scrolls."
  (format nil "~{~a ~d~%~}" (loop for i below count append (list label i))))

(defun band-indices (window)
  "The content index of each visible row of WINDOW, NIL on a row that owns none."
  (loop for row below (length (nle::band-window-visible window))
        collect (nle::band-window-row-index window row)))

(defun band-row-texts (model &aux (plains model.line-plain))
  "MODEL's band, one trimmed plain row per visible row — :SPACER on the pinned
band's separating row, which owns no line."
  ;; The lane a click and a selection read, so a gate proving what the band
  ;; paints proves they see the same.
  ;; A message's first row carries its clock at the right end; the gate reads
  ;; the words, so the clock and the gap before it are cut.
  (flet ((words (text &aux (gap (search "  " text :from-end t))
                           (tail (and gap (string-left-trim " " (subseq text gap)))))
           (if (and tail (find #\: tail)
                    (member (subseq tail (max 0 (- (length tail) 2))) '("AM" "PM") :test #'equal))
               (string-right-trim " " (subseq text 0 gap))
               text)))
    (loop for index in (band-indices model.band)
          collect (if index (words (string-trim " " (aref plains index))) :spacer))))

(defun band-bottom-index (model)
  "The content index of MODEL's last visible band row."
  (car (last (band-indices model.band))))

(defmacro with-prompt-band ((model window session-id) &body body)
  "Run BODY with MODEL an 80x24 shell over SESSION-ID holding two rendered
turns — `old prompt' answered with 40 lines, then `latest prompt' answered
with 20 — and WINDOW its band."
  ;; The band pins over a transcript projected whole: how much of its tail a
  ;; first projection takes in is the tail's own gate.
  `(with-frame-model (,model 80 24 :session-id ,session-id)
     (let ((nle::*tail-projection-screens* 100))
       (paint-model ,model
         (exchange "old prompt" (numbered-body "answer line" 40))
         (exchange "latest prompt" (numbered-body "streamed line" 20)))
       (let ((,window (nle::chat-model-band ,model)))
         ,@body))))

(deftest prompt-band-pins-the-message-over-its-answer ()
  (with-prompt-band (model window "pin-over-answer")
    (nle::scroll-transcript model :line-up)
    (let ((rows (band-row-texts model)))
      (is-values (start band-rows content-start content-rows) (nle::band-window-pinned window)
        (start is "the latest prompt pins once its answer fills the band")
        ((member "latest prompt" (subseq rows 0 band-rows) :test #'equal) is)
        ((<= band-rows nle::+max-pinned-prompt-rows+) is) ((plusp content-rows) is)
        (content-start = (+ (nle::band-window-offset window) band-rows))
        ((eq :spacer (nth band-rows rows)) is "the spacer owns no line")
        (start = (nle::band-window-row-index window 0))
        ((band-bottom-index model) = (- window.count 2))))))

(deftest prompt-band-scrolls-the-frame-by-one-row (with-prompt-band (model window "pin-scroll-row"))
  (let* ((height window.height) (top (nle::band-window-offset window)))
    (is (nle::band-window-pinned window) "the question pins at rest at the bottom")
    (is (= (1- window.count) (band-bottom-index model)))
    (nle::scroll-transcript model :line-up)
    (is (= (1- top) (nle::band-window-offset window)) "one row up")
    (is (nle::band-window-pinned window))
    (is (= (- window.count 2) (band-bottom-index model)) "the answer moved one row")
    (nle::scroll-transcript model :line-up)
    (is (= (+ (nle::band-window-offset window) (- height 2)) (band-bottom-index model)))))
(deftest prompt-band-switches-on-reaching-the-next-question ()
  (with-prompt-band (model window "pin-push")
    ;; The old question pins until the next one shows in the band, lets go
    ;; for the few rows its answer's close takes, and the next one pins on
    ;; reaching the top.
    (let* ((old (first (nle::band-window-prompt-spans window)))
           (latest (second (nle::band-window-prompt-spans window)))
           (last-pinned (loop for offset downfrom (1- (car latest)) to 0
                              do (setf (nle::band-window-offset window) offset)
                              when (eql (car old) (nle::band-window-pinned window))
                                return offset)))
      (is (and last-pinned (<= (- (car latest) last-pinned) 6)) "it lets go only for the answer's close")
      (is (member "old prompt" (subseq (band-row-texts model) 0 3) :test #'equal))
      (setf (nle::band-window-offset window) (car latest))
      (is (member "latest prompt" (subseq (band-row-texts model) 0 3) :test #'equal))
      (is (= 1 (count "latest prompt" (band-row-texts model) :test #'equal)) "no inline duplicate"))))
(deftest prompt-band-pins-a-short-transcript ()
  (with-frame-model (model 80 24 :session-id "pin-short"
                     :paint ((exchange "only prompt" (numbered-body "answer line" 3))))
    (let* ((window model.band) (rows (band-row-texts model)))
      (is (< window.count window.height))
      (is (nle::band-window-pinned window) "the question pins without overflow")
      (is (equal "only prompt" (second rows)))
      (is (member "answer line 0" rows :test #'equal) "the answer lead stays accessible")
      (is (zerop (nle::band-window-top-padding window)))
      (is (null (nle::band-window-row-index window (length rows))) "slack below maps to nothing"))))
(deftest a-new-prompt-keeps-the-viewport ()
  ;; Without this, a prompt or steer lifts itself to the band's top,
  ;; clearing the screen above it, and its live bar docks at the band's
  ;; foot: it lands under what the band showed, the bar under it.
  (with-frame-model (model 80 24 :session-id "keep-viewport")
    (let ((nle::*tail-projection-screens* 100))
      (paint-model model
        (exchange "old prompt" (numbered-body "answer line" 40))
        (nle::ingest-message-fact "user" "new prompt"))
      (let* ((rows (band-row-texts model))
             (close (position "answer line 39" rows :test #'equal))
             ;; A prompt's first row ends on its clock.
             (prompt (position-if (lambda (row) (and (stringp row) (search "new prompt" row))) rows)))
        (is (= (1- (nle::band-window-count model.band)) (band-bottom-index model)) "the band follows its tail")
        (is (search "old prompt" (second rows)) "under the question still pinned over its answer")
        (is (and close prompt (< close prompt)) "the answer's close stays in view, the new prompt under it"))))
  (with-frame-model (model 100 30 :session-id "keep-viewport-short")
    (fold-model model
      (exchange "first prompt" "first answer" :turn-id "t1")
      (nle::ingest-message-fact "user" "second prompt" :turn-id "t2"))
    (mark-turn-in-flight model :render t)
    (let* ((rows (nle::frame-plain-rows model))
           (first (position-if (lambda (row) (search "first prompt" row)) rows :start 3))
           (second (position-if (lambda (row) (search "second prompt" row)) rows :start 3))
           (bar (position-if (lambda (row) (search "waiting" row)) rows)))
      (is (eql 4 first) "a short transcript keeps its first question at the band's top")
      (is (and second (< first second)) "the new one under its answer")
      (is (and bar (< second bar (+ second 4))) "and the live bar right under it"))))

(deftest prompt-band-needs-a-prompt-to-pin ()
  (with-frame-model (model 80 24 :session-id "pin-no-prompt")
    (paint-model model
      (nle::ingest-message-fact "assistant" (numbered-body "answer line" 40)))
    (let ((window model.band))
      (is (> window.count window.height))
      (is-shape window (nle::band-window-prompt-spans null) (nle::band-window-pinned null))
      (is (= (nle::band-window-offset window)
             (nle::band-window-row-index window 0))))))

(deftest prompt-band-keeps-the-question-at-the-top-of-history ()
  (with-prompt-band (model window "pin-top-inline")
    (nle::band-window-goto-top window)
    (is (nle::band-window-pinned window))
    (is (equal "old prompt" (second (band-row-texts model))))
    (is (= 1 (count "old prompt" (band-row-texts model) :test #'equal)))
    (is (member "answer line 0" (band-row-texts model) :test #'equal) "Home reaches the answer lead")))
(deftest prompt-band-maps-a-press-to-the-prompt-it-paints ()
  (with-prompt-band (model window "pin-press")
    (nle::scroll-transcript model :line-up)
    (let* ((band-rows (nth-value 1 (nle::band-window-pinned window)))
           (line (nle::band-window-row-index window 1)))
      (is (plusp band-rows) "pinned")
      (is (nle::transcript-user-prompt-row-p (nle::transcript-item-at-line model line)))
      (is-values (frame-line column) (nle::frame-transcript-position model 10 5)
        (frame-line eql line) ((plusp column) is)))))

(deftest prompt-band-paints-in-the-frame-above-the-window ()
  (with-prompt-band (model window "pin-frame")
    (nle::scroll-transcript model :line-up)
    (let* ((band-rows (nth-value 1 (nle::band-window-pinned window)))
           (frame (nle::compose-frame model))
           (line (row-index-of "latest prompt" frame :required t)))
      ;; The band's rows are the frame rows 3..3+BAND-ROWS-1 — the header owns
      ;; the three above them — so a prompt painted in the band lands there.
      (is (< 3 line (+ 3 band-rows)))
      (is (search "latest prompt" (aref (nle::frame-plain-rows model) line))))))

;;; --- the pin and the answer's lead ------------------------------------------
;;; The question stays visible without making the answer lead unreachable.
;;; Recorded pairs remain excluded: only an operator question governs.

(deftest prompt-band-leaves-an-answers-lead-in-view ()
  (with-frame-model (model 80 24 :session-id "pin-lead"
                     :paint ((exchange "only prompt" (numbered-body "answer line" 30))))
    (let ((window model.band))
      (is (nle::band-window-at-bottom-p window))
      (is (nle::band-window-pinned window) "at rest the question is still there")
      (nle::band-window-goto-top window)
      (let ((rows (band-row-texts model)))
        (is (equal "only prompt" (second rows)))
        (is (member "answer line 0" rows :test #'equal))
        (is (member "answer line 1" rows :test #'equal))))))
(deftest prompt-band-pins-at-every-offset-that-shows-its-answer ()
  ;; A question pinned over only its answer's closing rows and the next
  ;; question read as never answered: it scrolls inline with its answer
  ;; until the next one reaches the top.
  (with-prompt-band (model window "pin-lead-steps")
    (let ((height window.height) (bottom (nle::band-window-max-offset window)) (released 0))
      (loop for offset from 0 to bottom
            do (setf (nle::band-window-offset window) offset)
               (multiple-value-bind (start rows content-start content-rows) (nle::band-window-pinned window)
                 (if (null rows)
                     (progn (incf released)
                            (is (find "latest prompt" (band-row-texts model) :test #'search) "it lets go only where the next question shows")
                            (is (not (find "old prompt" (band-row-texts model) :test #'search)) "its own question scrolled away with its answer"))
                     (progn
                       (is (<= (+ rows 1 content-rows) height))
                       (is (<= 0 content-start (+ content-start content-rows) window.count))
                       (let ((span (find start (nle::band-window-prompt-spans window) :key #'car)))
                         (is (>= content-start (cdr span)) "its inline block is excluded"))
                       (when (> offset (cdr (find start (nle::band-window-prompt-spans window) :key #'car)))
                         (is (= (+ offset height -2) (band-bottom-index model)) "away from its lead the answer moves one row per step"))))))
      (is (<= 1 released 6) "for the few rows where only its answer's close shows"))))
(deftest a-recorded-prompt-never-pins (with-frame-model (model 80 24 :session-id "pin-recorded"))
  ;; Standard hides the entire reflection recap without deleting its facts.
  ;; Only the operator's prompt can govern and pin the answer above it.
  (let ((nle::*tail-projection-screens* 100))
    (setf (nle::chat-model-tool-detail model) :standard)
    (paint-model model
      (exchange "operator prompt" (numbered-body "answer line" 30) :turn-id "t1")
      (nle::ingest-turn-terminal "completed" :turn-id "t1")
      (nle::ingest-message-fact "user" "[experience] recap of the turn above" :turn-id "t2" :recorded t)
      (nle::ingest-message-fact "assistant" (numbered-body "recap line" 30) :turn-id "t2")
      (nle::ingest-turn-terminal "completed" :turn-id "t2" :facts '(:recorder "reflection")))
    (nle::scroll-transcript model :line-up)
    (let* ((window model.band)
           (band-rows (nth-value 1 (nle::band-window-pinned window))))
      (is (not (rows-carry-p "Reflection recap" (coerce model.line-plain 'list))) "Standard draws no recap header")
      (is (not (rows-carry-p "recap line" (coerce model.line-plain 'list))) "nor its answer")
      (is (not (rows-carry-p "[experience] recap of the turn above" (coerce model.line-plain 'list))) "and its question draws no block at all")
      (is (notany (lambda (ref) (and (nle::tool-group-p ref) (eq :recap (first (nle::tool-group-key ref))))) model.line-rows) "nor any recap padding")
      (is (= 1 (length (nle::band-window-prompt-spans window))) "the operator's prompt alone")
      (is band-rows "pinned, a row up from the bottom")
      (is (rows-carry-p "operator prompt" (subseq (band-row-texts model) 0 band-rows))))))

(deftest a-tail-projection-never-starts-inside-a-recap
    (with-frame-model (model 80 24 :session-id "tail-recap"))
  ;; A recap folds from its prompt row (GROUP-ROWS), so a projection that
  ;; started at its answer or its divider would draw the rest of it unfolded.
  (dotimes (i 30)
    (let ((turn (format nil "t~d" i))
          (recap (format nil "r~d" i)))
      (fold-model model
        (exchange (format nil "question ~d" i) (format nil "answer ~d" i) :turn-id turn)
        (nle::ingest-turn-terminal "completed" :turn-id turn)
        (nle::ingest-message-fact "user" "[experience] recap" :turn-id recap :recorded t)
        (nle::ingest-message-fact "assistant" (format nil "recap ~d" i) :turn-id recap)
        (nle::ingest-turn-terminal "completed" :turn-id recap :facts '(:recorder "reflection")))))
  (dolist (mode '(:standard :developer))
    (setf model.tool-detail mode)
    (nle::with-render-scope (model)
      (flet ((inside-recap-p (start)
               (and start (not (nle::reflection-recap-p start))
                    (uiop:string-prefix-p "r" (nle::transcript-row-turn-id start)))))
        (let ((starts (loop for lines from 1 to 60
                            collect (nle::tail-projection-start model.state 80 lines))))
          (when (eq mode :developer)
            (is (some #'nle::reflection-recap-p starts) "Developer may start at a visible recap's prompt"))
          (is (notany #'inside-recap-p starts) "neither viewport starts inside a recap"))))))

(deftest a-tail-chosen-under-a-dialog-fills-the-window-it-closes-to
    (with-frame-model (model 80 24 :session-id "tail-under-dialog"))
  ;; A frame that lands while a dialog is docked renders under it, where the
  ;; band is a row. Without this, its tail would be sized to that row, so the
  ;; window the dialog closes to would draw the recap alone, and Home, at the
  ;; top of so short a projection already, would grow nothing.
  (paint-model model
    (nle::ingest-system-notice "a boot notice")
    (exchange "first prompt" "first answer" :turn-id "t1")
    (nle::ingest-turn-terminal "completed" :turn-id "t1")
    (exchange "stopped prompt" (numbered-body "streamed line" 4) :turn-id "t2")
    (nle::ingest-turn-terminal "cancelled" :turn-id "t2"))
  (let ((window model.band))
    (press model #\?)
    (is (< window.height 3) "the help dialog leaves the band a row or two")
    (paint-model model
      (nle::ingest-message-fact "user" "[experience] recap" :turn-id "r2" :recorded t)
      (nle::ingest-message-fact "assistant" "the recap" :turn-id "r2")
      (nle::ingest-turn-terminal "completed" :turn-id "r2" :facts '(:recorder "reflection")))
    (press model :escape)
    (is (null model.dialog) "Esc closes the dialog")
    (nle::compose-frame model)
    (is (>= window.count window.height) "the window it closes to is full")
    (press model :home)
    (let ((rows (band-row-texts model)))
      (is (rows-carry-p "a boot notice" rows) "Home reaches the boot notice")
      (is (rows-carry-p "first prompt" rows) "and the turn before the stopped one"))))

;;; Serialized paint bytes are not content columns on an inherited canvas.
(deftest inherited-canvas-scrollbar-stays-in-its-column-across-styles-and-profiles ()
  (dolist (nle:*palette* (mapcar #'theme-palette
                           '("Tungstene" "Komorebi" "Hello Kitty Lo-Fi" "Blueprint Daylight")))
    (dolist (nle:*color-profile* '(:truecolor :ansi256 :ansi :none))
      (dolist (width '(8 40 80 120))
        (let ((text-width (nle::transcript-text-width width)))
          (dolist (text '("" "short" "λ 界 café" "a very long styled line at the content boundary"))
            (dolist (fill '(nil :user-block))
              (let* ((model (make-instance 'nle::chat-model :session-id "scroll-column"))
                     (window (nle::make-band-window :height 4))
                     (line (nle::render-row nil
                             (list (nle::make-seg :text text :tone :tool :bold t :link "https://example.org/path"))
                             (or (and fill (not (eq nle:*color-profile* :none)) (nle::visual-style-bg (nle:style-for-tone fill)))
                                 (nle::canvas-fill-bg))
                             text-width)))
                (setf (nle::chat-model-band model) window)
                (nle::band-window-set-lines window (make-list 8 :initial-element line))
                (dolist (offset '(0 4))
                  (setf (nle::band-window-offset window) offset)
                  (dotimes (pass 2)
                    (let* ((rows (nle::transcript-band-rows model width text-width (nle::canvas-fill-bg)))
                           (screen (nlt:parse-styled-string rows width 4)))
                      (multiple-value-bind (top height) (nle:scrollbar-thumb 4 4 8 offset)
                        (loop for row in rows for y from 0
                              for thumb-p = (<= top y (+ top height -1))
                              for gutter = (nlt:screen-buffer-ref screen (- width 1) y)
                              do (is (= width (nle::text-display-width (nle::plain-frame-row row))) "fresh and memoized rows cover the exact frame width")
                                 (is (char= (if thumb-p #\▐ #\Space) (nlt:cell-char gutter)) "the thumb stays in the frame's last column, the canvas around it")
                                 (is (char= #\Space (nlt:cell-char (nlt:screen-buffer-ref screen (- width 2) y))) "one blank column before the rail")
                                 (is (equal (and (nle::canvas-fill-bg) (nle::color-sgr (nle::canvas-fill-bg) nil nle:*color-profile*))
                                            (nlt:cell-bg (nlt:screen-buffer-ref screen 0 y))) "canvas retains its opaque or inherited policy")
                                 (is (equal (nlt:cell-bg (nlt:screen-buffer-ref screen 0 y)) (nlt:cell-bg gutter)) "the rail stands on the canvas")
                                 (unless (or (eq nle:*color-profile* :none) (not thumb-p))
                                   (is (equal (nle::color-sgr (nle::visual-style-fg (nle:style-for-tone :muted)) t nle:*color-profile*)
                                              (nlt:cell-fg gutter)) "the thumb keeps its own theme colors")))))))))))))))

(deftest inherited-canvas-padding-preserves-painted-unicode-and-osc8-links ()
  (let* ((nle:*palette* (theme-palette "Tungstene"))
         (nle:*color-profile* :truecolor)
         (model (make-instance 'nle::chat-model :session-id "scroll-link"))
         (window (nle::make-band-window :height 1))
         (line (nle::render-row nil (list (nle::make-seg :text "λ 界" :tone :tool :link "https://example.org/path")) nil 76)))
    (setf (nle::chat-model-band model) window)
    (nle::band-window-set-lines window (list line))
    (let* ((row (first (nle::transcript-band-rows model 80 76 nil)))
           (screen (nlt:parse-styled-string (list row) 80 1)))
      (is (= 80 (nle::text-display-width (nle::plain-frame-row row))))
      (is (char= #\λ (nlt:cell-char (nlt:screen-buffer-ref screen 2 0))))
      (is (char= #\界 (nlt:cell-char (nlt:screen-buffer-ref screen 4 0))))
      (is (equal "https://example.org/path" (nlt:cell-link (nlt:screen-buffer-ref screen 4 0))))
      (is (null (nlt:cell-link (nlt:screen-buffer-ref screen 79 0))))
      (is (char= #\Space (nlt:cell-char (nlt:screen-buffer-ref screen 79 0))) "a band with nothing to scroll draws no rail"))))

(deftest inherited-canvas-scrollbar-needs-no-frame-guard-repairs-in-either-viewport ()
  (let ((nle:*palette* (theme-palette "Tungstene"))
        (nle:*color-profile* :truecolor))
    (dolist (mode '(:developer :standard))
      (with-frame-model (model 80 30 :session-id "scroll-real-frame" :tool-detail mode)
        (fold-model model
          (nle::ingest-message-fact "assistant" (format nil "**Styled λ 界** ~a~%~{body ~d~^~%~}" "https://example.org/path" (loop for i below 80 collect i))))
        (dolist (offset '(0 40))
          (let ((window (nle::chat-model-band model)))
            (rendering model (setf (nle::band-window-offset window) offset model.follow nil))
            (let* ((nle::*frame-guard-strict* t)
                   (screen (nlt:parse-styled-string (nle::compose-frame model) 80 30)))
              (multiple-value-bind (top height) (nle:scrollbar-thumb window.height window.height window.count (nle::band-window-offset window))
                (dotimes (y window.height)
                  (is (char= (if (<= top y (+ top height -1)) #\▐ #\Space)
                             (nlt:cell-char (nlt:screen-buffer-ref screen 79 (+ 3 y)))) "actual guarded frame has the right gutter geometry"))))))))))

(deftest prompt-band-remains-at-the-top-at-rest-and-through-resize ()
  (with-frame-model (model 80 24 :session-id "pin-always"
                     :paint ((exchange "Keep this question visible" (numbered-body "reply" 2))))
    (dolist (size '((80 24) (40 16) (100 32) (32 10)))
      (nle:update model (nlt:make-window-size-msg :width (first size) :height (second size)))
      (let* ((window model.band)
             (frame (mapcar #'nle::plain-frame-row (nle::compose-frame model))))
        (is (nle::band-window-pinned window) "the prompt pins even without overflow")
        (is (search "Keep this" (nth 4 frame)) "the question stays at the first text row of the viewport")
        (is (= 1 (count-if (lambda (row) (search "Keep this" row))
                          (subseq frame 3 (+ 3 window.height)))) "never duplicated inline")
        (is (= 1 (nle::frame-transcript-position model 3 5)) "the painted prompt and pointer mapping agree")))
    (paint-model model
      (nle::ingest-message-fact "assistant" (numbered-body "stream" 50)))
    (is (nle::band-window-at-bottom-p model.band))
    (is (nle::band-window-pinned model.band) "follow-bottom keeps the question pinned while output grows")
    (is (search "Keep this" (nth 4 (mapcar #'nle::plain-frame-row (nle::compose-frame model)))))))

(deftest pinned-prompt-overflow-is-counted-without-padding ()
  (with-frame-model (model 80 24 :session-id "pin-omitted"
                     :paint ((exchange (numbered-body "prompt" 10) (numbered-body "answer" 30))))
    (let* ((window (nle::chat-model-band model))
           (frame (nle::compose-frame model))
           (marker (row-index-of "[+8 lines omitted]" frame :required t)))
      (is (= 5 marker) "padding, head, omission and tail fit the four-row pin")
      (is (rows-carry-p "prompt 0" (subseq frame 3 7)))
      (is (rows-carry-p "prompt 9" (subseq frame 3 7)) "the last body line follows the marker")
      (is (not (rows-carry-p "prompt 1" (subseq frame 3 7))) "hidden lines are in the middle")
      (is (null (nle::band-window-row-index window 2)) "the omission marker never points at hidden prompt text")
      (let* ((plain (aref (nle::frame-plain-rows model) marker))
             (at (search "[+8 lines omitted]" plain))
             (column (nle::text-display-width (subseq plain 0 at))))
        (setf (nle::chat-model-selection model) (cons (cons marker column) (cons marker (+ column 17)))))
      (is (equal "[+8 lines omitted]" (nle::frame-selection-text model)) "copy uses the painted omission marker")
      (is (rows-carry-p "prompt 9" (coerce (nle::chat-model-line-plain model) 'list)) "the full prompt remains in the projection")
      (nle::band-window-goto-top window)
      (let ((seen nil))
        (loop for offset from 0 to (nle::band-window-max-offset window)
              do (setf (nle::band-window-offset window) offset)
                 (dolist (i (band-indices window)) (when i (pushnew i seen))))
        (let ((span (first (nle::band-window-prompt-spans window))))
          (is (every (lambda (i) (member i seen)) (loop for i from (car span) below (cdr span) collect i)) "scrolling exposes every original prompt row"))))))

(deftest pinned-prompt-fitting-text-has-no-omission (dolist (count '(1 2 3)))
  (with-frame-model (model 80 24 :session-id "pin-fits"
                     :paint ((exchange (numbered-body "fits" count) "short answer")))
    (let ((frame (nle::compose-frame model)))
      (is (not (rows-carry-p "lines omitted" frame)) "dropping only the bottom padding is not omitted text")
      (is (rows-carry-p (format nil "fits ~d" (1- count)) frame) "all fitting prompt lines remain visible"))))

(deftest pinned-prompt-omission-tracks-wrapped-width ()
  (with-frame-model (model 40 24 :session-id "pin-wrap-count")
    (paint-model model
      (exchange (make-string 180 :initial-element #\x) (numbered-body "answer" 30)))
    (dolist (width '(40 80 120))
      (nle:update model (nlt:make-window-size-msg :width width :height 24))
      (let* ((window model.band) (span (first (nle::band-window-prompt-spans window)))
             (body (- (cdr span) (car span) 2))
             (frame (nle::compose-frame model)))
        (is (if (> body 3) (rows-carry-p (format nil "[+~d lines omitted]" (- body 2)) frame) (not (rows-carry-p "lines omitted" frame))) "the omission count follows wrapped display lines after resize")))))

(deftest pinned-prompt-omission-stays-one-painted-row ()
  (with-frame-model (model 20 24 :session-id "pin-narrow"
                     :paint ((exchange (numbered-body "narrow" 10) (numbered-body "answer" 30))))
    (dolist (width '(20 25 30 40 80))
      (dolist (height '(3 5 10 24))
        (nle:update model (nlt:make-window-size-msg :width width :height height))
        (let* ((window (nle::chat-model-band model))
               (visible (nle::band-window-visible window (nle::projection-width model))))
          (is (<= (length visible) (nle::band-window-height window)) "the clipped marker never wraps outside the pin budget")
          (is (= (nle::band-window-height window) (length (nle::transcript-band-rows model width (nle::projection-width model) (nle::canvas-fill-bg)))) "even the tiniest transcript band paints without a negative slack budget"))))))

(deftest pinned-prompt-omission-closes-with-its-own-filled-padding ()
  (dolist (mode '(:standard :developer))
    (with-frame-model (model 80 24 :session-id "pin-compact" :tool-detail mode
                       :paint ((exchange (numbered-body "prompt" 10) (numbered-body "answer" 30))))
      (let* ((window (nle::chat-model-band model))
             (span (first (nle::band-window-prompt-spans window)))
             (screen (nlt:parse-styled-string (nle::compose-frame model) 80 24))
             (fill (nlt:cell-bg (nlt:screen-buffer-ref screen 3 3))))
        (is (eql (1- (cdr span)) (nle::band-window-row-index window 4)) "the pin reuses the original closing pad, not an extra canvas gap")
        (loop for y from 3 to 7 do
          (is (equal fill (nlt:cell-bg (nlt:screen-buffer-ref screen 3 y))) "top pad, body, omission and closing pad form one filled rectangle"))
        (is (null (nle::band-window-row-index window 5)) "the canvas gap outside the prompt owns no pointer target")
        (is (< (cdr span) (nle::band-window-row-index window 6)) "the answer resumes after the prompt's filled pad and shared canvas gap")
        (is (rows-carry-p "prompt 9" (coerce (nle::chat-model-line-plain model) 'list)))
        (is (null (nle::band-window-row-index window 2)) "the marker remains synthetic")))))

(deftest pinned-prompt-omission-hover-paints-the-whole-block ()
  (let ((nle:*palette* (theme-palette "Tungstene"))
        (nle:*color-profile* :truecolor))
    (dolist (mode '(:standard :developer))
      (with-frame-model (model 80 24 :session-id "pin-hover" :tool-detail mode
                         :paint ((exchange (numbered-body "prompt" 10)
                                           (numbered-body "answer" 30))))
        (let* ((prompt (find-if #'nle::transcript-user-prompt-row-p
                               (nle::transcript-rows (nle::chat-model-state model))))
               (normal (nlt:parse-styled-string (nle::compose-frame model) 80 24))
               (normal-bg (nlt:cell-bg (nlt:screen-buffer-ref normal 3 3))))
          (dolist (mouse-y '(5 6 7 8 4))
            (nle:update model (nlt:make-mouse-motion-msg :x 6 :y mouse-y))
            (let* ((hover (nlt:parse-styled-string (nle::compose-frame model) 80 24))
                   (hover-bg (nlt:cell-bg (nlt:screen-buffer-ref hover 3 3))))
              (is (eq prompt (nle::chat-model-hover-row model)) "body, marker and both pads hover the same prompt")
              (is (not (equal normal-bg hover-bg)) "hover enters the user-block wash")
              (loop for y from 3 to 7 do
                (loop for x from 2 below 75 do
                  (is (equal hover-bg (nlt:cell-bg (nlt:screen-buffer-ref hover x y))) "the full visible block, including the marker, hovers edge to edge"))))
            (is (null (nle::frame-transcript-position model 6 6)) "marker hit-testing never addresses hidden source text")
            (nle:update model (nlt:make-mouse-motion-msg :x 1 :y 1))
            (let ((left (nlt:parse-styled-string (nle::compose-frame model) 80 24)))
              (is (null (nle::chat-model-hover-row model)))
              (loop for y from 3 to 7 do
                (is (equal normal-bg (nlt:cell-bg (nlt:screen-buffer-ref left 3 y))) "hover leave restores every row")))))))))

(deftest pinned-prompt-one-row-omission-retains-hover-ownership ()
  (let ((nle:*palette* (theme-palette "Tungstene"))
        (nle:*color-profile* :truecolor))
    (with-frame-model (model 80 24 :session-id "pin-one-row-hover"
                       :paint ((exchange (numbered-body "prompt" 10) "answer lead")))
      (let* ((window model.band) (owner (car (first (nle::band-window-prompt-spans window)))))
        (setf (nle::band-window-height window) 1)
        (nle::band-window-goto-bottom window)
        (let* ((normal (nlt:parse-styled-string (nle::band-window-visible window 73) 73 1))
               (hover (nlt:parse-styled-string (nle::band-window-visible window 73 owner) 73 1)))
          (is (null (nle::band-window-row-index window 0)))
          (is (eql owner (nle::band-window-row-index window 0 t)) "even a body-only pin points hover at the original prompt owner")
          (is (not (equal (nlt:cell-bg (nlt:screen-buffer-ref normal 3 0))
                          (nlt:cell-bg (nlt:screen-buffer-ref hover 3 0)))) "the one-row omission inherits the prompt's hover fill"))))))

(deftest pinned-prompt-tail-uses-the-final-wrapped-body-row (dolist (mode '(:standard :developer)))
  (with-frame-model (model 40 24 :session-id "pin-tail-wrap" :tool-detail mode
                     :paint ((exchange (format nil "HEAD~%~a~%Final wrapped sentence ends with LAST"
                                               (make-string 160 :initial-element #\x))
                                       (numbered-body "answer" 30))))
    (dolist (width '(20 40 80 120))
      (nle:update model (nlt:make-window-size-msg :width width :height 24))
      (let* ((window model.band) (span (first (nle::band-window-prompt-spans window)))
             (tail (- (cdr span) 2))
             (visible (nle::band-window-visible window (nle::projection-width model))))
        (is (eql tail (nle::band-window-row-index window 3)) "tail points at the final wrapped body row, not closing padding")
        (is (equal (nth tail (nle::band-window-lines window)) (nth 3 visible)) "paint retains the exact final styled source line")
        (is (rows-carry-p "LAST" visible) "the final words survive narrow wrapping")
        (is (null (nle::band-window-row-index window 2)) "the middle marker is not a hidden source target")
        (is (eql (car span) (nle::band-window-row-index window 2 t)) "the middle marker retains hover-only prompt ownership")))))

(deftest pinned-prompt-small-bands-count-only-hidden-body-lines ()
  (with-frame-model (model 80 24 :session-id "pin-tail-small"
                     :paint ((exchange (numbered-body "body" 10) (numbered-body "answer" 30))))
    (let* ((window model.band) (span (first (nle::band-window-prompt-spans window))))
      (dolist (height '(1 2 3 4 5 6 10))
        (setf (nle::band-window-height window) height)
        (nle::band-window-goto-bottom window)
        (let* ((visible (nle::band-window-visible window 73))
               (hidden (if (= height 1) 10 (if (< height 4) 9 8))))
          (is (<= (length visible) height))
          (is (rows-carry-p (format nil "[+~d lines omitted]" hidden) visible) "small bands count only hidden body rows")
          (when (>= height 2)
            (is (rows-carry-p "body 9" visible) "every band that fits marker and tail retains the final body line")
            (is (eql (- (cdr span) 2) (nle::band-window-row-index window (1- (min 4 height)))) "the tail keeps its real source mapping")))))))

;;; The four-row cap is for a band the answer fills: a capped pin over a short
;;; answer would leave the band half empty under `[+4 lines omitted]'.
(deftest a-pinned-prompt-spends-spare-rows-before-it-omits ()
  (with-frame-model (model 80 24 :session-id "pin-spare-fits"
                     :paint ((exchange (numbered-body "prompt" 6) (numbered-body "answer" 3))))
    (let ((frame (nle::compose-frame model)))
      (is (not (rows-carry-p "lines omitted]" frame)) "a prompt the band has room for is never cut")
      (dotimes (i 6)
        (is (rows-carry-p (format nil "prompt ~d" i) frame)))))
  (with-frame-model (model 80 24 :session-id "pin-spare-long"
                     :paint ((exchange (numbered-body "prompt" 40) (numbered-body "answer" 3))))
    (let* ((window (nle::chat-model-band model))
           (visible (nle::band-window-visible window (nle::projection-width model))))
      (is (nle::band-window-at-bottom-p window))
      (is (= (nle::band-window-height window) (length visible)) "the pin takes every row the answer leaves")
      (is (rows-carry-p "prompt 1" visible) "the spare rows go to the prompt's head")
      (is (rows-carry-p "prompt 39" visible) "its last line still closes it")
      (is (rows-carry-p "lines omitted]" visible) "only what does not fit is counted out")
      (is (rows-carry-p "answer 2" visible) "and the whole answer shows under it"))))

;;; A prompt has one visible representation: pinned summary OR original block.
;;; Without this, a prompt pinned over nothing of its answer but the blank
;;; rows closing it, with the next prompt below, would read as a question
;;; never answered (`wrap up').
(deftest a-pinned-prompt-lets-go-once-its-answer-has-gone-under-it ()
  (with-frame-model (model 80 24 :session-id "pin-passed"
                     :paint ((exchange "wrap up" (numbered-body "answer" 12) :turn-id "t1")
                             (exchange "next question" (numbered-body "reply" 30) :turn-id "t2")))
    (let* ((window model.band) (spans (nle::band-window-prompt-spans window)) (released 0))
      (loop for top from (car (first spans)) below (car (second spans))
            do (setf (nle::band-window-offset window) top)
               (let ((frame (nle::compose-frame model)))
                 (if (nle::band-window-pinned window)
                     (is (rows-carry-p "answer" frame) "a pinned prompt stands over some of its answer")
                     (incf released))))
      (is (plusp released) "and lets go once the answer has gone under it"))))

(deftest truncated-prompt-scrolls-inline-without-its-pin ()
  (with-frame-model (model 80 24 :session-id "pin-single-inline")
    (paint-model model
      (exchange (numbered-body "single prompt" 10) (numbered-body "single answer" 30)))
    (let* ((window model.band) (span (first (nle::band-window-prompt-spans window))))
      (is (nle::band-window-pinned window) "bottom retains the capped head/marker/tail")
      (setf (nle::band-window-offset window) (car span))
      (is (null (nle::band-window-pinned window)) "visible original prompt replaces its summary")
      (let ((frame (nle::compose-frame model)))
        (is (= 1 (count-if (lambda (row) (search "single prompt 9" row)) frame)) "the final line is never painted twice")
        (is (not (rows-carry-p "lines omitted]" frame)) "inline prompt has no synthetic summary")
        (dotimes (i 10)
          (is (rows-carry-p (format nil "single prompt ~d" i) frame) "every original body row is visible inline"))))))

(deftest truncated-prompt-has-one-source-run-at-every-offset (dolist (body-count '(4 10 40)))
  (dolist (answer-count '(1 30))
    (with-frame-model (model 80 24 :session-id "pin-single-sweep")
      (let ((nle::*tail-projection-screens* 100))
        (paint-model model
          (exchange "earlier single prompt" (numbered-body "earlier single answer" 20))
          (exchange (numbered-body "swept prompt" body-count) (numbered-body "swept answer" answer-count))))
      (let* ((window model.band) (latest (car (last (nle::band-window-prompt-spans window)))))
        (dolist (height '(1 2 3 6 18 30))
          (setf (nle::band-window-height window) height)
          (let ((seen nil))
            (loop for offset from 0 to (nle::band-window-max-offset window)
                  do (setf (nle::band-window-offset window) offset)
                     (let ((indices (remove nil (band-indices window))))
                       (is (= (length indices) (length (remove-duplicates indices))) "paint and pointer never repeat a source row")
                       (dolist (index indices) (pushnew index seen)))
                     (multiple-value-bind (start rows content-start) (nle::band-window-pinned window)
                       (when rows
                         (let ((span (find-if (lambda (span) (<= (car span) start (1- (cdr span))))
                                              (nle::band-window-prompt-spans window))))
                           (is (>= content-start (cdr span)) "a pin excludes all of its inline owner")))))
            (is (every (lambda (i) (member i seen))
                       (loop for i from (car latest) below (cdr latest) collect i)) "all original prompt rows remain reachable")
            (when (> height 6)
              (is (member (1- (nle::band-window-count window)) seen) "the answer tail remains reachable when the capped pin leaves response room"))))))))

;;; Inline prompts retain the same top anchor as their pinned summaries.
(deftest inline-short-prompt-keeps-canvas-slack-below (dolist (mode '(:standard :developer)))
  (with-frame-model (model 80 40 :session-id "inline-short-anchor" :tool-detail mode
                     :paint ((exchange (numbered-body "inline question" 10) "inline answer")))
    (let* ((window model.band) (span (first (nle::band-window-prompt-spans window))))
      (is (< (nle::band-window-count window) (nle::band-window-height window)) "the fixture has spare canvas")
      (nle::scroll-transcript model :home)
      (is (not (rows-carry-p "lines omitted]" (nle::compose-frame model))) "a band with room shows the whole prompt, never its summary")
      (is (zerop (nle::band-window-top-padding window)) "inline prompts leave spare canvas below, never above")
      (let ((frame (mapcar #'nle::plain-frame-row (nle::compose-frame model))))
        (is (search "inline question 0" (nth 4 frame)) "the first body row follows the top pad, not a canvas gap")
        (is (= 1 (count-if (lambda (row) (search "inline question 9" row)) frame)) "the original final line paints once"))
      (is (eql (1+ (car span)) (nle::frame-transcript-position model 6 5)) "pointer coordinates follow the top-aligned paint")
      (is (null (nle::frame-transcript-position model 6 (+ 4 (length (nle::band-window-visible window))))) "unused canvas below owns no source row"))))

(deftest prompt-band-alignment-is-stable-across-scroll-and-resize ()
  (dolist (mode '(:standard :developer))
    (dolist (body-count '(4 10))
      (with-frame-model (model 80 40 :session-id "inline-anchor-sweep" :tool-detail mode)
        (paint-model model
          (exchange (numbered-body "anchor question" body-count) "anchor answer"))
        (dolist (size '((40 30) (80 40) (112 72)))
          (nle:update model (nlt:make-window-size-msg :width (first size) :height (second size)))
          (let ((window (nle::chat-model-band model)))
            (loop for offset from 0 to (nle::band-window-max-offset window) do
              (setf (nle::band-window-offset window) offset)
              (is (zerop (nle::band-window-top-padding window)) "scrolling or resizing cannot move spare canvas above a prompt")
              (let* ((all (band-indices window)) (indices (remove nil all)))
                (is (= (length indices) (length (remove-duplicates indices))) "alignment never duplicates source paint")
                (loop for index in all for row from 0 do
                  (is (eql index (nle::frame-transcript-position model 6 (+ 4 row))) "every painted row and its pointer share the same anchor"))))))))))

(deftest promptless-short-band-keeps-bottom-alignment ()
  (with-frame-model (model 80 40 :session-id "promptless-short-anchor"
                     :paint ((nle::ingest-message-fact "assistant" "promptless answer")))
    (let* ((window model.band) (slack (- window.height window.count)))
      (is (plusp slack))
      (is (null (nle::band-window-prompt-spans window)))
      (is (= slack (nle::band-window-top-padding window)) "the promptless alignment policy remains unchanged")
      (is (eql 0 (nle::frame-transcript-position model 6 (+ 4 slack))) "promptless paint and pointer retain their existing anchor")
      (is (null (nle::frame-transcript-position model 6 4)) "the promptless canvas above has no source owner"))))
