;;;; mouse.lisp --- the mouse suite, the one selection region, keyboard select mode. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; The Zig runtime.zig handleMouse gestures on the composed frame: scrollbar
;;; hit-test and drag, click-to-expand on tool cards, click-to-restore on
;;; user prompts, hover states, and drag-select with copy-on-release. All
;;; coordinates arrive 1-based from the decoder; the frame stack is the Zig
;;; ChatShell: blank, header, blank, then the transcript band.

(defun transcript-band-metrics (model &aux (window (chat-model-band model))
                                           (vp-height window.height)
                                           (total window.count))
  "=> (values VP-HEIGHT TOTAL Y-OFFSET TOP-PAD TEXT-WIDTH): the composed
frame's transcript band geometry."
  ;; Paint and pointer mapping share the same slack policy: a pinned
  ;; question stays at the top; only a promptless short band anchors below.
  (values vp-height total
          (band-window-offset window)
          (band-window-top-padding window)
          (transcript-text-width (max 20 (chat-model-term-width model)))))
(defun frame-transcript-position (model x y &optional owner-p)
  "Frame mouse X,Y (1-based) to transcript LINE-INDEX and display COLUMN.
Fixed composer status has no transcript owner."
  (bind (((vp-height total _ top-pad text-width) (transcript-band-metrics model))
         (band-row (- y 4))
         (column (- x 3))
         (index (and (<= 0 band-row) (< band-row vp-height) (<= 0 column) (< column text-width)
                     (band-window-row-index (chat-model-band model) (- band-row top-pad) owner-p))))
    (when (and index (<= 0 index) (< index total))
      (values index column))))
(defun transcript-item-at-line (model line-index &aux (refs (chat-model-line-rows model)))
  "What owns visual line LINE-INDEX: a TRANSCRIPT-ROW, a TOOL-GROUP's own
line, or NIL."
  (when (and (integerp line-index) (<= 0 line-index)
             (< line-index (length refs)))
    (let ((ref (aref refs line-index)))
      (and (or (transcript-row-p ref) (tool-group-p ref)) ref))))

(defun togglable-item-p (model item)
  "Whether a click on ITEM opens or closes it: a tool card, a group, or a
thought the detail level folds."
  (or (tool-group-p item)
      (and (transcript-row-p item)
           (or (eq :tool-call item.kind)
               (eq :reasoning item.kind)
               ;; A predicate keeps its Boolean contract across live callers.
               (and (row-notice-summary item) t)))))

(defun restorable-prompt-p (model item)
  "Whether a click on ITEM opens the checkpoint tree on it: an operator
prompt of the attached session, never one a dialog's preview shows."
  (and (transcript-row-p item)
       (eq :text item.kind)
       (equal "user" item.role)
       (equal (transcript-shown-session-id model) (chat-model-session-id model))))

(defun scroll-for-thumb-top (track-height visible-rows total-rows thumb-top
                             &key (round-fn #'floor))
  "The Zig transcriptScrollbarScrollForThumbTop math verbatim: the ceiling
division that exactly inverts SCROLLBAR-THUMB's floor."
  ;;
  ;; ROUND-FN is the thumb rule the caller is inverting — SCROLLBAR-THUMB's own
  ;; two: the transcript's floor and the docked picker's ceiling
  ;; (renderShortcutPickerScrollbar). A drag inverts the geometry of the bar it
  ;; is dragging, or the same track row would land on two different offsets.
  (if (zerop track-height)
      0
      (let ((max-scroll (max 0 (- (max total-rows visible-rows) visible-rows)))
            ;; The thumb this inverts, measured by the one thumb geometry.
            (max-top (- track-height (nth-value 1 (scrollbar-thumb track-height visible-rows
                                                                   total-rows 0
                                                                   :round-fn round-fn)))))
        (cond ((zerop max-scroll) 0)
              ((zerop max-top) (if (plusp thumb-top) max-scroll 0))
              (t (floor (+ (* (min thumb-top max-top) max-scroll)
                           max-top -1)
                        max-top))))))

(defun transcript-rail-at-p (model x y &aux (band-row (- y 4)))
  "Whether frame mouse X, Y (1-based) is on the transcript's scroll gutter:
the frame's last two columns, the rail and the blank beside it, over the
band's rows."
  (and (>= (1- x) (- (max 8 (chat-model-term-width model)) 2))
       (<= 0 band-row) (< band-row (transcript-band-metrics model))))

(defun rail-hot-p (model)
  "Whether the rail's thumb is lit: the pointer rests on the gutter or drags
the thumb."
  (or (chat-model-rail-hover model)
      (eq :transcript (car (chat-model-scrollbar-drag model)))))

(defun apply-scrollbar-track-row (model track-row grab-offset)
  "Move the transcript so the thumb top lands at TRACK-ROW - GRAB-OFFSET."
  ;; Reaching the bottom re-sticks the follow, matching the wheel handoff.
  (let ((vp-height (transcript-band-metrics model))
        (max-scroll (band-window-max-offset (chat-model-band model))))
    (when (plusp max-scroll)
      (let ((target (scroll-for-thumb-top vp-height vp-height
                                          (+ vp-height max-scroll)
                                          (max 0 (- track-row
                                                    grab-offset)))))
        (cond ((>= target max-scroll) (pin-transcript-bottom model))
              (t (clear-transcript-pin model)
                 (setf (band-window-offset (chat-model-band model)) target
                       (chat-model-follow-bottom model) nil)
                 (grow-transcript-projection model)))))))

(defun grab-scrollbar (model surface apply track-row thumb-top thumb-height)
  "Arm a drag of SURFACE's bar pressed at TRACK-ROW: the thumb grabbed where
the pointer landed, or the track jumped through APPLY and the thumb grabbed
at its midpoint (Zig pressTranscriptScrollbar)."
  ;; SURFACE is :transcript or :dialog, the bar the drag leg then moves.
  (let* ((on-thumb (<= thumb-top track-row (+ thumb-top thumb-height -1)))
         (grab (if on-thumb
                   (- track-row thumb-top)
                   (floor thumb-height 2))))
    (setf (chat-model-scrollbar-drag model) (cons surface grab))
    (unless on-thumb
      (funcall apply model track-row grab))))

(defun plain-column-index (text column)
  "The character index in TEXT at display COLUMN (wide characters claim
both their cells)."
  (loop with acc = 0
        for i from 0 below (length text)
        when (>= acc column) return i
        do (incf acc (char-display-width (char text i)))
        finally (return (length text))))

(defun word-char-p (ch)
  "Whether CH belongs to a selectable word: ASCII alphanumerics and `_',
plus every non-ASCII character — the Zig IS-WORD-GRAPHEME contract, so a
Unicode word or an identifier stays whole."
  (or (alphanumericp ch) (char= ch #\_) (>= (char-code ch) 128)))

(defun word-span-at (text index)
  "=> (values START END) character indices of the word run around INDEX in
TEXT, END exclusive, or NIL when INDEX holds a separator or is past the
end — the Zig WORD-DISPLAY-COLUMN-BOUNDS walk."
  (when (and (integerp index) (<= 0 index) (< index (length text))
             (word-char-p (char text index)))
    (values (if-let (before (position-if-not #'word-char-p text
                                             :end index :from-end t))
              (1+ before) 0)
            (or (position-if-not #'word-char-p text :start index)
                (length text)))))

;;; --- the frame: the one selection region ---------------------------------
;;; A mark belongs to the FRAME, not to the transcript. Every row the shell
;;; paints — header, transcript, notice, turn bar, composer, dropdown,
;;; dialog panel, footer — is markable, and a region added later is markable
;;; the day it is painted, because the lane is stamped from the composed
;;; frame itself and nothing here names a region. Coordinates are screen
;;; cells: (ROW . COLUMN), both 0-based, ROW from the frame's top and COLUMN
;;; from the screen's left edge — so no region's inset, wrapped width, or
;;; scroll offset has to be known to mark its text, and a drag that crosses
;;; from the transcript into the composer is one span, not two.
;;;
;;; What the wash covers and what the copy reads are the same lane of the
;;; same frame, by construction: the wash marks the cells the guarded rows of
;;; COMPOSE-FRAME are laid out into (what is painted), and the copy reads the
;;; style-free lane derivable from exactly those rows.
;;;
;;; The lane is model-keyed and weak rather than a CHAT-MODEL slot: the live
;;; image runs its shells out of this file, and a slot added to CHAT-MODEL
;;; with instances alive leaves every one of them obsolete.
(defstruct (frame-lane (:constructor make-frame-lane (rows width height)))
  "One composed frame: ROWS as painted, the WIDTH and HEIGHT it was composed
at (a lane from another geometry is not this frame), and PLAIN — the
style-free lane derived from ROWS on first read, NIL until a gesture asks."
  (rows #() :type simple-vector)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (plain nil))

(nlk:define-side-table frame-lanes (:weakness :key)
  "The last frame each model composed, a FRAME-LANE.")

(defun frame-width (model)
  "The frame's width in columns: the one expression COMPOSE-FRAME paints at,
FRAME-POSITION clamps to, and the wash spans."
  (max 8 (chat-model-term-width model)))

(defun frame-height (model)
  "The frame's height in rows: the one expression COMPOSE-FRAME stacks to and
FRAME-POSITION clamps to."
  (max 4 (chat-model-term-height model)))

(defun transcript-lane (model)
  "=> (values TOP BOTTOM LEFT RIGHT), the frame cells the transcript's words
stand in, 0-based and inclusive: the band's rows, between its two-column
inset and the scroll gutter -- the one region a selection marks, washes and
copies."
  ;; The band opens under the frame head's three rows, the row the pointer
  ;; legs read as band row 0 (Y - 4, 1-based); its columns are COMPOSE-FRAME's.
  ;; The header, the composer, the footer, a docked panel, the inset and the
  ;; rail are the shell's, never the transcript's words ("we should not select
  ;; the TUI elements/artifacts").
  (values 3 (+ 2 (transcript-band-metrics model))
          2 (+ 1 (transcript-text-width (frame-width model)))))

(defun lane-mark (model anchor cell)
  "The mark a drag from ANCHOR to CELL, frame (ROW . COLUMN)s, lays on the
transcript's words: the two cells clamped into TRANSCRIPT-LANE, earlier
first -- a cell above the band its first cell, below it its last -- or NIL
when ANCHOR is off the band's rows, so a drag begun on the shell's chrome
marks nothing."
  (multiple-value-bind (top bottom left right) (transcript-lane model)
    (flet ((clamp (cell &aux (row (car cell)))
             (cond ((< row top) (cons top left))
                   ((> row bottom) (cons bottom right))
                   (t (cons row (max left (min right (cdr cell))))))))
      (when (<= top (car anchor) bottom)
        (let ((from (clamp anchor)) (to (clamp cell)))
          (if (or (< (car from) (car to))
                  (and (= (car from) (car to)) (<= (cdr from) (cdr to))))
              (cons from to)
              (cons to from)))))))

(defun plain-frame-row (row &aux (len (length row)))
  "ROW's painted text with the escape sequences this shell writes removed:
the characters a display column indexes, and what a copy carries."
  (with-output-to-string (out)
    (loop with i = 0
          while (< i len)
          do (let* ((ch (char row i))
                    (end (and (char= ch #\Escape)
                              (own-sequence-end row i len))))
               (cond (end (setf i end))
                     (t (write-char ch out)
                        (incf i)))))))

(defun frame-plain-rows (model &aux (lane (frame-lanes model)))
  "The style-free text of every row of MODEL's frame, one string per screen
row — the selection's hit-test and copy source."
  ;; Built on first read and cached until the next frame; a frame that was
  ;; never painted, or was stamped at another geometry, is composed here,
  ;; because the frame a paint would produce IS the lane.
  (unless (and lane
               (= (frame-lane-width lane) (frame-width model))
               (= (frame-lane-height lane) (frame-height model)))
    (compose-frame model)
    (setf lane (frame-lanes model)))
  (or (frame-lane-plain lane)
      (setf (frame-lane-plain lane)
            (map 'vector #'plain-frame-row (frame-lane-rows lane)))))

(defun frame-position (model x y)
  "Frame mouse X,Y (1-based) as the screen cell (ROW . COLUMN), clamped
to the frame so a drag that leaves the window still marks the nearest cell."
  ;; No region geometry is consulted: this is the selection lane's own
  ;; coordinate system.
  (cons (max 0 (min (1- (frame-height model)) (1- (or y 1))))
        (max 0 (min (1- (frame-width model)) (1- (or x 1))))))

(defun word-selection-at (model row column &aux (plains (frame-plain-rows model)))
  "=> ((ROW . C1) . (ROW . C2)) covering the whole word at display COLUMN of
frame ROW, or NIL when that cell holds a separator, sits past the row, or no
row is there — the Zig TRANSCRIPT-WORD-SELECTION-AT behind the double-click
gesture, over the frame rather than over the transcript alone."
  ;; C2 is inclusive, the convention FRAME-SELECTION-TEXT and
  ;; SELECTION-OVERLAY share. Only the transcript's words select: a word on
  ;; the header, the composer or a panel is the shell's (TRANSCRIPT-LANE).
  (when (and (integerp row) column (< row (length plains))
             (multiple-value-bind (top bottom left right) (transcript-lane model)
               (and (<= top row bottom) (<= left column right))))
    (bind ((text (aref plains row))
           ((start end) (word-span-at text (plain-column-index text column))))
      (when start
        (cons (cons row (text-display-width (subseq text 0 start)))
              (cons row (1- (text-display-width (subseq text 0 end)))))))))

(defun selection-rows-text (rows)
  "ROWS -- (TEXT . EDGE) per marked row, TEXT the words cut at the mark and
EDGE whether the row opens on the lane's edge -- as the copy's text: the
gutter's indent the edge rows share taken off, and joined with newlines."
  ;; The rows that open on the lane's edge open on the gutter's indent too:
  ;; the fewest blanks any of them opens on go, so a code block keeps its own
  ;; indent under the prose around it. A row the mark opens inside keeps its
  ;; words, never blanks.
  (let ((indent (or (loop for (text . edge) in rows
                          when (and edge (plusp (length text)))
                            minimize (or (position #\Space text :test-not #'char=) 0))
                    0)))
    ;; The band's padding rows the mark crossed at either end are not
    ;; words; a blank row between paragraphs is.
    (string-trim '(#\Newline)
                 (format nil "~{~a~^~%~}"
                         (loop for (text . edge) in rows
                               collect (if edge
                                           (subseq text (min indent (length text)))
                                           (string-left-trim " " text)))))))

(defun cut-marked-row (text from to)
  "TEXT, a plain row, cut to display columns FROM..TO (inclusive) and
right-trimmed: the TEXT of a (TEXT . EDGE) row SELECTION-ROWS-TEXT takes."
  ;; PLAIN-COLUMN-INDEX never answers past the text, so both cuts land in it.
  (let ((start (plain-column-index text from))
        (end (plain-column-index text (1+ to))))
    (string-right-trim " " (subseq text start (max start end)))))

(defun frame-mark-rows (model selection &aux (plains (frame-plain-rows model)))
  "SELECTION's rows of MODEL's last frame, from its plain lane, as
SELECTION-ROWS-TEXT takes them: the mark's rows inside TRANSCRIPT-LANE, each
cut to the lane's columns and at the mark's endpoints."
  (when plains
    (multiple-value-bind (top bottom left right) (transcript-lane model)
      (destructuring-bind ((r1 . c1) . (r2 . c2)) selection
        (loop for row from (max top r1) to (min bottom r2 (1- (length plains)))
              for from = (if (= row r1) (max left c1) left)
              collect (cons (cut-marked-row
                             (without-message-clock model row (without-gutter-marks (aref plains row)))
                             from (if (= row r2) (min right c2) right))
                            (= from left)))))))

(defun span-mark-rows (model span &aux (plains (chat-model-line-plain model)))
  "SPAN's rows (MARK-SPAN) off the transcript's own lines, as
SELECTION-ROWS-TEXT takes them: every line between its ends, shown or
scrolled out of the window, cut at the ends' columns."
  (destructuring-bind ((l1 . c1) . (l2 . c2)) span
    (multiple-value-bind (top bottom left right) (transcript-lane model)
      (declare (ignore top bottom))
      (loop with end = (- right left)
            for line from l1 to (min l2 (1- (length plains)))
            for from = (if (= line l1) c1 0)
            collect (cons (cut-marked-row
                           (without-line-clock model line (without-gutter-marks (aref plains line)))
                           from (if (= line l2) (min end c2) end))
                          (zerop from))))))

(defun frame-selection-text (model &aux (selection (chat-model-selection model)))
  "The marked transcript words of MODEL: the mark's rows cut at its
endpoints (the release column inclusive), the gutter's marks and a message's
clock left behind, right-trimmed, the gutter's indent they share taken off,
and joined with newlines. A mark a drag laid is read off the transcript's
own lines (MARK-SPAN), so it holds the lines a scroll took out of the
window; any other off the last frame's plain lane."
  ;; Only the transcript's words copy: the header, the composer, a panel,
  ;; the band's inset and its rail are the shell's.
  (when selection
    (let ((span (mark-span model)))
      (selection-rows-text (if span
                               (span-mark-rows model span)
                               (frame-mark-rows model selection))))))

;;; A drag's mark is held on the transcript's lines, not on the screen: each
;;; end is a POINT -- (OWNER PLACE COLUMN LINE): the row that owns the line
;;; (or the nearest owned line above it), the line's place under that
;;; owner, its lane column, and the line index it had -- so the mark keeps
;;; its words while the window scrolls under it (the drag's own edge scroll,
;;; the wheel, a page key) and while the projection grows rows in above it.
;;; CHAT-MODEL-SELECTION stays the screen mark the gesture laid; the points
;;; ride beside it keyed on that very cons, so a mark replaced or cleared
;;; anywhere drops them with it.
(nlk:define-side-table mark-points (:weakness :key)
  "Weak memo: chat model -> (MARK . (FROM . TO)), the transcript points of
the screen MARK a drag laid.")

(nlk:define-side-table anchor-point (:weakness :key)
  "Weak memo: chat model -> (ANCHOR . POINT), a press's frame cell and the
transcript point it stood on when the drag left it.")

(nlk:define-side-table drag-edge (:weakness :key)
  "Weak memo: chat model -> (X Y . LAST): a held drag's pointer past the
band's first or last row, and when that last scrolled the window.")

(defun transcript-point (model line column &aux (refs (chat-model-line-rows model)))
  "The point at transcript LINE, lane COLUMN: (OWNER PLACE COLUMN LINE)."
  (let* ((owner (loop for i downfrom (min line (1- (length refs))) to 0
                      when (aref refs i) return (aref refs i)))
         (first (and owner (position owner refs))))
    (list owner (- line (or first 0)) column line)))

(defun point-line (model point &aux (refs (chat-model-line-rows model))
                                    (count (length (chat-model-line-plain model))))
  "POINT's transcript line now: its owner's first line plus its place, or
the line it had when the owner is no longer projected."
  (destructuring-bind (owner place column line) point
    (declare (ignore column))
    (let ((first (and owner (position owner refs))))
      (max 0 (min (1- count) (if first (+ first place) line))))))

(defun frame-cell-point (model cell)
  "The transcript point under frame CELL (ROW . COLUMN), clamped into
TRANSCRIPT-LANE: a cell above the band the first shown line's start, one
below it the last shown line's end, and a band row no line stands on
(padding, the spacer, an omission marker) the next line below it from its
start, else the last above it to its end. NIL when no line is shown."
  (multiple-value-bind (top bottom left right) (transcript-lane model)
    (let* ((window (chat-model-band model))
           (top-pad (nth-value 3 (transcript-band-metrics model)))
           (count (length (chat-model-line-plain model)))
           (end (- right left))
           (row (car cell))
           (column (- (max left (min right (cdr cell))) left)))
      (flet ((line-at (r &aux (index (band-window-row-index window (- r top top-pad))))
               (and index (< -1 index count) index)))
        (cond ((< row top) (setf row top column 0))
              ((> row bottom) (setf row bottom column end)))
        (let ((line (line-at row)))
          (unless line
            (loop for r from (1+ row) to bottom
                  when (line-at r) do (setf line (line-at r) column 0) (return))
            (unless line
              (loop for r downfrom (1- row) to top
                    when (line-at r) do (setf line (line-at r) column end) (return))))
          (and line (transcript-point model line column)))))))

(defun mark-span (model &aux (mark (chat-model-selection model))
                             (entry (mark-points model)))
  "=> ((LINE1 . COLUMN1) . (LINE2 . COLUMN2)), MODEL's mark on the
transcript's lines as they stand now, earlier end first, lane columns, the
last inclusive; NIL for a mark no drag laid over the transcript."
  (when (and mark entry (eq mark (car entry)))
    (flet ((at (point) (cons (point-line model point) (third point))))
      (let ((a (at (cadr entry))) (b (at (cddr entry))))
        (if (or (< (car a) (car b)) (and (= (car a) (car b)) (<= (cdr a) (cdr b))))
            (cons a b)
            (cons b a))))))

(defun hold-mark-points (model mark &optional (from (frame-cell-point model (car mark))))
  "Hold MARK, a screen mark on MODEL's frame, as the selection with its
transcript points: FROM (by default its first cell's) to its last cell's."
  (let ((to (frame-cell-point model (cdr mark))))
    (setf (chat-model-selection model) mark)
    (if (and from to)
        (setf (mark-points model) (cons mark (cons from to)))
        (forget-mark-points model))
    mark))

(defun extend-mark (model anchor cell &aux (old (chat-model-selection model))
                                           (before (mark-span model)))
  "Lay the mark a drag from ANCHOR to CELL makes (LANE-MARK) with its
transcript points, the anchor's taken where the drag left it, so a scroll
since cannot move that end. => whether the marked words moved."
  (let ((mark (lane-mark model anchor cell)))
    (when mark
      (let ((from (let ((entry (anchor-point model)))
                    (if (and entry (eq anchor (car entry)))
                        (cdr entry)
                        (cdr (setf (anchor-point model)
                                   (cons anchor (frame-cell-point model anchor))))))))
        ;; The points run anchor to pointer, not first to last: LANE-MARK's
        ;; order is the screen's, and the anchor may have scrolled off it.
        (setf (chat-model-selection model) mark)
        (let ((to (frame-cell-point model cell)))
          (if (and from to)
              (setf (mark-points model) (cons mark (cons from to)))
              (forget-mark-points model)))
        (let ((after (mark-span model)))
          (if after
              (not (equal before after))
              (not (equal old mark))))))))

(defparameter *drag-scroll-period-ms* 50
  "How often a drag held past the transcript band's edge scrolls it, in ms.")

(defun note-drag-edge (model x y)
  "Hold the drag's edge scroll while the pointer at frame X,Y (1-based)
stands above the band's first row or below its last; drop it otherwise."
  (multiple-value-bind (top bottom) (transcript-lane model)
    (let ((row (1- (or y 1))))
      (if (or (< row top) (> row bottom))
          (setf (drag-edge model) (list* x y (cddr (drag-edge model))))
          (forget-drag-edge model)))))

(defun drag-scroll-step (model &aux (held (drag-edge model)))
  "Scroll a drag held past the band's edge one step -- a row for each row
the pointer stands past it, up to three -- at most every
*DRAG-SCROLL-PERIOD-MS*, and extend the mark to the cell under the pointer.
The pointer's motion asks, and so does every tick while it holds still.
=> whether the window moved."
  (cond
    ((null held) nil)
    ((not (and (chat-model-selection-anchor model)
               (chat-model-selection-dragged model)
               (chat-model-selection model)))
     (forget-drag-edge model)
     nil)
    (t
     (destructuring-bind (x y . last) held
       (when (or (null last) (>= (since-ms last) *drag-scroll-period-ms*))
         (multiple-value-bind (top bottom) (transcript-lane model)
           (let* ((row (1- y))
                  (past (cond ((< row top) (- row top)) ((> row bottom) (- row bottom)) (t 0)))
                  (window (chat-model-band model))
                  (before (band-window-offset window)))
             (setf (drag-edge model) (list* x y (get-internal-real-time)))
             (unless (zerop past)
               (scroll-transcript model (if (minusp past) :line-up :line-down) (min 3 (abs past)))
               (unless (= before (band-window-offset window))
                 (extend-mark model (chat-model-selection-anchor model) (frame-position model x y))
                 t)))))))))

;;; The operator wants the selection faint, not solid: at the Zig
;;; design_system value of 55 the wash covered the canvas with a heavy burnt
;;; band. 35 keeps the theme's :selection colour as a light tint — selected
;;; content still keeps its own foreground, like a browser text selection.
(defparameter +selection-overlay-alpha+ 35
  "Opacity (percent) of the transcript selection wash.")

(defun sgr-rgb (sgr &aux (codes (mapcar (lambda (part) (parse-integer part :junk-allowed t))
                                        (uiop:split-string (or sgr "") :separator ";"))))
  "The rgb a cell's SGR colour text names — `48;2;R;G;B', `48;5;N', or a
basic 40-47/100-107 — or NIL for the terminal's default."
  (case (length codes)
    (5 (cddr codes))
    (3 (ansi256-rgb (third codes)))
    (1 (and (first codes)
            (nth (- (first codes) (if (< (first codes) 100) 40 92)) *ansi16-palette*)))))

(defun selection-overlay (model &aux (selection (chat-model-selection model))
                                      (profile (effective-color-profile))
                                      (selection-bg (visual-style-bg (style-for-tone :selection))))
  "The translucent drag-selection wash as the frame's overlay (NLT:MAKE-VIEW
:OVERLAY), or NIL while nothing is marked: every cell of the mark keeps its
glyph and foreground and gets the :selection background blended over its own
at +SELECTION-OVERLAY-ALPHA+ -- the Zig design_system.selectionOverlayStyle
cell loop."
  ;; Span semantics match FRAME-SELECTION-TEXT: screen cells inside
  ;; TRANSCRIPT-LANE, the release column inclusive, an interior row washed
  ;; across the lane's width, never the shell's chrome or the rail. A
  ;; cell with no background uses the canvas the terminal shows as its wash
  ;; ground (CANVAS-RGB), the theme's nominal one while the terminal has not
  ;; said what that is.
  ;; The wash reads the cells the painter laid out, so
  ;; it lands on whatever region painted the row and restyles no row: a
  ;; frame that moves only the mark lays out nothing twice. Colorless
  ;; frames (:none) carry no wash -- there is nothing to blend.
  (when (and selection selection-bg (not (eq profile :none)))
    (let ((canvas-bg (or (canvas-rgb) (visual-style-bg (style-for-tone :selection-ground))))
          ;; The parser hands every cell of one colour the same string, so a
          ;; frame blends each colour it washes once.
          (washes (make-hash-table :test 'eq)))
      (let ((cells (mark-frame-cells model selection)))
        (lambda (buffer &aux (width (nlt:screen-buffer-width buffer)))
          (loop for (row first last) in cells
                when (< row (nlt:screen-buffer-height buffer))
                do (loop for col from (max 0 first) below (min width (1+ last))
                         for cell = (nlt:screen-buffer-ref buffer col row)
                         for under = (nlt:cell-bg cell)
                         do (setf (nlt:cell-bg cell)
                                  (or (gethash under washes)
                                      (setf (gethash under washes)
                                            (color-sgr (blend-rgb selection-bg
                                                                  (or (sgr-rgb under) canvas-bg)
                                                                  +selection-overlay-alpha+)
                                                       nil profile)))))))))))

(defun handle-mouse-wheel (model msg)
  "One wheel notch, routed to what the pointer is on: the overlay under it — an
open dialog's list, the composer dropdown — else the transcript, three rows per
notch (the Zig transcript_mouse_wheel_rows) wherever the pointer sits, or the
/term panel while its takeover owns the band."
  (let* ((y (or msg.y 0))
         (delta (* (if (eq :up (nlt:mouse-wheel-direction msg)) -1 1)
                   (max 1 (or (nlt:mouse-wheel-count msg) 1))))
         (dialog (chat-model-dialog model)))
    (cond
      ;; An open sheet takes the wheel whole: a notch on its window scrolls
      ;; what it shows there, one anywhere else moves nothing behind it.
      ((sheet-dialog-p dialog)
       (multiple-value-bind (column row) (sheet-at model (or msg.x 0) y)
         (when column
           (funcall (sheet-dialog-wheel dialog) model dialog delta column row))))
      ;; A notch on a painted row of the docked panel — the Zig
      ;; pointInDialog test on the active dialog area. Rows are the whole
      ;; test: the panel spans the frame's full width. It paints one row
      ;; fewer than its area spans (its last row is the canvas gap the
      ;; frame owns), which is exactly what DOCKED-DIALOG-FRAME-ROWS stacks.
      ((and dialog
            (multiple-value-bind (panel above) (docked-dialog-frame-rows model)
              (and panel (<= (1+ above) y (+ above (1- panel))))))
       (when (dialog-list-selectable-p dialog)
         (multiple-value-bind (body-row height) (dialog-body-row-at model y)
           ;; The window moves DELTA rows; the selection lands on the row under the pointer.
           (let* ((total (length (picker-visible-rows dialog)))
                  (visible (min total (max 1 (or height (nth-value 1 (dialog-body-band model dialog))
                                                 1))))
                  (offset (max 0 (min (+ dialog.view-offset delta) (max 0 (- total visible))))))
             (when (plusp total)
               (setf dialog.view-offset offset)
               (let* ((row (if body-row
                               (min body-row (1- visible))
                               (max 0 (min (- dialog.selection offset) (1- visible)))))
                      (target (past-headings (picker-visible-rows dialog)
                                             (max 0 (min (+ offset row) (1- total))) 1)))
                 (unless (= target dialog.selection)
                   (setf dialog.selection target)
                   (picker-notify-highlight model dialog :instant t))))))))
      ;; A notch on the dropdown's panel: the last band above the shell
      ;; tail (COMPOSE-FRAME stacks the composer, the dropdown, then the
      ;; footer rows), so it is measured back from the frame's bottom and
      ;; cannot drift with the transcript band.
      ((bind (((rows _ count) (composer-dropdown-geometry model))
              (height (frame-height model))
              (first (and rows (1+ (- height (if (shell-footer-p height) 3 2) rows)))))
         (when (and rows (<= first y (+ first (1- rows))))
           ;; The selection moves DELTA rows, clamped at both ends (the Zig
           ;; scrollComposerSuggestionBy: a scroll stops where :up/:down wrap).
           (setf (chat-model-slash-selection model)
                 (alexandria:clamp (+ (min (chat-model-slash-selection model) (1- count))
                                      delta)
                                   0 (1- count)))
           t)))
      (t (scroll-transcript model (if (minusp delta) :line-up :line-down) (* 3 (abs delta)))))))

;;; Model-keyed because one image can serve several shells, each with its own
;;; model — the Zig keeps the same pair on its runtime
;;; \(transcript_last_left_press_at, transcript_last_left_press_cell).
(nlk:define-side-table left-press-state (:weakness :key)
  "The last left press per chat model, for the double-click test: (LINE
COLUMN INTERNAL-TIME).")

(defun note-left-press (model line column)
  "Record a left press at LINE, COLUMN and answer whether it is the second
click of a double-click — the same cell within 400ms."
  ;; A match consumes the pair, so a triple click starts a fresh cycle (the
  ;; Zig NOTE-TRANSCRIPT-LEFT-PRESS).
  (let* ((now (get-internal-real-time))
         (previous (left-press-state model))
         (doublep (and previous
                       (= (first previous) line)
                       (= (second previous) column)
                       (<= (since-ms (third previous) now) 400)))) ; Zig double_click_timeout_ns
    (if doublep
        (forget-left-press-state model)
        (setf (left-press-state model) (list line column now)))
    doublep))

(defun disarm-pointer (model)
  "Drop every armed pointer gesture: the tool-card, link, ✕ and exec presses,
the selection anchor and its drag flag."
  (setf (chat-model-pressed-tool-row model) nil
        (chat-model-pressed-link model) nil
        (chat-model-pressed-stop model) nil
        (chat-model-pressed-exec model) nil
        (chat-model-selection-anchor model) nil
        (chat-model-selection-dragged model) nil)
  (forget-anchor-point model)
  (forget-drag-edge model))

(defun handle-mouse-press (model msg &aux (x msg.x)
                                          (y msg.y))
  "The Zig press ladder over the frame: an open dialog's panel owns the
press on its rows and the rows under it — the cell its scrollbar paints
takes the grab, anywhere else there the press arms and the release confirms
— and above it, as with no dialog open, the transcript's scrollbar, ✕
arming, tool-card arming, link arming, prompt click-to-restore, and the
selection anchor — a second left click on that cell within 400ms
instead selects the word under the pointer."
  ;; The anchor is a screen cell wherever the press lands, so a press on
  ;; the header, the composer, the footer or a panel that moves is a drag,
  ;; never a click; only one on the transcript's words marks anything
  ;; (LANE-MARK), and the transcript's own affordances stay gated on its lane.
  ;;
  ;; No leg paints: the message that got here is folded in a batch the loop
  ;; paints once, and the frame's overlay washes the mark at that paint.
  ;;
  ;; A left press is the pointer taking the transcript back from the keys:
  ;; the keyboard selection closes, its mark with it, and the press goes on
  ;; to do what it does.
  (when (and (eq :left (nlt:mouse-event-button msg)) (transcript-select model))
    (end-transcript-select model))
  ;; A sheet's window is over everything, the toasts too, and takes the
  ;; pointer whole: a click on it is its own, run on the release, and a press
  ;; anywhere else but a toast does nothing, the shell behind it waiting as
  ;; its keys do.
  (when (and (sheet-dialog-p (chat-model-dialog model)) (sheet-at model x y))
    (return-from handle-mouse-press t))
  ;; A toast is over everything, an open dialog too: a press on one is its
  ;; own — on the × it takes the toast down, anywhere else it does nothing —
  ;; and reaches nothing under it.
  (multiple-value-bind (toast part) (toast-at model x y)
    (when toast
      (when (and (eq part :close) (eq :left msg.button)) (close-toast model toast))
      (return-from handle-mouse-press t)))
  (when (sheet-dialog-p (chat-model-dialog model))
    (return-from handle-mouse-press t))
  ;; The frame above the panel stays the transcript's: a card toggles, a
  ;; target opens, an exec stops while the keys stay the dialog's. No
  ;; panel painted, the dialog keeps the whole frame.
  (when (and (chat-model-dialog model)
             (multiple-value-bind (panel above) (docked-dialog-frame-rows model)
               (or (null panel) (<= (1+ above) y (+ above (1- panel))))))
    ;; A click confirms the row it lands on by replaying Enter, so mouse
    ;; and keyboard share one confirm path (the Zig handleActiveDialogHover
    ;; press leg) — on RELEASE, not here: the press arms the selection
    ;; anchor first, so a drag from a panel row confirms nothing (and marks
    ;; nothing: the panel is the shell's) and only a click confirms.
    (return-from handle-mouse-press
      (when (eq :left (nlt:mouse-event-button msg))
        (if-let (target (frame-target-at-position model x y))
          ;; A tab's chip, a key's hint or a link on the panel arms as a
          ;; target over the transcript does: the release on it with no drag
          ;; between acts (HANDLE-MOUSE-RELEASE), a drag from it confirms nothing.
          (setf (chat-model-pressed-link model) target
                (chat-model-selection-anchor model) (frame-position model x y)
                (chat-model-selection-dragged model) nil)
          (multiple-value-bind (first height total visible) (dialog-scrollbar-metrics model)
            (let ((track-row (and first (- y first))))
              ;; The gutter is the bar's own cell and every column right of it:
              ;; DOCKED-PANEL-ROW paints the bar between its two gaps with the right
              ;; padding outside it, so the bar sits *DOCKED-PANEL-PAD-RIGHT* columns
              ;; in from the frame's right edge — and those columns are the bar and
              ;; panel fill, never a cell carrying a row's own text, so a press
              ;; there takes nothing the pointer could have confirmed.
              (multiple-value-bind (thumb-top thumb-height)
                  (when (and track-row (<= 0 track-row (1- height))
                             (>= x (- (frame-width model) *docked-panel-pad-right*)))
                    (scrollbar-thumb height visible total
                                     (picker-dialog-view-offset (chat-model-dialog model))
                                     :round-fn #'ceiling :fits-nil t))
                (if thumb-height
                    ;; The painted bar owns its own cell: the grab is the gesture,
                    ;; so no anchor arms here and the release confirms nothing.
                    (grab-scrollbar model :dialog #'apply-dialog-scrollbar-track-row
                                    track-row thumb-top thumb-height)
                    ;; The anchor is FRAME-POSITION's (ROW . COLUMN) cell, the
                    ;; shape the drag leg compares whole.
                    (setf (chat-model-selection-anchor model) (frame-position model x y)
                          (chat-model-selection-dragged model) nil)))))))))
  (when (eq :left (nlt:mouse-event-button msg))
    ;; A fresh press always discards a drag whose release was lost.
    (setf (chat-model-scrollbar-drag model) nil)
    (bind (((vp-height total y-offset) (transcript-band-metrics model)) (band-row (- y 4)))
      (cond
        ;; The scroll gutter over the band rows owns the press — still when
        ;; nothing can scroll, so it never falls through into text selection.
        ((transcript-rail-at-p model x y)
         (let ((max-scroll (band-window-max-offset (chat-model-band model))))
           (when (plusp max-scroll)
             (multiple-value-call #'grab-scrollbar model :transcript #'apply-scrollbar-track-row
               band-row (scrollbar-thumb vp-height vp-height (+ vp-height max-scroll) y-offset)))))
        (t
         (disarm-pointer model)
         (setf (chat-model-selection model) nil)
         (bind ((cell (frame-position model x y))
                ((line column) (frame-transcript-position model x y))
                (frame-target (frame-target-at-position model x y))
                (row (and line (transcript-item-at-line model line)))
                (home (and line (home-command-at-position model line column)))
                (link (and line (link-at-position model line column)))
                (stop (and line (band-stop-at model line column)))
                (exec (and line (band-exec-at model line))))
           (macrolet ((arm (place value)
                        `(setf ,place ,value (chat-model-selection-anchor model) cell)))
             (cond
               (frame-target (arm (chat-model-pressed-link model) frame-target))
               ;; A running exec's ✕ arms its interrupt the way a target
               ;; arms its open: the release on it with no drag between
               ;; sends it.
               (stop (arm (chat-model-pressed-stop model) stop))
               ;; Anywhere else on an exec's lines arms its open or close,
               ;; as a press on a card does: open, it shows all it wrote.
               (exec (arm (chat-model-pressed-exec model) exec))
               ;; A target under the pointer owns the press, inside a
               ;; card or outside one: the glyphs it covers are the only
               ;; ones whose click can mean `open this', and the whole
               ;; rest of the row still toggles. It arms like a tool
               ;; card — a release on it with no drag between opens it,
               ;; a drag from it selects — and only the two schemes an
               ;; opener answers for arm at all (OPENABLE-TARGET-P): a
               ;; markdown link's target is the model's to write.
               (home (arm (chat-model-pressed-link model) home))
               ((or (openable-target-p link) (and (consp link) (eq :diff (first link))))
                (arm (chat-model-pressed-link model) link))
               ((and row (togglable-item-p model row))
                ;; Don't toggle yet: arm a pending click so a drag from
                ;; this cell selects text over the block instead. The
                ;; toggle resolves on release.
                (arm (chat-model-pressed-tool-row model) row))
               ((restorable-prompt-p model row)
                ;; Click-to-restore: a press on a user prompt opens the
                ;; checkpoint picker (the Zig
                ;; openCheckpointRestoreDialogForPrompt affordance).
                (when-let (notice (slash-tree-op model))
                  (show-status model notice)))
               (t
                (if-let (word (and (note-left-press model (car cell) (cdr cell))
                                   (word-selection-at model (car cell) (cdr cell))))
                  (setf (chat-model-selection model) word
                        (chat-model-selection-anchor model)
                        (car word)
                        (chat-model-selection-dragged model) t)
                  (setf (chat-model-selection-anchor model) cell)))))))))))

(defun handle-mouse-motion (model msg &aux (x msg.x) (y msg.y) (button msg.button))
  "Drag legs (scrollbar, selection) while the button is held; hover state
on pure motion. => whether anything a frame shows moved."
  ;; Terminals report the pointer at every cell it crosses, and most
  ;; crossings move nothing on screen -- the same mark, the same hovered
  ;; card, the same picker row. Those folds are quiet (UPDATE's third value),
  ;; so the loop paints no frame for them.
  (let ((changed
         (cond
           ((and (chat-model-scrollbar-drag model) (eq button :left))
            ;; One slot, two bars: the surface the grab was taken on decides which
            ;; window the pointer is dragging, and each bar clamps its own track.
            (let ((drag (chat-model-scrollbar-drag model)))
              (if (eq :dialog (car drag))
                  (multiple-value-bind (first height) (dialog-scrollbar-metrics model)
                    (when height
                      (apply-dialog-scrollbar-track-row model (- y first) (cdr drag))))
                  (apply-scrollbar-track-row
                   model (max 0 (min (- y 4) (1- (transcript-band-metrics model)))) (cdr drag))))
            t)
           ((eq button :left)
            (when-let (anchor (chat-model-selection-anchor model))
              (let ((cell (frame-position model x y)))
                ;; A press that has not left its cell is not yet a drag: a
                ;; jittery click stays a click (the tool card still toggles,
                ;; the dialog row still confirms), and the mark appears when
                ;; the pointer actually moves. Clamped, so a drag past the
                ;; frame's edge marks the nearest cell rather than stopping.
                ;; The mark keeps its earlier endpoint first.
                ;; The mark is the transcript's words the drag covers
                ;; (LANE-MARK); one begun on the shell's chrome is still a
                ;; drag, so its release confirms nothing, but marks nothing.
                ;; Held past the band's first or last row, the drag scrolls
                ;; the transcript under it and the mark follows (DRAG-SCROLL-
                ;; STEP; the tick goes on scrolling while the pointer holds
                ;; still), and the mark is held on the transcript's lines, so
                ;; it keeps the rows that scroll out of the window.
                (unless (equal anchor cell)
                  (setf (chat-model-selection-dragged model) t)
                  (note-drag-edge model x y)
                  (let ((moved (extend-mark model anchor cell)))
                    (or (drag-scroll-step model) moved))))))
           ;; Nothing behind an open sheet hovers: its window is over it,
           ;; and the pointer in it chooses what the sheet's HOVER says.
           ((sheet-dialog-p (chat-model-dialog model))
            (let ((sheet (chat-model-dialog model)))
              (and (null button) sheet.hover
                   (multiple-value-bind (column row) (sheet-at model x y)
                     (and column (funcall sheet.hover model sheet column row))))))
           ;; A toast under the pointer holds its five seconds and lifts its ground,
           ;; and nothing under it hovers.
           ((and (null button) (toast-at model x y))
            (hover-toast model (toast-at model x y)))
           ((null button)
            ;; An open picker follows the pointer (the Zig hover_selectable
            ;; dialogs): motion over an occupied body row pulls the selection
            ;; there, and the highlight seam fires — the /themes live preview.
            (let* ((left (hover-toast model nil))
                   (dialog (chat-model-dialog model))
                   (selection (and (picker-dialog-p dialog) dialog.selection)))
              (if (dialog-apply-hover model y)
                  (or (not (eql selection dialog.selection)) left)
                  (multiple-value-bind (line column) (frame-transcript-position model x y)
                    ;; The pointer highlights tool cards, groups, folded thoughts
                    ;; and user prompts, and marks the one target it rests on: the
                    ;; terminal has no cursor to change shape, so the span that would
                    ;; answer a click underlines instead, and a reader can see the
                    ;; gesture before spending it.
                    (let* ((owner-line (or line (frame-transcript-position model x y t)))
                           (item (and owner-line (transcript-item-at-line model owner-line)))
                           (row (and item
                                     (or (togglable-item-p model item)
                                         (restorable-prompt-p model item))
                                     item))
                           (target (or (frame-target-at-position model x y)
                                       (and line
                                        (or (home-command-at-position model line column)
                                            (if-let (stop (band-stop-at model line column))
                                              (cons :stop stop)
                                              (let ((uri (link-at-position model line column)))
                                                (and (openable-target-p uri) uri))))))))
                      (or (unless (and (eq row (chat-model-hover-row model))
                                       (equal target (chat-model-hover-target model)))
                            (setf (chat-model-hover-row model) row
                                  (chat-model-hover-target model) target)
                            (render-state model)
                            t)
                          left)))))))))
    (let ((hover-changed (note-message-timestamp-hover model msg))
          (rail-changed (let ((on (transcript-rail-at-p model x y)))
                          (unless (eq on (chat-model-rail-hover model))
                            (setf (chat-model-rail-hover model) on)
                            ;; Only a painted rail shows the change.
                            (plusp (band-window-max-offset (chat-model-band model)))))))
      (or changed hover-changed rail-changed))))
(defun copy-frame-selection (model &aux (text (frame-selection-text model))
                                        (program (chat-model-program model)))
  "Hand MODEL's marked text to the program's clipboard writer, which never
waits behind the worker (a turn occupies it), and say in the hint lane what
became of it once the writer knows (CLIPBOARD-OUTCOME-NOTICE): copied only
when a clipboard tool took it. With nothing marked it says so, and with no
live program that nothing was written. The release that ends a drag and the
:COPY-SELECTION key both come here."
  (cond ((zerop (length text)) (show-status model +selection-empty-hint+))
        ((null program) (show-status model +selection-copy-failed-hint+ :error-p t))
        (t (nlt:write-clipboard program text
                                (lambda (outcome)
                                  (nlt:send program (clipboard-outcome-notice outcome)))))))

(defun clipboard-outcome-notice (outcome)
  "The composer status a clipboard write's OUTCOME (NLT:WRITE-CLIPBOARD)
says, as the SYSTEM-NOTICE-MSG that lands it on the loop thread: copied for
a clipboard tool, sent for OSC 52 alone, failed for neither."
  (multiple-value-bind (text error-p)
      (cond ((and (consp outcome) (eq :command (first outcome))) +selection-copied-hint+)
            ((eq :osc52 outcome) +selection-osc52-hint+)
            (t (values +selection-copy-failed-hint+ t)))
    (make-instance 'system-notice-msg :text text :error-p error-p :answer t)))

(defun mark-frame-cells (model selection)
  "=> ((ROW FIRST LAST) ...): the frame rows SELECTION washes on MODEL's
frame, each with its first and last column (inclusive), inside
TRANSCRIPT-LANE -- for a drag's mark the shown rows whose lines it holds
(MARK-SPAN), wherever the window has scrolled to."
  (multiple-value-bind (top bottom left right) (transcript-lane model)
    (let ((span (mark-span model)))
      (if span
          (destructuring-bind ((l1 . c1) . (l2 . c2)) span
            (let ((window (chat-model-band model))
                  (top-pad (nth-value 3 (transcript-band-metrics model))))
              (loop for row from top to bottom
                    for line = (band-window-row-index window (- row top top-pad))
                    when (and line (<= l1 line l2))
                      collect (list row
                                    (if (= line l1) (+ left c1) left)
                                    (if (= line l2) (min right (+ left c2)) right)))))
          (destructuring-bind ((r1 . c1) . (r2 . c2)) selection
            (loop for row from (max top r1) to (min bottom r2)
                  collect (list row
                                (if (= row r1) (max left c1) left)
                                (if (= row r2) (min right c2) right))))))))

;;; --- keyboard select mode ------------------------------------------------
;;; The transcript's words mark from the keyboard as well as under a drag
;;; (Codex's transcript select mode, openai/codex#57221): the
;;; :SELECT-TRANSCRIPT chord sets a cursor on the last words the window shows,
;;; and the keys move it -- h j k l and the arrows by a character or a row,
;;; w b e and Ctrl+Left/Right by words, 0 ^ $ Home End to the ends of a
;;; logical line, the page keys and Ctrl+B/F/U/D by pages, g and G to the
;;; transcript's two ends. v (or Space) marks from the cursor, V marks whole
;;; lines, a Shift+arrow marks as it moves, o swaps the mark's ends, and y,
;;; Enter, Ctrl+C or the copy chord copy it and leave; Esc takes a mark away,
;;; then leaves.
;;;
;;; Nothing here paints a mark of its own: the mark is a drag's -- a screen
;;; mark held on transcript points (MARK-POINTS) -- so the wash, the copy and
;;; the scroll that keeps its words are the ones a drag's mark has, and a mark
;;; standing when the mode opens is taken up as its own. The hardware cursor
;;; parks on the cursor's cell while the mode lasts, the composer names the
;;; keys, and every key is the mode's but the quit chord: no letter reaches
;;; the composer, no Enter submits.

(defstruct (select-state (:constructor make-select-state (cursor &key anchor (unit :char))))
  "A keyboard selection over a transcript: CURSOR the transcript point the
keys move (TRANSCRIPT-POINT), ANCHOR the point its mark opened at or NIL
while nothing is marked, UNIT :CHAR or :LINE (a mark of whole logical
lines), and GOAL the display column a run of row motions keeps."
  cursor anchor (unit :char) goal)

(nlk:define-side-table transcript-select (:weakness :key)
  "Weak memo: chat model -> its SELECT-STATE while the keyboard selects the
transcript's words.")

(defun select-line-text (model line &aux (plains (chat-model-line-plain model)))
  "Transcript LINE's words as a copy reads them (SPAN-MARK-ROWS): the
gutter's marks and a message's clock blanked, so its display columns are the
lane columns a mark is cut at. Empty past the transcript's lines."
  (if (< -1 line (length plains))
      (without-line-clock model line (without-gutter-marks (aref plains line)))
      ""))

(defun select-text-bounds (text)
  "=> (values FIRST LAST): the indices of TEXT's first and last character
that is not a blank, or NIL for a blank TEXT."
  (when-let (first (position #\Space text :test-not #'char=))
    (values first (position #\Space text :test-not #'char= :from-end t))))

(defun select-word-char-p (char)
  "Whether CHAR belongs to a word the w, b and e keys step by: a letter or a
digit of any script, or `_' -- a run of punctuation, a rule or a bullet is
passed over, as Codex's word motions pass it."
  (or (alphanumericp char) (char= char #\_)))

(defun select-logical-line (model line &aux (plains (chat-model-line-plain model))
                                            (count (length plains)))
  "=> (values FIRST LAST): the first and last transcript lines of the logical
line LINE is a row of -- the rows a wrapped line goes on in
(PLAIN-ROW-CONTINUES) belong to the line above them."
  (let ((first line) (last line))
    (loop while (and (< 0 first count) (plain-row-continues (aref plains first)))
          do (decf first))
    (loop while (and (< -1 (1+ last) count) (plain-row-continues (aref plains (1+ last))))
          do (incf last))
    (values first last)))

(defun select-point (model line index &aux (text (select-line-text model line)))
  "The transcript point at character INDEX of transcript LINE."
  (transcript-point model line (text-display-width (subseq text 0 (min index (length text))))))

(defun select-cursor-position (model state)
  "=> (values LINE INDEX): STATE's cursor on MODEL's lines as they stand now,
its index on the line's last character at most."
  (let* ((point (select-state-cursor state))
         (line (point-line model point))
         (text (select-line-text model line)))
    (values line (min (plain-column-index text (third point)) (max 0 (1- (length text)))))))

(defun select-step-char (model line index delta)
  "=> (values LINE INDEX) one character from LINE's INDEX, DELTA -1 or 1: on
the line while its words go on that way, else the near end of the next line
that has words; NIL past the last."
  (multiple-value-bind (first last) (select-text-bounds (select-line-text model line))
    (if (and first (if (minusp delta) (> index first) (< index last)))
        (values line (+ index delta))
        (loop for at = (+ line delta) then (+ at delta)
              while (< -1 at (length (chat-model-line-plain model)))
              do (multiple-value-bind (first last) (select-text-bounds (select-line-text model at))
                   (when first
                     (return (values at (if (minusp delta) last first)))))))))

(defun select-word-edge-p (text index edge)
  "Whether character INDEX of TEXT opens (EDGE :START) or closes (:END) a word."
  (and (< -1 index (length text))
       (select-word-char-p (char text index))
       (let ((beside (if (eq edge :start) (1- index) (1+ index))))
         (not (and (< -1 beside (length text)) (select-word-char-p (char text beside)))))))

(defun select-step-word (model line index delta edge)
  "=> (values LINE INDEX) of the next word's EDGE (:START or :END) from
LINE's INDEX going DELTA (1 or -1), across lines -- w is forward to a
start, b back to one, e forward to an end; NIL when no word is left that way."
  (loop for at = line then (+ at delta)
        while (< -1 at (length (chat-model-line-plain model)))
        do (let ((text (select-line-text model at)))
             (loop for i = (cond ((= at line) (+ index delta))
                                 ((plusp delta) 0)
                                 (t (1- (length text))))
                     then (+ i delta)
                   while (< -1 i (length text))
                   when (select-word-edge-p text i edge)
                     do (return-from select-step-word (values at i))))))

(defun select-line-end (model line end)
  "=> (values LINE INDEX): the first words of LINE's logical line, or with
END its last character -- a row's ends are the wrapped line's."
  (multiple-value-bind (first last) (select-logical-line model line)
    (let ((at (if end last first)))
      (multiple-value-bind (from to) (select-text-bounds (select-line-text model at))
        (values at (or (if end to from) 0))))))

(defun select-step-rows (model line rows column)
  "=> (values LINE INDEX COLUMN) ROWS lines from LINE, up for a negative ROWS
and clamped to the transcript, at display COLUMN or the line's last character
when it ends before. A row with no words keeps COLUMN itself, the third
value, so a run of rows through a block's padding or a blank between
paragraphs goes straight down the column rather than out to the margin."
  (let* ((at (max 0 (min (1- (length (chat-model-line-plain model))) (+ line rows))))
         (text (select-line-text model at))
         (last (nth-value 1 (select-text-bounds text))))
    (if last
        (values at (min last (plain-column-index text column)))
        (values at 0 (min column (select-lane-end model))))))

(defun select-boundary (model end &aux (count (length (chat-model-line-plain model))))
  "=> (values LINE INDEX): the transcript's first words, or with END its
last character; NIL while no line has words."
  (loop for i below count
        for at = (if end (- count i 1) i)
        do (multiple-value-bind (first last) (select-text-bounds (select-line-text model at))
             (when first (return (values at (if end last first)))))))

(defun select-text-column (model line end)
  "The display column of transcript LINE's first character that is not a
blank, or with END its last; 0 for a line with no words."
  (let ((text (select-line-text model line)))
    (multiple-value-bind (first last) (select-text-bounds text)
      (if first (text-display-width (subseq text 0 (if end last first))) 0))))

(defun select-lane-end (model)
  "The last lane column of a transcript line: as far right as a row motion
keeps its column on a row with no words."
  (multiple-value-bind (top bottom left right) (transcript-lane model)
    (declare (ignore top bottom))
    (- right left)))

(defun sync-select-mark (model state &aux (anchor (select-state-anchor state)))
  "Lay STATE's mark as MODEL's selection, the drag's own shape -- a screen
mark held on transcript points (MARK-SPAN) -- from its anchor to its cursor,
both ends inclusive, or for whole lines from the first logical line's start
to the last one's end; with nothing marked, take the selection down."
  (if (null anchor)
      (progn (setf (chat-model-selection model) nil)
             (forget-mark-points model))
      (let ((cursor (select-state-cursor state))
            ;; The screen cells are the points' to say: MARK-SPAN reads the
            ;; points whenever they ride this very cons.
            (mark (cons (cons 0 0) (cons 0 0))))
        (multiple-value-bind (from to)
            (if (eq :line (select-state-unit state))
                ;; Whole lines, held on their words: from the first line's first
                ;; character to the last one's last, so the wash keeps to the
                ;; text a block stands on and never spills into the margin; the
                ;; rows between are washed across the lane, as a drag's are.
                (let ((first (select-logical-line model (min (point-line model anchor)
                                                             (point-line model cursor))))
                      (last (nth-value 1 (select-logical-line model (max (point-line model anchor)
                                                                         (point-line model cursor))))))
                  (values (transcript-point model first (select-text-column model first nil))
                          (transcript-point model last (select-text-column model last t))))
                (values anchor cursor))
          (setf (chat-model-selection model) mark
                (mark-points model) (cons mark (cons from to)))))))

(defun select-line-band-row (model line &aux (window (chat-model-band model))
                                             (pad (band-window-top-padding window)))
  "The band row, 0-based under the frame head, transcript LINE is painted
on, or NIL while the window does not show it."
  (loop for row from 0 below (- (band-window-height window) pad)
        when (eql line (band-window-row-index window row))
          return (+ row pad)))

(defun reveal-select-line (model line &aux (window (chat-model-band model)))
  "Scroll the transcript window the least that shows LINE, as a scroll moves
it: a found-block pin spent, the follow re-pinned only at the bottom."
  (unless (select-line-band-row model line)
    (clear-transcript-pin model)
    ;; The last piece is the run the window scrolls; a pinned prompt above it
    ;; can take rows, so the step is tried until the line stands.
    (loop repeat 4
          until (select-line-band-row model line)
          do (let ((run (car (last (band-window-pieces window)))))
               (unless (and (consp run) (integerp (first run)))
                 (return))
               (destructuring-bind (start count) run
                 (band-window-scroll window (if (< line start)
                                                (- line start)
                                                (- line (+ start count -1)))))))
    (setf (chat-model-follow-bottom model) (band-window-at-bottom-p window))))

(defun select-move (model state motion)
  "Move STATE's cursor by MOTION, carry its mark to it and scroll the window
to show it. MOTION is :LEFT, :RIGHT, (:ROWS N), :WORD-FORWARD,
:WORD-BACKWARD, :WORD-END, :LINE-START, :LINE-END, :BEGINNING or :END."
  ;; A motion up first grows the projection the way a scroll up does, so the
  ;; rows above the window's first are there to move onto; the points keep
  ;; the cursor on its words while the lines under it renumber.
  (when (or (member motion '(:word-backward :beginning))
            (and (consp motion) (minusp (second motion))))
    (setf (chat-model-follow-bottom model) nil)
    (grow-transcript-projection model :whole (eq motion :beginning)))
  (multiple-value-bind (line index) (select-cursor-position model state)
    (let ((rows (and (consp motion) (second motion)))
          (column (or (select-state-goal state) (third (select-state-cursor state)))))
      (multiple-value-bind (to at to-column)
          (cond (rows (select-step-rows model line rows column))
                (t (ecase motion
                     (:left (select-step-char model line index -1))
                     (:right (select-step-char model line index 1))
                     (:word-forward (select-step-word model line index 1 :start))
                     (:word-backward (select-step-word model line index -1 :start))
                     (:word-end (select-step-word model line index 1 :end))
                     (:line-start (select-line-end model line nil))
                     (:line-end (select-line-end model line t))
                     (:beginning (select-boundary model nil))
                     (:end (select-boundary model t)))))
        (when to
          (setf (select-state-cursor state) (if to-column
                                                 (transcript-point model to to-column)
                                                 (select-point model to at))
                ;; A run of row motions keeps the column it set out from.
                (select-state-goal state) (and rows column))
          (sync-select-mark model state)
          ;; G is the tail as End is: the window lands on the bottom, the
          ;; follow pinned again, and the cursor's line shown from there.
          (when (eq motion :end) (pin-transcript-bottom model))
          (reveal-select-line model to))))))

(defun select-toggle-mark (model state unit)
  "v and V: open a mark of UNIT at the cursor; the same key again takes it
away, and the other changes the standing mark's unit."
  (cond ((null (select-state-anchor state))
         (setf (select-state-anchor state) (select-state-cursor state)
               (select-state-unit state) unit))
        ((eq unit (select-state-unit state))
         (setf (select-state-anchor state) nil))
        (t (setf (select-state-unit state) unit)))
  (sync-select-mark model state))

(defun select-swap-ends (model state)
  "o: the cursor goes to the mark's other end, which stands where it was."
  (when-let (anchor (select-state-anchor state))
    (setf (select-state-anchor state) (select-state-cursor state)
          (select-state-cursor state) anchor
          (select-state-goal state) nil)
    (sync-select-mark model state)
    (reveal-select-line model (point-line model anchor))))

(defun begin-transcript-select (model)
  "Open the keyboard selection on MODEL's transcript: a mark standing is
taken up as its own, the cursor on its later end; else the cursor stands on
the first words of the last line the window shows. With no words to select
it says so and opens nothing. => whether it opened."
  (let* ((window (chat-model-band model))
         (span (mark-span model))
         (mark (chat-model-selection model))
         (ends (cond (span (cons (transcript-point model (caar span) (cdar span))
                                 (transcript-point model (cadr span) (cddr span))))
                     (mark (let ((from (frame-cell-point model (car mark)))
                                 (to (frame-cell-point model (cdr mark))))
                             (and from to (cons from to))))))
         (line (unless ends
                 (loop with pad = (band-window-top-padding window)
                       for row downfrom (- (band-window-height window) pad 1) to 0
                       for line = (band-window-row-index window row)
                       when (and line (select-text-bounds (select-line-text model line)))
                         return line))))
    ;; A notice that is news (a turn's `Completed') gives the mode's own line
    ;; its place at once; a held one, a failure, stays to be read.
    (when (and (or ends line) (chat-model-composer-status model)
               (not (composer-status-held (chat-model-composer-status model))))
      (setf (chat-model-composer-status model) nil))
    (cond
      (ends
       ;; The cursor stands on the mark's last character, never past it.
       (let* ((to (cdr ends))
              (at (point-line model to))
              (text (select-line-text model at))
              (last (nth-value 1 (select-text-bounds text)))
              (state (make-select-state
                      (select-point model at (min (or last 0) (plain-column-index text (third to))))
                      :anchor (car ends))))
         (setf (transcript-select model) state)
         (sync-select-mark model state)
         t))
      (line
       (setf (chat-model-selection model) nil
             (transcript-select model)
             (make-select-state (select-point model line (select-text-bounds (select-line-text model line)))))
       (forget-mark-points model)
       t)
      (t (show-status model "Nothing to select yet")
         nil))))

(defun end-transcript-select (model &key keep-mark)
  "Close MODEL's keyboard selection: the hardware cursor goes back to the
composer, and the mark goes with the mode unless KEEP-MARK -- a copy leaves
what it took washed, as a drag's release does."
  (forget-transcript-select model)
  (unless keep-mark
    (setf (chat-model-selection model) nil)
    (forget-mark-points model)))

(defun select-copy (model state &key leave)
  "y, Enter, Ctrl+C and the copy chord: copy the mark and leave, the mark
kept; with nothing marked, LEAVE leaves and anything else says nothing is
marked."
  (cond ((select-state-anchor state)
         (copy-frame-selection model)
         (end-transcript-select model :keep-mark t))
        (leave (end-transcript-select model))
        (t (show-status model +selection-empty-hint+))))

(defun transcript-select-key (model key mod &aux (state (transcript-select model)))
  "One key pressed while MODEL's keyboard selection runs, KEY and MOD the
press's code and modifiers => T when the selection took it, which every key
but the quit chord is: a letter it has no use for types nothing."
  (let* ((ctrl (nlt:mod-contains mod nlt:+mod-ctrl+))
         (alt (nlt:mod-contains mod nlt:+mod-alt+))
         (shift (nlt:mod-contains mod nlt:+mod-shift+))
         (height (max 1 (band-window-height (chat-model-band model))))
         (page (max 1 (1- height)))
         (half (ceiling height 2)))
    ;; A letter's case says its key, whatever the shift bit says (KEYBIND-
    ;; MATCHES-P's rule), so V and v, G and g are told apart by the character.
    (flet ((char-key (char) (and (not ctrl) (not alt) (eql key char)))
           (ctrl-key (char) (and ctrl (not alt) (characterp key) (char-equal key char))))
      (let ((motion (cond ((or (ctrl-key #\b) (and (eq key :page-up) (not ctrl))) (list :rows (- page)))
                          ((or (ctrl-key #\f) (and (eq key :page-down) (not ctrl))) (list :rows page))
                          ((ctrl-key #\u) (list :rows (- half)))
                          ((ctrl-key #\d) (list :rows half))
                          ((and (or ctrl alt) (eq key :left)) :word-backward)
                          ((and (or ctrl alt) (eq key :right)) :word-forward)
                          ((and ctrl (eq key :home)) :beginning)
                          ((and ctrl (eq key :end)) :end)
                          ((or ctrl alt) nil)
                          ((or (eq key :left) (char-key #\h)) :left)
                          ((or (eq key :right) (char-key #\l)) :right)
                          ((or (eq key :up) (char-key #\k)) (list :rows -1))
                          ((or (eq key :down) (char-key #\j)) (list :rows 1))
                          ((char-key #\w) :word-forward)
                          ((char-key #\b) :word-backward)
                          ((char-key #\e) :word-end)
                          ((or (eq key :home) (char-key #\0) (char-key #\^)) :line-start)
                          ((or (eq key :end) (char-key #\$)) :line-end)
                          ((char-key #\g) :beginning)
                          ((char-key #\G) :end))))
        (cond
          ;; The chord that opened the mode closes it, as Ctrl+S does the picker.
          ((model-key-matches-p model :select-transcript key mod)
           (end-transcript-select model))
          ((or (model-key-matches-p model :copy-selection key mod)
               (ctrl-key #\c) (char-key #\y) (and (eq key :enter) (not ctrl) (not alt)))
           (select-copy model state :leave (not (char-key #\y))))
          ((eq key :escape)
           (if (select-state-anchor state)
               (select-toggle-mark model state (select-state-unit state))
               (end-transcript-select model)))
          ((char-key #\q) (end-transcript-select model))
          ((or (char-key #\v) (char-key #\Space)) (select-toggle-mark model state :char))
          ((char-key #\V) (select-toggle-mark model state :line))
          ((char-key #\o) (select-swap-ends model state))
          (motion
           ;; A Shift+arrow marks as it goes: the first one opens the mark.
           (when (and shift (member key '(:left :right :up :down))
                      (null (select-state-anchor state)))
             (select-toggle-mark model state :char))
           (select-move model state motion))
          ;; The quit chord still quits; Ctrl+D, a page here, never does.
          ((model-key-matches-p model :quit key mod)
           (return-from transcript-select-key nil)))
        t))))

(defun select-cursor-cell (model &aux (state (transcript-select model)))
  "The frame cell (COLUMN . ROW), both 0-based, the keyboard selection's
cursor stands on -- where the hardware cursor parks while the mode lasts --
or NIL while the window does not show its line."
  (when state
    (let* ((point (select-state-cursor state))
           (row (select-line-band-row model (point-line model point))))
      (when row
        (multiple-value-bind (top bottom left right) (transcript-lane model)
          (when (<= (+ top row) bottom)
            (cons (min right (+ left (third point))) (+ top row))))))))

(defun select-mode-hint (state)
  "The composer's line while the keyboard selects: what moves the cursor,
or, with a mark standing, that moving carries it."
  (if (select-state-anchor state)
      "Selecting · move to extend the mark"
      "Selecting · move with h j k l, w b e, 0 $, PgUp PgDn, g G"))

(defun select-key-segs (state)
  "The keys the keyboard selection answers, each a filled cap before its
verb, as STEER-KEY-SEGS lays the running turn's."
  (loop for (cap verb) in (if (select-state-anchor state)
                              '(("o" "swap ends") ("y" "copy") ("esc" "unmark"))
                              '(("v" "mark") ("V" "lines") ("esc" "leave")))
        append (list (make-seg :text (format nil " ~a " cap) :tone :key-chip)
                     (make-seg :text (format nil " ~a  " verb) :tone :muted))))

(defun range-target (ranges index)
  "The TARGET of the (START END TARGET) in RANGES whose span holds INDEX, or NIL."
  (loop for (start end target) in ranges when (<= start index (1- end)) return target))

(defun frame-target-at-position (model x y)
  "The target at 1-based frame X,Y, recorded on that exact painted row.
Character ranges come from the painter's clipped segments; screen columns
are resolved through the frame's plain lane, including wide Unicode."
  (when (and (integerp x) (integerp y)
             (<= 1 x (frame-width model)) (<= 1 y (frame-height model)))
    ;; The plain lane first: reading it composes a frame that was never painted.
    (let ((plains (frame-plain-rows model)))
      (range-target (plain-row-links (aref (frame-lane-rows (frame-lanes model)) (1- y)))
                    (plain-column-index (aref plains (1- y)) (1- x))))))

(defun link-at-position (model line column &aux (plains (chat-model-line-plain model)))
  "The target painted at transcript visual LINE, display COLUMN, or NIL: the
ranges LLINE-ROWS recorded on that line's plain row."
  (when (and (integerp line) (< -1 line (length plains)) column)
    (let ((plain (aref plains line)))
      (range-target (plain-row-links plain) (plain-column-index plain column)))))

(defun open-target (uri &aux (path (file-uri-path uri)))
  "Hand URI — an http(s) page, or a file: target naming a file or a folder —
to the platform's own opener, detached with its output in the cache's
nodecode/open-url.log."
  ;; A folder reaches the operator's file manager and a file the program its
  ;; type is registered to; where the desktop answers a file with a chooser,
  ;; that chooser is what appears. The default handler decides, always: no
  ;; list of programs is kept here, and no host path is baked in. => T when
  ;; the opener started, else the keyword naming the refusal — :MISSING for a
  ;; file: target that is not there any more, :NO-OPENER for a POSIX shell
  ;; with neither WAYLAND_DISPLAY nor DISPLAY, which is an ssh session, or an
  ;; opener that would not start.
  ;;
  ;; A path is probed natively, never as a pathname: a `*' or a `[' in a file
  ;; name is a character here, the way it is everywhere else in this file.
  (if (and path
           (not (ignore-errors (uiop:probe-file* (uiop:parse-native-namestring path)))))
      :missing
      (let* ((target (or path uri))
             (command #+win32 (list "rundll32" "url.dll,FileProtocolHandler" target)
                      #+darwin (list "open" target)
                      #-(or win32 darwin)
                      (and (some (lambda (variable &aux (value (uiop:getenv variable)))
                                   (and value (plusp (length value))))
                                 '("WAYLAND_DISPLAY" "DISPLAY"))
                           (list "xdg-open" target))))
        (if (and command
                 (nlk:spawn-detached command (nlk:cache-path "nodecode/open-url.log")))
            t
            :no-opener))))

(defun open-refusal-hint (uri outcome &aux (path (file-uri-path uri)))
  "What a refused open says in the transient lane: which of the two things
it could not do, and why."
  ;; A click that did nothing has to say so — silence is what the operator
  ;; already reported as a broken click.
  (cond ((eq :missing outcome)
         (format nil "Not there any more — ~a" (home-abbreviated (or path uri))))
        (path "Nothing here opens files — drag across the path to copy it")
        (t "No browser to open links here — drag across one to copy it")))

(defun render-disclosure-at-cursor (model owner line y)
  "Reproject a toggled disclosure without moving its clicked owner at Y."
  (let* ((refs (chat-model-line-rows model))
         (relative (- line (or (position owner refs :test #'eq) line)))
         (window (chat-model-band model)))
    (clear-transcript-pin model)
    (setf (chat-model-follow-bottom model) nil)
    (render-state model)
    (let* ((refs (chat-model-line-rows model))
           (start (or (position owner refs :test #'eq)
                      (when-let (group (row-groups owner))
                        (position group refs :test #'eq))))
           (target (when start
                     (+ start (min relative
                                   (1- (loop for i from start below (length refs)
                                             while (eq (aref refs i) (aref refs start)) count t))))))
           (wanted (- y 4)))
      (when target
        ;; Invert the shared piece lane, including pinned prompt separators.
        ;; The first estimate puts the title inline; the correction accounts
        ;; for the prompt's synthetic rows without duplicating its policy.
        (loop with offset = (- target wanted)
              repeat 3
              do (setf (band-window-top window)
                       (if (<= offset 0) (1- offset)
                           (min offset (band-window-max-offset window))))
                 (let ((actual (loop with at = (band-window-top-padding window)
                                     for piece in (band-window-pieces window)
                                     for (start count) = (and (consp piece) piece)
                                     when (and (integerp start) (< -1 (- target start) count))
                                       return (+ at (- target start))
                                     do (incf at (if (integerp start) count 1)))))
                   (unless (and actual (/= actual wanted)) (return))
                   (incf offset (- actual wanted))))))))

(defun picker-dialog-command (kind)
  "The footer command a picker of KIND belongs to: a footer cell's repeat
click cancels the dialog its command opened. A kind is its command's name,
but /models walks three: its provider list, a provider's models and the
auxiliary model's."
  (case kind
    ((:model-providers :models :auxiliary-models) "models")
    (t (and kind (string-downcase (string kind))))))

(defun handle-mouse-release (model msg &aux (x msg.x)
                                            (y msg.y)
                                            (undragged (not (chat-model-selection-dragged model))))
  "Resolve the armed gestures: a scrollbar drag ends first, so it can never
confirm a row; an undragged tool-card press toggles the expand, an
undragged press on a band
row's ✕ interrupts that exec and one elsewhere on its lines opens or closes
it in place as a card's does,
an undragged press on a target opens it — a page in the browser, a file or a folder in whatever the
desktop opens it with —, an undragged click on an open dialog's row
confirms it, on a tab's chip shows that tab and on a key's hint answers that
key (=> its key press, which UPDATE runs as a typed one), and a dragged
selection copies to the clipboard through the program's own writer."
  (cond
    ;; A release that ends a scrollbar drag — the transcript's gutter or the
    ;; open dialog's own bar — clears the grab and confirms nothing: the
    ;; drag was the gesture, not the row the pointer came to rest on.
    ((chat-model-scrollbar-drag model)
     (setf (chat-model-scrollbar-drag model) nil))
    ;; A left click on an open sheet's window is the sheet's.
    ((sheet-dialog-p (chat-model-dialog model))
     (disarm-pointer model)
     (multiple-value-bind (column row) (sheet-at model x y)
       (when (and column (eq :left (nlt:mouse-event-button msg)))
         (funcall (sheet-dialog-click (chat-model-dialog model))
                  model (chat-model-dialog model) column row))))
    ((and (chat-model-pressed-tool-row model) undragged)
     (let ((pressed (chat-model-pressed-tool-row model)))
       (disarm-pointer model)
       (multiple-value-bind (line) (frame-transcript-position model x y)
         (when (and line (eq pressed (transcript-item-at-line model line)))
           ;; The Zig toggleExpandedToolCall accordion; a group's member closes to it.
           (let* ((key (cond ((tool-group-p pressed) pressed.key)
                             ((eq :tool-call (transcript-row-kind pressed))
                              (tool-row-expand-key pressed))
                             (t pressed)))
                  (group (row-groups pressed))
                  (current (chat-model-expanded-tool-call model)))
             (setf (chat-model-expanded-tool-call model)
                   (cond ((and (tool-group-p pressed) (eq :process (first key)))
                          ;; A run does what its arrow says: ▾ folds it, a
                          ;; member open in it too; ▸ opens its whole trail,
                          ;; a call open under its title still open in it.
                          (cond ((tool-group-open-p pressed current) nil)
                                ((tool-group-open-p pressed (open-key current)) (open-key current))
                                (t key)))
                         ((and group (not (tool-group-p pressed)) (eq :process (first group.key))
                               (not (tool-group-open-p group current)))
                          ;; A call under a shut run's title opens alone, the
                          ;; run still shut.
                          (unless (equal key (open-key current)) (list :fold key)))
                         ((and (tool-group-p pressed) (eq :mutation (first key))
                               (tool-group-open-p pressed current))
                          ;; Close the file list, not its enclosing work trail.
                          (and group (tool-group-key group)))
                         ((not (equal key current)) key)
                         ((and group (not (eq group pressed))) group.key))))
           (render-disclosure-at-cursor model pressed line y)))))
    ((and (chat-model-pressed-stop model) undragged)
     (let ((id (chat-model-pressed-stop model)))
       (disarm-pointer model)
       (multiple-value-bind (line column) (frame-transcript-position model x y)
         (when (and line (eql id (band-stop-at model line column)))
           (interrupt-band-exec model id)
           (render-status-frame model)))))
    ((and (chat-model-pressed-exec model) undragged)
     (let ((id (chat-model-pressed-exec model)))
       (disarm-pointer model)
       (let* ((line (frame-transcript-position model x y))
              (row (and line (band-exec-row-at model line))))
         ;; The view is /execs': a click here unfolds the lines.
         ;; Without this, a click would open a dialog instead.
         (when (and row (eql id (bg-band-row-shell-session-id row)))
           (let ((key (band-row-expand-key row)))
             (setf (chat-model-expanded-tool-call model)
                   (unless (equal key (chat-model-expanded-tool-call model)) key)))
           (render-disclosure-at-cursor model row line y)))))
    ((and (chat-model-pressed-link model) (eq :left (nlt:mouse-event-button msg)) undragged)
     (let ((uri (chat-model-pressed-link model)))
       (disarm-pointer model)
       (multiple-value-bind (line column) (frame-transcript-position model x y)
         (cond
           ((eq uri :status-hint) nil) ; hover-only chrome, not an action
           ((and line (consp uri) (eq :diff (first uri))
                 (equal uri (link-at-position model line column)))
            (let* ((row (transcript-item-at-line model line))
                   (group (row-groups row))
                   (current (chat-model-expanded-tool-call model))
                   (run (and group (eq :process (first group.key))))
                   (key (cond ((not (equal uri (open-key current))) uri)
                              ;; A run's call keeps its files listed under its line.
                              (run (tool-row-expand-key row))
                              (t (and group (tool-group-key group))))))
              ;; A file of a call open under a shut run's title keeps the run shut.
              (setf (chat-model-expanded-tool-call model)
                    (if (and run (not (tool-group-open-p group current))) (list :fold key) key))
              (render-disclosure-at-cursor model row line y)))
           ;; A tab's chip on the open panel shows its tab; a key's hint is
           ;; that key, handed back for UPDATE to run as a typed one.
           ((and (consp uri) (eq :dialog-tab (car uri))
                 (equal uri (frame-target-at-position model x y)))
            (picker-tab-pick model (cdr uri)))
           ((and (consp uri) (eq :key (car uri))
                 (equal uri (frame-target-at-position model x y)))
            (hint-key-press (cdr uri)))
           ;; A composer dropdown row is chosen as Enter chooses the
           ;; highlighted one: it takes the highlight, then Enter goes.
           ((and (consp uri) (eq :suggestion (car uri))
                 (equal uri (frame-target-at-position model x y)))
            (setf (chat-model-slash-selection model) (cdr uri))
            (nlt:make-key-press-msg :code :enter))
           ((equal uri (frame-target-at-position model x y))
            (if (and (consp uri) (eq :command (car uri)))
                (let* ((dialog (chat-model-dialog model))
                       (kind (and (picker-dialog-p dialog) (picker-dialog-kind dialog))))
                  ;; Every footer command toggles its canonical picker kind,
                  ;; not a whitelist of options. A repeat click cancels even
                  ;; with a query; it never selects or reruns the opener.
                  (if (and kind (string-equal (cdr uri) (picker-dialog-command kind)))
                      (dismiss-dialog model (picker-dialog-on-cancel dialog))
                      (multiple-value-bind (notice cmd) (execute-slash-command model (cdr uri) "")
                        (when notice (show-status model notice))
                        (when cmd (error "Footer command unexpectedly returned a program command"))))
                  (render-state model))
                (let ((outcome (open-target uri)))
                  (unless (eq t outcome)
                    (show-status model (open-refusal-hint uri outcome))
                    (render-state model)))))
           ((and (equal uri '(:home . :mark))
                 (equal uri (home-command-at-position model line column)))
            (click-mark model))
           ((and (consp uri) (eq :home (car uri))
                 (equal uri (home-command-at-position model line column)))
            (multiple-value-bind (notice cmd) (execute-slash-command model (cdr uri) "")
              (when notice (show-status model notice))
              (render-state model)
              (when cmd (error "Home view command unexpectedly returned a program command"))))
           ((and line (equal uri (link-at-position model line column)))
            (let ((outcome (open-target uri)))
              (unless (eq t outcome)
                (show-status model (open-refusal-hint uri outcome))
                (render-state model))))))))
    ;; An open dialog confirms on release: a press on its panel only armed
    ;; the anchor, so a click with no drag between it and this release
    ;; replays Enter on the row under the pointer (the Zig
    ;; handleActiveDialogHover press leg), while a drag confirms nothing.
    ;; The transcript's gestures above resolve first: a press over the
    ;; transcript armed them as with no dialog open.
    ((and (chat-model-dialog model) (eq :left (nlt:mouse-event-button msg)) undragged)
     (setf (chat-model-selection-anchor model) nil)
     (when (dialog-apply-hover model y)
       (picker-dialog-key model :enter)))
    ((and (chat-model-selection-anchor model)
          (chat-model-selection-dragged model)
          (chat-model-selection model))
     (disarm-pointer model)
     (copy-frame-selection model))
    (t
     (disarm-pointer model)
     (when (chat-model-selection model)
       (setf (chat-model-selection model) nil)
       (render-state model)))))

;;; --- update method ------------------------------------------------------