;;;; fact-block.lisp --- the fact block, the found-block pin, transcript scrolling. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- the fact block -------------------------------------------------------
;;;
;;; What the landing screen is for is orientation, and the facts worth
;;; orienting by are the ones nothing else on the screen carries: which home
;;; this is, how many cell folders this process holds, and how many sealed
;;; generations the home can restore. Load diagnostics stay in logs. Each
;;; row is labelled by the command whose catalog holds the rest -- /profile,
;;; /cells, /generations -- so the row teaches the command without a tail
;;; spelling out what it does.
;;;
;;; Row order is shedding order. A short band drops rows from the tail, and
;;; the two facts the footer still carries — the model identity and the
;;; workspace — are the right ones to lose.

(defun cell-label (name)
  "A folder name as a list of them says it, on the home screen or in setup:
the shipped `nodecode-` prefix is noise once five of them line up."
  (if (and (> (length name) 9) (uiop:string-prefix-p "nodecode-" name))
      (subseq name 9)
      name))

(defun home-meta-value-width (width)
  "Columns a fact value may occupy before it folds: the fixed lane, or what
the band can hold when the band is narrower than that."
  ;; The width is a constant and not a fraction of WIDTH, so the fact block
  ;; reads the same on every terminal instead of stretching with the window it
  ;; happens to sit in.
  (max 12 (min (- width +home-meta-label-width+ 4) +home-meta-value-width+)))

(defun clipped-path (path width &aux (head (subseq path 0 (1+ (or (position #\/ path) -1))))
                                     (room (- width (text-display-width head) 1)) (tail nil))
  "PATH in at most WIDTH columns: whole when it fits, else its head (`~/' or
`/'), an … and as many whole trailing folders as the room holds, so the last
folder always shows — cut from its front only when it alone is wider."
  (cond ((<= (text-display-width path) width) path)
        (t (loop for slash = (position #\/ path :from-end t)
                   then (position #\/ path :from-end t :end slash)
                 while (and slash (>= slash (length head))
                            (<= (text-display-width (subseq path slash)) room))
                 do (setf tail (subseq path slash)))
           (concatenate 'string head "…"
                        (or tail (subseq path (max (length head)
                                                   (- (length path) (max 0 room)))))))))

(defun home-meta-rows (width &aux (records (or (ignore-errors (nlk:cells)) '()))
                                  ;; The layer is part of the composed self, not an installed cell.
                                  ;; Knowledge cells and folders whose load failed still count.
                                  (folders (remove :layer records :key #'nlk:cell-kind))
                                  (lane (home-meta-value-width width))
                                  (rows '()))
  "The landing screen's fact block: an ordered list of (LABEL VALUE TONE
COMMAND), COMMAND the slash command a click on the row runs."
  ;; A folded value's continuation rows carry LABEL NIL, so the value lane
  ;; stays one column.
  ;;
  ;; Only facts the chrome cannot already give: the model identity and the
  ;; workspace sit in the footer of every frame, so repeating them here would
  ;; be two rows that say nothing new the moment the operator's eye has moved
  ;; once.
  (flet ((row (label value tone command)
           (when (and value (plusp (length value)))
             (push (list label value tone command) rows))))
    ;; Which organism this is. Every home has a name to say, the default
    ;; one included: identity by absence reads as unknown, not as default,
    ;; and an operator with two shells open is asking the question this
    ;; row answers.
    ;; The home is written as the operator reads it (`~/...') and kept to
    ;; the value lane from the left, so its last folder, the one that
    ;; names it, is the part that shows: the whole path cut at the
    ;; window's edge would lose exactly that. A long name leaves the
    ;; path eight columns.
    (let ((name (clip-text-to-width (nlk:profile-label) (max 1 (- lane 3 8)))))
      (row "/profile"
           (format nil "~a · ~a" name
                   (clipped-path (home-abbreviated (namestring (nlk:home)))
                                 (max 8 (- lane (text-display-width name) 3))))
           :chrome "profile"))
    (row "/cells" (format nil "~d installed" (length folders)) :chrome "cells")
    ;; Failed/skipped cell details stay in DESCRIBE-CELLS's boot log and
    ;; explicit inspection (/cells), never the landing screen.
    ;; Sealed generations are the organism's restore points, counted from
    ;; the canonical manifest roster -- by its stat, since this runs every
    ;; frame (NLK:MANIFEST-COUNT). The row stands before the first seal,
    ;; just as /cells stands before the first installed cell.
    (row "/generations" (format nil "~d sealed" (nlk:manifest-count)) :chrome "generations")
    ;; What is MISSING, which the rows above cannot say: they describe what
    ;; the folder did, and an operator whose model pin was refused or whose
    ;; channel never started reads a screen full of healthy cells and
    ;; concludes everything is fine. Cell load diagnostics stay in the
    ;; boot log and explicit inspection, so they are excluded here;
    ;; everything else — including a cell's own, which reach
    ;; HEALTH-FACTS through the hook — lands on the landing screen because
    ;; this is the frame an operator is looking at when they wonder.
    ;; :DANGER only, and the distinction is load-bearing. A bare image has
    ;; no config and no model pin, which HEALTH-FACTS reports as :warn —
    ;; true, and not a fault: the operator is standing in the wizard that
    ;; sets it. A landing screen that announced it would be telling a
    ;; first-time user their fresh install is broken, and would cost the
    ;; block its rule that a bare image shows no rows at all. What reaches
    ;; here is what is actually WRONG — a refused pin, a folder that failed
    ;; — and the rest waits for /doctor, which is asked rather than shown.
    ;; Folded to the value lane like the cells, whole: a refused pin's
    ;; reason is what to do, and one row cut at the window's edge ended
    ;; before it said so (xm-203).
    (dolist (fact (remove-if-not (lambda (fact) (eq (health-fact-tone fact) :danger))
                                 (or (ignore-errors (health-facts)) '())))
      (unless (member (health-fact-key fact) '(:cells :cell))
        (let ((text (format nil "~a~@[ — ~a~]" (health-fact-detail fact) (health-fact-remedy fact))))
          (loop for line in (wrapped-lines text lane)
                for first = t then nil
                do (row (and first (health-fact-label fact)) line :danger "doctor"))))))
  (nreverse rows))

(nlk:define-side-table home-row-actions (:weakness :key :synchronized t)
  "Live-only plain Home row -> ((START END TARGET) ...), from painted segments.")

(defun home-command-at-position (model line column &aux (plains (chat-model-line-plain model)))
  "The trusted Home action on the painted row at LINE/COLUMN, or NIL."
  (when (and (home-screen-p model) (integerp line) (< -1 line (length plains)) column)
    (let ((plain (aref plains line)))
      (range-target (home-row-actions plain) (plain-column-index plain column)))))

;;; One tip a shell. A line that never changed was read once and then not at
;;; all, so each launch picks its own. A home that has sealed no generation
;;; is on its first launch, and gets the first: what only a self that
;;; rewrites itself can do. The pick holds for the shell's life, /clear
;;; included, so the line never changes under the operator's eye.
(defparameter +home-tips+
  '(("Try \"" "you can't read PDFs. teach yourself." "\"")
    ("Type " "/" " to browse commands; /help shows shortcuts.")
    ("" "/generations" " restores any earlier self.")
    ("" "/tree" " rewinds to any turn, or continues from it.")
    ("" "/think" " sets how hard the model reasons."))
  "The landing screen's tips, each (BEFORE TYPED AFTER): the words around the
one thing to type.")

(defun home-tip (model)
  "MODEL's tip from +HOME-TIPS+, picked at its first landing screen."
  (or (chat-model-home-tip model)
      (setf (chat-model-home-tip model)
            (if (zerop (nlk:manifest-count))
                (first +home-tips+)
                (nth (random (length +home-tips+) (make-random-state t)) +home-tips+)))))

(defun home-band-lines (model width &aux (height (chat-model-term-height model)))
  "The home band as (values LINES PLAINS): painted rows sized to fill the
transcript band exactly, so the band's bottom-anchoring becomes the Zig
absolute-row placement."
  ;; NIL on cramped windows — home.zig draws nothing below 12 rows or 48
  ;; columns; row 1 belongs to the shared header.
  (when (and (>= height 12) (>= (chat-model-term-width model) 48))
    (let* ((full (max 1 (- height (shell-overhead height (composer-visible-rows model)))))
           ;; The setup walk's dialog docks under the landing screen and takes
           ;; its rows from the band: the screen centres in what is left, as
           ;; on a window that much shorter.
           (band-height (if (and (chat-model-dialog model) (chat-model-band model))
                            (progn (sync-viewport-geometry model)
                                   (min full (band-window-height (chat-model-band model))))
                            full))
           (height (- height (- full band-height)))
           ;; How many folded lines the cell lane may spend: what the
           ;; band can hold once the logo, the tip and their
           ;; blanks have taken theirs. `+N more` is then a last resort on
           ;; a genuinely full screen, not a fixed cut at three.
           ;; A screen that still does not fit folds the lane to one line
           ;; first: the rows under it are what is wrong, a refused pin's
           ;; reason ending on what to do, and the tail went first (xm-203).
           ;; The rows then shed from the tail until the whole landing
           ;; screen fits; the anchor lifts as the block grows, so each drop
           ;; is re-measured.
           ;; The blank that separates the block from the tip belongs to
           ;; the block: an empty block costs no row at all.
           (extra (lambda (rows) (if rows (1+ (length rows)) 0)))
           (pad (lambda (rows) (max 0 (- (home-logo-start-row height (funcall extra rows)) 3))))
           (meta (let ((rows (home-meta-rows width)))
                   (loop until (or (null rows)
                                   ;; logo, version, blank, tip, blocks
                                   (<= (+ (funcall pad rows) 6 (funcall extra rows)) band-height))
                         ;; Lower-priority orientation never cuts off a
                         ;; diagnostic's remedy. Generations and profile
                         ;; yield before the diagnostic tail.
                         for spare = (and (find :danger rows :key #'third)
                                          (or (find "generations" rows :key #'fourth :test #'equal)
                                              (find "profile" rows :key #'fourth :test #'equal)))
                         do (setf rows (if spare (remove spare rows :test #'eq)
                                           (butlast rows))))
                   rows))
           (top-pad (funcall pad meta))
           (bg (canvas-fill-bg))
           (lines '())
           (plains '()))
      ;; A row's plain lane is the text its segs paint (SEGS-TEXT), so the two
      ;; lanes cannot disagree about what the row says.
      (labels ((emit-at (pad segs &optional command
                        &aux (spaces (make-string pad :initial-element #\Space))
                        (plain (concatenate 'string spaces (segs-text segs))))
                 ;; Ranges are character indexes into this final plain object;
                 ;; the hit reader converts painted columns (Unicode included).
                 ;; A Home command covers its painted segments.
                 (let ((target (and command (cons :home command))) (offset pad) (actions nil))
                   (loop for seg in segs
                         for text = (seg-text seg)
                         for end = (+ offset (length (string-right-trim " " text)))
                         do (when (and target (< offset end))
                              (push (list offset end target) actions)
                              ;; The mark answers with its glint, not a rule
                              ;; through its glyphs.
                              (when (and (stringp command)
                                         (equal (chat-model-hover-target model) target))
                                (setf (seg-underline seg) t)))
                            (incf offset (length text)))
                   (when actions (setf (home-row-actions plain) (nreverse actions))))
                 (push (render-row (list (make-seg :text spaces)) segs bg width) lines)
                 (push plain plains))
               (blank ()
                 (push "" lines)
                 (push "" plains))
               (centred (columns) (max 0 (floor (- width columns) 2)))
               (emit (segs &optional command) (emit-at (centred (segs-display-width segs)) segs command)))
          (dotimes (i top-pad) (blank))
          (loop with heat = (mark-glow-heat model)
                for line in +home-mark+
                for row from 0
                do (emit (mark-line-segs line row heat) :mark))
          ;; The semantic tag belongs directly under the logo. Commit metadata
          ;; remains in `nodecode --version`; both read the same build identity.
          (emit (list (seg (format nil "v~a" (subseq *user-agent-version* 0 (position #\+ *user-agent-version*)))
                           :muted)))
          (blank)
          ;; The shell's tip (HOME-TIP), the one thing to type in it brighter
          ;; than the words around it.
          (destructuring-bind (before typed after) (home-tip model)
            (emit (list (seg "Tip " :tool) (seg before :muted) (seg typed :chrome) (seg after :muted))))
          ;; The fact block centres as one block, not line by line: a
          ;; column of labels only reads as a column when every row shares
          ;; one left edge.
          (when meta
            (blank)
            (let ((pad (centred (+ +home-meta-label-width+
                                   (loop for row in meta maximize (text-display-width (second row)))))))
              (loop for (label value tone command) in meta
                    do (emit-at pad
                                (list (seg (format nil "~va" +home-meta-label-width+ (or label ""))
                                           (if (eq tone :danger) :danger :muted))
                                      (seg value tone))
                                command)))))
      ;; Fill the band exactly: bottom-anchored full content is
      ;; top-anchored content, and rows land on their Zig absolute rows.
      ;; A band shorter than the content truncates from the tail, the
      ;; home.zig row >= content_bottom_row break.
      (flet ((fit (pushed &aux (rows (nreverse pushed)))
               (loop repeat band-height collect (or (pop rows) ""))))
        (values (fit lines) (fit plains))))))

(defun projection-width (model)
  "The width the transcript projection is built at."
  ;; One expression, because RENDER-STATE, the cache guard, and the animation
  ;; tick must agree on it — a guard reading a width the renderer never uses
  ;; either re-projects every frame or never re-projects at all.
  (transcript-text-width (max 20 (chat-model-term-width model))))

(defun projection-cache-live-p (model &aux (cache (chat-model-projection-cache model)))
  "Whether MODEL's cached transcript projection was built under the render
inputs now in force: the width it was laid out at, and the palette and color
profile it was PAINTED under."
  ;;
  ;; The paint inputs belong in this guard because a projected row is an
  ;; SGR-baked string, not a description of one — the viewport holds finished
  ;; bytes. Every render cache below this one already revalidates on *PALETTE*
  ;; (LLINE-RENDER, ROW-DISPLAY-KEY, the band-row epoch); the projection cache is
  ;; the last one above them, and it is where a theme engage used to survive.
  (and cache
       (eql (first cache) (projection-width model))
       (eq (second cache) *palette*)
       (eq (third cache) (effective-color-profile))))

(defmacro with-render-scope ((model) &body body)
  "BODY under the render-scope specials MODEL's rows are laid out under --
the detail level and its chord, the open card, the hover, the image mode."
  ;; Every look at the rows lays them out, and the row memos key on these, so
  ;; a look taken outside them would lay out rows the render then lays out
  ;; again -- or, for an input no memo keys on (the chord), keep them as the
  ;; look saw them.
  `(let ((*tool-detail* (chat-model-tool-detail ,model))
         (*fades* (and (chat-model-program ,model) t))
         (*expanded-tool-call* (chat-model-expanded-tool-call ,model))
         (*hover-row* (chat-model-hover-row ,model))
         (*hover-target* (chat-model-hover-target ,model))
         (*kitty-images-enabled* (chat-model-kitty-graphics ,model)))
     ,@body))

(defun transcript-projection-start (model state width)
  "The row MODEL projects STATE from: the tail while the window follows it;
where the window stood or grew to while it reads STATE's history; the first
under a found-block pin, which rests on a line of the whole table."
  ;; The tail is chosen again at every following render — the rows are
  ;; cached, so it is cheap — rather than kept: a render landing mid-replay
  ;; would otherwise pin the start of the few rows folded so far.
  (let ((entry (chat-model-projection-start model)))
    (cond ((transcript-pins model)
           (setf (chat-model-projection-start model) (cons state nil))
           nil)
          ((and entry (eq (car entry) state) (not (chat-model-follow-bottom model)))
           (cdr entry))
          ;; A preview under a dialog is looked at rather than read back, and a
          ;; held arrow lays one out a row: its first projection is one window.
          ;; Any other state's tail is measured in screens of the terminal, not
          ;; of the band: a dialog docked over the band leaves it a row or a
          ;; few, and a tail chosen under it outlives the dialog.
          (t (let ((band (chat-model-band model))
                   (preview (chat-model-transcript-preview model)))
               (cdr (setf (chat-model-projection-start model)
                          (cons state
                                (tail-projection-start
                                 state width
                                 (if (and preview (eq state (cdr preview)))
                                     (if band (band-window-height band) (chat-model-term-height model))
                                     (* *tail-projection-screens* (chat-model-term-height model))))))))))))

(defun grow-transcript-projection (model &key whole)
  "Project MODEL's transcript further back — WHOLE, or by another window once
its window is within a window of where the projection starts — keeping the
window on the rows it shows."
  ;; The window's offset counts from the top, so the rows grown in above
  ;; push what it shows down by exactly their lines. A window at a time,
  ;; because every line grown in is laid out for the first time (about 0.1
  ;; ms of layout and collection each): three windows made a hitch of
  ;; several frames every few dozen notches of the wheel.
  (let ((window (chat-model-band model))
        (entry (chat-model-projection-start model)))
    (when (and window entry (cdr entry)
               (eq (car entry) (visible-transcript-state model))
               (not (chat-model-follow-bottom model))
               (or whole (< (band-window-offset window) window.height)))
      (let ((before window.count))
        ;; The look lays out rows as the render will, so the render reuses
        ;; every block it memoized.
        (setf (cdr entry)
              (and (not whole)
                   (with-render-scope (model)
                     (tail-projection-start (car entry) (projection-width model)
                                            window.height (cdr entry)))))
        (render-state model)
        (band-window-scroll window (- window.count before))))))

;;; The per-row render memos are weak on the row, and a transcript keeps its
;;; rows: without this, every row ever projected would keep its logical lines,
;;; its block and its paint -- and through that paint the band rows keyed on
;;; it -- for the life of the shell. A full render keeps them for what it
;;; projects; a row scrolled back into the projection is built again once.
(defun forget-unprojected-rows (blocks &aux (projected (make-hash-table :test 'eq)))
  "Drop the render memos of every transcript row BLOCKS does not project."
  (flet ((keep (row) (when row (setf (gethash row projected) t))))
    (dolist (block blocks)
      (let ((row (display-block-row block)))
        (keep row)
        (when (tool-group-p row) (mapc #'keep (tool-group-members row))))
      (mapc #'keep (display-block-refs block))))
  (dolist (table (list *row-display-cache* *row-llines-cache* *row-md-cache*
                       *row-activity-cache*))
    (loop for row being the hash-keys of table
          unless (gethash row projected) do (remhash row table))))

(defun render-state (model)
  "Render the folded rows into the viewport at the transcript text width."
  ;; Streaming lives in the row lanes themselves — the reducers coalesce
  ;; deltas into live-only tail rows — so rendering is projection plus rhythm.
  ;; A transport NOTICE, when set, renders as one live-only system block at
  ;; the tail; the live turn bar follows it when a turn is in flight.
  ;;
  ;; This is the full render: it re-projects the whole transcript, refreshes
  ;; the projection cache, and clears the stream-coalescing dirty flag.
  ;; Animation ticks go through RENDER-STATUS-FRAME instead, which recomposes
  ;; only the live chrome over the cached projection.
  (with-render-scope (model)
    (bind ((width (projection-width model))
           (state (visible-transcript-state model))
           (blocks (project-state state :width width
                                        :start (transcript-projection-start model state width) :prompt t))
           ((lines plains refs) (transcript-line-table blocks)))
      (forget-unprojected-rows blocks)
      (setf (chat-model-projection-cache model)
            (list width *palette* (effective-color-profile)
                  lines plains refs blocks)
            (chat-model-transcript-dirty model) nil)
      (compose-transcript-viewport model width lines plains refs))))

(defun render-state-at-paint (model)
  "Leave MODEL's transcript for the next paint to project: the frame renders
a projection that is no longer live before it composes (the view's settle),
so any number of changes before it lay the rows out once."
  (setf (chat-model-projection-cache model) nil))

(defun status-chrome-key (model &optional (width (projection-width model)))
  "What the live chrome shows that only the clock moves, as it reads now:
the turn bar at WIDTH (its mark, elapsed clock and verb -- or the previewed
turn's), each band row's mark, and the transient hint while it stands. Two
composes with EQUAL keys paint the same chrome."
  ;; The bar is the bar's own render (LIVE-STATUS-LINE), never a second
  ;; derivation of its clocks: a clock the bar grows later moves the key by
  ;; construction. One row's render, against the frame compose it stands in
  ;; for, is what a tick that moved nothing costs.
  (let ((now (get-internal-real-time)))
    (list (live-status-line model width)
          (loop for row in (state-background-rows (visible-transcript-state model))
                collect (list (background-band-mark row now) (segs-text (band-row-tail-segs row nil))))
          (status-said model))))

(defun status-chrome-moved-p (model)
  "Whether the live chrome reads differently now than at the last compose: a
spinner step, an elapsed digit, a verb going to `waiting', a band mark, a
hint running out."
  ;; The frame clock ticks 60 times a second; the spinner holds a frame
  ;; 150 ms and the elapsed clock a digit 100 ms, so most ticks move nothing.
  ;; Without the moved-p check, every tick would compose the whole frame; that
  ;; composition would be a busy shell's entire cost.
  (not (equal (status-chrome-key model) (chat-model-status-chrome-key model))))
(defun render-status-frame (model)
  "Refresh live clocks over cached transcript blocks, without re-grouping or
re-projecting history. Only pending exec blocks get new command/elapsed paint."
  (if (and (projection-cache-live-p model)
           (or (seventh (chat-model-projection-cache model))
               (null (state-background-rows (visible-transcript-state model)))))
      (with-render-scope (model)
        (let* ((cache (chat-model-projection-cache model))
               (blocks (seventh cache)))
          (when (some (lambda (block) (bg-band-row-p (display-block-row block))) blocks)
            (setf blocks (mapcar (lambda (block &aux (row (display-block-row block)))
                                  (if (bg-band-row-p row) (row-display-block row :width (first cache)) block))
                                blocks))
            (multiple-value-bind (lines plains refs) (transcript-line-table blocks)
              (setf (cdddr cache) (list lines plains refs blocks))))
          (compose-transcript-viewport model (first cache) (fourth cache) (fifth cache) (sixth cache))))
      (render-state model)))
(defun fill-line-vector (vector rows count tail &aux (needed (+ count (length tail))))
  "VECTOR holding the first COUNT rows of ROWS followed by TAIL."
  ;; The lane the mouse and the kitty placements index has to be a vector, and
  ;; rebuilding it with COERCE copied the projected transcript twice per
  ;; frame. It is reset and refilled instead, and grows only when the
  ;; transcript does -- and shrinks when a projection that was far longer is
  ;; not coming back.
  ;;
  ;; A slot past the new fill pointer is out of the lane's reach, not the
  ;; collector's: it still names a row of the longer projection. Without
  ;; clearing it, a lane that shrinks would keep that row alive. Those slots
  ;; are emptied.
  (when (or (not (array-has-fill-pointer-p vector))
            (< (array-dimension vector 0) needed)
            (> (array-dimension vector 0) (max 1024 (* 4 needed))))
    (setf vector (make-array (max needed 64) :adjustable t :fill-pointer 0)))
  (let ((was (fill-pointer vector)))
    (setf (fill-pointer vector) needed)
    (replace vector rows :end1 count)
    (replace vector tail :start1 count)
    (loop for index from needed below was
          do (setf (aref vector index) nil)))
  vector)

;;; --- the found-block pin ------------------------------------------------------
;;; The conversation search's Enter lands on a block, not on a tail: each hit
;;; carries the durable log position of the fact its block was built from,
;;; the fold stamps every transcript row it builds with that position
;;; (STAMP-UNPOSITIONED-ROWS), and the pin is the promise to rest the window
;;; on it — now, and again when the replay that follows an attach replaces
;;; the folded rows. The two row lists are different lengths (the preview page
;;; ends near the block; the replay carries the whole session), so the line
;;; index is recomputed, never remembered.

;;; Written where a search hit is chosen, read by APPLY-TRANSCRIPT-PIN, and
;;; dropped by every gesture that moves the window off it on purpose — a
;;; scroll, a submit, a session switch.
(nlk:define-side-table transcript-pins (:weakness :key)
  "Weak memo: model -> (SESSION-ID . LOG-POSITION), the block the model's
transcript is resting on, or absent.")

(defun transcript-pin-p (model session-id &aux (pin (transcript-pins model)))
  "Whether MODEL's transcript is pinned to a block of SESSION-ID."
  (and pin (equal (car pin) session-id) t))

(defun transcript-shown-session-id (model &aux (preview (chat-model-transcript-preview model)))
  "The session MODEL's transcript shows right now: the armed preview's while
a dialog holds one, the attached session's otherwise — VISIBLE-TRANSCRIPT-STATE's
substitution, said as an id."
  ;; A pin is honored against the transcript the operator is looking at, which
  ;; is the previewed session for as long as the dialog previews one.
  (if (and preview (chat-model-dialog model))
      (car preview)
      (chat-model-session-id model)))

(defun transcript-line-for-position (model position &aux (refs (chat-model-line-rows model)))
  "The projection line MODEL's block at durable log POSITION starts on — the
first line whose owning row carries that position, else the first whose row
sits past it, because a fold that landed the block's row one lane over still
lands the operator in the right place."
  ;; NIL when the transcript has not folded that far: no line is invented for
  ;; a block that is not there.
  (position-if (lambda (ref)
                 (and (transcript-row-p ref) ref.log-position (>= ref.log-position position)))
               refs))

(defun apply-transcript-pin (model &aux (pin (transcript-pins model)))
  "Rest MODEL's transcript window on its pinned block: the block's first line
becomes the window's top row and the follow is off, because reading a found
block is not reading the tail."
  ;; Called from COMPOSE-TRANSCRIPT-VIEWPORT — the one pass that holds both
  ;; the folded rows and the line table they paint as — so every render
  ;; re-seats the window while the pin stands: a live frame moves the session,
  ;; never the promise. => T when the block was found and the window is there.
  (when (and pin (chat-model-band model)
             (equal (car pin) (transcript-shown-session-id model)))
    (when-let (line (transcript-line-for-position model (cdr pin)))
      (setf (band-window-offset (chat-model-band model)) line
            (chat-model-follow-bottom model) nil)
      t)))

(defun pin-transcript-position (model session-id position)
  "Pin MODEL's transcript on SESSION-ID's block at durable POSITION — a
conversation-search hit — and repaint, which lands the window on it
(APPLY-TRANSCRIPT-PIN runs inside that render and again in every later one,
the replay that follows an attach included). => T when the window moved."
  (setf (transcript-pins model) (cons session-id position))
  (render-state model)
  (transcript-pin-p model session-id))

(defun clear-transcript-pin (model)
  "Drop MODEL's viewport pin: the window is the operator's again."
  (forget-transcript-pins model)
  model)

(defun compose-status-block (model width &optional blank-above)
  "The live block the transcript ends on: the bar, what a retried provider
said (RETRY-REASON-LINES), queued prompts, and a row under them the newest
thought of its run (LIVE-THOUGHT-ROWS) -- no rows while nothing runs or
waits. BLANK-ABOVE: the transcript's last row is canvas already. Pending
execs belong to history."
  ;; The block scrolls with the transcript and the scroll gutter runs down
  ;; through it, one canvas row off the composer. Without this, a block
  ;; fixed between them would leave the gutter short of the box. A window
  ;; with the rows holds the bar off the work above by one canvas row; a
  ;; short window and the home screen's logo keep theirs.
  (bind ((status (first (chat-model-status-chrome-key model)))
         (height (chat-model-term-height model))
         (free (- height (shell-overhead height (composer-visible-rows model))
                  (or (composer-dropdown-geometry model) 0)))
         (spaced (and (shell-footer-p height) (>= free 8) (not (home-screen-p model))))
         (limit (max 1 (ceiling (- free (if spaced 1 0)) 2)))
         ;; A retry's words under the bar, then the queued prompts.
         ((said said-plains) (retry-reason-lines model width))
         ((steers steer-plains) (pending-steer-lines model width))
         (lines (append said steers)) (plains (append said-plains steer-plains))
         (count (length lines)) (room (max 0 (- limit 2)))
         (kept (if (> count room) (max 0 (1- room)) count))
         ;; On the column of the queued prompts it counts.
         ;; A painted row of the block like the ones it counts, so it fills
         ;; the band's width on its canvas and is cut where they are.
         ((omission omission-plain)
          (when (and (> count room) (plusp room))
            (bind (((rows plains)
                    (llines-rows (list (make-lline :text (format nil "… ~d status lines omitted" (- count kept))
                                                   :tone :muted :dim t :role :steer-band-cont))
                                 width)))
              (values (first rows) (first plains)))))
         ;; The thought stands a row off the bar and its lines, in the room
         ;; they leave it: a paragraph taller than that keeps its newest
         ;; rows, where its words arrive.
         (budget (max 0 (- room kept (if omission 1 0) 1)))
         ((thought thought-lines) (and status (plusp budget) (live-thought-rows model width)))
         (thought-rows (last thought budget)) (thought-plains (last thought-lines budget))
         ;; Under the bar the block keeps the most rows the turn's thoughts
         ;; have taken, in the room it has: a shorter paragraph after a
         ;; longer one leaves canvas, so a window following the tail holds
         ;; still.
         (beneath (if thought-rows (1+ (length thought-rows)) 0))
         ;; Over the bar, canvas keeps the running turn's run and the row
         ;; off it as tall as they have stood (RUN-LIFT).
         (lift (if status (run-lift model (if (and spaced (not blank-above)) 1 0)) 0))
         (pad (if (and status (plusp budget))
                  (max 0 (- (min (1+ budget) (live-mark model (nth-value 2 (bar-lane model)) :under beneath))
                            beneath))
                  0)))
    (flet ((lane (bar rows more blank under fill)
             (append (and spaced (not blank-above) (list blank))
                     (make-list lift :initial-element fill) (and status (list bar))
                     rows (and omission (list more))
                     (and under (list blank)) under
                     (make-list pad :initial-element fill))))
      (if (or status (plusp kept))
          (list (lane status (subseq lines 0 kept) omission "" thought-rows "")
                (lane (and status (plain-frame-row status)) (subseq plains 0 kept)
                      omission-plain "" thought-plains "")
                (lane :status (make-list kept :initial-element :status) :status nil
                      (make-list (length thought-rows) :initial-element :status) nil))
          (list nil nil nil)))))

(defun compose-transcript-viewport (model width lines plains refs &aux (base-count (length lines))
                                                                       (tail-lines '())
                                                                       (tail-plains '())
                                                                       (tail-refs '()))
  "Stack the live chrome under the projected transcript rows and hand the
result to the viewport."
  ;; Never mutates LINES/PLAINS/REFS — the projection cache shares them across
  ;; animation ticks.
  ;;
  ;; The chrome accumulates in lanes of its own rather than onto the
  ;; projection: the base is the same list object for every animation frame,
  ;; so appending a handful of chrome rows to it copied every transcript row
  ;; three times, once per chrome region, on every frame — and the copy grew
  ;; with the conversation.
  ;; The live block is the transcript's last rows.
  (setf (chat-model-status-chrome-key model) (status-chrome-key model width)
        (values tail-lines tail-plains tail-refs)
        (values-list (setf (status-blocks model)
                           (compose-status-block
                            model width
                            (let ((last (car (last lines))))
                              (and last (blank-band-line-p last width (canvas-fill-bg))))))))
 ;; An empty band is the home screen when the predicate holds (Zig
 ;; activeScreen): logo and tip. Otherwise it stays blank — the
 ;; connection narrative lives in the composer placeholder now. A home that
 ;; comes back -- a dialog closed over it, /clear -- is the launch's again,
 ;; glint and all; the launch plays its own (NLT:INIT).
 (let ((home (home-screen-p model)))
   (when (and home (null (chat-model-home-painted model)))
     (play-mark-glow model :shine :launch t))
   (setf (chat-model-home-painted model) home)
   (when (and (null lines) home)
     (setf (values lines plains) (home-band-lines model width)
           refs (make-list (length lines))
           base-count (length lines))))
 ;; Stamp the transcript's own hit-test lane — its rows, links and hover
 ;; targets. The selection wash is NOT applied here: a mark is a screen
 ;; region, so the frame's overlay (SELECTION-OVERLAY) washes the cells the
 ;; frame paints, whichever region painted the row.
 (setf (chat-model-line-plain model)
       (fill-line-vector (chat-model-line-plain model) plains base-count tail-plains)
       (chat-model-line-rows model)
       (fill-line-vector (chat-model-line-rows model) refs base-count tail-refs))
 (sync-viewport-geometry model)
 ;; The window takes the two row lists as they are, no join and re-split: the
 ;; rows keep the identity the band-row cache keys on, and the projected base
 ;; stays one list object across animation frames.
 ;; The pinned prompt band reads the projection's prompt spans, never its painted
 ;; rows: the refs lane is the one place that says which lines the operator's
 ;; prompts own, and it is rebuilt with the projection. Keyed by the base list
 ;; object, so a window showing a cached projection finds that projection's
 ;; spans and no other's.
 (setf (band-prompt-spans lines) (refs-prompt-spans refs)
       (band-disclosure-spans lines)
       (refs-disclosure-spans refs (chat-model-expanded-tool-call model) width))
 (band-window-set-lines (chat-model-band model) lines tail-lines (not (chat-model-follow-bottom model)))
 ;; A found-block pin is honored HERE, the pass that holds both the folded
 ;; rows and the line table they paint as: the window rests on the block
 ;; while the pin stands — a live frame moves the session, never the promise
 ;; — and the follow below must not snap a pinned window to the tail. After
 ;; the window has taken the lines: an offset is clamped to the content it
 ;; is set against, and set against the tail projected before the pin; without
 ;; that clamp, a block above that tail would land the window at the tail's top.
 (apply-transcript-pin model)
 ;; Follow the tail only while pinned: an operator reading history keeps
 ;; their place as new rows stream in below.
 (when (chat-model-follow-bottom model)
   (band-window-goto-bottom (chat-model-band model))))

;;; --- transcript scrolling ---------------------------------------------------
;;; PgUp/PgDn and the mouse wheel move the transcript window; every other
;;; key stays with the composer. Scrolling to the bottom re-pins the follow;
;;; submitting always re-pins, so the reply is visible.

(defun scroll-transcript (model action &optional (count 1))
  "Apply one scroll ACTION to the transcript window and update the
follow-bottom pin from where the window landed."
  ;; A page is the window's height. An explicit scroll is the operator taking
  ;; the window back from a found-block pin: the pin is spent the moment they
  ;; move it.
  (clear-transcript-pin model)
  (let ((window (chat-model-band model))
        (up (member action '(:page-up :line-up :home))))
    ;; Up is reading intent, even when a retained tool trail just collapsed
    ;; to less than one screen. Grow first, then spend the gesture on those
    ;; rows; a zero local range must not re-pin and strand older history.
    (when up
      (setf (chat-model-follow-bottom model) nil)
      (grow-transcript-projection model :whole (eq action :home)))
    (ecase action
      (:page-up (band-window-scroll window (- window.height)))
      (:page-down (band-window-scroll window window.height))
      (:line-up (band-window-scroll window (- count)))
      (:line-down (band-window-scroll window count))
      (:home (band-window-goto-top window))
      ;; End asks for the newest rows, as pinning the tail does: it spends a
      ;; place held over shrunk content (BAND-WINDOW-HOLD), or End would
      ;; land on the held offset with the rows below it blank, and the next
      ;; notch up would stop short of the history above it.
      (:end (forget-band-window-hold window)
            (band-window-goto-bottom window)))
    (setf (chat-model-follow-bottom model)
          (band-window-at-bottom-p window))
    (unless up (grow-transcript-projection model))))

(defun pin-transcript-bottom (model)
  "Re-pin the follow and snap to the tail."
  ;; Coming home to the tail spends a found-block pin: the operator asked for
  ;; the newest rows.
  (clear-transcript-pin model)
  (setf (chat-model-follow-bottom model) t)
  (forget-band-window-hold (chat-model-band model))
  (band-window-goto-bottom (chat-model-band model)))

