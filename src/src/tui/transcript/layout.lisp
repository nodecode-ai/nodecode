;;;; layout.lisp --- layout: logical lines to painted rows, and row chrome. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- layout: logical lines to painted rows ---------------------------------

(defstruct (seg (:constructor make-seg (&key (text "") tone bold italic dim
                                             underline strike link ground glint)))
  "One paint run: text plus tone and attribute overrides."
  ;; LINK wraps the painted run in an OSC-8 hyperlink under a painting profile
  ;; — so a terminal that follows hyperlinks opens the run itself, whether it
  ;; names a page or a file. GROUND, a tone or a style, is the background the
  ;; run stands on (GROUNDS-SHOWN-P); GLINT its span's (MSPAN-GLINT).
  text tone bold italic dim underline strike link ground glint)

(nlk:access (s seg) (seg seg))

(defun osc8-wrap (content url)
  "CONTENT wrapped in the OSC-8 hyperlink pair for URL."
  ;; The renderer decodes the sequence into cell links and treats it as
  ;; zero-width, so wrap math and diffing survive it. URL is content too (a
  ;; markdown link's target is the model's to write): a control codepoint in
  ;; it would end the OSC early — at the renderer's parser or at the
  ;; terminal's — and paint the rest of the URL as cells the row never
  ;; measured, so those are dropped, not pictured.
  (format nil "~c]8;;~a~c\\~a~c]8;;~c\\"
          #\Escape
          (remove-if (lambda (ch) (control-codepoint-p (char-code ch))) url)
          #\Escape content #\Escape #\Escape))

(defun canvas-fill-bg ()
  "The canvas RGB, or NIL for a transparent theme or the :none profile."
  ;; Missing canvas RGB means terminal-default background. Own panel fills
  ;; still paint under a color profile; transparency is not colorlessness.
  (unless (eq (effective-color-profile) :none)
    (visual-style-bg (style-for-tone :background))))

(defun canvas-rgb ()
  "The rgb a cell with no ground of its own shows: the theme's canvas, or a
transparent theme's terminal's own background as it answered OSC 11 -- NIL
while the terminal has not said, which nothing guesses at."
  ;; What a fade goes out into and a wash lies over. A transparent theme's
  ;; nominal canvas is not what is on the screen: without this, a fade would
  ;; end on a black box and a word would come up out of it.
  (or (visual-style-bg (style-for-tone :background)) *terminal-canvas*))

(defun grounds-shown-p ()
  "Whether an answer's grounds paint -- its chips, bands and slabs: under
256 colours and more, where a fill a step off the canvas still is one. In
16 it would land on black or white."
  (member (effective-color-profile) '(:truecolor :ansi256)))

(defun seg-render (seg fill-bg &key force-bg shadow)
  "Paint SEG, including the session-id's opacity dim over the title palette."
  ;; SHADOW lays the shadow over the finished style: the text at less of the
  ;; light, the fill and every background left as they are.
  (let* ((tone (or seg.tone :foreground))
         (session-id-p (eq tone :session-id))
         (style (style-for-tone
                 (if session-id-p :session-title tone))))
    (when session-id-p
      (let ((fg style.fg) (bg (or style.bg (visual-style-bg (style-for-tone :background)))))
        (when (and fg bg)
          (setf style (style-with
                       style :fg (blend-rgb
                                  fg bg 63)))))) ; Zig's 4.5:1 contrast floor
    (when seg.dim
      (setf style (dim-style style)))
    (when (or seg.bold seg.italic seg.underline seg.strike)
      (setf style (style-with style :bold (or seg.bold style.bold)
                                    :italic (or seg.italic style.italic)
                                    :underline (or seg.underline style.underline)
                                    :strike (or seg.strike style.strike))))
    ;; A shadowed or dimmed row is its words at less light, standing on
    ;; nothing of its own.
    (when (and seg.ground (not seg.dim) (not shadow) (grounds-shown-p))
      (setf style (style-with style :bg (visual-style-bg (style-for-tone seg.ground)))))
    (when (and fill-bg (or force-bg (null style.bg))) (setf style (style-with style :bg fill-bg)))
    (when (and shadow (not (eq tone :thought)))
      ;; A thought's disclosure label is chrome, not its shadowed prose.
      (setf style (shadow-style style)))
    (let ((painted (styled seg.text style)))
      (if (and (stringp seg.link)
               (not (eq (effective-color-profile) :none)))
          (osc8-wrap painted seg.link)
          painted))))

(defun segs-text (segs)
  "The plain text SEGS paint."
  ;; The hit-test and copy lane derives from the painted segments through here
  ;; rather than being rebuilt beside them, so the two lanes cannot disagree
  ;; about what a row says.
  (apply #'concatenate 'string (mapcar #'seg-text segs)))

(defun segs-display-width (segs)
  "Columns SEGS occupy."
  (loop for s in segs sum (text-display-width s.text)))

(defun spaces (count)
  "COUNT spaces, none when COUNT is not positive."
  (make-string (max 0 count) :initial-element #\Space))

(defun blank-frame-row (width bg &aux (spaces (spaces width)))
  "A full-WIDTH canvas row: painted spaces under a profile, plain under :none."
  (if bg (styled spaces (vstyle :bg bg)) spaces))

(defun clip-segs (segs width &key mark)
  "SEGS when they paint within WIDTH columns; otherwise the longest prefix
of them that does."
  ;; MARK, when given, is the elision text painted in the cut seg's tone at
  ;; the edge (the chrome … of CLIP-TEXT-TO-WIDTH, the docked row's
  ;; gap-dot-pad ` … '); its own display width comes out of the budget, so a
  ;; mark wider than WIDTH is dropped and the cut stays bare. Without it the
  ;; cut is bare too, the Zig `.wrap = .none' panel truncation. The one row
  ;; painter's overflow guard: a row never exceeds the width it was asked for,
  ;; so a lane that cannot fit — the live turn bar naming a long model on a
  ;; narrow window — elides instead of spilling past the edge into the
  ;; implicit wrap that would shift every row below.
  (if (<= (segs-display-width segs) width)
      segs
      (let* ((mark (and mark (>= width (text-display-width mark)) mark))
             (budget (max 0 (- width (if mark (text-display-width mark) 0))))
             (used 0)
             (kept '()))
        (dolist (seg segs)
          (let ((w (text-display-width seg.text)))
            (cond ((<= (+ used w) budget)
                   (push seg kept)
                   (incf used w))
                  (t
                   (let ((head (take-display-prefix seg.text
                                                    (- budget used))))
                     (when (plusp (length head))
                       (push (copy-with seg seg :text head) kept))
                     (when mark
                       (push (copy-with seg seg :text mark :link nil) kept)))
                   (return)))))
        (nreverse kept))))

(defun render-row (prefix-segs body-segs fill-bg width &key shadow)
  "One painted viewport row, never wider than WIDTH."
  ;; A fill paints the row background to WIDTH; bare rows end at their content
  ;; (Zig lineWithChrome). SHADOW paints the row's text at less light; the
  ;; fill is not text and keeps its tone.
  (let* ((segs (clip-segs (append prefix-segs body-segs) width :mark "…"))
         (content (apply #'concatenate 'string
                         (mapcar (lambda (s) (seg-render s fill-bg :shadow shadow))
                                 segs))))
    (if fill-bg
        (pad-frame-row content (segs-display-width segs) width fill-bg)
        content)))

(defun fragment-segs (line s e &aux (text (lline-text line))
                                    (segs '())
                                    (pos s))
  "Paint segments for the fragment [S,E) of LINE: spans styled, hidden marker
runs skipped, unspanned runs in the line's base style."
  ;; An unspanned run paints as a span that sets nothing: the line's own style.
  (flet ((run (from to &optional (span (load-time-value (mspan 0 0))))
           (when (< from to)
             (push (make-seg :text (subseq text from to)
                             :tone (or span.tone line.tone)
                             :bold (or span.bold line.bold)
                             :italic (or span.italic line.italic)
                             :dim (or span.dim line.dim)
                             :underline (or span.underline line.underline
                                            (and *hover-target* (equal span.link *hover-target*) t))
                             :strike span.strike
                             :link span.link
                             :ground span.ground
                             :glint span.glint)
                   segs))))
    (dolist (span line.spans)
      (let ((ss (max s span.start))
            (se (min e span.end)))
        (when (< ss se)
          (run pos ss)
          (unless span.hidden (run ss se span))
          (setf pos (max pos se)))))
    (run pos e))
  (nreverse segs))

(defparameter *aside-min-width* 40
  "Narrowest frame a line's aside stands in; below it the words take the row.")

(defun lline-aside-shown (line width)
  "LINE's aside SEG where WIDTH has room for it, else NIL."
  (and line.aside (>= width *aside-min-width*) line.aside))

(defun lline-widths (line width)
  "(values first-row-width continuation-width) for LINE at frame WIDTH."
  (bind (((_ _ first-indent cont-indent)
          (body-gutter line.role))
         (aside (lline-aside-shown line width)))
    (values (max 1 (- width first-indent *transcript-right-padding*
                      (if aside (+ 2 (text-display-width (seg-text aside))) 0)))
            (max 1 (- width cont-indent line.hang
                      *transcript-right-padding*)))))

(defun lline-fragments (line width &aux (text (lline-text line)))
  "LINE's rows at frame WIDTH, (START . END) each: broken where its painted
words fit, its hidden markers counted as nothing."
  (multiple-value-bind (first-width cont-width) (lline-widths line width)
    (break-fragments text first-width cont-width
                     :soft t
                     :hidden (loop with hidden = nil
                                   for span in line.spans
                                   when span.hidden
                                     do (fill (or hidden (setf hidden (make-array (length text)
                                                                                  :element-type 'bit
                                                                                  :initial-element 0)))
                                              1 :start span.start :end (min span.end (length text)))
                                   finally (return hidden)))))

;;; Every span carrying the same URI underlines while it holds — the
;;; terminal-shaped answer to a cursor that changes shape, and the one thing
;;; that tells a reader a path is a thing to click before they click it. Read
;;; in FRAGMENT-SEGS and revalidated by the render memo, so the only rows that
;;; repaint under a moving pointer are the rows with targets.
(defvar *hover-target* nil
  "The target the pointer is resting on this render (RENDER-STATE binds it
from the model), or NIL.")

;;; LLINE-ROWS records them in the walk that builds the row, so a click finds
;;; the link under it (LINK-AT-POSITION) whatever the color profile paints —
;;; the OSC-8 wrap only rides a painting profile. Keyed on the row string
;;; itself, which the render memo keeps EQ-stable across frames and every lane
;;; after it carries untouched; weak, so a row the transcript dropped takes
;;; its entry with it.
(nlk:define-side-table plain-row-links (:weakness :key :synchronized t)
  "Plain transcript row or final chrome row -> ((START END TARGET) ...),
character ranges in its style-free text. TARGET is a URI or (:COMMAND . NAME).")

(defun link-plain-row (row links)
  "ROW, with LINKS recorded as its PLAIN-ROW-LINKS when there are any."
  (when links (setf (plain-row-links row) links))
  row)

;;; LLINE-ROWS marks the rows a wrapped line goes on in, keyed on the plain row
;;; the way its links are, so the keyboard selection can take a logical line
;;; -- its line motions and its whole-line mark -- where the band shows rows.
(nlk:define-side-table plain-row-continues (:weakness :key :synchronized t)
  "Plain transcript row -> T when it goes on the logical line wrapped above
it: every row LLINE-ROWS lays after a line's first (SELECT-LOGICAL-LINE).")

(defun seg-links (segs start)
  "The (START END TARGET) ranges of SEGS' linked segs, SEGS laid from column START."
  (loop for seg in segs
        for from = start then end
        for end = (+ from (length (seg-text seg)))
        when (seg-link seg) collect (list from end (seg-link seg))))

(defparameter +ground-fade-floor+ 8
  "The fewest cells a ground's fade takes, short of a ground too short to
hold them.")

(defun ground-segs (segs ground text-end width)
  "SEGS, a row's paint, standing on GROUND, (TONE FROM TO FADE) (LLINE-GROUND):
TONE's background from column FROM below TO -- two past TEXT-END, the
words' end, for :TEXT, the text's right edge for :EDGE -- and no further,
nor past the edge. Under truecolor, over the canvas the terminal shows
(CANVAS-RGB), the last half of that span (at least +GROUND-FADE-FLOOR+
cells, at most FADE) goes out into it, under the last words; while that
canvas is unknown the span stands solid. A seg standing on its own ground
keeps it; a row short of the ground's end is padded to it."
  ;; A band is as long as its words: a fixed 36-cell fade past them runs a
  ;; short heading's band four times its length and a lead run's under the
  ;; prose after it. A fade toward a canvas the terminal does not show would
  ;; end on a box of the wrong colour: so it goes out into the canvas that
  ;; shows or not at all, and always all the way.
  (destructuring-bind (tone from to fade) ground
    (let* ((edge (- width *transcript-right-padding*))
           (canvas (canvas-rgb))
           (want (case to (:text (+ text-end 2)) (:edge edge) (t to)))
           (end (max (min from edge) (min want edge)))
           (fade (if (and fade canvas (eq (effective-color-profile) :truecolor))
                     (max 0 (min fade (- end from 1) (max +ground-fade-floor+ (ceiling (- end from) 2))))
                     0))
           (to (- end fade))
           (fill (visual-style-bg (style-for-tone tone)))
           (shown (segs-display-width segs))
           (column 0))
      ;; A cell's key says what it stands on: nothing, the solid ground, or
      ;; -- each its own -- a cell of the fade.
      (flet ((key (at) (cond ((or (< at from) (>= at end)) :none) ((< at to) :solid) (t at)))
             (ground (key) (case key
                             (:none nil)
                             (:solid tone)
                             (t (vstyle :bg (blend-rgb fill canvas (round (* 100 (- end key 1)) fade)))))))
        (loop for seg in (if (< shown end)
                             (append segs (list (make-seg :text (spaces (- end shown)))))
                             segs)
              nconc (loop with text = seg.text
                          with start = 0
                          with key = (key column)
                          for i from 0 below (length text)
                          for here = (key column)
                          unless (eql here key)
                            collect (copy-with seg seg :text (subseq text start i) :ground (or seg.ground (ground key)))
                              into pieces
                            and do (setf start i key here)
                          do (incf column (char-display-width (char text i)))
                          finally (return (nconc pieces (list (copy-with seg seg :text (subseq text start)
                                                                             :ground (or seg.ground (ground key))))))))))))

(defun lline-rows (line width)
  "Wrap LINE at WIDTH => (values PAINTED PLAIN GLINTS): the painted rows,
the style-free text of each one, which is the mouse hit-test and
selection-copy source, and each row's glints, ((X0 X1 AT) ...) the cells a
glint crosses and the time it set off (GLINT-OVERLAY)."
  ;; One walk over one set of fragment segs feeds both lanes, so the hit-test
  ;; columns cannot drift away from the paint; the same walk records a plain
  ;; row's links on it (*PLAIN-ROW-LINKS*). The gutter mark paints on the
  ;; first row; continuations indent to the role's continuation column plus
  ;; the hang, the line's HANG-MARK painted over the head of the hang. A row
  ;; stands on the line's ground, a heading's band or a code block's slab:
  ;; every row of it, or, for a ground ending at a column, the first.
  (bind (((lead mark first-indent cont-indent)
          (body-gutter line.role)))
    (let ((fill-bg (or (and line.fill
                            (visual-style-bg (style-for-tone line.fill)))
                       ;; Unfilled rows still paint the canvas to full width:
                       ;; the Zig frame memset means no row ever ends on the
                       ;; terminal's default background (NIL under :none, so
                       ;; plain-text goldens keep their ragged right edge).
                       (canvas-fill-bg)))
          (ground (and line.ground (not line.shadow) (not line.dim) (grounds-shown-p) line.ground))
          (gutter (list (make-seg :text lead)
                        (make-seg :text mark
                                  :tone (or line.gutter-tone
                                            line.tone)
                                  :italic line.gutter-italic)))
          (cont (let ((marks (alexandria:ensure-list line.hang-mark)))
                  (list* (make-seg :text (spaces cont-indent))
                         (append marks (list (make-seg :text (spaces (- line.hang (segs-display-width marks))))))))))
      (if line.rule
          ;; A full-width thematic-break rule (Zig markdown_rule_fill).
          (let ((rule (make-string (lline-widths line width) :initial-element #\─)))
            (values (list (render-row (list (make-seg :text lead))
                                      (list (make-seg :text rule :tone line.tone :dim line.dim))
                                      fill-bg width
                                      :shadow line.shadow))
                    (list (concatenate 'string lead rule))
                    (list nil)))
          (loop with aside = (lline-aside-shown line width)
                for (s . e) in (lline-fragments line width)
                for firstp = t then nil
                for prefix = (if firstp gutter cont)
                for words = (fragment-segs line s e)
                for segs = (if (and firstp aside)
                               ;; The aside ends where the words may: the gap
                               ;; before it is the rest of the first row.
                               (append words
                                       (list (make-seg :text (spaces (max 2 (- width first-indent *transcript-right-padding*
                                                                               (segs-display-width words)
                                                                               (text-display-width (seg-text aside))))))
                                             aside))
                               words)
                for paint = (if (and ground (or firstp (not (integerp (third ground)))))
                                (ground-segs (append prefix segs) ground
                                             (+ (segs-display-width prefix) (segs-display-width words))
                                             width)
                                (append prefix segs))
                collect (render-row nil paint fill-bg width :shadow line.shadow)
                  into painted
                collect (let* ((prefix (if firstp
                                           (concatenate 'string lead mark)
                                           (segs-text cont)))
                               ;; Narrow before attaching EQ-keyed link ranges:
                               ;; the string returned is the click lane's key.
                               (plain (link-plain-row (nlk:narrow-string
                                                       (uiop:strcat prefix (segs-text segs)))
                                                      (seg-links segs (length prefix)))))
                          (unless firstp (setf (plain-row-continues plain) t))
                          plain)
                  into plains
                collect (loop for seg in paint
                              for from = 0 then to
                              for to = (+ from (text-display-width seg.text))
                              when (and seg.glint (not seg.dim) (not line.shadow) (grounds-shown-p))
                                collect (list from to seg.glint))
                  into glints
                finally (return (values painted plains glints)))))))

(defun gutter-mark-p (char)
  "Whether CHAR is a mark the transcript's gutter paints rather than a word:
a role's mark (*BODY-GUTTERS*: a disclosure's ▸, a notice's ■, tool
output's ⎿ ...), or the ▌ edge and the ↦ a wrapped line goes on under."
  (or (find char "▌↦")
      (loop for (nil nil mark) in *body-gutters*
            thereis (and (plusp (length mark)) (char= char (char mark 0))))))

(defun without-gutter-marks (row)
  "ROW, one frame row's plain text, with the gutter marks it opens on
blanked (GUTTER-MARK-P): a disclosure's ▸, a notice's ■, tool output's ⎿,
a code block's and a quote's edges and the ↦ a wrapped line goes on under
are the gutter's, not the words', and a copy leaves them behind."
  ;; The copy reads what was painted (FRAME-SELECTION-TEXT), so the marks come
  ;; out there, a blank for each column they held, and the columns a selection
  ;; is cut at stay where they were. Prose that opened on one of these glyphs
  ;; would lose it the same way; a model writes none.
  (let ((row (copy-seq row)))
    (loop for i from 0 below (length row)
          for ch = (char row i)
          while (or (char= ch #\Space) (gutter-mark-p ch))
          do (setf (char row i) #\Space))
    row))

(defun llines-rows (llines width)
  "=> (values PAINTED PLAIN) for a whole list of llines wrapped at WIDTH."
  (loop for line in llines
        for (rows texts) = (multiple-value-list (lline-render line width))
        append rows into painted
        append texts into plains
        finally (return (values painted plains))))

;;; ROW-BODY-LLINES keeps a row's unchanged logical lines EQ-stable across
;;; re-renders, so during streaming only the tail lline of the growing row
;;; pays wrap and paint; everything above it is a fixnum-and-EQ probe. Weak
;;; keys, so llines dropped with their row release their rendered rows.
(nlk:define-side-table lline-render-cache (:weakness :key)
  "Weak per-lline memo: lline -> #(width palette profile hovered-span
(rows plain-rows)).")

;;; The memo holds a line's painted rows for as long as the line lives. A
;;; line dies with its row's memos, which a full render keeps only for what
;;; it projects (FORGET-UNPROJECTED-ROWS); what the memo keeps is kept
;;; narrow, and past this many lines -- a line held some other way -- it
;;; starts over. Without the limit, a line held some other way would keep
;;; its painted rows indefinitely.
(defparameter *lline-render-cache-limit* 4096
  "Logical lines whose painted rows LLINE-RENDER keeps before it starts over.")

(defparameter +glint-ms+ 800
  "Milliseconds a glint takes to cross a chip that has just arrived.")

(nlk:define-side-table row-glints (:weakness :key)
  "A painted row -> the glints it carries, ((X0 X1 AT) ...): the cells of a
chip, past the row's own start, and the time it arrived (LLINE-ROWS), so the
frame's GLINT-OVERLAY finds it on screen.")

(defun lline-render (line width)
  "(values ROWS PLAIN-ROWS) for LINE at WIDTH, memoized on the lline's
identity."
  ;; The entry revalidates on width, theme palette, color profile and the
  ;; pointer: the render inputs LLINE-ROWS reads outside the lline itself. The
  ;; pointer only ever changes a line that paints the target it moved on or
  ;; off of, so the entry keys on the one span of the line's own that the
  ;; pointer rests on (usually none), and a pointer that never touched the
  ;; line leaves the entry standing -- crossing a path in one card does not
  ;; repaint every other card's. Callers must not mutate LINE or the returned
  ;; lists: the memo shares both across frames (the same discipline
  ;; ROW-DISPLAY-BLOCK already imposes).
  (when (> (hash-table-count *lline-render-cache*) *lline-render-cache-limit*)
    (clrhash *lline-render-cache*))
  (values-list
   (memoized (lline-render-cache line)
     (width *palette* *color-profile*
      (and *hover-target* (find *hover-target* line.spans :key #'mspan-link :test #'equal)))
     (multiple-value-bind (rows plain glints) (lline-rows line width)
       ;; Plain rows already carry their final representation and link ranges.
       ;; Copying them here would lose the identity the mouse lane reads.
       (let ((rows (mapcar #'nlk:narrow-string rows)))
         (loop for row in rows
               for glint in glints
               when glint do (setf (row-glints row) glint))
         (list rows plain))))))

;;; --- row chrome: transcript rows to logical lines --------------------------
;;; The per-kind table from src/tui/transcript.zig: assistant prose in plain
;;; foreground (NOT the assistant green — that tone marks identity chrome),
;;; reasoning as that same prose under a shadow (the text at less light
;;; on the same canvas, not a quieter tone and not a darker band), user
;;; prompts as filled blocks, tool headers bold-named
;;; with muted args,
;;; dimmed tool output behind a "⎿" gutter, notices as "■" system blocks, and
;;; failures as "■ Error:" diagnostic blocks.

(defun block-pad-lline (tone &aux (cache (load-time-value (make-hash-table :test 'eq))))
  "One blank padding row — the Zig block_padding viewport line every
transcript block owns above and below its body (render_plan.zig
collectTranscriptRows)."
  ;; TONE, when non-NIL, extends a filled block's background through the pad
  ;; so the block reads as a card rather than a bare painted line; NIL leaves
  ;; the pad on the canvas, which is what the unfilled styles (assistant
  ;; prose, reasoning, tool blocks) render. The :status role keeps the
  ;; plain-text projection at the Zig writer's single space. Interned per
  ;; tone: pads are immutable like every rendered lline (the hover pass copies
  ;; before it writes), so every block shares the same pad object and
  ;; LLINE-RENDER memoizes its paint once per theme.
  (alexandria:ensure-gethash tone cache (make-lline :text "" :role :status :fill tone)))

(defun notice-url-spans (text)
  "Link spans for every bare http(s) URL in TEXT."
  ;; A notice is not markdown -- nothing in it is parsed, and an organism
  ;; saying `**` means `**` -- so this is the one run of INLINE-SPANS a
  ;; notice gets: the scanner over BARE-URL-END, the link tone, and the
  ;; target the OSC-8 wrap and the click both read (MSPAN-LINK). Without it a
  ;; notice carries no spans at all, and the address a cell says -- the
  ;; one line an operator is there to copy -- paints as prose.
  (loop with position = 0
        for start = (position #\h text :start position :test #'char-equal)
        for end = (and start (bare-url-end text start 0))
        while start
        when end
          collect (mspan start end :tone :link :underline t :link (subseq text start end))
        do (setf position (or end (1+ start)))))

(defun system-notice-llines (text error-p &aux (tone (if error-p :diagnostic-block :system-block)))
  (loop for line in (or (split-lines text) (list ""))
        for firstp = t then nil
        collect (make-lline :text line :tone tone :italic t
                            :role (if firstp (if error-p :diagnostic :system) :system-cont)
                            :gutter-tone tone :gutter-italic t
                            :spans (notice-url-spans line)
                            :hang (or (list-marker-end line)
                                      (nth-value 1 (ppcre:scan "^ *(?:\\S+ {2,}|[•·] )?" line)))
                            :fill tone)))

;;; The group pass classifies every tool row on every projection, so a row's
;;; words are computed once per change of the fields they read.
(nlk:define-side-table row-activity-cache (:weakness :key)
  "Weak per-row memo for ROW-ACTIVITY: row -> #(name args metadata error-p
activity).")

(defun row-activity (row)
  "ROW's ACTIVITY (tool-summary.lisp): what its tool call did, in words — or NIL
for a row that is no tool call, or a tool that module has no words for."
  ;; The reducers replace these fields rather than mutate them, so identity is
  ;; the change signal.
  (when (eq :tool-call row.kind)
    (let ((name row.tool-name) (args row.tool-args) (metadata row.metadata) (error-p row.error-p))
      (memoized (row-activity-cache row) (name args metadata error-p)
        (tool-arguments-activity name args :metadata metadata :error-p error-p)))))

(nlk:define-side-table row-load-errors (:weakness :key)
  "Live-only shell-load failures on their originating Eval row, never durable tool metadata.")

(defun ingest-tool-load-error (state call-id turn-id text)
  "Attach a shell-only load failure to its exact Eval call. Return T if seated, NIL otherwise."
  (when (and (stringp call-id) (plusp (length call-id)))
    (when-let (row (find-if (lambda (row)
                             (and (eq :tool-call (transcript-row-kind row))
                                  (equal "eval" (transcript-row-tool-name row))
                                  (equal call-id (transcript-row-tool-call-id row))
                                  (equal turn-id (transcript-row-turn-id row))))
                           (transcript-rows state)))
      (setf (row-load-errors row) (append (row-load-errors row) (list text)))
      t)))

(defun tool-failure-text (row)
  "A tool's status suffix plus its explicit error code; shell-load errors remain live-only."
  ;; Only a code the result carries rides the word: without this, a stand-in
  ;; `EVAL-FAILED' behind `failed' would say the same thing twice.
  (let* ((shell-error (row-load-errors row))
         (status (if shell-error "error" (transcript-row-tool-status row)))
         (code (and (equal "eval" (transcript-row-tool-name row))
                    (if shell-error "SHELL-LOAD-FAILED"
                        (json-value (transcript-row-metadata row) :text "error" "code")))))
    (format nil "~a~@[ [~a]~]"
            (tool-status-text status (row-exit-document row)) code)))

(defun exit-end (exited &optional words)
  "EXEC-END of EXITED, an exit's document -- with WORDS, EXEC-END-WORDS."
  (funcall (if words #'exec-end-words #'exec-end)
           (json-value exited :integer "exit_code") (json-value exited :integer "shell_exit")))

(defun tool-status-text (status &optional exited)
  "The status word a tool card's header ends with, for the row's STATUS —
for a call that ran on in the background, how it ended by EXITED, the exit's
background_shell document: `exited 2', `failed', `interrupted', and nothing
for one that ended done, as a foreground call that finished says nothing."
  ;; The bare `exited' says that it ended and never how.
  (if (and (equal status "exited") (hash-table-p exited))
      (let ((words (exit-end exited t)))
        (if (equal words "done") "" (format nil " ~a" words)))
      (or (cdr (assoc status '(("running" . " running") ("error" . " failed") ("exited" . " exited")
                               ("stopped" . " stopped") ("background" . " in the background"))
                      :test #'equal))
          "")))

(defun tool-command-llines (row &key (status-text ""))
  "The call's own text as the card showed it before it had words: the
decoded eval snippet one viewport line per snippet line, a command on one line,
or the tool name bold with its raw arguments muted — STATUS-TEXT after the
last line."
  ;; The command is TOOL-COMMAND-STRING over the row's arguments, the kernel's
  ;; decode, shared with the checkpoint listing. A command line carries
  ;; SOURCE's syntax spans over the same-length TEXT: the flat branch
  ;; tokenizes the unflattened command — the newlines are what tell the lexer
  ;; where a comment ends. The status suffix is the one every card title ends
  ;; with (APPEND-STATUS-TEXT).
  (let* ((name (or (json-value row.tool-name :text) "…"))
         (command (tool-command-string row.tool-args))
         (args (substitute #\Space #\Newline (or row.tool-args "")))
         (llines (cond ((null command)
                        (list (append-lline-run
                               (make-lline :text name :tone :muted
                                           :spans (list (mspan 0 (length name)
                                                               :tone :tool :bold t)))
                               (if (plusp (length args)) (concatenate 'string " " args) "")
                               :tone :muted)))
                       ((equal name "eval") (command-llines command))
                       (t (list (make-lline :text (substitute #\Space #\Newline command)
                                            :tone :syntax-function
                                            :spans (command-token-spans command)))))))
    (append-status-text (car (last llines))
                        (if (row-load-errors row) "error" row.tool-status)
                        status-text
                        (json-value row.metadata :object "background_shell"))
    llines))

(defun command-llines (command &key dim)
  "COMMAND one lline per line, each painted by its own syntax spans: an eval
snippet as its card shows it, and an exec's command in the band and its view."
  (mapcar (lambda (line)
            (make-lline :text line :tone :syntax-function :dim dim
                        :spans (command-token-spans line)))
          (or (split-lines command) (list ""))))

(defun title-phrases-lline (phrases &key (verb-tone :tool)
                                 &aux (title (make-lline :text "" :tone :foreground)))
  "One title lline over PHRASES, (VERB TEXT DETAIL TARGET) each joined by
commas: the verbs bold in VERB-TONE, what they acted on in the
foreground, the aside after it, and — where the phrase names an address
(ACTIVITY-SOURCE-TARGET) — a span over exactly those words carrying what a
click opens."
  ;; The activity classifier decided what the source IS; this writes the words
  ;; and spans each run as it lands (APPEND-LLINE-RUN), so no path is ever read
  ;; back out of a title.
  (loop for (verb text detail target) in phrases
        for first = t then nil
        do (unless first (append-lline-run title ", "))
           (when verb
             (append-lline-run title verb :tone verb-tone :bold t)
             (when (plusp (length text)) (append-lline-run title " ")))
           (append-lline-run title text :link (span-target-uri target))
           (when detail (append-lline-run title (format nil " (~a)" detail))))
  title)

(defun append-lline-run (line text &rest style)
  "Write TEXT onto LINE's end as a span of its own under STYLE. => LINE."
  ;; The one way an lline grows: a title's phrases, a card's status word, a
  ;; diff header's stats. An empty TEXT leaves LINE exactly as it was, and a
  ;; STYLE that sets nothing adds the text in the line's own style, unspanned.
  (when (plusp (length text))
    (let ((start (length (lline-text line))))
      (setf (lline-text line) (concatenate 'string (lline-text line) text))
      (when (loop for (nil value) on style by #'cddr thereis value)
        (alexandria:appendf line.spans (list (apply #'mspan start (length line.text) style))))))
  line)

(defun append-title-detail (line text)
  "Append a muted detail in the shared transcript content column.
Compact labels hang under their detail; compound activity titles keep their layout."
  ;; TEXT is a note, a comment or a summary as its source holds it, folded to
  ;; one line here; a source with none (NIL) leaves LINE as it was.
  (when-let (detail (and text (nlk:one-line text)))
    (let* ((width (text-display-width (lline-text line)))
           (compact (and (eq *tool-detail* :standard) (<= width 6)))
           (hang (- *transcript-detail-column* (nth-value 2 (body-gutter :prose))))
           (pad (if compact (- hang 1 width) 0))
           (separator (if compact " " " · ")))
      (append-lline-run line (format nil "~va~a~a" pad "" separator detail) :tone :muted)
      (when (and compact (zerop (lline-hang line)))
        (setf (lline-hang line) hang))))
  line)

(defun append-status-text (title status status-text &optional exited)
  "Write STATUS-TEXT onto TITLE's end, spanned in the tone the tool STATUS
earns — :danger for an error or for a call that ran on in the background and
ended any way but done or stopped by EXITED, its exit's background_shell
document; :tool for a call still running, here or in the background; :muted
otherwise."
  ;; A card that is simply complete has no status text and TITLE is left
  ;; alone. Answers TITLE. A background `exited 2' read grey beside a
  ;; foreground `failed' in red; a grey `running' read as the rest of its
  ;; grey line, the one live word on it.
  (append-lline-run title status-text
                    :tone (cond ((or (equal status "error")
                                     (and (equal status "exited")
                                          (hash-table-p exited)
                                          (not (member (exit-end exited)
                                                       '("done" "stopped") :test #'equal))))
                                 :danger)
                                ((member status '("running" "background") :test #'equal) :tool)
                                (t :muted))))

(defun tool-note (row)
  "The call's comment: eval's leading ;; note, or an explicit tool argument."
  (or (when-let (activity (row-activity row)) (activity-note activity))
      (when-let (args (ignore-errors (shasht:read-json row.tool-args)))
        (or (json-value args :text "description") (json-value args :text "comment")))))

(defun disclosure-hover-lline (line)
  "A title accented on hover, with its shared line and spans left untouched."
  (copy-with lline line :tone :disclosure-hover :shadow nil
             :spans (mapcar (lambda (span) (copy-with mspan span :tone :disclosure-hover))
                            (lline-spans line))))

(defun row-edited-p (row)
  "Whether this call's durable result carries captured file changes."
  (and (eq :tool-call (transcript-row-kind row))
       (plusp (length (json-array (transcript-row-metadata row) "workspace_diff" "files")))
       t))

(defun tool-summary-words (row &aux (activity (row-activity row)))
  "What the call did, in words: its note, the files it edited, or its
activity's title. NIL for a call with none."
  (or (tool-note row)
      (when (row-edited-p row)
        (let ((files (json-array row.metadata "workspace_diff" "files")))
          (if (= 1 (length files))
              (json-value (aref files 0) :string "path")
              (format nil "~d files" (length files)))))
      (and activity (activity-title activity))))

(defun tool-summary-lline (row)
  "The tool's own identity, note and status, even when its result includes edits."
  (let* ((name (or (json-value row.tool-name :text) "…"))
         (title (title-phrases-lline (list (list (if (equal name "eval") "Eval" name) "" nil nil))))
         (summary (tool-summary-words row)))
    (append-title-detail title summary)
    (append-status-text title (if (row-load-errors row) "error" row.tool-status)
                        (tool-failure-text row)
                        (row-exit-document row))
    title))

(defun fork-tree-llines (row width &key (indent 0))
  "Call ROW's forks, one line each on the rails /sessions draws, while the
gateway says how they stand (session_fork_progress, client.lisp): a running
fork's calls and its newest note, an ended one's mark, time and the first
line of its answer. NIL for a call that forked nothing."
  ;; Without these, a call that forked stands as one line for minutes while
  ;; its forks work in sessions of their own, and nothing says what they do.
  (when-let (forks (and (hash-table-p row.metadata) (json-value row.metadata :array "forks")))
    (let* ((last (1- (length forks)))
           (names (map 'list (lambda (fork) (or (json-value fork :text "label") "")) forks))
           (room (min 24 (reduce #'max names :key #'text-display-width :initial-value 0)))
           ;; Each fork's calls, time and -- for one that did not complete --
           ;; how it ended: one cell, as wide as the widest, so every note
           ;; starts in one column (`1 call' and `12 calls' moved it).
           (stats (map 'list (lambda (fork &aux (status (json-value fork :string "status"))
                                                 (ms (json-value fork :integer "ms")))
                               (cons (format nil "  ~2d call~:p~@[ · ~a~]" (or (json-value fork :integer "calls") 0)
                                             (and ms (format-turn-elapsed ms)))
                                     (unless (member status '("running" "completed") :test #'equal)
                                       (format nil " · ~a" status))))
                       forks))
           (stat-room (reduce #'max stats :key (lambda (cell) (+ (text-display-width (car cell))
                                                                 (text-display-width (or (cdr cell) ""))))
                                          :initial-value 0)))
      (loop for fork across forks
            for name in names
            for (counts . ending) in stats
            for index from 0
            collect (let* ((status (json-value fork :string "status"))
                           (running (equal status "running"))
                           (done (equal status "completed"))
                           (words (json-value fork :text (if running "note" "answer")))
                           (shown (clip-text-to-width name room))
                           (line (make-lline :text (spaces indent) :tone :muted)))
                      (append-lline-run line (if (= index last) "╰ " "├ ") :tone :muted)
                      (append-lline-run line (cond (running "○") (done "✓") (t "✗"))
                                        :tone (cond (running :tool) (done :success) (t :danger)))
                      (append-lline-run line (format nil " fork ~d  " (json-value fork :integer "n")) :tone :muted)
                      (append-lline-run line (concatenate 'string shown (spaces (- room (text-display-width shown))))
                                        :tone :foreground)
                      (append-lline-run line counts :tone :muted)
                      (when ending
                        (append-lline-run line ending :tone :danger))
                      (when words
                        (append-lline-run line (concatenate 'string
                                                            (spaces (- stat-room (text-display-width counts)
                                                                       (text-display-width (or ending ""))))
                                                            "  " (nlk:one-line words))
                                          :tone (if running :muted :foreground) :italic running))
                      (clip-lline line width))))))

(defun tool-header-llines (row &aux (activity (row-activity row))
                                    (status (if (row-load-errors row) "error" row.tool-status))
                                    (exited (json-value row.metadata :object "background_shell"))
                                    (status-text (tool-failure-text row)))
  "The tool card's header."
  ;; A call this image has words for (ROW-ACTIVITY) is titled by them, its
  ;; commands stand under the title while the title could not carry them
  ;; whole, and its snippet shows underneath only while the card is open
  ;; (DETAIL-OPEN-P) — the Lisp is one level down, not the first thing read. A
  ;; tool with no words keeps its name and raw arguments.
  (if (null activity)
      (if (tool-note row)
          (append (list (tool-summary-lline row))
                  (when (detail-open-p (tool-row-expand-key row))
                    (tool-command-llines row)))
          (tool-command-llines row :status-text status-text))
      (let* ((title (if (activity-code-title-p activity)
                        (make-lline :text activity.head :tone :syntax-function
                                    :spans (command-token-spans activity.head))
                        (if (activity-entries activity)
                            (title-phrases-lline
                             (activity-entries-phrases (activity-entries activity)
                                                      :running (member status '("running" "background")
                                                                       :test #'equal)))
                            (tool-summary-lline row)))))
        (when (activity-entries activity)
          (append-title-detail title (tool-note row)))
        ;; A code title has no entries (ACTIVITY-CODE-TITLE-P), so either arm, never both.
        (when (or (activity-entries activity) (activity-code-title-p activity))
          (append-status-text title status status-text exited))
        (append-row-diff-stats title row)
        (append
         (list title)
         ;; `$ <command>' lines for the commands a closed card's title could not carry whole.
         (when (and (not (detail-open-p (tool-row-expand-key row))) (eq :command activity.family))
           (loop for entry in (activity-entries activity)
                 for source = (second entry)
                 when (and (eq :command (first entry))
                           (stringp source)
                           (plusp (length (string-trim '(#\Space #\Tab #\Newline)
                                                       source)))
                           (not (search source title.text)))
                   ;; The `$ ' line is the source's first line, trimmed — the head
                   ;; names what the call runs, and a heredoc's body is not a
                   ;; source (the receipt's own rule). Tabs become spaces so the
                   ;; painted row cannot jump.
                   collect (make-lline
                            :text (format nil "$ ~a"
                                          (string-trim
                                           '(#\Space #\Tab)
                                           (substitute #\Space #\Tab (first-line source))))
                            :tone :muted :dim t :role :tool-cont)))
         (when (detail-open-p (tool-row-expand-key row))
           (let ((snippet (tool-command-llines row)))
             ;; A one-line snippet already stands as its own title.
             (unless (and (activity-code-title-p activity)
                          (= 1 (length snippet))
                          (equal (lline-text (first snippet))
                                 activity.head))
               (mapcar (lambda (line) (copy-with lline line :role :tool-cont :dim t))
                       snippet))))))))

(defun cut-lline (line &key (start 0) (end (length (lline-text line))) role)
  "A copy of LINE carrying only the [START,END) slice of its text and of its
spans, shifted to the slice, with ROLE replacing the copy's role when
supplied — the head and tail cuts the tool output window and the thinking
tail make at a fragment boundary."
  (copy-with lline line
             :text (subseq (lline-text line) start end)
             :spans (loop for span in (lline-spans line)
                          for from = (max start span.start)
                          for to = (min end span.end)
                          when (< from to)
                            collect (copy-with mspan span :start (- from start) :end (- to start)))
             :role (or role line.role)))

(defun llines-window (llines width budget &key from-end role &aux (rows 0) (kept '()))
  "The first BUDGET wrapped rows of LLINES at WIDTH — the last BUDGET with
FROM-END — the line that crosses the budget cut at a fragment boundary
(CUT-LLINE, the cut taking ROLE when given)."
  ;; The one windowing walk: the tool output window's head and tail and the
  ;; thinking tail are each a window of it.
  (dolist (line (if from-end (reverse llines) llines))
    (when (>= rows budget) (return))
    (let ((fragments (lline-fragments line width))
          (keep (- budget rows)))
      (push (cond ((<= (length fragments) keep) line)
                  (from-end (cut-lline line :role role
                                            :start (car (nth (- (length fragments) keep)
                                                             fragments))))
                  (t (cut-lline line :end (cdr (nth (1- keep) fragments)) :role role)))
            kept)
      (incf rows (min keep (length fragments)))))
  (if from-end kept (nreverse kept)))

(defun omission-text (hidden)
  "The five-row window's omission count and its per-card click control."
  (format nil "+~d line~:p (click to expand)" hidden))

(defun command-rows (command width &key open)
  "COMMAND's own lines as rows of segs WIDTH columns wide, painted as a card
paints an eval snippet: every row when OPEN or when they are five or fewer, else
the five-row window a closed card's output shows -- two head rows, the
omission line, two tail rows. => (values ROWS ALL), ALL the rows it takes
whole."
  ;; The background band and the exec view show a pending exec's command as
  ;; the call wrote it; flattened to one line it would be unreadable past a
  ;; few forms.
  (let ((rows (loop for line in (command-llines command :dim t)
                    append (loop for (start . end) in (break-fragments (lline-text line) width width)
                                 collect (fragment-segs line start end)))))
    (values (if (or open (<= (length rows) 5))
                rows
                (append (subseq rows 0 2)
                        (list (list (make-seg :text (omission-text (- (length rows) 4)) :tone :muted)))
                        (last rows 2)))
            (length rows))))

(defun stop-words (stop &aux (by (json-value stop :string "by"))
                             (after (json-value stop :integer "after_ms")))
  "The line a stopped call's card ends on, from its STOP document
(STOP-DOCUMENT): who stopped it and after how long."
  (format nil "~a~@[ after ~a~]"
          (cond ((equal by "operator") "stopped by you")
                ((equal by "shutdown") "stopped: the gateway shut down while this ran")
                ((equal by "model") "stopped by the model")
                (t (format nil "stopped~@[: ~a~]" (json-value stop :string "reason"))))
          (and after (not (equal by "shutdown")) (format-turn-elapsed after))))

(defun tool-output-llines (row width &aux (exited (row-exit-document row))
                                          (stop (or (json-value row.metadata :object "stopped")
                                                    (json-value exited :object "stopped")
                                                    (json-value row.metadata :object
                                                                "background_read" "stopped"))))
  "Tool output behind the ⎿ gutter, dimmed, collapsed to the Zig 5-visual-row
window: two head rows, one \"⋮ +N lines (click to expand)\" clause,
two tail rows. A
call that ran on in the background and exited shows the last lines its exit
kept instead, in the danger tone unless it ended done, and the report of the
condition it failed on after them, whole."
  ;; The budget counts wrapped visual rows at the tool body width, so one long
  ;; line cannot inflate the preview (Zig toolOutputPreviewForWidth). The
  ;; result reads as TOOL-RESULT-SHOWN has it, the page's reading too: a
  ;; run/write_stdin envelope as its stream, an eval result as the
  ;; text it produced without the harness's notes to the model, a background
  ;; job's read as what the job wrote; any other text unchanged. An exited
  ;; background call's result is its handoff, the output as it stood at the
  ;; yield: the card says nothing of how the exec went on.
  ;; A stopped call shows what it had written and who stopped it, never the
  ;; model's text of the abort.
  (let* ((lines (split-lines
                 (trim-block-text
                  (if-let (kept (or exited stop))
                    (kept-tail-text (or (json-value kept :string "tail") "")
                                    (json-value kept :boolean "tail_cut"))
                    (tool-result-shown row.tool-name row.text row.metadata)))))
         (report (and exited (split-lines (json-value exited :string "error"))))
         ;; A stop is no failure: it reads in the ordinary tone.
         (error-p (if exited
                      (not (member (exit-end exited) '("done" "stopped") :test #'equal))
                      row.error-p))
         (llines (loop for line in lines
                       for firstp = t then nil
                       collect (make-lline
                                :text line :tone (if error-p :danger :foreground) :dim t
                                :role (if firstp :tool :tool-cont)
                                :gutter-tone (if error-p :danger :muted))))
         (total (reduce #'+ llines :key (lambda (ll) (length (lline-fragments ll width))))))
    (append
     (if (or (detail-open-p (tool-row-expand-key row))
             (<= total 5))              ; the Zig max_output_preview_lines budget
         llines
         ;; Two visual rows each side of the omission line.
         (append
          (llines-window llines width 2)
          (list (make-lline :text (omission-text (- total 4))
                            :tone :muted :role :tool-omission
                            :gutter-tone :muted))
          (llines-window llines width 2 :from-end t :role :tool-cont)))
     (loop for line in report
           for firstp = (null llines) then nil
           for text = (if (eq line (first report)) (format nil "error: ~a" line) line)
           collect (make-lline :text text :tone :danger
                               :role (if firstp :tool :tool-cont) :gutter-tone :danger))
     (when stop
       (list (make-lline :text (stop-words stop) :tone :muted
                         :role (if llines :tool-cont :tool) :gutter-tone :muted)))
     (loop for line in (mapcan #'split-lines (row-load-errors row))
           collect (make-lline :text line :tone :danger :dim t
                               :role :tool-cont :gutter-tone :danger)))))

(defun row-exit-document (row &aux (shell (json-value row.metadata :object "background_shell")))
  "The exit's background_shell document on ROW, a call that ran on in the
background and exited (APPLY-BACKGROUND-SHELL-EXIT), else NIL."
  (and (equal "exited" (json-value shell :string "status")) shell))

