;;;; dialog.lisp --- the docked dialog panel, a paste into one, the list dialog's scrollbar. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- the docked dialog panel -------------------------------------------------
;;; One chrome for every dialog, mirroring src/tui/dialog.zig
;;; computeDockedDialog plus the dialogs.zig renderDockedPanel family. The
;;; panel docks at the bottom of the chat content area — over the composer,
;;; not above it, because Zig draws it after the composer onto the same rows
;;; (ui.zig ChatShell.render order) — and paints one composer-toned surface:
;;; content inset 3 columns from the left and 4 from the right, a four-row
;;; header (the tab row, a blank, the search-or-summary row, a blank), the
;;; list body with its scrollbar in the padded rect's last column, then a
;;; three-row footer whose middle row carries the key hints. The area's own
;;; last row stays canvas: it is the gap Zig leaves between the docked panel
;;; and the shell footer.
;;;
;;; Selection is tone, not a marker and not a highlight bar: the selected row
;;; prints in the composer tone and every other row in the system tone, and
;;; every tone is forced onto the panel surface (Zig shortcutPickerStyle), so
;;; the panel reads as a single surface.
;;;
;;; The dialog is model state (unlike the derived dropdown) because its rows
;;; come from a control-op response, not from the live composer text.

(defparameter *dialog-frame-max-height* 20
  "Tallest docked panel (Zig dialog_frame_max_height).")

(defparameter *docked-dialog-footer-rows* 3
  "Footer rows a docked panel reserves (Zig docked_dialog_footer_rows).")

(defparameter *docked-panel-pad-left* 3
  "Columns of panel fill left of the content (Zig computeDockedDialog padding).")

(defparameter *docked-panel-pad-right* 4
  "Columns of panel fill right of the content.")

;;; The tab row alone prints at column 0.
(defparameter *docked-panel-content-col* 1
  "Content column inside the padded rect (Zig docked_panel_content_col).")

(defparameter *docked-panel-search-row* 2
  "Header row carrying the search or summary text (Zig
docked_panel_search_row).")

(defparameter *dialog-row-overflow-mark* " … "
  "What a dialog entry too long for its row wears at the cut: a 1-width gap,
the triple dot (one character and one column wide, U+2026), then a 1-width
pad, so a row that runs into the right edge says it continues without the
text or the dots sitting flush against the edge.")

(defstruct picker-row
  "One selectable dialog entry."
  ;; KEY is the optional leading column (the Zig DialogListItem
  ;; key/label/detail triple); empty
  ;; for the ordinary two-column rows. SUFFIX is selected-row-only chrome
  ;; appended after DETAIL — the Zig /tree files preview, which only the
  ;; highlighted row carries. MARK is a status glyph painted ahead of
  ;; everything in its own MARK-TONE — the one column a row colors by what it
  ;; says rather than by whether it is selected. A HEADING row names the
  ;; rows under it and is never selected: the arrows, a page, the wheel and the
  ;; pointer pass over it, and a search hides it. PATH is selected-row-only
  ;; too: the folder the row is about (a session's working directory), which
  ;; the paint writes as the operator reads it and clips from the middle to
  ;; the room the row leaves, so its last folder shows. TONE, when given,
  ;; paints the row's label whether or not it is selected: a line that says
  ;; something went wrong reads so on either (the exec view's error).
  (key "" :type string)
  (label "" :type string)
  (detail "" :type string)
  (suffix "" :type string)
  (path "" :type string)
  (mark "" :type string)
  (mark-tone :muted)
  (tone nil)
  (value nil)
  (heading nil))

(nlk:access (row picker-row))

(nlk:define-side-table picker-row-prefix (:weakness :key)
  "A row's live-only prefix: (FIELD OFFSET TEXT), drawn muted before that
field's character at OFFSET. The row's raw label/key remains unchanged.")

(nlk:define-side-table picker-row-paint (:weakness :key)
  "A row that lays itself: ROW -> a function of (SELECTED ROOM) answering the
row's segments, at most ROOM columns from the panel's content edge, and the
tone whose ground the row stands on, or NIL for the panel's. Its slots stay
what a search reads and a test names; the function is only the look.")

(defun prefix-picker-row (row prefix &key (field :label) (offset 0))
  "Give ROW an explicit muted PREFIX in FIELD at OFFSET, returning ROW."
  (let ((text (ecase field (:label row.label) (:key row.key))))
    (assert (<= 0 offset (length text)))
    (setf (picker-row-prefix row) (list field offset (concatenate 'string prefix "  "))))
  row)

(defun picker-field-segs (row field text tone &aux (prefix (picker-row-prefix row)))
  "TEXT's segments in TONE, with ROW's explicit prefix in the muted tone."
  (if (and prefix (eq field (first prefix)))
      (let ((offset (second prefix)))
        (append (when (plusp offset) (list (seg (subseq text 0 offset) tone)))
                (list (seg (third prefix) :muted) (seg (subseq text offset) tone))))
      (list (make-seg :text text :tone tone))))

(defstruct picker-dialog
  "A docked list dialog."
  ;; ON-SELECT is called with (MODEL VALUE) for the chosen row and returns a
  ;; notice string or NIL; the dialog closes first so a handler can
  ;; immediately open a successor dialog. ON-DETAIL is the same contract on
  ;; the detail gesture (Right / i, the Zig isDialogDetailKey) — the cells
  ;; dialog's drill into one cell's facts.
  ;;
  ;; TITLE is the tab label the header row highlights and CONTEXT the muted
  ;; label beside it (the Zig renderDockedTabRow pair: " Sessions " plus "
  ;; Gateway"). SUBTITLE is the header's third row on a dialog with no search
  ;; field, the slot the search input owns otherwise. SUMMARY-ROWS, painted
  ;; rows (lists of segs) already fitted to the panel, stand in that row's
  ;; place as many rows as they are: the exec view's command, its lines kept.
  (title "" :type string)
  (context "" :type string)
  (subtitle "" :type string)
  (summary-rows '() :type list)
  (rows '() :type list)
  (selection 0 :type integer)
  ;; The list window's first visible row. The window used to be derived from
  ;; SELECTION alone, which cannot express a mouse scroll: the wheel moves the
  ;; window while the highlight stays on the row the pointer is on, and one
  ;; index cannot say both. DIALOG-BODY-VIEWPORT settles this on every paint
  ;; and every hit-test, so the two can never disagree about which rows show
  ;; — the Zig activeDialogViewportOffset, which its paged dialogs carry per
  ;; items view.
  (view-offset 0 :type integer)
  (on-select nil)
  ;; ON-HIGHLIGHT is called with (MODEL ROW) whenever the selection lands
  ;; on a row — the Zig selection-change lazy-fetch seam (the /tree
  ;; checkpoint files arm). Handlers own their idempotence. Delivery is
  ;; deferred: selection moves stage the row in PENDING-HIGHLIGHT and the
  ;; frame-clock tick delivers it (PICKER-FLUSH-HIGHLIGHT); only the
  ;; open-time initial highlight runs synchronously.
  (on-highlight nil)
  ;; The row awaiting ON-HIGHLIGHT delivery, or NIL. A burst of moves —
  ;; arrow repeat, search narrowing, hover sweep — overwrites this slot and
  ;; the tick delivers only the final row, once it has rested
  ;; *PICKER-HIGHLIGHT-REST* (SINCE is the stamp of the staged row's
  ;; arrival; restaging the same row keeps its clock running).
  (pending-highlight nil)
  (pending-highlight-since 0 :type integer)
  ;; When the last highlight actually delivered — the throttle's anchor:
  ;; during continuous movement the pending row still delivers once
  ;; *PICKER-HIGHLIGHT-THROTTLE* has elapsed since this stamp, so previews
  ;; follow the selection instead of waiting for a full stop.
  (last-highlight-at 0 :type integer)
  ;; ON-CANCEL is called with (MODEL) when Esc dismisses the dialog without
  ;; a selection — the Zig cancelThemePreviewSession seam: a dialog whose
  ;; highlight handler previews live state restores it here.
  (on-cancel nil)
  (on-detail nil)
  ;; HINT overrides the derived footer line while rows exist — dialogs
  ;; with bespoke key copy (the restore-checkpoint confirm stage). It is the
  ;; Zig renderDockedFooterText flavour: one muted line.
  (hint nil)
  ;; FOOTER-HINTS is the other Zig footer flavour (renderDockedKeyHintFooter):
  ;; a dialog kind whose DIALOG-PANEL-ROWS (KEY . LABEL) conses paint as
  ;; alternating system/muted segments, or those conses themselves, for a
  ;; dialog whose keys change while it is open (the exec view). It outranks
  ;; HINT and the derived line when present.
  (footer-hints nil :type (or symbol cons))
  ;; Type-to-filter: SEARCHABLE dialogs route printable keys into SEARCH,
  ;; and the visible rows are the subset that answers it, ranked best-first
  ;; (PICKER-VISIBLE-ROWS over the fuzzy matcher in keys.lisp).
  (searchable nil)
  (search "" :type string)
  (search-placeholder "" :type string)
  ;; The body's own copy while nothing matches (Zig DialogListView
  ;; empty_label), not a footer line: the hint row keeps its key copy.
  (empty-label "No items" :type string)
  ;; The distinguished copy while a live query hid every row (the Zig
  ;; "No matching sessions" / "No checkpoints match this filter" swap —
  ;; a dead end and a keystroke are different situations). NIL keeps
  ;; EMPTY-LABEL for both.
  (no-match-label nil)
  ;; Rows carry the two-column `> ` gutter on every profile, not only under
  ;; :none — the Zig option-row look (renderCheckpointRestoreOptionRow).
  (option-marker nil)
  ;; The DockedDialogOptions override: a three-row header drops the
  ;; blank under the summary row, which is the shape the answer-demanding
  ;; panels use (Zig's sudo prompt).
  (header-rows nil)
  ;; The Zig renderTitledDockedHeader tone pair. A :composer title is the
  ;; tab-row look and drops bold to match the chrome; an :approval title is
  ;; the answer-demanding look and keeps it. The summary tone is :muted on a
  ;; picker and :system where the third row is a live prompt, not a label.
  (title-tone :dialog-accent)
  (summary-tone :muted)
  ;; What the dialog is, for a handler that must find its own dialog after
  ;; the fact (the /models refresh repaint) without matching the title.
  (kind nil)
  ;; ON-TICK is called with (MODEL DIALOG) on every frame-clock tick while
  ;; the dialog is open, and its presence is what keeps the clock running
  ;; over an idle shell: a dialog whose rows carry clocks (the /sessions
  ;; running clock) repaints them here. Handlers own their own pacing.
  (on-tick nil)
  ;; ON-DELETE is called with (MODEL VALUE) for the highlighted row on the
  ;; delete gesture (Delete, Ctrl+X); the dialog stays open, so the handler
  ;; that removed the row refreshes it. NIL leaves the gesture unbound.
  (on-delete nil)
  ;; --- the checklist mode ---
  ;; ON-TOGGLE makes the dialog a checklist: Space toggles the highlighted
  ;; row's value in MARKS and every row's label carries its mark (`[x] ',
  ;; `[ ] '), and Enter hands ON-SELECT the whole MARKS list instead of one
  ;; row's value. ON-TOGGLE is called with (MODEL VALUE MARKED-P) after
  ;; each toggle; the dialog stays open.
  (on-toggle nil)
  (marks '() :type list)
  ;; --- the input mode ---
  ;; INPUT-LABEL non-NIL puts one text field in the body in place of the
  ;; list (the Zig renderConnectDockedInput row): INPUT is its text,
  ;; INPUT-SECRET-P masks it, INPUT-CONTEXT is the muted rule line under
  ;; it. Printable keys edit INPUT uncapped; Enter calls ON-INPUT with
  ;; (MODEL TEXT), Esc with (MODEL NIL) — a dialog without ON-INPUT leaves
  ;; the field on Esc. The /models wizard's two text steps ride this.
  (input-label nil)
  (input "" :type string)
  (input-secret-p nil)
  (input-context "" :type string)
  (on-input nil))

(nlk:access (dialog picker-dialog))

;;; The wizard IS a picker dialog. It occupies the same model slot, shows the
;;; same list body, and owns the same title/rows/search/selection state, so it
;;; INCLUDES the picker rather than restating those slots beside it — the panel
;;; below reads them through the picker's own accessors, and the wizard reads
;;; the very same slots through its own conc-name. What is genuinely the
;;; wizard's own is the step ladder in the /models section — and even the
;;; chrome that moves with the step is a picker slot, written from
;;; +CONNECT-STEPS+ on every transition (CONNECT-ENTER-STEP) rather than
;;; re-derived by a panel that would have to know a wizard from a picker.
(defstruct (connect-dialog (:include picker-dialog))
  "The /models wizard state."
  ;; STEP is :provider, :custom-provider, :address, :api-key, or :result; the provider
  ;; PICKER-ROWs live in the inherited ROWS slot (values are plists (:provider
  ;; id) or (:custom t)), which PROVIDER-ROWS, a function of no
  ;; arguments, answers anew at open and after each saved key; the text
  ;; steps ride the picker's own input mode (INPUT-LABEL / INPUT), cleared
  ;; whenever the flow leaves the step — the Zig zero-on-clear discipline,
  ;; minus the byte scrubbing CL cannot promise. CUSTOM-P is also the result's
  ;; return route: a typed provider id returns to its own step, a picked one
  ;; to the key step. The result is a failure's: a saved key returns to the
  ;; list, and Enter on a connected provider runs ON-SUCCESS.
  (step :provider)
  (provider-rows nil)
  (provider-id "" :type string)
  (provider-label "" :type string)
  (custom-p nil)
  (result-message "" :type string)
  (on-save nil)
  (on-success nil)
  ;; The context label every step wears instead of its own while a walk
  ;; runs the wizard (`Setup · step 1/3'), and the page that makes the
  ;; picked provider's key, when the rows know one.
  (walk nil)
  (key-page nil)
  ;; When the checking step began (internal real time), for its clock; and
  ;; the key a result keeps while nothing listened where it was checked,
  ;; so Enter checks the same key again once the server is up.
  (checking-since nil)
  (retry nil)
  ;; The ${NAME} blanks of the provider's catalog address still to ask for,
  ;; the one the address step asks first, and (NAME . VALUE) for each one
  ;; answered, newest first: the key's check dials the address they fill,
  ;; and they are saved with the key (CONNECT-ASK-BLANKS-OR-KEY).
  (blanks '())
  (blank-values '()))

;;; A sheet stands in the middle of the frame over the shell as it is -- the
;;; transcript and the composer behind it, dimmed -- where a docked dialog
;;; takes the composer's place: /settings. It IS a picker dialog, so every
;;; chord, the Esc that closes it and the footer's toggle treat it as one;
;;; its body is its own. PAINT, called with (MODEL SHEET WIDTH HEIGHT) for
;;; the frame, answers the window: (values ROWS X Y CURSOR), the painted
;;; rows, the frame cell the first one starts at, and the cell (X . Y) the
;;; hardware cursor parks on, or NIL. KEY takes (MODEL SHEET KEY &key CTRL
;;; ALT SHIFT), every key the shell's own chords leave; PASTE (MODEL SHEET
;;; TEXT); CLICK (MODEL SHEET X Y), a left click on the 0-based frame cell X,
;;; Y in the window; WHEEL (MODEL SHEET DELTA X Y), a notch there; HOVER
;;; (MODEL SHEET X Y), the pointer moved onto that cell with no button held,
;;; answering whether that moved anything. Its canvas
;;; and the helpers every sheet paints and calls with are sheet.lisp's.
(defstruct (sheet-dialog (:include picker-dialog))
  "A dialog drawn in the middle of the frame."
  paint key paste click wheel hover
  ;; What the last frame painted, (ROWS X Y WIDTH): the overlay lays it and
  ;; the pointer measures against it; the cells a click or the pointer
  ;; resting there acts on, ((ROW FROM TO ACTION HOVER) ...); the line on the key row, (TEXT TONE . TIME);
  ;; and whether a call is under way.
  ;; Whether the last paint showed one view at a time.
  (layout nil) (hits '()) (flash nil) (busy nil) (stacked nil))

(nlk:access (sheet sheet-dialog))

(defun sheet-box (width height want-width want-height &key whole)
  "=> (values X Y W H EDGES): where a sheet asking WANT-WIDTH x WANT-HEIGHT
cells stands on a WIDTH x HEIGHT frame -- centred over the rows above the
shell footer, its margins narrowing with the window, EDGES true -- or, under
50 columns or 16 rows above the footer, or WHOLE, every one of those rows
from the top, with no edges."
  (let ((bottom (if (shell-footer-p height) (- height 3) height)))
    (if (or whole (< width 50) (< bottom 16))
        (values 0 0 width bottom nil)
        (let ((w (min want-width (- width (* 2 (cond ((>= width 100) 6) ((>= width 70) 4) (t 2))))))
              (h (min want-height (- bottom 2))))
          (values (floor (- width w) 2) (max 1 (floor (- bottom h) 2)) w h t)))))

(defun settle-sheet (model width height &aux (sheet (chat-model-dialog model)))
  "Paint the open sheet's window for this WIDTH x HEIGHT frame, keep it for
the overlay and the pointer, and park the cursor where it says. => whether a
sheet is open."
  (when (sheet-dialog-p sheet)
    (multiple-value-bind (rows x y cursor) (funcall sheet.paint model sheet width height)
      (setf sheet.layout (list rows x y (reduce #'max rows :key #'text-display-width
                                                           :initial-value 0))
            (chat-model-parked-cursor model) cursor))
    t))

(defun sheet-at (model x y &aux (sheet (chat-model-dialog model)))
  "=> (values COLUMN ROW), the 0-based frame cell under mouse X, Y (1-based),
when it lies in the open sheet's window as the last frame painted it; NIL
otherwise."
  (when (and (sheet-dialog-p sheet) sheet.layout)
    (destructuring-bind (rows left top width) sheet.layout
      (let ((column (1- x)) (row (1- y)))
        (when (and (<= left column (+ left width -1))
                   (<= top row (+ top (length rows) -1)))
          (values column row))))))

(defparameter +sheet-backdrop-light+ 40
  "Percent of its own ink the frame behind an open sheet keeps, the rest its
ground's: what is behind reads as behind.")

(defun sheet-overlay (model &aux (sheet (chat-model-dialog model))
                                 (profile (effective-color-profile)))
  "The open sheet's window as a paint over the frame's cells, every cell
outside it dimmed toward its ground, or NIL while no sheet is open."
  ;; A basic colour's code names no rgb to blend, so the 16-colour profile
  ;; dims nothing; :none has nothing to dim.
  (when (and (sheet-dialog-p sheet) sheet.layout)
    (destructuring-bind (rows left top width) sheet.layout
      (let ((canvas (or (canvas-rgb) (visual-style-bg (style-for-tone :selection-ground))))
            (ink (visual-style-fg (style-for-tone :foreground)))
            ;; The parser hands every cell of one colour the same string, so
            ;; a frame blends each pair once.
            (dimmed (make-hash-table :test 'eq)))
        (flet ((rgb (sgr)
                 ;; An extended colour's only: `38;2;R;G;B' or `38;5;N'.
                 (and sgr (> (length sgr) 3) (sgr-rgb sgr))))
          (lambda (buffer)
            (when (and canvas (member profile '(:truecolor :ansi256)))
              (dotimes (row (nlt:screen-buffer-height buffer))
                (dotimes (column (nlt:screen-buffer-width buffer))
                  (unless (and (<= top row (+ top (length rows) -1))
                               (<= left column (+ left width -1)))
                    (let* ((cell (nlt:screen-buffer-ref buffer column row))
                           (grounds (alexandria:ensure-gethash (nlt:cell-fg cell) dimmed
                                                               (make-hash-table :test 'eq))))
                      (setf (nlt:cell-fg cell)
                            (alexandria:ensure-gethash (nlt:cell-bg cell) grounds
                                (color-sgr (blend-rgb (or (rgb (nlt:cell-fg cell)) ink)
                                                      (or (rgb (nlt:cell-bg cell)) canvas)
                                                      +sheet-backdrop-light+)
                                           t profile))))))))
            (nlt:paint-rows buffer rows :x left :y top)))))))

;;; The first deferral round moved handler cost off the keystroke but still
;;; delivered nearly one hydrate per move (measured: 140 picker keys -> 89
;;; flushes at ~21ms each): 20Hz ticks outpace human navigation, so coalescing
;;; alone barely coalesced. A row being arrowed past deserves no preview at
;;; all; only the row the operator rests on does. Two ticks' worth, so a
;;; delivery costs at most one preview per pause. The suite binds this to 0 —
;;; tests drive the flush by hand and assert delivery, not pacing.
(defparameter *picker-highlight-rest* 0.08
  "Seconds a staged highlight must rest before the tick delivers it.")

;;; Paces every staged lane — a held arrow and search narrowing alike. While
;;; the selection keeps moving, the pending row still delivers at this
;;; cadence, so the preview follows the scrub without paying a hydrate per
;;; landing; the final row lands via the rest window when the operator stops.
(defparameter *picker-highlight-throttle* 0.3
  "Seconds between tick deliveries while a STAGED highlight keeps moving.")

;;; Weakly keyed, so a dismissed dialog takes its entry with it; a table and
;;; not a slot for the reason PICKER-ARROW-HANDLERS gives.
(nlk:define-side-table picker-live-highlights (:weakness :key)
  "The open dialogs whose every landing delivers in the pass that makes it
(OPEN-PICKER-DIALOG's :HIGHLIGHT-DELIVERY :LIVE): PICKER-DIALOG -> T.")

(defun picker-notify-highlight (model dialog &key (instant (picker-live-highlights dialog)))
  "Deliver or stage the highlight for the currently selected visible row —
the Zig fetch-on-selection-change seam."
  ;; Highlight handlers do real work: the sessions dialog hydrates a whole
  ;; preview over HTTP, the tree dialog fetches checkpoint files, the themes
  ;; dialog rebuilds the palette. Two regimes, chosen by the caller:
  ;;
  ;; INSTANT — a landing the caller wants shown at once: a hover landing, the
  ;; search box emptied back to the listing, a listing arriving under a dialog
  ;; already open — delivers SYNCHRONOUSLY in the caller's own pass, one
  ;; delivery for one landing. A live dialog's landings are all instant: its
  ;; handler never blocks the loop, so a held arrow shows every row it crosses.
  ;;
  ;; Every other key stages — arrows and their chords (held repeat included), Tab,
  ;; search narrowing: the row is parked in PENDING-HIGHLIGHT and the
  ;; frame-clock tick delivers the survivor (PICKER-FLUSH-HIGHLIGHT) once it
  ;; rests or the throttle reopens, so a burst never pays a hydrate per
  ;; landing. The rows arrowed past were never chosen; only the row the
  ;; operator rests on, and one per throttle window while scrubbing, earn a
  ;; preview. Restaging the row already staged keeps its rest clock: a search
  ;; whose keystrokes leave the same top row selected must not postpone
  ;; that row's preview forever. No-op without a handler or without rows.
  (when-let (on-highlight dialog.on-highlight)
    (let ((row (selected-of (picker-visible-rows dialog)
                            dialog.selection))
          (now (get-internal-real-time)))
      (cond
        ;; A heading is never chosen, so it is never highlighted either: a
        ;; list that opens on one hands its handler nothing.
        ((or (null row) (picker-row-heading row)) nil)
        (instant
         (setf dialog.pending-highlight nil dialog.last-highlight-at now)
         (funcall on-highlight model row))
        ((not (eq row (picker-dialog-pending-highlight dialog)))
         (setf dialog.pending-highlight row dialog.pending-highlight-since now))))))

(defun picker-flush-highlight (model &aux (dialog (chat-model-dialog model)))
  "Deliver the open dialog's pending highlight when either pacing gate
opens: the staged row has rested *PICKER-HIGHLIGHT-REST* (the pause case),
or *PICKER-HIGHLIGHT-THROTTLE* has elapsed since the last delivery (the
live-follow case — previews keep ticking while the selection moves)."
  ;; The tick-branch flush: the pending row clears before the handler runs, so
  ;; a handler that opens a successor dialog or signals cannot wedge a stale
  ;; delivery. The span label moves with the work — the cost shows in /gc as
  ;; this seam, not as an anonymous tick. => T when a highlight was delivered.
  (when (picker-dialog-p dialog)
    (let ((row dialog.pending-highlight)
          (on-highlight dialog.on-highlight)
          (now (get-internal-real-time)))
      (when (and row on-highlight
                 (or (>= (since-ms dialog.pending-highlight-since now) (* 1000 *picker-highlight-rest*))
                     (>= (since-ms dialog.last-highlight-at now) (* 1000 *picker-highlight-throttle*))))
        (setf dialog.pending-highlight nil dialog.last-highlight-at now
              nlk:*attribution-label* 'picker-highlight-flush)
        (funcall on-highlight model row)
        t))))

(defparameter +checklist-marks+ '("[x] " "[ ] ")
  "The mark column a checklist row's label leads with, chosen and not.")

(defun picker-mark-rows (dialog)
  "Repaint every row's mark from MARKS — the one place a checklist row's
label says whether its value is chosen — over the label less whichever
mark it already carries."
  (dolist (row dialog.rows)
    (setf row.label
          (concatenate 'string
                       (nth (if (member row.value dialog.marks :test #'equal) 0 1)
                            +checklist-marks+)
                       (ppcre:regex-replace "^\\[[x ]\\] " row.label ""))))
  ;; This is the one handler that rewrites rows in place under a live query:
  ;; the memo a ranking left behind now describes labels that no longer exist.
  (forget-picker-visible-memo dialog))

;;; Weakly keyed, so a dismissed dialog takes its handler with it. A table
;;; rather than a dialog slot on purpose: a live image cannot grow a structure
;;; a slot, and the fold must reach the running shell without a relaunch.
(nlk:define-side-table picker-arrow-handlers (:weakness :key)
  "The open dialogs that carry a fold gesture: PICKER-DIALOG -> the handler
OPEN-PICKER-DIALOG's :ON-ARROW registered, called with (MODEL DIALOG
DIRECTION) for the two arrow keys the selection does not spend and answering
whether it took the key.")

(nlk:define-side-table picker-rename-handlers (:weakness :key)
  "The open dialogs whose rows take a name: PICKER-DIALOG -> the handler
OPEN-PICKER-DIALOG's :ON-RENAME registered, called with (MODEL DIALOG) on
the rename chord (F2), which opens the dialog's field on the highlighted
row.")

(nlk:define-side-table picker-space-handlers (:weakness :key)
  "The open dialogs, no checklist, whose rows answer Space: PICKER-DIALOG ->
the handler OPEN-PICKER-DIALOG's :ON-SPACE registered, called with (MODEL
DIALOG) and answering whether it took the key; declined, the Space is the
query's.")

;;; TABS is a list of (:LABEL the segment's own text, :COUNT the rows behind
;;; it, :VALUE the PICKER-ROW-KEY it selects — NIL for the tab that filters
;;; nothing, :MATCH an optional (ROW VALUE) predicate instead of key equality).
;;; A tabbed dialog says the bucket once, in the tab row, instead of once per
;;; row: the selected tab shows the rows its match or key equality accepts.
;;;
;;; Weakly keyed, and a table rather than a dialog slot on purpose: a live
;;; image cannot grow a structure a slot, and the strip must reach the running
;;; shell without a relaunch.
(nlk:define-side-table picker-tabs (:weakness :key)
  "The tab strip of a tabbed dialog: PICKER-DIALOG -> (TABS . ACTIVE-INDEX).")

(defun register-picker-tabs (dialog tabs &key (active 0))
  "Give DIALOG the tab strip TABS (plists as above), its ACTIVE-th tab
selected. => DIALOG. A dialog registered with no tabs has no strip."
  (when tabs (setf (picker-tabs dialog) (cons tabs active)))
  dialog)

;;; A picture is (:ROWS ROWS :LABEL LABEL), ROWS strings of `1' ink and `0'
;;; paper: a small image the body paints above the list -- LIST-DIALOG's
;;; :PICTURE, the link's QR code. With a :PALETTE of tone names each pixel is
;;; the base-36 digit of the tone it paints in (/context's grid), and :BESIDE
;;; stands it left of the list on a panel wide enough for both. A table for
;;; the reason the strip is one.
(nlk:define-side-table picker-pictures (:weakness :key)
  "The dialogs that carry a picture: PICKER-DIALOG -> its picture plist.")

(defun register-picker-picture (dialog picture)
  "Give DIALOG the PICTURE (a plist as above) its body paints above the list.
=> DIALOG. A dialog registered with none has none."
  (when (getf picture :rows) (setf (picker-pictures dialog) picture))
  dialog)

;;; A table for the reason the strip is one.
(nlk:define-side-table picker-body-floor (:weakness :key)
  "The body rows a dialog's panel held when last measured: PICKER-DIALOG ->
the rows a query in its box keeps it from shrinking under (DIALOG-BODY-HEIGHT).")

(defun picker-dialog-tabs (dialog)
  "DIALOG's tab strip as a list of tab plists, or NIL for a one-list dialog."
  (car (picker-tabs dialog)))

(defun picker-dialog-tab (dialog)
  "DIALOG's selected tab index — 0 for a dialog with no strip."
  (or (cdr (picker-tabs dialog)) 0))

(defun picker-dialog-tab-value (dialog)
  "The row key DIALOG's selected tab selects, or NIL when it filters nothing:
a dialog with no strip, or the all-rows tab of one."
  (when-let (tab (nth (picker-dialog-tab dialog) (picker-dialog-tabs dialog))) (getf tab :value)))

(defun picker-tab-mutate (dialog delta &aux (state (picker-tabs dialog))
                                            (tabs (car state)))
  "Move DIALOG's strip DELTA tabs, wrapping, and land the list at its top:
another tab is another list, so the cursor and the window come home, past
the headings it opens on."
  ;; => the
  ;; new index, or NIL for a dialog with no strip.
  (when tabs
    (setf (cdr state) (mod (+ (cdr state) delta) (length tabs)))
    ;; The ranking's memo describes the rows of the tab we left.
    (forget-picker-visible-memo dialog)
    (picker-seat-top dialog)
    (cdr state)))

(defun picker-tab-pick (model index &aux (dialog (chat-model-dialog model)))
  "Show the tab at INDEX of the open dialog's strip, as Tab walking there
would: the list at its top, its highlight delivered. => T when INDEX is a
tab of the strip."
  ;; A click on a tab's chip (DOCKED-DIALOG-TAB-SEGS): one landing, so the
  ;; highlight goes at once, as a hover's does.
  (when (and (picker-dialog-p dialog) (< -1 index (length (picker-dialog-tabs dialog))))
    (unless (= index (picker-dialog-tab dialog))
      (picker-tab-mutate dialog (- index (picker-dialog-tab dialog)))
      (picker-notify-highlight model dialog :instant t))
    t))

(defun open-picker-dialog (model title rows on-select &rest options
                           &key on-arrow on-search on-space on-rename
                           (highlight-delivery :instant) &allow-other-keys)
  "Open a dialog over MODEL."
  ;; Empty ROWS still open — the tab row plus the body's empty label tell the
  ;; operator there is nothing to pick. SEARCHABLE dialogs (the Zig dialogSpec
  ;; searchable kinds) own the header's third row with the shared
  ;; type-to-filter input; every other dialog shows SUBTITLE there.
  ;; ON-HIGHLIGHT fires immediately for the initially selected row.
  ;;
  ;; OPTIONS are MAKE-PICKER-DIALOG's own keywords, forwarded verbatim —
  ;; except :ON-ARROW, :ON-SPACE, :ON-RENAME and :HIGHLIGHT-DELIVERY, which
  ;; are this function's own:
  ;; the fold handler stays out of the dialog record because a live image
  ;; cannot grow a structure a slot, and it is registered in
  ;; *PICKER-ARROW-HANDLERS* instead. The defstruct already carries every
  ;; other default, so restating them here would be a second place for them to
  ;; drift.
  ;;
  ;; HIGHLIGHT-DELIVERY :STAGED opens without the synchronous first highlight:
  ;; the dialog paints in the caller's own pass and the row's highlight lands
  ;; on the frame clock -- the /tree picker, whose handler is a gateway read
  ;; paid on the loop. :LIVE delivers every landing at once, the open's
  ;; included (PICKER-LIVE-HIGHLIGHTS): a handler that never blocks the loop
  ;; -- the /sessions preview, whose reads run off it -- owes no stage.
  (let ((dialog (apply #'make-picker-dialog
                       :title title :rows rows :on-select on-select
                       (alexandria:remove-from-plist options :on-arrow :on-space :on-rename
                                                     :highlight-delivery :on-search))))
    ;; A checklist's rows carry their marks from the first paint.
    (when dialog.on-toggle (picker-mark-rows dialog))
    (when on-arrow (setf (picker-arrow-handlers dialog) on-arrow))
    (when on-search (setf (picker-search-handlers dialog) on-search))
    (when on-space (setf (picker-space-handlers dialog) on-space))
    (when on-rename (setf (picker-rename-handlers dialog) on-rename))
    (when (eq highlight-delivery :live) (setf (picker-live-highlights dialog) t))
    (setf (chat-model-dialog model) dialog)
    (picker-notify-highlight model dialog :instant (not (eq highlight-delivery :staged))))
  model)

;;; Weakly keyed, so a dismissed dialog takes its entry with it.
;;;
;;; The memo exists because the ranking is asked for twice per keystroke —
;;; once by the key ladder, once by the paint that follows it — and a dialog
;;; over a thousand rows (the prompt history) pays ~25 ms a pass. It is keyed
;;; on the rows LIST and the query, so any handler that swaps rows in place
;;; under a live query must say so (PICKER-MARK-ROWS is the one that does).
(nlk:define-side-table picker-search-handlers (:weakness :key)
  "An instance's query delivery policy, called with (DIALOG QUERY) before ranking.")

(nlk:define-side-table picker-visible-memo (:weakness :key)
  "The last ranking PICKER-VISIBLE-ROWS made, per dialog: (ROWS SEARCH .
VISIBLE).")

;;; The /sessions conversation search fills its rows from the gateway's grep
;;; over every session; PICKER-VISIBLE-ROWS shows them verbatim while the box
;;; still holds that query, because the dialog's own matcher would drop a row
;;; the store matched for a token the row's snippet window does not happen to
;;; carry.
(nlk:define-side-table picker-filtered-rows (:weakness :key)
  "Weak memo: dialog -> the query the dialog's rows already answer, when
something other than the dialog did the filtering.")

(defun picker-rows-filtered-for (dialog query)
  "Declare DIALOG's rows the answer to QUERY — NIL hands them back to the
dialog's own matcher. => DIALOG."
  (if query
      (setf (picker-filtered-rows dialog) query)
      (forget-picker-filtered-rows dialog))
  dialog)

(defun picker-visible-rows (dialog &aux (rows dialog.rows)
                                        (search dialog.search)
                                        (tab (picker-dialog-tab-value dialog)))
  "The rows the dialog currently shows: every row the search query answers,
best answer first."
  ;; SEARCH-SCORE is the matcher — tokenized, fuzzy, tiered — read over the
  ;; fields a row exposes in the order the eye reads them (label, key,
  ;; detail), and it both rejects a row and says where it belongs.
  ;;
  ;; The sort is STABLE, so rows of equal standing keep the dialog's own
  ;; order: the ranking lifts the better answers to the top without ever
  ;; scrambling a list whose order already carries meaning — a session's
  ;; recency, a provider's grouping, an alias ahead of the models it names.
  ;;
  ;; The answer is memoized per (rows, query, tab) — see
  ;; *PICKER-VISIBLE-MEMO*.
  ;;
  ;; A TABBED dialog narrows first to the selected tab's rows — the ones whose
  ;; KEY is its VALUE — and the query then filters what that tab shows. A tab
  ;; whose VALUE is NIL filters nothing, so the all-rows tab of a strip is the
  ;; plain list.
  ;; Filtered abroad: the rows under this dialog came from somewhere that
  ;; already matched and ordered them (*PICKER-FILTERED-ROWS*), and the box
  ;; still holds the query they answer — show them as they are.
  (when (equal (picker-filtered-rows dialog) search)
    (return-from picker-visible-rows rows))
  (memoized (picker-visible-memo dialog) (rows search tab)
    (let* ((match (getf (nth (picker-dialog-tab dialog) (picker-dialog-tabs dialog)) :match))
           (shown (if tab
                      (remove-if-not (lambda (row) (if match (funcall match row tab) (equal row.key tab))) rows)
                      rows)))
      (if (zerop (length search))
          shown
          (mapcar #'cdr (stable-sort (loop for row in shown
                                           for score = (and (not row.heading)
                                                            (search-score search (list row.label row.key row.detail)))
                                           when score collect (cons score row))
                                     #'> :key #'car))))))

(defun picker-reseat-selection (dialog value)
  "Seat DIALOG's selection after its rows were swapped in place: the row
carrying VALUE takes the cursor, and a value the new rows no longer hold
leaves the cursor on the seat it already had — clamped to the last row —
so a deletion hands the cursor to the row that slid up into the deleted
row's place rather than throwing it home -- or past it, when that is a
heading, which is never chosen. => the seated index."
  (let* ((visible (picker-visible-rows dialog))
         (index (and value
                     (position value visible
                               :key #'picker-row-value :test #'equal))))
    (setf dialog.selection
          (past-headings visible
                         (cond (index index) ((null visible) 0) (t (min dialog.selection (1- (length visible)))))
                         1))))

(defun picker-highlighted-value (dialog)
  "The value of the visible row DIALOG's selection points at, or NIL."
  (when-let (row (selected-of (picker-visible-rows dialog) dialog.selection)) row.value))

(defun picker-seat-top (dialog)
  "Seat DIALOG's highlight on its first row that is no heading, the window
at the top."
  (setf dialog.selection (past-headings (picker-visible-rows dialog) 0 1)
        dialog.view-offset 0))

(defun picker-search-mutate (dialog new-search)
  (setf dialog.search new-search)
  (when-let (handler (picker-search-handlers dialog))
    (funcall handler dialog new-search))
  ;; The window comes home with the selection: a query that shrinks the list
  ;; must not paint from the offset the old one scrolled to. A cleared query
  ;; shows the headings again, and the /models list opens on one.
  (picker-seat-top dialog))

(defun picker-page-move (model dialog action &aux (total (length (picker-visible-rows dialog))))
  "One PgUp/PgDn/Home/End on the open picker: ACTION moves the list's window
and takes the highlight with it — a page is as many rows as the panel's body
shows (DIALOG-BODY-BAND, the measure the wheel and the hit-test share) — and
Home/End seat the highlight on the list's first and last row with the window
at that end. => T when the list moved, NIL for an empty list."
  ;; A page past either end moves the window no further and lets the highlight
  ;; finish the trip on the row at that end, so the key never spends itself on
  ;; a scroll with nothing to show for it.
  (when (plusp total)
    (bind ((height (or (nth-value 1 (dialog-body-band model dialog)) total))
           (visible (min total (max 1 height))) (max-offset (max 0 (- total visible)))
           (page (max 1 visible)) (selection dialog.selection)
           (offset dialog.view-offset)
           ((seat window) (ecase action
                            (:page-up (values (max 0 (- selection page))
                                              (max 0 (- offset page))))
                            (:page-down (values (min (1- total) (+ selection page))
                                                (min max-offset (+ offset page))))
                            (:scroll-home (values 0 0))
                            (:scroll-end (values (1- total) max-offset)))))
      (setf dialog.selection (past-headings (picker-visible-rows dialog) seat 1)
            dialog.view-offset window)
      ;; The window then settles by the one rule paint and hit-test share,
      ;; so the highlighted row is on screen the moment the key lands.
      (dialog-body-viewport dialog visible)
      t)))

(nlk:define-side-table picker-type-through (:weakness :key)
  "Live-only: a picker that opened by itself after another pick -- the
reasoning effort /models offers once a model lands -- => T. Typing was never
meant for it: a printable key closes it, keeping what stands, and lands in the
composer (UPDATE).")

(defun dismiss-dialog (model handler)
  "Close MODEL's dialog, then call HANDLER — its ON-CANCEL or ON-SUCCESS, or NIL — on MODEL."
  (setf (chat-model-dialog model) nil)
  (when handler (funcall handler model)))

(defun picker-invoke-row (model dialog handler &key close)
  "Hand the highlighted visible row's value to HANDLER — one of DIALOG's
row handlers, or NIL — closing DIALOG first when CLOSE, and land the notice
it answers. No row, or no handler: nothing happens."
  (let ((row (selected-of (picker-visible-rows dialog)
                          dialog.selection)))
    (when (and row handler)
      (when close (setf (chat-model-dialog model) nil))
      (when-let (notice (funcall handler model row.value)) (show-status model notice)))))

(defun picker-dialog-key (model key &key ctrl alt shift (mod 0))
  "Route KEY to the open dialog; => T when the dialog consumed it."
  ;; A modal swallows every key except the quit chords its caller checks
  ;; first. Searchable dialogs edit the query: printable keys filter live,
  ;; Backspace deletes, Ctrl+W/Ctrl+Backspace deletes a word, Ctrl+U clears,
  ;; and Esc clears the query first, closing only once it is empty.
  ;; Refine the span label: a key routed here re-filters the dialog, which
  ;; is a different cost class from a composer keystroke, and the /gc
  ;; by-label table should say so.
  (setf nlk:*attribution-label* 'picker-dialog-key)
  (let ((dialog (chat-model-dialog model)))
    (when (and (connect-dialog-p dialog)
               (connect-dialog-key model dialog key :ctrl ctrl :alt alt))
      (return-from picker-dialog-key t))
    ;; A sheet works its own body: every key the shell's chords left is its.
    (when (sheet-dialog-p dialog)
      (funcall (sheet-dialog-key dialog) model dialog key :ctrl ctrl :alt alt :shift shift)
      (return-from picker-dialog-key t))
    (when dialog
      (let* ((rows (picker-visible-rows dialog))
             (count (length rows))
             ;; Ctrl+P / Ctrl+N walk the list alongside the arrows (the Zig
             ;; isPreviousDialogItemKey / isNextDialogItemKey chords). They
             ;; carry ctrl, so they never reach the search-input branch below.
             (delta (cond ((or (eq key :up) (and ctrl (characterp key) (char-equal key #\p)))
                           -1)
                          ((or (eq key :down) (and ctrl (characterp key) (char-equal key #\n)))
                           1))))
        (cond
          ;; Input mode: the field owns every key. Enter hands the text to
          ;; ON-INPUT and Esc hands it NIL — a dialog without a handler
          ;; leaves the field on Esc — and the rest edits the field,
          ;; uncapped: a path or a secret is not a search query.
          ((picker-dialog-input-label dialog)
           (let ((on-input dialog.on-input))
             (cond ((eq key :enter) (when on-input (funcall on-input model dialog.input)))
                   ((eq key :escape)
                    (if on-input (funcall on-input model nil) (setf dialog.input-label nil)))
                   (t
                    (when-let (new (edit-text-field dialog.input key :ctrl ctrl :alt alt))
                      (setf dialog.input new))))))
          (delta
           (when (plusp count)
             (setf dialog.selection
                   (past-headings rows (wrap-selection dialog.selection count delta) delta))))
          ;; PgUp/PgDn page the list and Home/End jump it to either end. These
          ;; are the transcript's own chords (the keymap's :page-up,
          ;; :page-down, :scroll-home and :scroll-end) spent on the dialog
          ;; while one is open, so the transcript behind the panel never
          ;; moves; this ladder runs only while a dialog is open, so the same
          ;; keys are the transcript's again the moment it closes.
          ((when-let (edge (find-if (lambda (action) (model-key-matches-p model action key mod))
                                    '(:page-up :page-down :scroll-home :scroll-end)))
             (picker-page-move model dialog edge)
             t))
          ;; The fold gesture (the sessions picker's Left/Right): consulted
          ;; on the arrow keys the selection does not spend, before the
          ;; detail gesture — and only a consumed answer ends the ladder
          ;; here, so a declined Right still reaches ON-DETAIL below.
          ((and (picker-arrow-handlers dialog)
                (or (eq key :left) (eq key :right))
                (funcall (picker-arrow-handlers dialog) model dialog key)))
          ;; Space on a list whose rows answer it -- /cells turns the
          ;; highlighted cell on or off -- ahead of the query, which would
          ;; take it as a character.
          ((and (eql key #\Space) (not ctrl) (not alt)
                (picker-space-handlers dialog)
                (funcall (picker-space-handlers dialog) model dialog)))
          ((eq key :escape)
           (if (and dialog.searchable (plusp (length dialog.search)))
               (picker-search-mutate dialog "")
               (dismiss-dialog model dialog.on-cancel)))
          ((eq key :enter)
           (setf (chat-model-dialog model) nil)
           ;; A checklist confirms its marks as one value; a picker
           ;; selects the highlighted row.
           (if dialog.on-toggle
               (when-let (notice (and dialog.on-select
                                      (funcall dialog.on-select model dialog.marks)))
                 (show-status model notice))
               (picker-invoke-row model dialog dialog.on-select)))
          ;; Space on a checklist toggles the highlighted row's mark; the
          ;; dialog stays open.
          ((and (picker-dialog-on-toggle dialog)
                (eql key #\Space) (not ctrl) (not alt))
           (when-let (row (selected-of rows dialog.selection))
             (let ((marked (not (member row.value dialog.marks :test #'equal))))
               (setf dialog.marks (if marked
                                      (append dialog.marks (list row.value))
                                      (remove row.value dialog.marks :test #'equal)))
               (picker-mark-rows dialog)
               (funcall dialog.on-toggle model row.value marked))))
          ;; The detail gesture (Zig isDialogDetailKey: Right, i, I) on
          ;; dialogs that carry a detail handler — the cells dialog's
          ;; drill into one cell's facts. Bare letters stay search input on
          ;; searchable dialogs; with no selected row the key is a
          ;; consumed no-op, exactly like the Zig empty-list case.
          ((and (picker-dialog-on-detail dialog)
                (or (eq key :right)
                    (and (not dialog.searchable) (not ctrl) (not alt)
                         (characterp key) (char-equal key #\i))))
           (picker-invoke-row model dialog dialog.on-detail
                              :close t))
          ;; The delete gesture on dialogs that carry a handler for it --
          ;; /execs' interrupt. Ctrl+X never reaches the search input;
          ;; Delete is consumed here before it could.
          ((and (picker-dialog-on-delete dialog)
                (or (eq key :delete)
                    (and ctrl (characterp key) (char-equal key #\x))))
           (picker-invoke-row model dialog dialog.on-delete))
          ;; The rename chord on a dialog whose rows take a name: the
          ;; handler puts the dialog in input mode on the highlighted row.
          ((and (picker-rename-handlers dialog)
                (model-key-matches-p model :rename key mod))
           (funcall (picker-rename-handlers dialog) model dialog))
          ;; Tab walks a tabbed dialog's strip: the scope tabs of one list
          ;; whose rows are bucketed, where the Zig keeps one dialog per
          ;; filter. Another tab is another list, so the cursor comes home
          ;; with it (PICKER-TAB-MUTATE), and Shift+Tab walks back.
          ((and (eq key :tab) (picker-dialog-tabs dialog))
           (picker-tab-mutate dialog (if shift -1 1)))
          ;; Left and Right walk the strip too, stopping at its ends: the
          ;; arrows across are a strip's as Up and Down are the list's. A
          ;; dialog's own Left/Right outranks them: the fold above
          ;; (/sessions' forks) takes an arrow it can use, and one it
          ;; declines -- a row with no forks -- walks the strip here.
          ((and (or (eq key :left) (eq key :right)) (not ctrl) (not alt)
                (picker-dialog-tabs dialog))
           (let ((delta (if (eq key :left) -1 1)))
             (when (< -1 (+ (picker-dialog-tab dialog) delta) (length (picker-dialog-tabs dialog)))
               (picker-tab-mutate dialog delta))))
          (dialog.searchable
           (when-let (new (edit-text-field dialog.search key :ctrl ctrl :alt alt
                                           :max *max-dialog-search-chars*))
             (picker-search-mutate dialog new))))
        ;; The selection may have landed on a new row (movement, filter
        ;; edits). Re-notify while this dialog is still the open one —
        ;; highlight handlers are idempotent, and a failed lazy fetch gets
        ;; its retry exactly here (the Zig cleared-target re-selection).
        ;; Every key stages: a held arrow is a burst, and the tick delivers
        ;; the row the operator rests on (one per throttle window while the
        ;; selection keeps moving) -- never a hydrate per landing. A live
        ;; dialog delivers each one here instead.
        (when (eq dialog (chat-model-dialog model))
          (picker-notify-highlight model dialog)))
      t)))

;;; --- a paste into a dialog ---------------------------------------------------
;;; A paste lands where typing would. The terminal's own paste (Ctrl+Shift+V)
;;; arrives as one bracketed message; Ctrl+V asks the system clipboard for its
;;; text. Both reach the field through PICKER-DIALOG-PASTE, and neither walks
;;; the key ladder, so no pasted character can select, toggle or submit.

(defun picker-dialog-paste (model text)
  "Route pasted TEXT to the open dialog => T when it landed."
  ;; It goes where a typed character goes: the field in input mode, uncapped
  ;; as typing is there; the query of a searchable list, capped as typing is.
  ;; The /models result step and a list with no query own no field, and let
  ;; it go.
  ;; TEXT lands as one line the field could have been typed: the characters
  ;; EDIT-TEXT-FIELD takes from a keystroke (the graphic ones), the blanks
  ;; around them trimmed. A key copied with its trailing newline arrives as
  ;; the key, and a line break inside a paste cannot split a one-line field.
  (let ((dialog (chat-model-dialog model))
        (line (string-trim " " (remove-if-not #'graphic-char-p (or text "")))))
    (when (and dialog (plusp (length line)))
      (cond
        ((sheet-dialog-p dialog)
         (funcall (sheet-dialog-paste dialog) model dialog line)
         t)
        (dialog.input-label (setf dialog.input (concatenate 'string dialog.input line)) t)
        ((and (connect-dialog-p dialog)
              (or (eq :result (connect-dialog-step dialog))
                  (connect-waiting-step-p (connect-dialog-step dialog))))
         nil)
        ((picker-dialog-searchable dialog)
         (picker-search-mutate dialog (clip (concatenate 'string dialog.search line)
                                            *max-dialog-search-chars* :ellipsis ""))
         (picker-notify-highlight model dialog)
         t)))))

(defparameter *clipboard-paste-commands*
  #+win32 '()
  #+darwin '(("pbpaste"))
  #-(or win32 darwin) '(("wl-paste" "--no-newline")
                        ("xclip" "-selection" "clipboard" "-o")
                        ("xsel" "--clipboard" "--output"))
  "The readers Ctrl+V over a dialog tries in order: the read half of the
chain clipboard writes take — macOS's pasteboard, else wl-copy, xclip,
xsel.")

(defun docked-panel-surface-bg ()
  "The panel's fill: the :composer surface every tone is forced onto (Zig
shortcutPickerStyle), or NIL under the :none profile."
  ;; SEG-RENDER already backs a tone without its own bg with the fill it is
  ;; handed, and :composer — the one panel tone that carries a bg — carries
  ;; exactly this one, so the forcing needs no per-segment override.
  (unless (eq (effective-color-profile) :none)
    (visual-style-bg (style-for-tone :composer))))

(defparameter *docked-panel-lane-gap* 2
  "The columns between a panel row's left flow and its right lane — a tab,
so a row's own text and the fact parked at its right edge never read as one
run. Only spent when the row was given a right lane.")

(defun scrollbar-cell (thumb-top thumb-height row)
  "What a panel row's scrollbar lane paints at ROW: :THUMB inside the thumb
SCROLLBAR-THUMB answered, :TRACK beside it, NIL when it answered no bar."
  ;; The one reading of that pair, so the two docked lanes — the picker's list
  ;; and the composer's dropdown — cannot disagree about where a thumb ends.
  (when thumb-height
    (if (<= thumb-top row (+ thumb-top thumb-height -1)) :thumb :track)))

(defun docked-panel-row (segs width surface
                         &key (col *docked-panel-content-col*) scrollbar
                         (pad-left *docked-panel-pad-left*)
                         (pad-right *docked-panel-pad-right*)
                         (bar-gap 0) mark right lead (lead-width 0) (ground surface))
  "One full-WIDTH panel row — the one row primitive every docked panel paints
through: PAD-LEFT columns of panel fill, SEGS printed at COL inside the
padded rect and clipped at its right edge, the slack filled, then SCROLLBAR
(:thumb or :track) in the rect's last columns with BAR-GAP of fill either
side of the bar cell, then the right padding. GROUND, an rgb, is the fill
from COL to the bar -- a chosen row's lift -- and the panel's SURFACE
otherwise."
  ;; The dialog panel takes the defaults; the composer dropdown passes its own
  ;; zero pads and its 2-column gaps. Under the :none profile the geometry is
  ;; identical in plain text, so the goldens stay byte-stable. MARK is what a
  ;; row too long for its rect wears at the cut — the dialog entry's ` … ', a
  ;; 1-width gap either side of the one-column triple dot, so a row hitting
  ;; the right edge says it continues instead of ending flush against it; NIL
  ;; cuts bare, the Zig `.wrap = .none' truncation. RIGHT, when given, is a
  ;; second lane painted flush against the rect's right edge, with
  ;; *DOCKED-PANEL-LANE-GAP* columns of fill between it and SEGS: the row's
  ;; own text keeps the left lane and is what truncates when the two would
  ;; meet, so a fact parked right — the dropdown's hotkey column — is never
  ;; walked off the row by a long description beside it. LEAD is a lane of
  ;; LEAD-WIDTH columns between the left padding and the row's own column,
  ;; its segments clipped to it -- a picture standing beside a list -- and a
  ;; segment whose tone is a VISUAL-STYLE keeps its own ground there.
  ;; The decomposition is exact by construction — pad-left + col + room + the
  ;; scrollbar lane + pad-right always sums to WIDTH — so a narrow window
  ;; loses padding and content rather than pushing the row past the frame.
  ;; The scrollbar only claims its lane when one is left for it, which is the
  ;; same guard Zig keeps (a scrollbar writes inside its window, never past
  ;; its right edge).
  (let* ((pad-left (min pad-left (max 0 width)))
         (pad-right (min pad-right (- width pad-left)))
         (lead-width (max 0 (min lead-width (- width pad-left pad-right))))
         (lead (and (plusp lead-width) (clip-segs lead lead-width)))
         (inner (- width pad-left pad-right lead-width))
         (col (min col inner))
         (lane (+ 1 (* 2 bar-gap)))
         (bar (and scrollbar (> (- inner col) (if (plusp bar-gap) lane 0))))
         (room (- inner col (if bar lane 0)))
         ;; The right lane is measured and cut first: its columns come out of
         ;; the left lane's room, so the two can never overrun the rect and
         ;; the left text — never the parked fact — is what clips.
         (right (clip-segs right room))
         (right-width (segs-display-width right))
         (gap (if (plusp right-width) *docked-panel-lane-gap* 0))
         (kept (clip-segs segs (max 0 (- room right-width gap)) :mark mark))
         ;; The targets its segs carry -- a tab's chip, a key's hint, a link --
         ;; as ranges of the characters they stand on in the row, the shape
         ;; FRAME-TARGET-AT-POSITION reads under the pointer: KEPT from just
         ;; past the pads, the lead lane and the content column, RIGHT past
         ;; KEPT and the slack between them.
         (start (+ pad-left col (if (plusp lead-width)
                                    (+ (length (segs-text lead)) (- lead-width (segs-display-width lead)))
                                    0)))
         (links (append (seg-links kept start)
                        (seg-links right (+ start (length (segs-text kept))
                                            (- room (segs-display-width kept) right-width))))))
    (let ((row
            (flet ((fill-run (count &optional (fill surface)) (blank-frame-row (max 0 count) fill))
                   (paint (lane &optional (fill surface))
                     (format nil "~{~a~}"
                             (mapcar (lambda (seg)
                                       ;; Every tone the panel paints sits on the
                                       ;; panel surface, tones with a bg of their
                                       ;; own included (Zig shortcutPickerStyle);
                                       ;; a style given whole keeps its own.
                                       (seg-render seg fill
                                                   :force-bg (not (visual-style-p seg.tone))))
                                     lane))))
              (concatenate 'string
                           ;; The column before the text is one run with the pads
                           ;; when the row stands on the panel, so a plain row's
                           ;; bytes are what they always were.
                           (let ((lift (if (equal ground surface) 0 col)))
                             (concatenate 'string
                                          (if (plusp lead-width)
                                              (concatenate 'string
                                                           (fill-run pad-left)
                                                           (paint lead)
                                                           (fill-run (+ (- lead-width (segs-display-width lead))
                                                                        (- col lift))))
                                              (fill-run (+ pad-left (- col lift))))
                                          (if (plusp lift) (fill-run lift ground) "")))
                           (paint kept ground)
                           ;; The slack is what neither lane took: the gap is
                           ;; spent in KEPT's budget above, never here, so a row
                           ;; with a right lane still fills its rect exactly.
                           (fill-run (- room (segs-display-width kept) right-width) ground)
                           (paint right ground)
                           (when bar
                             ;; The bar cell: the thumb glyph or a track blank,
                             ;; toned for the surface, bare under :none.
                             (let ((thumb-p (eq scrollbar :thumb)))
                               (concatenate 'string
                                            (fill-run bar-gap)
                                            (if surface
                                                (styled (if thumb-p "█" " ")
                                                        (style-for-tone (if thumb-p
                                                                            :scrollbar-thumb
                                                                            :scrollbar-track)))
                                                (if thumb-p "█" " "))
                                            (fill-run bar-gap))))
                           (fill-run pad-right)))))
      ;; Linked whole: the row the frame keeps is the one its ranges are on.
      (if links (link-plain-row (nlk:narrow-string row) links) row))))

(defun docked-panel-room (width &optional scrollbar)
  "The columns a dialog's DOCKED-PANEL-ROW gives its SEGS at WIDTH: the
panel's pads and content column out, and the bar's lane when SCROLLBAR."
  ;; What a row fits to before the row cuts it: the footer's hints drop whole
  ;; to it, and a selected row's path clips its middle to what is left of it.
  (max 0 (- width *docked-panel-pad-left* *docked-panel-content-col*
            *docked-panel-pad-right* (if scrollbar 1 0))))

(defun dialog-list-viewport (total selected offset height)
  "The Zig computeDialogListViewport contract: => (values SELECTED OFFSET
VISIBLE-COUNT) for TOTAL rows shown in a HEIGHT-row body, with the window
pulled just far enough to keep SELECTED inside it."
  (if (or (zerop total) (zerop height))
      (values 0 0 0)
      (let ((visible (min total height))
            (selected (min selected (1- total))))
        (values selected
                (alexandria:clamp (min offset (- total visible)) (- selected (1- visible)) selected)
                visible))))

(defun dialog-body-viewport (dialog height)
  "=> (values SELECTION OFFSET VISIBLE) for DIALOG's list in a HEIGHT-row
body, and settle VIEW-OFFSET to what it answered."
  ;; The window is the dialog's own stored one, pulled just far enough to keep
  ;; the selection inside it, which is what lets the two gestures coexist: a
  ;; wheel scroll moves the window and leaves the highlight under the pointer,
  ;; while keyboard navigation moves the highlight and drags the window after
  ;; it. Paint and hit-test both come through here, so they read the same
  ;; settled number — the row the operator sees highlighted is the row the
  ;; pointer is on.
  ;;
  ;; A heading stays with the rows under it: when the selection drags the
  ;; window up onto the first row under one, the heading comes into view with
  ;; it. Only that drag -- a window the wheel put somewhere stays there.
  (let ((rows (picker-visible-rows dialog)))
    (multiple-value-bind (selection offset visible)
        (dialog-list-viewport (length rows) dialog.selection dialog.view-offset height)
      (when (> dialog.view-offset selection)
        (loop while (and (plusp offset)
                         (picker-row-heading (nth (1- offset) rows))
                         (< (- selection (1- offset)) visible))
              do (decf offset)))
      (setf dialog.view-offset offset)
      (values selection offset visible))))

(defun dialog-entry-segs (row selected &key marker tabbed room columns)
  "One docked list row (the Zig renderDockedRow family): the primary column
in the dialog accent tone while selected and the system tone otherwise, every
trailing field muted, two spaces between columns."
  ;; The accent is :dialog-accent rather than Zig's :composer — same choose
  ;; chain, but :composer belongs to the composer box, and a layer that
  ;; neutralizes that box would otherwise leave the selected row the same grey
  ;; as every other one. A row carrying KEY reads as the Zig label/value shape
  ;; (renderLabelValueDockedRow) — the field name leads and its value follows
  ;; muted. SUFFIX is selected-row-only chrome, the Zig files preview that
  ;; would drown the list on every row.
  ;;
  ;; MARKER adds the two-column `> ` gutter Zig spends on its option rows
  ;; (renderCheckpointRestoreOptionRow). The panel asks for it only under a
  ;; profile that paints no color, where tone alone cannot say which row is
  ;; selected — Zig degrades the same way, but a shell with no colors is still
  ;; expected to be usable. Every row in the list gets the gutter, so the
  ;; columns stay aligned within a profile.
  ;;
  ;; TABBED says the dialog's tab row already names the row's bucket, so the
  ;; row does not spend a column saying it again (PICKER-DIALOG-TABS).
  ;;
  ;; ROOM is the columns the panel row gives the entry (DOCKED-PANEL-ROOM).
  ;; The selected row's PATH takes what the rest leaves of it, cut from the
  ;; middle (CLIPPED-PATH): cut at the edge, a session's folder would lose its
  ;; tail, which is the part that names it. Under eight columns left it is not
  ;; drawn.
  (let* ((key (if tabbed "" row.key))
         (label row.label)
         (detail (concatenate 'string row.detail (if selected row.suffix "")))
         (primary (cond (row.heading :muted) (row.tone) (selected :dialog-accent) (t :system)))
         (key-segs (picker-field-segs row :key key primary))
         (label-segs (picker-field-segs row :label label (if (plusp (length key)) :muted primary)))
         (key-segs (if columns (clip-segs key-segs (first columns) :mark "…") key-segs))
         (label-segs (if columns (clip-segs label-segs (second columns) :mark "…") label-segs))
         (key-width (if columns (first columns) (segs-display-width key-segs)))
         (label-width (if columns (second columns) (segs-display-width label-segs)))
         (mark-width (if columns (third columns) (text-display-width row.mark)))
         (segs (flet ((pad (gap want have) (make-string (+ gap (max 0 (- want have))) :initial-element #\Space)))
                 (append
                  (when marker (list (seg (if selected "> " "  ") primary)))
                  (when (plusp mark-width)
                    (list (seg row.mark row.mark-tone) (seg (pad 1 mark-width (text-display-width row.mark)) primary)))
                  (when (plusp key-width)
                    (append key-segs (list (seg (pad 2 key-width (segs-display-width key-segs)) primary))))
                  label-segs
                  (when (plusp (length detail))
                    (list (seg (concatenate 'string (pad 2 label-width (segs-display-width label-segs)) detail)
                               :muted))))))
         (left (and selected room (plusp (length row.path)) (- room (segs-display-width segs) 2))))
    (if (and left (>= left 8))
        (append segs (list (seg (concatenate 'string "  " (clipped-path (home-abbreviated row.path) left)) :muted)))
        segs)))

;;; The two panel reads that are still derived. Everything else the chrome
;;; needs — searchable, the tab context, the summary row, the footer copy —
;;; IS a picker slot: the /models wizard is a picker whose steps write those
;;; slots as the flow moves (CONNECT-ENTER-STEP), so no read below branches
;;; on which kind of dialog is open.

(defun dialog-header-row-count (dialog)
  "Header rows the panel reserves (Zig DockedDialogOptions header_rows): the
tab row, a blank, the summary -- one row, or its SUMMARY-ROWS -- and a blank."
  (or dialog.header-rows
      (+ 3 (max 1 (length dialog.summary-rows))))) 
(defun dialog-footer-row-count (dialog)
  "Footer reservation; :NONE removes both hint copy and its space."
  (if (eq :none (picker-dialog-footer-hints dialog)) 0 *docked-dialog-footer-rows*))
   ; Zig keymap_dialog_header_rows

;;; Panel geometry. The Zig docked area spans from its own top edge down to
;;; shellContentBottomRow, and the transcript yields the rows it covers
;;; (reserveTranscriptAboveDialog). Its height is the dialog's content, under
;;; the cap; without the cap, Zig's full-height panel would leave a short list
;;; blank rows the transcript could have had, the /sessions preview above it
;;; first of all.

(defun dialog-body-height (dialog width)
  "The body rows DIALOG's panel holds at WIDTH: the field rows of an input or
result step, else one for each row its list shows, or one for its empty
label -- and while a query is in its box, never fewer than it held as the
query began."
  ;; The panel follows its rows both ways: a fold that opens grows it and the
  ;; fold closing gives the rows back. A query alone holds it: the search row is
  ;; the panel's top edge, so a query narrowing the list would move the caret
  ;; under the typing. Cleared, the box lets the panel follow its rows again.
  (let ((rows (if-let (field (dialog-field-rows dialog width nil))
                (length field)
                (max 1 (length (picker-visible-rows dialog)))))
        (held (and (plusp (length dialog.search)) (picker-body-floor dialog))))
    (setf (picker-body-floor dialog) (max rows (or held 0)))))

(defun docked-dialog-frame-rows (model)
  "=> (values PANEL-HEIGHT ROWS-ABOVE ROWS-BELOW) for the open dialog, or NIL."
  ;; ROWS-ABOVE counts the frame rows before the panel's first painted row and
  ;; ROWS-BELOW those after its last, so the three always stack to TERM-HEIGHT
  ;; with the panel's painted rows between them.
  ;;
  ;; The area's last row is the canvas gap above the shell footer, so the
  ;; panel paints one row fewer than it spans.
  ;;
  ;; The area's bottom edge is the Zig shellContentBottomRow — the footer row
  ;; on a window tall enough to carry a footer, the window's own bottom
  ;; otherwise — and the frame head above it — the blank row, the header row
  ;; and the chat top-padding row — is out of the panel's reach (Zig's
  ;; full-height panel covers the padding row; here it stays the head's, so a
  ;; panel never touches the header). Under the shell's DIALOG-FULLSCREEN flag
  ;; the panel spans that whole reach, whatever the dialog's own rows say;
  ;; otherwise it is its chrome and its body (DIALOG-BODY-HEIGHT) under
  ;; *DIALOG-FRAME-MAX-HEIGHT*. A sheet docks nothing: its window stands
  ;; over the frame (SHEET-OVERLAY), which keeps its composer.
  (when-let (dialog (and (not (sheet-dialog-p (chat-model-dialog model)))
                         (chat-model-dialog model)))
    (let* ((height (max 4 (chat-model-term-height model)))
           ;; Every row under the frame head down to the shell footer, the
           ;; whole of a fullscreen panel. The head is three rows — the
           ;; blank, the header, the chat top-padding row — and the panel
           ;; reaches none of them: Zig's full-height panel covers the
           ;; padding row, so a fullscreen panel sits one canvas row under
           ;; the header the way a docked one sits one under the transcript.
           (reach (if (shell-footer-p height) (- height 5) (- height 3)))
           ;; The padding row and the canvas gap beside the header and footer.
           (chrome (+ (dialog-header-row-count dialog) (dialog-footer-row-count dialog) 2))
           (width (frame-width model))
           (panel (if (chat-model-dialog-fullscreen model)
                      reach
                      ;; A picture is painted whole or not at all, so the
                      ;; panel grows past the list's cap to hold it whole
                      ;; and the list under it; the line standing in for it
                      ;; is two rows of the list's panel.
                      ;; A picture beside the list holds the body to its
                      ;; own lines however short the list.
                      (multiple-value-bind (picture whole lead)
                          (dialog-picture-rows dialog (- reach chrome) width)
                        (min reach (+ chrome picture
                                      (max (dialog-body-height dialog width)
                                           (if lead
                                               (ceiling (length (getf (picker-pictures dialog) :rows)) 2)
                                               0)))
                             ;; /status and the privacy box are read whole, never scrolled.
                             (if (or whole (member (picker-dialog-kind dialog) '(:status :privacy)))
                                 reach *dialog-frame-max-height*))))))
      ;; Under 5 rows no panel: Zig's header-only strip has no shape in this shell.
      (when (>= panel 5)
        (if (shell-footer-p height)
            ;; …the panel's own canvas gap row, the footer, the final blank.
            (values panel (- height 2 panel) 3)
            (values panel (- height panel) 1))))))

(defun docked-dialog-above-bands (above &aux (slack (max 0 (- above 2)))
                                             (pad (min 1 slack))
                                             (gap (min 1 (- slack pad))))
  "=> (values PAD GAP BAND): how the ABOVE rows of a docked frame divide,
after the blank and header rows every frame owns."
  ;; PAD is the chat top-padding row (always present: the panel's reach stops
  ;; above it), GAP the one canvas row Zig leaves between the transcript and
  ;; the panel (reserveTranscriptAboveDialog stops the band at `dialog.area.y
  ;; - 1`, so that row paints neither transcript nor panel surface), and BAND
  ;; the transcript rows that remain. Both the frame stack and the viewport
  ;; reservation read this one seam, so they cannot disagree about which row
  ;; the separator is.
  (values pad gap (- slack pad gap)))

(defun docked-dialog-third-row-segs (dialog)
  "The header's third row: the live search query, or the placeholder, on a
searchable dialog; the summary line otherwise."
  ;; No caret glyph paints here — the hardware cursor parks just past the
  ;; query instead (Zig dockedDialogSearchFocus; COMPOSE-FRAME parks it).
  (cond ((not dialog.searchable)
         (when (plusp (length dialog.subtitle)) (list (seg dialog.subtitle dialog.summary-tone))))
        ((plusp (length dialog.search)) (list (seg dialog.search :composer)))
        (t (list (seg (or (json-value dialog.search-placeholder :text) "Type to search") :muted)))))

(defun docked-dialog-tab-segs (dialog &aux (none (eq (effective-color-profile) :none)))
  "DIALOG's tab strip as header segments, or NIL for a dialog with none:
every tab a filled chip, ` label  count `, two columns apart -- the one
selected in the sheet's held chip, the rest in its plain one -- the chips
/settings' options are, so a tab reads as one thing to press, and a click on
one shows it (PICKER-TAB-PICK)."
  ;; A chip keeps its own ground on the panel (a style given whole); under
  ;; the profile that paints no color the held one stands in brackets, as a
  ;; held option does on /settings.
  (loop for tab in (picker-dialog-tabs dialog)
        for index from 0
        for on = (= index (picker-dialog-tab dialog))
        for text = (format nil "~a~@[  ~:d~]" (getf tab :label) (getf tab :count))
        when (plusp index) collect (seg "  " :muted)
        collect (seg (if (and on none) (format nil "[~a]" text) (format nil " ~a " text))
                     (style-for-tone (if on :sheet-chip-on :sheet-chip)) :link (cons :dialog-tab index))))

(defun docked-dialog-header-rows (dialog width surface count)
  "The panel header, truncated to COUNT rows: the tab row at column 0, a
blank, the search-or-summary row at *DOCKED-PANEL-SEARCH-ROW* -- or the
dialog's SUMMARY-ROWS from there -- a blank."
  ;; A :dialog-accent title reads as the tab row and drops bold to match the
  ;; chrome (Zig renderTitledDockedHeader, which spends :composer there); any
  ;; other tone keeps it.
  (let* ((tabs (picker-dialog-tabs dialog))
         ;; A tabbed dialog's title is chrome, not the highlighted label: the
         ;; strip's selected tab says which list is on screen, and two
         ;; highlighted labels would each claim the row.
         (title-tone (if tabs :muted dialog.title-tone))
         (bold (and (not tabs) (not (eq dialog.title-tone :dialog-accent))))
         ;; A tabbed dialog's context is no tab: it stands flush right of the
         ;; strip, apart from the chips (`⠹ 1 running', `12 matches').
         (context (string-trim " " dialog.context))
         (rows (append
                (list (docked-panel-row
                       (append (when (or (not tabs) (plusp (length dialog.title)))
                                 (list (seg (format nil " ~a ~:[~; ~]" dialog.title tabs) title-tone :bold bold)))
                               (docked-dialog-tab-segs dialog)
                               (when (and (not tabs) (plusp (length dialog.context)))
                                 (list (seg dialog.context :muted))))
                       width surface :col 0
                       :right (when (and tabs (plusp (length context))) (list (seg context :muted))))
                      (docked-panel-row '() width surface))
                (or (loop for segs in dialog.summary-rows
                          collect (docked-panel-row segs width surface :mark *dialog-row-overflow-mark*))
                    ;; A summary too long for the row wears the list rows' cut
                    ;; mark, where it stops at the edge. A live query stays
                    ;; bare: the caret parks past its end.
                    (list (docked-panel-row (docked-dialog-third-row-segs dialog)
                                            width surface
                                            :mark (and (not dialog.searchable)
                                                       *dialog-row-overflow-mark*))))
                (list (docked-panel-row '() width surface)))))
    (subseq rows 0 (min count (length rows)))))

(defparameter *dialog-input-field-gap* 2
  "Columns between an input field's label and its value (Zig
connectDialogInputFocus field_col).")

(defun dialog-input-field (dialog)
  "=> (values LABEL SHOWN-VALUE) for a dialog in input mode (the Zig
connectInputFieldLabel pair), or NIL for every list body: the text, or one
* per byte of a secret — never the secret."
  ;; One seam: the field row paints from it and the hardware cursor parks from
  ;; it.
  (when (and dialog dialog.input-label)
    (values dialog.input-label
            (if dialog.input-secret-p
                (make-string (length dialog.input) :initial-element #\*)
                dialog.input))))

(defparameter *picture-beside-gap* 4
  "Columns between a picture standing beside the list and the list.")

(defparameter *picture-beside-list-room* 36
  "The fewest columns a list keeps beside a picture; a narrower panel stands
the picture over it.")

(defun dialog-picture-rows (dialog body width &aux (picture (picker-pictures dialog)))
  "=> (values ROWS WHOLE LEAD): the rows DIALOG's picture takes at the top of
a BODY-row panel body WIDTH columns wide, the blank under it counted, and
whether it is painted whole -- else one line in its place says what it is
and that a larger window shows it. 0 for a dialog with none, or a body too
short to say even that. LEAD is the columns a picture standing beside the
list takes left of it, its gap counted: then ROWS is 0, nothing stands over
the list."
  ;; One reading for the paint and the hit-test (DIALOG-BODY-BAND): the list
  ;; starts where this says the picture ends.
  (if (null picture)
      0
      (let* ((rows (getf picture :rows))
             (lines (ceiling (length rows) 2))
             (wide (reduce #'max rows :key #'length))
             (room (docked-panel-room width)))
        (cond ((and (getf picture :beside)
                    (<= lines body)
                    (<= (+ wide *picture-beside-gap* *picture-beside-list-room*) room))
               (values 0 t (+ wide *picture-beside-gap*)))
              ((and (<= (+ lines 2) body) (<= wide room))
               (values (1+ lines) t nil))
              ((>= body 3) (values 2 nil nil))
              (t 0)))))

(defun picture-lines (rows paper-lit &aux (wide (reduce #'max rows :key #'length)))
  "ROWS (strings of `1' ink and `0' paper) as text, two pixel rows to a line
in half blocks. PAPER-LIT draws the paper as the glyph, for a panel whose
text is lighter than its ground, and the ink otherwise: paper reads light and
ink dark on either, which is what a camera reading a code needs."
  (flet ((lit-p (row x)
           ;; Past a short row is paper; past the last row is the ground.
           (and row (eq paper-lit (not (and (< x (length row)) (char= #\1 (char row x))))))))
    (loop for (top bottom) on rows by #'cddr
          collect (coerce (loop for x below wide
                                for up = (lit-p top x)
                                for down = (lit-p bottom x)
                                collect (char "█▀▄ " (+ (if up 0 2) (if down 0 1))))
                          'string))))

(defun picture-palette-segs (rows palette surface)
  "ROWS of PALETTE digits as segment lines, two pixel rows to a line: each
cell an upper half block whose ink is the top pixel's tone and whose ground
is the bottom one's, an empty or missing tone the panel's SURFACE."
  ;; A tone names its color by its foreground, or its ground when it has
  ;; none. Runs of one pair of colors paint as one segment.
  (let ((colors (map 'vector (lambda (name)
                               (and (plusp (length name))
                                    (let ((style (style-for-tone (intern (string-upcase name) :keyword))))
                                      (or (visual-style-fg style) (visual-style-bg style)))))
                     palette))
        (wide (reduce #'max rows :key #'length)))
    (flet ((color (row x &aux (index (and row (< x (length row)) (digit-char-p (char row x) 36))))
             (or (and index (< index (length colors)) (aref colors index)) surface)))
      (loop for (top bottom) on rows by #'cddr
            collect (loop for pairs = (loop for x below wide
                                            collect (cons (color top x) (color bottom x)))
                            then (nthcdr run pairs)
                          for pair = (first pairs)
                          for run = (or (position pair pairs :test-not #'equal) (length pairs))
                          while pairs
                          collect (seg (make-string run :initial-element #\▀) (vstyle :fg (car pair) :bg (cdr pair))))))))

(defun picture-segs (picture surface &aux (rows (getf picture :rows)))
  "PICTURE's lines as segment lists on a panel of SURFACE: in its palette's
tones, or -- with no palette, or under a profile that paints no color -- in
ink and paper (PICTURE-LINES)."
  (if (and surface (getf picture :palette))
      (picture-palette-segs rows (getf picture :palette) surface)
      (let* ((fg (visual-style-fg (style-for-tone :foreground)))
             ;; No surface is the :none profile, painted as a dark terminal's.
             (paper-lit (or (null surface) (null fg) (> (reduce #'+ fg) (reduce #'+ surface))))
             (ink (if (getf picture :palette)
                      ;; Ink and paper of a palette picture: a tone is ink, its
                      ;; first entry (the ground) paper.
                      (mapcar (lambda (row) (substitute-if #\1 (lambda (char) (char/= char #\0)) row))
                              rows)
                      rows)))
        (mapcar (lambda (line) (list (seg line :foreground))) (picture-lines ink paper-lit)))))

(defun docked-dialog-picture-rows (dialog width surface whole
                                   &aux (picture (picker-pictures dialog)))
  "DIALOG's picture as panel rows and a blank under it: the picture when
WHOLE, else the one line that stands in for it."
  (append
   (if whole
       (mapcar (lambda (segs) (docked-panel-row segs width surface))
               (picture-segs picture surface))
       (list (docked-panel-row
              (list (seg (format nil "~a: a larger window shows it"
                                 (or (json-value (getf picture :label) :text) "The picture"))
                         :muted))
              width surface)))
   (list (docked-panel-row '() width surface))))

(defun docked-dialog-body-rows (dialog width surface count)
  "The panel body, always exactly COUNT rows: the dialog's picture, when it
carries one, then the list under it -- or, standing beside the list, the
list with the picture in its lead lane."
  (multiple-value-bind (picture whole lead) (dialog-picture-rows dialog count width)
    (if lead
        (docked-dialog-list-rows dialog width surface count
                                 :lead (picture-segs (picker-pictures dialog) surface)
                                 :lead-width lead)
        (append (and (plusp picture) (docked-dialog-picture-rows dialog width surface whole))
                (docked-dialog-list-rows dialog width surface (- count picture))))))

(defun dialog-field-rows (dialog width surface)
  "The rows DIALOG's body shows in place of a list: the input mode's field,
or the /models wizard's check and its result -- the lead row, the lines a
result breaks to, a blank, the muted context -- or NIL for a list body."
  (multiple-value-bind (lead context more)
      (cond
        ((picker-dialog-input-label dialog)
         ;; The field name, the gap, then the value — `(empty)' while
         ;; nothing is typed; the hardware cursor is the caret.
         (multiple-value-bind (label value) (dialog-input-field dialog)
           (values (list (seg label :system)
                         (seg (format nil "~va" *dialog-input-field-gap* "") :system)
                         (if (plusp (length value)) (seg value :composer) (seg "(empty)" :muted)))
                   dialog.input-context)))
        ((and (connect-dialog-p dialog)
              (or (eq :result (connect-dialog-step dialog))
                  (connect-waiting-step-p (connect-dialog-step dialog))))
         ;; A result's words break to the panel's width, as a toast's
         ;; do, and MORE carries the lines after the first: a refusal
         ;; ran its remedy off the edge, past column 72 at 80 columns.
         (let* ((checking (connect-waiting-step-p (connect-dialog-step dialog)))
                (room (max 8 (docked-panel-room width)))
                (lines (if checking
                           (list (connect-checking-clock dialog))
                           (loop for line in (split-lines (connect-dialog-result-message dialog))
                                 nconc (wrapped-lines line room)))))
           (values (list (seg (or (first lines) "")
                              (if checking :muted :danger)))
                   (connect-provider-name dialog)
                   (if (eq :signing-in (connect-dialog-step dialog))
                       ;; under the clock, where to sign in -- the page a link,
                       ;; its query cut, then the whole address verbatim and
                       ;; dimmed when it carries more than the page -- and the
                       ;; device code when there is one; a command that
                       ;; answered no page says its words
                       (multiple-value-bind (url code) (sign-in-facts (connect-dialog-result-message dialog))
                         (if url
                             ;; a blank row ahead of each part, so the clock,
                             ;; the page, the address and the code stand apart
                             (loop for part in (list (list (list (seg (format nil "Sign in at ~a" (sign-in-page-label url))
                                                                      :foreground :link url)))
                                                     (when (find-if (lambda (c) (member c '(#\? #\#))) url)
                                                       (loop for piece in (wrapped-lines url room)
                                                             collect (list (seg piece :muted :link url))))
                                                     (and code (list (list (seg (format nil "Code ~a" code) :foreground :bold t)))))
                                   when part nconc (cons '() part))
                             (loop for line in (split-lines (connect-dialog-result-message dialog))
                                   nconc (wrapped-lines line room))))
                       (rest lines))))))
    (when lead
      (append (list (docked-panel-row lead width surface))
              (loop for line in more
                    collect (docked-panel-row (if (stringp line) (list (seg line :danger)) line)
                                              width surface))
              (list (docked-panel-row '() width surface))
              (when (plusp (length context))
                (list (docked-panel-row (list (seg context :muted)) width surface)))))))

(defun docked-dialog-list-rows (dialog width surface count &key lead (lead-width 0))
  "The panel body's list: the windowed list rows with the scrollbar column,
the wizard's input body, or the empty label — always exactly COUNT rows.
LEAD, segment lines, stands in a LEAD-WIDTH lane left of the list, a line a
row (DOCKED-PANEL-ROW's LEAD): a picture beside it."
  (when (plusp count)
    ;; Clamped to COUNT and blank-padded up to it: the body always fills its height.
    (let ((rows (or (dialog-field-rows dialog width surface)
                  (let* ((rows (picker-visible-rows dialog))
                         (total (length rows))
                         (tabbed (and (picker-dialog-tabs dialog) t))
                         (columns (flet ((segs-width (row field text)
                                           (segs-display-width
                                            (picker-field-segs row field text :system))))
                                    (loop for row in (picker-dialog-rows dialog)
                                          maximize (segs-width row :key row.key) into key
                                          maximize (segs-width row :label row.label) into label
                                          maximize (text-display-width row.mark) into mark
                                          finally (return (list (if tabbed 0 (or key 0))
                                                                (or label 0) (or mark 0))))))
                         (room (max 1 (- (docked-panel-room width t)
                                         lead-width
                                         (if (or (null surface) dialog.option-marker) 2 0)
                                         (if (plusp (third columns)) (1+ (third columns)) 0)))))
                    ;; Leave a readable description lane on narrow panels; clip
                    ;; overlong labels before padding, not through their detail.
                    (when (some (lambda (row) (plusp (length row.detail))) rows)
                      (setf (first columns) (min (first columns) (floor room 3))
                            (second columns) (min (second columns)
                                                  (max 1 (- room (if (plusp (first columns)) (+ 2 (first columns)) 0)
                                                            (max 14 (floor room 2)))))))
                    (if (zerop total)
                        ;; The body's own copy while no row shows. A live query that
                        ;; hid a non-empty list reads differently from a list that
                        ;; was empty to begin with (the Zig empty_label swap); a
                        ;; dialog that declares no distinguished copy — the wizard's
                        ;; non-searching steps, whose plain copy is the field's rule
                        ;; line — falls through to the plain one.
                        (list (docked-panel-row
                               (let ((label (or (and dialog.searchable
                                                     (plusp (length dialog.search))
                                                     (plusp (length dialog.rows))
                                                     dialog.no-match-label)
                                                dialog.empty-label)))
                                 (when (plusp (length label)) (list (seg label :muted))))
                               width surface :lead (first lead) :lead-width lead-width))
                        (bind (((selection offset visible) (dialog-body-viewport dialog count))
                               ((thumb-top thumb-height)
                                (scrollbar-thumb count visible total offset
                                                 :round-fn #'ceiling :fits-nil t)))
                          (loop for row from 0 below count
                                for index = (+ offset row)
                                for entry = (and (< index (+ offset visible)) (nth index rows))
                                for paint = (and entry (picker-row-paint entry))
                                for room = (- (docked-panel-room width thumb-height) lead-width)
                                collect (if paint
                                            ;; A row that lays itself starts
                                            ;; at the content column's own
                                            ;; cell, its edge there, on the
                                            ;; ground it names, and keeps one
                                            ;; cell of the panel's between it
                                            ;; and the bar.
                                            (let ((lane (- (+ room *docked-panel-content-col*)
                                                           (if thumb-height 1 0))))
                                              (multiple-value-bind (segs ground)
                                                  (funcall paint (= index selection) lane)
                                                (docked-panel-row
                                                 (if thumb-height
                                                     (let ((kept (clip-segs segs lane :mark *dialog-row-overflow-mark*)))
                                                       (append kept
                                                               (list (seg (make-string (max 0 (- lane (segs-display-width kept))) :initial-element #\Space)
                                                                          :muted)
                                                                     (seg " " (if surface (vstyle :bg surface) :muted)))))
                                                     segs)
                                                 width surface
                                                 :col 0
                                                 :ground (if (and surface ground)
                                                             (visual-style-bg (style-for-tone ground))
                                                             surface)
                                                 :scrollbar (scrollbar-cell thumb-top thumb-height row)
                                                 :mark *dialog-row-overflow-mark*
                                                 :lead (nth row lead) :lead-width lead-width)))
                                            (docked-panel-row
                                             (when entry
                                               (dialog-entry-segs
                                                entry
                                                (= index selection)
                                                ;; A tabbed dialog spends no
                                                ;; column on the bucket its tab
                                                ;; row says (PICKER-DIALOG-TABS).
                                                :tabbed tabbed :columns columns
                                                :marker (or (null surface) dialog.option-marker)
                                                :room room))
                                             width surface
                                             :scrollbar (scrollbar-cell thumb-top thumb-height row)
                                             :mark *dialog-row-overflow-mark*
                                             :lead (nth row lead) :lead-width lead-width)))))))))
      (loop for row below count
            collect (or (pop rows)
                        (docked-panel-row '() width surface
                                          :lead (nth row lead) :lead-width lead-width))))))

(defun dialog-panel-rows (model width bg &aux (dialog (chat-model-dialog model))
                                              (panel (docked-dialog-frame-rows model)))
  "The open dialog's docked panel: the painted rows COMPOSE-FRAME stacks over
the band's tail, the transcript/composer gap, and the composer."
  ;; NIL while no dialog is open. The footer is a blank, the hint row, a
  ;; blank, truncated to its band; a one-row footer keeps only the blank —
  ;; Zig's renderer prints its hint at row 1 and returns early below two rows.
  (when (and dialog panel)
    (let* ((surface (docked-panel-surface-bg))
           (inner (- panel 2))
           (header (min (dialog-header-row-count dialog) inner))
           (footer (min (dialog-footer-row-count dialog) (- inner header)))
           ;; The key-hint sets by dialog kind, verbatim from the Zig renderDockedKeyHintFooter
           ;; call sites — keys and their trailing spacing exactly as Zig writes them,
           ;; minus chords this shell does not bind (the session tree's ctrl+f fork and
           ;; tab filter, both Zig-only, would name keys that do nothing here; ctrl+f
           ;; is the fullscreen toggle every dialog shares, appended once below).
           ;; /sessions says only what its rows do -- Esc, the arrows and typing
           ;; are every list's -- so its row holds five caps.
           (hints (if (consp dialog.footer-hints)
                      dialog.footer-hints
                      (cdr (assoc dialog.footer-hints
                              '((:sessions ("enter" . "open") ("tab" . "this folder/all")
                                 ("→" . "forks") ("f2" . "rename"))
                                (:session-tree ("enter" . "navigate") ("esc" . "close")
                                 ("up/down" . "select")
                                 ("left/right" . "fold"))
                                (:checkpoint ("enter" . "select") ("esc" . "close")
                                 ("up/down" . "navigate"))
                                (:help ("esc" . "close") ("up/down" . "navigate")))))))
           (verb (if (chat-model-dialog-fullscreen model) "restore" "full screen"))
           ;; A docked panel stands where the composer was, so its key row says
           ;; the composer status for its first five seconds: a pick refused, a
           ;; key saved, a row's line run, while the panel stays or comes back.
           (said (let ((status (active-status model)))
                   (and status (< (since-ms (composer-status-born status)) 5000) status)))
           ;; Without key hints: the picker's own HINT, else the Zig default copy.
           (hint (or dialog.hint
                     (cond ((picker-dialog-searchable dialog)
                            "esc close  up/down navigate  type search")
                           ((picker-dialog-on-detail dialog)
                            "enter select  i details  esc close")
                           (t "esc close  up/down navigate")))))
      ;; One padding row above, then the three bands; the area's bottom
      ;; padding row is the canvas gap and belongs to the frame, not here.
      (append (list (docked-panel-row '() width surface))
              (docked-dialog-header-rows dialog width surface header)
              (docked-dialog-body-rows dialog width surface (- inner header footer))
              (subseq (list (docked-panel-row '() width surface)
                            ;; One unit per hint: a key-hint set's key and
                            ;; label, or a HINT's two-space-separated runs.
                            (if said
                                (docked-panel-row
                                 (list (seg (substitute #\Space #\Newline (composer-status-text said))
                                            (if (composer-status-error-p said) :danger :muted)))
                                 width surface :mark *dialog-row-overflow-mark*)
                            (docked-panel-row
                             (fitted-hint-segs
                              ;; Every hint's keys are filled caps, as a
                              ;; sheet's are (PAINT-KEYS): a key-hint set's,
                              ;; and a HINT's runs that name keys
                              ;; (HINT-KEY-UNITS); a run naming none stays
                              ;; words. A hint naming one key is that key to
                              ;; click (HINT-KEY-PRESS), its words lit under
                              ;; the pointer as a link's are.
                              (flet ((target (key)
                                       (and (hint-key-press key) (cons :key key)))
                                     (tone (target)
                                       (if (and target (equal target (chat-model-hover-target model)))
                                           :disclosure-hover
                                           :muted)))
                                (loop for unit in (append (or hints (hint-key-units hint))
                                                          `(("ctrl+f" . ,verb)))
                                      collect (if (consp unit)
                                                  (let ((target (target (car unit))))
                                                    (list (seg (format nil " ~a " (car unit)) (style-for-tone :key-chip) :link target)
                                                          (seg (format nil " ~a" (cdr unit)) (tone target) :link target)))
                                                  (list (seg unit :muted)))))
                              (docked-panel-room width))
                             width surface :mark *dialog-row-overflow-mark*))
                            (docked-panel-row '() width surface))
                      0 (min footer 3))))))

(defun hint-key-units (hint)
  "HINT -- a dialog's footer copy, its runs two spaces apart -- as a key-hint
set's units: (KEY . WORDS) for a run whose first word is a cap's
(HINT-KEY-LABEL-P), the run itself for one naming no key (`/link off
switches it', a sentence)."
  (loop for run in (ppcre:split "  +" (nlk:trimmed (or hint "")))
        for space = (position #\Space run)
        collect (if (and space (hint-key-label-p (subseq run 0 space)))
                    (cons (subseq run 0 space) (subseq run (1+ space)))
                    run)))

(defun fitted-hint-segs (units room &aux (used 0) (segs '()))
  "UNITS -- each one key hint's segments -- two columns apart, as many of them
from the first as ROOM columns hold whole: the list's order is its priority,
so the hints that do not fit drop from its end."
  ;; The first hint stands whatever its width -- a HINT that is one long
  ;; sentence -- and the row's own cut mark says where it stops.
  (dolist (unit units)
    (let ((width (+ (segs-display-width unit) (if segs 2 0))))
      (when (and segs (> (+ used width) room)) (return))
      (when segs (push (seg "  " :muted) segs))
      (setf segs (revappend unit segs))
      (incf used width)))
  (nreverse segs))

(defun dialog-list-selectable-p (dialog)
  "Whether DIALOG draws selectable list rows: a picker outside input mode,
including /models's provider step (also used by /setup), but not the
wizard's credential fields, check or result body."
  ;; The hover, the wheel, the bar and row clicks share this Boolean gate.
  (and (picker-dialog-p dialog)
       (not (sheet-dialog-p dialog))
       (null dialog.input-label)
       (or (not (connect-dialog-p dialog))
           (eq :provider (connect-dialog-step dialog)))))

(defun dialog-body-band (model dialog)
  "=> (values FIRST-ROW HEIGHT): the 1-based frame row DIALOG's list body
starts on and how many rows it spans, or NIL while no panel fits."
  ;; The decomposition is DIALOG-PANEL-ROWS's — the panel's padding row, the
  ;; header band, then the body — and this is the one authority for that band:
  ;; the hover hit-test and the wheel both measure the body through here.
  (multiple-value-bind (panel above) (docked-dialog-frame-rows model)
    (when panel
      (let* ((inner (- panel 2))
             (header (min (dialog-header-row-count dialog) inner))
             (body (- inner header (min (dialog-footer-row-count dialog) (- inner header))))
             ;; A picture above the list is not rows a pointer picks.
             (picture (dialog-picture-rows dialog body (max 8 (chat-model-term-width model)))))
        (values (+ above 2 header picture) (- body picture))))))

(defun dialog-body-row-at (model y &aux (dialog (chat-model-dialog model)))
  "=> (values BODY-ROW HEIGHT): the 0-based row of the open dialog's list body
under frame mouse Y (1-based), and that body's height; NIL when Y is outside
the body or the panel owns no list."
  (when (dialog-list-selectable-p dialog)
    (multiple-value-bind (first height) (dialog-body-band model dialog)
      (when (and first (<= first y (+ first (1- height))))
        (values (- y first) height)))))

(defun dialog-hover-index (model y)
  "Frame mouse Y (1-based) to the visible-row index under the pointer in
the open picker's list body, or NIL outside the body or past its last
occupied row — the Zig handleActiveDialogHover hit-test
(dialogBodySelectionIndex through the shared list viewport)."
  ;; The window it indexes into is the one the paint settled
  ;; (DIALOG-BODY-VIEWPORT), so the row under the pointer is the row the
  ;; operator sees highlighted.
  (bind (((row height) (dialog-body-row-at model y)))
    (when row
      (bind (((_ offset visible) (dialog-body-viewport (chat-model-dialog model) height)))
        (when (< row visible)
          (+ offset row))))))

(defun dialog-apply-hover (model y &aux (dialog (chat-model-dialog model)))
  "Pull the open picker's selection onto the row under the pointer — the
Zig handleActiveDialogHover seam shared by every list dialog."
  ;; => T when Y
  ;; lands on an occupied body row. A selection move re-fires the highlight
  ;; seam, so hover previews exactly like keyboard navigation.
  ;; DIALOG-HOVER-INDEX answers only over a picker's selectable list.
  ;; A heading row takes no hover, so a click on one opens nothing.
  (when-let (index (dialog-hover-index model y))
    (unless (picker-row-heading (nth index (picker-visible-rows dialog)))
      (unless (= index dialog.selection)
        (setf dialog.selection index)
        ;; A hover landing is a selection move: deliver instantly, the
        ;; same scrub-through contract as a held arrow.
        (picker-notify-highlight model dialog :instant t))
      t)))

;;; --- the list dialog's own scrollbar ---------------------------------------
;;; The picker paints a scrollbar (renderShortcutPickerScrollbar) like the
;;; transcript's gutter, so it answers the pointer like one: the bar is
;;; grabbable and the grab drags the window. The wheel (HANDLE-MOUSE-WHEEL)
;;; moves the window under a stationary pointer; the bar moves it under a held
;;; button, and the highlight rides the window's edge where the wheel leaves
;;; it under the pointer. One predicate decides which panels follow the mouse
;;; at all (DIALOG-LIST-SELECTABLE-P), so the hover, the wheel and the bar
;;; cannot disagree about the body they measure.

(defun dialog-scrollbar-metrics (model &aux (dialog (chat-model-dialog model)))
  "=> (values FIRST HEIGHT TOTAL VISIBLE), the open dialog's list bar: the
1-based frame row its track starts on, the track's height in rows, the row
total and the rows the window shows — the band DIALOG-BODY-BAND hands the
hover and the wheel, measured once more for the bar."
  ;; NIL while no list dialog is open or its panel holds no body.
  (when (dialog-list-selectable-p dialog)
    (multiple-value-bind (first height) (dialog-body-band model dialog)
      (when (and first (plusp height))
        (let ((total (length (picker-visible-rows dialog))))
          (values first height total (min total height)))))))

(defun apply-dialog-scrollbar-track-row (model track-row grab-offset)
  "Move the open dialog's list so its thumb top lands on TRACK-ROW minus
GRAB-OFFSET — the dialog's tail of APPLY-SCROLLBAR-TRACK-ROW, inverting the
picker's own ceiling thumb."
  ;;
  ;; The list carries a highlight where the transcript carries none, and the
  ;; settled window is the one the paint pulls the highlight into: the drag takes
  ;; the highlight with the window rather than leaving the paint to pull the
  ;; window back, so a row dragged out of the window keeps its place in the list
  ;; and only the window's edge moves it. Nothing happens while the list fits
  ;; (no bar is painted) or the track row is the only one there is.
  (bind (((_ height total visible) (dialog-scrollbar-metrics model)))
    (when (and height (plusp total) (plusp (- total visible)))
      (let ((offset (scroll-for-thumb-top
                     height visible total
                     (max 0 (- (max 0 (min track-row (1- height)))
                               grab-offset))
                     :round-fn #'ceiling))
            (dialog (chat-model-dialog model)))
        (setf dialog.view-offset offset
              dialog.selection
              (max offset (min dialog.selection
                               (+ offset (1- visible)))))))))

