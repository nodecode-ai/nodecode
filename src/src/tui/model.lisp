;;;; model.lisp --- the TUI model and its generics, initialization, windows, bands, clocks. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- TUI model and generics ---------------------------------------------
;;; view and update are the terminal core's CLOS generics (term/message.lisp),
;;; so the organism can redefine how it presents itself and how it interprets
;;; input.

(defmacro seg (text &optional tone &rest options)
  "A MAKE-SEG of TEXT in TONE, OPTIONS its other keys: the positional spelling
for the many bars and rows that paint one run of text in one tone."
  `(make-seg :text ,text :tone ,tone ,@options))

(defclass chat-model ()
  ((session-id :initarg :session-id :reader chat-model-session-id)
   (band :initform nil :accessor chat-model-band)
   (editor :initform nil :accessor chat-model-editor)
   (state :initform nil :accessor chat-model-state)
   ;; Where the terminal's hardware cursor parks this frame: (col . row),
   ;; zero-based frame cells, or NIL to hide it. COMPOSE-FRAME writes it
   ;; while stacking rows (the composer caret cell); SHELL-VIEW reads it
   ;; into the view-state so the renderer emits the real cursor — the Zig
   ;; composer.zig cursor_visible/cursor contract on vaxis.
   (parked-cursor :initform nil :accessor chat-model-parked-cursor)
   (term-width :initform 80 :accessor chat-model-term-width)
   (term-height :initform 24 :accessor chat-model-term-height)
   (turn-in-flight :initform nil :accessor chat-model-turn-in-flight)
   ;; Live status clock: when the in-flight turn started (internal time) and
   ;; the latest activity as (verb-string . internal-time). Chrome inputs for
   ;; the live turn bar; never durable.
   (turn-started-at :initform nil :accessor chat-model-turn-started-at)
   ;; Per-turn memo for the live bar's model label: (started-at-stamp . label).
   ;; The gateway lane's label reads the durable session pin from the store;
   ;; caching per turn keeps that read off the ~150ms animation path.
   (live-label-cache :initform nil :accessor chat-model-live-label-cache)
   ;; Frame-clock support. PROJECTION-CACHE holds the last full transcript
   ;; projection as (width palette profile lines plains refs) so an
   ;; animation tick can recompose the live chrome without re-projecting the
   ;; whole transcript (RENDER-STATUS-FRAME). The palette and profile ride
   ;; along because the projected rows are SGR-baked strings, so they are
   ;; stale the moment either moves (PROJECTION-CACHE-LIVE-P). TRANSCRIPT-DIRTY marks state mutated by a
   ;; stream reducer without an immediate render: the next frame-clock tick
   ;; folds it into one full render — the Zig stream_framerate coalescing
   ;; rule, which bounds re-projection to the frame cadence instead of the
   ;; stream's arrival rate. Both live-only.
   (projection-cache :initform nil :accessor chat-model-projection-cache)
   ;; The live chrome as STATUS-CHROME-KEY read it at the last compose; a
   ;; tick composes again only when it reads differently. Live-only.
   (status-chrome-key :initform nil :accessor chat-model-status-chrome-key)
   ;; (STATE . ROW): the transcript row the projection of STATE starts at,
   ;; NIL for its first. A state the window has not grown into projects its
   ;; tail (TRANSCRIPT-PROJECTION-START). Live-only.
   (projection-start :initform nil :accessor chat-model-projection-start)
   ;; A boot's composer catalog refresh, put off until the frame the attach
   ;; is for (REFRESH-COMPOSER-CATALOGS-BEHIND-THE-FRAME). Live-only.
   (catalog-refresh-due :initform nil :accessor chat-model-catalog-refresh-due)
   ;; The home mark's glow (PLAY-MARK-GLOW): NIL until a launch (NLT:INIT
   ;; with a program) or a click starts one, (KIND . START) while it plays,
   ;; :DONE once it has (MARK-GLOWING-P); the times of the clicks that may
   ;; yet make five (CLICK-MARK); how far the keys have come through the
   ;; Konami code (KONAMI-KEY). Live-only.
   (mark-glow :initform nil :accessor chat-model-mark-glow)
   ;; Whether the band the last compose stacked was the home screen, :NEVER
   ;; before the first (COMPOSE-TRANSCRIPT-VIEWPORT). Live-only.
   (home-painted :initform :never :accessor chat-model-home-painted)
   (mark-clicks :initform nil :accessor chat-model-mark-clicks)
   (konami-progress :initform 0 :accessor chat-model-konami-progress)
   (transcript-dirty :initform nil :accessor chat-model-transcript-dirty)
   (turn-activity :initform nil :accessor chat-model-turn-activity)
   ;; The live bar's count of the running turn's tool calls — `12 actions'
   ;; — counted at the full render from the rows, read by every animation
   ;; tick. Live-only.
   (turn-tally :initform nil :accessor chat-model-turn-tally)
   ;; The dropdown selection index. The dropdown itself is derived state:
   ;; active exactly while SLASH-SUGGESTIONS of the live composer text is
   ;; non-empty, so only the selection needs storing (clamped at read).
   (slash-selection :initform 0 :accessor chat-model-slash-selection)
   ;; Prompt history (Zig ComposerState history): newest-first entries,
   ;; the index while navigating (NIL when editing), and the stashed draft
   ;; the walk started from — restored when Down passes the newest match.
   (prompt-history :initform nil :accessor chat-model-prompt-history)
   (history-index :initform nil :accessor chat-model-history-index)
   (history-stash :initform "" :accessor chat-model-history-stash)
   (history-loaded :initform nil :accessor chat-model-history-loaded)
   ;; Whether the transcript follows its tail. T until the operator scrolls
   ;; up; re-pinned when scrolling returns to the bottom and on submit.
   ;; While pinned, every render snaps to the newest rows (the streaming
   ;; follow); unpinned, new content leaves the window where it is.
   (follow-bottom :initform t :accessor chat-model-follow-bottom)
   ;; The open modal picker (PICKER-DIALOG), or NIL. While open it owns
   ;; every key; the frame overlays it above the composer like the slash
   ;; dropdown (the Zig dialog family: /tree, /sessions, pickers).
   (dialog :initform nil :accessor chat-model-dialog)
   ;; `nodecode setup' asked for the setup wizard on the first frame -- or
   ;; `nodecode gateway setup' for a channel's setup sheet, (:channel-setup
   ;; CHANNEL); the first-run detector (SETUP-REQUIRED-P, setup.lisp) opens
   ;; the wizard unasked.
   (setup-requested :initarg :setup-requested :initform nil
                    :accessor chat-model-setup-requested)
   ;; The setup walk in progress (setup.lisp SETUP-RUN), or NIL: set where a
   ;; walk starts, cleared where it ends. Live-only. While it is on screen a
   ;; footer with no model configured names the walk, not the way to start
   ;; one (FOOTER-FRAME-ROW).
   (setup-walk :initform nil :accessor chat-model-setup-walk)
   ;; The landing screen's tip (HOME-TIP), picked at the first landing
   ;; screen and kept for the shell's life. Live-only.
   (home-tip :initform nil :accessor chat-model-home-tip)
   ;; Ctrl+F over an open dialog (TOGGLE-DIALOG-FULLSCREEN): T sizes the
   ;; docked panel to every row under the frame head, over the Zig docked
   ;; height and over a dialog's own BODY-ROWS. One preference for every
   ;; dialog, kept for the shell's life and never persisted (live-only): a
   ;; picker closed and reopened comes back at the size the operator chose.
   (dialog-fullscreen :initform nil :accessor chat-model-dialog-fullscreen)
   ;; The open dialog's transcript substitution (the Zig
   ;; sessions_dialog_preview lane): (SESSION-ID . STATE) while a picker
   ;; previews another session's folded history, NIL otherwise. Live-only
   ;; and detached — the live state stays resident underneath, so closing
   ;; the dialog is the restore, by construction.
   (transcript-preview :initform nil :accessor chat-model-transcript-preview)
   ;; Ctrl+O: Developer or Standard viewport. Live-only, shell-local.
   (tool-detail :initform :standard :accessor chat-model-tool-detail)
   ;; The one clicked-open card, group or thought's expand key (the Zig
   ;; expandedToolCallId accordion), or NIL.
   (expanded-tool-call :initform nil :accessor chat-model-expanded-tool-call)
   ;; The resolved keymap: action keyword -> KEY-CHORD list, defaults
   ;; overlaid with the ~/.nodecode/keybinds.json overrides once at
   ;; model creation (keys.lisp RESOLVE-KEYBINDS).
   (keymap :initform (resolve-keybinds) :accessor chat-model-keymap)
   ;; Collapsed pastes: (id . text) newest-last, expanded into the outgoing
   ;; prompt at submit (the Zig ComposerState.pastes).
   (pastes :initform nil :accessor chat-model-pastes)
   (paste-counter :initform 0 :accessor chat-model-paste-counter)
   ;; Images staged for the next prompt: (media-type . base64) — drained at
   ;; submit (the Zig ComposerState.images pending-attachment slot).
   (pending-images :initform nil :accessor chat-model-pending-images)
   ;; The composer's @ catalog, walked on a thread of its own and never on a
   ;; trigger's keystroke (START-COMPOSER-CATALOG-WALK); the -LOADED flag
   ;; means "the walk answered", so a trigger that finds it NIL asks for the
   ;; walk again rather than waiting on it.
   (file-catalog :initform nil :accessor chat-model-file-catalog)
   (file-catalog-loaded :initform nil :accessor chat-model-file-catalog-loaded)
   ;; Mouse state: the transcript line table (parallel painted/plain/row
   ;; vectors stamped by RENDER-STATE), hover row, drag-selection state,
   ;; the armed scrollbar drag, the armed tool-block press, and the armed   ;; link press (its URL).
   (line-plain :initform #() :accessor chat-model-line-plain)
   (line-rows :initform #() :accessor chat-model-line-rows)
   (hover-row :initform nil :accessor chat-model-hover-row)
   ;; The target under the pointer, so the span carrying it underlines while
   ;; the pointer is there and a reader can see what a click would reach: a
   ;; URI, or (:STOP . EXEC-ID) for the band's ✕, which lights instead.
   (hover-target :initform nil :accessor chat-model-hover-target)
   (selection :initform nil :accessor chat-model-selection)
   (selection-anchor :initform nil :accessor chat-model-selection-anchor)
   (selection-dragged :initform nil :accessor chat-model-selection-dragged)
   ;; The armed scrollbar drag, (SURFACE . GRAB): SURFACE :transcript for the
   ;; band's gutter or :dialog for the open picker's own bar; GRAB the rows
   ;; between the pointer and the thumb's top. One slot — one bar at a time.
   (scrollbar-drag :initform nil :accessor chat-model-scrollbar-drag)
   ;; Whether the pointer rests on the transcript's scroll rail, which lights
   ;; its thumb (RAIL-HOT-P).
   (rail-hover :initform nil :accessor chat-model-rail-hover)
   (pressed-tool-row :initform nil :accessor chat-model-pressed-tool-row)
   (pressed-link :initform nil :accessor chat-model-pressed-link)
   ;; The armed press on a band row's ✕: the exec id its release interrupts.
   (pressed-stop :initform nil :accessor chat-model-pressed-stop)
   ;; The armed press anywhere else on an exec's band lines: the exec id
   ;; its release opens or closes in place.
   (pressed-exec :initform nil :accessor chat-model-pressed-exec)
   ;; What the shell says about what the operator just did or asked, in the
   ;; composer's placeholder: a COMPOSER-STATUS, or NIL (SHOW-STATUS).
   (composer-status :initform nil :accessor chat-model-composer-status)
   ;; The toasts up on the frame, newest first (TOAST); what each notice
   ;; key last said, so a keyed notice toasts once for its words; the toast
   ;; under the pointer; and where the last frame placed them (PLACE-TOASTS).
   ;; Live-only.
   (toasts :initform nil :accessor chat-model-toasts)
   (toasts-seen :initform (make-hash-table :test #'equal) :accessor chat-model-toasts-seen)
   (toast-hover :initform nil :accessor chat-model-toast-hover)
   (toast-layout :initform nil :accessor chat-model-toast-layout)
   ;; Kitty graphics: T once the terminal answered the capability query
   ;; (init sends NLT:REQUEST-KITTY-GRAPHICS; absence of a reply leaves the
   ;; fallback rendering), plus the transmitted-image cache — payload text
   ;; -> NLT:TERMINAL-IMAGE (or :FAILED), bounded like the Zig AssetCache.
   (kitty-graphics :initform nil :accessor chat-model-kitty-graphics)
   (kitty-image-cache :initform (make-hash-table :test #'equal)
                      :accessor chat-model-kitty-image-cache)
   ;; The open /term line-projection panel (exec.lisp TERMINAL-PANEL), or
   ;; NIL. Live-only chrome fed by the same background-process frames the
   ;; band consumes; a session switch or resync starts panel-less.
   (program :initform nil :accessor chat-model-program))
  (:documentation "The live TUI model holding folded state and UI components."))

;;; --- initialization -----------------------------------------------------

(defun transcript-text-width (width)
  "Columns transcript rows wrap at: the frame width minus the 2-column left
inset and the 2 the scroll gutter keeps at the right edge, a blank and the
rail, so the margins match (the one width authority for transcript content)."
  ;; The Zig gutter kept 5 (2 blank, the bar, 2 blank): the text stopped five
  ;; columns short of the edge and the strip read as an empty pane beside it.
  (max 1 (- width 4)))

(defun composer-box-width (width)
  "Columns the composer box spans: the transcript's own, so the box ends where
the message boxes do and the scroll gutter's columns stay clear beside it."
  ;; Without this, the Zig box would span WIDTH - 4 and stick out three
  ;; columns past every message box above it.
  (transcript-text-width width))

(defun composer-text-inset ()
  "Columns between the composer box's left edge and its text: the prose
gutter a sent prompt's words start on, so typed text stays put on Enter."
  ;; The box and the transcript start on the same column. The text sits 1 in,
  ;; on the gutter rail. Without this, every sent prompt would jump right into
  ;; its band.
  (nth-value 2 (body-gutter :prose)))

(defun composer-text-width (width)
  "Columns the composer's text wraps at — the one width authority for
composer content, and a different number from TRANSCRIPT-TEXT-WIDTH even
where they happen to agree."
  ;; The text starts at COMPOSER-TEXT-INSET. TWO trailing columns stay
  ;; reserved: the parked hardware cursor takes the first when the caret sits
  ;; at the end of a full row, and the second stays padding — one whole cell
  ;; of it — so neither the text nor the caret ever touches the box's right
  ;; edge.
  (max 1 (- (composer-box-width width) (composer-text-inset) 2)))

(defun shell-footer-p (height)
  "Whether the shell carries the footer band and the boxed composer; short
windows drop both (Zig shellFooterRow returns null below 12 rows and the
composer falls back to its single-line mode)."
  (>= height 12))

(defun shell-overhead (height &optional (composer-rows 1))
  "Rows the shell chrome owns around the transcript, the Zig ChatShell
stack: blank row 0, the header row, the chat top-padding row (3), the
transcript/composer gap (1), then either the composer box — COMPOSER-ROWS
text rows between two pad rows — the composer/footer gap, the footer, and
the final blank, or, below 12 rows, the single-line composer and two
trailing blank rows (3). The box grows with its content, so the transcript
band yields exactly the extra text rows."
  (if (shell-footer-p height)
      (+ 9 (max 1 composer-rows))
      7))

(defun composer-max-rows (height)
  "The composer growth cap (Zig composerMaxVisibleTextRows, pi-mono editor
parity): 30% of the window, at least 5 rows, never past HEIGHT - 10 — the
nine chrome rows SHELL-OVERHEAD stacks around the box plus the one
transcript row the band keeps at minimum (Zig's HEIGHT - 8 assumes its own
stack; here it let a 12-row window compose 14). The short shell mode stays
single-line."
  (if (shell-footer-p height)
      (max 1 (min (max 5 (floor (* height 3) 10)) (- height 10)))
      1))

;;; --- the transcript window --------------------------------------------------

(defstruct (band-window (:constructor make-band-window (&key (height 1))))
  "The transcript band's scroll model: LINES the rows RENDER-STATE has already
painted and wrapped at the band's text width, HEIGHT the rows the band shows,
TOP the requested first visible row."
  ;; Read the offset through BAND-WINDOW-OFFSET, which clamps TOP to the
  ;; content, so a window that shrinks or a transcript that is cleared never
  ;; scrolls past the end. Nothing here wraps or paints: TRANSCRIPT-BAND-ROWS
  ;; paints, SCROLLBAR-THUMB derives the gutter, and the mouse maps frame rows
  ;; through TRANSCRIPT-BAND-METRICS.
  ;;
  ;; The content is two lists and one count, not one list: LINES is the
  ;; projected transcript and TAIL the live chrome composed under it, with
  ;; COUNT their rows. A frame that moves only the chrome appends to the tail
  ;; and leaves the projection alone; the paint used to rebuild the whole
  ;; content list, three lanes at a time, for every chrome region on every
  ;; frame.
  (lines '() :type list)
  (count 0 :type fixnum)
  (height 1 :type fixnum)
  (top 0 :type fixnum)
  ;; Appended after TOP: a slot added anywhere but the end moves every
  ;; accessor below it, and this structure is live in a running image.
  (base-count 0 :type fixnum)
  (tail '() :type list))

(nlk:access (window band-window))

;;; Written by BAND-WINDOW-SET-LINES, read by the scroll range, dropped when
;;; the content grows back to it or the tail is pinned (PIN-TRANSCRIPT-BOTTOM).
(nlk:define-side-table band-window-hold (:weakness :key)
  "Weak memo: band window -> the offset it holds past the last its content
reaches: the place of a window read off the tail that the content shrank under.")

(defun band-window-max-offset (window &optional (hold (band-window-hold window)))
  "The last scroll offset, including the pinned prompt's separator budget and
the place HOLD keeps (BAND-WINDOW-SET-LINES)."
  ;; A truncated prompt needs an upward step even when its answer fits:
  ;; scrolling off bottom exposes the retained inline prompt below the pin.
  ;; The prompt budgeted is the one governing the tail's top row, never the
  ;; newest: lifting a new prompt or steer to the top would clear the screen
  ;; above it.
  (bind ((end (max 0 (- window.count window.height)))
         (span (find-if (lambda (span) (<= (car span) end)) (band-window-prompt-spans window) :from-end t))
         ((_ _ spacer omitted) (if span (band-window-prompt-header window span) (values 0 0 0 0))))
    (max 0 (+ (- window.count window.height) spacer)
         (+ (or (car span) 0) (if (plusp omitted) 1 0))
         (or hold 0))))

(defun band-window-offset (window)
  "The first source row; a negative TOP is canvas inset, not history."
  (max 0 (min window.top (band-window-max-offset window))))

(defun (setf band-window-offset) (offset window)
  (setf window.top
        (max 0 (min offset (band-window-max-offset window)))))

(defun band-window-set-lines (window lines &optional tail hold &aux (base (length lines))
                                                                    (held (band-window-hold window))
                                                                    (was (and (or hold held) (copy-band-window window))))
  "Replace the content with LINES followed by TAIL; an offset past the new
end clamps, and the clamp is written through so a later height change starts
from it. HOLD says the operator reads the window off the tail: when the content
shrinks under rows that stand where they stood, the window keeps its place,
the rows the content left standing empty (BAND-WINDOW-HOLD) until it grows
back into them, they move, or the tail is pinned."
  ;; The two stay apart rather than concatenated: LINES is the projected
  ;; transcript, the same list object for every animation frame, and the
  ;; chrome in TAIL is all a frame moves.
  (setf window.lines lines
        window.tail tail
        window.base-count base
        window.count (+ base (length tail)))
  ;; A running Standard turn shows its newest words alone, so the next
  ;; message takes the place of every row the last one had: without this,
  ;; the operator, scrolled up during a turn, would be clamped back onto
  ;; the new bottom. The offset counts from the top, so rows above the
  ;; shrink stand still on their own; only the clamp moves them. The place
  ;; is held while the rows the window shows from its top down to its
  ;; first with text are the same rows at the same index -- a window whose
  ;; own rows left lands on the new bottom as before, never on a blank
  ;; band. Once held, the place is a floor of the scroll range: the wheel
  ;; comes back down to it and locks the follow there, with no jump, until
  ;; the content reaches it again.
  (when was
    (let* ((top window.top)
           (end (band-window-max-offset window nil))
           (place (max (or held 0) (if hold top 0)))
           (lead (and (> place end) (< -1 top (band-window-count was))
                      (loop for line in (band-window-range was top (min (band-window-count was) (+ top window.height)))
                            collect line until (not (blank-band-row-p line))))))
      (if (and lead (not (blank-band-row-p (car (last lead))))
               (<= (+ top (length lead)) window.count)
               (every #'equal lead (band-window-range window top (+ top (length lead)))))
          (setf (band-window-hold window) place)
          (forget-band-window-hold window))))
  ;; A disclosure click can preserve leading canvas slack. Ordinary scroll
  ;; setters clear it; recomposing live chrome must not spend that anchor.
  (unless (minusp window.top)
    (setf (band-window-offset window) window.top))
  window)

(defun band-window-scroll (window delta)
  "Move the window DELTA rows, negative for up, clamped to the content."
  (setf (band-window-offset window) (+ (band-window-offset window) delta))
  window)

(defun band-window-goto-top (window)
  (setf (band-window-offset window) 0)
  window)

(defun band-window-goto-bottom (window)
  (setf (band-window-offset window) (band-window-max-offset window))
  window)

(defun band-window-at-bottom-p (window)
  "Whether the window shows the last row."
  (>= (band-window-offset window) (band-window-max-offset window)))

;;; --- the pinned prompt band --------------------------------------------------
;;; The governing operator prompt stays at the top at every scroll offset,
;;; at rest, while streaming and without overflow. The response scrolls below
;;; it. BAND-WINDOW-PIECES is the single authority for paint and pointer/copy
;;; mapping; a short pinned transcript leaves its unused canvas below it.
(defparameter +max-pinned-prompt-rows+ 4
  "Rows of a user prompt block the pinned band paints while the rows under it
fill the band. A truncated block keeps its opening pad, first body line,
omission marker and final wrapped body line; rows the band has spare go to
more of its head.")

;;; Written by COMPOSE-TRANSCRIPT-VIEWPORT, the one place holding both the
;;; painted rows and the refs lane that names each line's owner, and read at
;;; slice time by BAND-WINDOW-PINNED: whether a prompt pins is a function of
;;; the scroll offset, so it cannot be baked into the projection itself.
(nlk:define-side-table band-prompt-spans (:weakness :key)
  "Weak memo: a projection's row list -> the content-index spans of its
user prompt blocks, in order.")

(nlk:define-side-table band-disclosure-spans (:weakness :key)
  "Live-only projection rows -> expanded process spans (START END TITLE-ROWS).")

(defun refs-disclosure-spans (refs key &optional (width 80)
                                     &aux (spans '()) (owner nil)
                                     (start nil) (title-rows 0) (index 0))
  "Open output spans (START END TITLE-ROWS), read from projected owners."
  ;; A process owns its nested file/member lines. Otherwise an open tool or
  ;; receipt pins its first logical title, including wrapping and opening pad.
  ;; No parsing painted words, and no invented header for titleless prose.
  (flet ((seal ()
           (when (and start (plusp title-rows))
             (push (list start index title-rows) spans))
           (setf owner nil start nil title-rows 0)))
    (dolist (ref refs)
      (let* ((group (cond ((tool-group-p ref) ref)
                          ((transcript-row-p ref) (row-groups ref))))
             (group (if (and group (eq :mutation (first group.key))) (row-groups group) group))
             ;; A run shut over a call open under its title owns it the same way.
             (group (and group (eq :process (first group.key)) (tool-group-open-p group (open-key key)) group))
             (row (and (not group) (transcript-row-p ref)
                       (or (and (eq :tool-call ref.kind) ref.tool-name
                                (equal key (tool-row-expand-key ref)))
                           (and (row-notice-summary ref) (eq key ref)))
                       (first (row-llines-cache ref)) ref))
             (exec (and (bg-band-row-p ref) (equal key (band-row-expand-key ref)) ref))
             (next (or group row exec)))
        (unless (eq next owner)
          (seal)
          (when next
            (setf owner next start index
                  title-rows (if row
                                 (+ (if (row-block-padding-p row) 1 0)
                                    (length (lline-render (first (row-llines-cache row)) width)))
                                 (if exec 1 0)))))
        (when (and group (eq ref group)) (incf title-rows)))
      (incf index))
    (seal))
  (nreverse spans))

(defun transcript-user-prompt-row-p (row)
  "Whether ROW is a prompt submitted here: the block the pinned band stands
for. A recorded pair's input (NLK:RECORD-EXCHANGE-TURN) is not one."
  (and (transcript-row-p row) (eq :text row.kind) (equal "user" row.role)
       (not row.recorded)))

(defun refs-prompt-spans (refs &aux (owner nil)
                                    (start 0))
  "REFS — a projection's per-line owners — as the content-index spans of its
user prompt blocks: one (START . END) cons per run of consecutive lines owned
by one prompt row."
  ;; A block owns its padding rows and the blank spacer TRANSCRIPT-LINE-TABLE
  ;; leaves between two filled blocks belongs to nobody, so a span is exactly
  ;; the rows the prompt's block paints.
  (nconc (loop for ref in refs
               for index from 0
               for prompt = (and (transcript-user-prompt-row-p ref) ref)
               unless (eq prompt owner)
                 append (prog1 (and owner (list (cons start index)))
                          (setf owner prompt start index)))
         (and owner (list (cons start (length refs))))))

(defun band-window-prompt-spans (window)
  "The prompt spans of the projection WINDOW shows, or NIL — the home screen's
rows are no projection, and a row list with no entry here pins none."
  (band-prompt-spans window.lines))

(defun band-window-after-prompt (window span &aux (end (cdr span)))
  "The first content row past SPAN's prompt block: past the blank row after
it — the unowned separator or the next block's canvas pad — which a pin's own
canvas gap stands for."
  (let ((next (and (< end window.count) (first (band-window-range window end (1+ end))))))
    (+ end (if (and next (blank-band-row-p next)) 1 0))))

(defun band-window-prompt-header (window span)
  "The prompt header's START, pinned ROWS, SPACER and omitted body rows."
  ;; Both ends own padding, not prompt text. If body text would be cut,
  ;; reserve the last two pinned rows for the marker and final wrapped body
  ;; line. Count only hidden body rows, once here for every consumer.
  (let* ((height (band-window-height window))
         (start (+ (car span) (if (= height 1) 1 0)))
         (full (- (cdr span) start))
         ;; The rows under the prompt, read only when they could leave the
         ;; band room: the scroll range asks on every offset read, and the
         ;; separator lookup walks the row list.
         (below (when (<= (- window.count (cdr span)) height)
                  (- window.count (band-window-after-prompt window span))))
         ;; The cap is for a band the rows under the prompt fill. Rows they
         ;; leave go to the prompt: whole when it fits over them with its
         ;; gap, else as many as fit over them with the closing pad and gap.
         ;; A capped pin over a short answer left the band half empty with
         ;; `[+4 lines omitted]' at its top.
         (rows (min full height
                    (cond ((null below) +max-pinned-prompt-rows+)
                          ((<= (+ full 1 below) height) full)
                          (t (max +max-pinned-prompt-rows+ (- height 2 below))))))
         ;; A truncated pin keeps its filled closing pad and a canvas gap.
         (spacer (if (plusp rows)
                     (min (- height rows) (if (< (+ start rows) (cdr span)) 2 1)) 0))
         ;; A band the pin would fill lets it go while live chrome -- the
         ;; bar -- ends the content: the work going on outranks the question.
         (rows (if (and window.tail (>= (+ rows spacer) height)) 0 rows))
         (omitted (if (and (plusp rows) (> (1- (cdr span)) (+ start rows)))
                      (- (- (cdr span) (car span) 2)
                         (+ (max 0 (- rows 2 (if (= start (car span)) 1 0)))
                            (if (>= rows 2) 1 0))) 0)))
    ;; This one budget feeds the scroll range, painted pieces and pointer lane.
    (values start rows (if (plusp rows) spacer 0) omitted)))

(defun blank-band-row-p (row)
  "Whether the painted band ROW shows no character, whatever it fills."
  (zerop (length (string-trim " " (plain-frame-row row)))))

(defun band-window-answer-passed-p (window from to start rows)
  "Whether the answer in content rows [FROM, TO) has text, all of it above
the ROWS shown from START, and the next prompt, at TO, shows: an answer
scrolled wholly under its pinned prompt."
  (and (< from (min to start))
       (< to (+ start rows))
       (or (<= to start) (every #'blank-band-row-p (band-window-range window start to)))
       (notevery #'blank-band-row-p (band-window-range window from (min to start)))))

(defun band-window-pinned (window &aux (height window.height)
                                       (top (band-window-offset window)))
  "The pinned prompt's START, ROWS, CONTENT-START and CONTENT-ROWS,
  or NIL when its original block is scrolling inline."
  ;; A prompt has one representation. Upward intent within a truncated
  ;; prompt exposes its original block instead of retaining its summary.
  ;; At bottom, or past the block, the pin excludes the entire inline owner.
  ;; A prompt whose answer has all gone under it lets go. Without this,
  ;; a prompt pinned over the answer's closing blank rows and the next
  ;; prompt would read as a question never answered.
  (when (plusp height)
    (let* ((spans (band-window-prompt-spans window))
           (bottom (band-window-at-bottom-p window))
           (span (find-if (lambda (candidate) (<= (car candidate) top)) spans :from-end t)))
      (when span
        (multiple-value-bind (start pinned spacer omitted) (band-window-prompt-header window span)
          (when (and (plusp pinned)
                     (not (and (plusp omitted) (not bottom) (< top (cdr span)))))
            (let* (;; The pin already budgets its canvas row, so content
                   ;; starts past the row table's own.
                   (after-prompt (band-window-after-prompt window span))
                   (content-start (min window.count (max after-prompt (+ top pinned))))
                   (content-rows (min (- height pinned spacer) (- window.count content-start)))
                   (answer-end (car (second (member span spans)))))
              (unless (and answer-end
                           (band-window-answer-passed-p window after-prompt answer-end content-start content-rows))
                (values start pinned content-start content-rows)))))))))

(defun band-window-top-padding (window)
  "Leading canvas slack: a click anchor, else promptless short-band alignment."
  ;; Negative TOP encodes an explicit inset as -1 - rows, including zero.
  ;; A collapse at a pinned title can therefore stay top-aligned too.
  (min window.height
       (cond ((minusp window.top) (- -1 window.top))
             ((band-window-prompt-spans window) 0)
             (t (- window.height (min window.count window.height))))))

(defun band-window-range (window start end &aux (base window.base-count)
                                                (head (min end base))
                                                (tail-start (max 0 (- start base))))
  "Content rows [START, END) of WINDOW, across the projection/chrome join: the
tail rows count from BASE-COUNT."
  ;; A range lies in the projection, in the chrome, or across the join — a
  ;; band whose content is all chrome (a shell with no transcript yet) asks
  ;; for the last two.
  (cond ((<= end base) (subseq window.lines start head))
        ((>= start base) (subseq window.tail tail-start (- end base)))
        (t (append (subseq window.lines start head) (subseq window.tail tail-start (- end base))))))

(defun band-window-pieces (window)
  "WINDOW's paint and pointer/copy runs, with prompt and disclosure headers."
  ;; Synthetic omission/spacer rows own no source index. The omission lives
  ;; in this shared piece lane, never in the durable message or projection.
  (bind (((start pinned content-start) (band-window-pinned window))
         (prompt (when pinned
                   (find-if (lambda (span) (<= (car span) start (1- (cdr span))))
                            (band-window-prompt-spans window))))
         ((_ _ spacer omitted)
          (if prompt (band-window-prompt-header window prompt) (values 0 0 0 0)))
         (head (if (plusp omitted) (max 0 (- pinned 2)) (or pinned 0)))
         (prefix (when pinned
                   (append (when (plusp head) (list (list start head)))
                           (when (plusp omitted)
                             (append (list (list :omitted omitted (car prompt)))
                                     (when (>= pinned 2)
                                       (list (list (- (cdr prompt) 2) 1)))))
                           (when (> spacer 1)
                             (list (list (1- (cdr prompt)) 1)))
                           (when (plusp spacer) (list :spacer)))))
         (top (if pinned content-start (band-window-offset window)))
         (room (- window.height (band-window-top-padding window)
                  (if pinned (+ pinned spacer) 0)))
         (span (find-if (lambda (span)
                          (and (<= (+ (first span) (third span)) top) (< top (second span))))
                        (band-disclosure-spans window.lines)))
         (title-rows (if span (min room (third span)) 0)))
    (when (plusp title-rows)
      (setf prefix (append prefix (list (list (first span) title-rows)))
            top (min window.count (+ top title-rows)) room (- room title-rows)))
    (append prefix
            (when (and (plusp room) (< top window.count))
              (list (list top (min room (- window.count top))))))))

(defun band-window-visible (window &optional (width 80) hover-index)
  "Visible rows at WIDTH; omitted prompt lines retain the prompt's fill."
  (loop for piece in (band-window-pieces window)
        if (eq piece :spacer) collect ""
        else if (eq (first piece) :omitted)
          append (let* ((hover (eql hover-index (third piece)))
                        (line (make-lline :tone (if hover :user-block-hover :muted) :dim (not hover)
                                          :fill (if hover :user-block-hover :user-block))))
                   (setf (lline-text line)
                         (clip-text-to-width (format nil "[+~d lines omitted]" (second piece))
                                             (nth-value 0 (lline-widths line width))))
                   (nth-value 0 (lline-render line width)))
        else append (destructuring-bind (start count) piece
                      (band-window-range window start (+ start count)))))

(defun band-window-row-index (window row &optional owner-p)
  "Source index at visible ROW; OWNER-P gives synthetic rows' hover owner."
  ;; Never map the omission marker to hidden text, links or image placement.
  ;; Copy still reads the frame's actual painted plain lane.
  (when (minusp row) (return-from band-window-row-index nil))
  (dolist (piece (band-window-pieces window))
    (if (or (eq piece :spacer) (eq (first piece) :omitted))
        (if (zerop row)
            (return-from band-window-row-index
              (and owner-p (not (eq piece :spacer)) (third piece)))
            (decf row))
        (destructuring-bind (start count) piece
          (if (< row count) (return-from band-window-row-index (+ start row)) (incf row (- count)))))))

(defun composer-status-lines (model width &aux (status (active-status model))
                                               (empty (zerop (length (composer-draft model)))))
  "=> (values LINES UNDER STATUS): the composer status's lines as the box
shows them at WIDTH text columns, and whether they stand under a draft --
every status over an empty composer, under a draft only a held one, since
news hides under what is typed while a failure or an answer stays in sight
-- or NIL with none to show."
  (when (and status (or empty (composer-status-held status)))
    (values (status-text-lines (composer-status-text status) width) (not empty) status)))

(defun composer-visible-rows (model &aux (height (chat-model-term-height model)))
  "Text rows the composer box currently shows: its text's, and the composer
status's under it or in its place (COMPOSER-STATUS-LINES), to the growth cap."
  (if (shell-footer-p height)
      (let ((text (max 1 (editor-height (chat-model-editor model))))
            (cap (composer-max-rows height)))
        (multiple-value-bind (lines under)
            (composer-status-lines model (composer-text-width (chat-model-term-width model)))
          (cond ((null lines) text)
                (under (+ text (min (length lines) (max 1 (- cap text)))))
                (t (max 1 (min cap (length lines)))))))
      1))

(nlk:define-side-table status-blocks (:weakness :key)
  "Model -> (painted plain refs), the live block's three lanes at the band's tail.")

(defun sync-viewport-geometry (model &aux (height (chat-model-term-height model))
                                          (rows (composer-visible-rows model)))
  "Fit the transcript viewport under the composer's current height and under
an open dialog's docked panel."
  ;; The composer grows and shrinks as the operator edits and the panel docks
  ;; over the composer; the transcript band yields the rows (the Zig ChatShell
  ;; reflow plus reserveTranscriptAboveDialog).
  (setf (band-window-height (chat-model-band model))
        (max 1 (- height (shell-overhead height rows)
                  (multiple-value-bind (panel above) (docked-dialog-frame-rows model)
                    (if (null panel)
                        0
                        (let ((height (max 4 height)))
                          (max 0 (- (max 1 (- height (shell-overhead height rows)))
                                    (nth-value 2 (docked-dialog-above-bands above)))))))
                  ;; The dropdown panel's rows below the composer (the
                  ;; Zig composer lift: the band yields them so the
                  ;; frame keeps its height).
                  (or (composer-dropdown-geometry model) 0)))))

(defun initialize-chat-model-ui (model)
  "Initialize terminal widgets without choosing the model's state owner."
  ;; Read real terminal dimensions so the alt-screen layout fills the window.
  (multiple-value-bind (term-w term-h) (nlt:terminal-size)
    (setf (chat-model-term-width model) term-w (chat-model-term-height model) term-h)
    ;; The composer is the Zig box: text rows inside a filled rectangle,
    ;; no prompt glyph, the "Ready" placeholder (render_plan.zig
    ;; composer_ready_placeholder). The box grows with its content to the
    ;; pi-mono cap and then scrolls internally. The editor owns editing and
    ;; scrolling; COMPOSER-ROWS owns the paint; no painted caret — the
    ;; shell parks the terminal's hardware cursor at the caret cell (the
    ;; Zig composer cursor contract, which COMPOSE-FRAME keeps).
    ;;
    ;; The editor wraps at the composer's own width, unlike the transcript
    ;; viewport below: it holds live, unstyled text whose caret must map to
    ;; a visual cell, so the engine owns the break — through
    ;; BREAK-FRAGMENTS, the one line-break authority — word-aware like the
    ;; transcript's, over the raw text the caret maps into.
    (setf (chat-model-editor model)
          (make-editor :width (composer-text-width term-w)
                       :max-height (composer-max-rows term-h)
                       :placeholder "Ready"
                       :char-limit 100000))
    ;; The band window holds rows the display policy has already wrapped
    ;; through BREAK-FRAGMENTS, the one line-break authority, at the
    ;; transcript text width: inset 2 from the left, 2 kept at the right.
    (setf (chat-model-band model)
          (make-band-window :height (max 1 (- term-h (shell-overhead term-h))))))
  (render-state model))

;;; --- frame clock ---------------------------------------------------------
;;; The one animation pacing authority (the Zig frame loop, which renders
;;; every animating frame regardless of stream traffic). Pacing must NOT
;;; ride the program's worker: it is one thread (the single-writer
;;; discipline) and a local turn occupies it for its whole duration, so a
;;; tick queued as a command would wait behind the turn and the spinner
;;; would freeze until the turn ends. This thread sends tick messages
;;; straight into the program's mailbox — the same NLT:SEND wake path the
;;; stream delta-fn uses — so frames keep flowing while the worker is busy.

;;; The terminal core owns the one frame period. The animation clock and the
;;; physical event-loop paint share it so animation boundaries do not outrun
;;; the renderer or create a second pacing authority.


(defun live-chrome-animating-p (model &key (dialog-ticks t))
  "Whether chrome needs frames between input and stream events: a running
turn (spinner, elapsed clock, waiting sway — the Zig
transcriptStatusAnimating predicate), live background rows, or a transient
hint still on screen (its five-second decay only lands if something
repaints after it expires)."
  ;; With DIALOG-TICKS, an open dialog carrying a tick handler counts too:
  ;; that is the frame clock's question (keep ticking, the dialog's clocks
  ;; pace themselves). The tick's own render decision asks without it, because
  ;; the handler answers for the dialog whether the tick moved anything, and a
  ;; status frame per tick over a still dialog was the shell's whole idle
  ;; cost.
  (or (chat-model-turn-in-flight model)
      (state-background-rows (chat-model-state model))
      ;; news in the composer status goes after five seconds, and a docked
      ;; panel's row gives a held one back to its keys after as many
      (let ((status (chat-model-composer-status model)))
        (and status (or (not (composer-status-held status))
                        (< (since-ms (composer-status-born status)) 5500))))
      ;; A dialog whose rows carry clocks asks for ticks by having a
      ;; handler for them.
      (and dialog-ticks (picker-dialog-p (chat-model-dialog model))
           (picker-dialog-on-tick (chat-model-dialog model)) t)
      ;; The turn the bar shows in flight, a previewed one's included: its
      ;; mark and clock move.
      (and (nth-value 1 (bar-lane model)) t)))

(nlt:defmessage command-catalog-msg
    ((commands :initarg :commands :reader command-catalog-msg-commands))
  :documentation "The cell commands the gateway serves, read off the loop
thread: INIT queues the read as a command so the first frame never waits on
it — it is an HTTP round trip — and UPDATE installs what came back.")

(defun read-command-catalog-cmd (model)
  "A command that reads the cell commands MODEL's gateway serves and answers
COMMAND-CATALOG-MSG, or nothing when the read fails: the commands in hand
stay (builtins keep working, and the next connection or COMMANDS_CHANGED
reads again)."
  (lambda ()
    (with-handlers ((error () nil))
      (make-instance 'command-catalog-msg :commands (gateway-command-entries model)))))

(defmethod nlt:update ((model chat-model) (msg command-catalog-msg))
  (set-gateway-commands (command-catalog-msg-commands msg))
  (values model nil))

;;; --- the composer's @ catalog -----------------------------------------------
;;; A filesystem walk over up to 512 workspace paths costs more than a frame.
;;; Without the cache, the first `@' would paint its rows late while later
;;; presses land inside the frame. The walk is taken off the trigger's
;;; keystroke: it runs on a thread of its own. INIT starts it with the first
;;; frame, a trigger that finds the catalog unloaded asks for it, and what it
;;; answers waits in *COMPOSER-CATALOG-RESULTS* for the loop's tick branch, so
;;; the loop stays the model's one writer and the tick is what paints the rows.
;;; The answer is kept in *COMPOSER-CATALOG-CACHE-PATH*, read back on the boot
;;; thread before the first frame. A shell's boot is loaded work (a fresh
;;; `--ephemeral' launch boots an organism, its cells and its channels beside
;;; the shell), so the walk is not reliably done before the operator's first
;;; `@'. The cache is what makes that press the same frame as the second one;
;;; the walk that follows replaces it within the second.

(define-startup-parameter *composer-catalog-cache-path*
  (home "composer-catalogs.sexp")
  "Where the composer catalogs the last walk answered are kept, so a process
paints its first `@' from what the last one learned instead of paying the walk
under the boot's own load. NIL disables the cache; tests bind.")

(defparameter *composer-catalog-cache-workspaces* 4
  "Workspaces the catalog cache keeps: the current one first, then the most
recent others. The catalogs depend on the directory the shell runs in.")

(defun composer-catalog-cache-entries (path)
  "PATH's entries as an alist (WORKSPACE-STRING . RESULT-PLIST), newest first,
or NIL."
  ;; Anything unreadable — another shape, a half write, no file — reads as no
  ;; cache at all, and the walk fills the lanes as it always did. The read is
  ;; UIOP's safe one (standard syntax, *READ-EVAL* off), in CL-USER, not
  ;; KEYWORD: the file carries T and NIL too, and the keyword package reads
  ;; those as :T and :NIL.
  (when path
    (let ((entries (ignore-errors (uiop:safe-read-file-form path :package :cl-user))))
      (and (listp entries) entries))))

(defun read-composer-catalog-cache (workspace)
  "The walk's result for WORKSPACE as the plist it answers with, or NIL."
  (cdr (assoc (namestring workspace)
              (composer-catalog-cache-entries *composer-catalog-cache-path*)
              :test #'equal)))

(defun write-composer-catalog-cache (workspace result)
  "Remember RESULT — a walk's own plist — for WORKSPACE, newest first and
bounded."
  ;; Written beside itself and renamed over, so a reader never sees a half
  ;; file. Answers the path written, or NIL when the cache is off or the write
  ;; failed: a cache that cannot be written is not a walk that cannot finish.
  (when-let (path *composer-catalog-cache-path*)
    (ignore-errors
     (let* ((key (namestring workspace))
            (kept (remove key (composer-catalog-cache-entries path)
                          :key #'car :test #'equal))
            ;; The failures are a live diagnostic, not an answer.
            (entries (acons key (alexandria:remove-from-plist result :failures) kept)))
       ;; Readably, an SBCL base-string prints as #A((29) BASE-CHAR . "...")
       ;; — twice the bytes and a syntax only this implementation reads.
       (nlk:write-file-atomically
        path (write-to-string (subseq entries 0 (min (length entries) *composer-catalog-cache-workspaces*))
                              :escape t :length nil :level nil :circle nil :pretty nil :readably nil))))))

(defun install-composer-catalogs (model result)
  "Install a walk's RESULT — its plist — into MODEL: an answer takes its list
and its LOADED flag (an empty answer is an answer), a failed walk keeps the
flag NIL so the next trigger retries it."
  ;; Answers whether anything moved.
  (when (getf result :files-loaded)
    (setf (chat-model-file-catalog model) (getf result :files)
          (chat-model-file-catalog-loaded model) t)))

;;; A trigger may ask on any keystroke; the walk still runs one at a time per
;;; model.
(defvar *composer-catalog-walks* (make-hash-table :test #'eq)
  "The models whose composer-catalog walk is in flight.")

(defvar *composer-catalog-results* (make-hash-table :test #'eq)
  "The walks that finished and have not landed yet: MODEL -> a plist of
:FILES :FILES-LOADED :FAILURES, written by the walk
thread and drained by the loop's tick branch.")

(defvar *composer-catalog-walks-lock*
  (bt2:make-lock :name "composer-catalog-walks")
  "One lock for both tables: they are read and written together.")

(defun read-composer-catalogs (workspace)
  "WORKSPACE's composer catalog, as the plist the tables carry."
  ;; A walk that signals is carried as text, never swallowed: the old
  ;; (ignore-errors ...) left the lane silently empty for the rest of the
  ;; process, which is what a dropdown nothing can explain looks like.
  (handler-case (list :files (workspace-file-catalog workspace) :files-loaded t)
    (error (e)
      (list :files-loaded nil
            :failures (list (format nil "file catalog unavailable — ~a" e))))))

;;; With the cached catalogs already in the model, nothing the operator sees
;;; waits on the walk, and its allocation no longer competes with the
;;; keystrokes that follow the frame (a first press measured 17-60 ms
;;; on this machine, the walk running beside it). A trigger's request is not
;;; delayed: it is asked for because nothing is in hand.
(defparameter *composer-catalog-walk-delay* 1/2
  "Seconds a boot's own refresh walk waits past the frame its attach is for
before it starts — the warm-spawn rule, one process over: the folder's builder
lands behind the first paint, never inside it.")

(defun start-composer-catalog-walk (model &key delay &aux (program (chat-model-program model)))
  "Walk MODEL's @ catalog on a thread of its own if none is already in flight,
and tick the loop when it lands — DELAY seconds from now, so a boot's refresh
lands behind the first frame."
  ;; NIL without a program: no loop means no frame to paint. The keystroke
  ;; path only ever asks here — it never waits on the filesystem.
  (when (and program
             (bt2:with-lock-held (*composer-catalog-walks-lock*)
               (unless (gethash model *composer-catalog-walks*)
                 (setf (gethash model *composer-catalog-walks*) t))))
    (let ((workspace (uiop:getcwd)))
      (spawn "nodecode-composer-catalogs"
        (when (and delay (plusp delay)) (sleep delay))
        (with-cleanup ((bt2:with-lock-held (*composer-catalog-walks-lock*)
                         (remhash model *composer-catalog-walks*)))
          (let ((result (read-composer-catalogs workspace)))
            ;; The cache is what the next process paints its first `@'
            ;; from; written here, on the walk's own thread.
            (write-composer-catalog-cache workspace result)
            (bt2:with-lock-held (*composer-catalog-walks-lock*)
              (setf (gethash model *composer-catalog-results*) result))
            (nlt:send program (nlt:make-tick-msg))))))
    t))

(defun refresh-composer-catalogs-behind-the-frame (model)
  "Start the catalog refresh MODEL's boot put off, now that the frame its
attach was for is on screen."
  ;; A fixed half-second delay would still put the catalog walk, with its
  ;; SKILL.md reads and workspace listing, inside a resumed session's replay.
  (when (shiftf (chat-model-catalog-refresh-due model) nil)
    (start-composer-catalog-walk model :delay *composer-catalog-walk-delay*)))

(defun land-composer-catalog-walks (model)
  "Install the composer catalogs a walk landed since the last frame, on the
loop thread, and put each failure up as an error toast (INSTALL-COMPOSER-
CATALOGS owns what an answered lane and a failed one each take)."
  (when-let (result (bt2:with-lock-held (*composer-catalog-walks-lock*)
                      (prog1 (gethash model *composer-catalog-results*)
                        (remhash model *composer-catalog-results*))))
    (install-composer-catalogs model result)
    (dolist (failure (getf result :failures))
      (toast model failure :error-p t))
    t))

;;; A streaming turn's transcript is projected on this tick, so this is how
;;; often new text reaches the screen. 1/60 is three ticks per 50 ms, so the
;;; spinner's 150 ms step and the elapsed clock's 100 ms digit flip on a tick.
;;; Frames the operator asks for -- the pointer, a key, the wheel -- paint at
;;; the loop's own ceiling (NLT:*FRAME-PERIOD-SECONDS*), which is faster.
(defparameter *frame-clock-period-seconds* 1/60
  "How often the frame clock posts ticks; the loop suppresses idle paint.")

(defun run-frame-clock (program)
  "Post paced ticks until PROGRAM stops; the loop owns every UI read."
  ;; A live source fold can temporarily replace a CLOS accessor's methods.
  ;; Reading the model here killed the clock during that window; streaming
  ;; then only painted when input arrived. The clock knows only its program
  ;; and time. UPDATE owns animation, dirty projection, expiry and QUIET, so
  ;; idle ticks still owe no compose or physical paint.
  (loop while (nlt:program-running program)
        do (let* ((period (max 1 (round (* *frame-clock-period-seconds*
                                           internal-time-units-per-second))))
                  (now (get-internal-real-time)))
             ;; Stay on the monotonic grid without ratios or sleep drift.
             (sleep (/ (- period (mod now period)) (float internal-time-units-per-second 1d0)))
             (nlt:send program (nlt:make-tick-msg)))))

(defmethod nlt:init ((model chat-model))
  "Initialize local widgets over the empty state the gateway's replay fills."
  ;; Commands
  ;; ride along, for the worker: the kitty graphics query — a terminal that
  ;; answers flips the model into inline-image mode (KITTY-GRAPHICS-MSG below);
  ;; silence keeps the fallback — the truecolor probe when the environment
  ;; guessed a quantized profile — a terminal that confirms upgrades the paint
  ;; (TRUECOLOR-MSG below); silence keeps the guess — and the read of the
  ;; gateway's cell commands, so the dropdown, ghost text and /help know them
  ;; without the first frame waiting on the read.
  ;; The frame clock starts here — INIT runs on the live program's update loop,
  ;; after NLT:RUN has marked the program running.
  (initialize-chat-model-ui model)
  (setf (chat-model-state model) (initial-chat-state))
  (render-state model)
  ;; What the last walk answered for this workspace paints the first `@': read
  ;; here, on the boot thread, so no frame can race it. A model with no program
  ;; takes it too — the rows are in hand wherever a frame paints — and the walk
  ;; that refreshes them starts with the frame, below.
  (install-composer-catalogs model
                             (read-composer-catalog-cache (uiop:getcwd)))
  ;; The frame clock; without a program handle (tests drive INIT) no thread starts.
  (when-let (program (chat-model-program model))
    ;; The launch glint's clock starts with the frame clock that carries it.
    (play-mark-glow model :shine :launch t)
    (spawn "nodecode-frame-clock" (run-frame-clock program))
    ;; With the catalog in hand the refresh waits for the frame the attach
    ;; is for; without it, the walk is all that stands between the operator
    ;; and an empty dropdown, so it starts now.
    (if (chat-model-file-catalog-loaded model)
        (setf (chat-model-catalog-refresh-due model) t)
        (start-composer-catalog-walk model))
    ;; The /models catalog — a 4.7 MB models.dev parse folded with every
    ;; fetched listing — is built on a thread of its own, so the first press
    ;; of /models paints from the memo instead of paying the build. The
    ;; answer is a memo, not UI state: nothing lands it on a frame.
    (start-models-picker-warm model))
  (append (list (nlt:terminal-io-cmd #'nlt:request-kitty-graphics)
                (nlt:terminal-io-cmd #'nlt:request-background))
          (when (color-profile-upgradable-p)
            (list (nlt:terminal-io-cmd #'nlt:request-truecolor)))
          (list (read-command-catalog-cmd model))))

