;;;; frame.lisp --- the update method, frame composition, the frame guard. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- update method ------------------------------------------------------

(defmethod update :around ((model chat-model) msg &aux (nlk:*attribution-label* (type-of msg)))
  "Attribution seam: every message pass — reduce plus any paint, local and
gateway shells both, since GATEWAY-CHAT-MODEL subclasses CHAT-MODEL — runs as
one attributed span, so /gc can split collections that stalled visible work
from the ones COLLECT-DURING-WAIT hid inside provider wait."
  ;; The label is the message's type, free of consing (TYPE-OF returns the
  ;; struct name); the gateway shell refines the binding to the sync payload
  ;; kind once it has decoded the frame, so a worst span reads 'turn_event',
  ;; not just 'gw-projection-msg'.
  (nlk:with-gc-attribution (call-next-method)))

(defmethod update ((model chat-model) msg)
  "Handle messages and update the model."
  ;; The editor sees only what nothing here claimed. Every branch below
  ;; consumes the message it acts on by saying nothing at all; the two that
  ;; deliberately hand one on — a key no branch names, and a submit with
  ;; nothing to submit — say so, once each.
  (let ((pass-to-editor nil)
        ;; Whether the fold changed nothing a frame would show (the tick
        ;; branch and pointer motion say so): UPDATE's third value.
        (quiet nil))

    ;; A key or pointer event is the operator working: the idle collector's
    ;; stop-the-world pause waits out *IDLE-INTERVAL* from it (gc.lisp).
    (when (or (nlt:key-press-msg-p msg) (nlt:paste-msg-p msg) (typep msg 'nlt::mouse-event))
      (nlk:note-operator-input))

    (cond
      ;; Key press - handle before passing to the editor
      ((nlt:key-press-msg-p msg)
       ;; A keybind row defined into this image since the model was built
       ;; (or a cell's override) reaches the keyboard here.
       (refresh-model-keymap model)
       (let* ((key (nlt:key-event-code msg))
              (mod (nlt:key-event-mod msg))
              (ctrl (nlt:mod-contains mod nlt:+mod-ctrl+))
              (alt (nlt:mod-contains mod nlt:+mod-alt+))
              (shift (nlt:mod-contains mod nlt:+mod-shift+))
              (dialog (chat-model-dialog model)))
         (cond
           ;; A keyboard selection over the transcript owns the keys while it
           ;; runs (TRANSCRIPT-SELECT-KEY), ahead of every chord below: its
           ;; Ctrl+D pages, its Enter copies, its letters move the cursor, and
           ;; the quit chord it declines falls through to quit. An open dialog
           ;; keeps its own keys; the mode waits behind it.
           ((and (transcript-select model) (null dialog)
                 (transcript-select-key model key mod)))
           ;; Quit dispatches through the keymap (defaults Ctrl+Q/Ctrl+D).
           ;; Ctrl+C clears a non-empty composer first (the Zig keybind
           ;; split), then stops a running turn the way Esc does
           ;; (INTERRUPT-TURN), and quits only when there is neither --
           ;; Codex's order. Without that order, Ctrl+C would quit a
           ;; running turn instead of stopping it. Read ahead of the dialog
           ;; ladder, so a picker open over a running turn stops it too.
           ;;
           ;; Ctrl+D keeps the unix EOF rule every shell and readline
           ;; program shares: it exits only on an empty composer. With text
           ;; in the box it stays the readline delete-at-cursor (the Zig
           ;; isDeleteAtCursorKey chord), which the editor below applies.
           ((and (model-key-matches-p model :quit key mod)
                 (not (and ctrl (characterp key) (char-equal key #\d)
                           (not (composer-empty-p model)))))
            (return-from update (values model (nlt:quit-cmd))))
           ((and ctrl (characterp key) (char= key #\c))
            (cond ((plusp (length (editor-value (chat-model-editor model))))
                   (editor-reset (chat-model-editor model)))
                  ((interrupt-turn model))
                  (t (return-from update (values model (nlt:quit-cmd))))))

           ;; Ctrl+F over an open dialog flips its panel between the Zig
           ;; docked height and every row the window can give it. It is
           ;; read ahead of the dialog's own ladder so every dialog — the
           ;; wizard's text steps included — answers it the same way; with
           ;; no dialog open the chord stays the editor's. The frame reads
           ;; the flag on its next compose, so nothing here repaints; the
           ;; flag outlives the dialog, so the next one opens at the same
           ;; size.
           ((and dialog (model-key-matches-p model :toggle-dialog-fullscreen key mod))
            (setf (chat-model-dialog-fullscreen model)
                  (not (chat-model-dialog-fullscreen model))))

           ;; Ctrl+R on the history panel and Ctrl+S on the sessions picker
           ;; are the toggle back: the chord that opened the panel takes it
           ;; away again, the way Esc closes it — one gesture, never a reach
           ;; for a second key. It closes outright rather than clearing the
           ;; query first the way Esc does: a toggle toggles. Gated on the
           ;; panel's own kind, so a chord never dismisses a dialog that is
           ;; not its own; read ahead of the modal ladder, so the panel never
           ;; swallows the chord that would dismiss it (the sessions picker's
           ;; query box would otherwise take the #\s).
           ((and dialog
                 (model-key-matches-p model (case (picker-dialog-kind dialog)
                                              (:prompt-history :history-dialog)
                                              (:sessions :sessions-dialog))
                                      key mod))
            (dismiss-dialog model (picker-dialog-on-cancel dialog)))

           ;; The image-paste chord over a dialog pastes the clipboard's text
           ;; into its field: a field holds no image, and Ctrl+V is what an
           ;; operator reaches for with a key to paste. Where nothing reads
           ;; a clipboard, the dialog's subtitle — the instruction row the
           ;; operator is reading — names the paste that does reach this
           ;; process: the terminal's own. Not the composer status: a docked
           ;; panel stands over the composer, and the subtitle is the line
           ;; under the field; over ssh it never showed.
           ((and dialog (model-key-matches-p model :paste-image key mod))
            (if-let (text (command-output *clipboard-paste-commands*))
              (picker-dialog-paste model text)
              (setf (picker-dialog-subtitle dialog)
                    "No clipboard to read here — paste with your terminal (Ctrl+Shift+V)")))

           ;; A picker nobody opened -- the reasoning effort /models offers
           ;; once a model lands -- is not where typing goes: a printable key
           ;; closes it, keeping what stands, and reaches the composer as if
           ;; it had never opened. Without this, a `yo' typed after the pick
           ;; vanished and its Enter picked a rung (Mac take, 2026-10-21).
           ((and dialog (picker-type-through dialog)
                 (characterp key) (graphic-char-p key) (not ctrl) (not alt))
            (dismiss-dialog model (picker-dialog-on-cancel dialog))
            (unless (chat-model-dialog model)
              (return-from update (update model msg))))

           ;; An open modal picker owns every remaining key.
           (dialog (picker-dialog-key model key :ctrl ctrl :alt alt :shift shift :mod mod))

           ;; Ctrl+Y opens the keyboard selection over the transcript
           ;; (BEGIN-TRANSCRIPT-SELECT); the mode's own ladder above closes it.
           ((model-key-matches-p model :select-transcript key mod)
            (begin-transcript-select model))

           ;; Switch Developer / Standard viewport (Ctrl+O).
           ((model-key-matches-p model :toggle-viewport key mod)
            (toggle-tool-detail model)
            (render-state model))

           ;; `?` on an empty composer opens the keyboard help dialog (the
           ;; Zig handleHelpDialogKey composer-empty gate); with text it
           ;; stays a printable character.
           ((and (model-key-matches-p model :help key mod)
                 (composer-empty-p model))
            (open-help-dialog model))

           ;; Ctrl+R browses every prompt this shell submitted — the whole
           ;; recall ring in one searchable picker, where Up/Down walk it an
           ;; entry at a time. Deliberately no empty-composer gate: recalling
           ;; a past prompt is what to do when the box already holds a draft.
           ((model-key-matches-p model :history-dialog key mod)
            (open-prompt-history-dialog model))

           ;; Ctrl+S opens the sessions picker — the Zig
           ;; open_session_launcher chord (keySpec('s', .{ .ctrl = true })),
           ;; whose lisp surface is /sessions, so both reach the one op. No
           ;; empty-composer gate: switching session is what to do with a
           ;; draft in the box. A shell that cannot reach a gateway answers
           ;; the op's refusal as a notice, which lands in the composer status
           ;; exactly as the slash command's does (SUBMIT-SLASH's contract);
           ;; on the gateway shell the directory read runs on its own thread,
           ;; so no frame waits for it.
           ((model-key-matches-p model :sessions-dialog key mod)
            (when-let (notice (slash-sessions-op model))
              (show-status model notice)
              (render-state model)))

           ;; Ctrl+P on an empty composer opens the command palette: it
           ;; inserts "/" and the slash dropdown appears from the composer
           ;; text — the palette IS the slash dropdown under another
           ;; binding (Zig openComposerCommandPalette). With text the key
           ;; is previous-suggestion navigation while a dropdown is open
           ;; (Zig isPreviousSuggestionKey) and otherwise a consumed no-op
           ;; — never a raw #\p insert. The dialog branch above already
           ;; suppresses it while any dialog is open.
           ((model-key-matches-p model :open-command-palette key mod)
            (cond
              ((composer-empty-p model)
               (editor-insert
                (chat-model-editor model) "/"))
              ((composer-suggestions-for model)
               (composer-dropdown-key model :up))))

           ;; Image paste (default Ctrl+V / Ctrl+Alt+V): probe the system
           ;; clipboard for an image and stage it as a pending attachment.
           ((model-key-matches-p model :paste-image key mod)
            (paste-clipboard-image model))

           ;; Copy the marked text again (default Ctrl+Shift+C) — anywhere on
           ;; the frame, since the mark is a screen region.
           ((model-key-matches-p model :copy-selection key mod)
            (copy-frame-selection model))

           ;; Newline through the keymap (defaults Shift+Enter, Alt+Enter,
           ;; Ctrl+J). Plain Enter stays submit.
           ((model-key-matches-p model :insert-newline key mod)
            (editor-insert
             (chat-model-editor model) (string #\Newline)))

           ;; Dropdown navigation while any suggestion lane matches: the
           ;; slash lane, or the @file token lane. Accept-suggestion
           ;; (default Tab) completes the highlighted row. Ctrl+N walks the
           ;; list forward (the Zig isNextSuggestionKey chord); its backward
           ;; twin Ctrl+P is the command-palette branch above, which routes
           ;; to the same dropdown once one is open.
           ((and (composer-suggestions-for model)
                 (let ((dropdown-key
                         (cond ((model-key-matches-p model :history-previous key mod) :up)
                               ((or (model-key-matches-p model :history-next key mod)
                                    (and ctrl (characterp key) (char-equal key #\n)))
                                :down)
                               ((model-key-matches-p model :accept-suggestion key mod) :tab))))
                   (and dropdown-key (composer-dropdown-key model dropdown-key)))))

           ;; Prompt history recall: Up at the end of the text walks older
           ;; prompts, Down walks back toward the stashed draft. Each helper
           ;; declines — falling through to cursor movement — when it does
           ;; not apply.
           ;; While this composer holds a peeled prompt (the Up peel), both
           ;; history keys stay cursor keys: a walk would replace the text
           ;; the peels put there for editing.
           ((and (null (held-prompt (chat-model-state model)))
                 (model-key-matches-p model :history-previous key mod)
                 (composer-history-previous model)))
           ((and (null (held-prompt (chat-model-state model)))
                 (model-key-matches-p model :history-next key mod)
                 (composer-history-next model)))

           ;; Transcript paging. These belong to the chat window, not the
           ;; composer — the composer scrolls internally with the cursor.
           ((when-let (action (cond ((model-key-matches-p model :page-up key mod) :page-up)
                                    ((model-key-matches-p model :page-down key mod) :page-down)
                                    ;; Home/End jump the transcript window (Zig scroll_home/end)
                                    ;; while the composer is empty; with text they stay editing keys.
                                    ((plusp (length (editor-value (chat-model-editor model)))) nil)
                                    ((model-key-matches-p model :scroll-home key mod) :home)
                                    ((model-key-matches-p model :scroll-end key mod) :end)))
              (scroll-transcript model action)
              t))

           ;; Enter semantics belong to the keymap: the shell's own submit
           ;; arm (client.lisp) claims the submit chord before this update
           ;; runs, and an Enter chord matching neither submit nor
           ;; insert-newline (both rebound) must not reach the editor, whose
           ;; own default would insert a newline behind the operator's
           ;; configuration.
           ((eq key :enter))
           (t (setf pass-to-editor t)))))

      ;; Window resize
      ((nlt:window-size-msg-p msg)
       (let ((width (nlt:window-size-msg-width msg))
             (height (nlt:window-size-msg-height msg)))
         (setf (chat-model-term-width model) width
               (chat-model-term-height model) height
               ;; The band's rows are re-wrapped by the display policy at the new
               ;; width on the RENDER-STATE below; the composer takes its own
               ;; box-derived width and re-wraps its live text there.
               (editor-width (chat-model-editor model)) (composer-text-width width)
               ;; The composer growth cap tracks the window; the viewport takes
               ;; whatever the composer's current height leaves.
               (editor-max-height (chat-model-editor model)) (composer-max-rows height))
         (sync-viewport-geometry model)

         (render-state model)))

      ;; Bracketed paste arrives as one message, and it lands where typing
      ;; would: an open dialog's field when it has one — the API key the
      ;; setup wizard asks to have pasted — else the composer, through the
      ;; collapse rule ([Paste #N] past the Zig bounds).
      ;; An EMPTY paste is the terminal's own paste key over a clipboard that
      ;; holds no text: Windows Terminal keeps Ctrl+V for itself and, with only
      ;; an image copied, sends ESC [ 200 ~ ESC [ 201 ~ and nothing between --
      ;; so it asks for the image, as the paste-image key does. A paste that is
      ;; only an image file's path -- a snip, an image copied in Explorer, a
      ;; file dragged in -- stages that file.
      ((nlt:paste-msg-p msg)
       (cond ((chat-model-dialog model)
              (picker-dialog-paste model (nlt:paste-msg-text msg)))
             (t (let ((text (nlt:paste-msg-text msg)))
                  (if (zerop (length text))
                      (paste-clipboard-image model)
                      (multiple-value-bind (media-type base64) (pasted-image-file text)
                        (if media-type
                            (stage-composer-image model media-type base64)
                            (attach-composer-paste model text)))))
                (reset-history-navigation model)
                (setf (chat-model-slash-selection model) 0))))

      ;; The terminal answered the kitty graphics query: image cards gain
      ;; their inline preview band from the next render on.
      ((nlt:kitty-graphics-msg-p msg)
       (setf (chat-model-kitty-graphics model)
             (and (nlt:kitty-graphics-msg-ok-p msg) t))
       (render-state model))

      ;; The terminal answered the truecolor probe: its SGR honors 24-bit
      ;; color whatever the environment claimed. Upgrade and repaint — the
      ;; row caches key on the profile, so the quantized paint settles out.
      ;; A no never downgrades: the environment's guess stands.
      ((nlt:truecolor-msg-p msg)
       (when (and (nlt:truecolor-msg-ok-p msg) (color-profile-upgradable-p))
         (setf *color-profile* :truecolor)
         (render-state model)))

      ;; The terminal said what its background is: a theme that leaves the
      ;; canvas to it solves an answer's colours on that (ANSWER-TONES).
      ((nlt:background-msg-p msg)
       (unless (equal *terminal-canvas* (nlt:background-msg-rgb msg))
         (setf *terminal-canvas* (nlt:background-msg-rgb msg))
         (engage-theme)
         (render-state model)))

      ;; The terminal turned between light and dark (DEC 2031): its
      ;; background moved under a theme that leaves the canvas to it, so ask
      ;; again. Asked only at launch, a shell started in the dark kept dark-
      ;; canvas ink on the light background it woke up to.
      ((nlt:color-scheme-msg-p msg)
       (return-from update (values model (nlt:terminal-io-cmd #'nlt:request-background))))

      ;; Mouse wheel: the overlay under the pointer owns the notch — an open
      ;; dialog's list scrolls with the highlight staying under the cursor, the
      ;; composer dropdown moves its selection — and otherwise the transcript
      ;; takes it wherever the pointer is, or the /term panel while its
      ;; takeover owns the band.
      ((nlt:mouse-wheel-msg-p msg)
       (handle-mouse-wheel model msg))

      ;; Mouse gestures: press arms (scrollbar grab, tool card, selection
      ;; anchor), motion drags (thumb, selection) or hovers, release
      ;; resolves (expand toggle, copy-on-release).
      ((nlt:mouse-click-msg-p msg)
       (handle-mouse-press model msg))
      ((nlt:mouse-motion-msg-p msg)
       (setf quiet (not (handle-mouse-motion model msg))))
      ;; A click on a key's hint answers its key press: the key goes the way
      ;; a typed one goes, every chord and dialog ladder included.
      ((nlt:mouse-release-msg-p msg)
       (let ((key (handle-mouse-release model msg)))
         (when (nlt:key-press-msg-p key)
           (return-from update (update model key)))))

      ;; Tick from the frame clock: state a stream reducer dirtied since
      ;; the last frame takes one full render (the coalesced projection of
      ;; every delta that landed this frame); otherwise, while live chrome
      ;; animates, only the chrome recomposes over the cached projection so
      ;; the spinner, elapsed clock, and waiting sway advance between
      ;; stream frames. Pacing lives in the frame-clock thread
      ;; (NLT:INIT starts it) — a tick command re-armed here would queue on
      ;; the size-1 command pool behind a running local turn and never fire
      ;; mid-turn. The tick must not fall through to the editor: its NIL
      ;; command would clobber any pending TA-CMD.
      ((nlt:tick-msg-p msg)
       ;; A composer-catalog walk that finished since the last frame lands
       ;; here, so the rows the operator is waiting for paint with this one
       ;; (START-COMPOSER-CATALOG-WALK — the walk thread ticks). Deferred
       ;; dialog-highlight delivery rides the tick next, before the render
       ;; decision: a handler that dirties the transcript (the sessions
       ;; preview hydrate) gets this frame's full render; then an open
       ;; dialog's ON-TICK gets the tick and answers whether it moved
       ;; anything. A tick that moved nothing is QUIET: the loop skips the
       ;; compose and its diff. The dialog's clocks keep the ticks coming
       ;; (LIVE-CHROME-ANIMATING-P); without the skip, an idle dialog would
       ;; pay that compose and diff on every tick, while its handler says
       ;; which matter.
       ;; A running turn's ticks are gated the same way: the chrome is
       ;; re-read on each (STATUS-CHROME-MOVED-P) and composed only when a
       ;; clock of its stepped, so a tick inside the spinner's 150 ms frame
       ;; and the elapsed clock's 100 ms digit costs one bar render, not a
       ;; frame compose and its diff.
       (let* ((landed (land-composer-catalog-walks model))
              ;; A drag held still past the band's edge scrolls on the clock.
              (dragged (drag-scroll-step model))
              ;; News whose five seconds ran out leaves on this frame, and a
              ;; countdown strip that moved a step since the last one shows
              ;; it; a tick that moved neither stays quiet.
              (expired (or (expire-toasts model) (toast-strips-moved-p model)))
              (flushed (picker-flush-highlight model))
              (dialog (chat-model-dialog model))
              (ticked (and (picker-dialog-p dialog)
                           dialog.on-tick
                           (funcall dialog.on-tick model dialog)))
              (painted (cond ((or (chat-model-transcript-dirty model)
                                  ;; A place whose old thing has gone out takes
                                  ;; the newcomer where it stood.
                                  (fade-due-p model))
                              (render-state model)
                              t)
                             ((and (live-chrome-animating-p model :dialog-ticks nil)
                                   (status-chrome-moved-p model))
                              (render-status-frame model)
                              t)
                             ((mark-glowing-p model)
                              (render-state model)
                              t)
                             ;; The ember, the fades and the glints are the
                             ;; frame's paint: the rows stand.
                             ((or (ember-moved-p model) (fade-moved-p model) (glint-rows model)) t))))
         (setf quiet (not (or landed dragged expired flushed ticked painted))))))

    ;; Hand the key to the editor. The viewport deliberately gets no
    ;; messages: its own keymap (j/k/space/arrows) would fight the composer
    ;; for every keystroke, so the transcript moves only through the
    ;; explicit paging/wheel branches above. Any real edit ends a history
    ;; walk and re-homes the dropdown selection (the Zig every-text-mutation
    ;; reset); motion alone does not.
    (when (and pass-to-editor
               (editor-handle-key (chat-model-editor model) msg))
      (reset-history-navigation model)
      (setf (chat-model-slash-selection model) 0)
      ;; A command's trigger character opens its dialog over the draft.
      (when-let (entry (composer-trigger-command model msg))
        (when-let (notice (run-gateway-command model entry "" :trigger entry.trigger))
          (show-status model notice))))

    ;; The composer may have grown or shrunk under any of the branches
    ;; above; the transcript band always takes what it leaves.
    (sync-viewport-geometry model)

    ;; Every command a branch answers returns from that branch itself; the
    ;; shared tail answers none, and says whether the fold was quiet.
    (values model nil quiet)))

;;; --- frame composition ------------------------------------------------------
;;; The whole terminal frame, assembled row by row: the renderer
;;; paints any cell the view string does not cover as a reset-background
;;; blank, so canvas parity with the Zig full-frame memset means emitting
;;; exactly TERM-HEIGHT rows, each TERM-WIDTH columns, every one carrying
;;; the canvas background under a painting profile. The stack is the Zig
;;; ChatShell: blank, header, top padding, the transcript band with its
;;; scroll gutter, a gap, the composer, and the footer lane on tall windows.

(defun pad-frame-row (content used width bg &aux (pad (- width used)))
  "CONTENT, whose visible width is USED, padded to WIDTH with canvas cells."
  (if (plusp pad)
      (concatenate 'string content (blank-frame-row pad bg))
      content))

(defun format-token-count (value)
  "Compact token count (Zig FooterStatus.appendTokenCount, extended): plain
below 1000, tenth-precision k to a million, tenth-precision M past it, the
trailing .0 dropped."
  ;; The M tier exists for the header's bare-occupancy lane: a million-token
  ;; prompt spelled `1200k` reads as a count, `1.2M` reads as a window.
  (let ((value (or value 0)))
    (flet ((tenths-of (unit suffix &aux (tenths (floor (+ (* value 10) (floor unit 2)) unit)))
             (format nil "~d~@[.~d~]~a" (floor tenths 10)
                     (and (plusp (mod tenths 10)) (mod tenths 10)) suffix)))
      (cond ((< value 1000) (format nil "~d" value))
            ((< value 999950) (tenths-of 1000 "k"))
            (t (tenths-of 1000000 "M"))))))

(defun format-occupancy-percent (used window)
  "Window occupancy as a tenth-precision percent, half-up."
  ;; Whole percent is too coarse for the header: a 1.5k prompt against a 200k
  ;; window is 0.75%, which floors to a flat `0%` — indistinguishable from an
  ;; empty window — and every prompt below 2k reads the same. The tenth keeps
  ;; the early session moving. Capped at 999.9% so a mis-sized window cannot
  ;; widen the lane.
  (if (or (null window) (zerop window))
      "100.0%"
      (let ((tenths (min 9999 (floor (+ (* used 1000) (floor window 2)) window))))
        (format nil "~d.~d%" (floor tenths 10) (mod tenths 10)))))

(defun header-status-text (model &key (detail (chat-model-tool-detail model)))
  "The header's right lane (Zig chatShellHeaderStatus): what the SESSION has
cost and how full its context is; Standard keeps context and cache hit rate, Developer the bill."
  ;; Both are session-scoped on purpose — this lane is the standing readout
  ;; the operator glances at, so it must not shrink back to a single turn's
  ;; numbers every time a turn starts.
  ;;
  ;;   occupancy  the newest provider round's prompt over the window it was sent
  ;;              to, as `43.8%/200k`. Occupancy is a snapshot, never a sum: an
  ;;              agent loop resends a growing prefix each round, so summed
  ;;              prompts exceed the window many times over. When no source
  ;;              knows the window (no config entry, no catalog row — the
  ;;              capability seam answers NIL) the lane shows the prompt's
  ;;              absolute size instead, `ctx 87.6k`: a percent of nothing is
  ;;              meaningless, but the operator still wants to see the context
  ;;              grow. The `ctx` word is the live bar's pressure-clause label —
  ;;              a bare `87.6k` next to the arrows reads as one more count, the
  ;;              label says which one.
  ;;   ↑ / ↓      every round of every turn, summed — the session's running bill.
  ;;
  ;; The arrows carry the same two notes the live transcript bar and the finish
  ;; divider carry, reciprocated at session scope: `(cache N%)` is the share of
  ;; the session's whole prompt bill that was served from cache
  ;; (FORMAT-CACHE-HIT-PERCENT, the rule the turn meter reads), `(reasoning N)` is
  ;; what the session spent thinking inside its completion bill. Each note joins
  ;; only when the provider actually reported it — a lane that always printed
  ;; `(cache 0%)` would claim a fact no round ever sent.
  ;;
  ;; The window resolves through the capability seam from the model the newest
  ;; round itself named. NIL before any usage fact exists.
  (let ((parts (header-status-parts model :detail detail)))
    (and parts (format nil "~{~a~^ · ~}" (mapcar #'car parts)))))

(defun header-status-segs (model)
  "HEADER-STATUS-TEXT as the header row paints it: each part in its tone,
the gutters muted."
  (loop for ((text . tone) . more) on (header-status-parts model)
        collect (make-seg :text text :tone tone)
        when more collect (make-seg :text " · " :tone :muted)))

(defun header-status-parts (model &key (detail (chat-model-tool-detail model)))
  "The header's right lane as (TEXT . TONE) parts, NIL before any usage
fact: HEADER-STATUS-TEXT's words. The occupancy warms as it nears the
eviction line -- :WARN with the line's size from three quarters of the
window, :DANGER with what the next request does once past it -- and every
other part is muted."
  ;; One number, one place: the lane is the only meter of the window the
  ;; frame keeps standing, so the line it nears is said here rather than
  ;; drawn again beside it.
  (let* ((state (visible-transcript-state model))
         (session (json-value state :any "session_usage"))
         (context (json-value state :any "context_usage")))
    ;; A /sessions preview of a session longer than its page has summed only
    ;; the page: its lane waits for the gateway's numbers for the whole
    ;; session (USAGE_PENDING, PREVIEW-INSTALL-HEADER), never shows the page's.
    (when (and session (not (gethash "usage_pending" state)))
      (destructuring-bind (&key input output cached reasoning cost-known cost &allow-other-keys)
          session
        (let* ((used (context-occupancy-tokens context))
               (window (and (getf context :model)
                            (ignore-errors
                              (model-context-tokens (getf context :model)))))
               (share (and window (plusp window) (/ used window)))
               (past (and share (>= share *context-eviction-trigger-ratio*)))
               (near (and share (>= share 3/4)))
               (tone (cond (past :danger) (near :warn) (t :muted)))
               (hit (and cached (plusp cached) (format-cache-hit-percent input cached))))
          (remove nil
                  (list*
                   (when (plusp used)
                     (cons (if window
                               (format nil "~a/~a"
                                       (format-occupancy-percent used window)
                                       (format-token-count window))
                               (format nil "ctx ~a" (format-token-count used)))
                           tone))
                   (when near
                     (cons (if past
                               "evicts next request"
                               (format nil "evicts at ~a"
                                       (format-token-count (* window *context-eviction-trigger-ratio*))))
                           tone))
                   (if (eq detail :developer)
                       (list (cons (format nil "↑ ~a~@[ (cache ~a)~]" (format-token-count input) hit) :muted)
                             (cons (format nil "↓ ~a~@[ (reasoning ~a)~]" (format-token-count output)
                                           (and reasoning (plusp reasoning) (format-token-count reasoning)))
                                   :muted)
                             (and cost-known (cons (format nil "$~,3f" (or cost 0.0)) :muted)))
                       (list (and hit (cons (format nil "cache ~a" hit) :muted)))))))))))

(defun clip-text-to-width (text max-width)
  "TEXT clipped to at most MAX-WIDTH display columns, an … marking the cut."
  ;; The chrome-row overflow guard: every frame row must render exactly the
  ;; frame width, so a lane that cannot fit elides instead of spilling past
  ;; the terminal edge.
  (if (<= (text-display-width text) (max 0 max-width))
      text
      (let ((budget (1- max-width)))
        (if (plusp budget)
            (concatenate 'string (take-display-prefix text budget) "…")
            ""))))

(defun bilateral-frame-row (left-text left-tone right-text width bg &optional left-segs right-segs)
  "One chrome row: the clipped segments own both paint and target ranges.
The left text starts on the gutter rail and the right one ends where the
message boxes do, the scroll gutter's columns left clear."
  (let* ((segments (clip-segs (or left-segs
                                (list (make-seg :text left-text :tone left-tone)))
                            (max 1 (- width 3)) :mark "…"))
         (left-width (segs-display-width segments))
         (right (or right-segs (and right-text (list (make-seg :text right-text :tone :muted)))))
         (right-width (segs-display-width right))
         (inset (- width 2 (transcript-text-width width)))
         (right-fits (and right (<= (+ 3 left-width 2 right-width inset) width)))
         (gap (if right-fits (- width 3 left-width right-width inset) 0))
         (painted (if right-fits
                      (append segments (list (make-seg :text (make-string gap :initial-element #\Space))) right)
                      segments))
         ;; Finalize string identity before attaching EQ-keyed metadata. The
         ;; frame guard keeps this exact-width row, and frame gestures read it.
         (row (nlk:narrow-string
               (pad-frame-row
                (concatenate 'string (blank-frame-row 3 bg)
                             (apply #'concatenate 'string
                                    (mapcar (lambda (seg) (seg-render seg bg)) painted)))
                (+ 3 left-width (if right-fits (+ gap right-width) 0)) width bg))))
    (link-plain-row row (seg-links painted 3))))

(defun session-display-title (model &key (fallback (format nil "~a" (transcript-shown-session-id model))))
  "The title of the session the transcript shows: the name its state carries
(SESSION_TITLE), else the first committed prompt's first line, or FALLBACK
before any prompt. Directory surfaces default to the raw id; Standard's header
supplies a blank."
  ;; A /sessions preview holds one page of a session's history, whose first
  ;; prompt need not be the session's: the gateway's name for it rides the
  ;; previewed state (PREVIEW-PAGE-ENTRY), a blank while it is not known
  ;; rather than a prompt from the middle of the log. An attached session
  ;; folds its whole history and carries no name here.
  (let* ((state (visible-transcript-state model))
         (named (gethash "session_title" state)))
    (cond
      ((stringp named) (if (plusp (length named)) (first-line named 48) fallback))
      (t (if-let (first-user (find-transcript-row (lambda (row)
                                                    (and (eq :text row.kind)
                                                         (equal "user" row.role)
                                                         (plusp (length row.text))))
                                                  state))
           (first-line first-user.text 48)
           fallback)))))

(defvar *workspace-footer-label* nil
  "Memoized WORKSPACE-FOOTER-LABEL: nothing in the organism ever chdirs, so
the cwd is a launch fact — and the footer read its GETCWD syscall plus
pathname parse on every rendered frame.")

(defun workspace-footer-label ()
  "The workspace identity, home-abbreviated (the Zig workspaceFooterLabel
rule): `~/src/thing`, bare `~` at home, the trimmed path elsewhere."
  (or *workspace-footer-label*
      (setf *workspace-footer-label*
            (home-abbreviated (namestring (uiop:getcwd))))))

(defun forget-workspace-memos ()
  "Drop both cwd memos — the footer's label and the directory a clicked
relative path resolves against — so no dumped image carries the BUILD directory's
working directory: warm.lisp renders a frame as the last act of the load,
which memoizes it, and `just release' runs from src/ — so without this every
shipped binary showed `~/nodecode/nodecode/src' (or a packer's throwaway
tree) in its footer on every box, forever."
  ;; The cwd is a launch fact, but the launch that matters is the operator's,
  ;; not the builder's. Rides *SAVE-HOOKS* beside THEME's FORGET-COLOR-PROFILE
  ;; and NLT's FORGET-TERMINAL.
  (setf *workspace-footer-label* nil
        *workspace-directory* nil))

(pushnew 'forget-workspace-memos sb-ext:*save-hooks*)

(defun hover-seg (model text target &optional (tone :muted))
  "TEXT linked to TARGET: the disclosure accent while MODEL's pointer is on it, else TONE."
  (make-seg :text text :link target
            :tone (if (equal target (chat-model-hover-target model)) :disclosure-hover tone)))

(defun footer-frame-row (model width bg)
  "The footer's model, effort and actions left; profile, cwd and gateway right.
Segments are the one authority for text, color, clipping and target ranges."
  (multiple-value-bind (raw-provider raw-model state) (footer-identity model)
    (let* ((provider (json-value raw-provider :text))
           (model-id (json-value raw-model :text))
           (placeholder (cond ((eq state :pending) "setting up…")
                              ((and (chat-model-setup-walk model) (chat-model-dialog model)
                                    (or (eq state :unconfigured) (not (or provider model-id))))
                               "setting up…")
                              ((eq state :unconfigured) "run /setup to add a model")))
           (identity (cond ((and provider model-id)
                            (let ((model-id (if (and (> (length model-id) (1+ (length provider)))
                                                     (prefix-at-p model-id 0 (format nil "~a/" provider)))
                                                (subseq model-id (1+ (length provider))) model-id)))
                              ;; `deepseek deepseek-flash' said the provider twice.
                              (if (model-names-provider-p provider model-id)
                                  model-id
                                  (format nil "~a ~a" provider model-id))))
                           (t (or model-id provider)))))
      (flet ((action (text name) (hover-seg model text (cons :command name))))
        (let* ((left (if placeholder
                         (list (make-seg :text placeholder :tone :muted))
                         ;; The name is green or red by who can read a prompt sent to it
                         ;; (CONNECTION-IDENTITY-TONE): the footer's whole privacy word.
                         (append (and identity (list (hover-seg model identity (cons :command "models")
                                                                (or (connection-identity-tone model) :muted))))
                                 (when-let (effort (and provider model-id (footer-reasoning-effort model)))
                                   (list (action (format nil " (~a)" effort) "think")))
                                 (list (make-seg :text " · " :tone :muted)
                                       (action (footer-gesture-hint model) "sessions")))))
               (right (find-if (lambda (candidate)
                                 (<= (+ 3 (segs-display-width left) 2
                                        (segs-display-width candidate)
                                        (- width 2 (transcript-text-width width)))
                                     width))
                               (footer-right-candidates model))))
          (bilateral-frame-row (segs-text left) :muted (and right (segs-text right))
                               width bg left right))))))

;;; The projected line strings are identity-stable across frames
;;; (ROW-DISPLAY-BLOCK memoizes per row), so the per-row inset+pad+gutter
;;; concatenate — a measured animation-tick heap hotspot — runs once per line,
;;; not once per frame. Weak keys age out with the projection cache.
(defvar *band-row-cache*
  (make-hash-table :test #'eq #+sbcl :weakness #+sbcl :key)
  "viewport line string -> (thumb-row . track-row) banded frame rows.")

(defvar *band-row-cache-epoch* nil
  "(text-width bg palette profile bar-p hot) the band-row cache was built
under; any drift flushes it wholesale.")

(defun row-words-span (y plain &rest more)
  "(Y X0 X1 . MORE) for frame row Y whose style-free text PLAIN has words:
the cells from X0 below X1 they span, past the band's two-column inset; NIL
for no PLAIN or a blank one."
  (when-let (start (and plain (position #\Space plain :test-not #'char=)))
    (list* y (+ 2 (text-display-width (subseq plain 0 start)))
           (+ 2 (text-display-width (string-right-trim " " plain)))
           more)))

(defun transcript-band-rows (model width text-width bg)
  "The transcript band: each viewport row inset 2 columns from the left,
padded to TEXT-WIDTH, then one blank column and the scroll rail's cell in
the frame's last column, WIDTH - 1."
  ;; Submitted questions stay top-aligned, inline or pinned; only a
  ;; promptless short band paints its canvas slack above the content.
  (bind ((window (chat-model-band model))
         (vp-height window.height)
         ;; The visible rows ride straight out of the window's list, so the
         ;; line identity the band-row cache keys on survives the frame.
         (lines (let* ((visible (band-window-visible window text-width
                                            (when (chat-model-hover-row model)
                                              (position (chat-model-hover-row model)
                                                        (chat-model-line-rows model) :test #'eq))))
                       (top-pad (band-window-top-padding window)))
                  (append (make-list top-pad :initial-element "")
                          visible
                          (make-list (- vp-height top-pad (length visible)) :initial-element ""))))
         (inset (if bg (styled "  " (vstyle :bg bg)) "  "))
         (gap (if bg (styled " " (vstyle :bg bg)) " "))
         ;; A band with nothing to scroll draws no bar: a full-height thumb
         ;; would be the strongest mark on the screen and say nothing.
         (bar-p (plusp (band-window-max-offset window)))
         ;; The rail is its half-width thumb alone, lit while the pointer
         ;; rests on the gutter or drags it; off the thumb the gutter is
         ;; canvas. Without this, the Zig gutter's lit column and full-block
         ;; thumb would read as an empty pane beside the text, and a hairline
         ;; track under the thumb as a line down the whole terminal.
         (hot (rail-hot-p model))
         (thumb-cell (styled "▐" (vstyle :fg (visual-style-fg (style-for-tone (if hot :foreground :muted)))
                                          :bg bg)))
         (track-cell gap)
         (epoch (list text-width bg *palette* (effective-color-profile) bar-p hot))
         ((thumb-top thumb-height)
          (scrollbar-thumb vp-height vp-height (+ vp-height (band-window-max-offset window))
                           (band-window-offset window))))
    (unless (equal *band-row-cache-epoch* epoch)
      (setf *band-row-cache-epoch* epoch)
      (clrhash *band-row-cache*))
    ;; Where the running calls' listed lines stand this frame, for the ember:
    ;; under the frame head's three rows, past the two-column inset.
    (setf (ember-rows model)
          (loop for line in lines
                for y from 3
                for call = (ember-line line)
                when (row-words-span y (and call (plain-frame-row line)) call) collect it)
          ;; And where the rows of a place still fading stand, for its light.
          (fade-rows model)
          (loop for line in lines
                for y from 3
                for (fade . item) = (and (plusp (length line)) (fade-line line))
                when (row-words-span y (and fade (not (fade-settled-p fade)) (plain-frame-row line)) fade item)
                  collect it)
          ;; And where the chips a glint still crosses stand.
          (glint-rows model)
          (loop with now = (get-internal-real-time)
                for line in lines
                for y from 3
                nconc (loop for (x0 x1 at) in (and (plusp (length line)) (row-glints line))
                            when (< (since-ms at now) +glint-ms+)
                              collect (list y (+ 2 x0) (+ 2 x1) at))))
    (flet ((banded (line thumb-p)
             (let* ((row (nlk:narrow-string
                          (concatenate
                           'string inset
                           ;; Padding measures painted text, never control bytes.
                           (if (zerop (length line))
                               (blank-frame-row text-width bg)
                               (if bg line (pad-frame-row line (text-display-width (plain-frame-row line))
                                                          text-width nil)))
                           gap (if thumb-p thumb-cell track-cell)))))
               ;; The band owns the final painted string; preserve its source
               ;; ranges after the inset, on both fresh renders and memo hits.
               (link-plain-row row (loop for (start end target) in (plain-row-links line)
                                         collect (list (+ 2 start) (+ 2 end) target))))))
      (loop for line in lines
            for row from 0
            for thumb-p = (and bar-p (<= thumb-top row (+ thumb-top thumb-height -1)))
            collect
            (let ((entry (alexandria:ensure-gethash line *band-row-cache* (cons nil nil))))
              (if thumb-p
                  (or (car entry) (setf (car entry) (banded line t)))
                  (or (cdr entry) (setf (cdr entry) (banded line nil)))))))))

(defun blank-band-line-p (line text-width bg)
  "Whether LINE, one of the band's visible rows, paints bare canvas: the
empty spacer row or a block's unfilled pad."
  (or (zerop (length line))
      (string= line (first (lline-render (block-pad-lline nil) text-width)))
      (and bg (string= line (blank-frame-row text-width bg)))))

(defun steer-key-segs (model &aux (keymap (chat-model-keymap model)))
  "The keys that answer a running turn once the composer holds text, each a
filled cap before its verb: Enter steers the turn (the client's steer leg),
the queue key starts the next one after it. Two columns follow each verb,
the last two the box's right margin."
  ;; Both are rebindable, so they are read from the keymap, and an unbound
  ;; one drops out. Esc stands on the live bar (LIVE-STATUS-LINE).
  (loop for (action verb) in '((:submit "steer") (:submit-turn-queue "queue"))
        for chord = (first (gethash action keymap))
        when chord
          append (list (make-seg :text (format nil " ~(~a~) " (key-chord-label chord))
                                 :tone :key-chip)
                       (make-seg :text (format nil " ~a  " verb) :tone :muted))))

(defun composer-rows (model width height bg)
  "The composer band."
  ;; Tall windows get the box: a borderless filled rectangle over the
  ;; transcript's columns (COMPOSER-BOX-WIDTH from column 2), one pad row
  ;; above and below the text rows, text inset 1 more column, no prompt
  ;; glyph. The box carries one row per visible editor row — the editor grows
  ;; to the pi-mono cap and then scrolls internally. The fill tone is :composer while empty and flips to
  ;; :user-block once the operator types (composer.zig composerContentTone).
  ;; Typed into while a turn runs, Enter steers that turn: the box warms
  ;; (:composer-steer), an amber half-block stands at its left edge in the
  ;; running line's tool amber, and the bottom pad row names the keys
  ;; (STEER-KEY-SEGS) -- the box tells what Enter does before any key is read.
  ;; Short windows get the single-line mode: one full-width filled row.
  (bind ((ta (chat-model-editor model))
         (value (editor-value ta))
         (typed (and value (plusp (length value))))
         (steering (and typed (chat-model-turn-in-flight model)))
         (tone (cond (steering :composer-steer) (typed :user-block) (t :composer)))
         (surface (unless (eq (effective-color-profile) :none)
                    (visual-style-bg (style-for-tone tone))))
         (ta-lines (editor-visible-lines ta))
         (columns (if (shell-footer-p height) (composer-text-width width) (max 1 (- width 2))))
         ;; The placeholder lines while the composer is empty resolve in
         ;; priority order (Zig composerPlaceholderView + the home swap):
         ;; the composer status outranks the home prompt; the widget's own
         ;; Ready placeholder is the floor.
         ;; Expire status even while typed text hides its placeholder.
         ((status-lines under status) (composer-status-lines model columns))
         (select (transcript-select model))
         (hint (and (zerop (length (or value ""))) (not status)
                    (or (and select (select-mode-hint select))
                        (and (home-screen-p model) (home-prompt-text model))
                        ;; The floor, the widget's own placeholder, reads
                        ;; muted like every other placeholder line: it is
                        ;; absence of input, not input. Taking it through
                        ;; the hint lane is what gives it a tone at all —
                        ;; a widget line paints in the box's fill style,
                        ;; the composer's own bright foreground. Without this,
                        ;; while a turn runs `Ready' would read as idle.
                        (if (chat-model-turn-in-flight model)
                            "Type to steer"
                            (json-value ta.placeholder :text)))))
         ;; A placeholder bypasses the editor's own wrapping, so it must
         ;; clip to the composer text columns itself or a long notice
         ;; spills past the box on narrow windows.
         (hint (when hint (clip-text-to-width hint columns)))
         ;; The status wraps at the text columns -- a failure is shown whole
         ;; -- and paints muted, or in the danger tone for a failure.
         (hint-tone (if (and status (composer-status-error-p status)) :danger :muted))
         ;; Over an empty composer the status is its lines; under a draft a
         ;; held one stands below the text, in the rows the text leaves.
         (ta-lines (cond ((and status (not under)) status-lines)
                         (hint (cons hint (rest ta-lines)))
                         (t ta-lines)))
         (under (and under (subseq status-lines 0 (min (length status-lines)
                                                        (max 1 (- (composer-max-rows height)
                                                                  (max 1 (editor-height ta))))))))
         (hint (or hint (and status (not under))))
         ;; Ghost text: the top suggestion's remainder, dim, after the
         ;; cursor (the caret sits at the end whenever a ghost exists).
         (ghost (composer-ghost-remainder model)))
    (labels ((row (cols &rest segs)
               ;; The one composer row painter: SEGS laid down left to right
               ;; over the box fill, then the fill out to COLS — the docked
               ;; panel's own row, every tone forced onto the box surface, so a
               ;; run never counts SGR bytes as columns nor drops the fill
               ;; behind the pad.
               (docked-panel-row segs cols surface :col 0 :pad-left 0 :pad-right 0))
             (keyed (cols segs keys &aux (room (- cols (segs-display-width keys))))
               ;; The bottom pad row with KEYS flush right. The docked row forces
               ;; its surface onto every tone, which would take the caps' own
               ;; ground, so the keys paint through RENDER-ROW; a row too narrow
               ;; to keep its own text beside them goes without them.
               (if (>= room (+ 2 (segs-display-width segs)))
                   (concatenate 'string
                                (apply #'row room segs)
                                (render-row nil keys surface (- cols room)))
                   (apply #'row cols segs)))
             (run (line &optional (run-tone tone))
               ;; A typed line, or the placeholder in its own tone, at the text
               ;; inset: the message column in the box, two in the single-line mode.
               (make-seg :text (concatenate 'string
                                            (make-string (if (shell-footer-p height) (composer-text-inset) 2)
                                                         :initial-element #\Space)
                                            line)
                         :tone run-tone)))
      (if (shell-footer-p height)
          (bind ((side (concatenate 'string (blank-frame-row 1 bg)
                                    (if steering
                                        (styled "▐" (style-with (style-for-tone :tool) :bg bg))
                                        (blank-frame-row 1 bg))))
                 ;; While the keyboard selects, the pad row names its keys.
                 (keys (cond (select (select-key-segs select))
                             (steering (steer-key-segs model))))
                 (right (blank-frame-row (- width 2 (composer-box-width width)) bg))
                 (visible-count (min (length ta-lines)
                                     (- (composer-visible-rows model) (length under))))
                 ;; Text taller than the box scrolls inside it, and the pad
                 ;; row on each side counts the lines out of view there, in
                 ;; the omission line's words. Without this, a paste taller
                 ;; than the box would show its last visible lines with
                 ;; nothing saying the rest was there.
                 ((above below) (editor-hidden-lines ta)))
            (flet ((pad (count where)
                     (when (plusp count)
                       (list (run (format nil "+~d line~:p ~a" count where) :muted)))))
              (loop for (segs . more)
                      on (append (list (pad above "above"))
                                 (loop for line in (subseq ta-lines 0 visible-count)
                                       for index from 0
                                       collect (cond ((and status (not under)) (list (run line hint-tone)))
                                                     ((and hint (zerop index))
                                                      (list (run line :muted)))
                                                     ;; The dim ghost after the typed text,
                                                     ;; cut to the room the row leaves.
                                                     ((and ghost
                                                           (= index (1- visible-count)))
                                                      (list (run line) (make-seg :text ghost :tone :muted
                                                                                 :dim t)))
                                                     (t (list (run line)))))
                                 ;; a held status under the draft
                                 (mapcar (lambda (line) (list (run line hint-tone))) under)
                                 (list (pad below "below")))
                    collect (concatenate 'string side
                                         (if (and keys (null more))
                                             (keyed (composer-box-width width) segs keys)
                                             (apply #'row (composer-box-width width) segs))
                                         right))))
          (list (row width (run (first ta-lines) (if hint hint-tone tone))))))))

;;; --- the frame guard --------------------------------------------------------
;;; The renderer boundary's check of the display authority's contract
;;; (theme.lisp, DISPLAY-GLYPH): a frame is exactly TERM-HEIGHT rows, every
;;; row paints exactly TERM-WIDTH cells, and the only ESC bytes in it are
;;; the SGR runs and OSC-8 pairs this file writes. The authority makes that
;;; true by construction wherever content passes STYLED; this is the belt
;;; to that brace — the :none composer row that never passes it, a
;;; concatenate that never measured, a lane added next year — and it bounds
;;; any breach to the row it is on: nothing in a frame that passed here can
;;; move, style, or link another row.
;;;
;;; Live, a breach is repaired the way the authority would have painted it
;;; (a foreign ESC or control becomes its Control Picture, an over-wide row
;;; is cut at the last whole cell, a short row is padded with canvas) and
;;; counted in *FRAME-GUARD-REPAIRS*, a live-only debug fact that reaches
;;; the operator once as a system notice. Under *FRAME-GUARD-STRICT* — the
;;; suite posture — a breach signals instead, so every gate that composes a
;;; frame asserts the contract for free.

;;; Live-only; nonzero means a lane reached the renderer around the display
;;; authority or mis-measured a row. Cross-thread: SETF'd, never LET-bound.
(defvar *frame-guard-repairs* 0
  "Rows the frame guard repaired since boot.")

(defvar *frame-guard-strict* nil
  "When true a breach signals FRAME-GUARD-VIOLATION instead of being
repaired: the suite posture (CALL-WITH-HERMETIC-MACHINE-STATE binds it).")

(defvar *frame-guard-noticed* nil
  "Whether this image has already surfaced the one repair notice.")

(define-error frame-guard-violation (error) (index reason row)
  (:report "frame guard: row ~a ~a: ~s" index reason row))

(defun own-sequence-end (row i len &aux (j (1+ i)))
  "The index just past the control sequence this file could have written
at I, an ESC in ROW: an SGR run `ESC [ params m' with digit/semicolon
params (SGR-PREFIX, *SGR-RESET*), or an OSC-8 pair
`ESC ] 8 ; ; uri ESC \\' whose body holds no ESC and no control (OSC8-WRAP
drops them)."
  ;; NIL for anything else — a foreign ESC, which must paint as a cell.
  (when (< j len)
    (case (char row j)
      (#\[
       (let ((k (position-if-not (lambda (c) (or (digit-char-p c) (char= c #\;))) row
                                 :start (1+ j) :end len)))
         (and k (char= (char row k) #\m) (1+ k))))
      (#\]
       (and (< (+ j 3) len)
            (string= "8;;" row :start2 (1+ j) :end2 (+ j 4))
            (let ((st (search (load-time-value (format nil "~c\\" #\Escape))
                              row :start2 (+ j 4)))) ; ST, the OSC-8 terminator
              (and st
                   (not (find-if (lambda (ch)
                                   (or (char= ch #\Escape)
                                       (control-codepoint-p (char-code ch))))
                                 row :start (+ j 4) :end st))
                   (+ st 2))))))))

(defparameter *osc8-close*
  (format nil "~c]8;;~c\\" #\Escape #\Escape)
  "The OSC-8 pair's closing half: an empty target ends the link.")

(defun guard-frame-row (row width bg)
  "=> (values ROW' REASON): ROW itself with REASON NIL when it paints exactly
WIDTH cells through this file's own control sequences alone; otherwise the
repaired row and a keyword naming the first breach — :foreign-escape,
:control, :overflow, :short. One scan; the copy starts only at a breach."
  (let ((len (length row))
        (i 0) (cols 0)
        (out nil) (reason nil) (cut nil) (sgr-open nil) (link-open nil))
    (declare (type fixnum len i cols))
    (flet ((breach (kind)
             (unless reason (setf reason kind))
             (unless out
               (setf out (make-string-output-stream))
               (write-string row out :end i))))
      (loop while (< i len)
            do (let* ((ch (char row i))
                      (end (and (char= ch #\Escape)
                                (own-sequence-end row i len))))
                 (cond
                   (end
                    (if (char= (char row (1+ i)) #\[)
                        ;; An SGR run stays open unless its params are a
                        ;; reset: empty or `0'.
                        (setf sgr-open (and (/= (+ i 2) (1- end))
                                            (mismatch "0" row :start2 (+ i 2) :end2 (1- end))))
                        ;; `ESC ] 8 ; ; ESC \' (7 chars) closes; anything
                        ;; longer carries a target and opens.
                        (setf link-open (> end (+ i 7))))
                    (when out (write-string row out :start i :end end))
                    (setf i end))
                   (t
                    (let ((glyph ch) (w 0))
                      (declare (type (integer 0 2) w))
                      (cond ((or (char= ch #\Escape) (control-codepoint-p (char-code ch)))
                             (breach (if (char= ch #\Escape) :foreign-escape :control))
                             (setf glyph (display-glyph ch) w 1))
                            (t (setf w (char-display-width ch))))
                      (when (> (+ cols w) width)
                        (breach :overflow)
                        (setf cut t)
                        (return))
                      (when out (write-char glyph out))
                      (incf cols w)
                      (incf i))))))
      (when cut
        ;; Close what the cut left open, innermost first, so the rest of
        ;; the frame keeps its own style and link state.
        (when sgr-open (write-string *sgr-reset* out))
        (when link-open (write-string *osc8-close* out)))
      (when (< cols width)
        (breach :short)
        (write-string (blank-frame-row (- width cols) bg) out))
      (values (if out (get-output-stream-string out) row) reason))))

(defun guard-frame-rows (rows width height bg)
  "ROWS, the composed frame, checked against the display authority's
contract and repaired (live) or signalled (strict) per breach."
  ;; Always exactly HEIGHT rows of WIDTH painted cells.
  (unless (= (length rows) height)
    (frame-guard-breach nil :height
                        (format nil "~d rows for a ~d-row window" (length rows) height))
    (setf rows (subseq (append rows (make-list height :initial-element (blank-frame-row width bg)))
                       0 height)))
  (loop for tail on rows
        for index from 0
        do (multiple-value-bind (row reason)
               (guard-frame-row (car tail) width bg)
             (when reason
               (frame-guard-breach index reason (car tail))
               (setf (car tail) row))))
  rows)

(defun frame-guard-breach (index reason row)
  "Record one breach: signal under the strict posture, otherwise count it
and surface the one-per-image notice."
  (when *frame-guard-strict*
    (error 'frame-guard-violation :index index :reason reason :row row))
  (incf *frame-guard-repairs*)
  (unless *frame-guard-noticed*
    (setf *frame-guard-noticed* t)
    (surface-system-notice
     (format nil "The frame guard repaired a row (~(~a~)): content reached ~
                  the renderer around the display authority. The frame ~
                  stays aligned; the row may read short or cut."
             reason)
     :error-p nil)))

(defun compose-frame (model)
  "The full frame as a list of exactly TERM-HEIGHT rows (the Zig ChatShell
stack): blank, header, top padding, the transcript band, the gap, the
composer, and — on windows of 12+ rows — the composer/footer gap, the
footer, and the final blank; short windows end with two blank rows."
  (let* ((width (max 8 (chat-model-term-width model)))
         (height (max 4 (chat-model-term-height model)))
         (bg (canvas-fill-bg))
         (text-width (transcript-text-width width))
         (blank (blank-frame-row width bg))
         ;; The first row the toasts may not reach: the composer's gap, or
         ;; the rows under a docked panel.
         (toast-floor 0)
         ;; The header: title, Developer-only dimmed session id, usage lane.
         ;; Home and setup yield the identity; Standard has no raw-id fallback.
         (status (header-status-text model))
         (status-width (if status (text-display-width status) 0))
         (fits (and status (<= (+ 3 4 2 status-width 3) width)))
         (header-row
           (if (or (home-screen-p model) (chat-model-setup-walk model))
               (bilateral-frame-row "" :session-title (and fits status) width bg
                                    nil (and fits (header-status-segs model)))
               (let* ((developer-p (eq :developer (chat-model-tool-detail model)))
                      (session-id (format nil "~a" (transcript-shown-session-id model)))
                      (title (session-display-title model :fallback (if developer-p session-id "")))
                      (identity (if (or (not developer-p) (equal title session-id))
                                    title
                                    (format nil "~a · ~a" title session-id)))
                      ;; A column budget, not a character count: a wide glyph in
                      ;; the title costs two.
                      (max-cols (max 4 (- width 6 (if fits (+ 2 status-width) 0))))
                      (shown (clip-text-to-width identity max-cols))
                      (separator (and developer-p (not (equal title session-id))
                                      (search " · " shown)))
                      (left-segs (when separator
                                   (list (make-seg :text (subseq shown 0 separator)
                                                   :tone :session-title)
                                         (make-seg :text " · " :tone :muted :dim t)
                                         (make-seg :text (subseq shown (+ separator 3))
                                                   :tone :session-id)))))
                 (bilateral-frame-row shown :session-title (and fits status)
                                      width bg left-segs
                                      (and fits (header-status-segs model)))))))
    ;; Settle geometry before painting: branches that reset or fill the
    ;; composer outside the shared update tail (gateway submit, slash
    ;; dispatch) change its height without passing through a key message,
    ;; and the frame must stack to exactly TERM-HEIGHT rows. The editor's
    ;; height is computed from its content; the viewport is what settles.
    (sync-viewport-geometry model)
    ;; Settle paint the same way, and for the same reason: every band below
    ;; recomposes from live tones on this pass, but the transcript band reads
    ;; finished SGR strings out of the viewport, which only RENDER-STATE
    ;; refreshes. /themes moves *PALETTE* outside the render path — the
    ;; picker's live preview on every highlight, its Enter, its Esc restore —
    ;; so without this the whole shell re-themes except the one surface the
    ;; operator is reading. The guard is an EQ probe per frame; it re-projects
    ;; only when a paint input actually moved.
    ;;
    ;; The home screen is a compose input no projection carries: a dialog
    ;; closing or a notice going leaves the rows as they were, and without this
    ;; Esc off the /generations sheet would leave the band a frame under it
    ;; blanked. It moves only while the transcript is empty, so the projection
    ;; it costs is of nothing.
    (unless (and (projection-cache-live-p model)
                 (eq (home-screen-p model) (chat-model-home-painted model)))
      (render-state model))
    (let ((rows
           (guard-frame-rows
            (bind ((head (list blank header-row blank))
           (band (transcript-band-rows model width text-width bg))
           (panel (dialog-panel-rows model width bg))
           (foot (and (shell-footer-p height)
                      (list blank (footer-frame-row model width bg) blank))))
       (if panel
           ;; The frame head — blank, header, chat top padding — is all the
           ;; panel cannot reach; everything under it down to the shell
           ;; footer is its slot. The
           ;; budget is read back from the rows actually stacked rather than
           ;; from a closed form, so a surface another slice adds above the
           ;; composer keeps its rows.
           (multiple-value-bind (panel-height above below)
               (docked-dialog-frame-rows model)
             ;; The panel replaces the composer, so the composer caret is gone;
             ;; a panel that owns a text field takes the cursor with it (Zig
             ;; ui.zig: cursor_vis off, then the dialog's own focus puts it
             ;; back), and a panel that owns none leaves it hidden.
             (let* ((dialog (chat-model-dialog model))
                    (inner (- panel-height 2))
                    (header (min (dialog-header-row-count dialog) inner)))
               ;; Just past TEXT printed at COL inside the padded rect, the text
               ;; clipped to its room — DOCKED-PANEL-ROW's decomposition, whose
               ;; pads a frame at least 8 columns wide always holds whole.
               (flet ((cell (row col text &aux (inner (- width *docked-panel-pad-left*
                                                          *docked-panel-pad-right*)))
                        (when (< col inner)
                          (cons (+ *docked-panel-pad-left* col
                                   (min (text-display-width text) (- inner col)))
                                row))))
                 (setf (chat-model-parked-cursor model)
                       (if dialog.searchable
                           (when (> header *docked-panel-search-row*)
                             (cell (+ above 1 *docked-panel-search-row*)
                                   *docked-panel-content-col*
                                   dialog.search))
                           (multiple-value-bind (label value) (dialog-input-field dialog)
                             (when (and label
                                        (> (- inner header (dialog-footer-row-count dialog)) 0))
                               (cell (+ above 1 header)
                                     (+ *docked-panel-content-col*
                                        (text-display-width label)
                                        *dialog-input-field-gap*)
                                     value)))))))
             (let* ((tail (or foot (list blank)))
                    (tail (last tail (min below (length tail)))))
               (setf toast-floor (- height (length tail)))
               ;; Above the panel: the blank, the header, then the chat top
               ;; padding, the band, and the canvas separator row that keeps
               ;; the panel surface off the transcript's last line.
               (bind (((pad gap want) (docked-dialog-above-bands above))
                      (kept (min want (length band))))
                 (append (subseq head 0 (min 2 (length head)))
                         ;; The pad row, then — a band shorter than its slot pads
                         ;; above, so the transcript stays bottom-anchored against
                         ;; the gap — the band's slack.
                         (make-list (+ pad (- want kept)) :initial-element blank)
                         (last band kept)
                         (make-list gap :initial-element blank)
                         panel
                         tail))))
           ;; The composer dropdown docks BELOW the composer (the Zig
           ;; composer lift), so the band has already yielded its rows
           ;; through SYNC-VIEWPORT-GEOMETRY.
           (let ((dropdown (composer-dropdown-panel-rows model width bg)))
             ;; The composer paints, so its caret cell is where the hardware
             ;; cursor parks -- unless the keyboard selects the transcript,
             ;; whose cursor it stands on then (SELECT-CURSOR-CELL), hidden
             ;; while the window does not show that line.
             (setf (chat-model-parked-cursor model)
                   (if (transcript-select model)
                       (select-cursor-cell model)
                       (multiple-value-bind (x y)
                           (editor-cursor-screen-position (chat-model-editor model))
                         (when (and x (< y (composer-visible-rows model)))
                           (let* ((tall (shell-footer-p height))
                                  ;; blank + header + blank, the band, the gap blank, and
                                  ;; the box's top pad row on tall windows.
                                  (row (+ 3 (length band) 1 (if tall 1 0) y))
                                  ;; Tall windows inset the side gutter (2) plus the box's own
                                  ;; text inset; the short single-line mode only its inset (2).
                                  (col (+ (if tall (+ 2 (composer-text-inset)) 2) x)))
                             (when (< row height)
                               (cons (min col (1- width)) row)))))))
             (setf toast-floor (+ (length head) (length band)))
             ;; The live block is the band's own tail, so the scroll gutter
             ;; ends one canvas row over the composer.
             (append head
                     band
                     (list blank)
                     ;; Under an open sheet the composer takes no keys, and its
                     ;; box would show round the window's lower edge: its rows
                     ;; stand blank.
                     (let ((composer (composer-rows model width height bg)))
                       (if (sheet-dialog-p (chat-model-dialog model))
                           (make-list (length composer) :initial-element blank)
                           composer))
                     dropdown
                     (or foot (list blank blank))))))
            width height bg)))
      ;; An open sheet stands over this frame: its window is painted for it
      ;; here, the overlay lays it, and the cursor parks where it says.
      (settle-sheet model width height)
      ;; The frame is the selection's region, so it is stamped here — once,
      ;; for every row, whatever painted it: the transcript band, the turn
      ;; bar, the composer, a docked panel, the footer. The plain lane the copy
      ;; and the word gesture read is derived from these guarded rows, and the
      ;; wash (SELECTION-OVERLAY) marks the cells the painter lays them out
      ;; into, so what the wash covers and what a copy carries are the same
      ;; text.
      (setf (frame-lanes model)
            (make-frame-lane (coerce rows 'vector)
                             (frame-width model) (frame-height model))
            ;; The toasts over these rows, which the view's overlay paints
            ;; and the pointer's hit test reads.
            (chat-model-toast-layout model) (place-toasts model width toast-floor))
      rows)))

