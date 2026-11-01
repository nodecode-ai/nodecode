;;;; composer.lisp --- the composer: list surfaces, slash drops, status, autocomplete, paste. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- keyboard list surfaces ------------------------------------------------
;;; The three primitives every keyboard-driven list or field surface below
;;; shares — the composer's two dropdown lanes, the docked picker, and the
;;; /models wizard. Each surface keeps its own bindings and its own refusal
;;; copy; only the arithmetic is shared.

(defun wrap-selection (selection count delta)
  "SELECTION moved by DELTA over COUNT rows with wraparound."
  ;; SELECTION is clamped first, so an index left behind by a list that shrank
  ;; still lands in range; an empty list selects 0.
  (if (plusp count)
      (mod (+ (min selection (1- count)) delta) count)
      0))

(defun selected-of (rows selection)
  "The row SELECTION points at, clamped to ROWS, or NIL when ROWS is empty."
  (when rows
    (nth (min selection (1- (length rows))) rows)))

(defun past-headings (rows at delta &aux (count (length rows)))
  "AT, or the first index from it that is no heading row (PICKER-ROW-HEADING),
stepping DELTA through ROWS with wraparound."
  (loop repeat count
        while (picker-row-heading (nth at rows))
        do (setf at (wrap-selection at count delta)))
  at)

(defun edit-text-field (text key &key ctrl alt max)
  "One keystroke against a one-line text field => the field's new TEXT, or NIL
when the key is none of the field's own."
  ;; The shared edit ladder: Ctrl+U clears, Ctrl+W and Ctrl/Alt+Backspace
  ;; delete the trailing word, Backspace deletes one character, and a bare
  ;; printable character appends while the field is shorter than MAX (NIL for
  ;; no cap). Any other key — and Backspace on an empty field, which must not
  ;; disturb the surface's selection — declines, so the caller falls through
  ;; to its own bindings instead of the field swallowing them. The cleared
  ;; field answers "", which is a field's answer like any other.
  (cond ((and ctrl (characterp key) (char-equal key #\u)) "")
        ((or (and ctrl (characterp key) (char-equal key #\w))
             (and (eq key :backspace) (or ctrl alt)))
         ;; The trailing word goes, and the blanks after it.
         (values (ppcre:regex-replace "[^ \\t]*[ \\t]*\\z" text "")))
        ((eq key :backspace)
         (when (plusp (length text)) (subseq text 0 (1- (length text)))))
        ((and (characterp key) (not ctrl) (not alt) (graphic-char-p key))
         (if (or (null max) (< (length text) max))
             (concatenate 'string text (string key))
             text))))

;;; --- slash dropdown + dispatch --------------------------------------------
;;; The composer's command lane (slash.lisp owns the catalog/parser). The
;;; dropdown is derived from the live composer text every frame — no open/
;;; closed flag to fall out of sync — and only the selection index is stored.
;;; Both shells share these: the gateway model inherits the navigation keys
;;; through CALL-NEXT-METHOD and calls SUBMIT-SLASH from its own Enter branch.

;;; --- keybind consultation ---------------------------------------------------
;;; The update function dispatches through the resolved keymap (keys.lisp)
;;; instead of hardcoded characters, the Zig keybinds.zig resolution.

(defun model-key-matches-p (model action code mod)
  "Whether the pressed CODE+MOD dispatches ACTION under MODEL's keymap."
  (keybind-matches-p (chat-model-keymap model) action code mod))

(defun refresh-model-keymap (model)
  "Re-resolve MODEL's keymap when it was built from another action table than
the one now in force — what makes a keybind row (or an override) defined
into a running image live at the next keystroke."
  ;; One identity check per keystroke otherwise: the resolved keymap carries
  ;; its own source table.
  (unless (eq (gethash +keybind-source-key+ (chat-model-keymap model))
              *keybind-actions*)
    (setf (chat-model-keymap model) (resolve-keybinds))))

(defun composer-empty-p (model)
  "The Zig ComposerState.isEmpty gate: trimmed text empty."
  (zerop (length (nlk:trimmed (editor-value (chat-model-editor model))))))

;;; --- the composer status ------------------------------------------------------
;;; Everything the shell says about what the operator just did or asked -- a
;;; pick landed, a key saved, a command's answer, a prompt the gateway
;;; refused -- stands in the composer's placeholder, where the eye already is,
;;; and nowhere else. A lane of its own above the composer would take the home
;;; screen away and stand until the next prompt, and a failure that toasted
;;; would not show while popups were off. A line said once is news and goes
;;; after five seconds; a status that wraps, and a failure, stand until the
;;; composer is edited, a prompt is sent or another status takes the place. A
;;; failure paints in the danger tone. The status wraps rather than clips, so
;;; a failure is shown whole; one taller than the composer can grow opens as a
;;; list instead (SHOW-STATUS).

(defstruct (composer-status (:constructor make-composer-status (text error-p held draft)))
  "One composer status: its TEXT; ERROR-P, whether it says a failure; HELD,
whether it stands until the composer moves, and DRAFT, the composer's text
when it was said, which an edit moves off; BORN, when it was said."
  text error-p held draft (born (get-internal-real-time)))

(defparameter +selection-copied-hint+ "Copied selection to clipboard"
  "Zig selection_copied_notice, verbatim: said only once a clipboard tool took
the text (NLT:WRITE-CLIPBOARD's (:COMMAND NAME)).")

(defparameter +selection-osc52-hint+
  "Sent to the terminal as OSC 52: no clipboard tool here took it. Paste empty? Turn on OSC 52 in the terminal, or Shift+drag"
  "What a copy says when only the terminal's OSC 52 was left: the sequence was
written, and the terminal may have it off, so it never says `copied'.")

(defparameter +selection-copy-failed-hint+
  "Copy failed: no clipboard path worked here. Shift+drag selects with the terminal's own selection"
  "What a copy says when neither a clipboard tool nor the terminal took it.")

(defparameter +selection-empty-hint+ "Nothing selected to copy"
  "What a copy says with no marked words.")

(defun status-text-lines (text width)
  "TEXT as the composer status shows it at WIDTH columns: its lines, each
wrapped (WRAPPED-LINES), a blank line kept."
  (loop for line in (or (split-lines text) (list ""))
        append (or (wrapped-lines line (max 1 width)) (list ""))))

(defun show-status (model text &key error-p (title "Answer"))
  "Say TEXT in MODEL's composer status, as a failure with ERROR-P; NIL or an
empty TEXT takes the status down. A TEXT taller than the composer can grow
opens as a list titled TITLE while no dialog is open. => NIL."
  (let* ((text (and (stringp text) (string-right-trim '(#\Space #\Tab #\Return #\Newline) text)))
         (lines (and text (plusp (length text))
                     (status-text-lines text (composer-text-width (or (chat-model-term-width model) 80))))))
    (cond ((null lines)
           (setf (chat-model-composer-status model) nil))
          ((and (> (length lines) (composer-max-rows (or (chat-model-term-height model) 24)))
                (null (chat-model-dialog model)))
           (setf (chat-model-composer-status model) nil)
           (open-picker-dialog model title
                               (mapcar (lambda (line) (make-picker-row :label line)) lines)
                               nil
                               :searchable t :search-placeholder "Search"
                               :hint "esc close  ↑↓ navigate  type search"))
          (t
           (setf (chat-model-composer-status model)
                 (make-composer-status text (and error-p t) (and (or error-p (rest lines)) t)
                                       (composer-draft model))))))
  nil)

(defun composer-draft (model)
  "The text in MODEL's composer, or NIL for a model that has none."
  (when-let (editor (chat-model-editor model)) (editor-value editor)))

(defun active-status (model)
  "MODEL's composer status while it stands, else NIL: news for five seconds,
a held one until the composer's text moves off the draft it was said over."
  (when-let (status (chat-model-composer-status model))
    (if (if (composer-status-held status)
            (equal (composer-status-draft status) (composer-draft model))
            (< (since-ms (composer-status-born status)) 5000)) ; Zig composer_notice_visible_ms
        status
        (setf (chat-model-composer-status model) nil))))

(defun status-said (model)
  "The words MODEL's composer status says while it stands, else NIL."
  (when-let (status (active-status model)) (composer-status-text status)))

;;; --- composer autocomplete: the slash and @file lanes -----------------------
;;; The Zig composer_suggestions.zig triggers beside the slash dropdown: `/`
;;; completes catalog commands, `@` workspace paths. Both produce the same
;;; row, so the panel, the ghost, the navigation and the accept below are each
;;; written once. Every list is derived from the live composer text every
;;; frame — no open/closed flag to fall out of sync with — and only the
;;; selection index (the shared SLASH-SELECTION slot) is stored.

(defstruct (composer-suggestion
            (:constructor composer-suggestion (text remainder source
                                               &key detail alias entry)))
  "One dropdown row, whatever lane produced it."
  ;; TEXT is the completion
  ;; target as the row paints it, REMAINDER what the ghost shows after the cursor
  ;; (and what a token lane inserts there), SOURCE :slash or :file, DETAIL the
  ;; description column, ALIAS the command's other triggers painted beside the
  ;; label (slash rows only, empty elsewhere), and ENTRY the catalog
  ;; SLASH-COMMAND the row dispatches on Enter.
  (text "" :type string)
  (remainder "" :type string)
  (source nil)
  (detail "" :type string)
  (alias "" :type string)
  (entry nil))

(nlk:access (suggestion composer-suggestion))

(defun slash-active-suggestions (model &aux (text (editor-value (chat-model-editor model)))
                                            (prefix (subseq text (min 1 (length text)))))
  "The slash lane's rows: every catalog entry the live composer text matches."
  ;; TEXT is the canonical `/name` the label paints and Tab writes; REMAINDER
  ;; completes the trigger the operator is ACTUALLY typing, so a command
  ;; reached through an alias ghosts that alias while accepting it still
  ;; writes the canonical name — the Zig ghost/accept split, kept.
  (mapcar (lambda (entry)
            (let* ((name (slash-command-name entry))
                   (aliases entry.aliases)
                   (target (find-if (lambda (trigger) (slash-name-prefix-p prefix trigger))
                                    (cons name aliases))))
              (composer-suggestion
               (format nil "/~a" name)
               (if target (subseq target (length prefix)) "")
               :slash
               :detail (slash-command-description entry)
               :alias (format nil "~{/~a~^ ~}" aliases)
               :entry entry)))
          (slash-suggestions text)))

(defun submit-slash (model text)
  "Execute the slash command TEXT names."
  ;; The dropdown selection wins while open — Enter on a bare prefix runs the
  ;; highlighted command — and a literal name (typically with arguments)
  ;; parses directly. A known command resets the composer; an unknown name
  ;; keeps the text for editing and explains itself in the composer status.
  ;; Returns (values handled cmd).
  (multiple-value-bind (name args) (parse-slash-input text)
    (let ((entry (or (let ((row (selected-of (slash-active-suggestions model)
                                             (chat-model-slash-selection model))))
                       (and row (composer-suggestion-entry row)))
                     (find-slash-command name))))
      (setf (chat-model-slash-selection model) 0)
      (cond (entry
             (editor-reset (chat-model-editor model))
             ;; What stood there before is over; an op that says its own status
             ;; (/fork, /themes NAME) answers NIL, and its line stands.
             (show-status model nil)
             (multiple-value-bind (notice cmd)
                 (execute-slash-command model (slash-command-name entry) args)
               ;; The answer stands in the composer status.
               (when notice
                 (show-status model notice :title (format nil "/~a~@[ ~a~]" (slash-command-name entry)
                                                          (json-value args :text))))
               (render-state model)
               (values t cmd)))
            (t (show-status model (format nil "unknown command /~a — /help lists commands" name)
                            :error-p t)
               (render-state model)
               (values t nil))))))

(defun composer-token-filter (text trigger)
  "The bare token after the last TRIGGER character, or NIL once whitespace
follows it (Zig composerFileMentionFilterAtCursor with the cursor at end of
text)."
  (when-let (pos (position trigger text :from-end t))
    (let ((filter (subseq text (1+ pos))))
      (unless (find-if #'blank-char-p filter)
        filter))))

(defun token-suggestion (filter target source)
  "One suggestion when FILTER prefixes TARGET (Zig prefix + case rules:
strict case-insensitive prefix, or full-length case-insensitive equality)."
  ;; The detail column is the lane's own name, which is what the Zig
  ;; ComposerDropdownItem prints for a catalog entry carrying no description
  ;; of its own; a token row leaves the alias column empty.
  (when (slash-name-prefix-p filter target)
    (composer-suggestion target (subseq target (length filter)) source
                         :detail (string-downcase source))))

(defun token-suggestions-for (model &aux (text (editor-value (chat-model-editor model))))
  "The active @file suggestion list, or NIL."
  ;; The slash lane owns `/` inputs outright (Zig
  ;; composerSuggestionsForComposerText order); the list exists only while the
  ;; caret sits at the end of the text (the Zig composerSuggestionList cursor
  ;; gate), so completions always land on the trigger token. The catalog is
  ;; walked off this path: an unloaded one asks for the walk
  ;; (START-COMPOSER-CATALOG-WALK) and this reads whatever has landed, so no
  ;; keystroke ever waits on the filesystem.
  (unless (or (slash-input-p text)
              (not (composer-cursor-at-end-p (chat-model-editor model))))
    (when-let (filter (composer-token-filter text #\@))
      (unless (chat-model-file-catalog-loaded model)
        (start-composer-catalog-walk model))
      (loop for path in (chat-model-file-catalog model)
            when (token-suggestion filter path :file)
              collect it))))

(defun composer-trigger-command (model msg &aux (editor (chat-model-editor model))
                                                (key (nlt:key-event-code msg)))
  "The cell command whose TRIGGER the key MSG just typed at a word's
start, or NIL."
  ;; The core's `$' opens /index here, so a definition is named from the
  ;; index's own list. Only at the end of a draft that is no slash command,
  ;; and only where the character starts a word: `a$b' and `/x $' stay text.
  (when (and (characterp key) (composer-cursor-at-end-p editor))
    (let* ((text (editor-value editor))
           (end (length text)))
      (when (and (plusp end) (char= key (char text (1- end)))
                 (not (slash-input-p text))
                 (or (= end 1) (blank-char-p (char text (- end 2)))))
        (find (string key) *gateway-commands*
              :key #'slash-command-trigger :test #'equal)))))

(defun composer-suggestions-for (model)
  "The live dropdown's rows, from whichever lane matches: the slash catalog
owns `/` inputs outright (Zig composerSuggestionsForComposerText order) and
the token lanes take everything else."
  ;; NIL is also every caller's reading of `no dropdown is open`.
  (or (slash-active-suggestions model)
      (token-suggestions-for model)))

(defun selected-composer-suggestion (model)
  "The highlighted row of whichever lane is live, else NIL."
  (selected-of (composer-suggestions-for model)
               (chat-model-slash-selection model)))

(defun accept-suggestion (model)
  "Complete the highlighted row (Zig acceptComposerInlineSuggestion)."
  ;; A slash row rewrites the whole composer with the canonical name plus a
  ;; trailing space, so the caret lands at the end and the whitespace closes
  ;; the dropdown (whitespace ends name matching); a token row inserts its
  ;; remainder plus a space at the cursor. => T when a completion landed.
  (when-let (suggestion (selected-composer-suggestion model))
    (let ((editor (chat-model-editor model)) (slash (eq :slash suggestion.source)))
      (when slash (editor-reset editor))
      (editor-insert editor (uiop:strcat (if slash suggestion.text suggestion.remainder) " ")))
    (setf (chat-model-slash-selection model) 0)
    t))

(defun accept-token-suggestion-for-submit (model)
  "Enter over an open TOKEN dropdown completes instead of submitting, but
only when the completion would actually extend the text (Zig
acceptComposerTokenSuggestionForSubmit)."
  ;; The slash lane is not this lane: Enter there dispatches the command.
  (let ((suggestion (selected-composer-suggestion model)))
    (when (and suggestion (not (eq :slash suggestion.source)) (plusp (length suggestion.remainder)))
      (accept-suggestion model))))

(defun composer-dropdown-key (model key &aux (count (length (composer-suggestions-for model)))
                                             (selection (chat-model-slash-selection model)))
  "One dropdown navigation key over whichever completion lane is live."
  ;; Every lane shares the selection slot and the row type, so they share the
  ;; whole ladder: :up/:down move the selection with wraparound and :tab
  ;; accepts the highlighted row. => T when a live lane took the key.
  (when (plusp count)
    (if (eq key :tab)
        (accept-suggestion model)
        (setf (chat-model-slash-selection model)
              (wrap-selection selection count (ecase key (:up -1) (:down 1)))))
    t))

(defun composer-ghost-remainder (model &aux (ta (chat-model-editor model)))
  "The inline dim completion after the cursor: the selected row's remainder,
only while the caret sits at the end of the text (the Zig
composerSuggestionList cursor-at-end gate). NIL when nothing completes."
  (when (and (plusp (length (editor-value ta)))
             (composer-cursor-at-end-p ta))
    (let ((suggestion (selected-composer-suggestion model)))
      (and suggestion (json-value suggestion.remainder :text)))))

;;; --- the composer dropdown panel ---------------------------------------------
;;; The Zig composer dropdown (composer.zig renderDropdownBody): one row
;;; shape for the slash and @file lanes, docked BELOW the composer
;;; box — the composer lifts and the transcript band yields the rows — on
;;; the composer's own content surface with the box's two-column canvas
;;; margins. Rows have no selection bar: the completion target always
;;; paints in the composer token style (composerMarkerStyle) and selection
;;; brightens the description column from muted to the surface foreground.
;;; The token style is :syntax-command, not the Zig :syntax-builtin: every
;;; bundled theme keeps builtin green while the rest of the palette runs
;;; warm (Tungstene: builtin #65a30d against accents #fca864/#fa8b3e), so
;;; the green lane read as off-theme chrome. :syntax-command is the warm
;;; command-word tone the shell lexer already paints (#fdba74), and a
;;; completion target is a command word. A list longer than the window grows
;;; the Zig scrollbar lane at the rect's right edge, and the panel ends in
;;; one surface-filled padding row (composer_dropdown_rows = visible + 1).

(defun composer-dropdown-geometry (model)
  "=> (values PANEL-ROWS VISIBLE-COUNT TOTAL-COUNT) for the active
dropdown, or NIL."
  ;; PANEL-ROWS counts the visible entries plus the bottom padding row (Zig
  ;; composer_dropdown_rows = visible + 1), clamped so the transcript band
  ;; keeps a row. NIL while no lane matches or while a docked dialog owns the
  ;; bottom of the shell — a dialog outranks the composer for keys, so a stale
  ;; dropdown under it would be chrome without a gesture.
  (unless (chat-model-dialog model)
    (let ((count (length (composer-suggestions-for model))))
      (when (plusp count)
        (let* ((height (chat-model-term-height model))
               (room (max 0 (- height (shell-overhead height (composer-visible-rows model)) 1)))
               (panel (min (1+ (min count 8)) room))) ; 8 rows windowed on the selection
          (when (>= panel 2)
            (values panel (1- panel) count)))))))

(defun dropdown-label-width (suggestion)
  "Display columns SUGGESTION spends before the detail column: the
completion target plus the parenthesized alias (Zig
dropdownLabelWidthForItem)."
  (+ (text-display-width suggestion.text)
     (if (plusp (length suggestion.alias))
         (+ 2 (text-display-width suggestion.alias) 1)
         0)))

(defun composer-suggestion-hotkey (model suggestion)
  "The chord SUGGESTION's command also answers to, or NIL when it has none —
the / dropdown's hotkey lane (SLASH-COMMAND-KEYBIND)."
  ;; Read through MODEL's own resolved keymap, so an override in keybinds.json
  ;; shows the chord the operator actually has and a command whose action was
  ;; unbound shows nothing: a hotkey exists here only when a key really fires
  ;; it. NIL for a lane that is no command at all (@file), which is
  ;; every row without a catalog entry.
  (when-let (action (and suggestion.entry (slash-command-keybind suggestion.entry)))
    (keybind-chords-label (chat-model-keymap model) action :unbound nil)))

(defun dropdown-hotkey-lane (model suggestion key-width)
  "The row's right lane for SUGGESTION, or NIL when its command has no
chord: the label padded to KEY-WIDTH so every chord in the window starts at
the same column."
  ;; The lane is painted flush right, so that shared start falls out of the
  ;; lane's own width — one column for the whole window, never one per row.
  (when-let (label (composer-suggestion-hotkey model suggestion))
    (list (seg (pad-frame-row label (text-display-width label) key-width nil) :muted))))

(defun dropdown-panel-row (segs width bg surface &key scrollbar right)
  "One full-WIDTH dropdown row on the composer box's columns: its canvas
margins either side — painted with the box's own bg, not the dropdown surface —
around a DOCKED-PANEL-ROW with no side padding and the 2 + 1 + 2 scrollbar lane
the dropdown reserves (*SCROLLBAR-RESERVED-COLUMNS*, Zig dropdownItemWindow +
renderDropdownScrollbar)."
  ;; A description too long for the row ends in `…' at the cut; without this
  ;; it would stop mid-word at the edge.
  (let* ((box (composer-box-width width))
         (inner (docked-panel-row segs box surface
                                  :col 0 :pad-left 0 :pad-right 0
                                  :bar-gap 2 :scrollbar scrollbar
                                  :right right :mark "…"))
         (row (concatenate 'string
                           (blank-frame-row 2 bg)
                           inner
                           (blank-frame-row (- width 2 box) bg))))
    ;; The panel row's targets stand two columns in on the frame's row.
    (if-let (links (plain-row-links inner))
      (link-plain-row (nlk:narrow-string row)
                      (loop for (start end target) in links
                            collect (list (+ start 2) (+ end 2) target)))
      row)))

(defun composer-dropdown-panel-rows (model width bg)
  "The active dropdown's panel rows: the windowed entries plus the bottom
padding row, all on the composer surface (Zig renderDropdownBody fills the
whole dropdown rect). NIL while no lane matches or the window has no room."
  (multiple-value-bind (panel visible count)
      (composer-dropdown-geometry model)
    (when panel
      (bind ((entries (composer-suggestions-for model))
             (selection (min (chat-model-slash-selection model) (1- count)))
             (top (max 0 (min (- count visible) (1+ (- selection visible)))))
             (window (subseq entries top (+ top visible)))
             (label-width (reduce #'max window
                                  :key #'dropdown-label-width
                                  :initial-value 0))
             ;; The hotkey column: the chords of the commands in this window
             ;; ("/sessions Ctrl+S"), one shared width so every chord starts
             ;; at the same column and the description column reads the same
             ;; on every row. A window with no chords spends no columns —
             ;; the lane disappears rather than reserving blank space.
             (hotkey-width (loop for entry in window
                                 for label = (composer-suggestion-hotkey model entry)
                                 maximize (if label (text-display-width label) 0)))
             ;; The dropdown paints on the composer's content surface —
             ;; :user-block, the tone the box itself flips to once the
             ;; operator types, and a dropdown only exists over typed text
             ;; (composer.zig composerContentTone) — or NIL under :none.
             (surface (unless (eq (effective-color-profile) :none)
                        (visual-style-bg (style-for-tone :user-block))))
             (bar-p (> count visible))
             ;; The dropdown's bar spans its own window, so the track is
             ;; also the visible count (Zig dropdownScrollbarThumb).
             ((thumb-top thumb-height) (when bar-p (scrollbar-thumb visible visible count top))))
        (append
         (loop for entry in window
               for row from 0
               for selected = (= (+ top row) selection)
               for alias = (composer-suggestion-alias entry)
               for gap = (+ 2 (- label-width (dropdown-label-width entry)))
               ;; A row is the pointer's to choose, as Enter chooses the
               ;; highlighted one (HANDLE-MOUSE-RELEASE).
               for target = (cons :suggestion (+ top row))
               collect (dropdown-panel-row
                        (append
                         (list (seg (if (and (null surface) selected) "> " "  ") :user-block :link target)
                               (seg (composer-suggestion-text entry) :syntax-command :link target))
                         (when (plusp (length alias))
                           (list (seg (format nil " (~a)" alias) :muted :link target)))
                         ;; Two columns past the window's widest label, the
                         ;; docked rows' separator: at one, the widest row's
                         ;; alias would run into its description, `/tree
                         ;; (/checkpoints) Browse'.
                         (list (seg (format nil "~va" gap "") :user-block :link target)
                               (seg (composer-suggestion-detail entry) (if selected :user-block :muted) :link target)))
                        width bg surface
                        ;; The chord this command also answers to, parked
                        ;; at the row's right edge — past the description,
                        ;; the way a menu names a command's key.
                        :right (dropdown-hotkey-lane model entry hotkey-width)
                        :scrollbar (scrollbar-cell thumb-top thumb-height row)))
         ;; The panel's own bottom padding row: surface fill, no
         ;; scrollbar cell (Zig draws the bar on the row window only).
         (list (dropdown-panel-row '() width bg surface)))))))

;;; --- paste capture and image attachments -------------------------------------
;;; The Zig attachComposerPaste / attachComposerImage pair: large pastes
;;; collapse to a [Paste #N] marker re-expanded at submit; Ctrl+V probes the
;;; system clipboard for an image and stages it as a pending attachment.

(defun attach-composer-paste (model text &aux (normalized (normalize-pasted-text (or text ""))))
  "Insert pasted TEXT: verbatim when small, collapsed to a `[Paste #N] `
marker with the content stashed for submit-time expansion when large."
  ;; => T when anything landed.
  (when (plusp (length normalized))
    ;; Past either bound the paste collapses (Zig
    ;; shouldCollapseComposerPaste).
    (if (or (> (length normalized) *paste-collapse-max-chars*)
            (> (1+ (count #\Newline normalized)) *paste-collapse-max-lines*))
        (let ((id (incf (chat-model-paste-counter model))))
          (editor-insert (chat-model-editor model) (format nil "[Paste #~d] " id))
          (push (cons id normalized) (chat-model-pastes model)))
        (editor-insert (chat-model-editor model) normalized))
    t))

(defun expand-composer-pastes (model prompt)
  "PROMPT with every stored `[Paste #N]` marker replaced by its captured
content; markers without a store pass through (Zig
expandComposerPasteMarkers)."
  ;; A marker matches a store entry only as that entry spells it, so
  ;; `[Paste #03]' is text, not paste 3.
  (ppcre:regex-replace-all "\\[Paste #\\d+\\]" prompt
                           (lambda (marker)
                             (or (cdr (find marker (chat-model-pastes model)
                                            :key (lambda (paste)
                                                   (format nil "[Paste #~d]" (car paste)))
                                            :test #'string=))
                                 marker))
                           :simple-calls t))

(defun clear-composer-attachments (model)
  "Drop stored pastes and staged images after a submit consumed them."
  (setf (chat-model-pastes model) nil
        (chat-model-pending-images model) nil))

(defun command-output (argvs &key stripped)
  "The output of the first of ARGVS that exits 0 with some, or NIL: each an
argv run through RUN-BOUNDED, its output without its final newline when
STRIPPED."
  ;; The clipboard readers' ladder: the first helper this box has answers.
  (dolist (argv argvs)
    (bind (((output _ status)
            (ignore-errors (run-bounded argv :seconds 10 :error-output nil))))
      (let ((output (and (eql status 0) (stringp output)
                         (if stripped (string-right-trim '(#\Newline #\Return) output) output))))
        (when (plusp (length output))
          (return output))))))

(defparameter *mac-clipboard-png*
  "ObjC.import('AppKit');
var p = $.NSPasteboard.generalPasteboard, d = p.dataForType('public.png');
if (d.isNil()) { var t = p.dataForType('public.tiff');
  if (!t.isNil()) d = $.NSBitmapImageRep.imageRepWithData(t).representationUsingTypeProperties($.NSBitmapImageFileTypePNG, $({})); }
d.isNil() ? '' : d.base64EncodedStringWithOptions(0).js"
  "The Mac pasteboard's image as base64 PNG, through osascript's JavaScript:
a screenshot lands there as PNG, an image copied from Preview or a page as
TIFF, which is re-encoded.")

(defparameter *windows-clipboard-image*
  "$ProgressPreference = 'SilentlyContinue'
$ErrorActionPreference = 'Stop'
[void][Reflection.Assembly]::LoadWithPartialName('System.Windows.Forms')
[void][Reflection.Assembly]::LoadWithPartialName('System.Drawing')
$types = @{ '.png' = 'image/png'; '.jpg' = 'image/jpeg'; '.jpeg' = 'image/jpeg'; '.gif' = 'image/gif'; '.webp' = 'image/webp' }
$d = [Windows.Forms.Clipboard]::GetDataObject()
if ($d -eq $null) { exit 1 }
$png = $d.GetData('PNG')
if ($png -is [IO.MemoryStream]) { 'image/png ' + [Convert]::ToBase64String($png.ToArray()); exit 0 }
if ($d.GetDataPresent([Windows.Forms.DataFormats]::FileDrop)) {
  foreach ($f in $d.GetData([Windows.Forms.DataFormats]::FileDrop)) {
    $t = $types[[IO.Path]::GetExtension($f).ToLowerInvariant()]
    if ($t) { $t + ' ' + [Convert]::ToBase64String([IO.File]::ReadAllBytes($f)); exit 0 }
  }
}
$i = [Windows.Forms.Clipboard]::GetImage()
if ($i -ne $null) { $m = [IO.MemoryStream]::new(); $i.Save($m, [Drawing.Imaging.ImageFormat]::Png); 'image/png ' + [Convert]::ToBase64String($m.ToArray()); exit 0 }
exit 1"
  "The Windows clipboard's image as one `MEDIA-TYPE BASE64' line, through
Windows PowerShell: the PNG a browser or the snipping tool puts there, else
the first image file copied in Explorer, else the bitmap a screenshot key
leaves, re-encoded as PNG. Nothing is printed, and the exit is 1, when the
clipboard holds no image. No cmdlet runs, so no module loads: a cold run
is the .NET start alone, about a fifth of a second.")

(defun windows-clipboard-image-argv ()
  "The argv that runs *WINDOWS-CLIPBOARD-IMAGE*: Windows PowerShell, which
every Windows box carries, single-threaded as the clipboard requires, the
script passed as -EncodedCommand (UTF-16LE base64) so no quote of it meets
the command line."
  (list "powershell.exe" "-NoProfile" "-NonInteractive" "-STA" "-EncodedCommand"
        (cl-base64:usb8-array-to-base64-string
         (sb-ext:string-to-octets *windows-clipboard-image* :external-format :utf-16le))))

(defun clipboard-image-line (line)
  "LINE as *WINDOWS-CLIPBOARD-IMAGE* prints it => (values MEDIA-TYPE BASE64),
or NIL when it names no image type or carries no payload."
  (when-let (space (and line (position #\Space line)))
    (let ((media-type (subseq line 0 space))
          (base64 (subseq line (1+ space))))
      (when (and (member media-type '("image/png" "image/jpeg" "image/webp" "image/gif")
                         :test #'string=)
                 (plusp (length base64)))
        (values media-type base64)))))

(defun read-clipboard-image-base64 ()
  "=> (values MEDIA-TYPE BASE64) for the system clipboard's image, or NIL."
  ;; Probes the same Wayland/X11 helpers as the Zig readImageCommand,
  ;; capturing through `base64` so the payload never transits a character
  ;; decode; a Mac has neither, and answers PNG from its pasteboard; Windows
  ;; answers the type and the payload together, in one PowerShell run.
  #+win32
  (clipboard-image-line (command-output (list (windows-clipboard-image-argv)) :stripped t))
  #-win32
  (dolist (media-type '("image/png" "image/jpeg" "image/webp" "image/gif")) ; Zig image_media_types
    (when-let (output (command-output
                       #+darwin (and (equal media-type "image/png")
                                     (list (list "osascript" "-l" "JavaScript"
                                                 "-e" *mac-clipboard-png*)))
                       #-(or win32 darwin)
                       (list (shell-argv (format nil "wl-paste --type ~a | base64 -w0" media-type))
                             (shell-argv (format nil "xclip -selection clipboard -t ~a -o ~
                                                      | base64 -w0"
                                                 media-type)))
                       :stripped t))
      (return (values media-type output)))))

(defun stage-composer-image (model media-type base64)
  "Stage one image as a pending attachment of MODEL's next submit, behind an
`[Image #N] ' marker typed where the cursor is. => T."
  (editor-insert (chat-model-editor model)
                 (format nil "[Image #~d] "
                         (1+ (length (chat-model-pending-images model)))))
  (alexandria:appendf (chat-model-pending-images model)
                      (list (cons media-type base64)))
  t)

(defun paste-clipboard-image (model)
  "Stage the system clipboard's image in MODEL's composer. => T when there
was one."
  (multiple-value-bind (media-type base64) (read-clipboard-image-base64)
    (when media-type
      (stage-composer-image model media-type base64))))

(defun pasted-image-file (text)
  "TEXT, a paste, when it is nothing but the path of an image file on this
machine => (values MEDIA-TYPE BASE64), else NIL."
  ;; What Windows Terminal pastes for a snip (Win+Shift+S, PrintScreen) or an
  ;; image copied in Explorer -- the clipboard carries the file too, and the
  ;; terminal pastes files as their paths -- and what a file dragged onto a
  ;; terminal types. The path may come quoted; dragged onto a POSIX terminal,
  ;; its spaces may come backslashed. The type is the file's own bytes, and a
  ;; file past the provider's ceiling stays a path.
  (let* ((path (string-trim '(#\Space #\Tab #\Newline #\Return) text))
         (path (if (and (>= (length path) 2)
                        (find (char path 0) "\"'")
                        (char= (char path 0) (char path (1- (length path)))))
                   (subseq path 1 (1- (length path)))
                   path)))
    (when (and (< 0 (length path) 4096)
               (not (find #\Newline path))
               (some (lambda (extension)
                       (let ((end (- (length path) (length extension))))
                         (and (plusp end) (string-equal extension path :start2 end))))
                     '(".png" ".jpg" ".jpeg" ".gif" ".webp")))
      (dolist (candidate (remove-duplicates
                          (list path #-win32 (ppcre:regex-replace-all "\\\\(.)" path "\\1"))
                          :test #'equal))
        (let* ((octets (ignore-errors
                        (nlt:file-octets (uiop:parse-native-namestring candidate) *image-max-bytes*)))
               (media-type (nlt:image-media-type octets)))
          (when media-type
            (return (values media-type (cl-base64:usb8-array-to-base64-string octets)))))))))

