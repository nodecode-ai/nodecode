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

(defparameter +selection-copy-failed-hint+
  "Copy failed: no clipboard path worked here. Shift+drag selects with the terminal's own selection"
  "The one thing a copy says: that neither a clipboard tool nor the
terminal's OSC 52 took the text. A copy that lands says nothing, and nor
does one with nothing marked: the empty composer is the caret alone.")

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
  ;; (and what a token lane inserts there), SOURCE :slash, :file or :folder (an
  ;; @ path row that goes on into a folder, ACCEPT-SUGGESTION), DETAIL the
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

(defun submit-slash (model text &key (dropdown t))
  "Execute the slash command TEXT names; DROPDOWN NIL runs the line as it
stands, whatever the composer's dropdown highlights."
  ;; The dropdown selection wins while open — Enter on a bare prefix runs the
  ;; highlighted command — and a literal name (typically with arguments)
  ;; parses directly. A known command resets the composer; an unknown name
  ;; keeps the text for editing and explains itself in the composer status.
  ;; Returns (values handled cmd).
  (multiple-value-bind (name args) (parse-slash-input text)
    (let ((entry (or (let ((row (and dropdown
                                     (selected-of (slash-active-suggestions model)
                                                  (chat-model-slash-selection model)))))
                       (and row (composer-suggestion-entry row)))
                     (find-slash-command name))))
      (setf (chat-model-slash-selection model) 0)
      (cond (entry
             (editor-reset (chat-model-editor model))
             (run-slash-entry model entry args))
            (t (show-unknown-slash model name))))))

(defun run-slash-entry (model entry args)
  "Run the catalog ENTRY with ARGS and say its answer in the composer status.
Returns (values t cmd), SUBMIT-SLASH's."
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

(defun show-unknown-slash (model name)
  "Say that no command is named NAME. Returns (values t nil)."
  (show-status model (format nil "unknown command /~a — /help lists commands" name)
               :error-p t)
  (render-state model)
  (values t nil))

(defun run-bound-slash-line (model line)
  "Run LINE, a slash line keybinds.json binds to a chord, as if typed and
entered -- but around the composer: its draft and the dropdown stay as they
stand. Returns the command to run after the update, or NIL."
  ;; The draft is left alone because a bound line is a gesture over it, not
  ;; its replacement: a command that sets the draft aside reads it as it was.
  (multiple-value-bind (name args) (parse-slash-input line)
    (nth-value 1 (if-let (entry (find-slash-command name))
                   (run-slash-entry model entry args)
                   (show-unknown-slash model name)))))

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
  "The active @ suggestion list, or NIL."
  ;; The slash lane owns `/` inputs outright (Zig
  ;; composerSuggestionsForComposerText order); the list exists only while the
  ;; caret sits at the end of the text (the Zig composerSuggestionList cursor
  ;; gate), so completions always land on the trigger token. The catalog is
  ;; walked off this path: an unloaded one asks for the walk
  ;; (START-COMPOSER-CATALOG-WALK) and this reads whatever has landed, so no
  ;; keystroke ever waits on the filesystem. A token that starts at a root of
  ;; its own takes the path lane instead (PATH-TOKEN-SUGGESTIONS); while none
  ;; is typed the lane forgets its last folder, so the next one lists fresh.
  (let ((filter (unless (or (slash-input-p text)
                            (not (composer-cursor-at-end-p (chat-model-editor model))))
                  (composer-token-filter text #\@))))
    (cond ((and filter (path-token-p filter))
           (path-token-suggestions model filter))
          (t
           (setf (chat-model-path-listing-asked model) nil)
           (when filter
             (unless (chat-model-file-catalog-loaded model)
               (start-composer-catalog-walk model))
             (loop for path in (chat-model-file-catalog model)
                   when (token-suggestion filter path :file)
                     collect it))))))

;;; --- the @ lane's paths outside the workspace --------------------------------
;;; A token that starts at a root of its own -- the home `~/', the folder `./',
;;; its parent `../', an absolute `/', and on Windows `C:\' and `\' with
;;; either separator -- completes shell-style from a listing of the one folder
;;; it names, not from the workspace catalog. The listing runs off the input
;;; path as the catalog's walk does (ASK-PATH-LISTING), so no keystroke waits
;;; on the disk. The accepted text is the path as typed (`@~/notes.md'): a sent
;;; @path is plain text the agent reads with its own tools, so this lane opens
;;; no file to anyone.

(defun path-token-separator-p (char &optional (windows (uiop:os-windows-p)))
  "Whether CHAR separates the folders of an @ path token: `/', and on
Windows `\\' too."
  (or (char= char #\/) (and windows (char= char #\\))))

(defun path-token-p (filter &optional (windows (uiop:os-windows-p)) &aux (length (length filter)))
  "Whether FILTER, the text after an `@', names a path from a root of its own
rather than one in the workspace catalog. A bare `~' or `..' counts: it
completes to its folder."
  (flet ((separator-at (index)
           (and (< index length) (path-token-separator-p (char filter index) windows))))
    (or (member filter '("~" "..") :test #'string=)
        (separator-at 0)
        (and (plusp length) (member (char filter 0) '(#\~ #\.)) (separator-at 1))
        (and (uiop:string-prefix-p ".." filter) (separator-at 2))
        (and windows (> length 2) (alpha-char-p (char filter 0)) (char= #\: (char filter 1))
             (separator-at 2)))))

(defun path-token-head (filter &optional (windows (uiop:os-windows-p)))
  "FILTER, an @ path token, up to and including its last separator: the
folder it lists. NIL for a bare `~' or `..'."
  (when-let (last (position-if (lambda (char) (path-token-separator-p char windows))
                               filter :from-end t))
    (subseq filter 0 (1+ last))))

(defun path-token-folder (head &optional (workspace (uiop:getcwd)))
  "The folder HEAD, an @ path token's head, names: `~' read against the
operator's home, `./' and `../' against WORKSPACE, an absolute path as itself."
  (let ((home (char= #\~ (char head 0))))
    (let ((folder (uiop:parse-native-namestring (if home (subseq head 2) head)
                                                :ensure-directory t)))
      (cond (home (nlk:home-path folder))
            ((char= #\. (char head 0)) (uiop:merge-pathnames* folder workspace))
            (t folder)))))

(defun path-token-rows (filter entries &key (windows (uiop:os-windows-p))
                                            (fold-case (or windows (uiop:os-macosx-p))))
  "The @ path rows ENTRIES, the listing of FILTER's folder, offer FILTER: each
entry whose name starts with what FILTER typed past its head, a hidden one
only once that starts with `.'. A folder row ends in the separator FILTER
uses, so accepting it goes on into the folder. FOLD-CASE matches names as a
case-insensitive file system does; elsewhere a row's case is exact, so the
completed path names the file."
  (if-let (head (path-token-head filter windows))
    (let ((name (subseq filter (length head)))
          (separator (char head (1- (length head)))))
      (loop for (entry . folderp) in entries
            for target = (format nil "~a~a~:[~;~c~]" head entry folderp separator)
            when (and (or (uiop:string-prefix-p "." name) (not (uiop:string-prefix-p "." entry)))
                      (<= (length name) (length entry))
                      (if fold-case
                          (string-equal name entry :end2 (length name))
                          (string= name entry :end2 (length name))))
              collect (composer-suggestion target (subseq target (length filter))
                                           (if folderp :folder :file)
                                           :detail (if folderp "folder" "file"))))
    ;; A bare `~' or `..' is one row: its folder.
    (list (composer-suggestion (uiop:strcat filter "/") "/" :folder :detail "folder"))))

(defun path-token-suggestions (model filter &aux (head (path-token-head filter)))
  "The path lane's rows for FILTER from the listing MODEL holds for its head,
asking for that folder whenever the token moved to another one."
  ;; A listing in hand paints at once while the fresh one is read, so the
  ;; rows a folder answered before stand until its new listing lands.
  (when (and head (not (equal head (chat-model-path-listing-asked model))))
    (setf (chat-model-path-listing-asked model) head)
    (ask-path-listing model head))
  (path-token-rows filter (and head (cdr (assoc head (chat-model-path-listings model)
                                                :test #'string=)))))

(defun composer-trigger-command (model msg &aux (key (nlt:key-event-code msg)))
  "The cell command whose TRIGGER the key MSG just typed as the draft's
first character, or NIL."
  ;; The core's `$' opens /index here, so a definition is named from the
  ;; index's own list. Only as the whole draft so far: anywhere later in it
  ;; -- `a$b', `costs $5', a second line's start -- the character is text.
  (when (and (characterp key)
             (string= (editor-value (chat-model-editor model)) (string key)))
    (find (string key) *gateway-commands*
          :key #'slash-command-trigger :test #'equal)))

(defun composer-suggestions-for (model)
  "The live dropdown's rows, from whichever lane matches: the slash catalog
owns `/` inputs outright (Zig composerSuggestionsForComposerText order) and
the token lanes take everything else."
  ;; NIL is also every caller's reading of `no dropdown is open`. Under an
  ;; ask the text is the ask's, never a command or a token to complete.
  (unless (chat-model-composer-ask model)
    (or (slash-active-suggestions model)
        (token-suggestions-for model))))

(defun selected-composer-suggestion (model)
  "The highlighted row of whichever lane is live, else NIL."
  (selected-of (composer-suggestions-for model)
               (chat-model-slash-selection model)))

(defun accept-suggestion (model)
  "Complete the highlighted row (Zig acceptComposerInlineSuggestion)."
  ;; A slash row rewrites the whole composer with the canonical name alone:
  ;; a command is its name, so nothing is invited after it, and the dropdown
  ;; stays on the row, Enter running it. A token row inserts its remainder
  ;; plus a space at the cursor; a folder row its remainder alone, which ends
  ;; in the separator, so the dropdown goes on into the folder. => T when a
  ;; completion landed.
  (when-let (suggestion (selected-composer-suggestion model))
    (let ((editor (chat-model-editor model)) (slash (eq :slash suggestion.source)))
      (when slash (editor-reset editor))
      (editor-insert editor (case suggestion.source
                              (:slash suggestion.text)
                              (:folder suggestion.remainder)
                              (t (uiop:strcat suggestion.remainder " ")))))
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
  ;; A chord keybinds.json binds to the bare command (#199) is its hotkey too.
  (when-let (entry suggestion.entry)
    (or (when-let (action (slash-command-keybind entry))
          (keybind-chords-label (chat-model-keymap model) action :unbound nil))
        (keybind-line-label (chat-model-keymap model) (format nil "/~a" (slash-command-name entry))))))

(defun dropdown-hotkey-lane (model suggestion key-width)
  "The row's right lane for SUGGESTION, or NIL when its command has no
chord: the label padded to KEY-WIDTH so every chord in the window starts at
the same column."
  ;; The lane is painted flush right, so that shared start falls out of the
  ;; lane's own width — one column for the whole window, never one per row.
  (when-let (label (composer-suggestion-hotkey model suggestion))
    (list (seg (pad-frame-row label (text-display-width label) key-width nil) :muted))))

(defun dropdown-panel-row (segs width bg surface margin &key scrollbar right)
  "One full-WIDTH dropdown row on the composer box's columns: MARGIN canvas
columns on its left (the composer's own, COMPOSER-LEFT-MARGIN) and the rest
of the row on its right — painted with the box's own bg, not the dropdown
surface — around a DOCKED-PANEL-ROW with no side padding and the 2 + 1 + 2
scrollbar lane the dropdown reserves (*SCROLLBAR-RESERVED-COLUMNS*, Zig
dropdownItemWindow + renderDropdownScrollbar)."
  ;; A description too long for the row ends in `…' at the cut; without this
  ;; it would stop mid-word at the edge.
  (let* ((box (composer-box-width width))
         (inner (docked-panel-row segs box surface
                                  :col 0 :pad-left 0 :pad-right 0
                                  :bar-gap 2 :scrollbar scrollbar
                                  :right right :mark "…"))
         (row (concatenate 'string
                           (blank-frame-row margin bg)
                           inner
                           (blank-frame-row (- width margin box) bg))))
    ;; The panel row's targets stand MARGIN columns in on the frame's row.
    (if-let (links (plain-row-links inner))
      (link-plain-row (nlk:narrow-string row)
                      (loop for (start end target) in links
                            collect (list (+ start margin) (+ end margin) target)))
      row)))

(defun composer-dropdown-panel-rows (model width bg)
  "The active dropdown's panel rows: the windowed entries plus the bottom
padding row, all on the composer surface (Zig renderDropdownBody fills the
whole dropdown rect). NIL while no lane matches or the window has no room."
  (multiple-value-bind (panel visible count)
      (composer-dropdown-geometry model)
    (when panel
      (bind ((entries (composer-suggestions-for model))
             (height (chat-model-term-height model))
             ;; The composer's own columns: a completion target starts on
             ;; the column its typed text does, so `/mo' and the `/models'
             ;; it completes to read as one word.
             (margin (composer-left-margin height))
             (indent (composer-text-indent height))
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
                         ;; The text indent, ending in the selection mark where
                         ;; no surface brightens the selected row.
                         (list (seg (format nil "~v@a" indent (if (and (null surface) selected) "> " ""))
                                    :user-block :link target)
                               (seg (composer-suggestion-text entry) :syntax-command :link target))
                         (when (plusp (length alias))
                           (list (seg (format nil " (~a)" alias) :muted :link target)))
                         ;; Two columns past the window's widest label, the
                         ;; docked rows' separator: at one, the widest row's
                         ;; alias would run into its description, `/tree
                         ;; (/checkpoints) Browse'.
                         (list (seg (format nil "~va" gap "") :user-block :link target)
                               (seg (composer-suggestion-detail entry) (if selected :user-block :muted) :link target)))
                        width bg surface margin
                        ;; The chord this command also answers to, parked
                        ;; at the row's right edge — past the description,
                        ;; the way a menu names a command's key.
                        :right (dropdown-hotkey-lane model entry hotkey-width)
                        :scrollbar (scrollbar-cell thumb-top thumb-height row)))
         ;; The panel's own bottom padding row: surface fill, no
         ;; scrollbar cell (Zig draws the bar on the row window only).
         (list (dropdown-panel-row '() width bg surface margin)))))))

(defun attach-composer-prompt (model text)
  "Put TEXT, a prompt the command line gave (#141), in the composer exactly:
verbatim when a paste of it would land unchanged, else collapsed to a
`[Paste #N] ' marker whose stash is TEXT itself, so the prompt that goes out
is byte for byte the one given -- its tabs, controls and outer blanks
included, which a paste would picture or trim."
  (if (and (string= text (normalize-pasted-text text))
           (<= (length text) *paste-collapse-max-chars*)
           (<= (1+ (count #\Newline text)) *paste-collapse-max-lines*))
      (editor-insert (chat-model-editor model) text)
      (let ((id (incf (chat-model-paste-counter model))))
        ;; No space after it: the marker's expansion is the whole prompt.
        (editor-insert (chat-model-editor model) (format nil "[Paste #~d]" id))
        (push (cons id text) (chat-model-pastes model)))))

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

