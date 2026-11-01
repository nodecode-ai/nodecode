;;;; keys.lisp --- input material: dialog search, keybinds, composer catalogs,
;;;; and paste normalization. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure material the shells consume, ported from the Zig TUI — except the
;;;; dialog search, which outgrew its port:
;;;;   - dialog search   the tokenized, tiered fuzzy matcher every dialog
;;;;                     filters AND ranks through. Zig's dialog_search.zig
;;;;                     carried the fuzzy name over a substring-only test:
;;;;                     `gtw' did not answer `gateway' and nothing ordered
;;;;                     the survivors. This one matches a scattered
;;;;                     subsequence and scores how well it matched.
;;;;   - keybinds        src/tui/model/keybinds.zig (declarative default
;;;;                     keymap resolved through user config overrides)
;;;;   - file catalog    src/tui/file_catalog.zig (the @-mention workspace
;;;;                     walk: fixed skip list, no gitignore parse in Zig
;;;;                     either)
;;;;   - frontmatter     src/tools/skill_catalog.zig (the SKILL.md parser
;;;;                     the import cell reads)
;;;;   - paste collapse  src/tui/model/composer.zig (normalize + the 10-line /
;;;;                     1000-char collapse bounds)

(in-package #:nodecode.evolved)

;;; --- dialog search ---------------------------------------------------------
;;;
;;; A query is tokenized on blanks; every token must answer the row, over the
;;; fields the row exposes. A token answers a field in one of five tiers — the
;;; whole field, its prefix, a substring opening a word, a substring anywhere,
;;; a fuzzy subsequence — and the row's score is the sum of its tokens'.
;;;
;;; The score is a rank, not only a filter: SEARCH-SCORE says both whether a
;;; row survives (NIL, or a number) and where it belongs among the survivors.
;;; Within a tier it says nothing, deliberately — the caller sorts stably, so
;;; a list whose own order already means something (a session's recency, a
;;; provider's grouping) is lifted, never scrambled.

(defparameter *max-dialog-search-chars* 128
  "Bound on one dialog's search buffer.")

(defun search-separator-p (ch)
  "The word-boundary set: the punctuation names are built out of, so
`select-model', `select_model', `select model' and `select/model' all read
as the same two words."
  (member ch '(#\Space #\Tab #\Return #\Newline #\/ #\\ #\_ #\- #\. #\:)))

(defun search-word-start-p (text index)
  "Whether TEXT's character at INDEX opens a word: the first character, the
one after a separator, or the upper of a camelCase hump."
  (and (< index (length text))
       (or (zerop index)
           (search-separator-p (char text (1- index)))
           (and (upper-case-p (char text index))
                (not (upper-case-p (char text (1- index))))))))

(defun search-subsequence-p (needle haystack &key word-starts &aux (pos 0))
  "Whether NEEDLE's characters appear in HAYSTACK in order, case
insensitively — the fuzzy match itself."
  ;; Separators in NEEDLE are ignored,
  ;; so `select-model' and `selectmodel' ask the same question. WORD-STARTS
  ;; restricts every character to a word start, which is what lets an acronym
  ;; (`sm') answer `select-model' without letting it answer every string that
  ;; happens to carry an s somewhere before an m.
  (loop for ch across needle
        always (or (search-separator-p ch)
                   (loop for at from pos below (length haystack)
                         thereis (and (char-equal ch (char haystack at))
                                      (or (not word-starts) (search-word-start-p haystack at))
                                      (setf pos (1+ at)))))))

(defun search-lead-tier (needle field at)
  "The tier NEEDLE found AT FIELD's start earns: the whole field 5, its prefix 4."
  (and (eql at 0) (if (= (length needle) (length field)) 5 4)))

(defun search-score (search fields)
  "How well a row exposing FIELDS answers SEARCH — the number it ranks by,
or NIL when it does not answer at all: every query token must answer some
field, and one unmatched token rejects the row."
  ;; The row's score is the sum of its tokens', so a row answering a token
  ;; outright outranks one answering it by a scattered subsequence. A blank
  ;; query scores every row 0, which leaves the caller's own order untouched.
  ;; FIELDS are in priority order.
  (loop for token in (split-words search)
        for count below 16              ; the bound on tokens one query contributes
        ;; Four points a tier per field TOKEN answers, one less per earlier field.
        for scores = (loop for field in fields
                           for index from 0
                           for at = (search token field :test #'char-equal)
                           for tier = (cond
                                        ((zerop (length field)) nil)
                                        ((search-lead-tier token field at))
                                        (at (if (search-word-start-p field at) 3 2))
                                        ;; Then with separators dropped: `gpt5' finds "gpt-5".
                                        (t
                                         (let* ((bare (remove-if #'search-separator-p token))
                                                (plain (remove-if #'search-separator-p field))
                                                (bat (and (plusp (length bare))
                                                          (search bare plain :test #'char-equal))))
                                           (cond ((search-lead-tier bare plain bat))
                                                 ((search-subsequence-p token field
                                                                        :word-starts t)
                                                  3)
                                                 (bat 2)
                                                 ((search-subsequence-p token field) 1)))))
                           when tier collect (- (* 4 tier) index))
        unless scores return nil
        sum (reduce #'max scores)))

;;; --- keybinds (keybinds.zig) ----------------------------------------------

(defstruct (key-chord (:constructor key-chord (code &key ctrl alt shift)))
  "One configured key: CODE is a character (letters stored lowercase) or a
key keyword (:enter :tab :escape :backspace :delete :up :down :left
:right :home :end :page-up :page-down)."
  code ctrl alt shift)

(nlk:access (chord key-chord))

;;; A label is a config spelling too: spellings are matched ignoring case.
(defparameter +key-names+
  '((:enter "Enter") (:tab "Tab") (:escape "Esc" "escape")
    (:backspace "Backspace") (:delete "Delete" "del")
    (:up "Up") (:down "Down") (:left "Left") (:right "Right") (#\Space "Space")
    (:page-up "PgUp" "pageup")
    (:page-down "PgDn" "pagedown" "pgdown")
    (:home "Home") (:end "End")
    (:f1 "F1") (:f2 "F2") (:f3 "F3") (:f4 "F4") (:f5 "F5") (:f6 "F6") (:f7 "F7") (:f8 "F8")
    (:f9 "F9") (:f10 "F10") (:f11 "F11") (:f12 "F12"))
  "The named keys: (CODE LABEL . CONFIG-SPELLINGS) — the Zig
keyCodepointFromConfigPart table plus the paging/home/end names lisp actions
bind, with the label writeKeySpecLabel prints for each.")

(defun parse-key-chord (text &aux (trimmed (nlk:trimmed (or text ""))) ctrl alt shift code)
  "The Zig KeySpec.parseConfigString grammar: `+`-joined modifier parts
(ctrl/alt/shift; mod = ctrl) around one key part."
  ;; "none" disables the action (returns :none); malformed input returns NIL.
  ;; Key names extend the Zig table with pgup/pgdn/home/end, which lisp
  ;; actions genuinely bind.
  (cond
    ((string-equal trimmed "none") :none)
    (t
     (dolist (raw (uiop:split-string trimmed :separator "+"))
       (let ((part (string-trim '(#\Space #\Tab) raw)))
         (cond
           ((member part '("ctrl" "mod") :test #'string-equal) (setf ctrl t))
           ((string-equal part "alt") (setf alt t))
           ((string-equal part "shift") (setf shift t))
           ((null code)
            (setf code (or (car (find-if (lambda (row) (member part (cdr row) :test #'string-equal))
                                         +key-names+))
                           (and (= 1 (length part)) (char-downcase (char part 0)))
                           (return-from parse-key-chord nil))))
           (t (return-from parse-key-chord nil)))))
     (when code
       (key-chord code :ctrl ctrl :alt alt :shift shift)))))

(defparameter +hint-arrow-keys+ '(("←" . :left) ("→" . :right) ("↑" . :up) ("↓" . :down))
  "The arrow glyphs a key hint names an arrow key by.")

(defun hint-key-press (label &aux (label (nlk:trimmed (or label ""))))
  "The key press the key hint LABEL names -- `esc', `enter', `ctrl+f', `→',
a lowercase letter -- as a fresh message, the one a terminal sends for it;
NIL for a label naming no single key: `↑↓', `up/down', `type', prose."
  ;; A click on a key's hint is that key (DIALOG-PANEL-ROWS, PAINT-KEYS). A
  ;; hint is written in lowercase, so a capital -- the first word of a
  ;; sentence a dialog says there instead -- names no key.
  (let ((chord (if-let (arrow (cdr (assoc label +hint-arrow-keys+ :test #'string=)))
                 (key-chord arrow)
                 (and (string= label (string-downcase label)) (parse-key-chord label)))))
    (when (and (key-chord-p chord)
               (or (keywordp chord.code) (standard-char-p chord.code)))
      (nlt:make-key-press-msg
       :code chord.code
       :mod (logior (if chord.ctrl nlt:+mod-ctrl+ 0) (if chord.alt nlt:+mod-alt+ 0)
                    (if chord.shift nlt:+mod-shift+ 0))
       :text (if (and (characterp chord.code) (not chord.ctrl) (not chord.alt))
                 (string chord.code)
                 "")))))

(defun hint-key-label-p (word)
  "Whether WORD, a key hint's first word, is a cap's: one key HINT-KEY-PRESS
knows, a run of arrow glyphs (`↑↓', `←→'), those joined by `/' (`up/down',
`del/ctrl+x'), or `type', the letters a search reads -- the caps a sheet
names too (PAINT-KEYS)."
  ;; Broader than HINT-KEY-PRESS, which answers one key: a cap may name a
  ;; pair, and only a cap naming one key is a click target.
  (flet ((key-p (part)
           (and (plusp (length part))
                (or (every (lambda (char) (assoc (string char) +hint-arrow-keys+ :test #'string=))
                           part)
                    (and (hint-key-press part) t)))))
    (or (equal word "type")
        (every #'key-p (uiop:split-string (or word "") :separator "/")))))

;;; One row per action the lisp shells actually dispatch; the Zig action enum
;;; carries surfaces (tabs, dashboards) with no lisp backend, which stay
;;; absent rather than dead.
(defparameter *keybind-actions*
  '((:quit "Quit" ("ctrl+q" "ctrl+d"))
    (:submit "Submit message" ("enter"))
    ;; Tab is deliberately double-booked (the Zig keybinds.zig default):
    ;; submit-turn-queue wins only while a turn runs — the gateway shell
    ;; checks it ahead of accept-suggestion, the Zig evaluation order.
    (:submit-turn-queue "Submit turn-queue message" ("tab"))
    (:insert-newline "Insert newline" ("shift+enter" "alt+enter" "ctrl+j"))
    (:accept-suggestion "Accept suggestion" ("tab"))
    (:history-previous "Previous prompt / suggestion" ("up"))
    (:history-next "Next prompt / suggestion" ("down"))
    ;; Up/Down walk the prompt ring one entry at a time; this opens the whole
    ;; ring in a dialog. Ctrl+R is free (Ctrl+P is the palette, Ctrl+O the
    ;; tool cards, Ctrl+F the dialog height) and is the history chord an
    ;; operator already has in their fingers.
    (:history-dialog "Browse every past prompt" ("ctrl+r"))
    ;; The Zig open_session_launcher chord — keySpec('s', .{ .ctrl = true })
    ;; (keybinds.zig) — whose lisp surface is the /sessions picker; the port
    ;; kept the surface and dropped the key. Ctrl+S is unowned in both
    ;; dispatch ladders (the base update and the gateway shell's), and raw mode
    ;; clears IXON (term/terminal.lisp), so the terminal never eats it as the XOFF
    ;; flow-control character.
    (:sessions-dialog "Open or close the sessions picker" ("ctrl+s"))
    ;; F2 is the rename key file managers taught, and free: a picker's typing
    ;; is its search, so a letter cannot open the name field.
    (:rename "Rename the highlighted session in the sessions picker" ("f2"))
    (:toggle-viewport "Switch Developer / Standard viewport" ("ctrl+o"))
    ;; Ctrl+F while a dialog is open: the docked panel takes every row the
    ;; window can give it (TOGGLE-DIALOG-FULLSCREEN). Zig binds the chord to
    ;; the session tree's fork, a surface with no lisp backend, so it is free.
    (:toggle-dialog-fullscreen "Expand dialog to full height" ("ctrl+f"))
    (:page-up "Scroll transcript up" ("pgup"))
    (:page-down "Scroll transcript down" ("pgdn"))
    (:scroll-home "Jump to transcript top" ("home"))
    (:scroll-end "Jump to transcript bottom" ("end"))
    (:help "Open keyboard help" ("?"))
    ;; Ctrl+P on an empty composer inserts "/" — the command palette IS the
    ;; slash dropdown under another binding (Zig openComposerCommandPalette).
    (:open-command-palette "Open command palette" ("ctrl+p"))
    ;; Zig's fixed_image_paste_shortcut_label row folds in here: its keys are
    ;; a real action for the lisp shells, so it needs no separate fixed row.
    (:paste-image "Paste image from clipboard" ("ctrl+v" "ctrl+alt+v"))
    (:copy-selection "Copy selection" ("ctrl+shift+c"))
    ;; Ctrl+Y marks the transcript from the keyboard (TRANSCRIPT-SELECT-KEY):
    ;; y is the copy key of vi, tmux's copy mode and Codex's select mode, and
    ;; the chord is free in both key ladders and in the editor, which keeps
    ;; no kill ring for readline's yank to paste from.
    (:select-transcript "Select transcript text with the keyboard" ("ctrl+y")))
  "The declarative default keymap: (ACTION LABEL DEFAULT-CHORD-STRINGS).")

(define-startup-parameter *keybinds-path*
  (home "keybinds.json")
  "Where user keybind overrides live: a JSON object of action name (snake
case, e.g. \"toggle_viewport\") to one config chord string (the Zig
keybinds config shape). NIL disables overrides; tests bind.")

(defun load-keybind-overrides ()
  "The persisted override document as an alist (ACTION . CHORD-STRING),
or NIL — the document keys actions by their snake-case names."
  ;; Unreadable state is ignored: keybinds are convenience, never worth
  ;; refusing the shell over.
  (when *keybinds-path*
    (with-handlers ((error () nil))
      (let ((document (nlk:decode-json (nlk:read-text *keybinds-path*))))
        (loop for (action) in *keybind-actions*
              for value = (json-value document :string
                                      (substitute #\_ #\- (string-downcase (symbol-name action))))
              when value
                collect (cons action value))))))

;;; It is how a reader tells whether a resolved keymap still speaks for the
;;; action table now in force, which is what makes a keybind row defined into
;;; a running image reach the keyboard at the next keystroke instead of a
;;; relaunch (REFRESH-MODEL-KEYMAP).
(defparameter +keybind-source-key+ :keybind-source
  "The entry RESOLVE-KEYBINDS files its own source table under inside the
keymap it answers — never an action name, so no reader of a chord can meet
it.")

(defun resolve-keybinds (&optional (overrides (load-keybind-overrides)))
  "The resolved keymap: ACTION -> list of KEY-CHORD."
  ;; An override replaces every default chord for its action with the one
  ;; configured spec; "none" unbinds the action; a malformed spec keeps the
  ;; defaults (the Zig fromConfigEntries skip-on-parse-failure rule). The
  ;; table it was built from rides along under +KEYBIND-SOURCE-KEY+.
  (let ((keymap (make-hash-table :test #'eq)))
    (loop for (action nil defaults) in *keybind-actions*
          do (setf (gethash action keymap)
                   (remove-if-not #'key-chord-p (mapcar #'parse-key-chord defaults))))
    (loop for (action . spec) in overrides
          for chord = (parse-key-chord spec)
          do (cond ((eq chord :none) (setf (gethash action keymap) '()))
                   ((key-chord-p chord)
                    (setf (gethash action keymap) (list chord)))))
    (setf (gethash +keybind-source-key+ keymap) *keybind-actions*)
    keymap))

;;; The chord table above is written against the decoder the Zig TUI reads
;;; through (vaxis Parser.parseGround) and the core's decoder agrees with
;;; it: 0x09 arrives as Tab and 0x0D as Enter with no modifiers (a legacy
;;; terminal cannot say Ctrl+Enter, so the ctrl bit on those bytes would be
;;; an encoding artifact), 0x08 keeps its ctrl bit (a real gesture, distinct
;;; from DEL), and 0x0A is Ctrl+J — Enter sends CR in raw mode, so an LF can
;;; only be that chord, which is the whole point of the "ctrl+j" default.
;;; Where the terminal honours SHELL-VIEW's kitty disambiguation (CSI >1u)
;;; every chord arrives fully modified; a kitty-reported Ctrl+Enter keeps
;;; its ctrl bit and matches no default.

(defun keybind-matches-p (keymap action code mod)
  "Whether the pressed key dispatches ACTION under KEYMAP."
  (flet ((agrees (bit flag) (eq (not (nlt:mod-contains mod bit)) (not flag))))
    (some (lambda (chord &aux (spec-code chord.code) (shifted (agrees nlt:+mod-shift+ chord.shift)))
            (and (agrees nlt:+mod-ctrl+ chord.ctrl)
                 (agrees nlt:+mod-alt+ chord.alt)
                 ;; Zig KeySpec.matches: a letter's case stands in for shift either way.
                 (if (characterp spec-code)
                     (and (characterp code) (char-equal spec-code code)
                          (or (not (alpha-char-p code)) (upper-case-p code) shifted))
                     (and (eq spec-code code) shifted))))
          (gethash action keymap))))

(defun key-chord-label (chord &aux (code chord.code)
                                   (row (assoc code +key-names+)))
  "The display label (Zig writeKeySpecLabel): Ctrl+Alt+Shift prefixes then
the base key name."
  (format nil "~:[~;Ctrl+~]~:[~;Alt+~]~:[~;Shift+~]~a"
          chord.ctrl chord.alt chord.shift
          (cond (row (second row))
                ((characterp code) (string (char-upcase code)))
                (t (string-capitalize (symbol-name code))))))

(defun keybind-chords-label (keymap action &key (unbound "Unbound"))
  "Every bound chord for ACTION joined with \" / \" — the help dialog's key
column."
  ;; UNBOUND is what an action with no chord answers; NIL asks for a
  ;; deliberately empty answer instead, which is what a surface that shows a
  ;; chord only when there is one (the / dropdown's hotkey lane) means by a
  ;; command 'having' a hotkey.
  (if-let (chords (gethash action keymap))
    (format nil "~{~a~^ / ~}" (mapcar #'key-chord-label chords))
    unbound))

;;; --- workspace file catalog (file_catalog.zig) -----------------------------

(defun file-catalog-skip-name-p (name)
  "The Zig shouldSkipName set: hidden entries and the fixed build/vendor
directories. Zig walks no .gitignore either — the skip list is the filter."
  (or (zerop (length name))
      (char= (char name 0) #\.)
      (member name '("bazel-bin" "bazel-out" "bazel-testlogs" "node_modules"
                     "target" "zig-cache" "zig-out")
              :test #'string=)))

(defun workspace-file-catalog (root &aux (entries '()) (count 0) (max-depth 10) (max-entries 512))
  "Relative paths of the workspace files @-mentions complete against:
recursive walk under ROOT, hidden and vendored names skipped, depth and
entry bounded, sorted case-insensitively (Zig loadWorkspaceFileCatalog)."
  ;; RELATIVE is NIL at the root.
  (labels ((walk (dir relative depth)
             ;; Files first, then the folders under DIR, each named from the root.
             (dolist (path (append (ignore-errors (uiop:directory-files dir))
                                   (and (< depth max-depth)
                                        (ignore-errors (uiop:subdirectories dir)))))
               (let* ((name (nlk:folder-name path))
                      (entry (format nil "~@[~a/~]~a" relative name)))
                 (cond ((>= count max-entries) (return-from walk))
                       ((file-catalog-skip-name-p name))
                       ((uiop:directory-pathname-p path) (walk path entry (1+ depth)))
                       (t (push entry entries) (incf count)))))))
    (walk (uiop:ensure-directory-pathname root) nil 0))
  (sort entries #'string-lessp))

;;; --- SKILL.md frontmatter (skill_catalog.zig) ------------------------------
;;; The import cell reads another harness's SKILL.md name and description
;;; through this one parser.

(defun frontmatter-block (text &aux (length (length text)))
  "(LINES . BODY): the lines between TEXT's leading `---' line and the next
one, each without the CR before its LF, and the text after that line; NIL
when TEXT opens with no such block."
  ;; Only the block is split: without this, the whole text would be split twice
  ;; for every memory file of a home to learn whether it opened with one.
  (flet ((line-end (start) (or (position #\Newline text :start start) length))
         (line (start end)
           (subseq text start (if (and (< end length) (> end start)
                                       (char= #\Return (char text (1- end))))
                                  (1- end)
                                  end))))
    (let ((opening (line-end 0)))
      (when (and (< opening length) (string= "---" (nlk:trimmed (line 0 opening))))
        (loop with lines = '()
              for start = (1+ opening) then (1+ end)
              for end = (line-end start)
              for line = (line start end)
              when (string= "---" (nlk:trimmed line))
                return (cons (nreverse lines) (subseq text (min length (1+ end))))
              do (push line lines)
              until (= end length))))))

(defun frontmatter-field (lines dotted-key)
  "The raw value of DOTTED-KEY (\"name\" or \"metadata.short-description\")
across LINES, with `#` comments stripped and one level of section nesting
tracked (Zig frontmatterField); a block scalar value reads as its joined
lines."
  (let* ((dot (position #\. dotted-key))
         (want-section (and dot (subseq dotted-key 0 dot)))
         (want-key (if dot (subseq dotted-key (1+ dot)) dotted-key))
         (section ""))
    (loop for (raw . rest) on lines
          for line = (nlk:trimmed (subseq raw 0 (or (position #\# raw) (length raw))))
          for colon = (position #\: line)
          do (cond
               ((zerop (length line)))
               ((char= (char line (1- (length line))) #\:)
                (setf section (nlk:trimmed (subseq line 0 (1- (length line))))))
               ((and colon
                     (string-equal (string-trim '(#\Space #\Tab) (subseq line 0 colon)) want-key)
                     (or (null want-section)
                         (string-equal section want-section)))
                (let* ((value (string-trim '(#\Space #\Tab #\" #\') (subseq line (1+ colon))))
                       ;; A YAML block scalar: the lines indented under its key, space-joined.
                       (text (if (member value '("|" ">" "|-" ">-" "|+" ">+") :test #'string=)
                                 (format nil "~{~a~^ ~}"
                                         (loop for next in rest
                                               for trimmed = (nlk:trimmed next)
                                               while (or (zerop (length trimmed))
                                                         (member (char next 0)
                                                                 '(#\Space #\Tab)))
                                               when (plusp (length trimmed)) collect trimmed))
                                 value)))
                  (when (plusp (length text))
                    (return text))))))))

(defun parse-skill-frontmatter (text)
  "=> (values NAME DESCRIPTION) from SKILL.md frontmatter, or NIL."
  ;; The description resolves through the Zig fallback chain.
  (when-let (lines (car (frontmatter-block text)))
    (let ((name (one-line (frontmatter-field lines "name")))
          (description
            (one-line
             (or (frontmatter-field lines "metadata.short-description")
                 (frontmatter-field lines "short-description")
                 (frontmatter-field lines "description")))))
      (when (and (<= 1 (length name) 64) (<= 1 (length description) 1024))
        (values name description)))))

;;; --- paste normalization (composer.zig) ------------------------------------

(defparameter *paste-collapse-max-lines* 10
  "Zig composer_paste_collapse_max_lines.")

(defparameter *paste-collapse-max-chars* 1000
  "Zig composer_paste_collapse_max_chars.")

(defun normalize-pasted-text (text)
  "CR and CRLF collapse to LF; outer blanks trim (Zig normalizePastedText);
every other control codepoint becomes its Control Picture (a recorded
delta from Zig)."
  ;; The paste is the one composer ingress that can carry a control, and the
  ;; editor wraps at the renderer's widths — an ESC sequence at zero columns —
  ;; while the composer row paints through the display authority at one column
  ;; per control, so a raw control would overflow the box on a full line.
  ;; Pictured here, it wraps, paints, and reaches the model as the one visible
  ;; thing the operator saw in the box.
  (string-trim '(#\Space #\Tab #\Newline)
               (map 'string (lambda (ch) (if (char= ch #\Newline) ch (display-glyph ch)))
                    (ppcre:regex-replace-all "\\r\\n?" text (string #\Newline)))))
