;;;; theme-test.lisp --- design-system gate: palette, profile, wrap, gutters.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Gate claims, each pinned to the Zig authority the value was ported from:
;;;;
;;;;   T1. The default palette carries the Zig VisualPalette values verbatim
;;;;       (src/tui/render_plan.zig:1264-1317).
;;;;   T2. The 256/16-color quantizers match the Zig math
;;;;       (src/tui/design_system.zig rgbToAnsi256 / rgbToAnsi16Index).
;;;;   T3. Dimming blends toward the palette background under the 4.50:1
;;;;       contrast floor instead of emitting SGR 2 (design_system.zig dimStyle).
;;;;       The shadow halves every channel of the foreground, keeps the hue,
;;;;       and leaves the background alone (design_system.zig shadowStyle).
;;;;   T4. BREAK-FRAGMENTS obeys the wrap.zig LineBreaker invariants: word
;;;;       policy rewinds to the last space, a lone over-wide glyph still
;;;;       emits, empty text costs one row, wide glyphs count two columns.
;;;;   T5. Gutter geometry matches body_layout.zig, including the diagnostic
;;;;       mark whose continuations wrap back to the system indent.
;;;;   T6. The theme catalog resolves the shared bundled document plus the
;;;;       user overlay and paints the palette through the Zig choose chains
;;;;       (config.zig theme layer + render_plan.zig
;;;;       visualPaletteFromThemeColors), so the organism boots into
;;;;       Tungstene exactly as the Zig TUI does.

(in-package #:nodecode.test)

(nlk:access (bare nle::visual-style) (code nle::visual-style) (shadowed nle::visual-style)
            (token nle::syntax-token))

;;; --- T1: palette values ---------------------------------------------------

(deftest palette-defaults-match-zig ()
  (flet ((fg (tone) (nle::visual-style-fg (nle:style-for-tone tone)))
         (bg (tone) (nle::visual-style-bg (nle:style-for-tone tone))))
    (is (equal '(28 31 38) (bg :background)) "background void #1C1F26")
    (is-table (in want) (equal want (fg in)) (:foreground '(235 238 244) "foreground #EBEEF4")
      (:session-title '(246 192 117) "session title amber")
      (:assistant '(130 214 176) "assistant green") (:user '(132 178 255) "user blue")
      (:tool '(228 184 96) "tool amber") (:danger '(255 126 126) "danger red"))
    (is (equal '(61 65 74) (bg :user-block)) "user block promotes the former hover surface")
    (is (equal '(87 91 100) (bg :user-block-hover)) "hover keeps another lift of contrast")
    (is (equal '(76 42 41) (bg :diagnostic-block)) "diagnostic wash")
    (is (equal '(127 176 215) (fg :inline-code)) "inline code takes the answer's cool hue")
    (is (equal '(253 179 118) (fg :inline-strong)) "strong takes the answer's warm accent")
    (is (nle::visual-style-bold (nle:style-for-tone :inline-strong)))
    (is (nle::visual-style-bold (nle:style-for-tone :syntax-keyword)))
    (is (eq (nle:style-for-tone :foreground) (nle:style-for-tone :no-such-tone)))))

;;; --- T2: quantizers --------------------------------------------------------

(deftest quantizers-match-zig-math ()
  (is (= 16 (nle:rgb-to-ansi256 '(0 0 0))) "black hits the cube origin")
  (is (= 231 (nle:rgb-to-ansi256 '(255 255 255))) "white hits the cube top")
  ;; (95 135 175) is an exact cube color: indices 1,2,3 -> 16+36+12+3.
  (is (= 67 (nle:rgb-to-ansi256 '(95 135 175))) "exact cube colors round-trip")
  (is-table (in want) (= want (nle:rgb-to-ansi16 in)) ('(0 0 0) 0 "black -> ansi 0")
    ('(255 0 0) 9 "pure red -> bright red 9") ('(229 229 229) 7 "light gray -> ansi 7")))

;;; --- T3: the dim primitive -------------------------------------------------

(deftest dim-blends-under-a-contrast-floor ()
  (let* ((bg (nle::visual-style-bg (nle:style-for-tone :background)))
         (fg (nle::visual-style-fg (nle:style-for-tone :foreground)))
         (dimmed (nle::dim-color fg bg)))
    (is (not (equal fg dimmed)) "dimming visibly de-emphasizes")
    (is (>= (nle::contrast-ratio-x100 dimmed bg) 450))
    (is (< (nle::contrast-ratio-x100 dimmed bg)
           (nle::contrast-ratio-x100 fg bg))))
  (is (not (nle::visual-style-dim (nle:dim-style (nle:style-for-tone :muted))))))

(deftest shadow-dims-the-text-and-leaves-the-canvas ()
  ;; The Zig shadowStyle: an overlay on the text, not a tone and not a fill.
  ;; The foreground drops to *SHADOW-PERCENT* of its light — a style without
  ;; one resolves it from the palette first — and the background, own or
  ;; absent, is exactly what it was: a shadowed block is quieter text on the
  ;; same surface, never a darker band. The hue survives, which is what tells
  ;; a shadowed inline code span from a muted one.
  (is (equal '(159 161 165) (nle::shadow-rgb '(235 238 244))))
  (is (null (nle::shadow-rgb nil)) "no color, no shadow")
  (let* ((code (nle:style-for-tone :inline-code))
         (shadowed (nle:shadow-style code)))
    (is-shape shadowed (.fg (nle::shadow-rgb code.fg)) (.bg code.bg) (.dim not)))
  (let ((bare (nle:shadow-style (nle::vstyle))))
    (is (equal (nle::shadow-rgb (nle::visual-style-fg (nle:style-for-tone :foreground))) bare.fg))
    (is (null bare.bg)))
  (is (equal (nle::visual-style-bg (nle:style-for-tone :user-block))
             (nle::visual-style-bg (nle:shadow-style (nle:style-for-tone :user-block))))))

;;; --- T4: the line breaker --------------------------------------------------

(deftest break-fragments-honors-word-wrap-invariants ()
  (is-table (text lead width frags) (equal 'frags (nle:break-fragments text lead width))
    ("" 10 10 ((0 . 0)) "empty text still costs one row")
    ("beta gamma delta" 8 8 ((0 . 5) (5 . 11) (11 . 16))
     "a word the row cannot hold moves whole to the next row")
    ("alpha beta gamma" 10 10 ((0 . 6) (6 . 16))
     "the break lands just past the last bare space")
    ("abcdefghijkl" 5 5 ((0 . 5) (5 . 10) (10 . 12))
     "a word wider than every row falls back to a hard break")
    ("日本語" 4 4 ((0 . 2) (2 . 3)) "wide glyphs count two columns")
    ("語" 1 1 ((0 . 1)) "one glyph wider than the row still emits alone"))
  (is-let* ((frags (nle:break-fragments "one two three four five" 10 6))) ((first frags) '(0 . 8))
    ((every (lambda (f) (<= (- (cdr f) (car f)) 6)) (rest frags)) is)))

;;; --- T5: gutter geometry ---------------------------------------------------

(deftest body-gutters-land-on-the-rails ()
  ;; Each row is the role, then (lead mark first-indent continuation-indent),
  ;; inside the band's two-column inset: marks on the gutter (1), words on the
  ;; label lane (3), a queued prompt's hook under the live line's words (3).
  (is-table (role gutter) (equal 'gutter (multiple-value-list (nle:body-gutter role)))
    (:prose ("   " "" 3 3) "prose: three columns, no mark")
    (:system (" " "■ " 3 3) "notice: the mark on the gutter, the words on the label lane")
    (:diagnostic (" " "■ Error: " 10 3)
     "diagnostic: wide mark, continuations wrap to the system indent")
    (:disclosure (" " "▸ " 3 3) "a shut group's mark on the gutter")
    (:disclosure-open (" " "▾ " 3 3) "an open group's mark on the gutter")
    (:steer-band ("   " "⌎ " 5 5) "a queued prompt's words two in under the live line's")
    (:tool ("   " "⎿ " 5 5) "tool: branch mark at column five")
    (:no-such-role ("   " "" 3 3) "unknown roles fall back to prose")))

;;; --- render parity: row chrome under :none ---------------------------------
;;; The display policy itself, asserted as plain text (RUN-ALL pins the
;;; profile), so these are the Lisp goldens for the Zig row-chrome table.

(deftest tool-output-collapses-to-the-five-row-window ()
  ;; The card's output shows in the Developer view; Standard folds the call.
  (let* ((nle::*tool-detail* :developer)
         (state (nle::ingest-tool-call-fact nil "c1" "shell" "ls -la"))
         (state (nle::ingest-tool-result-fact
                 state "c1" "shell"
                 (format nil "~{line~d~^~%~}" '(1 2 3 4 5 6 7 8 9 10 11 12)))))
    (let ((lines (butlast (rest (nle::display-block-plain-lines (first (nle:project-state state)))))))
      (is (= 6 (length lines)))
      (is (search "shell" (first lines)) "header names the tool")
      (is (search "ls -la" (first lines)) "header carries the arguments")
      (is-shape lines (second "   ⎿ line1") (third "     line2") (fourth "   ⋮ +8 lines (click to expand)")
        (sixth "     line12")))))

(deftest failed-turn-renders-diagnostic-chrome ()
  (let* ((state (nle::ingest-turn-terminal nil "failed"
                                           :turn-id "t1" :detail "boom"))
         (block (first (nle:project-state state))))
    (is (eq :system-notice block.kind))
    (is (search "■ Error: turn failed: boom" (first (block-body-lines block))))))

(deftest assistant-markdown-styles-classify ()
  (let* ((state (nle::ingest-message-fact
                 nil "assistant"
                 (format nil "# Title~%- item one~%`code` span")))
         (lines (mapcar (lambda (line) (string-right-trim " " line))
                        (block-words (first (nle:project-state state))))))
    ;; The clock stands on its own row over a heading's band; the heading's
    ;; edge is in the gutter, a bullet a dot, a code span its words.
    (is-shape lines (second " ▌ Title") (third "   • item one") (fourth "   code span"))))

(deftest long-prose-wraps-with-the-prose-gutter ()
  (let* ((text (format nil "~{word~a~^ ~}" '(1 2 3 4 5 6 7 8 9 10)))
         (state (nle::ingest-message-fact nil "assistant" text))
         (lines (block-body-lines (first (nle::project-state state :width 24)))))
    (is (> (length lines) 1) "narrow frames wrap prose")
    (is (every (lambda (line) (<= (nle::text-display-width line) 23))
               lines))
    (is (every (lambda (line) (eql 0 (search "   " line))) lines))))

;;; --- scrollbar geometry (src/tui/transcript.zig) ----------------------------
;;; The Zig math verbatim: the gutter is always painted when the window can
;;; carry it, the bar sits at width - 3, the thumb is at least one row and
;;; fills the track when the content fits.

(deftest scrollbar-geometry-matches-zig ()
  ;; Each row is the thumb's arguments, then (top height).
  (is-table (arguments thumb) (equal 'thumb (multiple-value-list (apply #'nle:scrollbar-thumb 'arguments)))
    ((10 10 10 0) (0 10) "content that fits fills the whole track")
    ((10 10 1000 0) (0 1) "a deep transcript keeps the one-row minimum thumb at the top")
    ((10 10 1000 990) (9 1) "scrolled to the bottom, the thumb rests on the last track row")
    ((10 10 20 5) (2 5) "half-visible content: thumb half the track, position scaled")
    ((0 10 20 5) (0 0) "a zero track paints no thumb")))

;;; The live bar's text starts at a fixed column, so every mark is two braille
;;; cells; and every kind a verb can carry -- each activity family, the model's
;;; own two, and the pulse -- has one.

(deftest every-verb-kind-has-a-two-cell-mark ()
  (dolist (kind (list* :default :thinking :streaming (mapcar #'car nle::*activity-families*)))
    (let ((frames (cdr (assoc kind nle::*status-marks*))))
      (is (plusp (length frames)) (format nil "~(~a~) has a mark" kind))
      (loop for frame across frames
            do (is (and (= 2 (length frame))
                        (every (lambda (char) (<= #x2800 (char-code char) #x28FF)) frame))
                   (format nil "~(~a~) frame ~s is two braille cells" kind frame))))))

;;; --- T6: theme catalog + Tungstene overlay ---------------------------------
;;; Pinned to src/config.zig (document grammar, key mapping, derivations) and
;;; src/tui/render_plan.zig visualPaletteFromThemeColors (the choose chains).
;;; The Zig TUI renders Tungstene, not the compiled defaults, so this layer is
;;; what makes the two organisms actually paint the same frame.

(defun theme-palette (name &rest documents)
  "The palette NAME's colors paint, DOCUMENTS the THEME-COLORS-FOR-NAME keys."
  (nle:palette-from-theme-colors (apply #'nle:theme-colors-for-name name documents)))

(defun colored-palette (name layer &rest colors)
  "The palette NAME paints when the LAYER document (:BUNDLED or :USER) gives it
COLORS, alternating JSON keys and values."
  (let ((theme (nlk:json-object "name" name "colors" (apply #'nlk:make-json-object colors))))
    (theme-palette name layer (nlk:encode-json-object (nlk:json-object "themes" (vector theme))))))

(deftest bundled-catalog-lists-the-zig-themes ()
  (is (equal '("Tungstene" "Hello Kitty Lo-Fi" "Komorebi" "Blueprint Daylight")
             (nle:theme-catalog :user nil))))

(deftest tungstene-inherits-terminal-background-and-keeps-other-tones ()
  (let* ((transparent (theme-palette "Tungstene"))
         (opaque (theme-palette "Tungstene" :user
                  "{\"themes\":[{\"name\":\"Tungstene\",\"transparent-background\":false}]}"))
         (nle:*palette* transparent)
         (nle:*color-profile* :truecolor))
    (is (null (nle::canvas-fill-bg)) "Tungstene inherits the terminal canvas by default")
    (is (equal '(12 10 9) (nle::visual-style-bg (gethash :background opaque))) "explicit false restores the nominal canvas")
    (loop for tone being the hash-keys of transparent
          for actual = (gethash tone transparent)
          for expected = (copy-structure (gethash tone opaque))
          do (when (eq tone :background) (setf (nle::visual-style-bg expected) nil))
             (is (equalp actual expected) (format nil "only the canvas changes: ~a" tone)))
    (let ((prose (nle::seg-render (nle::make-seg :text "body") (nle::canvas-fill-bg)))
          (panel (nle::seg-render (nle::make-seg :text "panel" :tone :composer) (nle::canvas-fill-bg))))
      (is (not (search "48;" prose)) "body has no explicit background SGR")
      (is (search "48;2;59;57;55" panel) "panels retain their existing RGB background")))
  (dolist (name '("Hello Kitty Lo-Fi" "Komorebi" "Blueprint Daylight"))
    (is (nle::visual-style-bg (gethash :background (theme-palette name))) "other themes retain their canvas")))

(deftest transparent-background-retains-full-panel-fills (with-temp-store ())
  (let* ((nle:*palette* (theme-palette "Tungstene"))
         (model (make-frame-model 80 30)))
    (dolist (profile '(:truecolor :ansi256 :ansi :none))
      (let ((nle:*color-profile* profile))
        (is (null (nle::canvas-fill-bg)) "transparent canvas in every profile")
        (let* ((rows (nle::composer-rows model 80 30 nil))
               (cells (nlt::screen-buffer-cells
                        (nlt:parse-styled-string rows 80 (length rows)))))
          (is (null (nlt::cell-bg (aref cells 0))) "composer's outer margin inherits the terminal")
          (if (eq profile :none)
              (is (every (lambda (cell) (null (nlt::cell-bg cell))) cells) "colorless rows stay colorless")
              (is (equal (nle::color-sgr '(59 57 55) nil profile)
                         (nlt::cell-bg (aref cells 2))) "composer pad keeps its full fill")))
        (is (equal (unless (eq profile :none) '(59 57 55))
                   (nle::docked-panel-surface-bg)) "dialog fill follows profile, not canvas")))
    (let ((nle:*color-profile* :truecolor))
      (nle::editor-insert (nle::chat-model-editor model) "/")
      (let* ((rows (nle::composer-dropdown-panel-rows model 80 nil))
             (cells (nlt::screen-buffer-cells
                      (nlt:parse-styled-string rows 80 (length rows)))))
        (is rows "slash dropdown is open")
        (is (equal "48;2;59;57;55" (nlt::cell-bg (aref cells 2))) "dropdown's pad retains the user-block fill")))))

(deftest transparent-background-policy-is-boolean-and-overlayable ()
  (dolist (bad '("null" "1" "\"true\""))
    (signals-error error
      (nle:theme-colors-for-name "C" :bundled
       (format nil "{\"themes\":[{\"name\":\"C\",\"transparent-background\":~a}]}" bad))))
  (let* ((doc "{\"themes\":[{\"name\":\"C\",\"transparent-background\":true,\"colors\":{\"background-void\":\"#112233\"}}]}")
         (palette (theme-palette "C" :bundled doc)))
    (is (null (nle::visual-style-bg (gethash :background palette))) "custom themes can inherit terminal background")
    (is (equal '(17 34 51)
               (nle::visual-style-bg (gethash :background
                 (theme-palette "C" :bundled doc :user
                  "{\"themes\":[{\"name\":\"C\",\"transparent-background\":false}]}")))) "false overrides true without losing nominal RGB"))
  (let ((nle:*palette* (theme-palette "Komorebi")))
    (nle:engage-theme :name "Tungstene" :user-document nil)
    (is (null (nle::canvas-fill-bg)) "switching from opaque to transparent removes canvas")
    (nle:engage-theme :name "Komorebi" :user-document nil)
    (is (equal '(5 10 5) (nle::visual-style-bg (nle:style-for-tone :background))) "switching back restores canvas")))

(deftest tungstene-overlay-matches-the-zig-choose-chains ()
  (let ((palette (theme-palette "Tungstene")))
    ;; Each row is the style part read, the tone, and the rgb it must carry.
    (is-table (part tone rgb) (equal 'rgb (part (gethash tone palette)))
      (nle::visual-style-bg :background nil "the frame inherits the terminal background")
      (nle::visual-style-fg :foreground (231 229 228) "foreground #E7E5E4")
      (nle::visual-style-fg :title (250 250 249) "title takes primary")
      (nle::visual-style-fg :session-title (252 168 100) "session title takes accent1")
      (nle::visual-style-fg :muted (170 165 162) "muted clears 4.5:1 on the brighter panels")
      (nle::visual-style-fg :assistant (85 191 163) "assistant takes success")
      (nle::visual-style-fg :user (251 146 60) "user takes user-input")
      (nle::visual-style-fg :tool (252 168 100) "tool takes syntax-string")
      (nle::visual-style-fg :danger (235 139 138) "danger clears 4.5:1 on the brighter panels")
      (nle::visual-style-bg :composer (59 57 55) "composer takes the brighter background-input")
      (nle::visual-style-fg :dialog-accent (251 146 60) "the docked list's hover accent takes user-input")
      (nle::visual-style-bg :user-block (59 57 55) "user block promotes the former hover surface")
      (nle::visual-style-bg :section (16 14 13) "section takes the theme's explicit background-chrome")
      (nle::visual-style-fg :approval (253 186 116) "approval takes info")
      (nle::visual-style-fg :inline-code (110 171 221) "inline code takes the answer's cool hue")
      (nle::visual-style-fg :inline-strong (253 171 104) "strong takes the answer's warm accent")
      (nle::visual-style-fg :syntax-command (253 186 116) "shell command words take syntax-type"))
    (is (nle::visual-style-bold (gethash :title palette)))
    (is (nle::visual-style-dim (gethash :steer-preview palette)))
    (is (nle::visual-style-bold (gethash :inline-strong palette)))))

(deftest tungstene-success-is-teal-without-changing-other-accents ()
  (let* ((colors (nle:theme-colors-for-name "Tungstene"))
         (palette (nle:palette-from-theme-colors colors))
         (nle:*palette* palette)
         (nle:*color-profile* :truecolor))
    (is (equal '(85 191 163) (gethash :success colors)) "success is the approved #55BFA3 teal")
    (dolist (tone '(:success :assistant :diff-added :diff-added-emphasis))
      (is (equal '(85 191 163) (nle::visual-style-fg (gethash tone palette))) "success consumers share the approved teal"))
    (is (equal '(250 139 62) (gethash :accent2 colors)) "orange stays unchanged")
    (is (equal '(124 201 16) (gethash :accent4 colors)) "accent4 stays unchanged")
    (is (equal '(101 163 13) (gethash :syntax-builtin colors)) "syntax builtin stays unchanged")
    (is (search "38;2;85;191;163" (nle::seg-render (nle::make-seg :text "success" :tone :success) nil)) "success paint emits the approved truecolor SGR")))

(deftest tungstene-derives-inset-contrast-and-blends-washes ()
  (let ((palette (theme-palette "Tungstene")))
    (is-table (rgb tone) (equal rgb (nle::visual-style-bg (gethash tone palette)))
      ('(24 21 19) :approval)
      ('(77 43 42) :diagnostic-block))))

(deftest derived-chrome-fills-in-when-the-theme-omits-it ()
  (let ((palette (colored-palette "Derived" :bundled
                                  "background-void" "#000000" "background-canvas" "#101010")))
    (is (equal '(4 4 4) (nle::visual-style-bg (gethash :section palette))))
    (is (equal '(12 12 12) (nle::visual-style-bg (gethash :approval palette))))))

(deftest a-sparse-theme-keeps-the-compiled-defaults ()
  (let ((palette (colored-palette "Sparse" :bundled "foreground" "#102030")))
    (is-table (rgb part tone) (equal rgb (part (gethash tone palette)))
      ('(16 32 48) nle::visual-style-fg :foreground)
      ('(228 184 96) nle::visual-style-fg :tool)
      ('(28 31 38) nle::visual-style-bg :background))))

(deftest the-user-document-overlays-the-bundled-theme ()
  (let ((palette (colored-palette "Tungstene" :user "accent1" #(1 2 3))))
    (is-table (rgb tone) (equal rgb (nle::visual-style-fg (gethash tone palette)))
      ('(1 2 3) :session-title)
      ('(85 191 163) :assistant))))

(deftest theme-documents-tolerate-jsonc-and-reject-the-unknown ()
  (let ((document (format nil "{~%  // organism themes~%  \"themes\": [~%    ~
                                {\"name\": \"C\", /* one */ \"colors\": ~
                                {\"foreground\": \"#ffffff\", \"nonsense\": ~
                                \"#000000\",}},~%  ],~%}")))
    (is-let* ((colors (nle:theme-colors-for-name "C" :bundled document)))
      ((gethash :foreground colors) '(255 255 255)) ((gethash :nonsense colors) null))
    (signals-error error
      (nle:theme-colors-for-name "NoSuchTheme" :bundled document))
    (signals-error error
      (nle:theme-colors-for-name
       "C" :bundled "{\"themes\":[{\"name\":\"C\",\"colors\":{\"foreground\":\"fff\"}}]}"))))

(deftest configured-theme-name-reads-general-theme ()
  (is-table (name document) (equal name (nle:configured-theme-name document))
    ("Komorebi" "{\"general\": {\"theme\": \"Komorebi\"}} // jsonc"
     "general.theme is the selection authority")
    ("Tungstene" "not json at all" "an unreadable config falls back to the schema default")
    ("Tungstene" "{\"general\": {}}" "a config without a theme falls back to the schema default")))

(deftest engage-theme-installs-and-empty-name-keeps-defaults ()
  (let ((nle:*palette* (nle:default-palette)))
    (nle:engage-theme :name "Tungstene" :user-document nil)
    (is-table (rgb part tone) (equal rgb (part (nle:style-for-tone tone)))
      ('(252 168 100) nle::visual-style-fg :session-title)
      ('nil nle::visual-style-bg :background)))
  (let ((nle:*palette* (nle:default-palette)))
    (is (eq nle:*palette* (nle:engage-theme :name "")))))

;;; --- T7: syntax highlighting (src/tui/syntax_highlight.zig) ----------------
;;; The Zig file's own tests, ported with their offsets (ASCII, so byte and
;;; character positions agree), plus the language-gated comment rules and the
;;; token->tone map the transcript paints with.

(defun token-kind-at (tokens offset)
  "The kind covering OFFSET, or :plain — the Zig testKindAt helper."
  (loop for token in tokens
        when (and (>= offset token.start) (< offset token.end))
          return token.kind
        finally (return :plain)))

(deftests shell-tokenizer (text kinds)
  (let ((tokens (nle:tokenize-shell text)))
    (loop for (kind offset why) in kinds
          do (is (eq kind (token-kind-at tokens offset)) why)))
  (colors-pipeline-commands-and-flags "rg -n 'foo bar' src | head -5 && echo done"
   '((:command 0 "rg is the command") (:flag 3 "-n is a flag") (:string 6 "'foo bar' is a string")
     (:plain 16 "src is a plain argument") (:operator 20 "| is an operator")
     (:command 22 "head follows the pipe as a command") (:flag 27 "-5 is a flag")
     (:operator 30 "&& is an operator") (:command 33 "echo follows && as a command")
     ;; done is in the shell keyword set: the tokenizer is lexical, not
     ;; grammatical (the Zig comment, kept).
     (:keyword 38 "done colors as a keyword")))
  (env-prefix-variables-and-redirects "FOO=bar cmd $HOME ${X}y 2>&1 > out.txt"
   '((:variable 0 "FOO assignment name") (:operator 3 "= assignment operator")
     (:plain 4 "assigned value stays plain") (:command 8 "cmd is still the command")
     (:variable 12 "$HOME") (:variable 18 "${X}") (:plain 22 "y after ${X} stays plain")
     (:operator 24 "2>&1") (:operator 29 ">") (:plain 31 "out.txt stays plain")))
  (command-substitution-and-comment "echo $(date) # note"
   '((:command 0 "echo") (:operator 5 "$( opener") (:command 7 "date inside the substitution")
     (:punctuation 11 "closing paren") (:comment 13 "# note")
     (:comment 18 "comment runs to the end")))
  (quote-spans-newline-and-separator-restores-command (format nil "echo 'a~%b' ; ls")
   '((:string 5 "opening quote") (:string 7 "newline inside the quote") (:string 9 "closing quote")
     (:operator 11 "; separator") (:command 13 "ls after ; is a command")))
  (prefix-words-keep-command-position "sudo env rg pattern"
   '((:command 0 "sudo") (:command 5 "env") (:command 9 "rg")
     (:plain 12 "pattern is an argument"))))

(deftest line-tokenizer-classifies-code-and-gates-comments-by-language ()
  (let ((tokens (nle:tokenize-line "const x = try foo(42); // done" "zig")))
    (is-table (kind offset) (eq kind (token-kind-at tokens offset))
      (:keyword 0 "const") (:plain 6 "x") (:operator 8 "=") (:keyword 10 "try")
      (:function 14 "foo before ( is a function") (:number 18 "42") (:comment 23 "// comment")))
  ;; Each row is the kind at an offset of one line tokenized under a language.
  (is-table (kind line language offset) (eq kind (token-kind-at (nle:tokenize-line line language) offset))
    (:comment "# hi" "python" 0 "# opens a comment in hash-comment languages")
    (:plain "# hi" "zig" 0 "# is not a comment outside hash-comment languages")
    (:comment "-- drop" "sql" 0 "-- opens a comment in sql-like languages")
    (:string "x = \"a\\\"b\"" "python" 4 "escaped quotes stay inside the string")
    (:constant "return nil" "js" 7 "nil is a constant")
    (:type "let x: Widget" "ts" 7 "capitalized identifiers read as types")
    (:builtin "print(1)" "go" 0 "print is a builtin")))

(deftest token-tones-map-to-the-syntax-palette ()
  (is-table (in want) (eq want (nle:token-tone in)) (:keyword :syntax-keyword "keyword tone")
    (:command :syntax-command "command tone") (:flag :syntax-flag "flag tone")
    (:variable :syntax-variable "variable tone"))
  (is (null (nle:token-tone :plain)) "plain paints in the base style"))

;;; --- color profile: detection, the probe gate, the dump hook ----------------
;;; The environment detector under the ssh shapes that motivated the probe
;;; (ssh forwards TERM, strips COLORTERM), the rule for when a live probe
;;; may upgrade the resolved profile, and the save-hook that keeps a dumped
;;; image from carrying the build terminal's resolution.

(deftest color-profile-detection-answers-the-ssh-shapes ()
  ;; Each row is the profile, the environment, why, and whether a tty is there.
  (dolist (case '((:truecolor (("TERM" . "xterm-256color") ("COLORTERM" . "truecolor"))
                   "COLORTERM says truecolor")
                  (:ansi256 (("TERM" . "xterm-256color")) "ssh strips COLORTERM: TERM alone guesses ansi256")
                  (:truecolor (("TERM" . "xterm-kitty"))
                   "a terminal that has always honored 24-bit SGR resolves on its name alone")
                  (:truecolor (("TERM" . "wezterm")) "wezterm by name too")
                  (:ansi (("TERM" . "xterm")) "a bare xterm stays 16-color")
                  (:ansi (("TERM" . "xterm-kitty") ("NO_COLOR" . "1"))
                   "NO_COLOR outranks the name: 16-color, the Zig mapping")
                  (:none (("TERM" . "xterm-kitty")) "no tty, no color" :is-tty nil)))
    (destructuring-bind (profile env why &key (is-tty t)) case
      (is (eq profile (nle::detect-color-profile
                       :is-tty is-tty
                       :getenv (lambda (name) (cdr (assoc name env :test #'equal)))))
          why))))

(deftest probe-upgrades-only-an-environment-guess ()
  (flet ((upgradable-p (profile &optional no-color &aux (nle:*color-profile* profile))
           (nle::color-profile-upgradable-p
            :getenv (lambda (name)
                      (and no-color (equal name "NO_COLOR") no-color)))))
    (is (upgradable-p :ansi256) "an env-guessed ansi256 may upgrade")
    (is (upgradable-p :ansi) "16-color may upgrade")
    (is (not (upgradable-p :truecolor)) "truecolor has nothing to gain")
    (is (not (upgradable-p :none)) "a colorless posture never upgrades")
    (is (not (upgradable-p :ansi "1")) "NO_COLOR is the user's word: never past it")
    (is (upgradable-p :ansi "") "an empty NO_COLOR does not count")))

(deftest color-profile-memo-is-forgotten-before-a-dump ()
  ;; The resolved profile is a fact about one environment: the warm-up run
  ;; resolves it while rendering, and an image that carried the resolution
  ;; painted every later process with the build terminal's colors (the
  ;; shipped release answered :truecolor from the builder's COLORTERM).
  (is (member 'nle::forget-color-profile sb-ext:*save-hooks*))
  (let ((nle:*color-profile* :truecolor))
    (nle::forget-color-profile)
    (is (eq :auto nle:*color-profile*) "the memo goes back to :auto")))

(deftest every-bundled-theme-keeps-quiet-text-legible ()
  ;; Without this, a muted tone would read below 4.5:1 on a ground it is
  ;; painted on and an error notice would read below 4.5:1 on its wash. The
  ;; floor is WCAG AA, 4.5:1, on every ground each tone is painted on, dark
  ;; and light themes alike, and the lift keeps the hierarchy: quiet text
  ;; stays quieter than body text.
  (flet ((fg (palette tone) (nle::visual-style-fg (gethash tone palette)))
         (bg (palette tone) (nle::visual-style-bg (gethash tone palette))))
    (dolist (name (nle:theme-catalog :user nil))
      (let ((palette (theme-palette name)))
        (dolist (tone '(:muted :steer-preview :danger :syntax-comment))
          (dolist (ground '(:background :composer :user-block))
            (when (bg palette ground)
              (is (>= (nle::contrast-ratio-x100 (fg palette tone) (bg palette ground)) 450)
                  (format nil "~a ~(~a~) on ~(~a~)" name tone ground)))))
        (is (>= (nle::contrast-ratio-x100 (fg palette :diagnostic-block) (bg palette :diagnostic-block)) 450)
            (format nil "~a error notice text on its wash" name))
        (is (< (nle::contrast-ratio-x100 (fg palette :muted) (bg palette :composer))
               (nle::contrast-ratio-x100 (fg palette :foreground) (bg palette :composer)))
            (format nil "~a muted stays quieter than body text" name)))))
  ;; A tone already over the floor keeps its colour exactly.
  (is (equal '(191 162 172) (nle::visual-style-fg (gethash :muted (theme-palette "Hello Kitty Lo-Fi"))))))

(deftest line-tokenizer-leaves-an-unknown-language-plain ()
  ;; Without this, a bare ``` fence with no language would go through the
  ;; shared keyword sets and paint words like "with" and "if" bold orange.
  ;; No language, or one no profile names, is plain code: no tokens at all.
  (is-each (nle:tokenize-line)
    ("Deploy with care: if the lane is new, return" "" '() "no language, no tokens")
    ("with a default of 30 s, try again" "text" '() "an unknown language takes none either")
    ("with a default of 30 s, try again" "TEXT" '() "whatever its case"))
  (is-table (kind line language offset) (eq kind (token-kind-at (nle:tokenize-line line language) offset))
    (:keyword "if (x) return 1;" "js" 0 "a known alias still highlights")
    (:keyword "if (x) return 1;" "JavaScript" 0 "by any case of its name")
    (:keyword "(let ((x 1)) x)" "lisp" 1 "the Lisps keep the shared sets")
    (:keyword "def f():" "py" 0 "and a profiled language its own")))

(deftest dim-text-clears-the-floor-on-every-ground (dolist (name (nle:theme-catalog :user nil)))
  ;; Without checking every ground, a theme's dim text could clear 4.5:1 on
  ;; the canvas and still fall below it on a panel. In every bundled theme, a
  ;; dim run clears 4.5:1 on the canvas, the panels and the user block, and
  ;; stays quieter than the tone it dims.
  (let ((nle:*palette* (theme-palette name)))
    (flet ((rgb (tone part) (funcall part (nle:style-for-tone tone))))
      (dolist (tone '(:foreground :muted))
        (let ((dimmed (nle::visual-style-fg (nle:dim-style (nle:style-for-tone tone))))
              (canvas (or (rgb :background #'nle::visual-style-bg)
                          (rgb :composer #'nle::visual-style-bg))))
          (dolist (ground nle::*text-grounds*)
            (when (rgb ground #'nle::visual-style-bg)
              (is (>= (nle::contrast-ratio-x100 dimmed (rgb ground #'nle::visual-style-bg)) 450)
                  (format nil "~a dim ~(~a~) on ~(~a~)" name tone ground))))
          (is (<= (nle::contrast-ratio-x100 dimmed canvas)
                  (nle::contrast-ratio-x100 (rgb tone #'nle::visual-style-fg) canvas))
              (format nil "~a dim ~(~a~) is no louder than ~:*~(~a~)" name tone))))
      (is (< (nle::contrast-ratio-x100 (nle::visual-style-fg (nle:dim-style (nle:style-for-tone :foreground)))
                                       (rgb :composer #'nle::visual-style-bg))
             (nle::contrast-ratio-x100 (rgb :foreground #'nle::visual-style-fg)
                                       (rgb :composer #'nle::visual-style-bg)))
          (format nil "~a dim body text is dimmer than body text" name)))))

(deftest thought-tone-is-palette-owned-and-complementary-under-theme-switches ()
  (dolist (colors (list (make-hash-table :test 'eq)
                       (nle:theme-colors-for-name "Tungstene")
                       (nle:theme-colors-for-name "Blueprint Daylight")
                       (alexandria:plist-hash-table (list :syntax-string '(210 110 60) :transparent-background t) :test 'eq)))
    (let* ((palette (nle::palette-from-theme-colors colors))
           (warm (nle::visual-style-fg (gethash :tool palette)))
           (cool (gethash :thought palette))
           (sum (+ (reduce #'min warm) (reduce #'max warm))))
      (is-present cool "the palette has a real tone, not unknown-tone foreground fallback"
        (is (equal (mapcar (lambda (channel) (- sum channel)) warm) (nle::visual-style-fg cool)))))))


(deftest status-mark-holds-steps-and-wraps-every-kind ()
  (dolist (entry nle::*status-marks*)
    (let ((kind (car entry)) (frames (cdr entry))
          (step nle::*status-running-frame-ms*))
      (loop for expected across frames for i from 0
            do (is (equal expected (nle::status-mark (* i step) kind)))
               (is (equal expected (nle::status-mark (1- (* (1+ i) step)) kind))))
      (is (equal (aref frames 0) (nle::status-mark (* (length frames) step) kind)))
      (is (> (length (remove-duplicates (coerce frames 'list) :test #'equal)) 1)
          (format nil "~(~a~) actually animates" kind))))
  (is (handler-case (progn (nle::status-mark 0 :not-an-activity) nil)
        (error () t)) "an unknown kind fails, never silently takes a generic mark"))

(deftest user-block-matches-composer-and-hover-lifts-its-surface ()
  (dolist (colors (append
                   (mapcar (lambda (name) (nle:theme-colors-for-name name :user nil))
                           (nle:theme-catalog :user nil))
                   (loop for key in '(:background-input :background-inset :background-surface)
                         collect (alexandria:plist-hash-table
                                  (list key '(240 245 250) :background-faded '(1 2 3)) :test 'eq))
                   (list (make-hash-table :test 'eq))))
    (let* ((palette (nle:palette-from-theme-colors colors))
           (user (gethash :user-block palette))
           (hover (gethash :user-block-hover palette))
           (composer (gethash :composer palette)))
      (is (equal (nle::visual-style-bg user) (nle::visual-style-bg composer)) "the composer and user share the brighter surface")
      (is (equal (mapcar (lambda (channel) (min 255 (+ channel 26)))
                        (nle::visual-style-bg user))
                 (nle::visual-style-bg hover)) "hover lifts the shared surface with saturated channels")
      (is (equal (nle::visual-style-fg user) (nle::visual-style-fg hover)) "hover keeps the user text color")))
  (let ((palette (nle:palette-from-theme-colors
                   (nle:theme-colors-for-name "Tungstene" :user nil))))
    (is (equal '(59 57 55) (nle::visual-style-bg (gethash :composer palette))) "Tungstene composer takes the former hover color")
    (is (equal '(59 57 55) (nle::visual-style-bg (gethash :user-block palette))) "Tungstene user block takes the former hover color")
    (is (equal '(85 83 81) (nle::visual-style-bg (gethash :user-block-hover palette))) "Tungstene hover adds a second lift")))
