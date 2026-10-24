;;;; theme.lisp --- the terminal design system: palette, tones, profile, wrap.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Distilled from the Zig TUI's design authority so both organisms render the
;;;; same design: src/tui/render_plan.zig (VisualPalette, the tone table),
;;;; src/tui/design_system.zig (color-profile resolution, quantizers, the dim
;;;; primitive), src/tui/wrap.zig (the one line-break authority), and
;;;; src/tui/body_layout.zig (gutter geometry). Values are copied verbatim;
;;;; where this port narrows a mechanism, the docstring says so.
;;;;
;;;; Everything here is pure: no I/O beyond reading environment variables in
;;;; DETECT-COLOR-PROFILE. The mutable display policy in ui.lisp consumes this
;;;; module; a mutation restyling the organism edits the palette or the policy,
;;;; not the log.

(in-package #:nodecode.evolved)

;;; --- rgb and visual style ------------------------------------------------
;;; An rgb is a 3-element list (r g b), 0-255 each — the Zig Rgb = [3]u8.

(defstruct (visual-style (:constructor vstyle (&key fg bg bold dim italic underline strike)))
  "One resolved style: optional fg/bg rgb plus attributes."
  ;; The Zig VisualStyle carries fg/bg/bold/dim; italic and underline live at
  ;; the paint sites there and are folded into the style here so a segment is
  ;; one value. STRIKE is SGR 9, what a Markdown ~~run~~ draws with.
  (fg nil) (bg nil) (bold nil) (dim nil) (italic nil) (underline nil) (strike nil))

(nlk:access (style visual-style))

(defun style-with (style &key fg bg (bold nil boldp) (dim nil dimp)
                              (italic nil italicp) (underline nil underlinep)
                              (strike nil strikep))
  "A copy of STYLE with the given slots replaced."
  (vstyle :fg (or fg style.fg)
          :bg (or bg style.bg)
          :bold (if boldp bold style.bold)
          :dim (if dimp dim style.dim)
          :italic (if italicp italic style.italic)
          :underline (if underlinep underline style.underline)
          :strike (if strikep strike style.strike)))

;;; --- the palette ---------------------------------------------------------
;;; Field-for-field the Zig VisualPalette defaults (render_plan.zig:1264-1317).
;;; The Zig default theme "Tungstene" overlays these from JSON at startup; the
;;; theme-catalog layer is a recorded gap, so these compiled-in defaults are
;;; the whole palette for now.

(defparameter *tones*
  `((:background ,(vstyle :bg '(28 31 38)) (:foreground) (:background-void :background))
    (:foreground ,(vstyle :fg '(235 238 244)) (:foreground))
    (:title ,(vstyle :fg '(242 244 248) :bold t) (:primary :foreground))
    ;; Session header title — warm amber accent (Zig comment kept).
    (:session-title ,(vstyle :fg '(246 192 117)) (:accent1 :syntax-keyword :foreground))
    (:chrome ,(vstyle :fg '(210 214 222)) (:secondary :foreground))
    (:muted ,(vstyle :fg '(142 150 164)) (:tertiary :secondary))
    (:composer ,(vstyle :fg '(235 238 244) :bg '(61 65 74))
     (:user-input :foreground) (:background-input :background-inset :background-surface))
    ;; The docked list's hover accent: the tone the row under the
    ;; selection reads. Split from :composer, which carries the same
    ;; chain but belongs to the composer box — a layer that neutralizes
    ;; that box must not take the hover accent with it.
    (:dialog-accent ,(vstyle :fg '(235 238 244)) (:user-input :foreground))
    ;; nodecode.ai's own models where a list offers them (/models' Nodecode
    ;; section): the ember of the logo, apart from the hover accent so a row of
    ;; ours never reads as the one under the selection.
    (:own-model ,(vstyle :fg '(232 78 14)) (:accent3 :accent2 :user-input))
    (:assistant ,(vstyle :fg '(130 214 176)) (:success :syntax-builtin :accent4))
    ;; The status green a list row's mark column asks for by name. It was a
    ;; tone nothing defined, so every `•' a picker painted fell back to
    ;; :foreground and the column said nothing a plain row did not.
    (:success ,(vstyle :fg '(130 214 176)) (:success :syntax-builtin :accent4))
    (:user ,(vstyle :fg '(132 178 255)) (:user-input :accent2))
    (:steer-preview ,(vstyle :fg '(142 150 164) :dim t) (:tertiary :secondary))
    (:tool ,(vstyle :fg '(228 184 96)) (:syntax-string :accent1 :info))
    ;; Text disclosures hover like links, with the theme's saturated accent.
    (:disclosure-hover ,(vstyle :fg '(232 78 14)) (:accent3 :accent2 :user-input))
    ;; Thought labels oppose the tool hue, keeping its chroma/lightness.
    ;; The themed value is derived after the tool's overlay below.
    (:thought ,(vstyle :fg '(96 140 228)))
    (:system ,(vstyle :fg '(178 184 196)) (:secondary :foreground))
    (:danger ,(vstyle :fg '(255 126 126)) (:error-color))
    (:section ,(vstyle :fg '(162 170 184) :bg '(24 27 34) :bold t)
     (:secondary :foreground) (:background-chrome :background-subtle :background-surface))
    (:approval ,(vstyle :fg '(255 206 115) :bg '(45 38 24) :bold t)
     (:info :syntax-type :accent1) (:background-inset-contrast :background-selected :selection))
    (:selection ,(vstyle :fg '(242 244 248) :bg '(58 63 76))
     (:foreground :user-input) (:selection :background-selected :background-hover))
    (:hover ,(vstyle :bg '(41 45 55)) () (:background-chrome :background-hover :background-surface))
    ;; Composer and user blocks share a surface; hover stays one lift brighter.
    (:user-block ,(vstyle :fg '(235 238 244) :bg '(61 65 74))
     (:foreground :user-input)
     (:background-input :background-inset :background-surface))
    (:user-block-hover ,(vstyle :fg '(235 238 244) :bg '(87 91 100)))
    (:system-block ,(vstyle :fg '(178 184 196) :bg '(35 39 48))
     (:secondary :foreground) (:background-inset :background-surface))
    (:diagnostic-block ,(vstyle :fg '(255 126 126) :bg '(76 42 41)) (:error-color))
    ;; A toast (ui.lisp TOAST): the ground one step lighter than the panel
    ;; surface, so it stands off the canvas and the composer and user block
    ;; it floats over, and one step more under the pointer. An error's text
    ;; is the danger red on either. Both grounds are derived from the
    ;; surface under a theme (PALETTE-FROM-THEME-COLORS).
    (:toast ,(vstyle :fg '(210 214 222) :bg '(45 49 58)) (:secondary :foreground))
    (:toast-hover ,(vstyle :fg '(210 214 222) :bg '(55 59 68)) (:secondary :foreground))
    (:toast-danger ,(vstyle :fg '(255 126 126)) (:error-color))
    (:scrollbar-track ,(vstyle :bg '(35 39 48)) () (:background-surface :background-track))
    (:scrollbar-thumb ,(vstyle :fg '(142 150 164) :bg '(58 63 76))
     (:tertiary :secondary) (:background-thumb :background-selected :background-hover))
    ;; A hairline rule (the /sessions day heading's): the thumb's ground as a
    ;; glyph's ink. The transcript rail has no track; its thumb stands alone.
    (:scrollbar-rail ,(vstyle :fg '(58 63 76)) (:background-thumb :background-selected :background-hover))
    ;; The composer typed into while a turn runs (COMPOSER-ROWS): the box warms,
    ;; since Enter now steers that turn rather than starting one.
    (:composer-steer ,(vstyle :fg '(235 238 244) :bg '(69 58 46))
     (:user-input :foreground) (:background-selected :background-input :background-surface))
    ;; A key named on screen -- the live bar's esc, the composer's steer keys:
    ;; a small filled cap, the boxes' own filled language.
    (:key-chip ,(vstyle :fg '(242 244 248) :bg '(87 91 100))
     (:primary :foreground) (:background-floating :background-elevated :background-active))
    ;; The ground a docked list's chosen row stands on, when the row lays
    ;; itself (PICKER-ROW-PAINT): one lift over the panel surface, derived
    ;; from it below, as the toast's grounds are.
    (:panel-lift ,(vstyle :bg '(78 82 91)))
    ;; The sheet in the middle of the frame (settings.lisp): its ground, the
    ;; darker rail of sections beside it, the lift under the chosen setting,
    ;; and its options -- the one held, the one held and set, the one held
    ;; under the cursor -- with the amber that says a change waits for a
    ;; restart.
    (:sheet ,(vstyle :fg '(235 238 244) :bg '(35 39 48))
     (:foreground) (:background-surface :background-inset :background-input))
    (:sheet-rail ,(vstyle :fg '(210 214 222) :bg '(30 33 41))
     (:secondary :foreground) (:background-subtle :background-chrome :background-surface))
    (:sheet-focus ,(vstyle :fg '(235 238 244) :bg '(49 54 65))
     (:foreground) (:background-active :background-hover :background-selected))
    (:sheet-chip ,(vstyle :fg '(210 214 222) :bg '(58 63 76))
     (:secondary :foreground) (:background-elevated :background-badge :background-active))
    (:sheet-chip-set ,(vstyle :fg '(246 192 117) :bg '(69 58 46))
     (:user-input :accent2) (:background-selected :background-active))
    (:sheet-chip-on ,(vstyle :fg '(28 31 38) :bg '(246 192 117))
     (:background-canvas :background) (:user-input :accent2 :selection))
    (:sheet-restart ,(vstyle :fg '(255 206 115)) (:info :syntax-type :accent1))
    ;; /generations (generations.lisp): the rows going back would undo, the
    ;; ground of the question asked in place before it does.
    (:sheet-undone ,(vstyle :fg '(142 150 164) :dim t) (:tertiary :secondary))
    (:sheet-ask ,(vstyle :fg '(235 238 244) :bg '(69 58 46))
     (:foreground) (:background-selected :background-active))
    ;; Edit-diff lanes follow the theme's success/error families.
    (:diff-added ,(vstyle :fg '(130 214 176)) (:success :syntax-builtin :accent4))
    (:diff-removed ,(vstyle :fg '(255 126 126)) (:error-color))
    (:diff-added-line ,(vstyle :bg '(40 56 52)))
    (:diff-removed-line ,(vstyle :bg '(58 42 45)))
    (:diff-added-emphasis ,(vstyle :fg '(130 214 176) :bg '(62 92 86))
     (:success :syntax-builtin :accent4))
    (:diff-removed-emphasis ,(vstyle :fg '(255 126 126) :bg '(101 61 68)) (:error-color))
    ;; Word-level diff emphasis paint: the Zig renderer forces the
    ;; emphasis fg to the content foreground at paint time
    ;; (transcript.zig appendDiffBodySegments emphasis_style.fg =
    ;; content_style.fg), so these derived tones carry the
    ;; foreground fg over the emphasis bg.
    ;; The word-emphasis paint tones: content foreground over the
    ;; emphasis wash (the Zig fg-override; the wash is derived below).
    (:diff-added-word ,(vstyle :fg '(235 238 244) :bg '(62 92 86)) (:foreground))
    (:diff-removed-word ,(vstyle :fg '(235 238 244) :bg '(101 61 68)) (:foreground))
    (:syntax-keyword ,(vstyle :fg '(71 208 177) :bold t) (:syntax-keyword))
    (:syntax-string ,(vstyle :fg '(110 168 255)) (:syntax-string))
    (:syntax-type ,(vstyle :fg '(90 175 255)) (:syntax-type))
    (:syntax-function ,(vstyle :fg '(232 236 244)) (:syntax-function))
    (:syntax-number ,(vstyle :fg '(255 198 93)) (:syntax-number))
    (:syntax-comment ,(vstyle :fg '(134 145 160)) (:syntax-comment))
    (:syntax-punctuation ,(vstyle :fg '(112 123 139)) (:syntax-punctuation))
    (:syntax-constant ,(vstyle :fg '(168 176 188)) (:syntax-constant))
    (:syntax-builtin ,(vstyle :fg '(94 201 123)) (:syntax-builtin))
    (:syntax-operator ,(vstyle :fg '(168 176 188)) (:syntax-operator))
    ;; Shell flags/variables have no dedicated theme keys; derive from
    ;; accents (the Zig comments, kept with their chains).
    (:syntax-flag ,(vstyle :fg '(200 160 255)) (:accent2 :syntax-constant))
    (:syntax-variable ,(vstyle :fg '(255 198 93)) (:syntax-number :syntax-constant))
    ;; Shell command words: warm accent, deliberately not builtin-green.
    ;; Shell command words track the theme's lighter warm accent.
    (:syntax-command ,(vstyle :fg '(253 186 116)) (:syntax-type :syntax-keyword))
    ;; An answer's two hues (DERIVE-ANSWER-TONES), solved per theme; these
    ;; defaults are Tungstene's on a #131313 terminal. What it finds -- its
    ;; strong runs, filled as a chip, and its list marks -- is the strong
    ;; accent's warm hue; how it is laid out -- headings on a band, code,
    ;; quote bars, rules -- is that accent turned half way round, the cool.
    ;; The strong tone's chain names the accent the derivation reads.
    (:inline-strong ,(vstyle :fg '(253 179 118) :bold t) (:accent1 :primary))
    (:chip ,(vstyle :bg '(77 48 26)))
    (:list-mark ,(vstyle :fg '(207 119 48)))
    (:heading ,(vstyle :fg '(188 220 245) :bold t))
    (:heading-band ,(vstyle :bg '(29 55 75)))
    (:title-band ,(vstyle :bg '(33 71 100)))
    (:subheading ,(vstyle :fg '(145 196 238) :bold t))
    ;; A code run's name, a path's last segment, and what stands back: a
    ;; path's folders, a symbol's package, punctuation, ids.
    (:inline-code ,(vstyle :fg '(127 176 215)))
    (:code-path ,(vstyle :fg '(145 196 238)))
    (:code-quiet ,(vstyle :fg '(91 133 166)))
    (:markdown-bar ,(vstyle :fg '(104 152 190)))
    (:markdown-line ,(vstyle :fg '(69 93 112)))
    ;; A fenced block's ground; its edge is a bar's.
    (:slab ,(vstyle :bg '(27 33 37)))
    ;; Operator-selected Ice blue; reasoning's shadow still dims this RGB.
    (:thought-strong ,(vstyle :fg '(155 194 232) :bold t))
    ;; Markdown [text](url) links: underlined info-blue text.
    (:link ,(vstyle :fg '(110 168 255)))
    ;; A fact that wants a look before it is a problem: the header's
    ;; occupancy nearing the eviction line, /status's attention rows.
    (:warn ,(vstyle :fg '(253 186 116)) (:info :accent1))
    ;; /context's grid (client.lisp +CONTEXT-PARTS+): a part's cells and its
    ;; chip, the free cells, the free cells past the eviction line, and the
    ;; other parts' under one part's view. The history part paints in
    ;; :thought.
    (:context-prompt ,(vstyle :fg '(210 214 222)) (:secondary :foreground))
    (:context-tools ,(vstyle :fg '(130 214 176)) (:success))
    (:context-index ,(vstyle :fg '(246 192 117)) (:accent1))
    (:context-notes ,(vstyle :fg '(124 201 16)) (:accent4 :syntax-builtin))
    (:context-free ,(vstyle :fg '(41 37 36)) (:background-surface :background-subtle))
    (:context-zone ,(vstyle :fg '(90 44 24)))
    (:context-other ,(vstyle :fg '(87 83 78)) (:background-floating :background-elevated)))
  "Every colour the UI paints, one row per tone: the tone keyword, the
compiled-in default style, and the theme keys a theme document overlays it
from — the fg chain first, then the bg chain, the first key present winning.
One table, so a tone cannot reach the palette with its overlay forgotten.")

(defun default-palette (&aux (table (make-hash-table :test #'eq)))
  "A fresh palette carrying the compiled-in defaults."
  ;; The live *PALETTE* starts here, and PALETTE-FROM-THEME-COLORS overlays a
  ;; theme onto a fresh copy so re-engaging a theme never accretes state from
  ;; the previous one.
  (loop for (tone style) in *tones*
        do (setf (gethash tone table) style))
  (setf (gethash :selection-ground table) (gethash :background table))
  table)

(defparameter *palette* (default-palette)
  "Tone keyword -> VISUAL-STYLE. One authority for every color the UI paints.")

(defun style-for-tone (tone)
  "The named palette style or an already-resolved VISUAL-STYLE."
  ;; Unknown tones fall back to :foreground so a mutation naming a new tone
  ;; renders legibly instead of crashing the frame.
  (if (visual-style-p tone)
      tone
      (or (gethash tone *palette*) (gethash :foreground *palette*))))

;;; --- blending, contrast, and the dim primitive ---------------------------

(defun blend-rgb (fg bg percent)
  "PERCENT parts FG over BG, integer math rounded (Zig blendRgb)."
  (cond ((null fg) bg)
        ((null bg) fg)
        (t (mapcar (lambda (f b)
                     (floor (+ (* f percent) (* b (- 100 percent)) 50) 100))
                   fg bg))))

(defun relative-luminance (rgb)
  "WCAG relative luminance of RGB, 0 for black to 1 for white."
  (flet ((linear (channel &aux (scaled (/ channel 255.0d0)))
           (if (<= scaled 0.03928d0)
               (/ scaled 12.92d0)
               (expt (/ (+ scaled 0.055d0) 1.055d0) 2.4d0))))
    (+ (* 0.2126d0 (linear (first rgb)))
       (* 0.7152d0 (linear (second rgb)))
       (* 0.0722d0 (linear (third rgb))))))

(defun contrast-ratio (a b &aux (la (relative-luminance a)) (lb (relative-luminance b)))
  "WCAG relative-contrast ratio between rgb A and B."
  (/ (+ (max la lb) 0.05d0) (+ (min la lb) 0.05d0)))

(defun contrast-ratio-x100 (a b)
  "WCAG relative-contrast ratio between rgb A and B, scaled by 100."
  (round (* 100 (contrast-ratio a b))))

(defun dark-rgb-p (rgb)
  "Whether RGB is a dark ground, its WCAG luminance under 0.2: the side of
it text stands on is the light one."
  (< (relative-luminance rgb) 0.2d0))

(defun dim-color (foreground background &optional (grounds (list background)))
  "Deepest blend of FOREGROUND toward BACKGROUND still clearing the contrast
floor on every one of GROUNDS. Tones already at or below the floor return
unchanged (Zig dimColor)."
  ;; The Zig floor is 4.50:1 contrast, the strongest wash a 50% blend.
  ;; BLEND-RGB counts the foreground's share where Zig's dimColor counts
  ;; the background's, so the walk runs from half toward the tone. If it
  ;; followed Zig's count, it would go deeper instead, and a tone whose
  ;; half blend missed the floor would not dim at all.
  (let ((target (reduce #'min grounds :initial-value 450
                                      :key (lambda (ground) (contrast-ratio-x100 foreground ground)))))
    (loop for percent from 50 to 100
          for candidate = (blend-rgb foreground background percent)
          when (every (lambda (ground) (>= (contrast-ratio-x100 candidate ground) target)) grounds)
            return candidate)))

(defun legible-rgb (color grounds toward)
  "COLOR, or the least blend of it toward TOWARD that clears 4.50:1 contrast
on every one of GROUNDS; TOWARD itself when no blend does."
  ;; The floor DIM-COLOR dims down to, applied from below: a palette tone that
  ;; starts under it is lifted just enough.
  (loop for percent from 0 to 100
        for candidate = (blend-rgb toward color percent)
        when (every (lambda (ground) (>= (contrast-ratio-x100 candidate ground) 450))
                    (remove nil grounds))
          return candidate
        finally (return toward)))

;;; SEG-RENDER dims through this on every painted frame, and DIM-COLOR walks
;;; up to fifty contrast probes (double-float EXPT per channel) per miss — a
;;; measured animation-frame hotspot. Keys are the palette-interned style
;;; structs, which a theme engage replaces wholesale, so weak keys age dead
;;; palettes out on their own.
(defvar *dim-style-cache*
  (make-hash-table :test #'eq #+sbcl :weakness #+sbcl :key)
  "STYLE -> its dimmed variant.")

(defparameter *text-grounds* '(:background :composer :composer-steer :user-block
                                :sheet :sheet-rail :sheet-focus)
  "The tones whose backgrounds text is painted on: the canvas, the composer
and every docked panel on its surface (the dialogs, /execs), the composer
warmed by a running turn, the user block and the slash menu over it, and
the sheet, its rail and the lift under its chosen row.")

(defun dim-style (style)
  "The de-emphasized variant of STYLE: fg blended toward the palette
background under a 4.50:1 contrast floor on every ground it may be painted
on -- its own background, else each of *TEXT-GROUNDS*."
  ;; This replaces the terminal SGR 2 attribute, which only de-emphasizes
  ;; correctly on dark palettes (Zig dimStyle). A style with no resolvable fg
  ;; keeps the terminal attribute instead.
  (alexandria:ensure-gethash style *dim-style-cache*
   (let* ((grounds (if style.bg
                       (list style.bg)
                       (remove nil (mapcar (lambda (tone) (visual-style-bg (style-for-tone tone)))
                                           *text-grounds*))))
          (bg (first grounds))
          (fg (or style.fg
                  (visual-style-fg (style-for-tone :foreground)))))
     (if (and fg bg)
         (style-with style :fg (dim-color fg bg grounds) :dim nil)
         (style-with style :dim t)))))

;;; --- the shadow primitive ------------------------------------------------
;;; A shade laid over painted text rather than a tone: the Zig shadowStyle
;;; every cell of a shadowed region passes through (design_system.zig; the
;;; startup connect backdrop, the rewind shadow the transcript once carried).
;;; The color is kept, the light is cut. Only the glyphs darken: the
;;; canvas under them is left exactly as the block would paint it, so a
;;; shadowed block is quieter text on the same surface, not a darker band.

(defparameter *shadow-percent* 68
  "Share of each color channel a shadowed cell keeps.")
;; At half (the Zig shadowChannel), a folded thought's Ice blue reads 2.9:1
;; on the canvas, the one line on screen under 3:1. At 68 it reads 4.7:1, and
;; still sits under the 7:1 of the muted lines around it.

(defun shadow-rgb (rgb)
  "RGB at *SHADOW-PERCENT* of every channel: the same hue at less light,
integer floor per channel (Zig shadowColor). NIL stays NIL."
  ;; On a palette whose ink is dark the shade lifts instead: each channel
  ;; keeps that share of its distance from white, so a shadowed run fades
  ;; toward the paper there as it fades toward the dark here.
  (when rgb
    (if (dark-rgb-p (visual-style-fg (style-for-tone :foreground)))
        (mapcar (lambda (channel) (- 255 (floor (* (- 255 channel) *shadow-percent*) 100))) rgb)
        (mapcar (lambda (channel) (floor (* channel *shadow-percent*) 100)) rgb))))

(defun shadow-style (style)
  "STYLE under a shadow: the foreground dimmed, a missing one resolving to the
palette :foreground first so there is a color to dim."
  ;; The background is untouched — the text darkens, the surface it sits on
  ;; does not, which is what keeps a shadowed block from reading as a block in
  ;; its own darker band. An overlay, not a retint — every span keeps its own
  ;; hue, which is what tells a shadowed block from a block in a quieter tone.
  ;; Where Zig also raises the SGR 2 attribute, this port stops at the blend:
  ;; the dim law above holds here too, and SGR 2 is a terminal-defined second
  ;; darkening that would take shadowed prose under the legible floor.
  (style-with style :fg (shadow-rgb (or style.fg (visual-style-fg (style-for-tone :foreground))))))

;;; --- color profile -------------------------------------------------------
;;; The Zig ordering (design_system.zig detectAutoColorProfile), which differs
;;; from the detector cl-tuition carried in one deliberate choice: NO_COLOR maps to
;;; :ansi (16-color), not :none. Two additions past the Zig table, both born
;;; of ssh stripping COLORTERM while forwarding TERM: terminals known to have
;;; always honored 24-bit SGR resolve :truecolor on their TERM name alone,
;;; and a live probe (NLT:REQUEST-TRUECOLOR, sent by the shell's INIT) may
;;; upgrade a quantized environment guess when the terminal itself confirms.

;;; Tests pin :none so goldens stay byte-stable plain text.
(defvar *color-profile* :auto
  "Requested profile: :auto :truecolor :ansi256 :ansi :none.")

(defvar *terminal-canvas* nil
  "The terminal's own background as it answered OSC 11, or NIL: what a
transparent theme's colours are solved against, since its canvas is the
terminal's.")

(defun detect-color-profile (&key (is-tty t) (getenv #'uiop:getenv))
  "Resolve :auto against the environment, Zig rule order plus the
known-truecolor terminal names."
  ;; GETENV is the environment read, a function of the variable name — tests
  ;; hand a table.
  (flet ((env (name) (json-value (funcall getenv name) :text))
         (has (haystack &rest needles)
           (and haystack
                (some (lambda (n) (search n (string-downcase haystack)))
                      needles))))
    (let ((term (env "TERM")))
      (cond ((or (not is-tty) (equal term "dumb")) :none)
            ((env "NO_COLOR") :ansi)
            ((or (env "WT_SESSION") (env "TERM_PROGRAM")
                 (has (env "COLORTERM") "truecolor" "24bit" "direct")
                 ;; The names of terminals that have always honored 24-bit
                 ;; SGR: without them, xterm-kitty over ssh fell through
                 ;; "xterm" all the way to 16 colors.
                 (has term "truecolor" "24bit" "direct" "kitty" "ghostty"
                      "wezterm" "alacritty" "foot" "konsole" "iterm"))
             :truecolor)
            ((has term "256color") :ansi256)
            ((has term "color" "ansi" "xterm" "screen") :ansi)
            (t :ansi256)))))

(defun effective-color-profile ()
  "The profile frames render under: *COLOR-PROFILE*, resolving :auto once."
  (if (eq *color-profile* :auto)
      (setf *color-profile* (detect-color-profile))
      *color-profile*))

(defun color-profile-upgradable-p (&key (getenv #'uiop:getenv))
  "Whether the live truecolor probe may upgrade the resolved profile: only
a quantized profile the environment guessed (:ansi256, :ansi) ever is —
:truecolor has nothing to gain, :none is a test or pipe posture, and a
NO_COLOR verdict is the user's word, not the environment's shortfall."
  (and (member (effective-color-profile) '(:ansi256 :ansi))
       (zerop (length (funcall getenv "NO_COLOR")))))

(defun forget-color-profile ()
  "Drop the resolved profile memo, so no dumped image carries the build
terminal's environment (the warm-up run resolves it while rendering): the
next EFFECTIVE-COLOR-PROFILE re-resolves in the process that asks."
  ;; Rides
  ;; *SAVE-HOOKS* beside NLT's FORGET-TERMINAL. The terminal's background
  ;; goes with it, the same terminal's fact.
  (setf *terminal-canvas* nil
        *color-profile* :auto))

(pushnew 'forget-color-profile sb-ext:*save-hooks*)

;;; --- quantizers (Zig design_system.zig rgbToAnsi256 / rgbToAnsi16Index) ---

(defun rgb-to-ansi256 (rgb)
  "Nearest xterm-256 index: 6x6x6 cube vs grayscale ramp by squared distance."
  (flet ((dist2 (a b)
           (reduce #'+ (mapcar (lambda (x y) (* (- x y) (- x y))) a b))))
    (let* ((cube (mapcar (lambda (value)
                           (cond ((< value 48) 0)
                                 ((< value 115) 1)
                                 (t (max 0 (min 5 (truncate (- value 35) 40))))))
                         rgb))
           (gray-index (min 23 (truncate (+ (reduce #'+ rgb) 6) 30))))
      (if (<= (dist2 rgb (mapcar (lambda (index) (aref #(0 #x5f #x87 #xaf #xd7 #xff) index)) cube))
              (dist2 rgb (make-list 3 :initial-element (+ 8 (* 10 gray-index)))))
          (+ 16 (* 36 (first cube)) (* 6 (second cube)) (third cube))
          (+ 232 gray-index)))))


(defparameter *ansi16-palette*
  '((0 0 0) (205 0 0) (0 205 0) (205 205 0) (0 0 205) (205 0 205)
    (0 205 205) (229 229 229) (127 127 127) (255 0 0) (0 255 0)
    (255 255 0) (92 92 255) (255 0 255) (0 255 255) (255 255 255))
  "The Zig 16-color reference table (xterm defaults).")

(defun rgb-to-ansi16 (rgb)
  "Nearest 16-color index by squared distance."
  (let ((distances (mapcar (lambda (candidate)
                             (reduce #'+ (mapcar (lambda (x y) (* (- x y) (- x y))) rgb candidate)))
                           *ansi16-palette*)))
    (position (reduce #'min distances) distances)))

(defun ansi256-rgb (index)
  "The rgb behind xterm-256 INDEX: system colors from the 16-color table,
then the 6x6x6 cube, then the grayscale ramp. NIL for out-of-range."
  (cond ((or (null index) (minusp index) (> index 255)) nil)
        ((< index 16) (nth index *ansi16-palette*))
        ((< index 232)
         (let ((i (- index 16))
               (scale #(0 #x5f #x87 #xaf #xd7 #xff)))
           (list (aref scale (floor i 36))
                 (aref scale (mod (floor i 6) 6))
                 (aref scale (mod i 6)))))
        (t (let ((v (+ 8 (* 10 (- index 232))))) (list v v v)))))

;;; --- SGR rendering -------------------------------------------------------

(defun color-sgr (rgb foreground-p profile)
  "SGR parameter string for RGB under PROFILE, or NIL when colorless."
  (ecase profile
    ((:truecolor :auto)
     (format nil "~d;2;~d;~d;~d" (if foreground-p 38 48)
             (first rgb) (second rgb) (third rgb)))
    (:ansi256
     (format nil "~d;5;~d" (if foreground-p 38 48) (rgb-to-ansi256 rgb)))
    (:ansi
     ;; The bright eight sit 60 above the base eight: 90/100 over 30/40.
     (multiple-value-bind (bright index) (floor (rgb-to-ansi16 rgb) 8)
       (format nil "~d" (+ (if foreground-p 30 40) (* 60 bright) index))))
    (:none nil)))

(defparameter *sgr-reset* (format nil "~c[0m" #\Escape))

;;; STYLED paints thousands of spans per second from a bounded set of style
;;; values, and formatting the same escape prefix per span was a measured
;;; frame hotspot. EQUALP keys the style by value, so the fresh copies
;;; STYLE-WITH hands out all land on one entry.
(defvar *sgr-prefix-cache* (make-hash-table :test #'equalp)
  "VISUAL-STYLE -> (profile . \"ESC[<params>m\"), the empty string for a bare
style.")

(defun sgr-prefix (style profile &aux (hit (gethash style *sgr-prefix-cache*)))
  "The full SGR escape prefix for STYLE under PROFILE, \"\" when bare."
  (if (and hit (eq (car hit) profile))
      (cdr hit)
      ;; The full SGR parameter list, bare under :none.
      (let ((parts (unless (eq profile :none)
                     (loop for (reader code) in '((visual-style-bold "1") (visual-style-dim "2")
                                                  (visual-style-italic "3")
                                                  (visual-style-underline "4")
                                                  (visual-style-strike "9")
                                                  (visual-style-fg t) (visual-style-bg nil))
                           for value = (funcall reader style)
                           when value
                             collect (if (stringp code) code (color-sgr value code profile))))))
        (cdr (setf (gethash style *sgr-prefix-cache*)
                   (cons profile (if parts
                                     (format nil "~c[~{~a~^;~}m" #\Escape parts)
                                     "")))))))

(defun styled (text style &key (profile (effective-color-profile)))
  "TEXT, every control codepoint shown as its Control Picture, wrapped in the
SGR sequence for STYLE; plain (but still pictured) under :none."
  ;; The one paint seam: content enters the frame through here or not at all,
  ;; which is what makes the display authority below a guarantee rather than a
  ;; habit.
  (let ((text (display-text text))
        (prefix (and style (sgr-prefix style profile))))
    (if (and prefix (plusp (length prefix)) (plusp (length text)))
        (concatenate 'string prefix text *sgr-reset*)
        text)))

;;; --- the display authority -----------------------------------------------
;;; One string carries both lanes to the renderer: the content a row says and
;;; the SGR/OSC control this file writes around it. The renderer's parser
;;; (src/term/cells.lisp PARSE-STYLED-STRING) cannot tell a content ESC
;;; from ours: it honours a content SGR as style, swallows any other CSI/OSC
;;; as zero cells, an unterminated one eats the rest of the frame, and a C1
;;; byte (U+0080-U+009F) it emits raw is a live CSI/OSC to the terminal.
;;; The line breaker meanwhile measured those same bytes as columns, so
;;; measure and paint disagreed and the gutter cell, the padding and every
;;; row below moved (the btop capture in frame-guard-test is the shape).
;;;
;;; Content is escaped, never modelled: every control codepoint paints as
;;; its Control Picture (U+2400 block), one char to one char at width 1,
;;; and the measurers count it that way. Measure equals paint by
;;; construction for any string from any lane, and the index-based lanes
;;; (fragments, spans, plains, selection) never see the substitution.
;;;
;;; The resulting frame contract, checked once more at the renderer boundary
;;; by GUARD-FRAME-ROWS: the only ESC bytes in a frame are the SGR runs
;;; SGR-PREFIX / *SGR-RESET* write and the OSC-8 pairs OSC8-WRAP writes.

(declaim (inline control-codepoint-p))
(defun control-codepoint-p (code)
  "Whether CODE is one the renderer or the terminal reads as control rather
than paints: C0 minus tab (the compositor paints tab as one space cell),
DEL, and C1."
  ;; Newline is control here too — a row is one line by contract, and a
  ;; newline inside one shifts every row below it.
  (declare (type (integer 0 #x10FFFF) code))
  (or (and (< code 32) (/= code 9))
      (= code 127)
      (<= #x80 code #x9F)))

(declaim (inline display-glyph))
(defun display-glyph (ch &aux (code (char-code ch)))
  "The glyph painted for CH: CH itself, or a control codepoint's Control
Picture — U+2400+code for C0, U+2421 for DEL, U+2426 (substitute form two,
the block's one generic symbol; C1 has no pictures of its own) for C1."
  ;; The pictures are East-Asian-narrow, so width 1 on every terminal.
  (cond ((not (control-codepoint-p code)) ch)
        ((< code 32) (code-char (+ #x2400 code)))
        ((= code 127) (code-char #x2421))
        (t (code-char #x2426))))

(defun display-text (text)
  "TEXT with every control codepoint replaced by its Control Picture; TEXT
itself, uncopied, when it carries none — the common case, one scan."
  ;; The copy is a fresh CHARACTER string whatever TEXT's element type: a
  ;; symbol name arrives as a base-string, and a picture does not fit one.
  (if (find-if (lambda (ch) (control-codepoint-p (char-code ch))) text)
      (map 'string #'display-glyph text)
      text))

(declaim (inline blank-char-p))
(defun blank-char-p (ch)
  "Whether CH is one of the blanks NLK:TRIMMED strips: space, tab, CR, LF."
  (member ch '(#\Space #\Tab #\Return #\Newline)))

;;; --- display width and the line breaker ----------------------------------
;;; Port of src/tui/wrap.zig LineBreaker. Width measurement delegates to
;;; the terminal core's codepoint table (NLT:CODEPOINT-WIDTH: east-asian wide and
;;; combining marks, not full UAX #29 clusters — a recorded delta from the
;;; Zig grapheme iterator), under the display authority above for controls.

(declaim (inline char-display-width))
(defun char-display-width (ch)
  "Columns CH occupies as painted: 0, 1, or 2. A control codepoint is 1,
its Control Picture, whatever the renderer's table says of the raw code."
  (if (control-codepoint-p (char-code ch))
      1
      (nlt:codepoint-width ch)))

(defun text-display-width (text)
  "Columns TEXT occupies as painted."
  ;; TEXT is content, not a painted run: an SGR/OSC sequence here is counted
  ;; as the pictures STYLED would paint it as.
  (loop for ch across text
        sum (the (integer 0 2) (char-display-width ch)) of-type fixnum))

(defun take-display-prefix (text budget)
  "The longest prefix of TEXT whose display width fits BUDGET."
  ;; The one prefix-taker: the transcript's middle-truncated previews, the
  ;; chrome rows that elide rather than spill, and the diagram labels all cut
  ;; here, so none of them can measure a wide glyph differently from the wrap.
  (loop for index from 0 below (length text)
        sum (char-display-width (char text index)) into width
        when (> width budget) return (subseq text 0 index)
        finally (return text)))

(defun break-fragments (text first-width cont-width &key hidden soft &aux (len (length text)))
  "Split TEXT (one logical line, no newlines) into (start . end) fragments:
FIRST-WIDTH columns for the first, CONT-WIDTH for the rest. HIDDEN, a bit
vector over TEXT, marks characters never painted -- Markdown's markers --
which take no column and no break; SOFT breaks a word wider than the row
after its last , - / _ or . instead of where the row runs out."
  ;; Greedy and word-aware — a fragment takes all that fits and then rewinds
  ;; to just past the last bare space, so a word the row cannot hold moves
  ;; whole to the next row instead of being divided between two. A word wider
  ;; than any row is the one hard break; the overflow guard requires a
  ;; non-empty fragment first, so one glyph wider than the row still emits
  ;; alone (the Zig wrap.zig invariant). Empty text yields one empty fragment:
  ;; a blank line still costs one row.
  (declare (type fixnum len))
  (flet ((shown-p (index) (not (and hidden (= 1 (sbit hidden index))))))
    (if (zerop len)
        (list (cons 0 0))
        (loop with start of-type fixnum = 0
              for width of-type fixnum = (max 1 first-width) then (max 1 cont-width)
              while (< start len)
              collect (let ((fragment-width 0)
                            (index start)
                            (break-end nil))
                        (declare (type fixnum fragment-width index))
                        (loop while (< index len)
                              for delta of-type (integer 0 2)
                                = (if (shown-p index) (char-display-width (char text index)) 0)
                              do (when (and (plusp fragment-width)
                                            (> (+ fragment-width delta) width))
                                   (return))
                                 (incf fragment-width delta)
                                 (incf index)
                                 (when (and (char= (char text (1- index)) #\Space) (shown-p (1- index)))
                                   (setf break-end index)))
                        (let ((end (cond ((>= index len) len)
                                         ((and break-end (> break-end start)) break-end)
                                         ;; At most 31 back, and 9 kept on the row.
                                         ((and soft (loop for at from (1- index) above (max (+ start 8) (- index 32))
                                                          when (and (find (char text (1- at)) ",-/_.")
                                                                    (shown-p (1- at)))
                                                            return at)))
                                         (t index))))
                          (prog1 (cons start end) (setf start end))))))))

(defun wrapped-lines (text width)
  "TEXT broken at WIDTH columns (BREAK-FRAGMENTS), each line's trailing blanks trimmed."
  (loop for (start . end) in (break-fragments text width width)
        collect (string-right-trim " " (subseq text start end))))

(defun cap-lines (lines most width)
  "LINES, at most MOST of them, the last cut to WIDTH columns with an … when there were more."
  (let ((kept (subseq lines 0 (min most (length lines)))))
    (when (< most (length lines))
      (rplaca (last kept) (format nil "~a…" (take-display-prefix (car (last kept)) (1- width)))))
    kept))

;;; --- body gutter geometry -------------------------------------------------
;;; lead spaces, first-row mark, first indent, continuation indent, counted
;;; inside the band's 2-column inset. Every column lands on one of the
;;; transcript's rails: the gutter (1, absolute 3) for timestamps and row
;;; marks, the label lane (3, absolute 5) for Eval, Think and a notice's
;;; words, nested output two in from it (5, absolute 7), and the content rail
;;; (*TRANSCRIPT-DETAIL-COLUMN*, absolute 14). The diagnostic mark is wide but
;;; continuations wrap back to the system indent.
(defparameter *body-gutters*
  '((:prose         "   "  ""          3  3)
    ;; A heading's band opens on its edge in the gutter, the words on the
    ;; label lane; a wrapped heading's rows go on under the edge (its line's
    ;; hang mark), a lead's under its words.
    (:heading       " "    "▌ "        3  1)
    ;; A numbered item, a column out from the prose (ordinal llines inset
    ;; it by how much narrower its number is than its list's widest).
    (:ordinal       "  "   ""          2  2)
    ;; A fenced code block's body, two columns in from the prose around it,
    ;; on its slab, whose edge stands on the label lane. A code line too long
    ;; for the row goes on under the edge and a ↦ (its line's hang mark),
    ;; since at the body's own column a wrapped row would read as a new line
    ;; indented less.
    (:code          "   "  "▌ "        5  3)
    ;; A notice's mark rides the gutter and its words the label lane. Both
    ;; sit one column in from those rails, a grid of their own.
    (:system        " "    "■ "        3  3)
    (:system-cont   "   "  ""          3  3)
    (:diagnostic    " "    "■ Error: " 10 3)
    (:tool          "   "  "⎿ "        5  5)
    ;; The inline image band: blank rows a terminal draws an image over, so
    ;; only the indent matters — the card's own body column, which is where
    ;; the picture lines up with the header naming it.
    (:image-slot    "   "  ""          3  3)
    (:tool-cont     "     " ""         5  5)
    (:tool-omission "   "  "⋮ "        5  5)
    (:status        " "    ""          1  1)
    ;; A group that opens on a click says so in the gutter, where a notice
    ;; keeps its mark. Without this, at rest it would read as a plain muted
    ;; line, and only hover would tell it opened.
    (:disclosure      " " "▸ "         3  3)
    (:disclosure-open " " "▾ "         3  3)
    ;; A queued steer prompt hangs under the live line's words: the hook on
    ;; their column, the prompt two in, where a run's calls stand (absolute 5
    ;; and 7). In the gutter the hook sits under the spinner's two cells, which
    ;; the operator reads as paint under it. The
    ;; rows are composed inside the transcript band, whose two-column left
    ;; inset is added after lline layout, so these carry the same absolute
    ;; columns less that inset.
    (:steer-band      "   "   "⌎ " 5 5)
    (:steer-band-cont "     " ""   5 5))
  "role -> (lead mark first-indent continuation-indent): what a line's first
row opens on, and where it and the rows it wraps onto start.")

(defun body-gutter (role)
  "The gutter row for ROLE: (values lead mark first-indent cont-indent)."
  ;; An inset role is (BASE-ROLE . COLUMNS), keeping the base mark and
  ;; relative Markdown/code geometry without adding mutable line state.
  (if (consp role)
      (multiple-value-bind (lead mark first cont) (body-gutter (car role))
        (values (concatenate 'string (make-string (cdr role) :initial-element #\Space) lead)
                mark (+ first (cdr role)) (+ cont (cdr role))))
      (values-list (rest (or (assoc role *body-gutters*) (assoc :prose *body-gutters*))))))

(defparameter *transcript-detail-column* 12
  "Compact activity descriptions and a labelled notice's words, before the
band's outer inset.")

(defparameter *transcript-right-padding* 1
  "Trailing column body text never touches (Zig transcript_text_right_padding).")

(defparameter *max-markdown-hang* 24
  "Cap on markdown hanging indents (Zig max_markdown_hang).")

;;; --- status marks (src/tui/render_plan.zig) -------------------------------

(defparameter *status-mark-interrupted* "×")
;;; One mark per kind of verb the live bar names: every activity family
;;; (activity.lisp), the model's own :THINKING and :STREAMING, and :DEFAULT —
;;; the pulse a tool without words of its own keeps, and the one the /sessions
;;; rows and setup's loading rows wear. Two cells, the top three dot rows, so
;;; the bar's text never moves; every frame lasts *STATUS-RUNNING-FRAME-MS*,
;;; and a mark that wants to move slower holds a frame twice.
(defparameter *status-marks*
  '((:default   . #("⠸⠇" "⠺⠗" "⠿⠿" "⠺⠗"))
    (:wait      . #("⠐⠂" "⠐⠂" "⠐⠂" "⠐⠂" "⠂⠐" "⠂⠐" "⠂⠐" "⠂⠐"))
    (:thinking  . #("⠜⠀" "⠘⠠" "⠊⠠" "⠂⠨" "⠢⠈" "⠠⠊" "⠡⠂" "⠁⠆" "⠁⠔" "⠀⠕" "⠄⠑" "⠔⠁"))
    (:streaming . #("⠑⠔" "⠊⠢" "⠔⠑" "⠢⠊"))
    (:eval      . #("⠇⠸" "⠇⠸" "⠸⠇" "⠐⠂" "⠐⠂" "⠐⠂"))
    (:read      . #("⠾⠿" "⠷⠿" "⠿⠾" "⠿⠷" "⠽⠿" "⠯⠿" "⠿⠽" "⠿⠯" "⠻⠿" "⠟⠿" "⠿⠻" "⠿⠟"))
    (:search    . #("⠇⠀" "⠸⠀" "⠀⠇" "⠀⠸" "⠀⠇" "⠸⠀"))
    (:list      . #("⠯⠥" "⠯⠥" "⠷⠒" "⠷⠒" "⠿⠍" "⠿⠍" "⠿⠂" "⠿⠂" "⠿⠅" "⠿⠅" "⠟⠂" "⠟⠂"))
    (:fetch     . #("⠡⠂" "⠂⠌" "⠌⠐" "⠐⠡"))
    (:recall    . #("⠂⠌" "⠂⠌" "⠡⠂" "⠡⠂" "⠐⠡" "⠐⠡" "⠌⠐" "⠌⠐"))
    (:poll      . #("⠉⠁" "⠋⠀" "⠇⠀" "⠦⠀" "⠤⠄" "⠠⠤" "⠀⠴" "⠀⠸" "⠀⠙" "⠈⠉"))
    (:image     . #("⠺⠕" "⠺⠕" "⠺⠕" "⠪⠕" "⠪⠗" "⠪⠗" "⠪⠗" "⠪⠕" "⠒⠒" "⠪⠕"))
    (:edit      . #("⠒⠒" "⠒⠒" "⠒⠺" "⠒⠇" "⠺⠀" "⠺⠀" "⠒⠇" "⠒⠺"))
    (:write     . #("⠉⠀" "⠉⠉" "⠛⠉" "⠛⠛" "⠿⠛" "⠿⠿" "⠿⠿" "⠿⠿"))
    (:define    . #("⠇⠸" "⠇⠸" "⠏⠹" "⠟⠻" "⠿⠿" "⠿⠿"))
    (:command   . #("⠕⠇" "⠕⠌" "⠕⠒" "⠕⠡")))
  "Verb kind -> the live mark's frames: :DEFAULT the pulse, :WAIT the sway,
:THINKING a twinkle, :STREAMING a wave, :EVAL parens reducing to a value,
:READ a gap walking a page, :SEARCH a sweep, :LIST lines scrolling up, :FETCH
rain, :RECALL dots rising, :POLL an orbit, :IMAGE an eye, :EDIT a backspace
and retype, :WRITE a page filling, :DEFINE parens filling with a body,
:COMMAND a prompt and its spinner.")
(defparameter *status-running-frame-ms* 150)
(defparameter *status-waiting-after-ms* 2000)

(defun status-mark (elapsed-ms &optional (kind :default))
  "KIND's live mark at ELAPSED-MS into the turn."
  (let ((frames (cdr (assoc kind *status-marks*))))
    (aref frames (mod (truncate elapsed-ms *status-running-frame-ms*) (length frames)))))

;;; --- scrollbar geometry (src/tui/transcript.zig) ---------------------------
;;; The transcript's scroll rail: painted only while the band can scroll. The
;;; right edge of a frame is: text … gap(1) rail(1), so the rail sits in the
;;; last column, width - 1, and the right margin matches the left inset.

(defun scrollbar-thumb (track-height visible-rows total-rows scroll
                        &key (round-fn #'floor) fits-nil)
  "The thumb for a track of TRACK-HEIGHT rows over TOTAL-ROWS of content with
VISIBLE-ROWS in view, scrolled to SCROLL: (values top height), the Zig
transcriptScrollbarThumb math verbatim."
  ;; The thumb is at least one row; a content total of zero still paints one.
  ;; The one thumb geometry in the shell: the two rounding rules Zig ships
  ;; differ only in ROUND-FN — #'floor for the transcript gutter and the
  ;; composer dropdown (dropdownScrollbarThumb, which is this function with
  ;; the track as its own visible count), #'ceiling for the docked picker
  ;; (renderShortcutPickerScrollbar). FITS-NIL is that picker's other
  ;; divergence: it answers (values nil nil) while the content fits, because
  ;; Zig draws no bar there at all, where the transcript fills its track
  ;; instead.
  (if (or (zerop track-height)
          (and fits-nil (<= total-rows visible-rows)))
      (if fits-nil (values nil nil) (values 0 0))
      (let* ((total (max total-rows visible-rows))
             (height (min track-height
                          (max 1 (funcall round-fn (* track-height visible-rows) (max total 1)))))
             (max-scroll (max 0 (- total visible-rows)))
             (top (if (zerop max-scroll)
                      0
                      (floor (* (min scroll max-scroll) (- track-height height)) max-scroll))))
        (values top height))))

;;; --- theme catalog (src/config.zig) ---------------------------------------
;;; The Zig TUI never actually renders the compiled VisualPalette defaults: at
;;; startup it resolves general.theme (schema default "Tungstene") against the
;;; bundled themes document plus the user's ~/.nodecode/themes.json and overlays
;;; the result. This section is that layer: same documents, same key grammar,
;;; same derivations, same chooseRgb fallback chains.

(defparameter *bundled-themes-json*
  (uiop:read-file-string
   (asdf:system-relative-pathname
    "nodecode" "../resources/config/themes/default.json"))
  "The shared bundled themes document, read at load so a dumped image carries
its canvas and input-surface policy in the dumped image.")

(defun read-user-themes ()
  "The user themes document, or NIL when none exists."
  (nlk:read-text (home "themes.json")))

;;; Theme color objects. Keys are the ThemeColors field grammar: kebab JSON
;;; keys name snake fields, "error" names error_color, unknown keys are
;;; silently ignored (config.zig applyThemeColor).

(defun theme-colors-for-name (name &key (bundled *bundled-themes-json*)
                                        (user nil))
  "The resolved color table for theme NAME: every matching entry in the
bundled document, then every matching entry in the USER document, overlaid in
order (config.zig themeColorsForName)."
  ;; Signals when no document names the theme — the canonical path fails
  ;; clearly rather than downgrading.
  (let ((colors (make-hash-table :test #'eq))
        (matched nil)
        ;; What a theme document may name: the keys some tone's chain reads,
        ;; and the canvas the chrome and inset-contrast washes blend from.
        (keys (cons :background-canvas
                    (remove-duplicates
                     (loop for (nil nil fg bg) in *tones* append (append fg bg))))))
    (flet ((overlay (object &aux (explicit '()))
             (loop for field being the hash-keys of object using (hash-value value)
                   for key = (if (string= field "error")
                                 :error-color
                                 (find field keys :key #'string-downcase :test #'string=))
                   when key
                     do (push key explicit)
                        (setf (gethash key colors)
                              (etypecase value
                                (string
                                 (unless (ppcre:scan "^#[0-9A-Fa-f]{6}$" value)
                                   (error "theme colors are #rrggbb strings: ~s" value))
                                 (loop for start from 1 below 7 by 2
                                       collect (parse-integer value :start start
                                                                    :end (+ start 2) :radix 16)))
                                (vector
                                 (unless (= 3 (length value))
                                   (error "theme color arrays carry exactly three components: ~s"
                                          value))
                                 (let ((rgb (coerce value 'list)))
                                   (dolist (component rgb rgb)
                                     (unless (typep component '(integer 0 255))
                                       (error "theme color component out of range: ~s"
                                              component))))))))
             ;; Chrome leans three parts void to one of canvas; inset-contrast the inverse.
             (let ((void (gethash :background-void colors))
                   (canvas (gethash :background-canvas colors)))
               (when (and void canvas)
                 (loop for (key wv wc bias) in '((:background-chrome 3 1 2)
                                                 (:background-inset-contrast 1 3 0))
                       unless (member key explicit)
                         do (setf (gethash key colors)
                                  (mapcar (lambda (v c) (truncate (+ (* wv v) (* wc c) bias) 4))
                                          void canvas)))))))
      ;; Every theme a document names NAME overlays in turn; a document without a
      ;; "themes" array matches nothing (config.zig applyThemeDocumentColors).
      (dolist (document (remove nil (list bundled user)))
        (let ((root (nlk:decode-json (nlk::normalize-jsonc document))))
          (unless (hash-table-p root)
            (error "themes document root is not an object"))
          (when-let (themes (gethash "themes" root))
            (unless (and (vectorp themes) (not (stringp themes)))
              (error "themes document \"themes\" is not an array"))
            (loop for theme across themes
                  do (unless (hash-table-p theme)
                       (error "themes document entry is not an object"))
                     (let ((theme-name (or (json-value theme :string "name")
                                           (error "themes document entry has no name"))))
                       (when (string= theme-name name)
                         (setf matched t)
                         (multiple-value-bind (transparent present)
                             (gethash "transparent-background" theme)
                           (when present
                             (unless (member transparent '(t nil))
                               (error "theme transparent-background is a boolean: ~s" transparent))
                             (setf (gethash :transparent-background colors) transparent)))
                         (when-let (object (json-value theme :object "colors"))
                           (overlay object)))))))))
    (unless matched
      (error "theme ~s not found in any themes document" name))
    colors))

(defun theme-catalog (&key (user (read-user-themes)) &aux (names '()))
  "Theme names, bundled first then user additions, first occurrence winning
(config.zig themeCatalogFromDocuments)."
  (dolist (text (list *bundled-themes-json* user) (nreverse names))
    (when text
      (let ((root (nlk:decode-json (nlk::normalize-jsonc text))))
        (unless (hash-table-p root)
          (error "themes document root is not an object"))
        (loop for theme across (or (gethash "themes" root) #())
              for name = (gethash "name" theme)
              when (and (json-value name :text) (not (member name names :test #'string=)))
                do (push name names))))))

;;; --- an answer's two hues ---------------------------------------------------
;;; Every colour an answer adds is solved rather than picked: a hue, a
;;; saturation, and the lightness at which the two stand a set contrast off
;;; the canvas, so a light theme's are as far off its paper as a dark one's
;;; are off the dark. The cool hue is the strong accent turned half way
;;; round -- Tungstene's ice, Blueprint Daylight's brown -- the warm the accent
;;; itself.

(defun hsl-rgb (hue saturation lightness &aux (a (* saturation (min lightness (- 1 lightness)))))
  "The rgb at HUE in degrees, SATURATION and LIGHTNESS from 0 to 1."
  (mapcar (lambda (n &aux (k (mod (+ n (/ hue 30)) 12)))
            (max 0 (min 255 (floor (+ 1/2 (* 255 (- lightness (* a (max -1 (min (- k 3) (- 9 k) 1))))))))))
          '(0 8 4)))

(defun rgb-hsl (rgb)
  "(values HUE SATURATION LIGHTNESS) of RGB, the hue in degrees."
  (destructuring-bind (r g b) (mapcar (lambda (channel) (/ channel 255d0)) rgb)
    (let* ((max (max r g b)) (min (min r g b)) (l (/ (+ max min) 2)) (d (- max min)))
      (if (zerop d)
          (values 0d0 0d0 l)
          (values (mod (+ 360 (* 60 (cond ((= max r) (rem (/ (- g b) d) 6))
                                           ((= max g) (+ 2 (/ (- b r) d)))
                                           (t (+ 4 (/ (- r g) d))))))
                       360)
                  (/ d (- 1 (abs (- (* 2 l) 1))))
                  l)))))

(defun solved-lightness (hue saturation target canvas &optional (lighter (dark-rgb-p canvas)))
  "The lightness at which HUE at SATURATION stands TARGET:1 off CANVAS, on
the side of it LIGHTER says: lighter than it, or darker. Unsaid, the side
away from the canvas: lighter on a dark one. A target past that side's end
solves to the end."
  ;; A probe on the other side of the canvas is never far enough, however far
  ;; it stands: near a low target both ends of the scale clear it.
  (loop with lo = 0d0 and hi = 1d0
        repeat 40
        do (let* ((mid (/ (+ lo hi) 2))
                  (rgb (hsl-rgb hue saturation mid))
                  (far (and (eq lighter (> (relative-luminance rgb) (relative-luminance canvas)))
                            (>= (contrast-ratio rgb canvas) target))))
             (if (eq lighter far)
                 (setf hi mid)
                 (setf lo mid)))
        finally (return (if lighter hi lo))))

(defparameter +answer-ladder+
  '((:head 10d0 0.626d0) (:code 8d0 0.52d0) (:marks 6d0 0.4d0) (:id 4.7d0 0.3d0) (:lines 2.7d0 0.24d0))
  "The cool hue's steps, (ROLE CONTRAST SATURATION) each: what the answer
lays out with, brightest first.")

(defun answer-ladder (hue canvas &aux (taken '()))
  "+ANSWER-LADDER+ solved for HUE on CANVAS, a plist of ROLE and rgb. Each
step is nudged to the nearest that a 256-colour terminal still paints in
the hue, and the three that are text to cells of their own."
  ;; Text clears 4.5:1 before the cut and after it. Unnudged, Tungstene's
  ;; heads landed on a lavender cell and its marks and ids on one cell.
  (loop for (role target saturation) in +answer-ladder+
        for text = (member role '(:head :code :id))
        for lightness = (solved-lightness hue saturation target canvas)
        for rgb = (or (loop for dl in (cons 0d0 (loop for d = 0.004d0 then (+ d 0.004d0)
                                                      while (<= d 0.06d0) collect d collect (- d)))
                              thereis
                              (loop for ds in '(0 0.05d0 -0.05d0 0.1d0 -0.1d0 0.15d0 0.2d0)
                                    for candidate = (hsl-rgb hue (max 0 (min 1 (+ saturation ds)))
                                                             (max 0 (min 1 (+ lightness dl))))
                                    for cell = (rgb-to-ansi256 candidate)
                                    for ratio = (contrast-ratio candidate canvas)
                                    when (and (not (and text (member cell taken)))
                                              (<= (abs (- (/ ratio target) 1)) 0.1d0)
                                              (or (not text)
                                                  (and (>= ratio 4.5d0)
                                                       (>= (contrast-ratio (ansi256-rgb cell) canvas) 4.5d0)))
                                              (or (eq role :lines)
                                                  (multiple-value-bind (cell-hue cell-saturation)
                                                      (rgb-hsl (ansi256-rgb cell))
                                                    (and (>= cell-saturation 0.15d0)
                                                         (<= (min (mod (- cell-hue hue) 360)
                                                                  (mod (- hue cell-hue) 360))
                                                             22)))))
                                      return candidate))
                      (hsl-rgb hue saturation lightness))
        when text do (push (rgb-to-ansi256 rgb) taken)
        collect role collect rgb))

(defun answer-tones (accent link canvas)
  "An answer's colours, (TONE SLOT RGB) each, from ACCENT, the theme's
strong accent, and LINK, its links' blue, on CANVAS."
  (let* ((warm (rgb-hsl accent))
         (cool (rgb-hsl (let ((sum (+ (reduce #'min accent) (reduce #'max accent))))
                          (mapcar (lambda (channel) (- sum channel)) accent))))
         (ladder (answer-ladder cool canvas)))
    (flet ((solved (hue saturation target)
             (hsl-rgb hue saturation (solved-lightness hue saturation target canvas))))
      `((:inline-strong :fg ,(solved warm 0.97d0 10.5d0))
        (:chip :bg ,(solved warm 0.5d0 1.55d0))
        (:list-mark :fg ,(solved warm 0.62d0 5.6d0))
        (:heading :fg ,(solved cool 0.75d0 13d0))
        (:heading-band :bg ,(solved cool 0.45d0 1.5d0))
        (:title-band :bg ,(solved cool 0.5d0 1.9d0))
        (:subheading :fg ,(getf ladder :head))
        (:inline-code :fg ,(getf ladder :code))
        (:code-path :fg ,(getf ladder :head))
        (:code-quiet :fg ,(getf ladder :id))
        (:markdown-bar :fg ,(getf ladder :marks))
        (:markdown-line :fg ,(getf ladder :lines))
        (:slab :bg ,(solved cool 0.16d0 1.14d0))
        ;; The link's own blue, darkened on a paper it cannot be read on.
        (:link :fg ,(if (>= (contrast-ratio link canvas) 4.5d0)
                        link
                        (solved (rgb-hsl link) 0.7d0 4.6d0)))))))

;;; --- a theme turned over -------------------------------------------------
;;; A transparent theme stands on the terminal's canvas, which can be paper
;;; where the theme was drawn for the dark (Tungstene's ink read 1.05:1 on
;;; cream) or a colour of its own (PowerShell's navy). There every colour of
;;; the theme turns onto that canvas, as far off it as it stood off the
;;; theme's own; what derives from those colours derives on it too.

(defun washed-rgb (pole paper target &aux (lo 0d0) (hi 1d0))
  "PAPER washed toward POLE just far enough to stand TARGET:1 off it: POLE
itself when nothing short of it does."
  (flet ((wash (share)
           (mapcar (lambda (p c) (round (+ (* share p) (* (- 1 share) c)))) pole paper)))
    (loop repeat 30
          do (let ((mid (/ (+ lo hi) 2)))
               (if (>= (contrast-ratio (wash mid) paper) target)
                   (setf hi mid)
                   (setf lo mid))))
    (wash hi)))

(defun turned-rgb (rgb from to ink)
  "RGB, drawn on canvas FROM with INK for its text, turned onto canvas TO:
TO itself for FROM, else a colour standing as far off TO as RGB stood off
FROM, on INK's side of TO when RGB stood on INK's side of FROM and past TO
when it stood past FROM. A colour keeps its hue and saturation; a grey
turns into TO washed toward black or white."
  ;; A grey is paper and ink mixed, so on the new paper it is that paper
  ;; mixed: a panel a step off cream is cream a step darker, never a cold
  ;; slab laid on it, and quiet text is the paper's own grey.
  (if (equal rgb from)
      to
      (multiple-value-bind (hue saturation) (rgb-hsl rgb)
        (let* ((ratio (contrast-ratio rgb from))
               (inky (eq (> (relative-luminance rgb) (relative-luminance from))
                         (> (relative-luminance ink) (relative-luminance from))))
               (lighter (eq inky (dark-rgb-p to))))
          (if (< saturation 0.2d0)
              (washed-rgb (if lighter '(255 255 255) '(0 0 0)) to ratio)
              (hsl-rgb hue saturation (solved-lightness hue saturation ratio to lighter)))))))

(defun turning-canvas (colors &aux (own (gethash :background colors)) (terminal *terminal-canvas*))
  "The terminal canvas theme COLORS turns onto, or NIL: a transparent
theme's, once the terminal has said what it is, when it lies on the other
side of the theme's own or is a colour of its own: its channels at least
30 apart, PowerShell's navy (85) and Solarized's teal (54) but not a
near-black's blue tint (Dracula's 14)."
  (and (gethash :transparent-background colors) own terminal
       (not (equal own terminal))
       (or (not (eq (dark-rgb-p own) (dark-rgb-p terminal)))
           (>= (- (reduce #'max terminal) (reduce #'min terminal)) 30))
       terminal))

(defun turned-colors (colors to &aux (turned (make-hash-table :test #'eq))
                                     (from (gethash :background colors))
                                     (ink (or (gethash :foreground colors)
                                              (visual-style-fg (second (assoc :foreground *tones*))))))
  "COLORS, a theme's colour table, every colour in it turned from the theme's
own canvas onto TO."
  (maphash (lambda (key value)
             (setf (gethash key turned) (if (consp value) (turned-rgb value from to ink) value)))
           colors)
  turned)

(defun turned-palette (palette to &aux (from (visual-style-bg (gethash :background palette)))
                                       (ink (visual-style-fg (gethash :foreground palette))))
  "PALETTE, the compiled defaults, every tone's colours turned from its canvas
onto TO: a tone no theme key reaches stands on the turned canvas too."
  (maphash (lambda (tone style)
             (setf (gethash tone palette)
                   (style-with style
                               :fg (and style.fg (turned-rgb style.fg from to ink))
                               :bg (and style.bg (turned-rgb style.bg from to ink)))))
           palette)
  palette)

;;; --- the theme overlay (src/tui/render_plan.zig visualPaletteFromThemeColors)

(defun palette-from-theme-colors (colors &aux (canvas (turning-canvas colors))
                                              (colors (if canvas (turned-colors colors canvas) colors))
                                              (palette (if canvas
                                                           (turned-palette (default-palette) canvas)
                                                           (default-palette))))
  "A fresh palette: the compiled defaults with COLORS overlaid through the Zig
chooseRgb fallback chains, verbatim."
  ;; A chain that resolves to nothing keeps the default, so a sparse theme
  ;; still renders completely; attributes (bold, dim) always survive because
  ;; the overlay only replaces colors. On a canvas TURNING-CANVAS names,
  ;; the theme overlays turned onto it, defaults and all.
  (flet ((c (&rest keys)
           (some (lambda (key) (gethash key colors)) keys))
         (fg (tone) (visual-style-fg (gethash tone palette)))
         (bg (tone) (visual-style-bg (gethash tone palette)))
         (put (tone slot rgb)
           (when rgb
             (setf (gethash tone palette)
                   (style-with (gethash tone palette) slot rgb)))))
    ;; One row per tone, the table DEFAULT-PALETTE was built from: the first
    ;; theme key present wins, and a chain that resolves to nothing keeps the
    ;; compiled default.
    (loop for (tone nil fg bg) in *tones*
          do (put tone :fg (apply #'c fg))
             (put tone :bg (apply #'c bg)))
    ;; A 180-degree hue rotation: (min + max) - channel, not 255 - channel.
    ;; Same HSL saturation/lightness; Eval's orange becomes reciprocal blue.
    (let* ((warm (fg :tool))
           (sum (+ (reduce #'min warm) (reduce #'max warm))))
      (put :thought :fg (mapcar (lambda (channel) (- sum channel)) warm)))
    ;; An answer's colours, solved on the canvas they stand on: a transparent
    ;; theme's is the terminal's own once it has said what that is.
    (loop for (tone slot rgb) in (answer-tones (fg :inline-strong) (fg :link)
                                               (or (and (gethash :transparent-background colors)
                                                        *terminal-canvas*)
                                                   (bg :background)))
          do (put tone slot rgb))
    ;; The hover lift is derived from the bubble surface just overlaid,
    ;; so every theme keeps a visible affordance without a theme key
    ;; (brightened per channel, saturating at 255 — the Zig `+|`).
    (put :user-block-hover :fg (fg :user-block))
    (put :user-block-hover :bg
         ;; 26 a channel over the bubble's own surface (Zig user_block_hover_lift).
         (mapcar (lambda (channel) (min 255 (+ channel 26))) (bg :user-block)))
    (let ((success (c :success :syntax-builtin :accent4))
          (void (c :background-void :background))
          (danger (c :error-color)))
      ;; The wash blends against the already-overlaid danger fg, the Zig
      ;; ordering: danger was set from :error-color above.
      (put :diagnostic-block :bg
           (blend-rgb (or danger (fg :danger)) (c :background-surface :background-void) 18))
      (loop for (added removed percent) in '((:diff-added-line :diff-removed-line 15)
                                             (:diff-added-emphasis :diff-removed-emphasis 35)
                                             (:diff-added-word :diff-removed-word 35))
            do (put added :bg (blend-rgb success void percent))
               (put removed :bg (blend-rgb danger void percent))))
    ;; A toast's two grounds, one and two steps from the panel surface toward
    ;; the theme's text: a dark theme's toast lightens, a light one's darkens.
    (put :toast :bg (blend-rgb (fg :foreground) (bg :composer) 5))
    (put :toast-hover :bg (blend-rgb (fg :foreground) (bg :composer) 10))
    (put :panel-lift :bg (blend-rgb (fg :foreground) (bg :composer) 10))
    ;; Quiet text is still text to read: the muted grey of hints, placeholders,
    ;; help and the footer, the steer preview, code comments and error text
    ;; clear 4.5:1 (WCAG AA) on every ground they are painted on — the canvas,
    ;; the composer and docked panels, the user block — an error notice's
    ;; text on its own wash, and a toast's text on both its grounds. Without
    ;; this, a muted grey can fall below the floor; it is lifted toward the
    ;; theme's foreground, never onto it, so each keeps its hue and stays
    ;; quieter than body text; a theme that already clears the floor keeps
    ;; its colour exactly. The washes above were blended from the theme's own
    ;; danger first.
    (loop for (tones grounds) in `(((:muted :steer-preview :danger :syntax-comment)
                                    ,*text-grounds*)
                                   ((:diagnostic-block) (:diagnostic-block))
                                   ((:toast :toast-hover :toast-danger) (:toast :toast-hover)))
          do (dolist (tone tones)
               (put tone :fg (legible-rgb (fg tone) (mapcar #'bg grounds) (fg :foreground))))))
  ;; A transparent theme's text stands on the terminal's canvas: a tone
  ;; legible on the theme's own canvas and not on that one is lifted until
  ;; it is. A tone drawn faint stays faint.
  (let ((terminal (and (gethash :transparent-background colors) *terminal-canvas*))
        (own (visual-style-bg (gethash :background palette)))
        (ink (visual-style-fg (gethash :foreground palette))))
    (when (and terminal own (not (equal terminal own)))
      (loop for tone being the hash-keys of palette using (hash-value style)
            when (and style.fg (null style.bg)
                      (>= (contrast-ratio-x100 style.fg own) 450)
                      (< (contrast-ratio-x100 style.fg terminal) 450))
              do (setf (gethash tone palette)
                       (style-with style :fg (legible-rgb style.fg (list terminal) ink))))))
  ;; The selection is blended later, over the painter's cells. Retain its
  ;; nominal canvas ground before clearing the actual canvas fill: the
  ;; inherited terminal RGB is unknown, not the selection color itself.
  (setf (gethash :selection-ground palette) (gethash :background palette))
  ;; Keep nominal RGBs through derivation: washes and panel colors do not
  ;; change when the canvas inherits the terminal. NIL is the renderer's
  ;; existing terminal-default background, not a guessed terminal RGB.
  (when (gethash :transparent-background colors)
    (let ((canvas (copy-visual-style (gethash :background palette))))
      (setf (visual-style-bg canvas) nil
            (gethash :background palette) canvas)))
  palette)

;;; --- startup engagement ----------------------------------------------------

(defun configured-theme-name (&optional config-text)
  "The theme the operator configured: general.theme in ~/.nodecode/config.jsonc
(the same file the Zig organism reads), else the schema default \"Tungstene\"
(src/config/schema.zig)."
  ;; CONFIG-TEXT substitutes for the file in tests. An unreadable or shapeless
  ;; config falls back to the default: theme selection is cosmetic, not worth
  ;; refusing startup over — resolution of the NAMED theme stays strict in
  ;; THEME-COLORS-FOR-NAME.
  ;;
  ;; With no CONFIG-TEXT the document arrives through READ-SHARED-CONFIG, the one
  ;; memoized reader of that file, which also means theme selection honours
  ;; *SHARED-CONFIG-PATH* — NIL under test, so the suite never reads the
  ;; developer's real config.
  (let ((root (if config-text
                  (ignore-errors (nlk:decode-json
                                  (nlk::normalize-jsonc config-text)))
                  (read-shared-config))))
    (or (json-value root :string "general" "theme") "Tungstene")))

(define-startup-parameter *theme-selection-path*
  (home "theme-selection")
  "Where /themes persists the operator's selection — the organism's
own saveThemeSelection surface. NIL disables persistence; tests bind.")

(defun saved-theme-selection ()
  "The persisted /themes selection, or NIL."
  ;; Unreadable state is ignored: theme selection is cosmetic, never worth
  ;; refusing startup over.
  (when *theme-selection-path*
    (values (json-value (nlk:trimmed (or (nlk:read-text *theme-selection-path*) "")) :text))))

(defun save-theme-selection (name)
  "Persist NAME as the durable /themes selection -- NIL forgets it, the
configured theme the one engaged from then on; best-effort."
  (when *theme-selection-path*
    (ignore-errors
      (if name
          (nlk::with-output-file (out *theme-selection-path*)
            (write-string name out))
          (uiop:delete-file-if-exists *theme-selection-path*))
      t)))

(defun engage-theme (&key (name (or (saved-theme-selection)
                                    (configured-theme-name)))
                          (user-document nil user-document-p))
  "Resolve NAME against the bundled and user theme documents and install the
palette."
  ;; An empty NAME keeps the compiled defaults (Zig startupThemeColors skips
  ;; the overlay when no theme is configured); a NAME no document carries
  ;; signals. Returns the installed palette.
  (cond ((zerop (length name)) *palette*)
        (t
         ;; The dead palette's style values would otherwise sit in the SGR
         ;; memo forever; the dim cache is weak-keyed and ages out on its own.
         (clrhash *sgr-prefix-cache*)
         (setf *palette*
               (palette-from-theme-colors
                (theme-colors-for-name
                 name
                 :user (if user-document-p user-document
                           (read-user-themes))))))))

;;; --- syntax highlighting (src/tui/syntax_highlight.zig) --------------------
;;; The one tokenization authority behind every syntax-* tone: a language-aware
;;; generic tokenizer for fenced code bodies and a shell-state-aware tokenizer
;;; for command headers. Ported verbatim; offsets are character indices where
;;; the Zig works in bytes (equivalent for the ASCII classes both scan — a
;;; non-ASCII character is never alpha, so it stays plain in both).

(define-record (syntax-token (:constructor syntax-token (start end kind)) (:export :readers))
  "One classified run over a line: [START,END) plus a kind keyword."
  start end kind)

(defun token-tone (kind)
  "The palette tone a token KIND paints with, or NIL for :plain."
  (cdr (assoc kind (load-time-value
                    (loop for kind in '(:keyword :string :type :function :number :comment
                                        :punctuation :constant :builtin :operator :flag
                                        :variable :command)
                          collect (cons kind (alexandria:format-symbol :keyword "SYNTAX-~a" kind)))
                    t))))

;;; Identifier sets. Membership runs per identifier on every rendered frame,
;;; so these are hash sets like the Zig StaticStringMaps.

(macrolet ((define-word-sets (&rest rows)
             `(progn ,@(loop for (name . words) in rows
                             collect `(defparameter ,name (string-set ',words))))))
  (define-word-sets
   (*keyword-set*
    "abstract" "as" "async" "await" "break" "case" "catch"
    "class" "const" "continue" "default" "defer" "do" "else"
    "enum" "export" "extends" "extern" "finally" "fn" "for"
    "from" "function" "if" "impl" "implements" "import" "in"
    "inline" "interface" "is" "let" "match" "mod" "module"
    "namespace" "new" "package" "private" "protected" "public"
    "pub" "return" "static" "struct" "switch" "this" "throw"
    "trait" "try" "type" "union" "unsafe" "use" "using" "var"
    "while" "with" "yield")
   (*zig-keyword-set*
    "allowzero" "align" "and" "anyframe" "anytype" "asm"
    "callconv" "comptime" "errdefer" "error" "linksection"
    "noalias" "nosuspend" "opaque" "orelse" "or" "packed"
    "resume" "suspend" "test" "threadlocal" "unreachable"
    "usingnamespace" "volatile")
   (*python-keyword-set* "assert" "def" "del" "elif" "except" "global" "lambda"
                         "nonlocal" "pass" "raise")
   (*shell-keyword-set* "case" "coproc" "done" "elif" "esac" "fi" "select" "then"
                        "time" "until")
   (*sql-keyword-set* "alter" "by" "create" "delete" "desc" "distinct" "drop"
                      "group" "having" "insert" "into" "join" "limit" "offset"
                      "order" "select" "table" "update" "values" "view" "where")
   (*constant-set* "False" "Infinity" "NaN" "None" "True" "false" "nil" "null"
                   "self" "super" "true" "undefined")
   (*builtin-type-set* "Array" "Bool" "Boolean" "Error" "Map" "Object" "Option"
                       "Result" "Self" "Set" "String" "Vec" "anyerror" "bool"
                       "boolean" "byte" "char" "dict" "f16" "f32" "f64" "f80"
                       "f128" "float" "i8" "i16" "i32" "i64" "i128" "isize" "list"
                       "number" "str" "string" "tuple" "u8" "u16" "u32" "u64"
                       "u128" "usize" "void")
   (*builtin-set*
    "Array" "Boolean" "Date" "JSON" "Math" "Object" "Promise"
    "String" "console" "dict" "echo" "eprint" "eprintln" "len"
    "list" "map" "print" "printf" "println" "range" "set"
    "std")
   (*shell-builtin-set* "cd" "echo" "export" "printf" "pwd" "read" "set" "source"
                        "test" "unset")
   (*command-prefix-set* "sudo" "env" "command" "time" "exec" "xargs" "nice" "nohup"
                         "then" "do" "else" "elif")))

(defparameter *language-profiles*
  `((("zig") :keywords ,*zig-keyword-set* :builtin-prefix #\@)
    (("python" "py") :keywords ,*python-keyword-set* :hash-comments t)
    (("bash" "sh" "shell" "zsh" "fish")
     :keywords ,*shell-keyword-set* :builtins ,*shell-builtin-set*
     :hash-comments t)
    (("sql") :keywords ,*sql-keyword-set* :sql-like t)
    (("ruby" "rb" "yaml" "yml" "toml" "make" "makefile" "r" "perl" "pl")
     :hash-comments t)
    (("lua" "haskell" "hs") :sql-like t)
    ;; The languages the shared sets were written for, with // comments, and
    ;; the Lisps, which they read well enough.
    (("c" "h" "cc" "cpp" "c++" "hpp" "cs" "csharp" "java" "kotlin" "kt" "scala" "swift"
      "go" "golang" "rust" "rs" "js" "javascript" "jsx" "mjs" "ts" "typescript" "tsx"
      "json" "jsonc" "json5" "dart" "php" "css" "scss" "proto" "graphql"
      "lisp" "cl" "common-lisp" "elisp" "emacs-lisp" "scheme" "clojure" "clj" "racket")))
  "Per-language tokenizer profile behind the language's aliases: its keyword
and builtin sets (or a builtin prefix character) and its line-comment family —
:hash-comments for word-initial #, :sql-like for --, else //. A language no
row names — none at all, `text', `console' — has no profile and takes no
tokens.")

;;; Character classes (ASCII, the Zig predicates).

(defun %digit-p (ch) (char<= #\0 ch #\9))
(defun %alnum-p (ch) (and (< (char-code ch) 128) (alphanumericp ch)))
(defun %char-kind (ch)
  "The kind a lone character colors as: an operator, punctuation, or :plain."
  (cond ((find ch "+-*/%=!<>&|^~?:.@") :operator)
        ((find ch "(){}[],;") :punctuation)
        (t :plain)))
(defun %whitespace-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return #\Page)))

;;; Scanners, shared by both tokenizers.

(defun scan-quoted-string (text start quote &aux (len (length text))
                                                 (index (1+ start)))
  "End of the quoted run opened at START; backslash escapes skip a character
and an unterminated quote runs to the end (Zig scanQuotedString)."
  (loop while (< index len)
        do (let ((ch (char text index)))
             (setf index (min (+ index (if (char= ch #\\) 2 1)) len))
             (when (char= ch quote) (return))))
  index)

(defun number-start-p (text index &aux (ch (char text index)))
  (or (%digit-p ch)
      (and (char= ch #\.) (< (1+ index) (length text))
           (%digit-p (char text (1+ index))))))

(defun scan-run (text start extra)
  "End of the run of ASCII letters, digits and the characters of EXTRA that
opens at START."
  (or (position-if-not (lambda (ch) (or (%alnum-p ch) (find ch extra))) text :start start)
      (length text)))

(defun prefix-at-p (text index prefix &aux (end (+ index (length prefix))))
  (and (<= end (length text))
       (string= prefix text :start2 index :end2 end)))

(defmacro with-token-collector ((emit take index) &body body)
  "Bind EMIT to a (start end kind) collector that drops :plain and empty
runs, coalesces adjacent same-kind tokens, and stops at the token budget —
the Zig Tokens.append contract — INDEX to the scan cursor, from 0, and TAKE
to (end kind): emit INDEX..END as KIND and move the cursor to END."
  ;; The body's value is discarded; the collected token list returns.
  (let ((tokens (gensym "TOKENS")) (count (gensym "COUNT")))
    `(let ((,tokens '()) (,count 0) (,index 0))
       (flet ((,emit (start end kind)
                (unless (or (>= start end) (eq kind :plain))
                  (let ((last (first ,tokens)))
                    (cond ((and last
                                (= (syntax-token-end last) start)
                                (eq (syntax-token-kind last) kind))
                           (setf (syntax-token-end last) end))
                          ;; The Zig fixed-buffer bound: runs past 512 tokens stay plain.
                          ((< ,count 512)
                           (push (syntax-token start end kind) ,tokens)
                           (incf ,count)))))))
         (flet ((,take (end kind)
                  (,emit ,index end kind)
                  (setf ,index end)))
           ,@body))
       (nreverse ,tokens))))

(defun tokenize-line (line language)
  "Classified tokens over one code line under LANGUAGE
(Zig syntax_highlight.zig tokenizeLine), none when *LANGUAGE-PROFILES* does
not know LANGUAGE."
  ;; Unclassified runs are absent — they paint in the caller's base style.
  ;; A fence with no language, or one no profile names, is plain code: the
  ;; shared sets read its words as keywords, so a note in a bare ``` block
  ;; would paint "with" and "if" bold orange.
  (with-token-collector (emit take index)
    (let* ((len (length line))
           (known (assoc language *language-profiles*
                         :test (lambda (name aliases)
                                 (member name aliases :test #'string-equal))))
           (profile (cdr known)) end)
      (loop while (and known (< index len))
            do (cond
                 ((cond ((getf profile :hash-comments)
                         (and (char= (char line index) #\#)
                              (or (zerop index) (%whitespace-p (char line (1- index))))))
                        ((getf profile :sql-like)
                         (prefix-at-p line index "--"))
                        (t (prefix-at-p line index "//")))
                  (take len :comment))
                 ((prefix-at-p line index "/*")
                  (let ((close (search "*/" line :start2 (+ index 2))))
                    (take (if close (+ close 2) len) :comment)))
                 ((member (char line index) '(#\" #\' #\`))
                  (take (scan-quoted-string line index (char line index)) :string))
                 ;; Numbers keep the loose Zig shape: alphanumerics plus _ . + - so
                 ;; 0x1f, 1_000, and 1.5e-3 stay one token.
                 ((number-start-p line index)
                  (take (scan-run line index "_.+-") :number))
                 ((< index (setf end (scan-run line index "_$@")))
                  (let ((identifier (subseq line index end)))
                    (flet ((in (set) (and set (gethash identifier set))))
                      (take end
                            (cond ((gethash identifier *constant-set*) :constant)
                                  ((or (gethash identifier *keyword-set*)
                                       (in (getf profile :keywords)))
                                   :keyword)
                                  ((or (gethash identifier *builtin-type-set*)
                                       (char<= #\A (char identifier 0) #\Z))
                                   :type)
                                  ((or (gethash identifier *builtin-set*)
                                       (in (getf profile :builtins))
                                       (eql (char identifier 0) (getf profile :builtin-prefix)))
                                   :builtin)
                                  ((eql (find-if-not #'%whitespace-p line :start end) #\()
                                   :function)
                                  (t :plain))))))
                 (t (take (1+ index) (%char-kind (char line index)))))))))

;;; The shell tokenizer: tracks command position so the word after a
;;; pipeline/list separator colors as the command, and understands flags,
;;; $VARs, env-assignment prefixes, redirects, and # comments.

(defun scan-shell-variable (text start &aux (len (length text)))
  "End of the $-form opened at START: ${...}, special single-character
variables, or an identifier run (Zig scanVariable)."
  (cond ((>= (1+ start) len) (1+ start))
        ((char= (char text (1+ start)) #\{)
         (if-let (close (position #\} text :start (+ start 2))) (1+ close) len))
        ((find (char text (1+ start)) "?$!#@*") (+ start 2))
        (t (scan-run text (1+ start) "_"))))

(defun shell-operator-end (text index operators)
  "End of the first of OPERATORS, (text . length) rows, standing at INDEX, or NIL."
  (loop for (operator . length) in operators
        when (prefix-at-p text index operator)
          return (+ index length)))

;; The command/list separators, a separator shadowing any redirection it opens;
;; and the redirections, N>&M taking the descriptor after it.
(defparameter *shell-separators*
  '(("|&" . 2) ("&&" . 2) ("||" . 2) (";;" . 2) ("|" . 1) (";" . 1) ("&" . 1)))
(defparameter *shell-redirects*
  '((">>" . 2) ("<<" . 2) ("0>>" . 3) ("1>>" . 3) ("2>>" . 3) ("0>&" . 4) ("1>&" . 4) ("2>&" . 4)
    ("0>" . 2) ("1>" . 2) ("2>" . 2) (">" . 1) ("<" . 1)))

(defun tokenize-shell (text)
  "Classified tokens over full shell command TEXT, which may span newlines
(Zig syntax_highlight.zig tokenizeShell)."
  (with-token-collector (emit take index)
    (let ((len (length text))
          (command-position t) end)
      ;; ADVANCE takes the run, then sets the command position to POSITION.
      (flet ((advance (end kind position)
               (take end kind)
               (setf command-position position)))
        (loop
         while (< index len)
         do (let ((ch (char text index)))
              (cond
                ((blank-char-p ch)
                 (when (char= ch #\Newline) (setf command-position t))
                 (incf index))
                ((and (char= ch #\#) (or (zerop index) (blank-char-p (char text (1- index)))))
                 (take (or (position #\Newline text :start index) len) :comment))
                ((member ch '(#\" #\' #\`))
                 (advance (scan-quoted-string text index ch) :string nil))
                ((char= ch #\$)
                 ;; $( opens a command context; $VAR/${...} color as
                 ;; variables; a bare $ stays plain (the Zig fallthrough).
                 (cond ((prefix-at-p text index "$(") (advance (+ index 2) :operator t))
                       ((< (1+ index) (setf end (scan-shell-variable text index)))
                        (advance end :variable nil))
                       (t (incf index))))
                ((setf end (shell-operator-end text index *shell-separators*))
                 (advance end :operator t))
                ((setf end (shell-operator-end text index *shell-redirects*))
                 (take end :operator))
                ((member ch '(#\( #\{ #\) #\}))
                 (advance (1+ index) :punctuation (or (find ch "({") command-position)))
                ((number-start-p text index) (advance (scan-run text index "_.+-") :number nil))
                ((< index (setf end (scan-run text index "_-./+@~:=,%")))
                 (let* ((word (subseq text index end))
                        (eq-pos (position #\= word)))
                   (cond ((and (char= ch #\-) (not command-position)) (take end :flag))
                         ((and command-position eq-pos (plusp eq-pos)
                               (not (%digit-p (char word 0)))
                               (= eq-pos (scan-run word 0 "_")))
                          ;; FOO=bar prefix: the value stays plain and the
                          ;; real command is still ahead.
                          (emit index (+ index eq-pos) :variable)
                          (emit (+ index eq-pos) (+ index eq-pos 1) :operator)
                          (setf index end))
                         (command-position
                          (advance end (if (or (gethash word *keyword-set*)
                                               (gethash word *shell-keyword-set*))
                                           :keyword
                                           :command)
                                   (gethash word *command-prefix-set*)))
                         (t
                          (take end (if (gethash word *shell-keyword-set*)
                                        :keyword
                                        :plain))))))
                (t (take (1+ index) (%char-kind ch))))))))))
