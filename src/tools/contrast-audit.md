# Rendered terminal contrast diagnostics (developer-only)

This opt-in tool inspects the **final composed cells**, not palette tokens. It
reports text below a configurable contrast ratio (default **4.5:1**), together
with zero-based coordinates and counts of checked, passed, failed, skipped and
ignored cells. It never adjusts colors, hooks rendering, loads configuration,
starts a gateway, or writes files. It is not loaded by the application.

From the repository's `src/` directory, with normal build dependencies available:

```lisp
(require :asdf)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system :nodecode/contrast-audit)

;; ROWS: a list of complete rendered frame row strings (including SGR).
;; WIDTH: the same terminal width used when composing that frame.
(let ((report (nodecode.contrast-audit:audit-rows rows width
                :background '(20 20 20)
                :foreground '(235 238 244))))
  (princ (nodecode.contrast-audit:format-report report))
  report)
```

Supply defaults only when they are known to match the actual terminal/frame.
Do **not** substitute a theme's nominal canvas for an unknown transparent terminal
background. Omit unknown defaults: affected cells are explicitly `:skipped`, not
passed. Explicit per-cell backgrounds and foregrounds override defaults.

For an already layered screen, call `audit-buffer` with the final
`nodecode.term` screen buffer instead of re-parsing rows. This is the best seam
for overlay diagnostics: use the buffer **after all layers have been painted**.
`audit-rows` uses the existing terminal cell parser; it is not a terminal emulator
and must not be given arbitrary cursor-motion terminal logs.

## Options and results

Both audit entry points accept:

- `:threshold` — a number from 1 to 21; default 4.5. A threshold change is a
  diagnostic choice, not an accessibility certification.
- `:foreground`, `:background` — known terminal-default RGB lists, or NIL.
- `:ansi-palette` — the terminal's **actual** 16 system RGB colors (a list or
  vector). Without this, basic ANSI colors and indexed colors 0–15 are unknown;
  the tool does not silently assume xterm defaults. Indexed colors 16–255 use
  the xterm cube/ramp convention; RGB SGR is measured directly.
- `:faint-factor` — NIL by default: SGR 2 text is skipped because terminals do
  not have one universal dimming rule. A number 0–1 explicitly enables an
  **approximate model**, interpolating sRGB ink toward its actual background.
  Reports label that assumption. Supply a measured terminal-specific factor
  only when that model is appropriate; do not treat a model as observed pixels.
- `:exclude` — a `(lambda (cell x y) ...)` predicate marking known decorative
  cells. It returns true to ignore that cell. There is no reliable way to infer
  from a flattened glyph whether a line or symbol is semantic text or decoration.

The result is a plist with `:threshold`, `:checked`, `:passed`, `:failed`,
`:skipped`, `:ignored`, `:faint-factor`, and `:issues`. Issues contain `:x`, `:y`,
`:char`, `:status`, `:reason`, plus `:fg`, `:bg` and `:ratio` for failures.
`format-report` returns plain text without replaying source escape sequences.
A report with skipped cells is **incomplete**, even when `:failed` is zero.

Spaces and continuation halves of wide glyphs are ignored automatically; wide
glyphs are measured once. Reverse-video swaps effective foreground/background.
Blink is checked in its visible phase. Rendering/font rasterization, anti-aliasing,
color management and terminal-specific palette remapping are outside a cell-level
audit. Bold cells referencing system colors are skipped (`:terminal-bold`) even
with an explicit palette: terminals disagree on bold-to-bright remapping. Bold
truecolor and xterm cube/ramp colors are measured as emitted. Decorations are
checked as text unless explicitly excluded. This tool diagnoses rendered color contracts, not WCAG conformance of
an entire terminal application or visual approval on a real terminal.

Example exclusion for a border whose coordinates your renderer knows:

```lisp
(nodecode.contrast-audit:audit-buffer final-buffer
  :background '(20 20 20)
  :threshold 4.5d0
  :exclude (lambda (cell x y)
             (declare (ignore cell x))
             (= y border-row)))
```

## Tests

```lisp
(asdf:test-system :nodecode/contrast-audit)
```

This loads the existing hermetic test runner but runs only tests named
`CONTRAST-AUDIT-*`. The normal `nodecode` application and core test suite do not
load the diagnostic system. Tests cover failures and passing text, configurable
thresholds, explicit defaults, unknown colors, system palette opt-in, xterm-256,
reverse video, faint uncertainty/modeling, wide glyphs, decoration exclusions,
coordinate reporting, input validation, non-mutation of the source buffer, actual
`seg-render` output across all four bundled themes and truecolor/256-color modes,
and overlay inheritance of a painted background rather than terminal defaults.
