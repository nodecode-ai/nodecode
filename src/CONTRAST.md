# Terminal chrome contrast coverage

The contrast safeguard runs after segment styles, fills, dimming and reasoning
shadows are composed. Extended-color text targets 4.5:1 against the emitted
surface; ANSI-256 checks use quantized colors. This is a text readability
contract, not a requirement that every decorative pixel pass 4.5:1.

## Inventory

`test/tui/chrome-contrast-test.lisp` exercises actual chrome painters and parses
their ANSI output with the production terminal-cell parser. The matrix is all
four bundled themes, opaque backgrounds, and truecolor / ANSI-256:

| Surface | Painter / path | Checked content |
| --- | --- | --- |
| Session header and status | `bilateral-frame-row`, `header-status-segs` | title, usage and model/status roles |
| Footer | `footer-frame-row` | footer text and keyboard hints |
| Composer | `composer-rows` | placeholder, typed draft and in-flight steering |
| Docked picker | `docked-dialog-header-rows`, `docked-dialog-list-rows` | title, subtitle, search, selected and unselected labels/details |
| Notices | `toast-rows` | normal/error, hovered/unhovered text |
| Transcript drag selection | `selection-overlay` applied to parsed rendered rows | muted, quiet code, foreground, link and danger text |

Chrome text in the first five rows reaches `seg-render` through its segment
painters; no separate palette rewrite was needed. Tests inspect alphanumeric
cells so borders, rails and spacing are not mistaken for text. The selection
boundary test additionally covers punctuation, a wide glyph, OSC8 links and
text emphasis.

## Demonstrated extra seam: selection

Drag selection changes cell backgrounds *after* `seg-render`. The prior guard
could therefore pass text before selection and fail after the wash. The new
regression fails on the parent contrast commit (observed ratios include about
3.59:1 for selected text and 4.43:1 for quiet code).

The overlay now reuses the same foreground correction on its newly emitted
surface. Already-readable foregrounds remain unchanged. It preserves glyphs,
selection geometry, links and non-dim attributes, clearing terminal faint on
supported selected extended-color text. Per-overlay foreground/background caching avoids
repeating the color search for every selected character.

## Deliberate limits

- The test matrix is not a claim of complete application or visual approval.
  It does not enumerate every slash command, tool-specific panel or custom theme.
- Transparent unknown terminal canvases and basic ANSI/default foregrounds are
  terminal-defined (including indexed `38;5;0` through `38;5;15`). They are not assigned invented RGB values by the new ink
  correction. The pre-existing selection wash still uses its nominal ground
  when no detected canvas is available.
- Reverse-video selection cells are left alone rather than misreading their
  effective foreground/background. They are outside this guarantee.
- Spaces, wide-glyph continuation cells, Unicode box-drawing and block-element
  decoration are not recolored by the selection text correction.
- Markdown rules/gutters, scroll rails, notice edges and composer ornaments are
  decorations, not text contrast failures.
- Modal inactive-backdrop dimming and transient fade/ember/glint animation
  overlays intentionally alter appearance and are not certified by this matrix.
- User terminal opacity, font rasterization and compositor behavior still need
  a real-terminal visual check; parsed ANSI cells do not prove those properties.

Run the core suite with `just test` (including these regressions), and structural
checks with `just lint`. This change depends on the composed-text contrast
safeguard rather than duplicating its color math.
