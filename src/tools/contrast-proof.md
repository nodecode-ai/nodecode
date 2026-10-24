# Reproduce composed-text contrast evidence

These opt-in tools load the test system and reuse its ANSI terminal-cell decoder.
They do not start a Nodecode session, call a provider, alter a running image, or
change terminal settings. Run them in a fresh SBCL process, never an attached UI:
the benchmark temporarily replaces `readable-text-style` with `identity` to
measure the pre-correction renderer, restoring it with `unwind-protect`.

From `src/`, with the project's dependencies already available:

```sh
sbcl --dynamic-space-size 4096 --control-stack-size 8MB --noinform \
  --load /path/to/quicklisp/setup.lisp \
  --script tools/prove-text-contrast.lisp /tmp/contrast-proof
python3 tools/render-contrast-proof.py \
  /tmp/contrast-proof-cells.tsv /tmp/contrast-proof.html
```

Replace the Quicklisp path with the existing dependency bootstrap on your machine;
`--script` suppresses normal init-file loading. No dependency installation is
performed by these scripts. The renderer only needs Python's standard library.
Open the resulting HTML locally to inspect dark/light, truecolor/256-color
before/after specimens. The full TSV contains 480 cases: four bundled themes,
two profiles, five surfaces, six roles, and normal/shadowed text. Tungstene's
transparent canvas is explicitly supplied as RGB(28,31,38), not discovered from
the terminal.

## What this establishes

The TSV records the emitted foreground/background RGB and contrast ratio from
parsed terminal cells. The HTML displays those RGB values, including xterm
palette quantization, and selects 32 representative cases. It is a browser
specimen, **not a terminal screenshot or whole-application visual approval**.
Terminal font rasterization, terminal-specific dim behavior and custom ANSI
palettes are outside this proof. Normal primary/muted canvas specimens make it
possible to check that text hierarchy has not visibly collapsed; arbitrary
surfaces cannot always preserve every hierarchy distinction while satisfying a
common readability floor.

The timed workload calls the complete `seg-render`, including string generation:

- Three alternating baseline/warm rounds, each with 960,000 calls across the full
  matrix. Warm runs prepopulate the cache after clearing it.
- 48,000 cold calls, clearing the cache on every call (including clear cost).
- 48,000 arbitrary inline-color calls in 256-color mode, exceeding the 4,096-entry
  cache capacity to expose churn rather than only ideal cache hits.

These are synthetic microbenchmarks, not a whole-app latency or throughput test.
The baseline bypasses the final guard, not the rest of the renderer. Allocation
and timing depend on SBCL, compilation and GC; a faster warm sample is **not**
evidence that adding this guard speeds up the application. Cold/churn costs
must also be reported. Do not run the benchmark concurrently with other tests
when comparing numbers.

## Scope and rationale

The reference design lesson is to test semantic roles against their **composed
surfaces**, including shadow/dimming, rather than assume a palette checked only
against the canvas is sufficient. Existing Nodecode semantic palette roles are
retained. The guard is deliberately a final text safeguard, not a replacement
palette or a universal chrome auditor. It preserves already-readable colors and
non-dim emphasis; it excludes Markdown rule/gutter decorations. Unknown terminal
backgrounds and basic ANSI/no-color profiles remain terminal-dependent. Broader
chrome coverage and a developer audit command belong in independent follow-ups.
