#!/usr/bin/env python3
"""Render prove-text-contrast.lisp's TSV as HTML; not a terminal screenshot.

Usage: python3 tools/render-contrast-proof.py PREFIX-cells.tsv OUTPUT.html
No third-party packages, network, providers or terminal settings are used.
"""
import csv
import html
import sys
from pathlib import Path


def render(source, destination):
    with Path(source).open(newline="") as stream:
        rows = list(csv.DictReader(stream, delimiter="\t"))
    chosen = [
        ("NIL", "FOREGROUND", "NIL"),
        ("NIL", "MUTED", "NIL"),
        ("NIL", "THOUGHT-STRONG", "T"),
        ("TITLE-BAND", "CODE-QUIET", "NIL"),
        ("TITLE-BAND", "MUTED", "T"),
        ("USER-BLOCK", "LINK", "NIL"),
        ("SLAB", "INLINE-CODE", "NIL"),
        ("COMPOSER", "MUTED", "NIL"),
    ]
    output = ["""<!doctype html><meta charset="utf-8"><title>Composed text contrast proof</title>
<style>
body{font:15px system-ui,sans-serif;background:#e8eaed;color:#20242b;margin:24px;max-width:1320px}
h1{font-size:26px;margin-bottom:8px}h2{margin:24px 0 8px}p{max-width:1150px;line-height:1.45}
table{border-collapse:collapse;width:100%;table-layout:fixed;background:white}
th,td{text-align:left;padding:7px 12px;border:1px solid #c9cdd3}th:first-child{width:23%}
.sample{font:16px ui-monospace,'DejaVu Sans Mono',monospace;padding:11px 12px;white-space:nowrap}
label{font-size:12px}.ratio{font-size:12px;display:block;margin-top:4px}.fail{color:#9b1919}
</style><h1>Composed text: before / after</h1>
<p><b>Parsed terminal-cell RGB, displayed in a browser—not live terminal screenshots.</b>
Before disables only the final contrast correction. After uses the committed renderer.
Tungstene's transparent canvas is explicitly supplied as RGB(28,31,38).
256-color samples show the emitted xterm palette colors, not unquantized source colors.
Browser font rasterization differs from terminal rendering; this does not prove terminal visual approval.</p>"""]
    for theme in ("Tungstene", "Blueprint Daylight"):
        for profile in ("TRUECOLOR", "ANSI256"):
            output += [f"<h2>{html.escape(theme)} · {profile}</h2>",
                       "<table><tr><th>Role / surface</th><th>Before</th><th>After</th></tr>"]
            for ground, tone, shadow in chosen:
                row = next(r for r in rows if (r["theme"], r["profile"], r["ground"], r["tone"], r["shadow"]) == (theme, profile, ground, tone, shadow))
                label = f"{tone.lower()} / {ground.lower()}" + (" · shadow" if shadow == "T" else "")
                output.append(f"<tr><td><label>{html.escape(label)}</label></td>")
                for phase in ("before", "after"):
                    ratio = float(row[f"{phase}_ratio"])
                    cls = "fail" if ratio < 4.5 else ""
                    output.append(f'<td><div class="sample" style="color:rgb({row[phase + "_fg"]});background:rgb({row["bg"]})">Readable text / code</div><span class="ratio {cls}">{ratio:.2f}:1 · RGB({row[phase + "_fg"]})</span></td>')
                output.append("</tr>")
            output.append("</table>")
    failures = sum(float(r["after_ratio"]) < 4.5 for r in rows)
    output.append(f"<p>Full matrix: {len(rows)} cases; {failures} after-correction failures below 4.5:1. Contact sheet shows 32 representative cases.</p>")
    Path(destination).write_text("\n".join(output), encoding="utf-8")


if __name__ == "__main__":
    if len(sys.argv) != 3:
        raise SystemExit(__doc__)
    render(*sys.argv[1:])
