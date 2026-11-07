#!/usr/bin/env python3
"""table-png.py --- draw a room table as a PNG.

Invoked by the channel kit (tables.lisp) when the answer a turn is about
to deliver carries a GitHub-style table: ROWS is a TSV file, the first
row the header -- cells never carry tabs or newlines; OUT is where the
picture lands. Pure Pillow, no network.

Exit codes: 0 drawn; 2 usage/bad input; 3 no monospace font; 4 the table
is too wide to draw at the smallest size (the kit falls back to a fenced
code block, so a refusal is a code, never a traceback).
"""
import re
import sys

from PIL import Image, ImageDraw, ImageFont

FONTS = (
    "/usr/share/fonts/TTF/DejaVuSansMono.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
    "/usr/share/fonts/liberation/LiberationMono-Regular.ttf",
    "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf",
    "/usr/share/fonts/adobe-source-code-pro/SourceCodePro-Regular.otf",
)
BOLD_FONTS = (
    "/usr/share/fonts/TTF/DejaVuSansMono-Bold.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf",
    "/usr/share/fonts/liberation/LiberationMono-Bold.ttf",
    "/usr/share/fonts/truetype/liberation/LiberationMono-Bold.ttf",
    "/usr/share/fonts/adobe-source-code-pro/SourceCodePro-Bold.otf",
)
SIZES = (24, 21, 18, 16)
CARD = (43, 45, 49, 255)
HEAD_COLOR = (242, 243, 245, 255)
BODY_COLOR = (219, 222, 225, 255)
RULE = (63, 65, 71, 255)
PAD_X, PAD_Y, GAP, ROW_PAD, RADIUS = 20, 14, 30, 11, 12
MAX_WIDTH = 1800
NUMERIC = re.compile(r"^[~+-]?[¥$€£]?\d[\d.,]*(?:[kmb%])?$", re.IGNORECASE)


def load_font(candidates, size):
    for path in candidates:
        try:
            return ImageFont.truetype(path, size)
        except OSError:
            continue
    return None


def measure(rows, font, bold):
    widths = [0.0] * len(rows[0])
    for index, row in enumerate(rows):
        face = bold if index == 0 else font
        for col, cell in enumerate(row):
            widths[col] = max(widths[col], face.getlength(cell))
    return [int(width + 0.5) for width in widths]


def draw(rows, font, bold, widths, out):
    size = font.size
    head_h = size + 2 * ROW_PAD + 8
    row_h = size + 2 * ROW_PAD
    width = PAD_X * 2 + sum(widths) + GAP * (len(widths) - 1)
    height = PAD_Y * 2 + head_h + row_h * (len(rows) - 1)
    image = Image.new("RGBA", (width, height), (0, 0, 0, 0))
    painter = ImageDraw.Draw(image)
    painter.rounded_rectangle([0, 0, width - 1, height - 1], RADIUS, fill=CARD)
    numeric = [
        col > 0 and all(NUMERIC.match(row[col].strip()) for row in rows[1:])
        for col in range(len(widths))
    ]
    x = PAD_X
    for col, cell in enumerate(rows[0]):
        painter.text((x, PAD_Y + ROW_PAD), cell, font=bold, fill=HEAD_COLOR)
        x += widths[col] + GAP
    rule_y = PAD_Y + head_h
    painter.line([(0, rule_y), (width, rule_y)], fill=RULE, width=1)
    for index, row in enumerate(rows[1:], start=1):
        y = PAD_Y + head_h + ROW_PAD + (index - 1) * row_h
        x = PAD_X
        for col, cell in enumerate(row):
            offset = int(widths[col] - font.getlength(cell)) if numeric[col] else 0
            painter.text((x + offset, y), cell, font=font, fill=BODY_COLOR)
            x += widths[col] + GAP
    image.save(out, "PNG", optimize=True)


def main(argv):
    if len(argv) != 3:
        print("usage: table-png.py ROWS.tsv OUT.png", file=sys.stderr)
        return 2
    with open(argv[1], "r", encoding="utf-8") as handle:
        rows = [line.split("\t") for line in handle.read().split("\n") if line != ""]
    if not rows:
        print("no rows", file=sys.stderr)
        return 2
    cols = max(len(row) for row in rows)
    rows = [row + [""] * (cols - len(row)) for row in rows]
    for size in SIZES:
        font = load_font(FONTS, size)
        if font is None:
            print("no monospace font", file=sys.stderr)
            return 3
        bold = load_font(BOLD_FONTS, size) or font
        widths = measure(rows, font, bold)
        if PAD_X * 2 + sum(widths) + GAP * (cols - 1) <= MAX_WIDTH:
            draw(rows, font, bold, widths, argv[2])
            return 0
    print("table too wide", file=sys.stderr)
    return 4


if __name__ == "__main__":
    sys.exit(main(sys.argv))
