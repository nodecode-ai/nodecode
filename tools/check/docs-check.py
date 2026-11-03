#!/usr/bin/env python3
# docs-check.py --- the reference under docs/ must still build the site it feeds.
#
# SPDX-License-Identifier: MIT
#
# usage: docs-check.py [ROOT]     (default: the root of this checkout)
#
# nodecode.ai/docs renders the pages in docs/: one docs/SLUG.mdx a page, in
# the reading order docs/nav.json gives, with the media under docs/images/.
# The site reads them at a commit of this repository, so a change here that
# breaks a page is only seen there, after it merged. This reads the pages the
# way the site does and fails the change that breaks one:
#
#   nav      nav.json reads, names no slug twice, and every slug it names has
#            docs/SLUG.mdx -- or carries its own title and summary (the
#            overview, which has no file); every docs/*.mdx is in it. The
#            .md files beside the pages (README, CELLS, UPGRADE) are this
#            repository's own notes and no page.
#   front    every page opens with frontmatter that has a title and a summary:
#            the page's masthead, its row in the rail and in llms.txt.
#   links    a link to /SLUG lands on a page, and a #anchor on a heading or id
#            of that page (ids as rehype-slug gives them, github-slugger's).
#   media    a picture a page shows (![](...), src=, poster=) is a file under
#            docs/images/: /docs/PATH or images/PATH names docs/images/PATH.
#   sources  every source path a page names (src/...lisp|asd|json,
#            tools/...sh|py, cells/...lisp|asd, nodecode.asd) is a file of this commit, or of
#            the shipped cell it falls in, as tools/build/pull-cells.sh lays
#            it down from cells/bundle.json. A code move that leaves a page
#            naming the old path fails here, in its own pull request.
#   dates    dates.json, when present, dates only pages, as YYYY-MM-DD.
#   release  changelog.mdx has a `## X.Y.Z` section for the version
#            nodecode.asd builds, so the commit a release is tagged at
#            carries what changed in it, and the page cannot fall behind.
#
# Each problem is one line, FILE:LINE: what is wrong; the last line counts
# what was read. Exit 0 when nothing is wrong, 1 when something is, 2 when the
# check cannot run. Python 3's standard library only, as pull-cells.sh needs.
import json
import os
import re
import subprocess
import sys

ROOT = os.path.abspath(sys.argv[1] if len(sys.argv) > 1 else os.path.join(os.path.dirname(__file__), "..", ".."))
DOCS = os.path.join(ROOT, "docs")
problems = []


def problem(path, line, text):
    problems.append(f"{os.path.relpath(path, ROOT)}:{line}: {text}")


# --- nav ----------------------------------------------------------------------


def nav_entries(node, out):
    """Every page entry of the nav, in order, wherever its groups and folds
    nest it: a bare string is a slug, an object with a "slug" is one."""
    if isinstance(node, list):
        for item in node:
            if isinstance(item, str):
                out.append({"slug": item})
            else:
                nav_entries(item, out)
    elif isinstance(node, dict):
        if isinstance(node.get("slug"), str):
            out.append(node)
        for value in node.values():
            if isinstance(value, (list, dict)):
                nav_entries(value, out)
    return out


nav_path = os.path.join(DOCS, "nav.json")
pages = sorted(name[:-4] for name in os.listdir(DOCS) if name.endswith(".mdx")) if os.path.isdir(DOCS) else []
if not os.path.exists(nav_path):
    if pages:
        problem(nav_path, 1, f"missing, and docs/ holds {len(pages)} pages: the site has no order to read them in")
    entries = []
else:
    try:
        with open(nav_path, encoding="utf-8") as stream:
            nav = json.load(stream)
    except (ValueError, UnicodeDecodeError) as error:
        problem(nav_path, getattr(error, "lineno", 1), f"does not read as JSON: {error}")
        nav = []
    entries = nav_entries(nav, [])

nav_text = open(nav_path, encoding="utf-8", errors="replace").read() if os.path.exists(nav_path) else ""


def nav_line(slug):
    at = nav_text.find(json.dumps(slug))
    return nav_text.count("\n", 0, at) + 1 if at >= 0 else 1


slugs = set()
for entry in entries:
    slug = entry["slug"]
    if slug in slugs:
        problem(nav_path, nav_line(slug), f"names {json.dumps(slug)} twice")
    slugs.add(slug)
    if slug not in pages and not (entry.get("title") and entry.get("summary")):
        problem(nav_path, nav_line(slug), f"names {json.dumps(slug)}, and there is no docs/{slug}.mdx (nor a title and summary of its own)")
for slug in pages:
    if slug not in slugs:
        problem(os.path.join(DOCS, slug + ".mdx"), 1, "is a page no nav.json entry names: the site never shows it")

# --- the pages, read once -----------------------------------------------------

FENCE = re.compile(r"^\s*(```|~~~)")
HEADING = re.compile(r"^(#{1,6})\s+(.*?)\s*#*\s*$")
INLINE_CODE = re.compile(r"(`+)(.+?)\1")


def github_slug(text):
    """github-slugger's slug of a heading's text, rehype-slug's id."""
    text = text.lower()
    return "".join(c for c in text if c.isalnum() or c in " -_").replace(" ", "-")


def heading_text(markdown):
    """A heading's text content as the site renders it: code, links and marks
    reduced to their words, tags and MDX comments dropped."""
    def words(text):
        text = re.sub(r"!?\[([^\]]*)\]\([^)]*\)", r"\1", text)
        text = re.sub(r"</?[A-Za-z][^>]*>", "", text)
        text = re.sub(r"(\*+|~~|(?<!\w)_+|_+(?!\w))", "", text)
        return re.sub(r"\\(.)", r"\1", text)

    text, out, at = re.sub(r"\{/\*.*?\*/\}", "", markdown), [], 0
    for code in INLINE_CODE.finditer(text):
        out += [words(text[at:code.start()]), code.group(2).strip()]
        at = code.end()
    return "".join(out) + words(text[at:])


def read_page(slug):
    path = os.path.join(DOCS, slug + ".mdx")
    with open(path, encoding="utf-8") as stream:
        lines = stream.read().split("\n")
    prose, ids, seen, fence = [], set(), {}, None
    for number, line in enumerate(lines, 1):
        opened = FENCE.match(line)
        if fence:
            if opened and opened.group(1) == fence:
                fence = None
            continue
        if opened:
            fence = opened.group(1)
            continue
        heading = HEADING.match(line)
        if heading:
            base = github_slug(heading_text(heading.group(2)))
            result = base
            while result in seen:
                seen[base] += 1
                result = f"{base}-{seen[base]}"
            seen[result] = 0
            ids.add(result)
        ids.update(re.findall(r'\bid=["\']([^"\']+)["\']', line))
        prose.append((number, INLINE_CODE.sub("", line)))
    return path, lines, prose, ids


read = {}
for slug in pages:
    try:
        read[slug] = read_page(slug)
    except UnicodeDecodeError as error:
        problem(os.path.join(DOCS, slug + ".mdx"), 1, f"is not UTF-8: {error}")

# --- front --------------------------------------------------------------------

for path, lines, _, _ in read.values():
    if not lines or lines[0].strip() != "---":
        problem(path, 1, "has no frontmatter: the first line must be ---")
        continue
    end = next((i for i in range(1, len(lines)) if lines[i].strip() == "---"), None)
    if end is None:
        problem(path, 1, "frontmatter opened on line 1 is never closed by ---")
        continue
    fields = {}
    for line in lines[1:end]:
        field = re.match(r"^([A-Za-z_][\w-]*):\s*(.*)$", line)
        if field:
            fields[field.group(1)] = field.group(2).strip().strip("\"'").strip()
    for key in ("title", "summary"):
        if not fields.get(key):
            problem(path, 1, f"frontmatter has no {key}: the site's masthead, rail and llms.txt read it")

# --- links and media ----------------------------------------------------------

LINK = re.compile(r"(?<!!)\[[^\]]*\]\(\s*([^)\s]+)[^)]*\)|\bhref=[\"']([^\"']+)[\"']")
MEDIA = re.compile(r"!\[[^\]]*\]\(\s*([^)\s]+)[^)]*\)|\b(?:src|poster)=[\"']([^\"']+)[\"']")
EXTERNAL = re.compile(r"^(?:[a-z][a-z0-9+.-]*:|//)", re.I)
IMAGES = os.path.join(DOCS, "images")
links = media = 0

for path, _, prose, ids in read.values():
    for number, line in prose:
        for match in LINK.finditer(line):
            target = match.group(1) or match.group(2)
            if EXTERNAL.match(target):
                continue
            links += 1
            page, _, anchor = target.partition("#")
            if page == "":
                if anchor and anchor not in ids:
                    problem(path, number, f"links to #{anchor}, and this page has no heading or id {anchor}")
                continue
            if not page.startswith("/"):
                problem(path, number, f"links to {target}: a page is linked as /SLUG, the way the site serves it")
                continue
            name = page.strip("/")
            name = name[:-3] if name.endswith(".md") else name
            if name.startswith("docs/") or name == "docs":
                problem(path, number, f"links to {target}: write /SLUG, the site adds /docs itself")
            elif name not in slugs:
                problem(path, number, f"links to {target}, and nav.json names no page {name}")
            elif anchor and name in read and anchor not in read[name][3]:
                problem(path, number, f"links to {target}, and {name} has no heading or id {anchor}")
        for match in MEDIA.finditer(line):
            target = match.group(1) or match.group(2)
            if EXTERNAL.match(target) or target.startswith("data:"):
                continue
            media += 1
            rest = re.sub(r"^(?:/docs/|\./images/|images/)", "", target.split("?")[0].split("#")[0])
            if rest == target.split("?")[0].split("#")[0]:
                problem(path, number, f"shows {target}: a picture is /docs/PATH or images/PATH, a file under docs/images/")
            elif not os.path.isfile(os.path.join(IMAGES, rest)):
                problem(path, number, f"shows {target}, and there is no docs/images/{rest}")

# --- sources ------------------------------------------------------------------

# Rooted at the repository, as the site's own check reads them: a src/ or
# cells/ after another segment (lisp/src/..., ~/.nodecode/cells/...) is some
# other tree's.
SOURCE = re.compile(r"(?<![\w./~-])(?:src/[\w./-]+?\.(?:lisp|asd|json)|tools/[\w./-]+?\.(?:sh|py)|cells/[\w./-]+?\.(?:lisp|asd)|nodecode(?:-perf)?\.asd)\b")
try:
    tracked = set(subprocess.run(["git", "-C", ROOT, "ls-files", "-z"], check=True, capture_output=True).stdout.decode().split("\0"))
except (OSError, subprocess.CalledProcessError) as error:
    print(f"docs-check: cannot list the files of {ROOT}: {error}", file=sys.stderr)
    sys.exit(2)
bundle_path = os.path.join(ROOT, "cells", "bundle.json")
bundle = json.load(open(bundle_path, encoding="utf-8")) if os.path.exists(bundle_path) else {"cells": {}}
sources = 0

for path, lines, _, _ in read.values():
    for number, line in enumerate(lines, 1):
        for match in SOURCE.finditer(line):
            source = match.group(0)
            sources += 1
            if source in tracked:
                continue
            rest = source[len("cells/"):] if source.startswith("cells/") else None
            cell = rest and max((key for key in bundle["cells"] if rest.startswith(key + "/")), key=len, default=None)
            if not cell:
                problem(path, number, f"names {source}, which is no file of this commit")
            elif not os.path.isfile(os.path.join(ROOT, "cells", cell, ".bundle")):
                problem(path, number, f"names {source}, in the shipped cell {cell}, which is not laid down here: run just cells-pull")
            elif not os.path.isfile(os.path.join(ROOT, source)):
                problem(path, number, f"names {source}, which the shipped cell {cell} has no file for at the pinned commit")

# --- dates --------------------------------------------------------------------

dates_path = os.path.join(DOCS, "dates.json")
if os.path.exists(dates_path):
    try:
        dates = json.load(open(dates_path, encoding="utf-8"))
    except ValueError as error:
        problem(dates_path, getattr(error, "lineno", 1), f"does not read as JSON: {error}")
        dates = {}
    for slug, date in (dates.items() if isinstance(dates, dict) else []):
        if slug not in pages:
            problem(dates_path, 1, f"dates {json.dumps(slug)}, and there is no docs/{slug}.mdx")
        elif not (isinstance(date, str) and re.fullmatch(r"\d{4}-\d{2}-\d{2}", date)):
            problem(dates_path, 1, f"dates {slug} as {json.dumps(date)}, not YYYY-MM-DD")

# --- release ------------------------------------------------------------------

asd_path = os.path.join(ROOT, "nodecode.asd")
changelog = "changelog"
if os.path.exists(asd_path) and changelog in read:
    version = re.search(r'^\s*:version\s+"([^"]+)"', open(asd_path, encoding="utf-8").read(), re.M)
    path, lines, _, _ = read[changelog]
    if not version:
        problem(asd_path, 1, "has no :version string")
    elif not any(line.strip() == "## " + version.group(1) for line in lines):
        problem(path, 1, f"has no '## {version.group(1)}' section, and nodecode.asd builds {version.group(1)}: say what changed in it")

for line in problems:
    print(line)
summary = f"{len(pages)} pages, {len(entries)} nav entries, {links} links, {media} pictures, {sources} source paths"
if problems:
    print(f"docs-check: {len(problems)} problems in {summary}")
    sys.exit(1)
print(f"docs-check: {summary}, all good")
