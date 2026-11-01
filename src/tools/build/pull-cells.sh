#!/usr/bin/env bash
# pull-cells.sh --- lay down the shipped cells: every folder the release builds in.
#
# usage: pull-cells.sh [CELLS_DIR]    (default: src/cells beside this script)
#
# CELLS_DIR/bundle.json names a repository, the commit it is read at, and the
# folders taken from it, each at the shipped path it takes here:
#   {"git": URL, "commit": SHA, "cells": {"channels/discord": "rooms/nodecode-channel-discord", ...}}
# Each folder is laid down from that commit and marked (.bundle: the URL, the
# commit, the path in the repository), so a run at the same pin does nothing
# and a moved pin replaces the folder whole. The source of a shipped cell is
# in that repository, never here: inside a git checkout every local path must
# be git-ignored, and the run refuses one that is not. The organism reads the
# folder as any shipped one (SHIPPED-CELLS), and a hub index never lists a
# shipped name.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cells="${1:-$here/src/cells}"
manifest="$cells/bundle.json"
test -f "$manifest" || { printf 'pull-cells: no %s\n' "$manifest" >&2; exit 2; }
command -v git >/dev/null 2>&1 || { printf 'pull-cells: git is needed\n' >&2; exit 2; }
py="$(command -v python3 || command -v python || true)"
test -n "$py" || { printf 'pull-cells: python3 is needed to read %s\n' "$manifest" >&2; exit 2; }

# the manifest as lines: URL, COMMIT, then one "LOCAL HUB" a folder -- read
# line by line, since a Mac's /bin/bash is 3.2 and has no mapfile
lines=()
while IFS= read -r line; do lines+=("$line"); done < <("$py" - "$manifest" <<'PY'
import json, re, sys
d = json.load(open(sys.argv[1]))
assert d["git"].startswith("https://"), d["git"]
assert re.fullmatch(r"[0-9a-f]{40}", d["commit"]), d["commit"]
print(d["git"]); print(d["commit"])
for local, hub in d["cells"].items():
    assert re.fullmatch(r"[A-Za-z0-9_][A-Za-z0-9._-]*(/[A-Za-z0-9_][A-Za-z0-9._-]*)*", local), local
    assert re.fullmatch(r"[A-Za-z0-9_][A-Za-z0-9._-]*(/[A-Za-z0-9_][A-Za-z0-9._-]*)*", hub), hub
    print(local, hub)
PY
)
url="${lines[0]}"; commit="${lines[1]}"; pairs=("${lines[@]:2}")

if git -C "$cells" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  for pair in "${pairs[@]}"; do
    local_path="${pair%% *}"
    # the pattern names a directory (a trailing slash), which an absent path
    # only matches when asked for as one
    git -C "$cells" check-ignore -q "$local_path/" || {
      printf 'pull-cells: %s is not git-ignored; its source lives at %s, add src/cells/%s/ to .gitignore\n' \
        "$local_path" "$url" "$local_path" >&2; exit 1; }
  done
fi

# A folder laid down here (its .bundle marker) that the manifest no longer
# names is removed: left in place it would still load at every launch and
# every release's cell compile.
named=" "
for pair in "${pairs[@]}"; do named+="${pair%% *} "; done
while IFS= read -r marker; do
  dir="$(dirname "$marker")"; local_path="${dir#"$cells"/}"
  case "$named" in *" $local_path "*) continue ;; esac
  # a marker inside a named folder belongs to that folder's own content
  for pair in "${pairs[@]}"; do
    case "$local_path/" in "${pair%% *}/"*) continue 2 ;; esac
  done
  if git -C "$cells" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    git -C "$cells" check-ignore -q "$local_path/" || continue
  fi
  rm -rf "${dir:?}"
  parent="$(dirname "$dir")"
  while [ "$parent" != "$cells" ] && rmdir "$parent" 2>/dev/null; do parent="$(dirname "$parent")"; done
  printf 'pull-cells: %s removed, the manifest no longer names it\n' "$local_path"
done < <(find "$cells" -mindepth 2 -name .bundle -type f)

need=()
for pair in "${pairs[@]}"; do
  local_path="${pair%% *}"; hub_path="${pair#* }"
  marker="$cells/$local_path/.bundle"
  # the marker stands for the folder only while the folder is there: a commit
  # that took a pulled folder's tracked copy out of git deleted its files and
  # left the ignored marker, and a run at the same pin pulled nothing back
  if [ -f "$marker" ] && [ "$(sed -n 1p "$marker")" = "$url" ] && [ "$(sed -n 2p "$marker")" = "$commit" ] \
     && [ "$(sed -n 3p "$marker")" = "$hub_path" ] && [ -n "$(ls "$cells/$local_path"/*.asd 2>/dev/null)" ]; then
    continue
  fi
  need+=("$pair")
done
if [ "${#need[@]}" -eq 0 ]; then
  printf 'pull-cells: every shipped cell is at %s\n' "${commit:0:9}"; exit 0
fi

work="$(mktemp -d)"; trap 'rm -rf "$work"' EXIT
git init -q "$work/repo"
git -C "$work/repo" fetch -q --depth 1 "$url" "$commit"
for pair in "${need[@]}"; do
  local_path="${pair%% *}"; hub_path="${pair#* }"
  depth="$(printf '%s' "$hub_path" | awk -F/ '{print NF}')"
  rm -rf "${cells:?}/$local_path"; mkdir -p "$cells/$local_path"
  git -C "$work/repo" archive FETCH_HEAD "$hub_path" | tar -x --strip-components="$depth" -C "$cells/$local_path"
  test -n "$(ls "$cells/$local_path"/*.asd 2>/dev/null)" || { printf 'pull-cells: %s at %s has no .asd\n' "$hub_path" "${commit:0:9}" >&2; exit 1; }
  printf '%s\n%s\n%s\n' "$url" "$commit" "$hub_path" > "$cells/$local_path/.bundle"
  printf 'pull-cells: %s <- %s at %s\n' "$local_path" "$hub_path" "${commit:0:9}"
done
