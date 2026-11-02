#!/usr/bin/env bash
# latency.sh BASE [latency.py options] -- hold this checkout's latency and memory
# to BASE's.
#
# Builds this checkout as it stands (uncommitted edits included) and BASE, a
# commit, which is built once and kept under $NODECODE_LATENCY_CACHE (default
# ~/.cache/nodecode-latency, the newest six). Then src/tools/check/latency.py drives
# both builds as a person drives the shell -- in a terminal, in scratch homes,
# against a provider played on loopback -- in interleaved rounds, and fails a
# metric only when this checkout is slower past its tolerance with 95%
# confidence (latency.py's docstring says what is measured and why).
#
# Exits 0 when this checkout holds, 1 when it regressed, 2 when it cannot
# measure. Linux; needs SBCL, Quicklisp, git and python3 (pyte is fetched
# into the cache when missing).
set -euo pipefail
repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
[ $# -ge 1 ] || { echo "usage: latency.sh BASE [latency.py options]" >&2; exit 2; }
base="$1"; shift
cache="${NODECODE_LATENCY_CACHE:-${XDG_CACHE_HOME:-$HOME/.cache}/nodecode-latency}"
sha="$(git -C "$repo" rev-parse --verify --quiet "$base^{commit}")" \
  || { echo "latency: $base names no commit here" >&2; exit 2; }
mkdir -p "$cache"

# A build as the release makes it, without warming an image into this
# machine's own cache: latency.py's scratch homes warm their own.
build() { # TREE LABEL
  local log="$1/.latency-build.log" compile=src/tools/build/compile-cells.sh
  # A BASE from before tools/ took one folder per job keeps it at the top.
  [ -e "$1/$compile" ] || compile=src/tools/compile-cells.sh
  if ! ( cd "$1" && NODECODE_BUILD_COMMIT="$2" just release && rm -rf src/fasl \
           && "$compile" ./nodecode src/cells src/fasl ) > "$log" 2>&1; then
    tail -40 "$log" >&2
    echo "latency: the build of $2 in $1 failed (log $log)" >&2
    return 1
  fi
}

# BASE, once per commit: its tree is kept beside its binary, which reads its
# shipped cells from the tree it was built in.
tree="$cache/$sha"
exec 9> "$cache/$sha.lock"
flock 9
if [ ! -x "$tree/nodecode" ] || [ ! -e "$tree/.latency-ok" ]; then
  rm -rf "$tree" "$tree.tmp"
  mkdir -p "$tree.tmp"
  git -C "$repo" archive "$sha" | tar -x -C "$tree.tmp"
  # The cells this checkout already pulled, where BASE pins the same ones;
  # pull-cells fetches whatever BASE pins differently.
  if [ -d "$repo/src/cells" ]; then cp -an "$repo/src/cells/." "$tree.tmp/src/cells/"; fi
  echo "latency: building base ${sha:0:9}"
  build "$tree.tmp" "${sha:0:9}" || exit 2
  mv "$tree.tmp" "$tree"
  touch "$tree/.latency-ok"
fi
touch "$tree"
flock -u 9
# The newest six base builds stay.
find "$cache" -mindepth 1 -maxdepth 1 -type d -regextype posix-extended -regex '.*/[0-9a-f]{40}' \
     ! -name "$sha" -printf '%T@ %p\n' | sort -rn | tail -n +6 | cut -d' ' -f2- |
  while read -r old; do rm -rf "$old" "$old.lock"; done

echo "latency: building head (this checkout)"
build "$repo" head || exit 2

py=python3
if ! python3 -c 'import pyte' 2>/dev/null; then
  if [ ! -x "$cache/venv/bin/python" ] || ! "$cache/venv/bin/python" -c 'import pyte' 2>/dev/null; then
    rm -rf "$cache/venv"
    if ! { python3 -m venv "$cache/venv" && "$cache/venv/bin/pip" install -q 'pyte==0.8.2'; }; then
      echo "latency: pyte is missing and could not be installed" >&2
      exit 2
    fi
  fi
  py="$cache/venv/bin/python"
fi
exec "$py" "$repo/src/tools/check/latency.py" "$tree/nodecode" "$repo/nodecode" "$@"
