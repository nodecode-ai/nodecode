#!/usr/bin/env bash
# upgrade-proof-refused.sh --- a running gateway of this build, offered a
# release it refuses live, still ends up serving that release.
#
# SPDX-License-Identifier: MIT
#
# Usage: upgrade-proof-refused.sh [TARBALL]
#
# upgrade-proof.py upgrades the release users run now onto this build; whether
# that goes live or is refused is up to the two builds. This proves the other
# half, the one this build owns: once users run it, a release it cannot take
# live must not strand its gateway on it. The next release is made here: this
# commit plus one form outside any definition -- boot state, which a running
# image cannot replay, so CLASSIFY-UPGRADE holds it for a restart --
# committed to the object store only (no branch, no checkout touched) and
# packed. Then this build's gateway, serving by hand with no unit, is offered
# it, and passes only by restarting onto it (--expect-restart): a release it
# takes live proves nothing here. The shape was a new side table, as 0.1.5 had
# (tui/ui.lisp, *PARSED-PATCH*), until #122 took that live and this proof
# went on passing without a restart.
#
# TARBALL is this build already packed (the release job's); without it HEAD
# is packed. Packing honours NODECODE_BUILD_HOME as `just tarball' does.
set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo"
# pack.sh writes OUT under the checkout: a scratch name there, gone at exit.
rel="upgrade-proof-out.$$"
index="$(mktemp)"
trap 'rm -rf "$index" "$index.pack" "$repo/$rel"' EXIT
pack() {
  src/tools/pack.sh "$1" "$rel" > "$index.pack" 2>&1 || { cat "$index.pack" >&2; exit 2; }
  ls "$repo/$rel/nodecode-$(git rev-parse --short=9 "$1")"-*.tar.gz
}

head="$(git rev-parse HEAD)"
if [ "$#" -ge 1 ]; then
  this="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
else
  this="$(pack "$head")"
fi

# The refused release: one more commit, made with a scratch index.
file=src/src/tui/entry.lisp
blob="$( { git show "$head:$file"; cat <<'LISP'

;;; upgrade-proof-refused.sh's release: a form outside any definition is boot
;;; state, which a running image cannot replay, so it refuses this build live.
(setf (get 'upgrade-proof-refusal 'release) t)
LISP
} | git hash-object -w --stdin)"
GIT_INDEX_FILE="$index" git read-tree "$head"
GIT_INDEX_FILE="$index" git update-index --cacheinfo "100644,$blob,$file"
tree="$(GIT_INDEX_FILE="$index" git write-tree)"
refused="$(git -c user.name=upgrade-proof -c user.email=upgrade-proof@invalid \
  commit-tree "$tree" -p "$head" -m 'upgrade-proof: a release the running image refuses live')"

next="$(pack "$refused")"
rm -f "$index.pack"

python3 src/tools/upgrade-proof.py "$next" --from "$this" --expect-restart
