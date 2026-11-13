#!/usr/bin/env bash
# pack-stage-test.sh --- two packs of one commit on one build home keep apart.
#
# SPDX-License-Identifier: MIT
#
# CI and a Release rehearsal of the same commit can pack at once, on runners
# that share a box and its NODECODE_BUILD_HOME. Both name the same release, so
# a stage keyed by that name alone was one folder for both: the second pack's
# wipe deleted the first one's tree while its cells compiled, and the first
# pack refused as if a cell did not load. This runs the real pack.sh twice on
# a tiny committed tree with the build stubbed out (no SBCL, no cells): the
# first pack holds inside its cell compile until the second has packed, then
# finds its own stage still there. Linux only -- the build home is pack.sh's
# Linux arm.
set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
if [ "$(uname -s)" != Linux ]; then
  echo 'pack-stage-test: skipped, the build home is Linux only'
  exit 0
fi

root="$(mktemp -d)"
first=''
trap '[ -n "$first" ] && kill "$first" 2>/dev/null; rm -rf "$root"' EXIT
fail() { echo "pack-stage-test: FAIL $1" >&2; exit 1; }

# The tree pack.sh archives and runs from: its own scripts, stubs beside them.
t="$root/tree"
mkdir -p "$t/tools/release" "$t/tools/build/sbcl/patches" "$t/tools/install" "$t/src" "$t/cells"
cp "$repo/tools/release/pack.sh" "$repo/tools/release/layout.sh" "$repo/tools/release/README.alpha.txt" "$t/tools/release/"
cp "$repo/tools/install/install.sh" "$t/tools/install/"
echo 'echo stub' > "$t/tools/build/sbcl/build.sh"
echo 'stub' > "$t/tools/build/sbcl/patches/0001-stub.patch"
printf '#!/bin/sh\nexit 0\n' > "$t/tools/build/pull-cells.sh"
# compile-cells.sh IMAGE CELLS FASL: the first pack marks its fasl folder, says
# it is compiling, and holds until the second pack is done; then its image and
# its mark must both still be there.
cat > "$t/tools/build/compile-cells.sh" <<'STUB'
#!/bin/sh
mkdir -p "$3" && echo "$PACK_ROLE" > "$3/compiled"
[ "$PACK_ROLE" = first ] || exit 0
: > "$PACK_SYNC/first-compiling"
i=0
while [ ! -e "$PACK_SYNC/second-done" ] && [ "$i" -lt 600 ]; do sleep 0.1; i=$((i + 1)); done
[ -x "$1" ] && [ "$(cat "$3/compiled" 2>/dev/null)" = first ] || { echo 'the stage was deleted under the first pack' >&2; exit 1; }
STUB
chmod +x "$t/tools/build/pull-cells.sh" "$t/tools/build/compile-cells.sh"
echo '(defsystem "nodecode")' > "$t/nodecode.asd"
echo ';; stub' > "$t/src/stub.lisp"
echo 'stub' > "$t/cells/README"
( cd "$t" && git init -q && git add -A \
  && git -c user.name=pack-stage-test -c user.email=pack-stage-test@invalid -c commit.gpgsign=false commit -qm tree )
name="nodecode-$(git -C "$t" rev-parse --short=9 HEAD)-x86_64-linux"

# The build home, its SBCL and quicklisp already in place; `just release'
# leaves a stub image, and the glibc floor reads nothing from it.
bh="$root/build-home"
key="$(cat "$t/tools/build/sbcl/build.sh" "$t"/tools/build/sbcl/patches/* | sha256sum | cut -d' ' -f1)"
mkdir -p "$bh/env/.cache/nodecode-sbcl/$key/bin" "$bh/env/quicklisp" "$root/bin" "$root/sync"
printf '#!/bin/sh\nexit 0\n' > "$bh/env/.cache/nodecode-sbcl/$key/bin/sbcl"
: > "$bh/env/quicklisp/setup.lisp"
cat > "$root/bin/just" <<'STUB'
#!/bin/sh
[ "$1" = release ] || exit 1
echo '#!/bin/sh' > nodecode && chmod +x nodecode
STUB
printf '#!/bin/sh\nexit 0\n' > "$root/bin/objdump"
chmod +x "$bh/env/.cache/nodecode-sbcl/$key/bin/sbcl" "$root/bin/just" "$root/bin/objdump"
export PATH="$root/bin:$PATH" NODECODE_BUILD_HOME="$bh" PACK_SYNC="$root/sync"

( cd "$t" && PACK_ROLE=first bash tools/release/pack.sh HEAD "$root/out-first" ) > "$root/first.log" 2>&1 &
first=$!
i=0
while [ ! -e "$root/sync/first-compiling" ] && kill -0 "$first" 2>/dev/null && [ "$i" -lt 600 ]; do sleep 0.1; i=$((i + 1)); done
[ -e "$root/sync/first-compiling" ] || fail "the first pack never reached its cell compile: $(tail -5 "$root/first.log")"

( cd "$t" && PACK_ROLE=second bash tools/release/pack.sh HEAD "$root/out-second" ) > "$root/second.log" 2>&1 \
  || fail "the second pack failed: $(tail -5 "$root/second.log")"
: > "$root/sync/second-done"
status=0
wait "$first" || status=$?
first=''
[ "$status" -eq 0 ] \
  || fail "the first pack failed beside a second pack of the same commit: $(tail -3 "$root/first.log")"

for role in first second; do
  got="$(tar -xOzf "$root/out-$role/$name.tar.gz" "$name/src/fasl/compiled" 2>/dev/null || true)"
  [ "$got" = "$role" ] || fail "the $role pack's tarball carries the fasls of ${got:-no pack}"
done
[ -z "$(ls -A "$bh/stage" 2>/dev/null)" ] || fail "a pack left its stage behind: $(ls -A "$bh/stage")"

echo 'pack-stage-test: ok (two packs of one commit, each its own stage, none left behind)'
