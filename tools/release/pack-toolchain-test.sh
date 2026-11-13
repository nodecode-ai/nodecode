#!/usr/bin/env bash
# pack-toolchain-test.sh --- two packs on one fresh build home build its SBCL once.
#
# SPDX-License-Identifier: MIT
#
# The build home's SBCL cache, quicklisp and pinned dist are shared by every
# pack on it: CI's Linux runners and a Release rehearsal share one box and one
# NODECODE_BUILD_HOME. Two packs that both found the SBCL missing built into
# one `.new' folder, and the second's wipe deleted the first one's half-built
# SBCL -- or, later, the finished SBCL the first was already building on. This
# runs the real pack.sh twice on a tiny committed tree with the build stubbed
# out (no SBCL is compiled, no quicklisp fetched): the first pack holds inside
# its SBCL build until the second has either started a build of its own or
# said it waits for the first, and inside its release build until the
# second's release build has begun beside it. Two different commits, so the
# release stage is not what they share. Linux only -- the build home is
# pack.sh's Linux arm.
set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
if [ "$(uname -s)" != Linux ]; then
  echo 'pack-toolchain-test: skipped, the build home is Linux only'
  exit 0
fi

root="$(mktemp -d)"
first=''
trap '[ -n "$first" ] && kill "$first" 2>/dev/null; rm -rf "$root"' EXIT
fail() { echo "pack-toolchain-test: FAIL $1" >&2; exit 1; }

# The tree pack.sh archives and runs from: its own scripts, stubs beside them.
t="$root/tree"
mkdir -p "$t/tools/release" "$t/tools/build/sbcl/patches" "$t/tools/install" "$t/src" "$t/cells"
cp "$repo/tools/release/pack.sh" "$repo/tools/release/layout.sh" "$repo/tools/release/README.alpha.txt" "$t/tools/release/"
cp "$repo/tools/install/install.sh" "$t/tools/install/"
echo 'stub' > "$t/tools/build/sbcl/patches/0001-stub.patch"
# build.sh PREFIX WORKDIR: counts itself, lays a stub SBCL out in PREFIX and a
# stub test suite in WORKDIR. The first pack's build then holds until the
# second pack has begun a build or said it waits; its PREFIX must still be its
# own when it lets go.
cat > "$t/tools/build/sbcl/build.sh" <<'STUB'
#!/bin/sh
set -eu
echo "$PACK_ROLE" >> "$PACK_SYNC/builds"
mkdir -p "$1/bin" "$2/sbcl-stub/tests"
printf '#!/bin/sh\n# %s\nexec "$PACK_STUB_SBCL" "$@"\n' "$PACK_ROLE" > "$1/bin/sbcl"
chmod +x "$1/bin/sbcl"
printf '#!/bin/sh\nexit 0\n' > "$2/sbcl-stub/tests/run-tests.sh"
[ "$PACK_ROLE" = first ] || exit 0
: > "$PACK_SYNC/first-building"
i=0
until [ "$(grep -c . "$PACK_SYNC/builds")" -gt 1 ] || grep -q 'waiting for it' "$PACK_SYNC/second.log" 2>/dev/null; do
  [ "$i" -lt 600 ] || { echo 'the second pack neither built nor waited' >&2; exit 1; }
  sleep 0.1; i=$((i + 1))
done
sleep 0.5
grep -q '# first' "$1/bin/sbcl" 2>/dev/null || { echo 'the SBCL being built was deleted under the first pack' >&2; exit 1; }
STUB
printf '#!/bin/sh\nexit 0\n' > "$t/tools/build/pull-cells.sh"
printf '#!/bin/sh\nmkdir -p "$3"\n' > "$t/tools/build/compile-cells.sh"
chmod +x "$t/tools/build/pull-cells.sh" "$t/tools/build/compile-cells.sh"
echo '(defsystem "nodecode")' > "$t/nodecode.asd"
echo ';; stub' > "$t/src/stub.lisp"
echo 'stub' > "$t/cells/README"
git_commit() {
  ( cd "$t" && git add -A && git -c user.name=pack-toolchain-test -c user.email=pack-toolchain-test@invalid \
      -c commit.gpgsign=false commit -qm "$1" )
}
( cd "$t" && git init -q )
git_commit one
echo ';; two' > "$t/src/two.lisp"
git_commit two

# The box: `sbcl' installs quicklisp when asked to and does nothing else;
# `curl' leaves the file it is told to write; `just release' leaves a stub
# image; the glibc floor reads nothing from it.
mkdir -p "$root/bin" "$root/sync"
cat > "$root/bin/sbcl" <<'STUB'
#!/bin/sh
case "$*" in
  *quicklisp-quickstart:install*)
    echo "$PACK_ROLE" >> "$PACK_SYNC/quicklisp-installs"
    mkdir -p "$HOME/quicklisp" && : > "$HOME/quicklisp/setup.lisp" ;;
esac
exit 0
STUB
printf '#!/bin/sh\n: > "$2"\n' > "$root/bin/curl"
# The first pack's release build holds until the second's has begun: the
# toolchain is the one thing packs take turns at, never the build.
cat > "$root/bin/just" <<'STUB'
#!/bin/sh
[ "$1" = release ] || exit 1
: > "$PACK_SYNC/$PACK_ROLE-releasing"
if [ "$PACK_ROLE" = first ]; then
  i=0
  until [ -e "$PACK_SYNC/second-releasing" ]; do
    [ "$i" -lt 300 ] || { echo 'the second pack never began its release build beside the first' >&2; exit 1; }
    sleep 0.1; i=$((i + 1))
  done
fi
echo '#!/bin/sh' > nodecode && chmod +x nodecode
STUB
printf '#!/bin/sh\nexit 0\n' > "$root/bin/objdump"
chmod +x "$root/bin/sbcl" "$root/bin/curl" "$root/bin/just" "$root/bin/objdump"
bh="$root/build-home"
export PATH="$root/bin:$PATH" NODECODE_BUILD_HOME="$bh" PACK_SYNC="$root/sync" PACK_STUB_SBCL="$root/bin/sbcl"

( cd "$t" && PACK_ROLE=first bash tools/release/pack.sh HEAD "$root/out-first" ) > "$root/first.log" 2>&1 &
first=$!
i=0
while [ ! -e "$root/sync/first-building" ] && kill -0 "$first" 2>/dev/null && [ "$i" -lt 600 ]; do sleep 0.1; i=$((i + 1)); done
[ -e "$root/sync/first-building" ] || fail "the first pack never reached its SBCL build: $(tail -5 "$root/first.log")"

second_status=0
( cd "$t" && PACK_ROLE=second bash tools/release/pack.sh HEAD~1 "$root/out-second" ) > "$root/sync/second.log" 2>&1 \
  || second_status=$?
status=0
wait "$first" || status=$?
first=''
[ "$status" -eq 0 ] \
  || fail "the first pack failed beside a second pack on the same fresh build home: $(tail -3 "$root/first.log")"
[ "$second_status" -eq 0 ] || fail "the second pack failed: $(tail -3 "$root/sync/second.log")"

builds="$(grep -c . "$root/sync/builds")"
[ "$builds" -eq 1 ] || fail "two packs on one build home built its SBCL $builds times: $(tr '\n' ' ' < "$root/sync/builds")"
installs="$(grep -c . "$root/sync/quicklisp-installs" 2>/dev/null || true)"
[ "$installs" = 1 ] || fail "two packs on one build home installed quicklisp ${installs:-0} times"
grep -q 'waiting for it' "$root/sync/second.log" || fail 'the second pack never said it waits for the first'
for role in first second; do
  ls "$root/out-$role"/nodecode-*.tar.gz >/dev/null 2>&1 || fail "the $role pack left no tarball"
done
sbcl_dir="$(echo "$bh"/env/.cache/nodecode-sbcl/*)"
grep -q '# first' "$sbcl_dir/bin/sbcl" || fail "the cached SBCL is not the one the first pack built"
[ ! -e "$sbcl_dir.new" ] || fail 'a pack left its half-built SBCL behind'

echo 'pack-toolchain-test: ok (two packs on one fresh build home: one SBCL build, one quicklisp install, the second waits, the release builds overlap)'
