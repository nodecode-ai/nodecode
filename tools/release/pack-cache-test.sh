#!/usr/bin/env bash
# pack-cache-test.sh --- a pack leaves no compile cache of its build tree.
#
# SPDX-License-Identifier: MIT
#
# ASDF caches each fasl at common-lisp/<impl>/<the source's truename> under
# XDG_CACHE_HOME, and pack.sh's exit removes the cache of its build tree by the
# tree's name. On macOS that name is a symlinked path (mktemp answers
# /var/folders/..., the truename is /private/var/folders/...), so the removal
# never matched and every Mac build left its compiled tree behind, about 13 MB
# each. This runs the real pack.sh on a tiny committed tree with the build
# stubbed out, under a TMPDIR reached through a symlink -- the Mac's shape on
# any box: the stub build caches a fasl the way ASDF does, by truename, and the
# pack must leave none. Linux only -- the build home is pack.sh's Linux arm.
set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
if [ "$(uname -s)" != Linux ]; then
  echo 'pack-cache-test: skipped, the build home is Linux only'
  exit 0
fi

root="$(mktemp -d)"
trap 'rm -rf "$root"' EXIT
fail() { echo "pack-cache-test: FAIL $1" >&2; exit 1; }

# The tree pack.sh archives and runs from: its own scripts, stubs beside them.
t="$root/tree"
mkdir -p "$t/tools/release" "$t/tools/build/sbcl/patches" "$t/tools/install" "$t/src" "$t/cells"
cp "$repo/tools/release/pack.sh" "$repo/tools/release/layout.sh" "$repo/tools/release/README.alpha.txt" "$t/tools/release/"
cp "$repo/tools/install/install.sh" "$t/tools/install/"
echo 'echo stub' > "$t/tools/build/sbcl/build.sh"
echo 'stub' > "$t/tools/build/sbcl/patches/0001-stub.patch"
printf '#!/bin/sh\nexit 0\n' > "$t/tools/build/pull-cells.sh"
printf '#!/bin/sh\nmkdir -p "$3"\n' > "$t/tools/build/compile-cells.sh"
chmod +x "$t/tools/build/pull-cells.sh" "$t/tools/build/compile-cells.sh"
echo '(defsystem "nodecode")' > "$t/nodecode.asd"
echo ';; stub' > "$t/src/stub.lisp"
echo 'stub' > "$t/cells/README"
( cd "$t" && git init -q && git add -A \
  && git -c user.name=pack-cache-test -c user.email=pack-cache-test@invalid -c commit.gpgsign=false commit -qm tree )

# The build home, its SBCL and quicklisp already in place. `just release' runs
# in the build tree: it caches a fasl of it where ASDF would, by truename, and
# leaves a stub image; the glibc floor reads nothing from it.
bh="$root/build-home"
key="$(cat "$t/tools/build/sbcl/build.sh" "$t"/tools/build/sbcl/patches/* | sha256sum | cut -d' ' -f1)"
mkdir -p "$bh/env/.cache/nodecode-sbcl/$key/bin" "$bh/env/quicklisp" "$root/bin"
printf '#!/bin/sh\nexit 0\n' > "$bh/env/.cache/nodecode-sbcl/$key/bin/sbcl"
: > "$bh/env/quicklisp/setup.lisp"
cat > "$root/bin/just" <<'STUB'
#!/bin/sh
[ "$1" = release ] || exit 1
fasl="$XDG_CACHE_HOME/common-lisp/sbcl-stub$(pwd -P)/src"
mkdir -p "$fasl" && : > "$fasl/stub.fasl"
echo '#!/bin/sh' > nodecode && chmod +x nodecode
STUB
printf '#!/bin/sh\nexit 0\n' > "$root/bin/objdump"
chmod +x "$bh/env/.cache/nodecode-sbcl/$key/bin/sbcl" "$root/bin/just" "$root/bin/objdump"

# The temp folder pack.sh takes its build tree from, reached through a link.
mkdir -p "$root/tmp-real"
ln -s tmp-real "$root/tmp-link"

( cd "$t" && PATH="$root/bin:$PATH" NODECODE_BUILD_HOME="$bh" TMPDIR="$root/tmp-link" \
    bash tools/release/pack.sh HEAD "$root/out" ) > "$root/pack.log" 2>&1 \
  || fail "the pack failed: $(tail -5 "$root/pack.log")"
[ -n "$(ls "$root/out"/*.tar.gz 2>/dev/null)" ] || fail 'the pack made no tarball'
left="$(find "$bh/env/.cache/common-lisp" -name '*.fasl' 2>/dev/null)"
[ -z "$left" ] || fail "the pack left its build tree's compile cache behind: $left"
[ -z "$(ls -A "$root/tmp-real")" ] || fail "the pack left its build tree behind: $(ls -A "$root/tmp-real")"

echo 'pack-cache-test: ok (a build tree under a symlinked temp folder leaves no compile cache)'
