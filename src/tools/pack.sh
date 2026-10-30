#!/usr/bin/env bash
# pack.sh --- the POSIX arm of the release: a tarball a stranger can run.
#
# SPDX-License-Identifier: MIT
#
# Run through `just tarball'. Builds a committed revision in a throwaway tree
# and packs the four things the organism needs beside its executable, so the
# result carries no local drift: a shared checkout always has some, and a
# build handed to an outsider is the one place it must not travel.
#
# One script for Linux and macOS. SBCL cannot cross-compile, so the box this
# runs on names the build: nodecode-<rev>-x86_64-linux from the Linux release
# box, nodecode-<rev>-arm64-darwin from a Mac. Windows is a third machine
# again and has its own arm -- pack-windows.sh drives the local Windows
# sandbox, and a publish carries those two. The installer asks uname the same
# two questions and fetches the name they spell.
#
# The six, and a seventh on macOS:
#   nodecode          the dumped image
#   nodecode-launcher (macOS) what launchd runs the gateway through
#   install.sh        the installer, so a running organism can stage the
#                     next release beside itself (`nodecode update')
#   src/src/          the source it reads its own definitions from (SOURCE-ROOT
#                     resolves <runtime-directory>/src/src/ first, so the
#                     directory beside the binary IS the organism's self-view)
#   src/cells/        the shipped cell folders, found as <source>/../cells/
#   src/fasl/         those folders compiled by this very image, found as
#                     <source>/../fasl/, so a launch loads and never compiles
#   src/nodecode.asd  the system definition those folders declare a depend-on
#
# resources/ does NOT travel: the bundled themes document is read at load time
# and dumped into the image, and nothing else under it is read at runtime.
set -euo pipefail

REV="${1:-HEAD}"
OUT="${2:-dist}"

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo"

os="$(uname -s | tr '[:upper:]' '[:lower:]')"
arch="$(uname -m)"
case "$os" in
  linux|darwin) ;;
  *) echo "pack: no release arm for $os (Linux and macOS build with this script; Windows is tools/pack-windows.sh)" >&2; exit 1 ;;
esac
case "$arch" in
  x86_64|amd64) arch=x86_64 ;;
  arm64|aarch64) arch=arm64 ;;
  *) echo "pack: no release arm for $arch" >&2; exit 1 ;;
esac

# A Mac release runs on macOS 15 and later whichever Mac packs it: clang
# otherwise targets the builder's own macOS, and the launcher it compiles
# would refuse to start on an older one.
floor=15
if [ "$os" = darwin ]; then
  export MACOSX_DEPLOYMENT_TARGET="${MACOSX_DEPLOYMENT_TARGET:-$floor.0}"
fi

# NINE hex digits, explicitly. A bare `--short' takes its length from the
# repository it runs in -- core.abbrev=auto sizes it by object count -- so the
# same commit would be abbreviated to different lengths in a fresh
# single-branch clone and a full one. Two lengths for one commit would be two
# labels, and a label is what a channel pointer names.
label="$(git rev-parse --short=9 "$REV")"
name="nodecode-${label}-${arch}-${os}"
work="$(mktemp -d)"
trap 'rm -rf "$work" "${XDG_CACHE_HOME:-$HOME/.cache}"/common-lisp/*"$work"' EXIT

# The build tree is the revision, not the checkout: `git archive' takes what is
# committed and nothing else -- no unstaged edits, and no untracked files,
# which is also what keeps the operator's own cell symlinks out. Two of them
# point at the directory they sit in, and a recursive copy of one never ends.
git archive "$REV" | tar -x -C "$work"
# The shipped cells are in no revision of this repository: they come from
# nodecode-ai/cells at the pin in src/cells/bundle.json (pull-cells.sh).
"$repo/src/tools/pull-cells.sh" "$work/src/cells"
# ...which also means the build tree has no .git, so the image cannot ask git
# what it was built from: every released build reported a bare `0.0.1' while a
# build from the checkout reported 0.0.1+<commit>. The revision is known right
# here, so stamp it — *USER-AGENT-VERSION* prefers the stamp over git.
# A Linux release runs on glibc 2.34 and later (Ubuntu 22.04, Debian 12, RHEL
# 9) whichever box packs it. Every symbol is bound to the version the build
# box's glibc defines, so an import above the floor would make the image
# refuse to start on those boxes at all; the build stops instead.
glibc_floor=2.34
glibc_within_floor() { # FILE WHAT
  local over
  command -v objdump >/dev/null 2>&1 || { echo "pack: objdump (binutils) reads the glibc floor; install it" >&2; return 1; }
  over="$(objdump -T "$1" | awk -v floor="$glibc_floor" '
    match($0, /GLIBC_[0-9]+\.[0-9]+/) {
      split(substr($0, RSTART + 6, RLENGTH - 6), v, "."); split(floor, f, ".")
      if (v[1] + 0 > f[1] + 0 || (v[1] + 0 == f[1] + 0 && v[2] + 0 > f[2] + 0)) print substr($0, RSTART, RLENGTH), $NF
    }')"
  [ -z "$over" ] && return 0
  echo "pack: refusing, $2 imports glibc symbols above the release floor $glibc_floor:" >&2
  printf '%s\n' "$over" >&2
  return 1
}

# A stranger reads this build, so no account's home may be written in it.
# SBCL records every path it compiles or loads -- quicklisp's libraries,
# ASDF's cache, the SBCL runtime's own contribs, the cells' sources -- in the
# image and the fasls, so the build runs under a home that names nobody. On
# Linux that is NODECODE_BUILD_HOME (/var/tmp/nodecode-build by default), which
# keeps quicklisp on the pinned dist, ASDF's cache and the SBCL build between
# runs; the Mac arm points HOME at its own neutral root (pack-macos.sh). The
# gate before the tarball refuses a tree that still carries the account's home.
account_home="$(eval echo "~$(id -un)")"
if [ "$os" = linux ]; then
  build_home="${NODECODE_BUILD_HOME:-/var/tmp/nodecode-build}"
  case "$build_home" in
    "$account_home"|"$account_home"/*) echo "pack: NODECODE_BUILD_HOME must not be under $account_home" >&2; exit 1 ;;
  esac
  mkdir -p "$build_home/env"
  export HOME="$build_home/env" XDG_CACHE_HOME="$build_home/env/.cache"
  unset SBCL_HOME
fi
# The Linux image is dumped on build.sh's SBCL, never on the `sbcl' the box
# has on PATH: that runtime was compiled against the box's own glibc headers,
# and from 2.38 on they turn an atoi into __isoc23_strtol, which no older
# glibc has (patch 0006 removes the one such call). Built once per build.sh
# and patches -- this revision's copies -- with the box's sbcl as the host,
# tested where the patches touch the runtime, and kept under the cache.
if [ "$os" = linux ]; then
  key="$(cat "$work/src/tools/sbcl/build.sh" "$work"/src/tools/sbcl/patches/* | sha256sum | cut -d' ' -f1)"
  sbcl="${XDG_CACHE_HOME:-$HOME/.cache}/nodecode-sbcl/$key"
  if [ ! -x "$sbcl/bin/sbcl" ]; then
    rm -rf "$sbcl.new"
    sh "$work/src/tools/sbcl/build.sh" "$sbcl.new" "$work/.sbcl-build"
    glibc_within_floor "$sbcl.new/bin/sbcl" 'the SBCL runtime' || exit 1
    ( cd "$work"/.sbcl-build/sbcl-*/tests && sh run-tests.sh exhaust.impure.lisp gc.impure.lisp gc-threads.impure.lisp \
        fcb-threads.impure.lisp dynamic-extent.pure.lisp make-thread.pure.lisp interrupt-consing.impure.lisp \
        run-program.impure.lisp )
    rm -rf "$sbcl"
    mv "$sbcl.new" "$sbcl"
  fi
  export PATH="$sbcl/bin:$PATH" SBCL_HOME="$sbcl/lib/sbcl"
  sbcl --version
  # quicklisp in the neutral home, on the dist every arm builds against, and
  # the system's dependencies present before the build asks for them.
  dist=2026-01-01
  if [ ! -f "$HOME/quicklisp/setup.lisp" ]; then
    curl -fsSLo "$work/quicklisp.lisp" https://beta.quicklisp.org/quicklisp.lisp
    sbcl --noinform --non-interactive --no-userinit --load "$work/quicklisp.lisp" --eval '(quicklisp-quickstart:install)'
  fi
  printf '%s\n' '(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))' \
    '  (when (probe-file quicklisp-init) (load quicklisp-init)))' > "$HOME/.sbclrc"
  sbcl --noinform --non-interactive \
    --eval "(unless (equal (ql-dist:version (ql-dist:find-dist \"quicklisp\")) \"$dist\") (ql-dist:install-dist \"http://beta.quicklisp.org/dist/quicklisp/$dist/distinfo.txt\" :replace t :prompt nil))"
  ( cd "$work/src" && sbcl --dynamic-space-size 4096 --control-stack-size 8MB --noinform --non-interactive \
      --eval '(push (truename ".") asdf:*central-registry*)' --eval '(ql:quickload "nodecode" :silent t)' >/dev/null )
fi

( cd "$work" && NODECODE_BUILD_COMMIT="$label" just release )

# Staged beside the neutral home, never inside the checkout: the cells compile
# here and their fasls keep this path. The tree moves to OUT once it passes.
stage="$(dirname "$HOME")/stage/$name"
rm -rf "$stage"
mkdir -p "$stage/src"
cp "$work/nodecode"          "$stage/nodecode"
# macOS's gateway runs through a launcher that never moves (background.lisp):
# a Mac build without one cannot turn its gateway on.
if [ "$os" = darwin ]; then
  [ -x "$work/nodecode-launcher" ] || { echo "pack: refusing, the build left no nodecode-launcher" >&2; exit 1; }
  cp "$work/nodecode-launcher" "$stage/nodecode-launcher"
  for bin in nodecode nodecode-launcher; do
    minos="$(vtool -show-build "$stage/$bin" 2>/dev/null | awk '$1 == "minos" { print $2; exit }')"
    [ -n "$minos" ] && [ "${minos%%.*}" -le "$floor" ] || {
      echo "pack: refusing, $bin needs macOS ${minos:-unknown}; the release floor is $floor (src/tools/sbcl/build.sh builds its runtime)" >&2
      exit 1
    }
  done
fi
if [ "$os" = linux ]; then
  glibc_within_floor "$stage/nodecode" nodecode || exit 1
fi
cp -r "$work/src/src"        "$stage/src/src"
cp -r "$work/src/cells"     "$stage/src/cells"
cp "$work/src/nodecode.asd"  "$stage/src/nodecode.asd"

# A cell's gates are the maintainer's, not the tester's, and the cell
# loads without them: its .asd declares the test slice as a separate system,
# which nothing on this path asks ASDF to find.
find "$stage/src/cells" -type d -name test -prune -exec rm -rf {} +

# The shipped folders compile once, here, into src/fasl/ beside them, and a
# launch loads what is compiled instead of paying the compiler at its first
# paint.
# One step, shared with a checkout's `just release' (compile-cells.sh): the
# fasl directory made, every shipped folder linked into a scratch home, the
# image's `trial' verb run there -- its load is the compile, and its score is
# the gate: a folder that fails to load does not ship.
if ! "$repo/src/tools/compile-cells.sh" "$stage/nodecode" "$stage/src/cells" "$stage/src/fasl"; then
  echo "pack: refusing, a shipped cell does not load against this build" >&2
  exit 1
fi

cp "$repo/src/tools/README.alpha.txt" "$stage/README.txt"
# The installer travels too: a running organism stages the next release
# through it (`nodecode update' runs it with --stage, then --link).
cp "$work/src/tools/install.sh"  "$stage/install.sh"

# A symlink that leaves the tree is a broken file on the other box, and one
# that points at its own parent loops tar. Neither may be in a release.
if find "$stage" -type l | grep -q .; then
  echo "pack: refusing, symlinks in the staged tree:" >&2
  find "$stage" -type l >&2
  exit 1
fi

# The gate: the build account's home is written nowhere in the tree a stranger
# unpacks (SBCL keeps every build path in the image and the fasls).
leak="$(grep -rlaF "$account_home/" "$stage" || true)"
if [ -n "$leak" ]; then
  echo "pack: refusing, the build account's home ($account_home) is written in $(printf '%s\n' "$leak" | wc -l | tr -d ' ') files, e.g.:" >&2
  printf '%s\n' "$leak" | head -5 >&2
  exit 1
fi
# OUT is under the checkout unless it names a folder from the root: CI keeps
# its tarball in a scratch folder of its own.
case "$OUT" in
  /*) out_dir="$OUT" ;;
  *) out_dir="$repo/$OUT" ;;
esac
mkdir -p "$out_dir"
rm -rf "$out_dir/$name"
mv "$stage" "$out_dir/$name"

cd "$out_dir"
rm -f "$name.tar.gz" "$name.tar.gz.sha256"
# gzip, not zstd: measured 26 MB against 25 MB on this tree, and a bare
# debian:trixie-slim has tar but no zstd -- a megabyte is not worth handing
# someone an archive their box cannot open. macOS's tar would also write an
# AppleDouble sidecar (._name) for every file carrying extended attributes,
# junk beside every source file once a Linux tar unpacks it: told not to.
COPYFILE_DISABLE=1 tar -czf "$name.tar.gz" "$name"
# coreutils on Linux, perl's shasum on macOS; both print `<hash>  <name>'.
if command -v sha256sum >/dev/null 2>&1; then
  sha256sum "$name.tar.gz" > "$name.tar.gz.sha256"
else
  shasum -a 256 "$name.tar.gz" > "$name.tar.gz.sha256"
fi

printf 'just tarball complete: %s/%s.tar.gz (%s)\n' \
  "$OUT" "$name" "$(du -h "$name.tar.gz" | cut -f1)"
cat "$name.tar.gz.sha256"
