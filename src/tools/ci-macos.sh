#!/bin/bash
# ci-macos.sh --- the Mac CI job: the release's own Mac arm on the checked-out
# commit.
#
# SPDX-License-Identifier: MIT
#
# Run from a checkout's root on the runner that drives the Mac. It is
# src/tools/pack-macos.sh, exactly as a release runs it: the commit shipped
# over ssh as a one-commit repository, src/tools/mac-build.sh run on the Mac
# (the patched SBCL, the stack scrub, quicklisp on the pinned dist, `just
# tarball', the trial seams, a headless serve, the release feed proof), and the
# tarball brought back and proven byte-equal to the tree. The Mac is the
# publish setting NODECODE_MACOS_HOST (an ssh host the runner's account
# reaches); a green job is pack-macos.sh's own verdict and exit 0.
#
# The Mac builds under CI's own neutral root (NODECODE_MACOS_BUILD_DIR,
# /Users/Shared/nodecode-ci by default): its toolchain, quicklisp and compile
# caches stay there between jobs, apart from the release's root
# (/Users/Shared/nodecode-build), which this job never reads, writes or locks.
set -euo pipefail
repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo"
export NODECODE_MACOS_BUILD_DIR="${NODECODE_MACOS_BUILD_DIR:-/Users/Shared/nodecode-ci}"
case "$NODECODE_MACOS_BUILD_DIR" in
  /Users/Shared/nodecode-build|/Users/Shared/nodecode-build/*)
    echo "ci-macos: $NODECODE_MACOS_BUILD_DIR is the release's root; CI builds under its own" >&2; exit 1 ;;
esac
out="$(mktemp -d "${TMPDIR:-/tmp}/nodecode-ci-macos.XXXXXX")"
trap 'rm -rf "$out"' EXIT
src/tools/pack-macos.sh HEAD "$out"
