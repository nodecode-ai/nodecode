#!/usr/bin/env bash
# ci-nightly.sh -- the Nightly job: what main costs a person, measured once a
# night, where a pull request's minutes would not pay for it.
#
# SPDX-License-Identifier: MIT
#
#   latency    main against main a day before, each built as the release is
#              and driven in a terminal as a person drives it: boot, keys,
#              Enter to the provider, first and last token painted, the
#              wheel, the session list, Pss, CPU per frame. Interleaved
#              rounds, compared in pairs (tools/check/latency.sh).
#   footprint  the built shell held to the memory ceilings (`just footprint').
#
# Run from a checkout's root, with history (fetch-depth 0). NIGHTLY_BASE, when
# set, is the commit to hold this one to (a manual run's input). Else, on a
# pull request -- one that changes the nightly itself -- the base is HEAD^1,
# main as GitHub's merge saw it; else main as it stood 24 hours before, and
# when nothing landed since then the latency half is skipped: it measured
# this commit last night.
#
# Both halves run; exits 1 when either found a regression, else 2 when either
# could not measure, else 0. CI_OUT, when set, keeps latency.json and
# footprint.txt there.
set -euo pipefail
repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo"

# The SBCL and Quicklisp the Linux job's release build keeps on this runner;
# `just release' starts a bare SBCL that reads only ~/.sbclrc, as ci-linux.sh
# arranges.
build_home="${NODECODE_BUILD_HOME:-/var/tmp/nodecode-ci-build}"
key="$(cat tools/build/sbcl/build.sh tools/build/sbcl/patches/* | sha256sum | cut -d' ' -f1)"
sbcl_root="$build_home/env/.cache/nodecode-sbcl/$key"
ql_setup="$build_home/env/quicklisp/setup.lisp"
if [ -x "$sbcl_root/bin/sbcl" ]; then
  export PATH="$sbcl_root/bin:$PATH" SBCL_HOME="$sbcl_root/lib/sbcl"
elif ! command -v sbcl >/dev/null; then
  echo "ci-nightly: no SBCL at $sbcl_root and none on PATH; the Linux job's release build makes it" >&2
  exit 2
fi
if ! grep -qs quicklisp "$HOME/.sbclrc"; then
  if [ -n "${GITHUB_ACTIONS:-}" ] && [ -f "$ql_setup" ]; then
    printf '(load "%s")\n' "$ql_setup" > "$HOME/.sbclrc"
  else
    echo "ci-nightly: ~/.sbclrc loads no quicklisp, and the build needs it" >&2
    exit 2
  fi
fi

out="${CI_OUT:-$(mktemp -d "${TMPDIR:-/tmp}/nodecode-nightly.XXXXXX")}"
mkdir -p "$out"
head="$(git rev-parse HEAD)"
base="${NIGHTLY_BASE:-}"
if [ -z "$base" ] && [ "${GITHUB_EVENT_NAME:-}" = pull_request ]; then
  base="$(git rev-parse HEAD^1)"
elif [ -z "$base" ]; then
  base="$(git rev-list -1 --before='24 hours ago' HEAD)"
  [ "$base" != "$head" ] || base=""
fi

latency=0
if [ -n "$base" ]; then
  echo "ci-nightly: latency, ${head:0:9} against ${base:0:9}"
  # Builds the base (kept in its cache) and this checkout, into ./nodecode.
  tools/check/latency.sh "$base" --json "$out/latency.json" || latency=$?
else
  echo "ci-nightly: nothing landed on main in 24 hours; latency measured ${head:0:9} last night"
fi

# The footprint runs on this checkout's build: latency.sh left one, else it
# is made here, the same way, without warming this machine's own image cache.
if [ ! -x ./nodecode ] || [ ! -d fasl ]; then
  if ! { NODECODE_BUILD_COMMIT="${head:0:9}" just release && rm -rf fasl \
           && tools/build/compile-cells.sh ./nodecode cells fasl; } > "$out/build.log" 2>&1; then
    tail -40 "$out/build.log" >&2
    echo "ci-nightly: the build of ${head:0:9} failed" >&2
    exit 2
  fi
fi
echo "ci-nightly: footprint of ${head:0:9}"
footprint=0
just footprint 2>&1 | tee "$out/footprint.txt" || footprint=${PIPESTATUS[0]}

if [ "$latency" = 1 ] || [ "$footprint" = 1 ]; then exit 1; fi
exit $(( latency > footprint ? latency : footprint ))
