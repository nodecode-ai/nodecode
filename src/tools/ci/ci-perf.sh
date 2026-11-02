#!/usr/bin/env bash
# ci-perf.sh --- the Perf CI job: hold this revision to its base where a
# person waits (perf-gate.py, `just perf').
#
# SPDX-License-Identifier: MIT
#
# Run from a checkout's root. PERF_BASE is the revision to hold it to: the
# workflow passes the commit a push moved main from; a pull request's checkout
# is GitHub's merge commit, whose first parent is the base branch's tip, so
# with no usable PERF_BASE the base is HEAD^1 -- the change exactly as it
# would land. Runs after the Linux job, on the SBCL and quicklisp that job's
# release build keeps under NODECODE_BUILD_HOME, the ones the suite runs on.
#
# CI_OUT, when set, keeps perf.json there (every run's numbers, both sides).
set -euo pipefail
repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$repo"

base="${PERF_BASE:-}"
if [ -z "$base" ] || [ -z "${base//0/}" ] || ! git cat-file -e "$base^{commit}" 2>/dev/null; then
  base="$(git rev-parse HEAD^1)"
fi

build_home="${NODECODE_BUILD_HOME:-/var/tmp/nodecode-ci-build}"
key="$(cat src/tools/build/sbcl/build.sh src/tools/build/sbcl/patches/* | sha256sum | cut -d' ' -f1)"
sbcl_root="$build_home/env/.cache/nodecode-sbcl/$key"
ql_setup="$build_home/env/quicklisp/setup.lisp"
scratch="$(mktemp -d "${TMPDIR:-/tmp}/nodecode-perf.XXXXXX")"
trap 'rm -rf "$scratch"' EXIT
if [ -x "$sbcl_root/bin/sbcl" ] && [ -f "$ql_setup" ]; then
  export PATH="$sbcl_root/bin:$PATH" SBCL_HOME="$sbcl_root/lib/sbcl"
  printf '(load "%s")\n' "$ql_setup" > "$scratch/sbclrc"
  export PERF_SBCL_USERINIT="$scratch/sbclrc"
else
  echo "ci-perf: no CI SBCL at $sbcl_root; using sbcl on PATH" >&2
fi
export PERF_HOME="${PERF_HOME:-/var/tmp/nodecode-perf-ci}"
out="${CI_OUT:-$scratch}"
mkdir -p "$out"
echo "ci-perf: HEAD $(git rev-parse --short HEAD) against base $(git rev-parse --short "$base")"
python3 src/tools/check/perf-gate.py "$base" --json "$out/perf.json"
