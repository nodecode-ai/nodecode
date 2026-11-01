#!/usr/bin/env bash
# ci-scaling.sh --- the Scaling CI job: every hot path's growth held to its contract.
#
# SPDX-License-Identifier: MIT
#
# Run from a checkout's root (the CI workflow runs it on the Linux runner once
# the Linux job is green, so the box is not also running a suite of twelve
# shards; any Linux box with sbcl, just and git does). It runs the gate `just
# perf' runs -- src/perf/scaling.lisp -- on the SBCL and quicklisp the Linux
# job's release build keeps under NODECODE_BUILD_HOME, with the shipped cells
# pulled into the checkout, as the suite runs.
#
# A case prints each size's cost and its fitted slopes; a breach is measured
# again before it counts, and any breach that holds fails the job.
set -euo pipefail
repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$repo"

build_home="${NODECODE_BUILD_HOME:-/var/tmp/nodecode-ci-build}"
key="$(cat src/tools/build/sbcl/build.sh src/tools/build/sbcl/patches/* | sha256sum | cut -d' ' -f1)"
sbcl_root="$build_home/env/.cache/nodecode-sbcl/$key"
ql_setup="$build_home/env/quicklisp/setup.lisp"
[ -x "$sbcl_root/bin/sbcl" ] || { echo "ci-scaling: no SBCL at $sbcl_root; the Linux job builds it" >&2; exit 1; }
[ -f "$ql_setup" ] || { echo "ci-scaling: no quicklisp at $ql_setup; the Linux job sets it up" >&2; exit 1; }

scratch="$(mktemp -d "${TMPDIR:-/tmp}/nodecode-scaling.XXXXXX")"
trap 'rm -rf "$scratch"' EXIT
src/tools/build/pull-cells.sh
printf '(load "%s")\n' "$ql_setup" > "$scratch/sbclrc"
mkdir -p "$scratch/home" "$scratch/cache"
cd src
NODECODE_HOME="$scratch/home" XDG_CACHE_HOME="$scratch/cache" \
  PATH="$sbcl_root/bin:$PATH" SBCL_HOME="$sbcl_root/lib/sbcl" \
  timeout 1500 sbcl --dynamic-space-size 4096 --control-stack-size 64MB --noinform --non-interactive \
  --userinit "$scratch/sbclrc" --eval '(require :asdf)' --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(asdf:load-system :nodecode-perf)' \
  --eval '(sb-ext:exit :code (min 1 (nodecode.perf.scaling:run-scaling)))'
