#!/usr/bin/env bash
# compile-cells-test.sh --- compile-cells.sh tells its three outcomes apart.
#
# Every shipped folder loads: exit 0. A folder fails: exit 1, the folder
# named. The trial itself dies before it scores (an SBCL fatal error, as a
# runtime crash prints it): exit 3, said as the build's failure and never as
# a cell that does not load. The binaries are fakes standing in for
# `nodecode trial', so nothing is built.
#
# SPDX-License-Identifier: MIT
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
compile="${COMPILE_CELLS:-$here/compile-cells.sh}"
root="$(mktemp -d)"
trap 'rm -rf "$root"' EXIT
fail() { echo "compile-cells-test: FAIL $1" >&2; exit 1; }

mkdir -p "$root/cells/demo"
printf '(defsystem "demo")\n' > "$root/cells/demo/demo.asd"

# fake NAME BODY: an executable standing in for the release binary.
fake() {
  printf '#!/bin/sh\ncat >/dev/null\n%s\n' "$2" > "$root/$1"
  chmod +x "$root/$1"
}

# run BINARY: compile-cells' exit status and stderr, into $status and $err.
run() {
  rm -rf "$root/fasl"
  status=0
  "$compile" "$root/$1" "$root/cells" "$root/fasl" >"$root/out" 2>"$root/err" || status=$?
  err="$(cat "$root/err")"
}

fake loads "mkdir -p '$root/fasl' && : > '$root/fasl/demo.fasl'
echo 'nodecode-trial-score 1'"
run loads
[ "$status" -eq 0 ] || fail "every folder loaded, yet exit $status: $err"

fake cell-fails "printf 'demo is FAILED\\n  load: boom\\n' > \"\$NODECODE_PACK_DIAGNOSTICS\"
echo 'nodecode-trial-score 0'"
run cell-fails
[ "$status" -eq 1 ] || fail "a folder failed, yet exit $status: $err"
case "$err" in *"a shipped cell does not load"*) ;; *) fail "a failed folder is not reported as one: $err" ;; esac
case "$err" in *"demo is FAILED"*) ;; *) fail "the failed folder is not named: $err" ;; esac

fake crashes "printf 'fatal error encountered in SBCL pid 1 pthread 0x1:\\nmisuse of mprotect() on dynamic space @ 0x800362f2c0\\n' >&2
exit 1"
run crashes
[ "$status" -eq 3 ] || fail "a trial that died before scoring exits $status, not 3: $err"
case "$err" in *"died before it judged any cell"*) ;; *) fail "a dead trial is not said as one: $err" ;; esac
case "$err" in *"a shipped cell does not load"*) fail "a dead trial is blamed on a cell: $err" ;; esac
case "$err" in *"misuse of mprotect()"*) ;; *) fail "the trial's own error is not passed on: $err" ;; esac

echo "compile-cells-test: ok (a load, a failed folder, a dead trial)"
