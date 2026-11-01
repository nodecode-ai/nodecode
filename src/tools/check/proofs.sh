#!/usr/bin/env bash
# proofs.sh --- check every proof under proofs/.
#
# SPDX-License-Identifier: MIT
#
# `just proofs' runs this from a checkout's root. Each proofs/NAME.lean is
# checked twice, by Lean at the version proofs/lean-toolchain pins:
#
#   lean -DwarningAsError=true NAME.lean
#       the kernel checks every proof, and a warning fails the file. Plain
#       `lean' exits 0 on a `sorry', which Lean only warns about.
#   lean --run src/tools/check/proof-axioms.lean NAME.lean
#       every theorem rests only on propext, Classical.choice and Quot.sound.
#       An `axiom' the file declares, or native_decide (admitted through an
#       axiom Lean adds, the compiler trusted in place of the kernel), fails
#       it although strict Lean accepts the file.
#
# A file is checked once it is there; each is named, and any failure fails
# the run. Lean comes from elan (https://github.com/leanprover/elan), whose
# `lean +TOOLCHAIN' fetches the pinned toolchain on first use.
set -euo pipefail
repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$repo"

toolchain="$(tr -d '[:space:]' < proofs/lean-toolchain)"
if ! command -v elan >/dev/null 2>&1; then
  if [ -x "$HOME/.elan/bin/elan" ]; then
    PATH="$HOME/.elan/bin:$PATH"
  else
    echo "proofs: no elan on PATH; install it (https://github.com/leanprover/elan) and it fetches $toolchain" >&2
    exit 1
  fi
fi

shopt -s nullglob
files=(proofs/*.lean)
if [ "${#files[@]}" = 0 ]; then
  echo 'proofs: no proofs/*.lean to check' >&2
  exit 1
fi
failed=()
for f in "${files[@]}"; do
  if lean "+$toolchain" -DwarningAsError=true "$f" \
      && lean "+$toolchain" --run src/tools/check/proof-axioms.lean "$f"; then
    printf 'proof ok: %s\n' "$f"
  else
    printf 'proof FAILED: %s\n' "$f"
    failed+=("$f")
  fi
done
if [ "${#failed[@]}" != 0 ]; then
  echo "proofs: ${#failed[@]} of ${#files[@]} failed: ${failed[*]}" >&2
  exit 1
fi
echo "proofs: all ${#files[@]} checked ($toolchain)"
