#!/usr/bin/env bash
# ci-proofs.sh --- the Proofs CI job: every proof under proofs/ checks.
#
# SPDX-License-Identifier: MIT
#
# Run from a checkout's root (the CI workflow runs it on the Linux runner; any
# Linux box with elan and bash does; a GitHub-hosted runner, which has no
# just, checks a fork's pull request). It runs src/tools/proofs.sh, the
# script `just proofs' runs, on the toolchain proofs/lean-toolchain pins.
#
# Lean comes from elan. A runner account without one gets the elan release
# pinned below in its own home, the download checked against its SHA-256,
# under GitHub Actions only; anywhere else a missing elan is an error naming the fix. The
# toolchain is fetched once and kept in ~/.elan between runs. The two Linux
# runners share one account, so the install and the fetch hold a lock.
set -euo pipefail
repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo"

elan_version=4.2.4
elan_sha256=42b94d4244e8353142c456ec0e4ca6528fd898a6c604d4059f494e706e431f63
toolchain="$(tr -d '[:space:]' < proofs/lean-toolchain)"
export PATH="$HOME/.elan/bin:$PATH"

exec 9> "$HOME/.nodecode-ci-elan.lock"
flock 9
if ! command -v elan >/dev/null 2>&1; then
  if [ -z "${GITHUB_ACTIONS:-}" ]; then
    echo "ci-proofs: no elan on PATH; install it (https://github.com/leanprover/elan)" >&2
    exit 1
  fi
  case "$(uname -s)-$(uname -m)" in
    Linux-x86_64) ;;
    *) echo "ci-proofs: no elan, and no pinned elan for $(uname -s)-$(uname -m)" >&2; exit 1 ;;
  esac
  tmp="$(mktemp -d)"
  trap 'rm -rf "$tmp"' EXIT
  curl -sSfL -o "$tmp/elan.tar.gz" \
    "https://github.com/leanprover/elan/releases/download/v$elan_version/elan-x86_64-unknown-linux-gnu.tar.gz"
  echo "$elan_sha256  $tmp/elan.tar.gz" | sha256sum -c --quiet -
  tar -xzf "$tmp/elan.tar.gz" -C "$tmp"
  "$tmp/elan-init" -y --no-modify-path --default-toolchain none
fi
elan toolchain install "$toolchain"
flock -u 9

exec src/tools/proofs.sh
