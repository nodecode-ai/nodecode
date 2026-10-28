#!/usr/bin/env bash
# ci-linux.sh --- the Linux CI job: lint, the release build, the core suite.
#
# SPDX-License-Identifier: MIT
#
# Run from a checkout's root (the CI workflow runs it on the Linux runner; any
# Linux box with sbcl, just, git, curl, gcc and make does). Three steps, each
# timed; a red one fails the run. Once this revision's SBCL is built, the
# release build runs beside the suite rather than before it:
#
#   lint     `just lint': every .lisp and .asd under src/.
#   release  `just tarball HEAD OUT': pack.sh builds the committed revision the
#            way a release does -- the patched SBCL, quicklisp on the pinned
#            dist, the shipped cells, the gate that refuses an account's home
#            in the tree -- under NODECODE_BUILD_HOME, a root of CI's own that
#            keeps the SBCL build and quicklisp between runs. Never the release
#            root (/var/tmp/nodecode-build): a publish on the same box must not
#            share its caches with a CI run.
#   suite    the core suite on that same SBCL and quicklisp, the shipped cells
#            pulled into the checkout: compiled once, then run as CI_SHARDS
#            (default 12) processes at once, shard I taking every test whose
#            place in the suite is I modulo the count (nodecode.test::run-shard),
#            each with its own NODECODE_HOME and a hard-linked copy of the
#            compiled cache. Every FAIL line is named, with its reasons, and
#            any one fails the run.
#
# CI_OUT, when set, keeps the tarball and suite.log there (the workflow uploads
# them); otherwise they go with the scratch directory.
set -euo pipefail
repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo"

build_home="${NODECODE_BUILD_HOME:-/var/tmp/nodecode-ci-build}"
case "$build_home" in
  /var/tmp/nodecode-build|/var/tmp/nodecode-build/*)
    echo "ci-linux: $build_home is the release build root; CI keeps its own" >&2; exit 1 ;;
esac
scratch="$(mktemp -d "${TMPDIR:-/tmp}/nodecode-ci.XXXXXX")"
trap 'rm -rf "$scratch"' EXIT
out="${CI_OUT:-$scratch/out}"
mkdir -p "$out"
out="$(cd "$out" && pwd)"

gh_actions="${GITHUB_ACTIONS:-}"
started=$SECONDS
step_start=$SECONDS
in_group=''
end_group() { if [ -n "$in_group" ]; then echo '::endgroup::'; in_group=''; fi; }
step() {
  end_group
  step_start=$SECONDS
  if [ -n "$gh_actions" ]; then echo "::group::$1"; in_group=1; else printf '\n== %s\n' "$1"; fi
}
times=()
done_step() { times+=("$1 $((SECONDS - step_start))s"); }

step lint
# The commit's own files: cells a previous run pulled into src/cells are no
# revision of this repository.
# shellcheck disable=SC2046
just lint $(git ls-files -- 'src/*.lisp' 'src/*.asd' | LC_ALL=C sort)
done_step lint

# The release build and the suite run on one SBCL and quicklisp, whose cache
# key is this revision's build.sh and patches. When that SBCL is already built
# (every run but the first after a patch changes it) the release build runs
# beside the suite; when it is not, the release build runs first and builds it.
key="$(cat src/tools/sbcl/build.sh src/tools/sbcl/patches/* | sha256sum | cut -d' ' -f1)"
sbcl_root="$build_home/env/.cache/nodecode-sbcl/$key"
ql_setup="$build_home/env/quicklisp/setup.lisp"
release_log="$out/release.log"
release_pid=''
if [ -x "$sbcl_root/bin/sbcl" ] && [ -f "$ql_setup" ]; then
  release_start=$SECONDS
  ( NODECODE_BUILD_HOME="$build_home" just tarball HEAD "$out"; r=$?
    echo "$((SECONDS - release_start))" > "$scratch/release.secs"; exit "$r" ) > "$release_log" 2>&1 &
  release_pid=$!
else
  step 'release build (pack.sh)'
  NODECODE_BUILD_HOME="$build_home" just tarball HEAD "$out"
  done_step release
  [ -x "$sbcl_root/bin/sbcl" ] || { echo "ci-linux: no SBCL at $sbcl_root after the release build" >&2; exit 1; }
  [ -f "$ql_setup" ] || { echo "ci-linux: no quicklisp at $ql_setup after the release build" >&2; exit 1; }
fi

step 'core suite'
src/tools/pull-cells.sh
# The suite runs under the account's own HOME (its home-naming tests assert
# that), and each shard's NODECODE_HOME sits inside it: the startup tests
# resolve the store from $HOME and expect the home under it, as on a person's
# box.
suite_home="$(mktemp -d "$HOME/.nodecode-ci-suite.XXXXXX")"
trap 'rm -rf "$scratch" "$suite_home"; [ -n "$release_pid" ] && kill "$release_pid" 2>/dev/null' EXIT
mkdir -p "$scratch/cache"
# Quicklisp comes in through an init file of the run's own; the worker tests
# start a bare child SBCL that reads only the account's ~/.sbclrc, so that must
# load quicklisp too. A runner account is set up by its job; a person's init
# file is never written.
printf '(load "%s")\n' "$ql_setup" > "$scratch/sbclrc"
if ! grep -qs quicklisp "$HOME/.sbclrc"; then
  if [ -n "$gh_actions" ]; then
    printf '(load "%s")\n' "$ql_setup" > "$HOME/.sbclrc"
  else
    echo "ci-linux: ~/.sbclrc loads no quicklisp; the worker tests' child SBCL needs one, e.g. (load \"$ql_setup\")" >&2
    exit 1
  fi
fi
# The layer tests commit and merge with git: an account with no identity of its
# own (a CI account) gets one for this run only.
if ! git config user.email >/dev/null 2>&1; then
  export GIT_AUTHOR_NAME='nodecode CI' GIT_AUTHOR_EMAIL='ci@nodecode.invalid'
  export GIT_COMMITTER_NAME="$GIT_AUTHOR_NAME" GIT_COMMITTER_EMAIL="$GIT_AUTHOR_EMAIL"
fi
sbcl_run() {
  # sbcl_run NODECODE_HOME XDG_CACHE_HOME FORM: one SBCL on the suite's SBCL
  # and quicklisp, from src/, with the system loaded and FORM evaluated.
  ( cd src && NODECODE_HOME="$1" XDG_CACHE_HOME="$2" \
      PATH="$sbcl_root/bin:$PATH" SBCL_HOME="$sbcl_root/lib/sbcl" \
      timeout 5400 sbcl --dynamic-space-size 4096 --control-stack-size 64MB --noinform --non-interactive \
      --userinit "$scratch/sbclrc" --eval '(require :asdf)' --eval '(push (truename ".") asdf:*central-registry*)' \
      --eval '(asdf:load-system :nodecode/test)' --eval "$3" )
}
# One compile, then the shards: each loads the compiled suite from a cache of
# its own (hard links to the one compile), so no two processes share a cache
# or a home. Shard I of N runs every test whose place in the suite is I
# modulo N (nodecode.test::run-shard), the same split for a revision every
# time, so a red shard reruns alone with its two numbers.
shards="${CI_SHARDS:-12}"
log="$out/suite.log"
mkdir -p "$suite_home/compile/nodecode-home"
if ! sbcl_run "$suite_home/compile/nodecode-home" "$scratch/cache" '(values)' > "$out/compile.log" 2>&1; then
  tail -40 "$out/compile.log"
  echo 'ci-linux: the suite did not compile' >&2
  exit 1
fi
done_step compile
step_start=$SECONDS
set +e
pids=()
for i in $(seq 0 $((shards - 1))); do
  mkdir -p "$suite_home/s$i/nodecode-home" "$scratch/cache-$i"
  cp -al "$scratch/cache/." "$scratch/cache-$i/"
  sbcl_run "$suite_home/s$i/nodecode-home" "$scratch/cache-$i" \
    "(nodecode.test::run-shard $i $shards)" > "$out/shard-$i.log" 2>&1 &
  pids+=($!)
done
rc=0
for i in "${!pids[@]}"; do
  wait "${pids[$i]}"; r=$?
  [ "$r" != 0 ] && rc=$r && echo "ci-linux: shard $i of $shards exited $r"
done
set -e
cat "$out"/shard-*.log > "$log"
passed="$(grep -c '^pass ' "$log" || true)"
failed="$(grep -c '^FAIL ' "$log" || true)"
grep -h ' tests, [0-9]* failed$' "$out"/shard-*.log || true
done_step "suite ($shards shards)"
end_group

if [ -n "$release_pid" ]; then
  step 'release build (pack.sh, beside the suite)'
  set +e
  wait "$release_pid"; release_rc=$?
  release_pid=''
  set -e
  cat "$release_log"
  times+=("release $(cat "$scratch/release.secs" 2>/dev/null || echo '?')s (beside the suite)")
  end_group
  if [ "$release_rc" != 0 ]; then
    echo "ci-linux: the release build failed ($release_rc)" >&2
    exit "$release_rc"
  fi
fi

printf '\nsuite: %s passed, %s failed (worst shard exit %s); log %s\n' "$passed" "$failed" "$rc" "$log"
if [ "$failed" != 0 ]; then
  # Each FAIL line and the reasons indented under it.
  awk '/^FAIL /{p=1; print; next} p && /^       /{print; next} {p=0}' "$log"
  if [ -n "$gh_actions" ]; then
    grep '^FAIL ' "$log" | sed 's/^FAIL /::error title=test failed::/'
  fi
fi
printf 'time: %s, total %ss\n' "${times[*]}" "$((SECONDS - started))"
if [ "$rc" != 0 ] || [ "$failed" != 0 ] || [ "$passed" = 0 ]; then
  [ "$passed" = 0 ] && tail -40 "$log"
  exit 1
fi
echo 'ci-linux: green'
