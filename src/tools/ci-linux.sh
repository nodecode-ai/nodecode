#!/usr/bin/env bash
# ci-linux.sh --- the Linux CI job: lint, the release build, the core suite.
#
# SPDX-License-Identifier: MIT
#
# Run from a checkout's root (the CI workflow runs it on the Linux runner; any
# Linux box with sbcl, just, git, curl, gcc and make does). Three steps, each
# timed; the first red one ends the run with its own exit code:
#
#   lint     `just lint': every .lisp and .asd under src/.
#   release  `just tarball HEAD OUT': pack.sh builds the committed revision the
#            way a release does -- the patched SBCL, quicklisp on the pinned
#            dist, the shipped cells, the gate that refuses an account's home
#            in the tree -- under NODECODE_BUILD_HOME, a root of CI's own that
#            keeps the SBCL build and quicklisp between runs. Never the release
#            root (/var/tmp/nodecode-build): a publish on the same box must not
#            share its caches with a CI run.
#   suite    asdf:test-system :nodecode on that same SBCL and quicklisp, the
#            shipped cells pulled into the checkout, under a scratch HOME,
#            NODECODE_HOME and XDG_CACHE_HOME. Every FAIL line is named, with
#            its reasons, and any one fails the run.
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

step 'release build (pack.sh)'
NODECODE_BUILD_HOME="$build_home" just tarball HEAD "$out"
done_step release

step 'core suite'
# The SBCL and quicklisp pack.sh just used: its cache key is this revision's
# build.sh and patches.
key="$(cat src/tools/sbcl/build.sh src/tools/sbcl/patches/* | sha256sum | cut -d' ' -f1)"
sbcl_root="$build_home/env/.cache/nodecode-sbcl/$key"
ql_setup="$build_home/env/quicklisp/setup.lisp"
[ -x "$sbcl_root/bin/sbcl" ] || { echo "ci-linux: no SBCL at $sbcl_root after the release build" >&2; exit 1; }
[ -f "$ql_setup" ] || { echo "ci-linux: no quicklisp at $ql_setup after the release build" >&2; exit 1; }
src/tools/pull-cells.sh
# The suite runs under the account's own HOME (its home-naming tests assert
# that), and its NODECODE_HOME sits inside it: the startup tests resolve the
# store from $HOME and expect the home under it, as on a person's box.
suite_home="$(mktemp -d "$HOME/.nodecode-ci-suite.XXXXXX")"
trap 'rm -rf "$scratch" "$suite_home"' EXIT
mkdir -p "$suite_home/nodecode-home" "$scratch/cache"
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
log="$out/suite.log"
set +e
( cd src && NODECODE_HOME="$suite_home/nodecode-home" XDG_CACHE_HOME="$scratch/cache" \
    PATH="$sbcl_root/bin:$PATH" SBCL_HOME="$sbcl_root/lib/sbcl" \
    timeout 5400 sbcl --dynamic-space-size 4096 --control-stack-size 64MB --noinform --non-interactive \
    --userinit "$scratch/sbclrc" --eval '(require :asdf)' --eval '(push (truename ".") asdf:*central-registry*)' \
    --eval '(asdf:test-system :nodecode)' ) > "$log" 2>&1
rc=$?
set -e
passed="$(grep -c '^pass ' "$log" || true)"
failed="$(grep -c '^FAIL ' "$log" || true)"
done_step suite
end_group

printf '\nsuite: %s passed, %s failed (sbcl exit %s); log %s\n' "$passed" "$failed" "$rc" "$log"
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
