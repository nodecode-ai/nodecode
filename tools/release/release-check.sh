#!/usr/bin/env bash
# release-check.sh --- the Release workflow's first job: which build the run
# names, and whether it may be released.
#
# NODECODE_RELEASE_MODE says what the run is (the workflow sets it; without
# it a pushed tag is a publish and anything else a rehearsal):
#
#   publish   a tag v<version> releases the commit it names, to stable: only
#             when that commit's nodecode.asd declares <version> -- publish.sh
#             names the release after that line, so a tag that disagrees with
#             it would publish one version under another's name -- and only
#             when the commit is on main, where CI built and tested it.
#   rehearse  a run by hand: the commit it checked out and its version.
#   nightly   main's newest commit a CI run went green on (that run's tree,
#             so a squash merge of a green pull request counts), for the
#             nightly channel. A night whose pick every nightly pointer
#             already names, or with no green commit near main's head, is
#             skipped: skip=true, and no build job runs.
#
# A publish also says whether the nightly follows it: nightly_follows=true when
# the Linux nightly pointer names a commit the tag carries, so publish.sh will
# move the nightly homes onto this release too, and the Linux job proves that
# upgrade from the nightly they run.
#
# Prints the commit, its nine-digit label and its version, and writes them as
# commit=, label=, version=, mode=, skip= and nightly_follows= to
# $GITHUB_OUTPUT when the workflow sets it.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/../.." && pwd)"
. "$here/layout.sh"
cd "$repo"

mode="${NODECODE_RELEASE_MODE:-}"
if [ -z "$mode" ]; then
  if [ "${GITHUB_REF_TYPE:-}" = tag ]; then mode=publish; else mode=rehearse; fi
fi
case "$mode" in
  publish|rehearse|nightly) ;;
  *) echo "release: the mode is publish, rehearse or nightly, not '$mode'" >&2; exit 2 ;;
esac

git fetch -q origin main
main="$(git rev-parse FETCH_HEAD)"
skip=false

if [ "$mode" = nightly ]; then
  # The trees CI went green on: its own successful runs, newest first, each
  # naming the tree it tested. NODECODE_RELEASE_GREEN_TREES stands in for
  # GitHub (release-test.sh).
  if [ -n "${NODECODE_RELEASE_GREEN_TREES+set}" ]; then
    green="$NODECODE_RELEASE_GREEN_TREES"
  else
    green="$(gh api "repos/${GITHUB_REPOSITORY:?the run names no repository}/actions/workflows/ci.yml/runs?status=success&per_page=100" \
               --jq '.workflow_runs[].head_commit.tree_id')" || {
      echo 'release: GitHub did not answer which CI runs went green; no nightly' >&2
      exit 1
    }
  fi
  commit=''
  while read -r hash tree; do
    if grep -qxF "$tree" <<<"$green"; then commit="$hash"; break; fi
  done < <(git log --first-parent --format='%H %T' -n 50 "$main")
  if [ -z "$commit" ]; then
    echo "release: none of main's newest 50 commits has a green CI run; no nightly tonight"
    commit="$main"
    skip=true
  fi
else
  commit="$(git rev-parse 'HEAD^{commit}')"
fi

# Nine digits, explicitly: --short alone sizes itself by the clone (pack.sh).
label="$(git rev-parse --short=9 "$commit")"
asd="$(layout_asd_at "$commit")"
version="$(git show "$commit:$asd" | sed -n 's/^ *:version "\([0-9][0-9A-Za-z.-]*\)".*/\1/p' | head -n 1)"
[ -n "$version" ] || { echo "release: $asd at $label declares no :version" >&2; exit 1; }

if [ "$mode" = publish ]; then
  tag="${GITHUB_REF_NAME:?the run names no tag}"
  [ "${GITHUB_REF_TYPE:-}" = tag ] || { echo "release: a publish is a pushed tag; $tag is not one" >&2; exit 1; }
  [ "$tag" = "v$version" ] || {
    echo "release: $tag names $label, whose $asd declares $version: tag it v$version, or bump :version and tag that commit" >&2
    exit 1
  }
fi

git merge-base --is-ancestor "$commit" "$main" || {
  echo "release: $label is not on main; only a commit main carries, built and tested there, is released" >&2
  exit 1
}

# A night with nothing new: every nightly pointer already names the pick. The
# read carries a cache-buster: this is a decision, and the zone holds the
# plain URL for hours. A pointer that does not answer (none yet, a host
# mid-restart) is a night to publish.
if [ "$mode" = nightly ] && [ "$skip" = false ]; then
  base_url="${NODECODE_RELEASE_BASE_URL:-https://nodecode.ai}"
  base_url="${base_url%/}"
  named=0
  for platform in x86_64-linux arm64-darwin x86_64-windows; do
    pointer="$(curl -fsS -m 20 "$base_url/dl/nightly-$platform?cb=$(date +%s)" 2>/dev/null | tr -d '[:space:]')" || pointer=''
    [ "$pointer" = "$label" ] && named=$((named + 1))
  done
  if [ "$named" = 3 ]; then
    echo "release: every nightly pointer already names $label, main's newest green commit; nothing to publish"
    skip=true
  fi
fi

# A publish past the nightly carries the nightly homes with it (publish.sh), so
# the upgrade from the nightly they run is proved too. Read like the nightly's
# own check: a cache-buster, and a pointer that does not answer follows nothing.
nightly_follows=false
if [ "$mode" = publish ]; then
  base_url="${NODECODE_RELEASE_BASE_URL:-https://nodecode.ai}"
  base_url="${base_url%/}"
  nightly="$(curl -fsS -m 20 "$base_url/dl/nightly-x86_64-linux?cb=$(date +%s)" 2>/dev/null | tr -d '[:space:]')" || nightly=''
  if [ -n "$nightly" ] && [ "$nightly" != "$label" ] && git merge-base --is-ancestor "$nightly" "$commit" 2>/dev/null; then
    nightly_follows=true
    echo "release: the nightly names $nightly, which $label carries: the nightly homes take this release too"
  fi
fi

case "$mode" in
  publish) echo "release: $label is nodecode $version, tagged $tag" ;;
  rehearse) echo "release: $label is nodecode $version, a rehearsal" ;;
  nightly) [ "$skip" = true ] || echo "release: $label is nodecode $version, main's newest green commit, for the nightly channel" ;;
esac
if [ -n "${GITHUB_OUTPUT:-}" ]; then
  printf 'commit=%s\nlabel=%s\nversion=%s\nmode=%s\nskip=%s\nnightly_follows=%s\n' \
    "$commit" "$label" "$version" "$mode" "$skip" "$nightly_follows" >> "$GITHUB_OUTPUT"
fi
