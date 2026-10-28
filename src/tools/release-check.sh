#!/usr/bin/env bash
# release-check.sh --- the Release workflow's first job: which build the run
# names, and whether a pushed tag may release it.
#
# A tag v<version> releases the commit it names only when that commit's
# src/nodecode.asd declares <version> -- publish.sh names the release after
# that line, so a tag that disagrees with it would publish one version under
# another's name -- and only when the commit is on main, where CI built and
# tested it before any tag named it. A run by hand has no tag: it names the
# commit it checked out and that commit's version, for the rehearsal.
#
# Prints the commit, its nine-digit label and its version, and writes them as
# commit=, label= and version= to $GITHUB_OUTPUT when the workflow sets it.
set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo"

commit="$(git rev-parse 'HEAD^{commit}')"
# Nine digits, explicitly: --short alone sizes itself by the clone (pack.sh).
label="$(git rev-parse --short=9 "$commit")"
version="$(git show "$commit:src/nodecode.asd" | sed -n 's/^ *:version "\([0-9][0-9A-Za-z.-]*\)".*/\1/p' | head -n 1)"
[ -n "$version" ] || { echo "release: src/nodecode.asd at $label declares no :version" >&2; exit 1; }

if [ "${GITHUB_REF_TYPE:-}" = tag ]; then
  tag="${GITHUB_REF_NAME:?the run names no tag}"
  [ "$tag" = "v$version" ] || {
    echo "release: $tag names $label, whose src/nodecode.asd declares $version: tag it v$version, or bump :version and tag that commit" >&2
    exit 1
  }
fi

git fetch -q origin main
git merge-base --is-ancestor "$commit" FETCH_HEAD || {
  echo "release: $label is not on main; only a commit main carries, built and tested there, is released" >&2
  exit 1
}

echo "release: $label is nodecode $version${tag:+, tagged $tag}"
if [ -n "${GITHUB_OUTPUT:-}" ]; then
  printf 'commit=%s\nlabel=%s\nversion=%s\n' "$commit" "$label" "$version" >> "$GITHUB_OUTPUT"
fi
