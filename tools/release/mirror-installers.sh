#!/usr/bin/env bash
# mirror-installers.sh --- the site's branch serves authority's installers:
# committed there and pushed, so the next image the site builds carries them.
#
# SPDX-License-Identifier: MIT
#
#   mirror-installers.sh SITE_REMOTE BRANCH INSTALL_SH INSTALL_PS1 MESSAGE
#
# nodecode.ai serves /install.sh and /install.ps1 out of the web image's
# public/, and that image is built from the site repository's main (its
# deploy workflow, or deploy/lab.sh from a clone of it). A mirror committed in
# a local clone and never pushed was reverted by every image built after it:
# on 2026-10-22 the site served a 269-line install.ps1 from September, without
# the keep of a release already laid out, so a second paste of the one-liner
# downloaded 30 MB again and laid a fifth 101 MB copy out.
#
# So the mirror is made where the site builds from: a scratch clone of BRANCH,
# the two files copied over, one commit through the commit helper, pushed. A
# push that loses a race to another lane's is rebased and pushed again. A
# branch that already carries both files byte for byte is left alone.
set -euo pipefail

remote="${1:?usage: mirror-installers.sh SITE_REMOTE BRANCH INSTALL_SH INSTALL_PS1 MESSAGE}"
branch="${2:?branch}"
install_sh="${3:?install.sh}"
install_ps1="${4:?install.ps1}"
message="${5:?message}"
[ -s "$install_sh" ] || { echo "mirror-installers: no $install_sh" >&2; exit 1; }
[ -s "$install_ps1" ] || { echo "mirror-installers: no $install_ps1" >&2; exit 1; }

. "$(dirname "${BASH_SOURCE[0]}")/publish-config.sh"
commit_helper="$(publish_setting NODECODE_GIT_COMMIT_HELPER)" || exit 1
[ -x "$commit_helper" ] || { echo "mirror-installers: missing commit helper $commit_helper" >&2; exit 1; }

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT INT TERM
clone="$work/site"
git clone -q --depth 50 --branch "$branch" --single-branch "$remote" "$clone"
mkdir -p "$clone/web/public"

if cmp -s "$install_sh" "$clone/web/public/install.sh" && cmp -s "$install_ps1" "$clone/web/public/install.ps1"; then
  echo "mirror-installers: the site's $branch already carries authority's installers"
  exit 0
fi
cp "$install_sh" "$clone/web/public/install.sh"
cp "$install_ps1" "$clone/web/public/install.ps1"
(cd "$clone" && "$commit_helper" --message "$message" --path web/public/install.sh --path web/public/install.ps1) >/dev/null

for try in 1 2 3 4 5; do
  if git -C "$clone" push -q origin "HEAD:$branch"; then
    echo "mirror-installers: pushed authority's installers to the site's $branch ($(git -C "$clone" rev-parse --short HEAD))"
    exit 0
  fi
  [ "$try" -lt 5 ] || break
  echo "mirror-installers: the site's $branch moved; rebasing onto it" >&2
  git -C "$clone" pull -q --rebase origin "$branch"
done
echo "mirror-installers: could not push to the site's $branch" >&2
exit 1
