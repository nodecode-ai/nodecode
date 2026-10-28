#!/usr/bin/env bash
# release-publish.sh --- the Release workflow's last job: the three archives
# the build jobs made, published under their label by publish.sh -- or, for a
# run by hand, checked and every credential the publish uses tried, with
# nothing moved.
#
#   release-publish.sh publish|rehearse DIST
#
# DIST holds what the build jobs uploaded: the Linux and Mac tarballs and the
# Windows zip, each beside its sidecar. publish.sh finds every archive already
# there and packs none (its `have' check), so this job only transports; the
# Windows tar.zst is packed here from the zip first, on this build host.
#
# The settings come from the environment, which the workflow fills from the
# `release' environment's secrets -- that environment answers only a v* tag
# or main, so a pull request's code never reads them:
#   PUBLISH_ENV          publish.env lines (publish-config.sh): the host alias,
#                        its layout, the web container, the GitHub repository,
#                        and NODECODE_PUBLISH_SITE_REMOTE, the site repository
#   PUBLISH_SSH_CONFIG   one ssh Host block, for that alias
#   PUBLISH_SSH_KEY      the key that host accepts for a publish
#   PUBLISH_KNOWN_HOSTS  that host's key, pinned
#   SITE_DEPLOY_KEY      the site repository's deploy key, for the installers
#   GH_TOKEN             the run's own token, for the release on GitHub
# They are written into one folder of this run's own, removed at exit. ssh,
# scp and git reach the hosts through shims that name that folder's config, so
# the runner account's ~/.ssh is never read or written.
set -euo pipefail

mode="${1:?publish or rehearse}"
dist="${2:?DIST, the folder the build jobs archives were downloaded to}"
case "$mode" in
  publish|rehearse) ;;
  *) echo "release: the mode is publish or rehearse, not '$mode'" >&2; exit 2 ;;
esac

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo"
commit="$(git rev-parse 'HEAD^{commit}')"
label="$(git rev-parse --short=9 "$commit")"
dist="$(cd "$dist" && pwd)"

for name in PUBLISH_ENV PUBLISH_SSH_CONFIG PUBLISH_SSH_KEY PUBLISH_KNOWN_HOSTS SITE_DEPLOY_KEY GH_TOKEN; do
  [ -n "${!name:-}" ] || { echo "release: $name is empty: the release environment holds no such secret" >&2; exit 1; }
done

# --- the archives -------------------------------------------------------------
# Each arrives with its sidecar, and the two agree; an archive of another
# label means a build job built some other commit.
for name in "nodecode-$label-x86_64-linux.tar.gz" "nodecode-$label-arm64-darwin.tar.gz" \
            "nodecode-$label-x86_64-windows.zip"; do
  [ -s "$dist/$name" ] && [ -s "$dist/$name.sha256" ] || {
    echo "release: $name or its sidecar did not arrive from its build job" >&2
    exit 1
  }
  (cd "$dist" && sha256sum -c --quiet "$name.sha256") || {
    echo "release: $name does not match its sidecar" >&2
    exit 1
  }
  echo "release: $name $(cut -d' ' -f1 < "$dist/$name.sha256")"
done
stray=''
for f in "$dist"/* "$dist"/.[!.]*; do
  [ -e "$f" ] || continue
  case "$(basename "$f")" in "nodecode-$label-"*) ;; *) stray="$stray $(basename "$f")" ;; esac
done
[ -z "$stray" ] || { echo "release: $dist holds what no build job of $label made:$stray" >&2; exit 1; }
zst="$dist/nodecode-$label-x86_64-windows.tar.zst"
if [ ! -s "$zst" ]; then
  src/tools/pack-windows-zst.sh --here "$dist/nodecode-$label-x86_64-windows.zip"
fi

# --- the credentials, in a folder of the run's own ----------------------------
umask 077
priv="$(mktemp -d "${RUNNER_TEMP:-${TMPDIR:-/tmp}}/release-publish.XXXXXX")"
trap 'rm -rf "$priv"' EXIT INT TERM
printf '%s\n' "$PUBLISH_ENV" > "$priv/publish.env"
printf '%s\n' "$PUBLISH_SSH_KEY" > "$priv/host.key"
printf '%s\n' "$SITE_DEPLOY_KEY" > "$priv/site.key"
printf '%s\n' "$PUBLISH_KNOWN_HOSTS" > "$priv/known_hosts"
# GitHub's own host keys, from GitHub's API over TLS.
gh api meta --jq '.ssh_keys[] | "github.com " + .' >> "$priv/known_hosts"
{
  printf 'Host github.com\n  User git\n  IdentityFile %s\n  IdentitiesOnly yes\n\n' "$priv/site.key"
  printf '%s\n' "$PUBLISH_SSH_CONFIG"
  printf '  IdentityFile %s\n  IdentitiesOnly yes\n\n' "$priv/host.key"
  printf 'Host *\n  UserKnownHostsFile %s\n  StrictHostKeyChecking yes\n  BatchMode yes\n' "$priv/known_hosts"
} > "$priv/ssh_config"
mkdir "$priv/bin"
for tool in ssh scp; do
  printf '#!/bin/sh\nexec %s -F %s "$@"\n' "$(command -v "$tool")" "$priv/ssh_config" > "$priv/bin/$tool"
done
chmod 0700 "$priv/bin"/*
export PATH="$priv/bin:$PATH"
export NODECODE_PUBLISH_CONFIG="$priv/publish.env"

. src/tools/publish-config.sh
host="$(publish_setting NODECODE_PUBLISH_HOST)"
remote_dir="$(publish_setting NODECODE_PUBLISH_REMOTE_DIR)"
web_container="$(publish_setting NODECODE_PUBLISH_WEB_CONTAINER)"
site_remote="$(publish_setting NODECODE_PUBLISH_SITE_REMOTE)"
github_repo="$(publish_setting NODECODE_PUBLISH_GITHUB_REPO)"

if [ "$mode" = publish ]; then
  # Not exec: the trap must still clear the credentials when it returns.
  rc=0
  src/tools/publish.sh "$commit" "$dist" || rc=$?
  exit "$rc"
fi

# --- the rehearsal: every door the publish opens, opened and shut -------------
ssh "$host" "test -d '$remote_dir' && docker ps --format '{{.Names}}' | grep -qx '$web_container'" || {
  echo "release: the publish host does not answer with its download folder and running web container" >&2
  exit 1
}
echo 'release: the publish host answers: its download folder and web container are there'
site_branch="${NODECODE_PUBLISH_SITE_BRANCH:-main}"
git ls-remote --exit-code "$site_remote" "refs/heads/$site_branch" > /dev/null || {
  echo "release: the site repository's $site_branch does not answer the deploy key" >&2
  exit 1
}
echo "release: the site repository's $site_branch answers the deploy key"
gh api "repos/$github_repo/releases" --jq 'length' > /dev/null
echo "release: $github_repo's releases answer the run's token"
echo "release-rehearsal-ok $label: all three archives, the Windows tar.zst, the host, the site and GitHub answer; nothing moved"
