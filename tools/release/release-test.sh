#!/usr/bin/env bash
# release-test.sh --- hermetic checks of the Release workflow's scripts.
#
# release-check.sh releases a tag only when nodecode.asd declares its
# version and main carries its commit. release-publish.sh's rehearsal takes
# the three archives only with their sidecars and nothing else beside them,
# packs the Windows tar.zst, reaches the publish host only through a config of
# its own run, and leaves no credential behind. No network and no real host:
# ssh and gh are fakes, the site repository a local one.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/../.." && pwd)"
root="$(mktemp -d)"
trap 'rm -rf "$root"' EXIT
fail() { echo "release-test: $1" >&2; exit 1; }
say() { printf 'release-test: %s\n' "$1"; }
git_c() { git -c user.name=release-test -c user.email=release-test@invalid "$@"; }

# --- a repository with an origin, as a runner's checkout has ------------------
origin="$root/origin.git"
work="$root/work"
git init -q --bare "$origin"
git init -q -b main "$work"
mkdir -p "$work/tools/release"
printf '%s\n' '(defsystem "nodecode"' '  :version "1.2.3")' > "$work/nodecode.asd"
git_c -C "$work" add -A
git_c -C "$work" commit -qm one
git -C "$work" remote add origin "$origin"
git -C "$work" push -q origin main
for f in layout.sh release-check.sh release-publish.sh publish-config.sh pack-windows-zst.sh; do
  cp "$repo/tools/release/$f" "$work/tools/release/$f"
done
label="$(git -C "$work" rev-parse --short=9 HEAD)"

# --- every script release.yml runs by path is executable in git ------------------
# 2522703 rewrote release-publish.sh 0644 and the next run's Publish answered
# `Permission denied' (exit 126) before a line of it ran.
while read -r script; do
  script_mode="$(git -C "$repo" ls-files -s -- "$script" | cut -d' ' -f1)"
  [ "$script_mode" = 100755 ] || fail "$script is $script_mode in git; release.yml runs it by path"
done < <(grep -o -E '(^|[ "])tools/[a-z0-9/-]+\.sh' "$repo/.github/workflows/release.yml" | tr -d ' "' | sort -u)
say 'every script release.yml runs by path is executable in git'

# --- release-check.sh -----------------------------------------------------------
# No real host is ever read: a pointer read goes to the .invalid host, or to
# the stub curl the later checks put on PATH.
check() { (cd "$work" && env NODECODE_RELEASE_BASE_URL=https://release-test.invalid "$@" bash tools/release/release-check.sh) > "$root/check-out" 2>&1; }
check GITHUB_REF_TYPE=tag GITHUB_REF_NAME=v1.2.3 GITHUB_OUTPUT="$root/gh-output" \
  || fail "a matching tag on main was refused: $(cat "$root/check-out")"
grep -qx 'version=1.2.3' "$root/gh-output" || fail 'the version was not written for the workflow'
grep -qx "label=$label" "$root/gh-output" || fail 'the nine-digit label was not written for the workflow'
say 'a tag that matches the asd, on main, is released'

if check GITHUB_REF_TYPE=tag GITHUB_REF_NAME=v1.2.4; then fail 'a tag the asd does not declare was released'; fi
grep -q 'v1.2.4 names .*whose nodecode.asd declares 1.2.3' "$root/check-out" \
  || fail "the disagreeing tag was not named: $(cat "$root/check-out")"
say 'a tag the asd does not declare is refused'

git -C "$work" checkout -q -b side
echo side > "$work/side"
git_c -C "$work" add side
git_c -C "$work" commit -qm side
if check GITHUB_REF_TYPE=tag GITHUB_REF_NAME=v1.2.3; then fail 'a commit main does not carry was released'; fi
grep -q 'is not on main' "$root/check-out" || fail "the off-main commit was not named: $(cat "$root/check-out")"
git -C "$work" checkout -q main
say 'a commit main does not carry is refused'

check || fail "a run by hand on main was refused: $(cat "$root/check-out")"
say 'a run by hand needs no tag'

# --- pack-windows-zst.sh: one zip, one tar.zst ----------------------------------------
# A publish that uploads and then fails is re-run, and the re-run packs the
# Windows tar.zst again: the host already holds the first pack under that name,
# and a published name keeps its bytes, so the second pack must be the first.
packdir="$root/pack"
mkdir -p "$packdir/one" "$packdir/two"
python3 - "$packdir/one/nodecode-$label-x86_64-windows.zip" <<'PY'
import sys, zipfile
with zipfile.ZipFile(sys.argv[1], 'w') as z:
    z.writestr('nodecode/nodecode.exe', b'MZ' + bytes(range(256)) * 64)
    z.writestr('nodecode\\lib\\zstd.dll', b'dll' * 1000)
    z.writestr('nodecode/README.txt', b'read me\n')
PY
cp "$packdir/one/nodecode-$label-x86_64-windows.zip" "$packdir/two/"
bash "$work/tools/release/pack-windows-zst.sh" --here "$packdir/one/nodecode-$label-x86_64-windows.zip" > "$root/pack-out" 2>&1 \
  || fail "the first pack failed: $(cat "$root/pack-out")"
sleep 1.1
(umask 0077 && bash "$work/tools/release/pack-windows-zst.sh" --here "$packdir/two/nodecode-$label-x86_64-windows.zip") > "$root/pack-out" 2>&1 \
  || fail "the second pack failed: $(cat "$root/pack-out")"
[ "$(cut -d' ' -f1 < "$packdir/one/nodecode-$label-x86_64-windows.tar.zst.sha256")" = \
  "$(cut -d' ' -f1 < "$packdir/two/nodecode-$label-x86_64-windows.tar.zst.sha256")" ] \
  || fail 'the same zip packed twice gave two tar.zst bodies'
say 'the same zip packs to the same tar.zst, a second later and under another umask'

# --- release-publish.sh rehearse ---------------------------------------------------
site="$root/site.git"
git init -q --bare "$site"
git -C "$work" push -q "$site" main
mkdir -p "$root/bin" "$root/runner-temp"
cat > "$root/bin/gh" <<'EOF'
#!/bin/sh
case "$*" in
  'api meta '*) echo 'github.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAItest' ;;
  'api repos/test/nodecode/releases '*) echo 0 ;;
  *) echo "gh: unexpected $*" >&2; exit 1 ;;
esac
EOF
# The fake ssh keeps what it was given: the config it was pointed at, and the
# mode of every key that config names.
cat > "$root/bin/ssh" <<'EOF'
#!/bin/sh
printf '%s\n' "$*" >> "$RELEASE_TEST_SSH"
[ "$1" = -F ] || { echo 'ssh: no -F' >&2; exit 1; }
cp "$2" "$RELEASE_TEST_SSH.config"
for key in $(sed -n 's/^ *IdentityFile //p' "$2"); do stat -c '%a %n' "$key" >> "$RELEASE_TEST_SSH.modes"; done
exit "${RELEASE_TEST_SSH_EXIT:-0}"
EOF
chmod 0755 "$root/bin"/*

dist="$root/dist"
make_dist() {
  rm -rf "$dist"
  mkdir -p "$dist"
  printf 'linux tarball\n' > "$dist/nodecode-$label-x86_64-linux.tar.gz"
  printf 'mac tarball\n' > "$dist/nodecode-$label-arm64-darwin.tar.gz"
  python3 - "$dist/nodecode-$label-x86_64-windows.zip" <<'PY'
import sys, zipfile
with zipfile.ZipFile(sys.argv[1], 'w') as z:
    z.writestr('nodecode\\nodecode.exe', b'MZ' + b'\0' * 64)
    z.writestr('nodecode/README.txt', b'hello\n')
PY
  for f in "$dist"/nodecode-*; do (cd "$dist" && sha256sum "$(basename "$f")" > "$(basename "$f").sha256"); done
}
rehearse() {
  : > "$root/ssh"
  rm -f "$root/ssh.config" "$root/ssh.modes"
  (cd "$work" && env PATH="$root/bin:$PATH" RUNNER_TEMP="$root/runner-temp" RELEASE_TEST_SSH="$root/ssh" \
    PUBLISH_ENV="$(printf '%s\n' NODECODE_PUBLISH_HOST=pubhost NODECODE_PUBLISH_REMOTE_DIR=dl \
      NODECODE_PUBLISH_WEB_CONTAINER=web NODECODE_PUBLISH_SITE_REMOTE="$site" NODECODE_PUBLISH_GITHUB_REPO=test/nodecode)" \
    PUBLISH_SSH_CONFIG="$(printf '%s\n' 'Host pubhost' '  HostName pub.invalid' '  User pub')" \
    PUBLISH_SSH_KEY='host key' PUBLISH_KNOWN_HOSTS='pub.invalid ssh-ed25519 AAAAtest' \
    SITE_DEPLOY_KEY='site key' GH_TOKEN=token "$@" \
    bash tools/release/release-publish.sh "${mode:-rehearse}" "$dist") > "$root/rehearse-out" 2>&1
}

make_dist
rehearse || fail "the rehearsal failed: $(cat "$root/rehearse-out")"
grep -q "^release-rehearsal-ok $label" "$root/rehearse-out" || fail 'the rehearsal did not say it passed'
[ -s "$dist/nodecode-$label-x86_64-windows.tar.zst" ] && [ -s "$dist/nodecode-$label-x86_64-windows.tar.zst.sha256" ] \
  || fail 'the Windows tar.zst and its sidecar were not packed beside the zip'
grep -q "^-F .*/ssh_config pubhost test -d 'dl'" "$root/ssh" || fail "the host was not reached through the run's config: $(cat "$root/ssh")"
grep -qx 'Host pubhost' "$root/ssh.config" || fail 'the host block was not in the config'
grep -q '^  StrictHostKeyChecking yes' "$root/ssh.config" || fail 'the config does not pin host keys'
[ "$(grep -c '^600 ' "$root/ssh.modes")" = 2 ] || fail "a key was not owner-only: $(cat "$root/ssh.modes")"
[ -z "$(ls -A "$root/runner-temp")" ] || fail "the credentials outlived the run: $(ls -A "$root/runner-temp")"
say 'the rehearsal checks the archives, packs the tar.zst, reaches the host by its own config, and clears its keys'

make_dist
rm "$dist/nodecode-$label-arm64-darwin.tar.gz.sha256"
if rehearse; then fail 'an archive without its sidecar was taken'; fi
grep -q "nodecode-$label-arm64-darwin.tar.gz or its sidecar did not arrive" "$root/rehearse-out" || fail "the missing sidecar was not named: $(cat "$root/rehearse-out")"
make_dist
printf 'other\n' > "$dist/nodecode-$label-x86_64-linux.tar.gz"
if rehearse; then fail 'an archive that disagrees with its sidecar was taken'; fi
grep -q 'does not match its sidecar' "$root/rehearse-out" || fail "the mismatch was not named: $(cat "$root/rehearse-out")"
make_dist
printf 'x\n' > "$dist/nodecode-aaaaaaaaa-x86_64-linux.tar.gz"
if rehearse; then fail 'an archive of another label was taken'; fi
grep -q 'holds what no build job of' "$root/rehearse-out" || fail "the stray archive was not named: $(cat "$root/rehearse-out")"
make_dist
if rehearse SITE_DEPLOY_KEY=; then fail 'a rehearsal ran without a secret'; fi
grep -q 'SITE_DEPLOY_KEY is empty' "$root/rehearse-out" || fail "the missing secret was not named: $(cat "$root/rehearse-out")"
make_dist
if rehearse RELEASE_TEST_SSH_EXIT=1; then fail 'a host that does not answer passed the rehearsal'; fi
[ -z "$(ls -A "$root/runner-temp")" ] || fail 'a failed rehearsal left its credentials behind'

# --- release-publish.sh publish: the installer mirror commits in the run --------
# v0.1.3 (2026-10-28) was the first tag whose installers changed, and its
# publish died at the mirror: no NODECODE_GIT_COMMIT_HELPER in the run. The
# publish here is the real mirror-installers.sh against the local site.
cp "$repo/tools/release/mirror-installers.sh" "$work/tools/release/mirror-installers.sh"
cat > "$work/tools/release/publish.sh" <<'EOF'
#!/usr/bin/env bash
# A publish.sh of nothing but the installer mirror, the step the helper serves.
set -euo pipefail
. tools/release/publish-config.sh
d="$(mktemp -d)"
printf 'echo sh installer\n' > "$d/install.sh"
printf 'Write-Host ps1 installer\n' > "$d/install.ps1"
bash tools/release/mirror-installers.sh "$(publish_setting NODECODE_PUBLISH_SITE_REMOTE)" main \
  "$d/install.sh" "$d/install.ps1" 'release: test installers'
EOF
chmod 0755 "$work/tools/release/publish.sh"
make_dist
mode=publish rehearse || fail "a publish whose installers changed did not mirror them: $(cat "$root/rehearse-out")"
[ "$(git -C "$site" show main:web/public/install.ps1)" = 'Write-Host ps1 installer' ] \
  || fail 'the site main does not carry the new install.ps1'
[ "$(git -C "$site" log -1 --format=%s main)" = 'release: test installers' ] \
  || fail "the site's commit is not the mirror's: $(git -C "$site" log -1 --format=%s main)"
[ "$(git -C "$site" show --name-only --format= main | sort | tr '\n' ' ')" = 'web/public/install.ps1 web/public/install.sh ' ] \
  || fail 'the mirror commit carries more than the two installers'
[ -z "$(ls -A "$root/runner-temp")" ] || fail 'a publish left its credentials behind'
say 'a publish commits the changed installers to the site through the run helper'
say 'a missing or wrong archive, a missing secret, or a silent host stops it, and no key is left'

# --- release-publish.sh nightly: publish.sh on the nightly channel ------------------
# A nightly is a publish whose channel is nightly, of the commit the check
# named -- which may be older than the checkout running the job.
cat > "$work/tools/release/publish.sh" <<'EOF'
#!/usr/bin/env bash
printf 'channel=%s args=%s\n' "${NODECODE_PUBLISH_CHANNEL:-}" "$*" > "$RELEASE_TEST_PUBLISHED"
EOF
chmod 0755 "$work/tools/release/publish.sh"
head_commit="$(git -C "$work" rev-parse HEAD)"
make_dist
mode=nightly rehearse RELEASE_TEST_PUBLISHED="$root/published" || fail "the nightly publish failed: $(cat "$root/rehearse-out")"
grep -qx "channel=nightly args=$head_commit $dist" "$root/published" || fail "the nightly did not publish to nightly: $(cat "$root/published")"
make_dist
mode=publish rehearse RELEASE_TEST_PUBLISHED="$root/published" || fail "the tag publish failed: $(cat "$root/rehearse-out")"
grep -qx "channel=stable args=$head_commit $dist" "$root/published" || fail "a tag did not publish to stable: $(cat "$root/published")"
[ -z "$(ls -A "$root/runner-temp")" ] || fail 'a nightly left its credentials behind'
say 'a nightly publishes its commit to the nightly channel, a tag to stable'

# --- release-check.sh nightly: main's newest green commit -------------------------
# A nightly builds main's newest commit a CI run went green on -- by tree, so
# the squash of a green pull request counts -- and a night whose pick every
# nightly pointer already names builds nothing.
echo two > "$work/two"
git_c -C "$work" add two
git_c -C "$work" commit -qm two
git -C "$work" push -q origin main
first="$(git -C "$work" rev-parse HEAD~1)"
second="$(git -C "$work" rev-parse HEAD)"
mkdir -p "$root/curl-bin" "$root/pointers"
cat > "$root/curl-bin/curl" <<'EOF'
#!/bin/sh
for url; do :; done
name="${url##*/}"
name="${name%%\?*}"
[ -f "$RELEASE_TEST_POINTERS/$name" ] || exit 22
cat "$RELEASE_TEST_POINTERS/$name"
EOF
chmod 0755 "$root/curl-bin/curl"
nightly() {
  : > "$root/gh-output"
  check PATH="$root/curl-bin:$PATH" RELEASE_TEST_POINTERS="$root/pointers" NODECODE_RELEASE_MODE=nightly \
    GITHUB_OUTPUT="$root/gh-output" "$@"
}
tree_of() { git -C "$work" rev-parse "$1^{tree}"; }
nightly NODECODE_RELEASE_GREEN_TREES="$(tree_of "$first")" || fail "the nightly check failed: $(cat "$root/check-out")"
grep -qx "commit=$first" "$root/gh-output" || fail "the nightly did not pick the newest green commit: $(cat "$root/gh-output")"
grep -qx 'mode=nightly' "$root/gh-output" && grep -qx 'skip=false' "$root/gh-output" || fail "a green commit was skipped: $(cat "$root/gh-output")"
nightly NODECODE_RELEASE_GREEN_TREES="$(printf '%s\n' "$(tree_of "$first")" "$(tree_of "$second")")" || fail 'the nightly check failed'
grep -qx "commit=$second" "$root/gh-output" || fail "the nightly did not take main's head when it is green: $(cat "$root/gh-output")"
say "a nightly builds main's newest green commit"

nightly NODECODE_RELEASE_GREEN_TREES= || fail "a night with no green commit failed: $(cat "$root/check-out")"
grep -qx 'skip=true' "$root/gh-output" || fail 'a night with no green commit was not skipped'
grep -q "none of main's newest 50 commits has a green CI run" "$root/check-out" || fail "the red night was not said: $(cat "$root/check-out")"
for platform in x86_64-linux arm64-darwin x86_64-windows; do
  git -C "$work" rev-parse --short=9 "$second" > "$root/pointers/nightly-$platform"
done
nightly NODECODE_RELEASE_GREEN_TREES="$(tree_of "$second")" || fail 'the nightly check failed'
grep -qx 'skip=true' "$root/gh-output" || fail 'a night with nothing new was not skipped'
grep -q 'every nightly pointer already names' "$root/check-out" || fail "the night with nothing new was not said: $(cat "$root/check-out")"
rm "$root/pointers/nightly-arm64-darwin"
nightly NODECODE_RELEASE_GREEN_TREES="$(tree_of "$second")" || fail 'the nightly check failed'
grep -qx 'skip=false' "$root/gh-output" || fail 'a nightly one platform never got was skipped'
if check NODECODE_RELEASE_MODE=weekly; then fail 'an unknown mode was taken'; fi
say 'a night with no green commit, or nothing new on every platform, builds nothing'

# --- release-check.sh publish: whether the nightly follows the tag ----------------
# A tag past the nightly carries the nightly homes with it (publish.sh), and the
# Linux job proves that upgrade too: nightly_follows=true only when the Linux
# nightly pointer names a commit the tag carries and is not the tag itself.
release() {
  : > "$root/gh-output"
  check PATH="$root/curl-bin:$PATH" RELEASE_TEST_POINTERS="$root/pointers" GITHUB_REF_TYPE=tag \
    GITHUB_REF_NAME=v1.2.3 GITHUB_OUTPUT="$root/gh-output"
}
git -C "$work" rev-parse --short=9 "$first" > "$root/pointers/nightly-x86_64-linux"
release || fail "the tag past the nightly was refused: $(cat "$root/check-out")"
grep -qx 'nightly_follows=true' "$root/gh-output" || fail "a tag past the nightly does not carry it: $(cat "$root/gh-output")"
grep -q "the nightly names $(git -C "$work" rev-parse --short=9 "$first"), which .* carries" "$root/check-out" \
  || fail "the nightly the tag carries was not said: $(cat "$root/check-out")"
git -C "$work" rev-parse --short=9 "$second" > "$root/pointers/nightly-x86_64-linux"
release || fail 'the tag the nightly names was refused'
grep -qx 'nightly_follows=false' "$root/gh-output" || fail 'a nightly already on the tag was proved again'
rm "$root/pointers/nightly-x86_64-linux"
release || fail 'the tag with no nightly was refused'
grep -qx 'nightly_follows=false' "$root/gh-output" || fail 'a tag with no nightly to follow it says one follows'
git -C "$work" rev-parse --short=9 "$second" > "$root/pointers/nightly-x86_64-linux"
git -C "$work" checkout -q --detach "$first"
release || fail "a tag behind the nightly was refused: $(cat "$root/check-out")"
grep -qx 'nightly_follows=false' "$root/gh-output" || fail 'a tag behind the nightly would move it back'
git -C "$work" checkout -q main
say 'a tag carries the nightly only when the nightly is behind it'
