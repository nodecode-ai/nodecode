#!/usr/bin/env bash
# release-test.sh --- hermetic checks of the Release workflow's scripts.
#
# release-check.sh releases a tag only when src/nodecode.asd declares its
# version and main carries its commit. release-publish.sh's rehearsal takes
# the three archives only with their sidecars and nothing else beside them,
# packs the Windows tar.zst, reaches the publish host only through a config of
# its own run, and leaves no credential behind. No network and no real host:
# ssh and gh are fakes, the site repository a local one.
set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
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
mkdir -p "$work/src/tools"
printf '%s\n' '(defsystem "nodecode"' '  :version "1.2.3")' > "$work/src/nodecode.asd"
git_c -C "$work" add -A
git_c -C "$work" commit -qm one
git -C "$work" remote add origin "$origin"
git -C "$work" push -q origin main
for f in release-check.sh release-publish.sh publish-config.sh pack-windows-zst.sh; do
  cp "$repo/src/tools/$f" "$work/src/tools/$f"
done
label="$(git -C "$work" rev-parse --short=9 HEAD)"

# --- release-check.sh -----------------------------------------------------------
check() { (cd "$work" && env "$@" bash src/tools/release-check.sh) > "$root/check-out" 2>&1; }
check GITHUB_REF_TYPE=tag GITHUB_REF_NAME=v1.2.3 GITHUB_OUTPUT="$root/gh-output" \
  || fail "a matching tag on main was refused: $(cat "$root/check-out")"
grep -qx 'version=1.2.3' "$root/gh-output" || fail 'the version was not written for the workflow'
grep -qx "label=$label" "$root/gh-output" || fail 'the nine-digit label was not written for the workflow'
say 'a tag that matches the asd, on main, is released'

if check GITHUB_REF_TYPE=tag GITHUB_REF_NAME=v1.2.4; then fail 'a tag the asd does not declare was released'; fi
grep -q 'v1.2.4 names .*whose src/nodecode.asd declares 1.2.3' "$root/check-out" \
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
    bash src/tools/release-publish.sh rehearse "$dist") > "$root/rehearse-out" 2>&1
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
say 'a missing or wrong archive, a missing secret, or a silent host stops it, and no key is left'
