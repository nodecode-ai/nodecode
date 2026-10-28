#!/usr/bin/env bash
# publish-test.sh --- hermetic regression for the nodecode.ai/dl publisher.
set -eu

repo="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$repo"
[ -x "$repo/src/tools/publish.sh" ] || {
  echo 'publish-test: src/tools/publish.sh is not executable' >&2
  exit 1
}
root="$(mktemp -d)"
trap 'rm -rf "$root"' EXIT INT TERM
fake_bin="$root/bin"
mkdir -p "$fake_bin"

# The three arms the publisher packs: `just tarball' on this host, `just
# macos' on the Mac it reaches over ssh, `just windows' in the sandbox. Each
# writes the archive and sidecar its own arm produces, named for the label the
# publisher pinned.
cat > "$fake_bin/just" <<'EOF'
#!/bin/sh
set -eu
[ -z "${PUBLISH_TEST_NO_BUILD:-}" ] || { echo "just: built again: $*" >&2; exit 1; }
recipe="${1:-}"
out="${3:-dist}"
label="$(git rev-parse --short "${2:-HEAD}")"
mkdir -p "$out"
case "$recipe" in
  tarball) name="nodecode-${label}-x86_64-linux"; suffix=tar.gz ;;
  windows)
    [ -z "${PUBLISH_TEST_WINDOWS_FAIL:-}" ] || { echo 'just: the sandbox build failed' >&2; exit 1; }
    name="nodecode-${label}-x86_64-windows"; suffix=zip ;;
  macos)
    [ -z "${PUBLISH_TEST_MACOS_FAIL:-}" ] || { echo 'just: the Mac does not answer ssh' >&2; exit 1; }
    name="nodecode-${label}-arm64-darwin"; suffix=tar.gz ;;
  *) echo "just: no such arm: $*" >&2; exit 1 ;;
esac
printf 'hermetic %s release\n' "$recipe" > "$out/$name.$suffix"
(cd "$out" && sha256sum "$name.$suffix" > "$name.$suffix.sha256")
# The Windows arm stages the same tree twice: the zip an installer older
# than the tar.zst prefers, and the tar.zst the current one asks for first.
if [ "$recipe" = windows ]; then
  printf 'hermetic %s release, zstd transport\n' "$recipe" > "$out/$name.tar.zst"
  (cd "$out" && sha256sum "$name.tar.zst" > "$name.tar.zst.sha256")
fi
EOF

# No CI: every arm the publisher needs is packed on this host or in the
# sandbox it drives, so gh is for the releases page alone: the repository, a
# tag's commit, the release list, and one release made or updated. Anything
# else (a workflow, a run) is a reach for a runner. Every call is recorded,
# and the release lives in PUBLISH_TEST_GH_STATE across publishes.
cat > "$fake_bin/gh" <<'EOF'
#!/bin/sh
set -eu
printf '%s\n' "$*" >> "$PUBLISH_TEST_GH"
state="$PUBLISH_TEST_GH_STATE"
mkdir -p "$state"
assets() { n=0; for a in "$@"; do case "$a" in */nodecode-*) n=$((n + 1)) ;; esac; done; echo "$n"; }
notes() { while [ $# -gt 0 ]; do [ "$1" = --notes-file ] && cp "$2" "$state/notes"; shift; done; }
case "$1 ${2:-}" in
  'api repos/test/nodecode') printf '%s\n' test/nodecode ;;
  'api repos/test/nodecode/commits/'*)
    # As GitHub answers a tag it does not have: the error body on stdout, and
    # a nonzero exit.
    [ -n "${PUBLISH_TEST_TAGGED:-}" ] || { printf '%s\n' '{"message":"No commit found for SHA: v9.9.9","status":"422"}'; exit 1; }
    printf '%s\n' "$PUBLISH_TEST_TAGGED" ;;
  'release list') printf '%s\n' v9.9.8 ;;
  'release view')
    [ -f "$state/assets" ] || exit 1
    case "$*" in
      *assets*) cat "$state/assets" ;;
      *url*) printf '%s\n' "https://github.invalid/test/nodecode/releases/tag/$3" ;;
    esac ;;
  'release create') assets "$@" > "$state/assets"; notes "$@" ;;
  'release upload') assets "$@" > "$state/assets" ;;
  'release edit') notes "$@" ;;
  *) printf '%s\n' "gh: the publish path called CI: $*" >&2; exit 1 ;;
esac
EOF

cat > "$fake_bin/scp" <<'EOF'
#!/bin/sh
set -eu
[ "${1:-}" = -q ]
shift
while [ "$#" -gt 1 ]; do
  [ -f "$1" ]
  basename "$1" >> "$PUBLISH_TEST_SCP"
  shift
done
EOF

cat > "$fake_bin/ssh" <<'EOF'
#!/bin/sh
set -eu
exit 0
EOF

# The edge serves what scp would have put in /dl: the archives out of OUT by
# name, the pointer as the label. Like preview, it keeps /install.sh and sends
# install.ps1 to the apex with a 301, so a read that does not follow it gets
# the redirect's empty body.
cat > "$fake_bin/curl" <<'EOF'
#!/bin/sh
set -eu
output=''
headers=''
url=''
follow=''
while [ "$#" -gt 0 ]; do
  case "$1" in
    -o) output="$2"; shift 2 ;;
    -D) headers="$2"; shift 2 ;;
    -L) follow=1; shift ;;
    -*) shift ;;
    *) url="$1"; shift ;;
  esac
done
[ -n "$output" ]
case "$url" in
  */install.sh*)
    if [ -n "${PUBLISH_TEST_FLAKY:-}" ] && [ ! -f "$PUBLISH_TEST_FLAKY" ]; then
      : > "$PUBLISH_TEST_FLAKY"
      printf '%s\n' 'curl: (22) The requested URL returned error: 502' >&2
      exit 22
    fi
    cp "$PUBLISH_TEST_INSTALLER" "$output" ;;
  */install.ps1*)
    if [ -z "$follow" ]; then
      : > "$output"
    elif [ -n "${PUBLISH_TEST_STALE_PS1:-}" ]; then
      printf '%s\n' '# a copy the edge never let go of' > "$output"
    else
      cp "$PUBLISH_TEST_PS1" "$output"
    fi ;;
  *.sha256|*.tar.gz|*.tar.zst|*.zip|*/changes-*) cp "$PUBLISH_TEST_DL/${url##*/}" "$output" ;;
  *) printf '%s\n' "$PUBLISH_TEST_LABEL" > "$output" ;;
esac
if [ -n "$headers" ]; then
  printf '%s\n' 'HTTP/2 200' 'cf-cache-status: MISS' > "$headers"
fi
EOF

# Retries wait on nothing here, and a retry loop that never gives up fails the
# run instead of hanging it.
cat > "$fake_bin/sleep" <<'EOF'
#!/bin/sh
set -eu
n="$(cat "$PUBLISH_TEST_SLEEPS" 2>/dev/null || echo 0)"
n=$((n + 1))
printf '%s\n' "$n" > "$PUBLISH_TEST_SLEEPS"
[ "$n" -le 100 ] || { echo 'sleep: the publisher never stopped retrying' >&2; exit 1; }
EOF

cat > "$fake_bin/git" <<'EOF'
#!/bin/sh
set -eu
dir=.
if [ "${1:-}" = -C ]; then
  dir="$2"
  shift 2
fi
case "${1:-}" in
  rev-parse) printf '%s\n' "$PUBLISH_TEST_LABEL" ;;
  # The system definition at the commit declares the version the release is.
  show) printf '%s\n' '(defsystem "nodecode"' '  :version "9.9.9"' ;;
  # A push lands the clone's installers on the site's remote, as GitHub would.
  push) printf '%s\n' "$*" >> "$PUBLISH_TEST_PUSHES"
        if [ -d "$dir/web/public" ]; then cp "$dir"/web/public/install.* "$PUBLISH_TEST_SITE/web/public/"; fi ;;
  # The site repository: its origin, and a clone of its branch is the
  # sandbox's copy of it.
  remote) printf '%s\n' 'git@site.invalid:nodecode-website.git' ;;
  clone) printf '%s\n' "$*" >> "${PUBLISH_TEST_CLONES:-/dev/null}"
         for arg in "$@"; do dest="$arg"; done
         mkdir -p "$dest"; cp -R "$PUBLISH_TEST_SITE/." "$dest/" ;;
  # Two commits, the newer touching what ships; asked by path, only it.
  log) case "$*" in
         *' -- '*) printf '%s\n' aaaaaaaaa1111111111111111111111111111111 ;;
         *) printf '%s\n' 'aaaaaaaaa1111111111111111111111111111111 channels: a card -- the why' \
                           'bbbbbbbbb2222222222222222222222222222222 docs: a page' ;;
       esac ;;
  *) exit 0 ;;
esac
EOF

cat > "$fake_bin/commit-owned" <<'EOF'
#!/bin/sh
set -eu
printf '%s\n' "$*" >> "$PUBLISH_TEST_COMMITS"
EOF

chmod 0755 "$fake_bin"/*
sandbox="$root/repo"
# marketplace/ is the operator's local clone of the site; site-remote/ is its
# origin, the branch the site builds its image from.
mkdir -p "$sandbox/src/tools" "$sandbox/marketplace/web/public" "$sandbox/site-remote/web/public"
cp "$repo/src/tools/publish.sh" "$sandbox/src/tools/publish.sh"
cp "$repo/src/tools/publish-config.sh" "$sandbox/src/tools/publish-config.sh"
cp "$repo/src/tools/mirror-installers.sh" "$sandbox/src/tools/mirror-installers.sh"
printf '%s\n' '#!/bin/sh' '# reads the channel pointer' > "$sandbox/src/tools/install.sh"
printf '%s\n' '#!/bin/sh' '# an older copy of the installer' > "$sandbox/marketplace/web/public/install.sh"
printf '%s\n' '# reads the channel pointer' > "$sandbox/src/tools/install.ps1"
printf '%s\n' '# an older copy of the installer' > "$sandbox/marketplace/web/public/install.ps1"
cp "$sandbox/marketplace/web/public/install.sh" "$sandbox/marketplace/web/public/install.ps1" "$sandbox/site-remote/web/public/"

# publish OUT RUN: one publish of HEAD into the sandbox's OUT, its output and
# every file it shipped under $root/RUN-*. A relative OUT resolves under the
# sandbox repo the publisher cd's into.
publish() {
  PATH="$fake_bin:$PATH" \
  PUBLISH_TEST_LABEL=latest999 \
  PUBLISH_TEST_DL="$sandbox/$1" \
  PUBLISH_TEST_FLAKY="$root/flaky-502" \
  PUBLISH_TEST_COMMITS="$root/$2-commits" \
  PUBLISH_TEST_PUSHES="$root/$2-pushes" \
  PUBLISH_TEST_CLONES="$root/$2-clones" \
  PUBLISH_TEST_SCP="$root/$2-scp" \
  PUBLISH_TEST_SLEEPS="$root/$2-sleeps" \
  PUBLISH_TEST_SITE="$sandbox/site-remote" \
  PUBLISH_TEST_GH="$root/$2-gh" \
  PUBLISH_TEST_GH_STATE="$root/gh-state" \
  NODECODE_PUBLISH_GITHUB_REPO=test/nodecode \
  PUBLISH_TEST_INSTALLER="$sandbox/src/tools/install.sh" \
  PUBLISH_TEST_PS1="$sandbox/src/tools/install.ps1" \
  NODECODE_PUBLISH_INSTALLER_PATH="$sandbox/src/tools/install.sh" \
  NODECODE_PUBLISH_PS1_PATH="$sandbox/src/tools/install.ps1" \
  NODECODE_PUBLISH_MARKETPLACE_DIR="${PUBLISH_TEST_MARKETPLACE:-$sandbox/marketplace}" \
  NODECODE_GIT_COMMIT_HELPER="$fake_bin/commit-owned" \
  NODECODE_PUBLISH_CONFIG="$root/no-publish-config" \
  NODECODE_PUBLISH_HOST=publish-test.invalid NODECODE_PUBLISH_REMOTE_DIR=dl \
  NODECODE_PUBLISH_APP_DIR=app NODECODE_PUBLISH_COMPOSE_FILE=compose.yml \
  NODECODE_PUBLISH_ENV_FILE=.env NODECODE_PUBLISH_WEB_CONTAINER=web \
  NODECODE_PUBLISH_BASE_URL=https://test.invalid \
    bash "$sandbox/src/tools/publish.sh" HEAD "$1" > "$root/$2-output" 2> "$root/$2-errors"
}

fail() {
  echo "publish-test: $1" >&2
  exit 1
}

# --- one publish, both platforms, one label ----------------------------------
publish dist full || { cat "$root/full-errors" >&2; fail 'the publish failed'; }
for platform in x86_64-linux arm64-darwin x86_64-windows; do
  grep -q "$platform local sha256" "$root/full-output" || fail "no local proof for $platform"
  grep -q "nodecode-latest999-$platform\..* edge sha256" "$root/full-output" || fail "no edge proof for $platform"
  grep -q "stable-$platform points at latest999" "$root/full-output" \
    || fail "stable-$platform does not name latest999"
  grep -qx "stable-$platform" "$root/full-scp" || fail "stable-$platform was never shipped"
done
grep -qx 'nodecode-latest999-x86_64-windows.zip' "$root/full-scp" || fail 'the Windows zip was never shipped'
# Both containers ship and are proved at the edge; the installer asks for the
# tar.zst first and reads the zip only for a release that predates it.
grep -qx 'nodecode-latest999-x86_64-windows.tar.zst' "$root/full-scp" || fail 'the Windows tar.zst was never shipped'
grep -qx 'nodecode-latest999-x86_64-windows.tar.zst.sha256' "$root/full-scp" || fail 'the Windows tar.zst sidecar was never shipped'
grep -q 'nodecode-latest999-x86_64-windows.tar.zst edge sha256' "$root/full-output" || fail 'the Windows tar.zst was never proved at the edge'
# The Mac rides every release: its tarball and sidecar ship with the others,
# and nothing says it was skipped.
grep -qx 'nodecode-latest999-arm64-darwin.tar.gz' "$root/full-scp" || fail 'the Mac tarball was never shipped'
grep -qx 'nodecode-latest999-arm64-darwin.tar.gz.sha256' "$root/full-scp" || fail 'the Mac sidecar was never shipped'
if grep -q SKIP_MACOS "$root/full-errors"; then
  fail 'a full publish says it skipped the Mac'
fi
# No CI: no runner branch is pushed, and gh only ever touched the releases page.
# The one push is the installers' mirror, to the branch the site builds its
# image from: a copy only in the container is reverted by the next image.
grep -qx 'push -q origin HEAD:main' "$root/full-pushes" || fail 'the installers were never pushed to the site main'
! grep -vx 'push -q origin HEAD:main' "$root/full-pushes" >/dev/null || fail "the publisher pushed elsewhere: $(tr '\n' ' ' < "$root/full-pushes")"
# The site's own clone is never written: the mirror is made in a scratch one.
grep -qx '# an older copy of the installer' "$sandbox/marketplace/web/public/install.ps1" || fail 'publish wrote into the local site clone'
# The pointers move after every archive: the last archive shipped comes before
# the first pointer.
last_archive="$(grep -n '^nodecode-' "$root/full-scp" | tail -n 1 | cut -d: -f1)"
first_pointer="$(grep -n '^stable-' "$root/full-scp" | head -n 1 | cut -d: -f1)"
[ "$last_archive" -lt "$first_pointer" ] || fail 'a pointer shipped before an archive it names'
# The release's changes ship before any pointer names it: the head of each
# commit that touched what ships, the label alone of one that did not.
grep -q 'changes-latest999 edge sha256' "$root/full-output" || fail 'no edge proof for the changes list'
changes_line="$(grep -n '^changes-latest999$' "$root/full-scp" | head -n 1 | cut -d: -f1)"
[ -n "$changes_line" ] && [ "$changes_line" -lt "$first_pointer" ] || fail 'the changes list did not ship before the pointers'
[ "$(cat "$sandbox/dist/changes-latest999")" = "$(printf '%s\n' 'aaaaaaaaa channels: a card' bbbbbbbbb)" ] \
  || fail "the changes list reads: $(cat "$sandbox/dist/changes-latest999")"
grep -q 'install.sh is live at https://test.invalid/install.sh, reading the stable pointer' "$root/full-output"
grep -q 'install.ps1 is live at https://test.invalid/install.ps1' "$root/full-output"
# The installer is a mirror of authority, not a carrier of a pin: the only
# commit a publish makes to it is the mirror itself.
grep -q 'release: public installer follows authority' "$root/full-commits"
grep -q -- '--path web/public/install.sh' "$root/full-commits"
grep -q -- '--path web/public/install.ps1' "$root/full-commits"
if grep -q -- '--path src/tools/install.sh' "$root/full-commits"; then
  fail 'publish still commits to the authority installer'
fi
[ -f "$root/flaky-502" ] || fail 'the 502 retry path was not exercised'
printf '%s\n' 'publish-test: one label on every platform, pointers last, 502 retry, edge hash verification pass'

# The release is a version on GitHub's releases page: the asd's base, tagged at
# the commit, made after every pointer and installer is proved, with the notes
# since the release before and all eight archives and sidecars.
grep -q 'latest999 is nodecode 9.9.9, tagged v9.9.9' "$root/full-output" || fail 'the publish did not name its version'
grep -q '^release create v9.9.9 -R test/nodecode --target latest999 --title nodecode 9.9.9 ' "$root/full-gh" \
  || fail "no release v9.9.9 at the commit: $(tr '\n' ' ' < "$root/full-gh")"
[ "$(cat "$root/gh-state/assets")" = 8 ] || fail "the release carries $(cat "$root/gh-state/assets") assets, not 8"
grep -q 'v9.9.9 is on the releases page with 8 assets' "$root/full-output" || fail 'the release page was not read back'
! grep -vE '^(api|release) ' "$root/full-gh" > /dev/null || fail "gh was asked for more than the releases page: $(tr '\n' ' ' < "$root/full-gh")"
last_pointer_said="$(grep -n 'install.ps1 is live at' "$root/full-output" | cut -d: -f1)"
release_said="$(grep -n 'is on the releases page' "$root/full-output" | cut -d: -f1)"
[ "$last_pointer_said" -lt "$release_said" ] || fail 'the release page was made before the installers were proved'
grep -qx '## Changes since v9.9.8' "$root/gh-state/notes" || fail "the notes do not start from the release before: $(cat "$root/gh-state/notes")"
grep -qx -- '- channels: a card (aaaaaaaaa)' "$root/gh-state/notes" || fail 'the notes do not carry the commit heads'
grep -q 'prints 9.9.9+latest999' "$root/gh-state/notes" || fail 'the notes do not name the version the build prints'
printf '%s\n' 'publish-test: the release is v9.9.9 on the releases page, after the pointers, with its notes and every archive'

# --- an archive already built is the release ----------------------------------
# A second publish of the same commit ships what OUT holds: no arm packs again
# and no sandbox boots, so no second body goes out under a name the edge
# cached.
PUBLISH_TEST_NO_BUILD=1 publish dist again || { cat "$root/again-errors" >&2; fail 'the republish failed'; }
[ ! -s "$root/again-pushes" ] || fail 'the republish pushed the installers again, though the site already carried them'
grep -q 'stable-x86_64-windows points at latest999' "$root/again-output" || fail 'the republish moved no pointer'
grep -q '^release upload v9.9.9 -R test/nodecode --clobber ' "$root/again-gh" || fail 'the republish did not update its own release'
! grep -q '^release create' "$root/again-gh" || fail 'the republish made a second release'
printf '%s\n' 'publish-test: a republish ships the archives already built and updates its own release'

# --- a publisher with no clone of the site names it ---------------------------
# The Release workflow holds no clone of the site: NODECODE_PUBLISH_SITE_REMOTE
# names the repository the installers are mirrored to, and wins over a clone.
PUBLISH_TEST_NO_BUILD=1 PUBLISH_TEST_MARKETPLACE="$root/no-site-clone" \
  NODECODE_PUBLISH_SITE_REMOTE=git@site-env.invalid:site.git publish dist siteenv \
  || { cat "$root/siteenv-errors" >&2; fail 'a publish naming the site repository failed'; }
grep -q 'git@site-env.invalid:site.git' "$root/siteenv-clones" \
  || fail "the installers' mirror did not clone the named site: $(cat "$root/siteenv-clones" 2>/dev/null)"
printf '%s\n' 'publish-test: NODECODE_PUBLISH_SITE_REMOTE stands in for a clone of the site'

# --- a version already released elsewhere moves nothing ----------------------
# A version is released once: when its tag names another commit, the publish
# stops before any arm packs, and says to bump the asd.
if PUBLISH_TEST_TAGGED=ccccccccc3333333333333333333333333333333 publish fresh-tagged tagged; then
  fail 'a version tagged at another commit still published'
fi
grep -q 'v9.9.9 is already released at ccccccccc; bump :version in src/nodecode.asd' "$root/tagged-errors" \
  || fail "the released version was not named: $(cat "$root/tagged-errors")"
[ ! -s "$root/tagged-scp" ] || fail "a released version still shipped: $(tr '\n' ' ' < "$root/tagged-scp")"
[ ! -e "$sandbox/fresh-tagged/nodecode-latest999-x86_64-linux.tar.gz" ] || fail 'an arm packed a version already released'
printf '%s\n' 'publish-test: a version already released at another commit packs nothing'

# --- an installer the edge never converges on gives up ------------------------
# The installer's retries are counted apart from each read's own: a shared
# counter was reset by every read, and the publish slept forever.
if PUBLISH_TEST_NO_BUILD=1 PUBLISH_TEST_STALE_PS1=1 publish dist stale; then
  fail 'a stale install.ps1 still published'
fi
grep -q 'the edge still serves an older install.ps1 after 200s' "$root/stale-errors" \
  || fail "the stale installer was not named: $(cat "$root/stale-errors")"
printf '%s\n' 'publish-test: an installer the edge never serves gives up after its cap'

# --- an arm that fails moves nothing ------------------------------------------
# The tarball is packed first and the sandbox zip second: a commit the sandbox
# cannot build fails the publish before either archive leaves OUT.
if PUBLISH_TEST_WINDOWS_FAIL=1 publish fresh failed; then
  fail 'a failed Windows arm still published'
fi
grep -q 'the Windows arm did not pack latest999' "$root/failed-errors" \
  || fail 'the failed arm was not named'
[ ! -s "$root/failed-scp" ] || fail "a failed arm still shipped: $(tr '\n' ' ' < "$root/failed-scp")"
printf '%s\n' 'publish-test: an arm that fails ships no archive and moves no pointer'

# --- a Mac that does not answer moves nothing --------------------------------
# The Mac arm runs before the sandbox's: a Mac asleep or away fails the
# publish by name, and no archive leaves OUT, no pointer moves.
if PUBLISH_TEST_MACOS_FAIL=1 publish fresh-mac macfail; then
  fail 'a failed macOS arm still published'
fi
grep -q 'the macOS arm did not pack latest999' "$root/macfail-errors" \
  || fail "the failed macOS arm was not named: $(cat "$root/macfail-errors")"
grep -q 'NODECODE_PUBLISH_SKIP_MACOS=1 publishes without it' "$root/macfail-errors" \
  || fail 'the failed macOS arm does not say how to publish without it'
[ ! -s "$root/macfail-scp" ] || fail "a failed macOS arm still shipped: $(tr '\n' ' ' < "$root/macfail-scp")"
[ ! -e "$sandbox/fresh-mac/nodecode-latest999-x86_64-windows.zip" ] \
  || fail 'the sandbox was booted after the Mac had already failed'
printf '%s\n' 'publish-test: a macOS arm that fails ships no archive and moves no pointer'

# --- leaving the Mac out is said ---------------------------------------------
# NODECODE_PUBLISH_SKIP_MACOS is the one way to publish without the Mac: the
# publish says so, packs no Mac arm, and ships no darwin archive or pointer.
NODECODE_PUBLISH_SKIP_MACOS=1 PUBLISH_TEST_MACOS_FAIL=1 publish fresh-skip skip \
  || { cat "$root/skip-errors" >&2; fail 'the publish without the Mac failed'; }
grep -q 'NODECODE_PUBLISH_SKIP_MACOS is set: no macOS arm' "$root/skip-errors" \
  || fail 'the skipped Mac was not said'
if grep -q darwin "$root/skip-scp"; then
  fail "a publish without the Mac shipped: $(grep darwin "$root/skip-scp" | tr '\n' ' ')"
fi
for platform in x86_64-linux x86_64-windows; do
  grep -q "stable-$platform points at latest999" "$root/skip-output" \
    || fail "stable-$platform does not name latest999 without the Mac"
done
printf '%s\n' 'publish-test: leaving the Mac out is said, and ships no darwin archive or pointer'

# --- the Windows arm runs on the build host -----------------------------------
# A Windows guest is a whole VM: pack-windows.sh starts it on the host that
# NODECODE_WINBOX_HOST names and never on the publishing machine, so without
# that setting it stops before any podman, and with it every container call
# goes over ssh to that host.
pw_bin="$root/pw-bin"
mkdir -p "$pw_bin" "$root/pw-out"
: > "$root/pw-empty.env"
cat > "$pw_bin/podman" <<'EOF'
#!/bin/sh
printf '%s\n' "$*" >> "$PW_TEST_PODMAN"
exit 1
EOF
cat > "$pw_bin/ssh" <<'EOF'
#!/bin/sh
while [ "$#" -gt 0 ]; do
  case "$1" in -o|-i|-p) shift 2 ;; -*) shift ;; *) break ;; esac
done
printf '%s | %s\n' "$1" "$*" >> "$PW_TEST_SSH"
exit 1
EOF
chmod +x "$pw_bin/podman" "$pw_bin/ssh"
pack_windows() {
  env -u NODECODE_WINBOX_HOST PATH="$pw_bin:$PATH" NODECODE_PUBLISH_CONFIG="$root/pw-empty.env" \
    PW_TEST_PODMAN="$root/pw-podman" PW_TEST_SSH="$root/pw-ssh" "$@" \
    bash "$repo/src/tools/pack-windows.sh" HEAD "$root/pw-out" 2> "$root/pw-errors"
}
: > "$root/pw-podman"; : > "$root/pw-ssh"
if pack_windows; then fail 'pack-windows ran without NODECODE_WINBOX_HOST'; fi
grep -q 'NODECODE_WINBOX_HOST is not set' "$root/pw-errors" \
  || fail "the missing build host was not named: $(cat "$root/pw-errors")"
[ ! -s "$root/pw-podman" ] || fail "pack-windows ran podman here: $(cat "$root/pw-podman")"
[ ! -s "$root/pw-ssh" ] || fail "pack-windows reached a host it was never given: $(cat "$root/pw-ssh")"
: > "$root/pw-podman"; : > "$root/pw-ssh"
if pack_windows NODECODE_WINBOX_HOST=buildbox; then fail 'pack-windows ran against a host with no rig'; fi
grep -q 'buildbox holds no rig at ~/nodecode-winbox' "$root/pw-errors" \
  || fail "the rigless build host was not named: $(cat "$root/pw-errors")"
grep -q '^buildbox | .*podman' "$root/pw-ssh" \
  || fail "the rig check did not ask buildbox: $(cat "$root/pw-ssh")"
[ ! -s "$root/pw-podman" ] || fail "pack-windows ran podman here: $(cat "$root/pw-podman")"
[ ! -e "$root/pw-out/nodecode-$(git rev-parse --short=9 HEAD)-x86_64-windows.zip" ] \
  || fail 'a rigless build host still left a zip'
printf '%s\n' 'publish-test: the Windows arm needs its build host and never runs podman here'
