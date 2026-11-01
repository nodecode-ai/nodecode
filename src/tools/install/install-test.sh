#!/bin/sh
# install-test.sh --- focused regression for the active-unit installer refresh.
#
# The release download and executable are fake, but the unit discovery and
# environment handoff are real. This keeps the test independent of a running
# user manager and of the release host.
set -eu

root="$(mktemp -d)"
trap 'rm -rf "$root"' EXIT INT TERM

release="$root/release"
mkdir -p "$release/nodecode-test-x86_64-linux"
printf '%s\n' '#!/bin/sh' 'case "${1:-}" in' '  --help) printf "%s\\n" "usage: nodecode" ;;' '  gateway) [ "${2:-}" = restart ] || exit 1; printf "NODECODE_HOME=%s\\nXDG_CONFIG_HOME=%s\\n" "${NODECODE_HOME:-}" "${XDG_CONFIG_HOME:-}" > "$NODECODE_INSTALL_TEST_TRACE" ;;' '  *) exit 1 ;;' 'esac' > "$release/nodecode-test-x86_64-linux/nodecode"
chmod +x "$release/nodecode-test-x86_64-linux/nodecode"
tar -czf "$release/nodecode-test-x86_64-linux.tar.gz" -C "$release" nodecode-test-x86_64-linux
sha256sum "$release/nodecode-test-x86_64-linux.tar.gz" > "$release/nodecode-test-x86_64-linux.tar.gz.sha256"

fake_bin="$root/bin"
mkdir -p "$fake_bin"
cat > "$fake_bin/systemctl" <<'EOF'
#!/bin/sh
if [ "${2:-}" = is-active ] && [ "${3:-}" = --quiet ] &&
   [ "${4:-}" = nodecode-gateway.service ]; then
  exit 0
fi
exit 1
EOF
chmod +x "$fake_bin/systemctl"

home="$root/home"
mkdir -p "$home/.config/systemd/user" "$home/.local/bin"
printf '%s\n' \
  "# nodecode gateway: $root/organism/" \
  '[Unit]' \
  '[Service]' \
  'Type=simple' > "$home/.config/systemd/user/nodecode-gateway.service"

PATH="$fake_bin:$PATH" \
HOME="$home" \
NODECODE_HOME="$root/install" \
NODECODE_VERSION=test \
NODECODE_BASE_URL="file://$release" \
NODECODE_INSTALL_TEST_TRACE="$root/trace" \
  sh "$(dirname "$0")/install.sh" --no-modify-path > "$root/output"

grep -q 'refreshing active gateway nodecode-gateway.service' "$root/output"
grep -q 'refreshed and restarted nodecode-gateway.service on test' "$root/output"
grep -q "NODECODE_HOME=$root/organism" "$root/trace"
grep -q "XDG_CONFIG_HOME=$home/.config" "$root/trace"
printf '%s\n' 'install-test: active gateway refresh carries the systemd config root'

# --- the channel pointer names the release ----------------------------------
# No release is written into the installer any more: it reads
# <base>/<channel>-<arch>-<os>, the same file a running organism's own update
# check reads. A Mac could not upgrade itself while that pointer was never
# written, so its absence is a named failure here, not a silent one.
release="$root/pointer"; mkdir -p "$release"
cp -r "$root/release/nodecode-test-x86_64-linux" "$release/nodecode-abcdef1-x86_64-linux"
tar -czf "$release/nodecode-abcdef1-x86_64-linux.tar.gz" -C "$release" nodecode-abcdef1-x86_64-linux
sha256sum "$release/nodecode-abcdef1-x86_64-linux.tar.gz" > "$release/nodecode-abcdef1-x86_64-linux.tar.gz.sha256"
printf '%s\n' 'abcdef1' > "$release/stable-x86_64-linux"

PATH="$fake_bin:$PATH" \
HOME="$home" \
NODECODE_HOME="$root/install-pointer" \
NODECODE_BASE_URL="file://$release" \
NODECODE_INSTALL_TEST_TRACE="$root/trace-pointer" \
  sh "$(dirname "$0")/install.sh" --no-modify-path > "$root/output-pointer"

grep -q "reading file://$release/stable-x86_64-linux" "$root/output-pointer"
grep -q 'release abcdef1' "$root/output-pointer"
grep -q 'current is abcdef1' "$root/output-pointer"
grep -q 'sha256 verified' "$root/output-pointer"

# A channel with no pointer published for this platform: the install stops and
# names the URL it asked, instead of fetching a name that is not there.
if PATH="$fake_bin:$PATH" \
   HOME="$home" \
   NODECODE_CHANNEL=beta \
   NODECODE_HOME="$root/install-missing" \
   NODECODE_BASE_URL="file://$release" \
   NODECODE_INSTALL_TEST_TRACE="$root/trace-missing" \
     sh "$(dirname "$0")/install.sh" --no-modify-path > "$root/output-missing" 2>&1; then
  printf '%s\n' 'install-test: an unwritten pointer did not stop the install' >&2
  exit 1
fi
grep -q 'no beta release is published for x86_64-linux yet' "$root/output-missing"
printf '%s\n' 'install-test: the channel pointer names the release, and its absence is named'
# --- installs into one home take turns ---------------------------------------
# Several installs at once into one home (a second curl | sh, or an update
# beside a manual install) each lay out and probe the same folder; without a
# turn, one removed the folder while another probed it and both failed. The
# probe here is slow so the overlap is certain.
release="$root/slow"; mkdir -p "$release/nodecode-slow-x86_64-linux"
printf '%s\n' '#!/bin/sh' 'case "${1:-}" in' '  --help) sleep 0.3; printf "%s\\n" "usage: nodecode" ;;' '  *) exit 1 ;;' 'esac' > "$release/nodecode-slow-x86_64-linux/nodecode"
chmod +x "$release/nodecode-slow-x86_64-linux/nodecode"
tar -czf "$release/nodecode-slow-x86_64-linux.tar.gz" -C "$release" nodecode-slow-x86_64-linux
sha256sum "$release/nodecode-slow-x86_64-linux.tar.gz" > "$release/nodecode-slow-x86_64-linux.tar.gz.sha256"
bare="$root/bare"; mkdir -p "$bare"
slow() {
  PATH="$fake_bin:$PATH" HOME="$bare" NODECODE_HOME="$root/install-slow" NODECODE_VERSION=slow \
  NODECODE_BASE_URL="file://$release" sh "$(dirname "$0")/install.sh" --no-modify-path > "$root/output-slow-$1" 2>&1
}
pids=''
for n in 1 2 3 4; do slow "$n" & pids="$pids $!"; done
failed=0
for pid in $pids; do wait "$pid" || failed=$((failed + 1)); done
if [ "$failed" -ne 0 ]; then
  printf 'install-test: %s of 4 concurrent installs failed\n' "$failed" >&2
  cat "$root"/output-slow-* >&2
  exit 1
fi
[ ! -e "$root/install-slow/.install-lock" ] || { printf '%s\n' 'install-test: the install lock was left behind' >&2; exit 1; }
"$root/install-slow/current/nodecode" --help | grep -q 'usage: nodecode'

# A lock whose owner died is taken over rather than waited on.
mkdir "$root/install-slow/.install-lock"
sh -c 'exit 0' & dead=$!; wait "$dead"
printf '%s\n' "$dead" > "$root/install-slow/.install-lock/pid"
slow stale || { printf '%s\n' 'install-test: a dead owner held the install lock' >&2; cat "$root/output-slow-stale" >&2; exit 1; }
printf '%s\n' 'install-test: installs into one home take turns, and a dead owner releases its turn'

# --- a release already laid out is kept ---------------------------------------
# A label names one build: installing it again re-downloaded the whole tarball
# and replaced the folder (a running organism's own files among them). A copy
# that answers beside its src/ is kept and only the links move; a broken one is
# laid out again; --force or NODECODE_FORCE=1 downloads it regardless.
release="$root/kept"; mkdir -p "$release/nodecode-kept-x86_64-linux/src"
printf '%s\n' '#!/bin/sh' 'case "${1:-}" in' '  --help) sleep 0.2; printf "%s\\n" "usage: nodecode" ;;' '  *) exit 1 ;;' 'esac' > "$release/nodecode-kept-x86_64-linux/nodecode"
chmod +x "$release/nodecode-kept-x86_64-linux/nodecode"
tar -czf "$release/nodecode-kept-x86_64-linux.tar.gz" -C "$release" nodecode-kept-x86_64-linux
sha256sum "$release/nodecode-kept-x86_64-linux.tar.gz" > "$release/nodecode-kept-x86_64-linux.tar.gz.sha256"
kept() {
  out="$root/output-kept-$1"; shift
  PATH="$fake_bin:$PATH" HOME="$bare" NODECODE_HOME="$root/install-kept" NODECODE_VERSION=kept \
  NODECODE_BASE_URL="file://$release" sh "$(dirname "$0")/install.sh" --no-modify-path "$@" > "$out" 2>&1
}
fail() { printf 'install-test: %s\n' "$1" >&2; cat "$root"/output-kept-* >&2; exit 1; }

# Four at once into an empty home: one lays the release out, the rest keep it.
pids=''
for n in 1 2 3 4; do kept "race$n" & pids="$pids $!"; done
failed=0
for pid in $pids; do wait "$pid" || failed=$((failed + 1)); done
[ "$failed" -eq 0 ] || fail "$failed of 4 concurrent installs of one release failed"
laid="$(cat "$root"/output-kept-race* | grep -c 'laid out kept at' || true)"
[ "$laid" -eq 1 ] || fail "4 concurrent installs laid one release out $laid times"

# Again, with the tarball gone: nothing is downloaded, the folder is not replaced.
printf '%s\n' witness > "$root/install-kept/kept/src/witness"
mv "$release/nodecode-kept-x86_64-linux.tar.gz" "$release/away.tar.gz"
kept again || fail 'a reinstall of the laid-out release reached for the download'
grep -q 'kept is already laid out at' "$root/output-kept-again" || fail 'the reinstall did not say it kept the release'
grep -q 'downloading' "$root/output-kept-again" && fail 'the reinstall downloaded the release again'
[ -f "$root/install-kept/kept/src/witness" ] || fail 'the reinstall replaced the laid-out folder'
"$root/install-kept/current/nodecode" --help | grep -q 'usage: nodecode'
mv "$release/away.tar.gz" "$release/nodecode-kept-x86_64-linux.tar.gz"

# --force downloads it again, as NODECODE_FORCE=1 does.
kept force --force || fail '--force did not install'
grep -q 'downloading' "$root/output-kept-force" || fail '--force did not download'
[ ! -f "$root/install-kept/kept/src/witness" ] || fail '--force kept the old folder'
printf '%s\n' witness > "$root/install-kept/kept/src/witness"
NODECODE_FORCE=1 kept forceenv || fail 'NODECODE_FORCE=1 did not install'
grep -q 'downloading' "$root/output-kept-forceenv" || fail 'NODECODE_FORCE=1 did not download'

# A laid-out copy that does not answer is laid out again.
printf '%s\n' '#!/bin/sh' 'exit 1' > "$root/install-kept/kept/nodecode"
kept broken || fail 'a broken laid-out copy stopped the reinstall'
grep -q 'laid out kept at' "$root/output-kept-broken" || fail 'a broken laid-out copy was kept'
"$root/install-kept/current/nodecode" --help | grep -q 'usage: nodecode'
[ ! -e "$root/install-kept/.install-lock" ] || fail 'the install lock was left behind'
printf '%s\n' 'install-test: a release already laid out is kept, a broken one replaced, --force downloads'

# --- the glibc floor ---------------------------------------------------------
# pack.sh refuses a Linux build importing anything above GLIBC_2.34, so 22.04's
# 2.35 installs and 20.04's 2.31 is refused before the download, naming 2.34.
floor() { # NAME LDD-LINE
  mkdir -p "$root/ldd-$1"
  printf '#!/bin/sh\nprintf "%%s\\n" "%s"\n' "$2" > "$root/ldd-$1/ldd"
  chmod +x "$root/ldd-$1/ldd"
  PATH="$root/ldd-$1:$PATH" HOME="$home" NODECODE_HOME="$root/install-glibc-$1" NODECODE_VERSION=test \
  NODECODE_BASE_URL="file://$root/release" NODECODE_INSTALL_TEST_TRACE="$root/trace-glibc" \
    sh "$(dirname "$0")/install.sh" --no-modify-path > "$root/output-glibc-$1" 2>&1
}
floor jammy 'ldd (Ubuntu GLIBC 2.35-0ubuntu3.15) 2.35' || fail 'glibc 2.35 (Ubuntu 22.04) was refused'
grep -q 'sha256 verified' "$root/output-glibc-jammy" || fail 'glibc 2.35 did not reach the download'
if floor focal 'ldd (Ubuntu GLIBC 2.31-0ubuntu9.16) 2.31'; then fail 'glibc 2.31 (Ubuntu 20.04) installed'; fi
grep -q 'glibc 2.31 is too old .* needs 2.34 or newer' "$root/output-glibc-focal" || fail 'the refusal did not name 2.34'
grep -q 'downloading' "$root/output-glibc-focal" && fail 'glibc 2.31 downloaded before the refusal'
printf '%s\n' 'install-test: glibc 2.35 installs, 2.31 is refused before the download'

# --- a release is installed only as its host vouches for it ------------------
# The sha256 sidecar was optional: a host that served none had its tarball
# installed after a WARNING, so whoever could serve the tarball needed no
# sidecar to go with it. Its absence now stops the install before anything is
# laid out. Plain http is refused before anything is read, and a label that is
# not one -- `..' made the reinstall's folder the install root's parent --
# never reaches the download.
release="$root/vouch"; mkdir -p "$release"
cp -r "$root/release/nodecode-test-x86_64-linux" "$release/nodecode-bare-x86_64-linux"
tar -czf "$release/nodecode-bare-x86_64-linux.tar.gz" -C "$release" nodecode-bare-x86_64-linux
vouch() { # NAME VERSION BASE
  PATH="$fake_bin:$PATH" HOME="$bare" NODECODE_HOME="$root/install-vouch" NODECODE_VERSION="$2" \
  NODECODE_BASE_URL="$3" sh "$(dirname "$0")/install.sh" --no-modify-path > "$root/output-vouch-$1" 2>&1
}
vfail() { printf 'install-test: %s\n' "$1" >&2; cat "$root"/output-vouch-* >&2; exit 1; }
if vouch nosum bare "file://$release"; then vfail 'a release with no sha256 sidecar installed'; fi
grep -q 'no sha256 is published for' "$root/output-vouch-nosum" || vfail 'the refusal did not name the missing sha256'
[ ! -e "$root/install-vouch/bare" ] || vfail 'a release with no sha256 sidecar was laid out'
if vouch http test "http://127.0.0.1:9/dl"; then vfail 'a plain http release host was taken'; fi
grep -q 'NODECODE_BASE_URL is https:// or file://' "$root/output-vouch-http" || vfail 'the http refusal did not name the rule'
grep -q 'downloading' "$root/output-vouch-http" && vfail 'a plain http release host was read'
for label in .. . ../up a/b; do
  if vouch label "$label" "file://$root/release"; then vfail "the label $label was installed"; fi
  grep -q "NODECODE_VERSION is not a release label: $label" "$root/output-vouch-label" || vfail "the label $label was not refused as one"
  grep -q 'downloading' "$root/output-vouch-label" && vfail "the label $label reached the download"
done
printf '%s\n' 'install-test: no sha256 sidecar, a plain http host or a label like .. is refused before anything is laid out'
