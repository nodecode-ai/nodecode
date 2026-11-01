#!/usr/bin/env bash
# pack-macos.sh --- the macOS arm of the release: built, smoked and packed on
# a Mac over ssh.
#
# SPDX-License-Identifier: MIT
#
# Run through `just macos'. SBCL cannot cross-compile and the release waits on
# no CI lane, so the Apple Silicon tarball is packed the way pack.sh packs the
# Linux one -- `just tarball' of the committed revision, the shipped cells
# pulled in from src/cells/bundle.json, named nodecode-<label>-arm64-darwin --
# on a Mac this box reaches over ssh (NODECODE_MACOS_HOST, a publish setting:
# publish-config.sh). The Mac half is src/tools/release/mac-build.sh, sent with the
# revision -- the Mac CI job runs the same script (ci-macos.sh); it is what
# the retired macOS lane ran
# (533bb5ad6^:.github/workflows/macos.yml): the patched SBCL and its tests, the
# stack scrub, quicklisp on the pinned dist, the tarball, the trial seams, a
# headless serve and the release feed proof. Not the lane's launchd step:
# the Mac is a person's machine, and its LaunchAgents are theirs.
#
# Everything it writes on the Mac is under one directory that names no account
# (NODECODE_MACOS_BUILD_DIR, /Users/Shared/nodecode-build by default: SBCL
# writes every build path into the release), with HOME pointed into it for
# every step: the toolchain (just, the bootstrap SBCL taken from
# Homebrew's bottle without installing it, the patched SBCL), quicklisp and
# the compile caches stay there between releases, and the build tree of each
# run is removed once the tarball is home. It installs nothing: Homebrew's zstd
# and openssl@3 must already be there (the runtime links one, the image and the
# feed proof read the other), and a Mac without them is refused by name.
#
# This driver then proves what came back is the revision it sent: every file of
# src/src and of the shipped cells, the system definition and the installer
# byte-equal inside the tarball, and the sidecar the hash of the bytes. It
# brings the tarball home as soon as the Mac calls it final, while the Mac's
# own proofs still run, and holds it to the hash the Mac ends on.
set -euo pipefail

REV="${1:-HEAD}"
OUT="${2:-dist}"

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$repo"

. "$repo/src/tools/release/publish-config.sh"
mac="$(publish_setting NODECODE_MACOS_HOST)" || exit 1
# The Mac builds under a root that names nobody, never under the account's
# home: SBCL writes every path it compiles or loads into the image and the
# fasls, and pack.sh's gate refuses a tree that carries the account's home.
mroot="${NODECODE_MACOS_BUILD_DIR:-/Users/Shared/nodecode-build}"
case "$mroot" in
  /Users/Shared/?*|/opt/?*) ;;
  *) echo "pack-macos: NODECODE_MACOS_BUILD_DIR is an absolute path under /Users/Shared or /opt, not '$mroot'" >&2; exit 1 ;;
esac
command -v python3 >/dev/null 2>&1 || { echo 'pack-macos: python3 is needed to read the tarball back against the tree' >&2; exit 1; }

commit="$(git rev-parse "$REV^{commit}")"
# NINE hex digits, explicitly (pack.sh's note): a label is what a pointer names.
label="$(git rev-parse --short=9 "$commit")"
name="nodecode-$label-arm64-darwin"
mkdir -p "$OUT"
OUT="$(cd "$OUT" && pwd)"
archive="$OUT/$name.tar.gz"
log="$OUT/macos-$label.log"

ssh_mac() {
  ssh -o BatchMode=yes -o ConnectTimeout=15 -o ServerAliveInterval=30 -o ServerAliveCountMax=10 "$mac" "$@"
}

# A Mac that is asleep, away or another kind of machine fails here, by name,
# before anything is sent.
facts="$(ssh_mac 'uname -sm' 2>&1)" || {
  printf 'pack-macos: %s does not answer ssh: %s\n' "$mac" "$facts" >&2
  exit 1
}
[ "$facts" = 'Darwin arm64' ] || {
  printf 'pack-macos: %s is %s, not an Apple Silicon Mac\n' "$mac" "$facts" >&2
  exit 1
}

work="$(mktemp -d)"
locked=''
fetch_pid=''

# One build at a time on the Mac: the run directory is one per account. The
# lock names the driver that holds it, and that driver frees it on its way out.
# A driver can end without a way out: a cancelled CI job's is killed with its
# whole tree, so no trap of its own runs and its lock stays behind. A lock is
# taken over, its holder named in the log, once that holder is provably gone:
# the lock is from before the Mac last booted, or the driver it names ran on
# this machine and runs no more. A driver that ran elsewhere, or may still run,
# keeps it. Taking it over first stops what the holder's run left on the Mac
# (mac-build.sh names its process group in the lock).
owner="$(hostname) pid $$ $label $(date -u +%FT%TZ)"

# driver_id PID: who a driver is, so a later one can tell whether it still
# runs -- this machine, its boot, its process namespace, the pid and the pid's
# start in clock ticks after boot. Nothing where /proc cannot say.
driver_id() {
  local start
  start="$(sed -E 's/^.*\) //' "/proc/$1/stat" 2>/dev/null | cut -d' ' -f20)" || return 1
  [ -n "$start" ] || return 1
  printf '%s %s %s %s %s\n' "$(cat /etc/machine-id)" "$(cat /proc/sys/kernel/random/boot_id)" \
    "$(readlink /proc/self/ns/pid)" "$1" "$start"
}
me="$(driver_id $$ 2>/dev/null || true)"

# driver_gone RECORD: the driver a lock names ran on this machine and runs no
# more -- the machine booted since, or its pid is gone or another process's.
# A record from another machine or process namespace, or none, proves nothing.
driver_gone() {
  local machine boot ns pid start
  read -r machine boot ns pid start <<<"$1"
  [ -n "$start" ] && [ "$machine" = "$(cat /etc/machine-id 2>/dev/null)" ] || return 1
  [ "$boot" = "$(cat /proc/sys/kernel/random/boot_id)" ] || return 0
  [ "$ns" = "$(readlink /proc/self/ns/pid)" ] || return 1
  [ "$(driver_id "$pid" 2>/dev/null)" != "$1" ]
}

# mac_lock take [STALE] | free: the lock's Mac half, one ssh round trip.
# `take' takes the lock -- over from its holder when the lock is from before the
# Mac's last boot, or still names STALE, a holder this driver found gone -- or
# prints `held', the holder and its driver record, and exits 3. `free' frees
# the lock while it is still this driver's, stopping what its run left.
mac_lock() {
  ssh_mac "/bin/bash -s $1 \"$mroot\" '$owner' '$me' '${2:-}'" <<'LOCK'
set -eu
verb="$1" root="$2" owner="$3" me="$4" stale="$5"
lock="$root/.lock"
started() { ps -o lstart= -p "$1" 2>/dev/null | tr -s ' ' | sed 's/^ //; s/ $//'; }
# The run the holder started here, as mac-build.sh named it in the lock: its
# process group, stopped -- TERM, then KILL after ten seconds -- while the
# group's leader is still that process.
stop_run() {
  local pg start
  [ -f "$lock/build" ] && read -r pg start < "$lock/build" || return 0
  [ -n "$pg" ] && [ "$(started "$pg")" = "$start" ] || return 0
  kill -TERM -- "-$pg" 2>/dev/null || return 0
  for _ in 1 2 3 4 5 6 7 8 9 10; do
    kill -0 -- "-$pg" 2>/dev/null || break
    sleep 1
  done
  kill -KILL -- "-$pg" 2>/dev/null || true
  echo "pack-macos: stopped the run left on the Mac (process group $pg)" >&2
}
case "$verb" in
  take)
    mkdir -p "$root"
    if [ -d "$lock" ]; then
      held="$(cat "$lock/owner" 2>/dev/null || echo 'no owner')"
      boot="$(/usr/sbin/sysctl -n kern.boottime | sed -E 's/^[^0-9]*([0-9]+).*/\1/')"
      if [ "$(/usr/bin/stat -f %m "$lock")" -lt "$boot" ]; then
        why="is from before the Mac's last boot"
      elif [ -n "$stale" ] && [ "$held" = "$stale" ]; then
        why='names a driver that runs no more'
      else
        printf 'held\t%s\t%s\n' "$held" "$(cat "$lock/driver" 2>/dev/null || true)"
        exit 3
      fi
      stop_run
      echo "pack-macos: $lock $why; taken over from $held" >&2
      rm -rf "$lock"
    fi
    mkdir "$lock"
    printf '%s\n' "$owner" > "$lock/owner"
    printf '%s\n' "$me" > "$lock/driver"
    ;;
  free)
    [ "$(cat "$lock/owner" 2>/dev/null || true)" = "$owner" ] || exit 0
    stop_run
    rm -rf "$lock"
    ;;
esac
LOCK
}

cleanup() {
  if [ -n "$fetch_pid" ]; then kill "$fetch_pid" 2>/dev/null || true; fi
  if [ -n "$locked" ]; then mac_lock free >/dev/null 2>&1 || true; fi
  rm -rf "$work"
}
# An interrupt ends the driver, and its way out is the EXIT trap, once: a
# cleanup that ran on the signal and then carried on would build on with no
# lock and no work directory.
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

stale=''
while :; do
  rc=0
  answer="$(mac_lock take "$stale")" || rc=$?
  [ "$rc" != 0 ] || break
  [ "$rc" = 3 ] || { printf 'pack-macos: taking %s/.lock on %s failed (rc %s)\n' "$mroot" "$mac" "$rc" >&2; exit 1; }
  held="$(printf '%s\n' "$answer" | cut -f2)"
  if [ "$held" != "$stale" ] && driver_gone "$(printf '%s\n' "$answer" | cut -f3)"; then
    stale="$held"
    continue
  fi
  printf 'pack-macos: another build holds %s/.lock on %s (%s); remove it when none runs\n' "$mroot" "$mac" "$held" >&2
  exit 1
done
locked=1

# What the Mac builds: a one-commit repository of the revision (pack.sh asks
# git for the label and archives REV), and the script that drives it.
ship="$work/ship"
mkdir -p "$ship"
git init -q "$ship/repo"
git -C "$ship/repo" fetch -q --depth 1 "file://$repo" "$commit"
git -C "$ship/repo" -c advice.detachedHead=false checkout -q FETCH_HEAD
[ "$(git -C "$ship/repo" rev-parse --short=9 HEAD)" = "$label" ] || {
  echo "pack-macos: the shipped repository is not at $label" >&2
  exit 1
}
# The tree the proof reads the tarball against: the revision and the cells its
# pin names, laid down here the way pack.sh lays them down there.
mkdir -p "$work/tree"
git archive "$commit" | tar -x -C "$work/tree"
"$repo/src/tools/build/pull-cells.sh" "$work/tree/src/cells" >/dev/null

cp "$repo/src/tools/release/mac-build.sh" "$ship/mac.sh"
chmod 0755 "$ship/mac.sh"

rm -f "$archive" "$archive.sha256"
echo "pack-macos: building $name on $mac under $mroot; the log is $log"
# The run directory is each build's own: what the last one left is replaced.
tar -C "$ship" -cf - repo mac.sh |
  ssh_mac "rm -rf \"$mroot/run\" && mkdir -p \"$mroot/run\" && tar -xf - -C \"$mroot/run\""
# What shipped is the tree, not what the Mac says it packed (pack-windows.sh's
# proof, for a tarball): every file of src/src and of the shipped cells, the
# system definition and the installer, hashed against the tree sent.
prove_tree() {
python3 - "$archive" "$work/tree" "$name" <<'PY'
import hashlib, os, sys, tarfile

archive, tree, name = sys.argv[1], sys.argv[2], sys.argv[3]
with tarfile.open(archive) as t:
    members = {m.name: m for m in t.getmembers()}
    links = [n for n, m in members.items() if m.issym() or m.islnk()]
    if links:
        sys.exit('pack-macos: the tarball carries links, first %s' % links[:3])
    junk = [n for n in members if os.path.basename(n).startswith('._')]
    if junk:
        sys.exit('pack-macos: the tarball carries AppleDouble files, first %s' % junk[:3])
    wanted = []
    for base, prune_tests in (('src/src', False), ('src/cells', True)):
        for dirpath, dirnames, filenames in os.walk(os.path.join(tree, base)):
            if prune_tests:
                dirnames[:] = [d for d in dirnames if d != 'test']
            for f in filenames:
                wanted.append(os.path.relpath(os.path.join(dirpath, f), tree))
    wanted += ['src/nodecode.asd']
    pairs = [(rel, rel) for rel in wanted] + [('src/tools/install/install.sh', 'install.sh')]
    missing, differ = [], []
    for here_rel, there_rel in pairs:
        member = members.get('%s/%s' % (name, there_rel))
        if member is None or not member.isfile():
            missing.append(there_rel)
            continue
        with open(os.path.join(tree, here_rel), 'rb') as f:
            here = hashlib.sha256(f.read()).hexdigest()
        there = hashlib.sha256(t.extractfile(member).read()).hexdigest()
        if here != there:
            differ.append(there_rel)
    if missing:
        sys.exit('pack-macos: %d of %d tree files are not in the tarball, first %s' % (len(missing), len(pairs), missing[:5]))
    if differ:
        sys.exit('pack-macos: %d files of the tarball are not the tree, first %s' % (len(differ), differ[:5]))
    for required in ('nodecode', 'nodecode-launcher'):
        m = members.get('%s/%s' % (name, required))
        if m is None or not m.isfile() or m.size == 0 or not (m.mode & 0o111):
            sys.exit('pack-macos: the tarball carries no executable %s' % required)
    if not any(n.startswith('%s/src/fasl/' % name) and members[n].isfile() for n in members):
        sys.exit('pack-macos: the tarball carries no compiled cells (src/fasl/)')
    tests = [n for n in members if '/test/' in n]
    if tests:
        sys.exit('pack-macos: the tarball carries %d test paths, first %s' % (len(tests), tests[:3]))
    print('pack-macos: %d tree files byte-equal inside %s (%d members, %d bytes)'
          % (len(pairs), os.path.basename(archive), len(members), os.path.getsize(archive)))
PY
}

# The tarball comes home once the Mac says it is final (`macos-tarball-ready'),
# and is proven against the tree while the Mac's own proofs -- the trial seams,
# a serve, the feed proof's minute and more -- still run. A Mac run that ends
# before saying it ends the wait; its own failure is the one reported.
: > "$log"
ended="$work/mac-ended"
fetch_log="$work/fetch.log"
(
  until grep -q "^macos-tarball-ready $label " "$log"; do
    [ ! -e "$ended" ] || exit 3
    sleep 1
  done
  scp -q "$mac:$mroot/run/repo/dist/$name.tar.gz" "$mac:$mroot/run/repo/dist/$name.tar.gz.sha256" "$OUT/"
  prove_tree
) >"$fetch_log" 2>&1 &
fetch_pid=$!
rc=0
ssh_mac "/bin/bash \"$mroot/run/mac.sh\" $label \"$mroot\"" 2>&1 | tee -a "$log" || rc=$?
touch "$ended"
fetch_rc=0
wait "$fetch_pid" || fetch_rc=$?
fetch_pid=''
[ "$rc" = 0 ] || { echo "pack-macos: the Mac run failed (rc $rc); read $log (its tree stays in $mroot/run on $mac)" >&2; exit 1; }
verdict="$(grep "^macos-release-ok $label " "$log" | tail -n 1)"
[ -n "$verdict" ] || { echo "pack-macos: the Mac never said 'macos-release-ok $label'; read $log" >&2; exit 1; }
cat "$fetch_log"
[ "$fetch_rc" = 0 ] || { echo "pack-macos: bringing the tarball home failed (rc $fetch_rc)" >&2; exit 1; }

# The bytes brought home are the bytes the Mac proved to the end: a tarball
# that changed after it was called final fails here.
local_sha="$(sha256sum "$archive" | cut -d' ' -f1)"
sidecar_sha="$(cut -d' ' -f1 < "$archive.sha256")"
mac_sha="$(printf '%s\n' "$verdict" | sed -n 's/.* sha256 \([0-9a-f]*\).*/\1/p')"
[ "$local_sha" = "$sidecar_sha" ] && [ "$local_sha" = "$mac_sha" ] || {
  echo "pack-macos: the tarball is not the bytes the Mac proved: here $local_sha, sidecar $sidecar_sha, Mac $mac_sha" >&2
  exit 1
}

# The build tree goes; the toolchain, quicklisp and the caches stay for the
# next release.
ssh_mac "rm -rf \"$mroot/run\"" || true
echo "pack-macos: $verdict"
echo "pack-macos: $name.tar.gz sha256 $local_sha"
