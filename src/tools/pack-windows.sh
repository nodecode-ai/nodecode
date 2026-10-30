#!/usr/bin/env bash
# pack-windows.sh --- the Windows arm of the release: built, smoked and zipped
# in the Windows sandbox on the build host (NODECODE_WINBOX_HOST).
#
# SPDX-License-Identifier: MIT
#
# Run through `just windows'. SBCL cannot cross-compile and the release waits
# on no CI lane, so the Windows zip is packed the way pack.sh packs the POSIX
# tarball -- from `git archive REV', the shipped cells pulled in from
# src/cells/bundle.json, named nodecode-<label>-x86_64-windows.zip -- with the
# compiler running on a Windows machine instead of this one. That machine is
# the winbox container on NODECODE_WINBOX_HOST (a publish setting, like
# NODECODE_MACOS_HOST), never the publishing machine: a Windows guest is a
# whole VM, and a release's heavy work belongs on the build box. The rig is
# NODECODE_WINBOX_DIR under that host's home (default nodecode-winbox): its
# `win -f SCRIPT.ps1' hands a script to the guest over ssh (the host's
# localhost:2222), and the rig's shared/ directory is what the guest reads as
# \\host.lan\Data. The guest half is release-windows.ps1, checked in beside
# this driver; the guest's whole log lands here as OUT/windows-<label>.log.
#
# This script sends the tree, runs the guest, and proves what came back is the
# tree it sent: every source file, every shipped cell file, the system
# definition and the installer byte-equal inside the zip. What a host cannot
# check from here -- that the exe runs and the seams work -- the guest proves
# before it stages the zip, and it says so in one line before this one reads
# the bytes.
set -euo pipefail

REV="${1:-HEAD}"
OUT="${2:-dist}"

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo"

. "$repo/src/tools/publish-config.sh"
host="$(publish_setting NODECODE_WINBOX_HOST)" || exit 1
winbox="${NODECODE_WINBOX_DIR:-nodecode-winbox}"
case "$winbox" in
  ''|/*|*..*|*' '*) echo "pack-windows: NODECODE_WINBOX_DIR is a path under $host's home, not '$winbox'" >&2; exit 1 ;;
esac
# Every remote command names the rig relative to the host's home, so no path of
# this machine reaches one and nothing in it needs quoting there.
on_host() { ssh -o BatchMode=yes -o ServerAliveInterval=30 "$host" "$@"; }
on_host "test -x $winbox/win && test -s $winbox/winbox_key && test -d $winbox/shared && command -v podman >/dev/null" || {
  printf 'pack-windows: %s holds no rig at ~/%s (win, winbox_key, shared/, podman): that rig builds Windows; NODECODE_WINBOX_HOST and NODECODE_WINBOX_DIR point at it\n' "$host" "$winbox" >&2
  exit 1
}
command -v python3 >/dev/null 2>&1 || { echo 'pack-windows: python3 is needed to read the zip back against the tree' >&2; exit 1; }

commit="$(git rev-parse "$REV^{commit}")"
# NINE hex digits, explicitly: a bare --short sizes itself by the repository's
# object count, and a label is what a channel pointer names (pack.sh's note).
label="$(git rev-parse --short=9 "$commit")"
name="nodecode-$label-x86_64-windows"
archive="$repo/$OUT/$name.zip"
log="$OUT/windows-$label.log"

work="$(mktemp -d)"
mkdir -p "$OUT"

# The guest's disk carries what a build needs -- SBCL, the quicklisp tree, the
# DLLs -- so a stopped sandbox is started, and given back the way it was found.
started=""
cleanup() {
  if [ -n "$started" ]; then on_host podman stop winbox >/dev/null 2>&1 || true; fi
  rm -rf "$work"
}
trap cleanup EXIT INT TERM
if [ "$(on_host "podman inspect -f '{{.State.Running}}' winbox" 2>/dev/null || true)" != true ]; then
  echo "pack-windows: starting the winbox sandbox on $host"
  on_host podman start winbox >/dev/null
  started=1
fi

guest_up() {
  on_host "ssh -i $winbox/winbox_key -p 2222 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR -o ConnectTimeout=8 -o BatchMode=yes Docker@127.0.0.1 'echo winbox-up'" 2>/dev/null |
    grep -q winbox-up
}

# dockur's Windows boots in minutes, and answers ssh only once its desktop is
# up: nothing here is hung, it is a machine starting.
waited=0
until guest_up; do
  [ "$waited" -lt 1800 ] || {
    printf 'pack-windows: the sandbox did not answer ssh within %ss\n' "$waited" >&2
    exit 1
  }
  sleep 15
  waited=$((waited + 15))
done

# The build tree is the revision, not the checkout (pack.sh's note), plus the
# shipped cells -- which no revision of this repository carries: they come from
# the pin in src/cells/bundle.json and mark themselves (.bundle), so a folder
# that was never pulled is visible to the proof below.
mkdir -p "$work/nodecode"
git archive "$commit" | tar -x -C "$work/nodecode"
tree="$work/nodecode"
"$repo/src/tools/pull-cells.sh" "$tree/src/cells"
[ -s "$tree/src/cells/bundle.json" ] || { echo "pack-windows: no bundle.json under $tree/src/cells" >&2; exit 1; }
[ -n "$(find "$tree/src/cells" -mindepth 2 -name .bundle -print -quit)" ] || {
  echo "pack-windows: pull-cells laid down no cell under $tree/src/cells" >&2
  exit 1
}

tar -cf - -C "$work" nodecode | on_host "cat > $winbox/shared/nodecode-src.tar"
printf '%s\n' "$label" | on_host "cat > $winbox/shared/release-rev.txt"
on_host "cat > $winbox/release-windows.ps1" < "$repo/src/tools/release-windows.ps1"
# A zip left by an earlier run must not be mistaken for this run's: the guest
# stages both files again, and the verdict line below is required either way.
on_host "rm -f $winbox/shared/$name.zip $winbox/shared/$name.zip.sha256"

echo "pack-windows: building $name in the sandbox on $host; the guest's log is $log"
rc=0
on_host "$winbox/win -f $winbox/release-windows.ps1" < /dev/null 2>&1 | tee "$log" || rc=$?
[ "$rc" = 0 ] || { echo "pack-windows: the sandbox run failed (rc $rc); read $log" >&2; exit 1; }
grep -q "^winbox-release-ok $label\$" "$log" || {
  echo "pack-windows: the guest never said 'winbox-release-ok $label'; read $log" >&2
  exit 1
}

artifact="$winbox/shared/$name.zip"
on_host "test -s $artifact && test -s $artifact.sha256" || {
  echo "pack-windows: the guest reported the build but left no $name.zip beside the tar on $host" >&2
  exit 1
}
scp -q -o BatchMode=yes "$host:$artifact" "$archive"
scp -q -o BatchMode=yes "$host:$artifact.sha256" "$archive.sha256"
local_sha="$(sha256sum "$archive" | cut -d' ' -f1)"
sidecar_sha="$(cut -d' ' -f1 < "$archive.sha256")"
[ "$local_sha" = "$sidecar_sha" ] || {
  echo "pack-windows: the zip does not hash to its own sidecar: $local_sha != $sidecar_sha" >&2
  exit 1
}

# What shipped is the tree, not what a staging step says it copied: every file
# of src/src and of the shipped cells, the system definition and the installer
# are read back out of the zip and hashed against the tree this run sent. A
# zip is a copy of a copy, and the cell folders inside it are the ones a user's
# organism offers: a release that ships without them installs an organism that
# can offer no cell at all (SHIPPED-CELLS-DIRECTORY reads <source>\..\cells\).
python3 - "$archive" "$tree" <<'PY'
import hashlib, os, sys, zipfile

archive, tree = sys.argv[1], sys.argv[2]


def digest(reader):
    h = hashlib.sha256()
    for chunk in iter(lambda: reader.read(1 << 20), b''):
        h.update(chunk)
    return h.hexdigest()


with zipfile.ZipFile(archive) as z:
    # Compress-Archive writes backslash separators; a reader on Windows takes
    # either, so normalize both sides to slash.
    entries = {name.replace('\\', '/'): name for name in z.namelist()}
    if len(entries) != len(z.namelist()):
        sys.exit('pack-windows: the zip carries one name twice')

    wanted = []
    for base, prune_tests in (('src/src', False), ('src/cells', True)):
        for dirpath, dirnames, filenames in os.walk(os.path.join(tree, base)):
            if prune_tests:
                dirnames[:] = [d for d in dirnames if d != 'test']
            for f in filenames:
                # Compress-Archive omits hidden files, so the pulled folders'
                # .bundle markers are the tree's proof, not the zip's.
                if prune_tests and f.startswith('.'):
                    continue
                wanted.append(os.path.relpath(os.path.join(dirpath, f), tree).replace(os.sep, '/'))

    missing = [rel for rel in wanted if rel not in entries]
    if missing:
        sys.exit('pack-windows: %d of %d tree files are not in the zip, first %s'
                 % (len(missing), len(wanted), missing[:5]))
    differ = []
    for rel in wanted:
        with open(os.path.join(tree, rel), 'rb') as f:
            here = digest(f)
        with z.open(entries[rel]) as f:
            there = digest(f)
        if here != there:
            differ.append(rel)
    if differ:
        sys.exit('pack-windows: %d files of the zip are not the tree, first %s'
                 % (len(differ), differ[:5]))

    # The installer rides beside the binary in the zip, as install.sh does in
    # the tarball -- a running organism stages its own update through it.
    if 'install.ps1' not in entries:
        sys.exit('pack-windows: the zip carries no install.ps1')
    with open(os.path.join(tree, 'src/tools/install.ps1'), 'rb') as f:
        here = digest(f)
    with z.open(entries['install.ps1']) as f:
        there = digest(f)
    if here != there:
        sys.exit("pack-windows: the zip's installer is not the tree's")

    for required in ('nodecode.exe', 'nodecode-launcher.exe', 'libsqlite3.dll', 'libssl-3-x64.dll', 'libcrypto-3-x64.dll'):
        if required not in entries or z.getinfo(entries[required]).file_size == 0:
            sys.exit('pack-windows: the zip carries no %s' % required)
    tests = [name for name in entries if '/test/' in name]
    if tests:
        sys.exit('pack-windows: the zip carries %d test paths, first %s' % (len(tests), tests[:3]))

    print('pack-windows: %d tree files byte-equal inside %s (%d entries, %d bytes)'
          % (len(wanted), os.path.basename(archive), len(entries), os.path.getsize(archive)))
PY

# The transport the installer prefers: the same tree as tar.zst, proved
# against the zip that was just proved against the tree (pack-windows-zst.sh
# hashes every file of one out of the other). The zip ships beside it, because
# an installer older than the tar.zst arm reads only the zip.
"$repo/src/tools/pack-windows-zst.sh" "$archive"

echo "pack-windows: $name.zip sha256 $local_sha"
echo "pack-windows: the sandbox's own log is $log"
