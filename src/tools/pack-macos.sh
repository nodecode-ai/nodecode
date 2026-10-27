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
# publish-config.sh). The Mac half is src/tools/mac-build.sh, sent with the
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
# byte-equal inside the tarball, and the sidecar the hash of the bytes.
set -euo pipefail

REV="${1:-HEAD}"
OUT="${2:-dist}"

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo"

. "$repo/src/tools/publish-config.sh"
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
cleanup() {
  if [ -n "$locked" ]; then ssh_mac "rm -rf \"$mroot/.lock\"" >/dev/null 2>&1 || true; fi
  rm -rf "$work"
}
trap cleanup EXIT INT TERM

# One build at a time on the Mac: the run directory is one per account.
owner="$(hostname) pid $$ $label $(date -u +%FT%TZ)"
if ! ssh_mac "mkdir -p \"$mroot\" && mkdir \"$mroot/.lock\" && printf '%s\n' '$owner' > \"$mroot/.lock/owner\""; then
  held="$(ssh_mac "cat \"$mroot/.lock/owner\"" 2>/dev/null || true)"
  printf 'pack-macos: another build holds %s/.lock on %s (%s); remove it when none runs\n' "$mroot" "$mac" "${held:-no owner}" >&2
  exit 1
fi
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
"$repo/src/tools/pull-cells.sh" "$work/tree/src/cells" >/dev/null

cp "$repo/src/tools/mac-build.sh" "$ship/mac.sh"
chmod 0755 "$ship/mac.sh"

rm -f "$archive" "$archive.sha256"
echo "pack-macos: building $name on $mac under $mroot; the log is $log"
# The run directory is each build's own: what the last one left is replaced.
tar -C "$ship" -cf - repo mac.sh |
  ssh_mac "rm -rf \"$mroot/run\" && mkdir -p \"$mroot/run\" && tar -xf - -C \"$mroot/run\""
rc=0
ssh_mac "/bin/bash \"$mroot/run/mac.sh\" $label \"$mroot\"" 2>&1 | tee "$log" || rc=$?
[ "$rc" = 0 ] || { echo "pack-macos: the Mac run failed (rc $rc); read $log (its tree stays in $mroot/run on $mac)" >&2; exit 1; }
verdict="$(grep "^macos-release-ok $label " "$log" | tail -n 1)"
[ -n "$verdict" ] || { echo "pack-macos: the Mac never said 'macos-release-ok $label'; read $log" >&2; exit 1; }

scp -q "$mac:$mroot/run/repo/dist/$name.tar.gz" "$mac:$mroot/run/repo/dist/$name.tar.gz.sha256" "$OUT/"
local_sha="$(sha256sum "$archive" | cut -d' ' -f1)"
sidecar_sha="$(cut -d' ' -f1 < "$archive.sha256")"
mac_sha="$(printf '%s\n' "$verdict" | sed -n 's/.* sha256 \([0-9a-f]*\).*/\1/p')"
[ "$local_sha" = "$sidecar_sha" ] && [ "$local_sha" = "$mac_sha" ] || {
  echo "pack-macos: the tarball is not the bytes the Mac proved: here $local_sha, sidecar $sidecar_sha, Mac $mac_sha" >&2
  exit 1
}

# What shipped is the tree, not what the Mac says it packed (pack-windows.sh's
# proof, for a tarball): every file of src/src and of the shipped cells, the
# system definition and the installer, hashed against the tree sent.
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
    pairs = [(rel, rel) for rel in wanted] + [('src/tools/install.sh', 'install.sh')]
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

# The build tree goes; the toolchain, quicklisp and the caches stay for the
# next release.
ssh_mac "rm -rf \"$mroot/run\"" || true
echo "pack-macos: $verdict"
echo "pack-macos: $name.tar.gz sha256 $local_sha"
