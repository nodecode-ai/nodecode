#!/usr/bin/env bash
# pack-windows-zst.sh --- the Windows arm's second container: the same tree as
# tar.zst, packed from the zip the sandbox already proved.
#
# SPDX-License-Identifier: MIT
#
# pack-windows.sh reads the zip back against the tree it sent and refuses one
# that differs, so the zip is the proven body; this writes the transport the
# installer prefers -- 22.6 MB against 30.0, and bsdtar (Windows 10 1803+)
# unpacks it straight into the release folder in a quarter of a second, where
# the zip is read by .NET and then copied into the same place. The zip keeps
# being published beside it: an installer older than tar.zst reads only that.
#
#   pack-windows-zst.sh dist/nodecode-<label>-x86_64-windows.zip [OUT.tar.zst]
#
# The pack is a minute of every core (zstd --ultra -22), and a release's heavy
# work belongs on the build host, never the publishing machine: this sends the
# zip to NODECODE_WINBOX_HOST, runs itself there (--here), and brings the
# tar.zst and its sidecar back, checking one against the other. Every file of
# the tar.zst is hashed against the zip's on that host before it is kept, so a
# release cannot ship two different trees under one label. The zip's own entry
# names may carry backslashes (Compress-Archive writes them); the tar carries
# slash names, which is what tar.exe and GNU tar both read.
set -euo pipefail

if [ "${1:-}" = --here ]; then
  shift
  zip="${1:?--here ARCHIVE.zip}"
  out="${zip%.zip}.tar.zst"
  for tool in zstd python3 tar; do
    command -v "$tool" >/dev/null 2>&1 || { echo "pack-windows-zst: $tool is needed on $(hostname)" >&2; exit 1; }
  done
  work="$(mktemp -d)"
  trap 'rm -rf "$work"' EXIT INT TERM
  tree="$work/tree"
  mkdir -p "$tree"

  python3 - "$zip" "$tree" <<'PY'
import os, sys, zipfile

archive, tree = sys.argv[1], sys.argv[2]
with zipfile.ZipFile(archive) as z:
    for info in z.infolist():
        name = info.filename.replace('\\', '/')
        if name.endswith('/') or not name:
            continue
        path = os.path.join(tree, name)
        os.makedirs(os.path.dirname(path) or tree, exist_ok=True)
        with open(path, 'wb') as f, z.open(info) as src:
            for chunk in iter(lambda: src.read(1 << 20), b''):
                f.write(chunk)
print('pack-windows-zst: read %d entries out of %s' % (len(z.namelist()), os.path.basename(archive)))
PY

  # Long-window ultra: the payload is one 84 MB executable, so the smallest
  # body wins over the fastest pack -- but no wider than 27: a 2 GB window
  # needs 2 GB to decode, which tar.exe (and zstd without --long) refuses.
  # 27 is 128 MB, and every reader reads it.
  #
  # The bytes are the zip's alone -- names sorted, no clock, no owner, one mode
  # -- so a re-run after an upload packs the body the host already holds (a
  # published name keeps its bytes) instead of one that only differs in when
  # and by whom it was unpacked. zstd's output does not depend on -T.
  tar --sort=name --mtime=@0 --owner=0 --group=0 --numeric-owner --mode='u=rwX,go=rX' \
      --format=gnu -cf - -C "$tree" . | zstd --ultra -22 --long=27 -T0 -q -f -o "$out.tmp"
  mv "$out.tmp" "$out"
  # The sidecar names the file alone, as every sidecar on /dl does.
  (cd "$(dirname "$out")" && sha256sum "$(basename "$out")") > "$out.sha256"

  # Read it back: every file the zip holds is in the tar.zst, byte for byte.
  back="$work/back"
  mkdir -p "$back"
  tar --zstd -xf "$out" -C "$back"
  python3 - "$zip" "$back" <<'PY'
import hashlib, os, sys, zipfile

archive, back = sys.argv[1], sys.argv[2]

def digest(path):
    h = hashlib.sha256()
    with open(path, 'rb') as f:
        for chunk in iter(lambda: f.read(1 << 20), b''):
            h.update(chunk)
    return h.hexdigest()

checked = 0
with zipfile.ZipFile(archive) as z:
    for info in z.infolist():
        name = info.filename.replace('\\', '/')
        if name.endswith('/') or not name:
            continue
        there = os.path.join(back, name)
        if not os.path.isfile(there):
            sys.exit('pack-windows-zst: %s is in the zip and not in the tar.zst' % name)
        with z.open(info) as src:
            here = hashlib.sha256()
            for chunk in iter(lambda: src.read(1 << 20), b''):
                here.update(chunk)
        if here.hexdigest() != digest(there):
            sys.exit('pack-windows-zst: %s differs between the zip and the tar.zst' % name)
        checked += 1
print('pack-windows-zst: %d files byte-equal between the zip and the tar.zst' % checked)
PY
  exit 0
fi

zip="${1:?usage: pack-windows-zst.sh ARCHIVE.zip [OUT.tar.zst]}"
[ -s "$zip" ] || { echo "pack-windows-zst: no $zip" >&2; exit 1; }
out="${2:-${zip%.zip}.tar.zst}"
case "$out" in *.tar.zst) ;; *) echo "pack-windows-zst: $out is not a .tar.zst" >&2; exit 1 ;; esac

. "$(dirname "${BASH_SOURCE[0]}")/publish-config.sh"
host="$(publish_setting NODECODE_WINBOX_HOST)" || exit 1

name="$(basename "$zip")"
# A directory of its own under the host's home, named for the archive, so two
# releases packing at once never share one; removed however the run ends.
remote="nodecode-zst-${name%.zip}"
on_host() { ssh -o BatchMode=yes -o ServerAliveInterval=30 "$host" "$@"; }
trap 'on_host "rm -rf $remote" >/dev/null 2>&1 || true' EXIT INT TERM
on_host "rm -rf $remote && mkdir -p $remote"
scp -q "$zip" "$host:$remote/$name"
echo "pack-windows-zst: packing ${name%.zip}.tar.zst on $host"
on_host "bash -s -- --here $remote/$name" < "${BASH_SOURCE[0]}"
scp -q "$host:$remote/${name%.zip}.tar.zst" "$out"
scp -q "$host:$remote/${name%.zip}.tar.zst.sha256" "$out.sha256.tmp"
# The sidecar names the published file, whatever this run named its output.
sha="$(cut -d' ' -f1 < "$out.sha256.tmp")"
rm -f "$out.sha256.tmp"
[ "$(sha256sum "$out" | cut -d' ' -f1)" = "$sha" ] || {
  echo "pack-windows-zst: $out does not hash to the sidecar $host wrote" >&2
  rm -f "$out"
  exit 1
}
printf '%s  %s\n' "$sha" "$(basename "$out")" > "$out.sha256"
chmod 0644 "$out" "$out.sha256"
echo "pack-windows-zst: $out $(stat -c%s "$out") bytes, sha256 $sha"
