#!/usr/bin/env bash
# Build the SBCL Windows runtime nodecode.exe is dumped on: sbcl.exe of the
# official SBCL 2.6.8 x86-64 Windows release, with two patches:
#
# - core-views.patch maps the core into memory copy-on-write instead of
#   reading it whole (below);
# - signed-core.patch finds the core in a signed executable. SBCL reads the
#   core's offset from a trailer at the very end of the file, and an
#   Authenticode signature appends its certificate table there: a stock
#   runtime then finds no core and starts as plain SBCL. The patched one reads
#   the trailer just before the table the PE header names (and its zero
#   padding), and saves no stale certificate pointer into an image it dumps.
#
# Every nodecode process on Windows (the shell, the organism, each worker)
# starts by loading the ~90 MB image. The stock runtime reads it all into
# fresh memory: ~30 ms per process before a line of Lisp runs, where the
# Linux and macOS runtimes map the same file in ~5. The patched runtime maps
# it the way they do, so a page costs a fault only when the process touches
# it, served from the file cache every process shares.
#
# The runtime is cross-compiled here, on Linux, with mingw-w64, and loads
# the official release's sbcl.core unchanged: the build answers make-config
# as the official build's MSYS2 shell did (features, endianness) and takes
# the official core's build id, which the runtime checks at every start. A
# runtime that does not match the installed SBCL refuses its core, and
# build-windows.ps1 falls back to the stock runtime.
#
#   tools/release/sbcl-win/build-runtime.sh [OUT]
#
# OUT defaults to the checked-in sbcl-2.6.8-patched.exe beside this script.
# Needs: curl, tar, patch, make, a host sbcl (genesis), x86_64-w64-mingw32-
# gcc/dlltool/strip. The build is reproducible: the same inputs give the same
# bytes, whatever directory it runs in (--check rebuilds and compares).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
ver=2.6.8
src_sha=ad5126dfdfba5db27ee77bcc25893020fe522d0b7653d45b4c4795ade3ddc23d
build_id=MSEDGEWIN10-IEUser-2026-08-28-21-57-29   # the official 2.6.8 MSI's sbcl.core
url=https://downloads.sourceforge.net/project/sbcl/sbcl/$ver/sbcl-$ver-source.tar.bz2
X=x86_64-w64-mingw32
# never a name with "nodecode" in it: SERVE-COMMAND takes a runtime so named
# for the dumped organism, and the Windows suite runs on this one
checked_in=$here/sbcl-$ver-patched.exe

check=0; out=$checked_in
case "${1:-}" in --check) check=1; out=$(mktemp);; ?*) out=$1;; esac

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cd "$work"
curl -fsSL -o src.tar.bz2 "$url"
echo "$src_sha  src.tar.bz2" | sha256sum -c --quiet
tar -xjf src.tar.bz2
cd sbcl-$ver
patch -p1 --quiet < "$here/core-views.patch"
patch -p1 --quiet < "$here/signed-core.patch"

# make-config.sh as the official build's MSYS2 shell ran it
mkdir -p "$work/bin"
cat > "$work/bin/uname" <<'EOF'
#!/bin/sh
case "$1" in -m) echo x86_64 ;; -r) echo 3.4.10 ;; *) echo MINGW64_NT-10.0-19045 ;; esac
EOF
chmod +x "$work/bin/uname"
python3 - <<'EOF'
s = open('make-config.sh').read()
for old, new in [
    # the AVX2 probe runs the target machine: it only picks contribs, and the runtime needs none
    ('if ! $GNUMAKE -C tools-for-build avx2 2> /dev/null || tools-for-build/avx2 ; then', 'if false ; then'),
    # probes that would run a Windows program: the answers the official build got
    ('''        $GNUMAKE -C tools-for-build determine-endianness -I ../src/runtime
        tools-for-build/determine-endianness >> $ltf''', "        printf ' :little-endian' >> $ltf"),
    ('sh tools-for-build/grovel-features.sh >> $ltf', "printf ' :sb-futex' >> $ltf"),
]:
    assert old in s, old
    s = s.replace(old, new)
open('make-config.sh', 'w').write(s)
EOF
PATH="$work/bin:$PATH" sh make-config.sh --with-sb-ldb --with-sb-thread --with-arena-allocator > "$work/config.log" 2>&1 \
  || { tail -20 "$work/config.log"; exit 1; }
printf '"%s"' "$build_id" > output/build-id.inc

# genesis: the C headers that describe the core's layout to the runtime
sh make-host-1.sh > "$work/host-1.log" 2>&1 || { tail -30 "$work/host-1.log"; exit 1; }

# the import library for WaitOnAddress & co. (:sb-futex), which mingw-w64 lacks
cat > "$work/synch.def" <<'EOF'
LIBRARY api-ms-win-core-synch-l1-2-0.dll
EXPORTS
WaitOnAddress
WakeByAddressAll
WakeByAddressSingle
EOF
$X-dlltool -d "$work/synch.def" -l "$work/libSynchronization.a"

cd src/runtime
# PE headers carry a time stamp: the source release date, not the clock
export SOURCE_DATE_EPOCH=$(date -u -d 2026-08-28 +%s)
CFLAGS="-ffile-prefix-map=$work=." make -j"$(nproc)" CC=$X-gcc LD=$X-ld AR=$X-ar NM=$X-nm OBJCOPY=$X-objcopy \
  LINKFLAGS="-Wl,-export-all-symbols -Wl,--disable-dynamicbase -Wl,--no-insert-timestamp -L$work" \
  sbcl.exe > "$work/runtime.log" 2>&1 || { tail -30 "$work/runtime.log"; exit 1; }
$X-strip --strip-all -o "$out" sbcl.exe
if [ $check = 1 ]; then
  if cmp -s "$out" "$checked_in"; then echo "reproduced $checked_in"; rm -f "$out"; exit 0; fi
  echo "differs from $checked_in: $(sha256sum < "$out") vs $(sha256sum < "$checked_in")"; rm -f "$out"; exit 1
fi
echo "$(sha256sum "$out")"
