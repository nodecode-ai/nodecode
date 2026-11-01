#!/bin/sh
# build.sh --- the SBCL both POSIX releases are dumped on: 2.6.8 as Homebrew
# builds it (the release tarball, Homebrew's one patch, its flags), plus this
# folder's own patches. pack-macos.sh builds it on the Mac, pack.sh on the
# Linux release box.
#
#   src/tools/build/sbcl/build.sh PREFIX [WORKDIR]
#
# Builds with the `sbcl' on PATH as the host and installs into PREFIX; the
# built tree stays in WORKDIR (a fresh temp directory by default), where
# tests/run-tests.sh runs SBCL's own suite against it. The version stays
# "2.6.8", so fasls compiled by Homebrew's build load here and the reverse.
#
# SPDX-License-Identifier: MIT
set -eu

prefix="$1"
work="${2:-$(mktemp -d)}"
here="$(cd "$(dirname "$0")" && pwd)"
version=2.6.8
sha256=ad5126dfdfba5db27ee77bcc25893020fe522d0b7653d45b4c4795ade3ddc23d

mkdir -p "$work"
tarball="$work/sbcl-$version-source.tar.bz2"
curl -fsSL --retry 3 -o "$tarball" \
  "https://downloads.sourceforge.net/project/sbcl/sbcl/$version/sbcl-$version-source.tar.bz2"
if command -v sha256sum >/dev/null 2>&1; then
  echo "$sha256  $tarball" | sha256sum -c -
else
  echo "$sha256  $tarball" | shasum -a 256 -c -
fi
rm -rf "$work/sbcl-$version"
tar -xjf "$tarball" -C "$work"
cd "$work/sbcl-$version"
for patch in "$here"/patches/*.patch; do
  echo "sbcl: applying $(basename "$patch")"
  patch -p1 < "$patch"
done
# The release runs on macOS 15 and later, whichever Mac builds it: a runtime
# built for the builder's own macOS refuses to start on any older one.
# SBCL's arm64-darwin Config reads no SBCL_MACOSX_VERSION_MIN (only its x86-64
# and ppc Configs do), so the floor also goes to clang itself, which reads
# MACOSX_DEPLOYMENT_TARGET: without it an Apple Silicon runtime takes the
# builder's own macOS (minos 26 on rev) and pack.sh refuses it.
if [ "$(uname -s)" = Darwin ]; then
  SBCL_MACOSX_VERSION_MIN="${SBCL_MACOSX_VERSION_MIN:-15}"
  MACOSX_DEPLOYMENT_TARGET="${SBCL_MACOSX_VERSION_MIN}.0"
  export SBCL_MACOSX_VERSION_MIN MACOSX_DEPLOYMENT_TARGET
fi
# Core compression is Homebrew's flag and links the runtime against libzstd.
# The Linux runtime links nothing but libc and libm, so every box with a new
# enough glibc runs it without one more library to install.
compression=
if [ "$(uname -s)" = Darwin ]; then
  compression=--with-sb-core-compression
fi
sh make.sh --prefix="$prefix" $compression --with-sb-ldb --with-sb-thread
INSTALL_ROOT="$prefix" sh install.sh
echo "sbcl: $version with $(ls "$here"/patches | wc -l | tr -d ' ') patches installed in $prefix"
