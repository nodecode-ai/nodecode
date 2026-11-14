#!/usr/bin/env bash
# pack-env-test.sh --- the build sees no organism's environment.
#
# SPDX-License-Identifier: MIT
#
# A shell a nodecode session started carries NODECODE_HOME (the live profile,
# under the account's home) and NODECODE_SESSION_ID, and an operator may
# export provider settings such as NODECODE_API_KEY. The image resolves its
# startup parameters from them as it loads and dumps the values, so a pack run
# from such a shell wrote the builder's home into the image (the home gate
# refused, naming no cause) and would have written a key into it unnoticed.
# This runs the real pack.sh on a tiny committed tree with the build stubbed
# out: the stub image records the NODECODE_* environment and the HOME its
# build saw. Linux only -- the build home is pack.sh's Linux arm.
set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
if [ "$(uname -s)" != Linux ]; then
  echo 'pack-env-test: skipped, the build home is Linux only'
  exit 0
fi

root="$(mktemp -d)"
trap 'rm -rf "$root"' EXIT
fail() { echo "pack-env-test: FAIL $1" >&2; exit 1; }

# The tree pack.sh archives and runs from: its own scripts, stubs beside them.
t="$root/tree"
mkdir -p "$t/tools/release" "$t/tools/build/sbcl/patches" "$t/tools/install" "$t/src" "$t/cells"
cp "$repo/tools/release/pack.sh" "$repo/tools/release/layout.sh" "$repo/tools/release/README.alpha.txt" "$t/tools/release/"
cp "$repo/tools/install/install.sh" "$t/tools/install/"
echo 'echo stub' > "$t/tools/build/sbcl/build.sh"
echo 'stub' > "$t/tools/build/sbcl/patches/0001-stub.patch"
printf '#!/bin/sh\nexit 0\n' > "$t/tools/build/pull-cells.sh"
printf '#!/bin/sh\nmkdir -p "$3"\n' > "$t/tools/build/compile-cells.sh"
chmod +x "$t/tools/build/pull-cells.sh" "$t/tools/build/compile-cells.sh"
echo '(defsystem "nodecode")' > "$t/nodecode.asd"
echo ';; stub' > "$t/src/stub.lisp"
echo 'stub' > "$t/cells/README"
( cd "$t" && git init -q && git add -A \
  && git -c user.name=pack-env-test -c user.email=pack-env-test@invalid -c commit.gpgsign=false commit -qm tree )
name="nodecode-$(git -C "$t" rev-parse --short=9 HEAD)-x86_64-linux"

# The build home, its SBCL and quicklisp already in place. `just release'
# leaves a stub image that holds what a real image's load would have read:
# every NODECODE_* variable in its environment, and its HOME.
bh="$root/build-home"
key="$(cat "$t/tools/build/sbcl/build.sh" "$t"/tools/build/sbcl/patches/* | sha256sum | cut -d' ' -f1)"
mkdir -p "$bh/env/.cache/nodecode-sbcl/$key/bin" "$bh/env/quicklisp" "$root/bin"
printf '#!/bin/sh\nexit 0\n' > "$bh/env/.cache/nodecode-sbcl/$key/bin/sbcl"
: > "$bh/env/quicklisp/setup.lisp"
cat > "$root/bin/just" <<'STUB'
#!/bin/sh
[ "$1" = release ] || exit 1
{ echo '#!/bin/sh'; env | grep '^NODECODE_' | sed 's/^/# /'; echo "# HOME=$HOME"; } > nodecode
chmod +x nodecode
STUB
printf '#!/bin/sh\nexit 0\n' > "$root/bin/objdump"
chmod +x "$bh/env/.cache/nodecode-sbcl/$key/bin/sbcl" "$root/bin/just" "$root/bin/objdump"

# The session's environment: a profile under the account's home, a session,
# and a provider key.
account_home="$(eval echo "~$(id -un)")"
secret="pack-env-test-key-$$"
status=0
( cd "$t" && env PATH="$root/bin:$PATH" NODECODE_BUILD_HOME="$bh" \
    NODECODE_HOME="$account_home/.nodecode/profiles/pack-env-test" \
    NODECODE_SESSION_ID=pack-env-test NODECODE_API_KEY="$secret" \
    bash tools/release/pack.sh HEAD "$root/out" ) > "$root/pack.log" 2>&1 || status=$?
[ "$status" -eq 0 ] || fail "a pack from a nodecode session's shell refused: $(grep refusing "$root/pack.log" || tail -3 "$root/pack.log")"

image="$(tar -xOzf "$root/out/$name.tar.gz" "$name/nodecode")"
leaked="$(printf '%s\n' "$image" | sed -n 's/^# \(NODECODE_[A-Z_]*\)=.*/\1/p' | grep -v '^NODECODE_BUILD_' || true)"
[ -z "$leaked" ] || fail "the build saw the session's $(echo $leaked)"
printf '%s\n' "$image" | grep -qF "$secret" && fail 'the image carries the session API key'
printf '%s\n' "$image" | grep -qxF "# HOME=$bh/env" \
  || fail "the build ran outside NODECODE_BUILD_HOME: $(printf '%s\n' "$image" | grep '^# HOME=')"

echo 'pack-env-test: ok (a pack from a session shell builds with no NODECODE_* but its own NODECODE_BUILD_*)'
