#!/bin/bash
# mac-build.sh LABEL B --- the Mac half of the release (pack-macos.sh ships
# it as run/mac.sh) and of the Mac CI job (ci-macos.sh): the toolchain, the
# stack scrub, quicklisp on the pinned dist, `just tarball' of the tree at
# B/run/repo, the trial seams, a headless serve and the feed proof, ending in
# one line, `macos-release-ok LABEL minos M sha256 S'.
#
# SPDX-License-Identifier: MIT
#
# macOS's own bash (3.2): no mapfile, no associative arrays. Every path it
# writes is under B.
set -euo pipefail
label="$1"
B="$2"
run="$B/run"
repo="$run/repo"
name="nodecode-$label-arm64-darwin"
export HOME="$B/env"
export XDG_CACHE_HOME="$HOME/.cache"
export PATH="$B/bin:/usr/bin:/bin:/usr/sbin:/sbin"
unset NODECODE_HOME SBCL_HOME CPATH LIBRARY_PATH || true
mkdir -p "$HOME" "$B/bin" "$run/dl"
step() { printf '\n== %s (%ss)\n' "$1" "$SECONDS"; }
cd "$repo"
[ "$(git rev-parse --short=9 HEAD)" = "$label" ] || { echo "mac: the tree is not $label" >&2; exit 1; }

step 'Homebrew zstd and openssl@3, present (nothing is installed)'
for f in /opt/homebrew/opt/zstd/include/zstd.h /opt/homebrew/opt/zstd/lib/libzstd.1.dylib \
         /opt/homebrew/opt/openssl@3/lib/libssl.dylib /opt/homebrew/opt/openssl@3/bin/openssl; do
  [ -e "$f" ] || { echo "mac: $f is missing; this arm needs Homebrew's zstd and openssl@3 and installs nothing outside $B" >&2; exit 1; }
done
export CPATH=/opt/homebrew/opt/zstd/include LIBRARY_PATH=/opt/homebrew/opt/zstd/lib

step 'just'
just_version=1.59.0
just_sha=6574cea774ed4cae12e24b6aa11857de33e80dab4667cf628fd5840df96a2a9d
if ! "$B/bin/just" --version 2>/dev/null | grep -qx "just $just_version"; then
  curl -fsSL --retry 3 -o "$run/dl/just.tar.gz" \
    "https://github.com/casey/just/releases/download/$just_version/just-$just_version-aarch64-apple-darwin.tar.gz"
  echo "$just_sha  $run/dl/just.tar.gz" | shasum -a 256 -c -
  tar -xzf "$run/dl/just.tar.gz" -C "$run/dl" just
  mv "$run/dl/just" "$B/bin/just"
fi
just --version

# The release is dumped on SBCL 2.6.8 plus src/tools/sbcl/patches (build.sh),
# rebuilt when those change, and SBCL's own tests for what the patches touch
# run on every fresh build. Its host is Homebrew's SBCL bottle, unpacked here
# and pointed at the zstd that is present: fetched, never installed.
step 'the patched SBCL'
# The release starts on macOS 15 and later: build.sh sets that floor (for
# clang too); a cached runtime above it is rebuilt below.
minos_of() { vtool -show-build "$1" 2>/dev/null | awk '$1 == "minos" { print $2; exit }'; }
key="$(cat src/tools/sbcl/build.sh src/tools/sbcl/patches/* | shasum -a 256 | cut -d' ' -f1)"
runtime_minos="$(minos_of "$B/sbcl/bin/sbcl" || true)"
if [ ! -x "$B/sbcl/bin/sbcl" ] || [ "$(cat "$B/sbcl/.key" 2>/dev/null || true)" != "$key" ] \
   || [ -z "$runtime_minos" ] || [ "${runtime_minos%%.*}" -gt 15 ]; then
  host="$(ls -d "$B"/host/sbcl/*/ 2>/dev/null | head -n 1 || true)"
  if [ -z "$host" ] || [ ! -x "${host}libexec/bin/sbcl" ]; then
    brewenv="HOMEBREW_CACHE=$run/brew-cache HOMEBREW_NO_AUTO_UPDATE=1 HOMEBREW_NO_ANALYTICS=1 HOMEBREW_NO_INSTALL_CLEANUP=1 HOMEBREW_NO_ENV_HINTS=1"
    env $brewenv /opt/homebrew/bin/brew fetch --formula sbcl
    bottle="$(env $brewenv /opt/homebrew/bin/brew --cache --formula sbcl)"
    rm -rf "$B/host"
    mkdir -p "$B/host"
    tar -xzf "$bottle" -C "$B/host"
    host="$(ls -d "$B"/host/sbcl/*/ | head -n 1)"
    install_name_tool -change @@HOMEBREW_PREFIX@@/opt/zstd/lib/libzstd.1.dylib \
      /opt/homebrew/opt/zstd/lib/libzstd.1.dylib "${host}libexec/bin/sbcl" 2>/dev/null
    codesign -f -s - "${host}libexec/bin/sbcl"
  fi
  mkdir -p "$run/hostbin"
  printf '#!/bin/sh\nSBCL_HOME="%slib/sbcl" exec "%slibexec/bin/sbcl" "$@"\n' "$host" "$host" > "$run/hostbin/sbcl"
  chmod 0755 "$run/hostbin/sbcl"
  "$run/hostbin/sbcl" --version
  rm -rf "$B/sbcl"
  PATH="$run/hostbin:$PATH" sh src/tools/sbcl/build.sh "$B/sbcl" "$run/sbcl-build"
  runtime_minos="$(minos_of "$B/sbcl/bin/sbcl")"
  echo "minos sbcl runtime $runtime_minos"
  [ -n "$runtime_minos" ] && [ "${runtime_minos%%.*}" -le 15 ] || {
    echo "mac: the SBCL runtime needs macOS ${runtime_minos:-unknown}; the floor is 15" >&2
    exit 1
  }
  ( cd "$run"/sbcl-build/sbcl-*/tests && sh run-tests.sh exhaust.impure.lisp gc.impure.lisp gc-threads.impure.lisp \
      fcb-threads.impure.lisp dynamic-extent.pure.lisp make-thread.pure.lisp interrupt-consing.impure.lisp )
  printf '%s\n' "$key" > "$B/sbcl/.key"
fi
export PATH="$B/sbcl/bin:$PATH" SBCL_HOME="$B/sbcl/lib/sbcl"
sbcl --version

step 'the stack scrub'
sbcl --control-stack-size 8MB --non-interactive --no-sysinit --no-userinit --load src/tools/sbcl/stack-check.lisp

step 'quicklisp on the pinned dist'
dist=2026-01-01
if [ ! -f "$HOME/quicklisp/setup.lisp" ]; then
  curl -fsSLo "$run/dl/quicklisp.lisp" https://beta.quicklisp.org/quicklisp.lisp
  sbcl --noinform --non-interactive --no-userinit --load "$run/dl/quicklisp.lisp" --eval '(quicklisp-quickstart:install)'
fi
cat > "$HOME/.sbclrc" <<'RC'
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
RC
sbcl --noinform --non-interactive \
  --eval "(unless (equal (ql-dist:version (ql-dist:find-dist \"quicklisp\")) \"$dist\") (ql-dist:install-dist \"http://beta.quicklisp.org/dist/quicklisp/$dist/distinfo.txt\" :replace t :prompt nil))" \
  --eval '(format t "~&dist ~a~%" (ql-dist:version (ql-dist:find-dist "quicklisp")))'
( cd src && sbcl --dynamic-space-size 4096 --control-stack-size 8MB --noinform --non-interactive \
    --eval '(push (truename ".") asdf:*central-registry*)' \
    --eval '(ql:quickload "nodecode" :silent t)' \
    --eval '(format t "~&dependencies present~%")' )

step 'pack the tarball'
rm -rf dist
just tarball HEAD dist
ls -ln dist

step 'unpack it where it never was'
mkdir -p "$run/unpacked" "$run/bin"
tar -xzf "dist/$name.tar.gz" -C "$run/unpacked"
dir="$run/unpacked/$name"
ln -sfn "$dir/nodecode" "$run/bin/nodecode"
minos=''
for bin in nodecode nodecode-launcher; do
  m="$(vtool -show-build "$dir/$bin" | awk '$1 == "minos" { print $2; exit }')"
  echo "minos $bin $m"
  [ -n "$m" ] && [ "${m%%.*}" -le 15 ] || { echo "mac: $bin needs macOS ${m:-unknown}; the floor is 15" >&2; exit 1; }
  [ -n "$minos" ] || minos="$m"
done
version="$("$dir/nodecode" --version 2>&1 | tail -n 1)"
echo "version: $version"
case "$version" in *"+$label"*) ;; *) echo "mac: the binary says '$version', not $label" >&2; exit 1 ;; esac

# The trial seams, as the lane ran them. A scratch home that is not
# $HOME/.nodecode: no step here may stand for the account's own home.
step 'the platform seams through the trial verb'
HOME_SCRATCH="$run/trial-home"
mkdir -p "$HOME_SCRATCH"
trial() {
  local out
  out="$(printf '%s\n' "$1" | NODECODE_HOME="$HOME_SCRATCH" "$dir/nodecode" trial 2>&1 || true)"
  { echo "trial: $1"; echo "$out"; } >&2
  local score
  score="$(printf '%s\n' "$out" | sed -n 's/.*nodecode-trial-score \([^ ]*\).*/\1/p' | tail -n 1)"
  [ -n "$score" ] || { echo "no score line for: $1" >&2; exit 1; }
  printf '%s' "$score"
}
expect() { local got; got="$(trial "$2")"; [ "$got" = "$1" ] || { echo "FAIL ($3): expected $1, got $got" >&2; exit 1; }; echo "ok: $3"; }
expect 3 '(+ 1 2)' arithmetic
expect 1 '(if (eq :darwin (nlk:platform)) 1 0)' platform
expect 1 '(if (= 2 (length (multiple-value-list (nlt:terminal-size)))) 1 0)' 'terminal-size without a terminal'
expect 1 '(if (equal "/bin/sh" nlk:*shell*) 1 0)' 'the shell is /bin/sh'
expect 1 '(if (equal "/usr/bin/perl" (first nlk:*session-wrapper*)) 1 0)' 'perl leads the session'
expect 1 '(multiple-value-bind (out err status) (nlk:run-bounded (nlk:shell-argv "echo hi; echo bad 1>&2; exit 4") :seconds 20) (if (and (search "hi" out) (search "bad" err) (eql status 4)) 1 0))' run-bounded
expect 1 '(if (eq :timeout (nth-value 2 (nlk:run-bounded (nlk:shell-argv "sleep 30") :seconds 1))) 1 0)' 'run-bounded timeout'
expect 1 '(multiple-value-bind (out err status) (nlk:run-bounded (nlk:session-argv (nlk:shell-argv "echo led; exit 4")) :seconds 20) (declare (ignore err)) (if (and (search "led" out) (eql status 4)) 1 0))' 'a session of its own answers its status'
expect 1 '(labels ((deep (n) (if (zerop n) (list (nle::control-stack-usage)) (cons n (deep (1- n)))))) (multiple-value-bind (used span) (nle::control-stack-usage) (if (< 0 used (car (last (deep 2000))) (floor span 2)) 1 0)))' 'control stack usage grows with depth'
expect 0 '(handler-case (progn (nle::eval-runaway-thread-probe) 0) (nle::eval-runaway () 1))' 'a shallow evaluation takes no runaway verdict'
expect 0 '(multiple-value-bind (out err status) (nlk:run-bounded (nlk:session-argv (nlk:shell-argv "python3 -c '"'"'import os,sys; sys.exit(0 if os.getsid(0) == int(sys.argv[1]) else 3)'"'"' $$")) :seconds 20) (declare (ignore out err)) (if (integerp status) status 99))' 'the shell leads a session of its own'
expect 1 '(if (nlk:pid-live-p (sb-posix:getpid)) 1 0)' 'pid-live-p self'
expect 1 '(if (nlk:pid-live-p 4000000) 0 1)' 'pid-live-p dead'
lock="$run/probe.lock"
expect 1 "(let ((lock (nlk:lock-file \"$lock\"))) (if (and lock (nlk:file-locked-p \"$lock\") (progn (nlk:unlock-file lock) (not (nlk:file-locked-p \"$lock\")))) 1 0))" lock-file
log="$run/detached.log"
expect 1 "(progn (nlk:spawn-detached (list \"/bin/sh\" \"-c\" \"sleep 1; echo detached-marker\") \"$log\") (sleep 4) (if (search \"detached-marker\" (uiop:read-file-string \"$log\")) 1 0))" spawn-detached
from="$run/from.txt"; to="$run/to.txt"
echo new > "$from"; echo old > "$to"
expect 1 "(progn (nlk:replace-file \"$from\" \"$to\") (if (and (search \"new\" (uiop:read-file-string \"$to\")) (not (probe-file \"$from\"))) 1 0))" replace-file
expect 1 '(if nlk:*source-root* 1 0)' 'the source beside the binary is found'
linked="$(printf '%s\n' '(if (and nlk:*source-root* (nlk:runtime-directory)) 1 0)' | NODECODE_HOME="$HOME_SCRATCH" "$run/bin/nodecode" trial 2>&1 || true)"
printf '%s\n' "$linked" | grep -q 'nodecode-trial-score 1' || { echo "$linked"; echo 'FAIL: the source is not found through the link' >&2; exit 1; }
echo 'ok: the source is found through the link'
expect 1 '(if (plusp (length (nlk::shipped-cells))) 1 0)' 'shipped cells travel'

step 'a headless serve'
store="$run/smoke/history.db"
mkdir -p "$(dirname "$store")"
slog="$run/serve.log"
NODECODE_HOME="$run/serve-home" "$dir/nodecode" serve --store "$store" >"$slog" 2>&1 &
pid=$!
port=''
for i in $(seq 1 120); do
  sleep 0.5
  kill -0 "$pid" 2>/dev/null || break
  if [ -f "$store.owner" ]; then
    port="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1])).get("port") or "")' "$store.owner" 2>/dev/null || true)"
    [ -n "$port" ] && break
  fi
done
kill -0 "$pid" 2>/dev/null || { tail -n 40 "$slog"; echo 'mac: serve exited before publishing a port' >&2; exit 1; }
[ -n "$port" ] || { tail -n 40 "$slog"; kill "$pid"; echo 'mac: serve never published its port' >&2; exit 1; }
echo "serve published port $port"
kill "$pid"
sleep 2
if kill -0 "$pid" 2>/dev/null; then kill -9 "$pid"; echo 'mac: serve did not stop on SIGTERM' >&2; exit 1; fi
echo 'serve stopped'

# The release feed over real TLS against a host that goes silent and never
# closes: the gateway must leave it and act on the next label.
step 'the release feed leaves a silent connection'
OPENSSL=/opt/homebrew/opt/openssl@3/bin/openssl python3 src/tools/feed-proof.py "$dir/nodecode"

sha="$(shasum -a 256 "dist/$name.tar.gz" | cut -d' ' -f1)"
printf '\nmacos-release-ok %s minos %s sha256 %s (%ss)\n' "$label" "$minos" "$sha" "$SECONDS"
