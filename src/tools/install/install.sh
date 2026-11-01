#!/bin/sh
# install.sh --- the Linux and macOS one-liner.
#
# SPDX-License-Identifier: MIT
#
#   curl -fsSL https://nodecode.ai/install.sh | sh
#
# Fetches the release tarball for this machine — x86_64 Linux, Apple Silicon
# macOS — checks it against the sha256
# the release carries (no sidecar, no install), and lays it out under ~/.local/share/nodecode/<version>/
# — the executable and src/, which must stay together: SOURCE-ROOT resolves
# <runtime-directory>/src/src/, so that directory is the organism's own view of
# itself, not an install artifact. A stable `current` symlink points at the
# version and ~/.local/bin/nodecode points at that, so an upgrade is one
# symlink move and a running organism keeps the files it started with.
#
# Which release that is is not written into this file. The channel pointer
# <base>/<channel>-<arch>-<os> is the one copy of that fact, written by the
# publisher in the same breath as the bytes it names, and it is the same file
# a running organism's own update check reads (update.lisp) — so an install
# and a self-update can never name different builds. A pin here could only
# ever be moved by hand, and drifted a day behind the release it named.
#
# The preflight is not decoration. Every check here is a failure someone
# actually hit: glibc below 2.34 cannot run the image at all, and a missing CA
# bundle makes the setup wizard's provider list come up EMPTY WITH NO ERROR,
# which reads as a broken build. On a Mac the one hard gate is Homebrew's
# OpenSSL 3: the image loads it by that path at every start, and without it
# the executable does not answer at all.
#
#   NODECODE_VERSION   install this version instead of the one the channel names
#   NODECODE_CHANNEL   the channel whose pointer to read (default: stable)
#   NODECODE_BASE_URL  fetch from here instead of the release host: https://,
#                      or a folder as file://
#   NODECODE_HOME      install here instead of ~/.local/share/nodecode
#   NODECODE_STAGE     full (the default), stage or link -- see below
#   --no-modify-path   leave ~/.local/bin alone and print what to add
#   --stage            lay the version out beside the others and stop:
#                      `current' does not move, nothing restarts. What a
#                      running organism does before it classifies an
#                      upgrade (`nodecode update').
#   --link             point `current' at a version laid out earlier and
#                      stop: no download, no restart.
#   --force            download and lay the version out again even when it is
#                      already laid out and answers (NODECODE_FORCE=1 too)
set -eu

BASE_URL="${NODECODE_BASE_URL:-https://nodecode.ai/dl}"
HOME_DIR="${NODECODE_HOME:-$HOME/.local/share/nodecode}"
BIN_DIR="$HOME/.local/bin"
MODIFY_PATH=1
STAGE="${NODECODE_STAGE:-full}"
FORCE=0
case "${NODECODE_FORCE:-}" in ''|0) ;; *) FORCE=1 ;; esac

for arg in "$@"; do
  case "$arg" in
    --no-modify-path) MODIFY_PATH=0 ;;
    --stage) STAGE=stage ;;
    --link) STAGE=link ;;
    --force) FORCE=1 ;;
    -h|--help) sed -n '3,25p' "$0" 2>/dev/null || true; exit 0 ;;
    *) echo "install.sh: unknown argument $arg" >&2; exit 2 ;;
  esac
done

say()  { printf 'nodecode: %s\n' "$1"; }
die()  { printf 'nodecode: %s\n' "$1" >&2; exit 1; }
have() { command -v "$1" >/dev/null 2>&1; }

case "$STAGE" in
  full|stage|link) ;;
  *) die "NODECODE_STAGE is full, stage or link (got $STAGE)" ;;
esac

# The release host is https:// or a folder (file://), and nothing else: the
# rule the organism's own update check holds too (update.lisp,
# RELEASE-HOST-P). Over plain http anyone on the path could hand over another
# pointer, another tarball and the sha256 sidecar that matches it.
case "$BASE_URL" in
  https://*|file://*) ;;
  *) die "NODECODE_BASE_URL is https:// or file:// (got $BASE_URL): over plain http anyone on the path could swap the release" ;;
esac

# --- what this machine is ---------------------------------------------------

os="$(uname -s)"
arch="$(uname -m)"
case "$os" in
  Linux)
    platform=linux
    case "$arch" in
      x86_64|amd64) arch=x86_64 ;;
      *) die "no Linux build for $arch yet — this release is x86_64 only" ;;
    esac
    ;;
  Darwin)
    platform=darwin
    case "$arch" in
      arm64) ;;
      x86_64) die "no macOS build for Intel yet — this release is Apple Silicon only" ;;
      *) die "no build for $arch" ;;
    esac
    ;;
  *) die "this build runs on Linux and macOS (found $os)" ;;
esac

if [ "$platform" = linux ]; then
# glibc is the hard gate: the image imports symbols versioned up to
# GLIBC_2.34 (pack.sh refuses a build that needs more), so anything older
# cannot start at all and there is no workaround short of a newer distro.
# The LAST field of ldd's first line is the bare version. Reaching for the
# first number-shaped word instead finds the distro's package version —
# `ldd (Ubuntu GLIBC 2.35-0ubuntu3.11) 2.35' hands back `2.35-0ubuntu3.11)',
# whose minor part is `35-0ubuntu3', which is not an integer, so the -lt test
# errors. Silence that error and the gate reads as PASS: 22.04 downloaded the
# whole tarball and left a broken install before this was caught.
glibc="$(ldd --version 2>/dev/null | head -1 | awk '{print $NF}')"
major="$(printf '%s' "${glibc%%.*}" | tr -cd '0-9')"
minor="$(printf '%s' "${glibc#*.}" | tr -cd '0-9' | cut -c1-2)"
if [ -n "$major" ] && [ -n "$minor" ]; then
  if [ "$major" -lt 2 ] || { [ "$major" -eq 2 ] && [ "$minor" -lt 34 ]; }; then
    die "glibc $glibc is too old — nodecode needs 2.34 or newer.
  Ubuntu 22.04+, Debian 12+, RHEL/Alma/Rocky 9+, Amazon Linux 2023, Fedora 35+
  and Arch are fine; Ubuntu 20.04, Debian 11 and RHEL 8 are not, and no flag
  here can change that."
  fi
else
  say "WARNING: could not read a glibc version from ldd; 2.34 or newer is required"
fi
fi

if [ "$platform" = darwin ]; then
  # The macOS build is made on macOS 15 and the executable carries it as its
  # floor (LC_BUILD_VERSION minos 15.0): below that the loader refuses it.
  macos="$(sw_vers -productVersion 2>/dev/null || true)"
  macos_major="$(printf '%s' "${macos%%.*}" | tr -cd '0-9')"
  if [ -n "$macos_major" ] && [ "$macos_major" -lt 15 ]; then
    die "macOS $macos is too old — nodecode needs 15 (Sequoia) or newer"
  fi
fi

have curl || have wget || die "need curl or wget to download"
have tar || die "need tar to unpack"

# --- what it will need to actually work -------------------------------------

if [ "$platform" = linux ]; then
missing=''
for lib in libsqlite3.so.0 libssl.so.3 libcrypto.so.3; do
  if have ldconfig; then
    ldconfig -p 2>/dev/null | grep -q "$lib" || missing="$missing $lib"
  fi
done
[ -n "$missing" ] && say "WARNING: not found:$missing — install sqlite3 and openssl 3"

# A missing CA bundle is the quiet one: the provider catalog is fetched over
# HTTPS, and without it the setup wizard shows an empty list and says nothing.
ca=0
for f in /etc/ssl/certs/ca-certificates.crt /etc/pki/tls/certs/ca-bundle.crt \
         /etc/ssl/ca-bundle.pem /etc/ssl/cert.pem; do
  [ -e "$f" ] && ca=1 && break
done
[ "$ca" -eq 0 ] && say "WARNING: no CA bundle found — install ca-certificates, or the
  provider list on first run will be EMPTY with no error on screen."
fi

if [ "$platform" = darwin ]; then
  # The executable opens two libraries by Homebrew's path at every start:
  # OpenSSL 3, which cl+ssl reopens by the path it had on the box that built
  # the image, and zstd, which the SBCL runtime itself links (its core
  # compression). Either one missing and dyld aborts before a line of Lisp
  # runs, so openssl@3 alone -- which does not pull zstd in -- was a Mac the
  # installer passed and the executable never answered on. sqlite3 is the
  # system's own. The CA bundle comes with openssl@3 (its ca-certificates
  # dependency), so the quiet failure Linux has is not this platform's —
  # unless the bundle is gone. Everything missing is named in one command,
  # and Homebrew itself when a fresh Mac has none.
  formulae=''
  [ -e /opt/homebrew/opt/openssl/lib/libssl.dylib ] || formulae="$formulae openssl@3"
  [ -e /opt/homebrew/opt/zstd/lib/libzstd.1.dylib ] || formulae="$formulae zstd"
  if [ -n "$formulae" ] && [ -x /opt/homebrew/bin/brew ]; then
    die "nodecode needs Homebrew's$formulae —
  brew install$formulae
  then run this installer again."
  elif [ -n "$formulae" ]; then
    die "nodecode needs Homebrew (https://brew.sh) and its$formulae —
  /bin/bash -c \"\$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\"
  /opt/homebrew/bin/brew install$formulae
  then run this installer again."
  fi
  [ -e /opt/homebrew/etc/openssl@3/cert.pem ] || say "WARNING: no CA bundle at /opt/homebrew/etc/openssl@3/cert.pem —
  brew reinstall ca-certificates, or the provider list on first run will be
  EMPTY with no error on screen."
fi

have git || say "WARNING: git is not installed — nodecode commits its layers,
  its memory and its checkpoint history through it.$([ "$platform" = darwin ] && printf '\n  xcode-select --install puts it on a Mac.')"

# --- which release -----------------------------------------------------------

# NODECODE_VERSION is the explicit override: `nodecode update' stages and links
# by label, and a human re-installing a known build says so here. Anything else
# asks the channel.
VERSION="${NODECODE_VERSION:-}"
# A label becomes a folder under $HOME_DIR that a reinstall replaces whole, so
# `.' or `..' would name the install root or its parent: a label is 1 to 63
# of letters, digits and ._+-, never starting with a dot -- update.lisp's
# RELEASE-LABEL-P, the rule a label the organism stages already passed.
if [ -n "$VERSION" ]; then
  case "$VERSION" in
    .*|*[!A-Za-z0-9._+-]*) die "NODECODE_VERSION is not a release label: $VERSION" ;;
  esac
  [ "${#VERSION}" -lt 64 ] || die "NODECODE_VERSION is not a release label: $VERSION"
fi
if [ -z "$VERSION" ]; then
  if [ "$STAGE" = link ]; then
    die "--link needs the version to point at: set NODECODE_VERSION"
  fi
  CHANNEL="${NODECODE_CHANNEL:-stable}"
  pointer_url="$BASE_URL/$CHANNEL-$arch-$platform"
  # Read past any HTTP cache: the release host's edge answers /dl from cache
  # for hours and rewrites the cache-control it was given, so a plain read can
  # be an answer from before this file existed — or a label that has moved.
  # The pointer is a live fact; a stranger's first install must see the build
  # that is current now.
  pointer_read="$pointer_url"
  case "$BASE_URL" in
    https://*) pointer_read="$pointer_url?cb=$(date +%s)" ;;
  esac
  say "reading $pointer_url"
  if have curl; then
    VERSION="$(curl -fsSL "$pointer_read" 2>/dev/null | tr -d '[:space:]')"
  else
    VERSION="$(wget -qO- "$pointer_read" 2>/dev/null | tr -d '[:space:]')"
  fi
  [ -n "$VERSION" ] || die "no $CHANNEL release is published for $arch-$platform yet —
  $pointer_url did not answer"
  # (the read itself carries ?cb= on http(s): a cached answer is the one way
  # this can be wrong about a release that is already published)
  # A label is a short commit id. Without this check a host answering with an
  # error page would be read as a release name.
  case "$VERSION" in
    *[!0-9a-f]*) die "$pointer_url did not name a release: $VERSION" ;;
  esac
  [ "${#VERSION}" -ge 7 ] || die "$pointer_url did not name a release: $VERSION"
fi
say "release $VERSION"

# --- fetch ------------------------------------------------------------------

name="nodecode-${VERSION}-${arch}-${platform}"
url="${BASE_URL}/${name}.tar.gz"
work="$(mktemp -d)"
# Two installs into one home -- a second curl | sh, or `nodecode update'
# beside a manual install -- take turns from the lay-out to the last step:
# one replacing the folder or the links while the other probes them failed
# both. The lock is a directory (mkdir is atomic everywhere) holding its
# owner's pid, so a lock whose owner died is taken over, never waited on.
lock="$HOME_DIR/.install-lock"
locked=0
trap 'rm -rf "$work"; [ "$locked" = 1 ] && rm -rf "$lock"; :' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
take_lock() {
  mkdir -p "$HOME_DIR"
  waited=0
  until mkdir "$lock" 2>/dev/null; do
    holder="$(cat "$lock/pid" 2>/dev/null || true)"
    if { [ -n "$holder" ] && ! kill -0 "$holder" 2>/dev/null; } ||
       { [ -z "$holder" ] && [ "$waited" -ge 10 ]; }; then
      rm -rf "$lock"; continue
    fi
    [ "$waited" -eq 0 ] && say "waiting for another install into $HOME_DIR (pid ${holder:-starting})"
    waited=$((waited + 1))
    [ "$waited" -le 600 ] || die "another install still holds $lock (pid ${holder:-unknown}); remove it if none runs"
    sleep 1
  done
  locked=1
  printf '%s\n' "$$" > "$lock/pid"
}
target="$HOME_DIR/$VERSION"

# A label names one build, so a release already laid out here -- its
# executable answering beside its src/ -- is that build: installing it again
# re-downloaded 90 MB and replaced the folder a running organism may hold.
# It is kept and only the links move. One that does not answer (a half-removed
# folder, a broken copy) is laid out again; --force asks for that regardless.
answers() {
  case "$("$1" --help 2>&1 || true)" in *'usage: nodecode'*) return 0 ;; esac
  return 1
}
laid_out() { [ -x "$target/nodecode" ] && [ -d "$target/src" ] && answers "$target/nodecode"; }
reuse=0
if [ "$STAGE" != link ] && [ "$FORCE" = 0 ] && laid_out; then
  reuse=1
  say "$VERSION is already laid out at $target; keeping it (--force downloads it again)"
fi

if [ "$STAGE" != link ] && [ "$reuse" = 0 ]; then

# coreutils on Linux, perl's shasum on macOS; both print `<hash>  <name>'.
# A download is installed only against the sha256 its release published, so
# a machine that cannot compute one stops here, before the download.
if have sha256sum; then
  sha256() { sha256sum "$1"; }
elif have shasum; then
  sha256() { shasum -a 256 "$1"; }
else
  die "neither sha256sum nor shasum is installed, so the download cannot be checked"
fi

say "downloading $url"
nosum="no sha256 is published for $url: a release nothing vouches for is not installed"
if have curl; then
  curl -fsSL "$url" -o "$work/$name.tar.gz" || die "download failed: $url"
  curl -fsSL "$url.sha256" -o "$work/$name.tar.gz.sha256" 2>/dev/null || die "$nosum"
else
  wget -q "$url" -O "$work/$name.tar.gz" || die "download failed: $url"
  wget -q "$url.sha256" -O "$work/$name.tar.gz.sha256" 2>/dev/null || die "$nosum"
fi
[ -s "$work/$name.tar.gz.sha256" ] || die "$nosum"
want="$(cut -d' ' -f1 < "$work/$name.tar.gz.sha256")"
got="$(sha256 "$work/$name.tar.gz" | cut -d' ' -f1)" || die "could not compute the sha256 of $name.tar.gz"
[ -n "$got" ] && [ "$want" = "$got" ] || die "sha256 mismatch: expected $want, got $got"
say "sha256 verified"

# --- lay it out -------------------------------------------------------------

take_lock
# An install that held the turn before this one may have laid the same
# release out while this one downloaded it: that folder is kept, not replaced.
if [ "$FORCE" = 0 ] && laid_out; then
  say "$VERSION was laid out at $target meanwhile; keeping it"
else
tar -xzf "$work/$name.tar.gz" -C "$work"
[ -x "$work/$name/nodecode" ] || die "the archive did not carry an executable"

mkdir -p "$HOME_DIR"
# A new version lands beside the old rather than over it, so an organism that
# is running right now keeps every file it started with.
rm -rf "$target.new"
mv "$work/$name" "$target.new"
rm -rf "$target"
mv "$target.new" "$target"
say "laid out $VERSION at $target"
fi

fi
[ "$locked" = 1 ] || take_lock

# The probe is an argument the CLI rejects: it answers any unknown one with its
# usage, which is proof the executable and its libraries load. A staged
# version is probed where it lies; nothing points at it yet. One that does not
# answer is reported by its own first line -- dyld's `Library not loaded:
# <path>' names exactly what is missing -- on the one line `nodecode update'
# reads back as the reason.
probe() {
  answer="$("$1" --help 2>&1)" || true
  case "$answer" in *'usage: nodecode'*) return 0 ;; esac
  first="$(printf '%s\n' "$answer" | awk 'NF { print; exit }')"
  die "$2 did not answer: ${first:-no output}"
}
if [ "$STAGE" = stage ]; then
  probe "$target/nodecode" "the staged executable"
  say "staged $VERSION at $target"
  exit 0
fi

[ -x "$target/nodecode" ] || die "$VERSION is not laid out at $target; run without --link first"
# The host a whole install came from is the one its updates read (update.lisp):
# a build from a mirror or a folder otherwise moved itself to the project's
# release. The organism's own stage and link pass the host they read, so only
# a whole install writes it.
if [ "$STAGE" = full ]; then
  if [ -n "${NODECODE_BASE_URL:-}" ]; then printf '%s\n' "$BASE_URL" > "$HOME_DIR/base-url"; else rm -f "$HOME_DIR/base-url"; fi
fi
ln -sfn "$VERSION" "$HOME_DIR/current"
say "current is $VERSION"

path_step=''
if [ "$MODIFY_PATH" -eq 1 ]; then
  mkdir -p "$BIN_DIR"
  ln -sfn "$HOME_DIR/current/nodecode" "$BIN_DIR/nodecode"
  say "linked $BIN_DIR/nodecode"
  # zsh is the Mac's login shell, and reads ~/.zprofile, never ~/.profile.
  [ "$platform" = darwin ] && profile=~/.zprofile || profile=~/.profile
  # One PATH step, said once at the end: a new shell alone when its profile
  # already adds ~/.local/bin once the folder exists (Debian's and Ubuntu's
  # do), the line to add first when it does not. A login shell with a bare
  # environment is what a new terminal reads; `timeout' keeps a profile that
  # waits from holding it.
  case ":$PATH:" in
    *":$BIN_DIR:"*) ;;
    *) limit=''; have timeout && limit='timeout 5'
       case "$($limit env -i HOME="$HOME" "${SHELL:-/bin/sh}" -lc 'printf ":%s:" "$PATH"' </dev/null 2>/dev/null || true)" in
         *":$BIN_DIR:"*) path_step='open a new shell, then' ;;
         *) path_step="add $BIN_DIR to your PATH:
  echo 'export PATH=\"\$HOME/.local/bin:\$PATH\"' >> $profile
then open a new shell and" ;;
       esac ;;
  esac
else
  say "run it at $HOME_DIR/current/nodecode"
fi

probe "$HOME_DIR/current/nodecode" "the executable"
say "the executable runs"

# A link is the organism's own move (`nodecode update'): it restarts itself
# when it must, at an idle moment, and tells its shells.
[ "$STAGE" = link ] && exit 0

# A background gateway (`nodecode gateway on') runs the build it started with.
# Its unit names the `current' link, so a restart lands on this one. Only a
# unit nodecode wrote — its first line says so — and only one that is running:
# a stopped gateway starts on the new build whenever it next starts. Rewriting
# an active unit also upgrades older Type=simple units to the watchdog contract
# before the restart below.
units="${XDG_CONFIG_HOME:-$HOME/.config}/systemd/user"
if have systemctl && [ -d "$units" ]; then
  for unit in "$units"/nodecode-gateway*.service; do
    [ -f "$unit" ] || continue
    head -n 1 "$unit" | grep -q '^# nodecode gateway: ' || continue
    unit_name="$(basename "$unit")"
    if systemctl --user is-active --quiet "$unit_name" 2>/dev/null; then
      unit_home="$(sed -n 's/^# nodecode gateway: //p' "$unit" | head -n 1)"
      unit_home="${unit_home%/}"
      unit_user_home="$(sed -n 's/^Environment="HOME=\(.*\)"$/\1/p' "$unit" | head -n 1)"
      unit_user_home="${unit_user_home:-$HOME}"
      # The unit's folder is authoritative even when the old unit did not
      # carry XDG_CONFIG_HOME. Passing the config root makes gateway restart
      # find and rewrite this unit instead of creating a second one under the
      # installer's current environment.
      unit_config_home="$(dirname "$(dirname "$units")")"
      say "refreshing active gateway $unit_name"
      unit_cache_home="$(sed -n 's/^Environment="XDG_CACHE_HOME=\(.*\)"$/\1/p' "$unit" | head -n 1)"
      unit_data_home="$(sed -n 's/^Environment="XDG_DATA_HOME=\(.*\)"$/\1/p' "$unit" | head -n 1)"
      unit_path="$(sed -n 's/^Environment="PATH=\(.*\)"$/\1/p' "$unit" | head -n 1)"
      unit_path="${unit_path:-$PATH}"
      restart_gateway() {
        NODECODE_HOME="$unit_home" HOME="$unit_user_home" \
          XDG_CONFIG_HOME="$unit_config_home" \
          XDG_CACHE_HOME="$unit_cache_home" XDG_DATA_HOME="$unit_data_home" \
          PATH="$unit_path" \
          "$HOME_DIR/current/nodecode" gateway restart >/dev/null 2>&1
      }
      if restart_gateway; then
        say "refreshed and restarted $unit_name on $VERSION"
      else
        die "could not refresh active gateway $unit_name with the watchdog unit; run '$HOME_DIR/current/nodecode gateway restart'"
      fi
    fi
  done
fi

# The same for a gateway under launchd: an agent nodecode wrote — the marker
# on its second line, after the XML declaration — that launchd reports as
# running is restarted on this build through the same verb, which rewrites
# the agent and bootstraps it again.
agents="$HOME/Library/LaunchAgents"
if [ "$platform" = darwin ] && have launchctl && [ -d "$agents" ]; then
  for agent in "$agents"/nodecode-gateway*.plist; do
    [ -f "$agent" ] || continue
    sed -n '2p' "$agent" | grep -q '^<!-- nodecode gateway: ' || continue
    label="$(basename "$agent" .plist)"
    if launchctl print "gui/$(id -u)/$label" 2>/dev/null | grep -q 'state = running'; then
      unit_home="$(sed -n '2s/^<!-- nodecode gateway: \(.*\) -->$/\1/p' "$agent")"
      unit_home="${unit_home%/}"
      unit_user_home="$(sed -n 's/.*<key>HOME<\/key><string>\(.*\)<\/string>.*/\1/p' "$agent" | head -n 1)"
      unit_user_home="${unit_user_home:-$HOME}"
      unit_path="$(sed -n 's/.*<key>PATH<\/key><string>\(.*\)<\/string>.*/\1/p' "$agent" | head -n 1)"
      unit_path="${unit_path:-$PATH}"
      if NODECODE_HOME="$unit_home" HOME="$unit_user_home" PATH="$unit_path" \
           "$HOME_DIR/current/nodecode" gateway restart >/dev/null 2>&1; then
        say "restarted $label on $VERSION"
      else
        say "WARNING: could not restart $label — 'nodecode gateway restart' moves it to $VERSION"
      fi
    fi
  done
fi

# Proving the absolute path runs proves nothing about the name the operator
# will type. An earlier `nodecode' on PATH — an older install under another
# name, a system copy — shadows the link laid down above, and the install
# reads as a success right up until the first verb the old build never had:
#   $ nodecode import codex
#   nodecode-lisp: unknown argument "import"
# Report it here, where the two paths are still in hand, rather than leaving
# it to be diagnosed from a usage line.
resolved="$(command -v nodecode 2>/dev/null || true)"
if [ -n "$resolved" ]; then
  installed="$HOME_DIR/current/nodecode"
  # Compare what each name actually resolves to, so the symlink chain and any
  # /usr/local copy are judged by their target, not their spelling.
  real_resolved="$(readlink -f "$resolved" 2>/dev/null || printf '%s' "$resolved")"
  real_installed="$(readlink -f "$installed" 2>/dev/null || printf '%s' "$installed")"
  if [ "$real_resolved" != "$real_installed" ]; then
    say "WARNING: 'nodecode' on your PATH is NOT the build just installed.
  it runs:       $resolved
  this installed: $installed
  Remove the other one, or put $BIN_DIR ahead of it in PATH.
  Until then run the full path above."
  fi
fi

# Cell folders written before the rename declare :depends-on ("nodecode-lisp")
# and :class nodecode-lisp.kernel:layer — a system and a package this image does
# not provide. Every one of them fails at boot with
#   cell <name>: FAILED Component "nodecode-lisp" not found
# which names the missing component but not the reason, and an operator whose
# whole layer went dark reads it as the install being broken. The install is
# fine; the folders are older than it. Say so here, by name, once.
cells="$HOME/.nodecode/cells"
if [ -d "$cells" ]; then
  stale=''
  dangling=''
  for entry in "$cells"/*; do
    [ -e "$entry" ] || [ -L "$entry" ] || continue
    name="$(basename "$entry")"
    if [ ! -e "$entry" ]; then
      dangling="$dangling $name"
    elif grep -qs 'nodecode-lisp' "$entry"/*.asd 2>/dev/null; then
      stale="$stale $name"
    fi
  done
  if [ -n "$stale" ]; then
    say "WARNING: these cells predate the rename and will not load —
 $stale
  Their .asd names the system 'nodecode-lisp', which this build does not
  provide. A shipped cell is fixed by relinking it at this install:
    ln -sfn $HOME_DIR/current/src/cells/<folder> $cells/<name>
  Your OWN layer is fixed in place, and keeps every definition:
    sed -i 's/nodecode-lisp\\./nodecode./g; s/\"nodecode-lisp\"/\"nodecode\"/g' \\
      $cells/<layer>/*.lisp $cells/<layer>/*.asd
  Copy the folder first if you want a way back."
  fi
  [ -n "$dangling" ] && say "WARNING: these cell links point at nothing:$dangling"
fi

say "${path_step:+$path_step }run 'nodecode' to start; the first launch opens the setup wizard"
