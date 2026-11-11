#!/usr/bin/env bash
# publish.sh --- build one committed release for Linux, Windows and macOS and
# publish it to nodecode.ai/dl under one label.
#
# Three archives, one commit, no CI. The Linux tarball is packed by pack.sh,
# the Windows zip in the Windows sandbox on the build host (pack-windows.sh,
# NODECODE_WINBOX_HOST), the Apple Silicon tarball on a Mac over ssh
# (pack-macos.sh, NODECODE_MACOS_HOST), and this script drives all three: SBCL cannot
# cross-compile, so the arms are the machines that can build them, and each
# builder stays the authority for what its release carries. This arm transports
# the archives and sidecars, proves the bytes the public endpoints serve, and
# only then writes the channel pointers -- all to the same label, or none. No
# version is written into any file: a client reads the pointer named for its
# own platform, and one publish writing every pointer together is what keeps
# them one fact. Without writing every pointer together, one platform pointer
# could advance while the others still name the previous commit.
set -euo pipefail

REV="${1:-HEAD}"
OUT="${2:-dist}"

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/../.." && pwd)"
. "$here/layout.sh"
cd "$repo"

commit="$(git rev-parse "$REV^{commit}")"
# NINE hex digits, explicitly, and the same nine everywhere: --short alone
# sizes itself by the repository's object count, which differs between this
# box and a build tree cut far from it (PACK.SH carries the full note).
label="$(git rev-parse --short=9 "$commit")"
# The Linux arm is packed here, and pack.sh names what it builds after uname.
if [ "$(uname -s)" != Linux ] || [ "$(uname -m)" != x86_64 ]; then
  echo "publish: the release box packs the x86_64 Linux arm; this is $(uname -s) $(uname -m)" >&2
  exit 1
fi

# The channel whose pointers this publish moves. stable is what every install
# and every home follows unless its config says otherwise: a version on
# GitHub's releases page, the installers mirrored. nightly is main's newest
# green build, for the homes whose `update.channel' names it: no tag, no
# GitHub release, and the installers stay the stable release's. A stable
# publish also moves each nightly pointer naming a commit it carries, so the
# nightly is never behind stable (the pointers, below).
channel="${NODECODE_PUBLISH_CHANNEL:-stable}"
case "$channel" in
  stable|nightly) ;;
  *) echo "publish: the channel is stable or nightly, not '$channel'" >&2; exit 2 ;;
esac

# Every platform rides every release. Leaving one out is said, never assumed
# (platforms.sh): NODECODE_PUBLISH_SKIP_MACOS publishes without the Mac's arm,
# NODECODE_PUBLISH_SKIP_WINDOWS holds Windows on the release it runs. A
# platform left out packs nothing, and every pointer of it keeps naming the
# label it names.
. "$here/platforms.sh"
platforms="$(release_platforms)"
held="$(release_held)"
for platform in $held; do
  echo "publish: $(release_hold_setting "$platform") is set: no $(release_platform_name "$platform") arm, and every $platform pointer keeps the label it names" >&2
done
has_platform() { case " $platforms " in *" $1 "*) return 0 ;; *) return 1 ;; esac; }
suffix_of() { case "$1" in *-windows) echo .zip ;; *) echo .tar.gz ;; esac; }
# An arm may ship a second container under the same label. Windows carries
# tar.zst beside its zip: the same tree, and what the installer asks for first
# (22.6 MB against 30.0, and bsdtar unpacks it straight into the release
# folder). Both are published, proved and refused-republished alike.
extra_of() { case "$1" in *-windows) echo "$OUT/nodecode-$label-$1.tar.zst" ;; *) echo '' ;; esac; }
archive_of() { echo "$OUT/nodecode-$label-$1$(suffix_of "$1")"; }

# The deploy host, its layout and the commit helper are this machine's
# (publish-config.sh), never a default here.
. "$(dirname "${BASH_SOURCE[0]}")/publish-config.sh"
host="$(publish_setting NODECODE_PUBLISH_HOST)" || exit 1
remote_dir="$(publish_setting NODECODE_PUBLISH_REMOTE_DIR)" || exit 1
app_dir="$(publish_setting NODECODE_PUBLISH_APP_DIR)" || exit 1
compose_file="$(publish_setting NODECODE_PUBLISH_COMPOSE_FILE)" || exit 1
env_file="$(publish_setting NODECODE_PUBLISH_ENV_FILE)" || exit 1
base_url="${NODECODE_PUBLISH_BASE_URL:-https://nodecode.ai}"
base_url="${base_url%/}"

# Every stable release is a version on GitHub's releases page: the base that
# src/nodecode.asd declares at the commit, tagged v<base> at that commit, with
# its notes and archives (the block at the end). A version is released once, so
# a version already tagged at another commit stops the publish here, before any
# arm packs: bump :version in src/nodecode.asd first.
asd="$(layout_asd_at "$commit")"
version="$(git show "$commit:$asd" | sed -n 's/^ *:version "\([0-9][0-9A-Za-z.-]*\)".*/\1/p' | head -n 1)"
[ -n "$version" ] || { echo "publish: $asd at $label declares no :version" >&2; exit 1; }
tag="v$version"
if [ "$channel" = stable ]; then
  github_repo="$(publish_setting NODECODE_PUBLISH_GITHUB_REPO)" || exit 1
  gh api "repos/$github_repo" --jq .full_name > /dev/null || {
    echo "publish: GitHub does not answer for $github_repo (gh auth status), so $tag could not be listed. Nothing moves." >&2
    exit 1
  }
  # A miss is a nonzero exit, and gh prints GitHub's error body on stdout with
  # it, so only an answer that succeeded names a commit.
  tagged="$(gh api "repos/$github_repo/commits/$tag" --jq .sha 2> /dev/null)" || tagged=""
  if [ -n "$tagged" ] && [ "$tagged" != "$commit" ]; then
    echo "publish: $tag is already released at ${tagged:0:9}; bump :version in $asd for $label. Nothing moves." >&2
    exit 1
  fi
  echo "publish: $label is nodecode $version, tagged $tag"
else
  echo "publish: $label is nodecode $version, for the $channel channel: no tag, no GitHub release, the installers stay"
fi
# The site builds its image from this branch of the repository the local
# clone tracks; the installers are mirrored there, never into the clone.
# NODECODE_PUBLISH_SITE_REMOTE names that repository directly, for a publisher
# with no clone of the site (the Release workflow); else the clone's origin.
# Only a stable publish mirrors them: an install reads the stable pointer by
# default, so the installers it runs are the stable release's.
if [ "$channel" = stable ]; then
  authority_installer="${NODECODE_PUBLISH_INSTALLER_PATH:-$(layout_system "$repo")/tools/install/install.sh}"
  # The Windows arm is served beside the shell one and reads the same pointer,
  # so it follows the same authority/mirror discipline. Windows held back
  # keeps the one the release it runs came with: none is named here, and the
  # site's stays (mirror-installers.sh).
  authority_ps1=''
  has_platform x86_64-windows \
    && authority_ps1="${NODECODE_PUBLISH_PS1_PATH:-$(layout_system "$repo")/tools/install/install.ps1}"
  site_remote="$(publish_setting NODECODE_PUBLISH_SITE_REMOTE 2> /dev/null)" || site_remote=''
  if [ -z "$site_remote" ]; then
    marketplace="$(publish_setting NODECODE_PUBLISH_MARKETPLACE_DIR)" || exit 1
    site_remote="$(git -C "$marketplace" remote get-url origin 2>/dev/null)" || {
      echo "publish: $marketplace has no origin to push the installers to" >&2
      exit 1
    }
  fi
  site_branch="${NODECODE_PUBLISH_SITE_BRANCH:-main}"
  web_container="$(publish_setting NODECODE_PUBLISH_WEB_CONTAINER)" || exit 1
  [ -f "$authority_installer" ] || { echo "publish: missing authority installer $authority_installer" >&2; exit 1; }
  [ -z "$authority_ps1" ] || [ -f "$authority_ps1" ] || { echo "publish: missing authority installer $authority_ps1" >&2; exit 1; }
fi

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT INT TERM
mkdir -p "$OUT"

# An archive already in OUT under this label is the release. A second build
# would put a second body under a name the edge may already have cached, and
# the proof below would then fail against the cached first.
have() { test -s "$(archive_of "$1")" && test -s "$(archive_of "$1").sha256"; }

# Each arm is packed by the machine that can pack it: the Linux tarball here,
# the Mac tarball on the Mac (pack-macos.sh builds, smokes and proves the
# revision there over ssh), the Windows zip in the sandbox on the build host --
# pack-windows.sh starts the winbox container on NODECODE_WINBOX_HOST, builds
# and smokes the revision there -- each handing back an archive named for this
# label. One red arm fails the publish: the pointers below name one commit for every platform, so
# a platform that cannot take it leaves them all where they are rather than
# moving some alone.
have x86_64-linux || just tarball "$commit" "$OUT" || {
  echo "publish: the Linux arm did not pack $label; nothing moves." >&2
  exit 1
}
# The Mac before the sandbox: a Mac that does not answer fails in seconds,
# where the sandbox takes minutes to boot.
if has_platform arm64-darwin; then
  have arm64-darwin || just macos "$commit" "$OUT" || {
    echo "publish: the macOS arm did not pack $label; $OUT/macos-$label.log names the step. Nothing moves." >&2
    echo "publish: NODECODE_PUBLISH_SKIP_MACOS=1 publishes without it, leaving stable-arm64-darwin where it is." >&2
    exit 1
  }
fi
if has_platform x86_64-windows; then
  have x86_64-windows || just windows "$commit" "$OUT" || {
    echo "publish: the Windows arm did not pack $label; $OUT/windows-$label.log names the step. Nothing moves." >&2
    exit 1
  }
fi
# A zip already in OUT from before the tar.zst arm carries no tar.zst beside
# it: pack that out of the zip (every file of one hashed out of the other)
# rather than boot a Windows sandbox for a transport it already proved.
tar_zst="$OUT/nodecode-$label-x86_64-windows.tar.zst"
if has_platform x86_64-windows && [ ! -s "$tar_zst" ] && [ -s "$OUT/nodecode-$label-x86_64-windows.zip" ]; then
  "$(dirname "${BASH_SOURCE[0]}")/pack-windows-zst.sh" "$OUT/nodecode-$label-x86_64-windows.zip" || {
    echo "publish: the Windows tar.zst did not pack; nothing moves." >&2
    exit 1
  }
fi

# The restart's log is this run's own: a fixed /tmp name is a file another
# account's publish may own, and the release runner's could not write it.
restart_web() {
  ssh -o BatchMode=yes "$host" \
    "cd '$app_dir' && docker compose -f '$compose_file' --env-file '$env_file' restart web" \
    >"$work/restart.log" 2>&1 || {
      cat "$work/restart.log"
      return 1
    }
}

sha_of() {
  sha256sum "$1" | cut -d' ' -f1
}

# The container answers 502 until the app inside it has booted, so a public
# read taken right after a restart is expected to lose that race: retry it.
# Its counter is its own: without that, a caller's retry loop would be reset to
# one by every read and never reach its cap.
fetch() {
  local output="$1" attempt=1
  shift
  while :; do
    if curl -fsS -o "$output" "$@" 2>/dev/null; then
      return 0
    fi
    [ "$attempt" -lt 30 ] || break
    attempt=$((attempt + 1))
    sleep 2
  done
  curl -fsS -o "$output" "$@"
}

# What the release changed, for the note a box says when it first runs it
# (update.lisp RELEASE-CHANGES): the newest 200 commits up to this one,
# newest first, a line each -- the commit's nine-digit label and its head (the
# subject up to ` -- ') when it touched what ships, the label alone when it
# did not, so a box finds the release it ran before either way. One list for
# every platform, written here: this host has the history, a sandbox tree
# is one commit deep.
#
# The list reaches back at least to the stable release before this commit,
# however far that is: RELEASE-DIRECTION reads a move between channels off
# these lists, so a home switched from nightly back to stable knows stable is
# a step back -- and refuses it unasked -- only when stable's label is on the
# nightly's list.
changes="$OUT/changes-$label"
depth=200
previous="$(git describe --tags --abbrev=0 --match 'v[0-9]*' "$commit^" 2>/dev/null)" || previous=''
previous_label=''
if [ -n "$previous" ] && previous_commit="$(git rev-parse -q --verify "$previous^{commit}")"; then
  previous_label="${previous_commit:0:9}"
  since="$(git rev-list --count "$previous_commit..$commit")"
  [ "$since" -lt "$depth" ] || depth=$((since + 1))
fi
# What ships has two spellings (layout.sh): under src/ until the move put the
# system at the root -- the commit that added the root nodecode.asd -- and at
# the root since. Each stretch of history is read with its own, so the commits
# before the move keep their mark.
declare -A ships=()
move="$(git log --no-renames --diff-filter=A --format=%H -n 1 "$commit" -- nodecode.asd)"
ships_since() { # REV RANGE
  local specs; mapfile -t specs < <(layout_ships_at "$1")
  git log --format=%H -n "$depth" "$2" -- "${specs[@]}"
}
write_changes() {
  local hash subject
  ships=()
  while read -r hash; do ships[$hash]=1; done < <(
    if [ -n "$move" ]; then
      ships_since "$commit" "$move^..$commit"
      ships_since "$move^" "$move^"
    else
      ships_since "$commit" "$commit"
    fi)
  git log --format='%H %s' -n "$depth" "$commit" | while read -r hash subject; do
    if [ -n "${ships[$hash]:-}" ]; then
      printf '%s %s\n' "${hash:0:9}" "${subject%% -- *}"
    else
      printf '%s\n' "${hash:0:9}"
    fi
  done > "$changes"
}
write_changes
# The log walks by commit date, so a merged branch's older commits can sit
# between this one and the stable release; the list grows until it holds it.
while [ -n "$previous_label" ] && ! cut -d' ' -f1 "$changes" | grep -qxF "$previous_label"; do
  [ "$depth" -lt 20000 ] || { echo "publish: changes-$label never reaches $previous ($previous_label)" >&2; exit 1; }
  depth=$((depth * 2))
  write_changes
done
chmod 0644 "$changes"

uploads=("$changes")
for platform in $platforms; do
  archive="$(archive_of "$platform")"
  local_sha="$(sha_of "$archive")"
  sidecar_sha="$(cut -d' ' -f1 < "$archive.sha256")"
  [ "$local_sha" = "$sidecar_sha" ] || {
    echo "publish: $platform sidecar mismatch: $sidecar_sha != $local_sha" >&2
    exit 1
  }
  echo "publish: $platform local sha256 $local_sha"
  uploads+=("$archive" "$archive.sha256")
  extra="$(extra_of "$platform")"
  if [ -n "$extra" ]; then
    [ -s "$extra" ] && [ -s "$extra.sha256" ] || {
      echo "publish: $(basename "$extra") is missing beside the $platform archive; pack-windows.sh stages it" >&2
      exit 1
    }
    echo "publish: $platform local sha256  $(sha_of "$extra") ($(basename "$extra"))"
    uploads+=("$extra" "$extra.sha256")
  fi
done
# A published name keeps its bytes. The zone caches /dl/nodecode-* at the edge
# for 30 days (Cloudflare cache rule), so a second body under a name
# the host already holds would reach some readers and not others, and fail
# every install that checks it against the other .sha256. The `have' check
# above only sees this machine's OUT, so a name the host already holds is
# published with the host's bytes: they replace OUT's before anything ships.
# That is how a stable release of a commit the nightly channel already carries
# goes out -- the nightly's archives, built from the same commit by the same
# jobs, and run on the nightly homes since. A host body its own sidecar there
# does not name is refused, and nothing moves.
keep_published() {
  local names='' published name remote remote_sidecar
  for f in "$@"; do names="$names '$(basename "$f")'"; done
  # One read of the host: each name's body hash, and the hash its sidecar
  # there names.
  published="$(ssh -o BatchMode=yes "$host" "cd '$remote_dir' && for n in $names; do
      [ -f \"\$n\" ] && printf 'body %s %s\n' \"\$(sha256sum < \"\$n\" | cut -d' ' -f1)\" \"\$n\";
      [ -f \"\$n.sha256\" ] && printf 'sidecar %s %s\n' \"\$(cut -d' ' -f1 < \"\$n.sha256\")\" \"\$n\";
    done; true")"
  for f in "$@"; do
    name="$(basename "$f")"
    remote="$(awk -v n="$name" '$1 == "body" && $3 == n { print $2 }' <<<"$published")"
    [ -n "$remote" ] && [ "$remote" != "$(sha_of "$f")" ] || continue
    remote_sidecar="$(awk -v n="$name" '$1 == "sidecar" && $3 == n { print $2 }' <<<"$published")"
    if [ "$remote_sidecar" != "$remote" ]; then
      echo "publish: $name is already on $host with other bytes ($remote), which its sidecar there (${remote_sidecar:-none}) does not name. Nothing moves." >&2
      return 1
    fi
    scp -q "$host:$remote_dir/$name" "$host:$remote_dir/$name.sha256" "$(dirname "$f")/"
    [ "$(sha_of "$f")" = "$remote" ] && (cd "$(dirname "$f")" && sha256sum -c --quiet "$name.sha256") || {
      echo "publish: $name could not be taken from $host as published there. Nothing moves." >&2
      return 1
    }
    echo "publish: $name is already published ($remote): this release ships those bytes"
  done
}
archives=()
for f in "${uploads[@]:1}"; do case "$f" in *.sha256) ;; *) archives+=("$f") ;; esac; done
keep_published "${archives[@]}"
scp -q "${uploads[@]}" "$host:$remote_dir/"
restart_web

# Bare URLs, no cache-buster. A query string is a different cache key, so a
# ?cb= read can come back 200 while every reader of the plain URL is served a
# cached 404 or a cached older body - the proof would be of a URL nobody uses.
probe="$work/probe"
mkdir -p "$probe"
# Every body is read back at once: each is a whole public read, and taken one
# after another their times only add up.
reads=()
for platform in $platforms; do
  name="$(basename "$(archive_of "$platform")")"
  fetch "$probe/$name" -D "$probe/$name-headers" "$base_url/dl/$name" & reads+=($!)
  fetch "$probe/$name.sha256" "$base_url/dl/$name.sha256" & reads+=($!)
done
for platform in $platforms; do
  extra="$(extra_of "$platform")"
  [ -n "$extra" ] || continue
  name="$(basename "$extra")"
  fetch "$probe/$name" -D "$probe/$name-headers" "$base_url/dl/$name" & reads+=($!)
  fetch "$probe/$name.sha256" "$base_url/dl/$name.sha256" & reads+=($!)
done
fetch "$probe/changes-$label" "$base_url/dl/changes-$label" & reads+=($!)
for read_pid in "${reads[@]}"; do wait "$read_pid"; done
verify_body() {
  local body="$1" name local_sha edge_sha edge_sidecar_sha
  name="$(basename "$body")"
  local_sha="$(sha_of "$body")"
  edge_sha="$(sha_of "$probe/$name")"
  edge_sidecar_sha="$(cut -d' ' -f1 < "$probe/$name.sha256")"
  echo "publish: $name edge sha256  $edge_sha"
  [ "$local_sha" = "$edge_sha" ] || {
    echo "publish: $name edge archive mismatch: $edge_sha != $local_sha" >&2
    exit 1
  }
  [ "$local_sha" = "$edge_sidecar_sha" ] || {
    echo "publish: $name edge sidecar mismatch: $edge_sidecar_sha != $local_sha" >&2
    exit 1
  }
  grep -iE '^HTTP/|cf-cache-status|content-length|age:' "$probe/$name-headers" || true
}
for platform in $platforms; do
  verify_body "$(archive_of "$platform")"
  extra="$(extra_of "$platform")"
  if [ -n "$extra" ]; then verify_body "$extra"; fi
done
[ "$(sha_of "$changes")" = "$(sha_of "$probe/changes-$label")" ] || {
  echo "publish: the edge serves another changes-$label" >&2
  exit 1
}
echo "publish: changes-$label edge sha256 $(sha_of "$probe/changes-$label")"

# The installers go into the container before the pointers, so the pointers'
# restart serves both: one restart fewer, and a mirror that cannot be made
# fails the publish while every pointer still names the release before. The
# mirror is pushed to the branch the site's image is built from: a copy only
# in the running container is reverted by the next image (mirror-installers.sh).
# A nightly leaves both as the stable release put them.
if [ "$channel" = stable ]; then
  "$(dirname "${BASH_SOURCE[0]}")/mirror-installers.sh" "$site_remote" "$site_branch" \
    "$authority_installer" "$authority_ps1" "release: public installer follows authority ($label)" || {
    echo "publish: the installers could not be pushed to the site's $site_branch; nothing moves." >&2
    exit 1
  }

  scp -q "$authority_installer" "$host:/tmp/nodecode-install-${label}.sh"
  copy_in="docker cp /tmp/nodecode-install-${label}.sh '$web_container:/app/public/install.sh'"
  staged="/tmp/nodecode-install-${label}.sh"
  if [ -n "$authority_ps1" ]; then
    scp -q "$authority_ps1" "$host:/tmp/nodecode-install-${label}.ps1"
    copy_in="$copy_in && docker cp /tmp/nodecode-install-${label}.ps1 '$web_container:/app/public/install.ps1'"
    staged="$staged /tmp/nodecode-install-${label}.ps1"
  fi
  ssh -o BatchMode=yes "$host" "$copy_in && rm -f $staged"
fi

# The pointers move LAST, once every body they name is served. The release
# feed hands a box the new label within seconds of a pointer landing, and a
# box that reacted before its archive was readable would fail its staging and
# then wait for the next label. So the order is: bytes, restart, proof,
# installers and pointers, restart, proof.
#
# The second restart is not symmetry. Next builds the list of files it serves
# out of public/ AT STARTUP, and /dl is that directory bind-mounted; a file
# written into it while the container runs answers 404 -- with a query string
# or without, and it caches that 404 for hours -- until the next restart. The
# first write of a platform's pointer is exactly such a file, which is how a
# Mac could read no channel at all.
#
# The channel pointer: one line naming the label, at <channel>-<arch>-<os>
# beside the archives. A running organism polls it (`update' in the config;
# update.lisp) and stages what it names: stable unless its config names
# another channel ($channel, from the top).
#
# The nightly is never behind stable. A home on the nightly channel reads the
# nightly pointers alone, so a stable release of a commit past the nightly
# would reach every home but those until the next nightly. A stable publish
# therefore moves each nightly pointer that names a commit this release
# carries, in the same write as stable's: one that names a newer commit, or
# one this clone does not know, stays, and a platform with no nightly yet gets
# none. The host is read, not the edge: the zone holds the plain pointer for
# hours.
pointers="$work/pointers"
mkdir -p "$pointers"
pointer_names=()
for platform in $platforms; do
  printf '%s\n' "$label" > "$pointers/$channel-$platform"
  pointer_names+=("$channel-$platform")
done
if [ "$channel" = stable ]; then
  nightly_names=''
  for platform in $platforms; do nightly_names="$nightly_names 'nightly-$platform'"; done
  nightlies="$(ssh -o BatchMode=yes "$host" "cd '$remote_dir' && for n in $nightly_names; do
      [ -f \"\$n\" ] && printf '%s %s\n' \"\$n\" \"\$(tr -d '[:space:]' < \"\$n\")\";
    done; true")"
  for platform in $platforms; do
    nightly="$(awk -v n="nightly-$platform" '$1 == n { print $2 }' <<<"$nightlies")"
    if [ -z "$nightly" ] || [ "$nightly" = "$label" ]; then
      continue
    elif git merge-base --is-ancestor "$nightly" "$commit" 2>/dev/null; then
      echo "publish: nightly-$platform names $nightly, which $label carries: it moves with stable"
      printf '%s\n' "$label" > "$pointers/nightly-$platform"
      pointer_names+=("nightly-$platform")
    else
      echo "publish: nightly-$platform names $nightly, which $label does not carry: it stays"
    fi
  done
fi
# scp carries a file's mode to the host, and the web app reads the pointer it
# serves as its own user, so each is made world-readable whatever the umask.
chmod 0644 "$pointers"/*
scp -q "$pointers"/* "$host:$remote_dir/"
restart_web
for pointer_name in "${pointer_names[@]}"; do
  pointer_attempt=1
  while :; do
    if fetch "$probe/$pointer_name" "$base_url/dl/$pointer_name"; then
      edge_pointer="$(tr -d '[:space:]' < "$probe/$pointer_name")"
      [ "$edge_pointer" = "$label" ] && break
    else
      edge_pointer="(no answer)"
    fi
    [ "$pointer_attempt" -lt 10 ] || {
      printf 'publish: %s pointer still reads %s after %ss; %s was written to the host.\n  The zone caches /dl on its own terms, so a URL that did not exist at its last\n  read stays old for a few minutes.\n' \
        "$pointer_name" "$edge_pointer" "$((pointer_attempt * 15))" "$label" >&2
      exit 1
    }
    pointer_attempt=$((pointer_attempt + 1))
    sleep 15
  done
  echo "publish: $pointer_name points at $label"
done
echo "publish: $label is live at $base_url/dl for $platforms"

# Each served arm is proved the way the tarball is: the bytes at the edge are
# authority's bytes, and neither arm carries a release pin - the pointer is the
# one copy of that fact, and a pin that crept back in would drift behind it.
verify_installer() {
  local served="$1" authority="$2" authority_sha served_sha attempt=1
  authority_sha="$(sha_of "$authority")"
  while :; do
    # Following redirects: preview keeps /dl and /install.sh and sends every
    # other path, install.ps1 among them, to the apex with a 301 -- the same
    # container -- and curl -f takes a 301 for an answer, its empty body for
    # the installer, a hash no retry could ever match.
    fetch "$probe/$served" -D "$probe/$served-headers" -L "$base_url/$served"
    [ -s "$probe/$served" ] || {
      echo "publish: $base_url/$served answers an empty body" >&2
      return 1
    }
    served_sha="$(sha_of "$probe/$served")"
    [ "$served_sha" = "$authority_sha" ] && break
    # The zone caches this path on its own terms and rewrites the
    # cache-control it was given, so a replaced installer arrives at the edge
    # a minute or two behind the origin. That is a delay, not a failure; a
    # copy that never converges is one.
    [ "$attempt" -lt 10 ] || {
      printf 'publish: the edge still serves an older %s after %ss\n  (served %s, authority %s); the origin serves authority bytes\n' \
        "$served" "$((attempt * 20))" "$served_sha" "$authority_sha" >&2
      return 1
    }
    attempt=$((attempt + 1))
    sleep 20
  done
  echo "publish: $served sha256 $served_sha"
  if grep -qE '^(LINUX|DARWIN)_VERSION=|\$pinned = ' "$probe/$served"; then
    echo "publish: public $served still pins a release" >&2
    return 1
  fi
  grep -iE '^HTTP/|cf-cache-status|content-length|age:' "$probe/$served-headers" || true
  echo "publish: $served is live at $base_url/$served, reading the $channel pointer"
}

if [ "$channel" != stable ]; then
  echo "publish: $label is the $channel channel's build; stable, the installers and the releases page are as they were"
  exit 0
fi

verify_installer install.sh "$authority_installer"
if [ -n "$authority_ps1" ]; then
  verify_installer install.ps1 "$authority_ps1"
else
  echo "publish: install.ps1 stays as it was: Windows is held on the release stable-x86_64-windows names"
fi

# What a platform held back stays on, for the notes: its stable pointer, read
# on the host as the nightly pointers are, and the version declared there.
# A label this clone does not know is named alone.
held_notes=''
if [ -n "$held" ]; then
  held_names=''
  for platform in $held; do held_names="$held_names 'stable-$platform'"; done
  held_pointers="$(ssh -o BatchMode=yes "$host" "cd '$remote_dir' && for n in $held_names; do
      [ -f \"\$n\" ] && printf '%s %s\n' \"\$n\" \"\$(tr -d '[:space:]' < \"\$n\")\";
    done; true")" || held_pointers=''
  for platform in $held; do
    name="$(release_platform_name "$platform")"
    held_label="$(awk -v n="stable-$platform" '$1 == n { print $2 }' <<<"$held_pointers")"
    held_version=''
    if [ -n "$held_label" ] && held_commit="$(git rev-parse -q --verify "$held_label^{commit}" 2>/dev/null)"; then
      held_version="$(git show "$held_commit:$(layout_asd_at "$held_commit")" 2>/dev/null \
        | sed -n 's/^ *:version "\([0-9][0-9A-Za-z.-]*\)".*/\1/p' | head -n 1)" || held_version=''
    fi
    if [ -n "$held_version" ]; then on="nodecode $held_version ($held_label)"
    elif [ -n "$held_label" ]; then on="build $held_label"
    else on='the release it runs now'
    fi
    held_notes+="$(printf '%s is held back: this release ships no %s build, so %s installs and updates stay on %s.' "$name" "$name" "$name" "$on")"$'\n\n'
    echo "publish: $name stays on $on"
  done
fi

# The releases page, last: the tag at the commit, the notes since the release
# before it (NODECODE_PUBLISH_NOTES_SINCE names another base), and every
# archive with its sidecar, the same bytes the pointers name. The pointers have
# already moved, so a failure here says the release is live and only the page
# is missing; a rerun of the label uploads over its own assets.
notes="$work/release-notes.md"
since="${NODECODE_PUBLISH_NOTES_SINCE:-$(gh release list -R "$github_repo" --exclude-drafts --limit 20 --json tagName --jq '.[].tagName' | grep -vx "$tag" | head -n 1 || true)}"
if [ -n "$since" ] && ! git rev-parse -q --verify "$since^{commit}" > /dev/null; then
  git fetch -q origin tag "$since" || true
fi
heading="Changes"
range=(-n 30 "$commit")
if [ -n "$since" ] && base="$(git rev-parse -q --verify "$since^{commit}")"; then
  heading="Changes since $since"
  range=("$base..$commit")
fi
{
  printf 'nodecode %s, build %s: `nodecode --version` prints %s+%s.\n\n' "$version" "$label" "$version" "$label"
  printf 'Install:\n\n    curl -fsSL %s/install.sh | sh\n    irm %s/install.ps1 | iex    # Windows PowerShell\n\n' "$base_url" "$base_url"
  printf 'A running nodecode takes it on its own.\n\n'
  printf '%s' "$held_notes"
  printf '## %s\n\n' "$heading"
  git log --no-merges --format='%H %s' "${range[@]}" | while read -r hash subject; do
    printf -- '- %s (%s)\n' "${subject%% -- *}" "${hash:0:9}"
  done
} > "$notes"
assets=()
for platform in $platforms; do
  archive="$(archive_of "$platform")"
  assets+=("$archive" "$archive.sha256")
  extra="$(extra_of "$platform")"
  [ -z "$extra" ] || assets+=("$extra" "$extra.sha256")
done
if gh release view "$tag" -R "$github_repo" > /dev/null 2>&1; then
  gh release upload "$tag" -R "$github_repo" --clobber "${assets[@]}" &&
    gh release edit "$tag" -R "$github_repo" --notes-file "$notes" > /dev/null
else
  gh release create "$tag" -R "$github_repo" --target "$commit" --title "nodecode $version" \
    --notes-file "$notes" --latest "${assets[@]}" > /dev/null
fi || {
  echo "publish: $label is live, but GitHub release $tag did not land; rerun the publish of $label to list it" >&2
  exit 1
}
listed="$(gh release view "$tag" -R "$github_repo" --json assets --jq '.assets | length')"
[ "$listed" -ge "${#assets[@]}" ] || {
  echo "publish: GitHub release $tag lists $listed assets, not ${#assets[@]}" >&2
  exit 1
}
echo "publish: $tag is on the releases page with $listed assets: $(gh release view "$tag" -R "$github_repo" --json url --jq .url)"
