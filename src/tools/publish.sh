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

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
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

# Every platform rides every release. Leaving the Mac out is said, never
# assumed: NODECODE_PUBLISH_SKIP_MACOS publishes without its arm, and its
# pointer keeps naming the label it named (a Mac that is asleep or away is a
# red arm, not a quiet skip).
platforms="x86_64-linux arm64-darwin x86_64-windows"
if [ -n "${NODECODE_PUBLISH_SKIP_MACOS:-}" ]; then
  platforms="x86_64-linux x86_64-windows"
  echo "publish: NODECODE_PUBLISH_SKIP_MACOS is set: no macOS arm, and stable-arm64-darwin keeps the label it names" >&2
fi
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

# Every release is a version on GitHub's releases page: the base that
# src/nodecode.asd declares at the commit, tagged v<base> at that commit, with
# its notes and archives (the block at the end). A version is released once, so
# a version already tagged at another commit stops the publish here, before any
# arm packs: bump :version in src/nodecode.asd first.
github_repo="$(publish_setting NODECODE_PUBLISH_GITHUB_REPO)" || exit 1
version="$(git show "$commit:src/nodecode.asd" | sed -n 's/^ *:version "\([0-9][0-9A-Za-z.-]*\)".*/\1/p' | head -n 1)"
[ -n "$version" ] || { echo "publish: src/nodecode.asd at $label declares no :version" >&2; exit 1; }
tag="v$version"
gh api "repos/$github_repo" --jq .full_name > /dev/null || {
  echo "publish: GitHub does not answer for $github_repo (gh auth status), so $tag could not be listed. Nothing moves." >&2
  exit 1
}
# A miss is a nonzero exit, and gh prints GitHub's error body on stdout with
# it, so only an answer that succeeded names a commit.
tagged="$(gh api "repos/$github_repo/commits/$tag" --jq .sha 2> /dev/null)" || tagged=""
if [ -n "$tagged" ] && [ "$tagged" != "$commit" ]; then
  echo "publish: $tag is already released at ${tagged:0:9}; bump :version in src/nodecode.asd for $label. Nothing moves." >&2
  exit 1
fi
echo "publish: $label is nodecode $version, tagged $tag"
authority_installer="${NODECODE_PUBLISH_INSTALLER_PATH:-$repo/src/tools/install.sh}"
# The Windows arm is served beside the shell one and reads the same pointer,
# so it follows the same authority/mirror discipline.
authority_ps1="${NODECODE_PUBLISH_PS1_PATH:-$repo/src/tools/install.ps1}"
# The site builds its image from this branch of the repository the local
# clone tracks; the installers are mirrored there, never into the clone.
# NODECODE_PUBLISH_SITE_REMOTE names that repository directly, for a publisher
# with no clone of the site (the Release workflow); else the clone's origin.
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
[ -f "$authority_ps1" ] || { echo "publish: missing authority installer $authority_ps1" >&2; exit 1; }

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
have x86_64-windows || just windows "$commit" "$OUT" || {
  echo "publish: the Windows arm did not pack $label; $OUT/windows-$label.log names the step. Nothing moves." >&2
  exit 1
}
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

restart_web() {
  ssh -o BatchMode=yes "$host" \
    "cd '$app_dir' && docker compose -f '$compose_file' --env-file '$env_file' restart web" \
    >/tmp/nodecode-publish-restart.log 2>&1 || {
      cat /tmp/nodecode-publish-restart.log
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
changes="$OUT/changes-$label"
declare -A ships=()
while read -r hash; do ships[$hash]=1; done < <(
  git log --format=%H -n 200 "$commit" -- src/src src/cells src/nodecode.asd resources \
    src/tools/install.sh src/tools/install.ps1 ':(exclude,glob)src/cells/**/test/**')
git log --format='%H %s' -n 200 "$commit" | while read -r hash subject; do
  if [ -n "${ships[$hash]:-}" ]; then
    printf '%s %s\n' "${hash:0:9}" "${subject%% -- *}"
  else
    printf '%s\n' "${hash:0:9}"
  fi
done > "$changes"
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
# every install that checks it against the other .sha256. The same bytes again
# pass; a different body is refused before anything moves. The `have' check
# above only sees this machine's OUT.
refuse_republish() {
  local published name remote
  published="$(ssh -o BatchMode=yes "$host" "cd '$remote_dir' && sha256sum -- $(for f in "$@"; do printf "'%s' " "$(basename "$f")"; done) 2>/dev/null; true")"
  for f in "$@"; do
    name="$(basename "$f")"
    remote="$(awk -v n="$name" '$2 == n { print $1 }' <<<"$published")"
    if [ -n "$remote" ] && [ "$remote" != "$(sha_of "$f")" ]; then
      echo "publish: $name is already on $host with other bytes ($remote); the edge caches /dl/nodecode-* for 30 days, so a published name keeps its bytes. Nothing moves." >&2
      return 1
    fi
  done
}
refuse_republish "${uploads[@]:1}"
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
"$(dirname "${BASH_SOURCE[0]}")/mirror-installers.sh" "$site_remote" "$site_branch" \
  "$authority_installer" "$authority_ps1" "release: public installer follows authority ($label)" || {
  echo "publish: the installers could not be pushed to the site's $site_branch; nothing moves." >&2
  exit 1
}

scp -q "$authority_installer" "$host:/tmp/nodecode-install-${label}.sh"
scp -q "$authority_ps1" "$host:/tmp/nodecode-install-${label}.ps1"
ssh -o BatchMode=yes "$host" \
  "docker cp /tmp/nodecode-install-${label}.sh '$web_container:/app/public/install.sh' &&
   docker cp /tmp/nodecode-install-${label}.ps1 '$web_container:/app/public/install.ps1' &&
   rm -f /tmp/nodecode-install-${label}.sh /tmp/nodecode-install-${label}.ps1"

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
# update.lisp) and stages what it names. Stable unless told otherwise.
channel="${NODECODE_PUBLISH_CHANNEL:-stable}"
pointers="$work/pointers"
mkdir -p "$pointers"
for platform in $platforms; do
  printf '%s\n' "$label" > "$pointers/$channel-$platform"
done
# scp carries a file's mode to the host, and the web app reads the pointer it
# serves as its own user, so each is made world-readable whatever the umask.
chmod 0644 "$pointers"/*
scp -q "$pointers"/* "$host:$remote_dir/"
restart_web
for platform in $platforms; do
  pointer_name="$channel-$platform"
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

verify_installer install.sh "$authority_installer"
verify_installer install.ps1 "$authority_ps1"

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
  printf 'A running nodecode takes it on its own.\n\n## %s\n\n' "$heading"
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
