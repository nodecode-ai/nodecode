#!/usr/bin/env bash
# publish-hub.sh --- put Nodecode's cell index where the organism reads it.
#
# The index is written in nodecode-ai/cells, by pull request. This serves
# index.json as it stands at one commit of that repository (main's head unless
# another is named) at nodecode.ai/dl/cells/index.json, the address
# *HUB-INDEX-URL* reads (cells.lisp), then proves the served bytes; each
# folder entry also gets its archive beside it (see below). The file
# is read from GitHub at the commit, never from a checkout here, so what is
# served is what merged; and only a commit whose check passes here -- the
# repository's .github/check.sh at that commit, every entry installed and
# loaded against a release in a scratch home -- is served, since the
# organism refuses a broken index whole. The release is nodecode on PATH, or
# NODECODE_HUB_CHECK_BINARY. The one dl/ directory answers at the
# apex and at preview alike, so a release that still reads preview's address
# reads the same bytes.
set -euo pipefail
REF="${1:-main}"
repo="${NODECODE_HUB_REPO:-nodecode-ai/cells}"
. "$(dirname "${BASH_SOURCE[0]}")/publish-config.sh"
host="$(publish_setting NODECODE_PUBLISH_HOST)" || exit 1
remote_dir="$(publish_setting NODECODE_PUBLISH_REMOTE_DIR)/cells" || exit 1
base_url="${NODECODE_PUBLISH_BASE_URL:-https://nodecode.ai}"
url="${base_url%/}/dl/cells/index.json"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT INT TERM
binary="${NODECODE_HUB_CHECK_BINARY:-$(command -v nodecode || true)}"
test -x "$binary" || { echo "publish-hub: no nodecode release to check with (NODECODE_HUB_CHECK_BINARY)" >&2; exit 1; }
commit="$(gh api "repos/$repo/commits/$REF" --jq .sha)"
mkdir -p "$work/repo/.github"
for file in index.json .github/check.sh .github/check.lisp; do
  gh api -H 'Accept: application/vnd.github.raw' "repos/$repo/contents/$file?ref=$commit" > "$work/repo/$file"
done
bash "$work/repo/.github/check.sh" "$binary" || {
  printf 'publish-hub: %s at %s fails its check here; nothing is served.\n' "$repo" "$commit" >&2
  exit 1
}
# Each folder entry's archive: its folder at its pin as one .tar.gz under
# cells/<commit>/<name>.tar.gz beside the index, which /dl answers as
# immutable and Cloudflare keeps at the edge, so a pick downloads one cell
# from near by instead of the repository from GitHub (HUB-ARCHIVE-CHECKOUT).
# An archive already on the host is never rewritten -- the sha256 an index
# names never changes under an edge's copy -- and the index names only an
# archive whose served bytes were just read back to that sha256.
NODECODE_HUB_WORK="$work" python3 - <<'PY'
import hashlib, json, os, subprocess
work = os.environ["NODECODE_HUB_WORK"]
fetched = set()
for e in json.load(open(f"{work}/repo/index.json"))["cells"]:
    if not e.get("path"):
        continue
    git = f"{work}/git/{hashlib.sha1(e['git'].encode()).hexdigest()[:16]}"
    if not os.path.isdir(git):
        subprocess.run(["git", "init", "-q", "--bare", git], check=True)
    if (git, e["commit"]) not in fetched:
        subprocess.run(["git", "-C", git, "fetch", "-q", "--depth", "1", "--no-tags", "--", e["git"],
                        f"+{e['commit']}:refs/pins/{e['commit']}"], check=True)
        fetched.add((git, e["commit"]))
    os.makedirs(f"{work}/archives/{e['commit']}", exist_ok=True)
    subprocess.run(["git", "-C", git, "archive", "--format=tar.gz", "-o",
                    f"{work}/archives/{e['commit']}/{e['name']}.tar.gz", e["commit"], "--", e["path"]], check=True)
PY
if [ -d "$work/archives" ]; then
  chmod -R u=rwX,go=rX "$work/archives"
  tar -C "$work/archives" -cf - . | ssh -o BatchMode=yes "$host" "mkdir -p '$remote_dir' && tar -C '$remote_dir' --skip-old-files -xf -"
  ssh -o BatchMode=yes "$host" "cd '$remote_dir' && find . -mindepth 2 -maxdepth 2 -name '*.tar.gz' -exec sha256sum {} +" > "$work/sums"
fi
NODECODE_HUB_WORK="$work" NODECODE_HUB_BASE="${base_url%/}" python3 - <<'PY'
import hashlib, json, os, urllib.request
work, base = os.environ["NODECODE_HUB_WORK"], os.environ["NODECODE_HUB_BASE"]
sums = {}
if os.path.exists(f"{work}/sums"):
    for line in open(f"{work}/sums"):
        digest, name = line.split(None, 1)
        sums[name.strip().removeprefix("./")] = digest
index = json.load(open(f"{work}/repo/index.json"))
for e in index["cells"]:
    file = f"{e['commit']}/{e['name']}.tar.gz"
    if e.get("path") and file in sums:
        url = f"{base}/dl/cells/{file}"
        request = urllib.request.Request(url, headers={"User-Agent": "nodecode-publish-hub"})
        served = hashlib.sha256(urllib.request.urlopen(request, timeout=60).read()).hexdigest()
        if served != sums[file]:
            raise SystemExit(f"publish-hub: {url} serves {served}, not the host's {sums[file]}")
        e["archive"], e["sha256"] = url, sums[file]
with open(f"{work}/index.json", "w") as out:
    json.dump(index, out, indent=2, ensure_ascii=False)
    out.write("\n")
PY
# One rename on the host, so a reader gets the old file or the new, never half.
chmod 0644 "$work/index.json"
ssh -o BatchMode=yes "$host" "mkdir -p '$remote_dir'"
scp -q "$work/index.json" "$host:$remote_dir/.index.json.$commit"
ssh -o BatchMode=yes "$host" "mv -f '$remote_dir/.index.json.$commit' '$remote_dir/index.json'"
# /dl keeps a file with a dot in its name a minute at the edge (web's dl route).
attempt=1
while :; do
  if curl -fsS -o "$work/served" "$url" 2>/dev/null && cmp -s "$work/index.json" "$work/served"; then
    break
  fi
  [ "$attempt" -lt 12 ] || {
    printf 'publish-hub: %s still serves other bytes after %ss; the new file is on the host.\n' "$url" "$((attempt * 15))" >&2
    exit 1
  }
  sleep 15
  attempt=$((attempt + 1))
done
printf 'publish-hub: %s serves %s at %s\n' "$url" "$repo" "$commit"
