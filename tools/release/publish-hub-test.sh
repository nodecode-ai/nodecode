#!/usr/bin/env bash
# publish-hub-test.sh --- hermetic regression for the cell index publisher.
#
# publish-hub.sh against a host and an edge on this machine: gh answers the
# cells repository out of a fixture, ssh and scp reach a folder here, the
# fixture's check passes, and the edge is an HTTP server on loopback over that
# folder that records every read and can be told to stall or misanswer one.
set -eu

here="$(cd "$(dirname "$0")" && pwd)"
root="$(mktemp -d)"
edge_pid=''
trap '[ -z "$edge_pid" ] || kill "$edge_pid" 2>/dev/null || true; rm -rf "$root"' EXIT INT TERM
fake_bin="$root/bin"
host="$root/host"
faults="$root/faults"
mkdir -p "$fake_bin" "$host" "$faults"
fail() {
  printf 'publish-hub-test: %s\n' "$*" >&2
  exit 1
}

# gh: the commit a ref names, and a file of the fixture repository at it.
cat > "$fake_bin/gh" <<'EOF'
#!/bin/sh
set -eu
case "$1 $2" in
  'api repos/test/cells/commits/'*) printf '%s\n' "$PUBLISH_HUB_TEST_COMMIT" ;;
  'api -H')
    file="${4#repos/test/cells/contents/}"
    cat "$PUBLISH_HUB_TEST_REPO/${file%%\?ref=*}" ;;
  *) printf 'gh: not a call the publisher makes: %s\n' "$*" >&2; exit 1 ;;
esac
EOF
# ssh runs its command here, where the host's folder is; scp copies here.
cat > "$fake_bin/ssh" <<'EOF'
#!/bin/sh
for last; do :; done
exec sh -c "$last"
EOF
cat > "$fake_bin/scp" <<'EOF'
#!/bin/sh
set -eu
[ "$1" = -q ]
cp "$2" "${3#*:}"
EOF
printf '#!/bin/sh\nexit 0\n' > "$fake_bin/nodecode"
chmod +x "$fake_bin"/*

# The edge: /dl/cells/ is the host's cells folder. FAULTS/<name> makes a read
# of the archive <name> stall past the publisher's read timeout (stall-once:
# the first read only; stall: every read) or answer other bytes (lie).
cat > "$root/edge.py" <<'EOF'
import http.server, os, sys, threading, time
host, faults, log, stall = sys.argv[1], sys.argv[2], sys.argv[3], float(sys.argv[4])
lock = threading.Lock()

class Edge(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        with lock, open(log, "a") as out:
            out.write(self.path + "\n")
        name = os.path.basename(self.path)
        fault = os.path.join(faults, name)
        kind = open(fault).read().strip() if os.path.exists(fault) else ""
        if kind == "stall-once":
            os.remove(fault)
        if kind in ("stall", "stall-once"):
            time.sleep(stall)
        file = os.path.join(host, self.path.removeprefix("/dl/"))
        if not self.path.startswith("/dl/cells/") or not os.path.isfile(file):
            self.send_error(404)
            return
        body = b"other bytes" if kind == "lie" else open(file, "rb").read()
        self.send_response(200)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, *args):
        pass

server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Edge)
server.daemon_threads = True
with open(sys.argv[5], "w") as out:
    out.write(str(server.server_address[1]))
server.serve_forever()
EOF
python3 "$root/edge.py" "$host" "$faults" "$root/edge-log" "${PUBLISH_HUB_TEST_STALL:-8}" "$root/edge-port" 2> "$root/edge-errors" &
edge_pid=$!
for _ in $(seq 100); do [ -s "$root/edge-port" ] && break; sleep 0.1; done
[ -s "$root/edge-port" ] || fail 'the edge did not start'
base="http://127.0.0.1:$(cat "$root/edge-port")"

# The cells: one git repository, each cell a folder pinned at a commit of it.
cells="$root/cells"
gitc() { git -C "$cells" -c user.name=t -c user.email=t@t.invalid -c gc.auto=0 -c maintenance.auto=false "$@"; }
git init -q "$cells"
for n in 1 2 3 4 5; do mkdir -p "$cells/c$n" && printf '(cell %s)\n' "$n" > "$cells/c$n/cell.lisp"; done
gitc add -A && gitc commit -q -m one
one="$(git -C "$cells" rev-parse HEAD)"
mkdir -p "$cells/c6" && printf '(cell 6)\n' > "$cells/c6/cell.lisp"
gitc add -A && gitc commit -q -m two
two="$(git -C "$cells" rev-parse HEAD)"
mkdir -p "$cells/c7" && printf '(cell 7)\n' > "$cells/c7/cell.lisp"
gitc add -A && gitc commit -q -m three
three="$(git -C "$cells" rev-parse HEAD)"

# index NAME COMMIT:CELL... --- the fixture repository NAME, whose index pins
# each CELL at COMMIT, beside a cell with no folder (a URL entry).
index() {
  repo="$root/repo-$1"
  shift
  mkdir -p "$repo/.github"
  printf 'exit 0\n' > "$repo/.github/check.sh"
  : > "$repo/.github/check.lisp"
  {
    printf '{"cells": [{"name": "elsewhere", "git": "https://example.invalid/elsewhere.git", "commit": "%s"}' "$one"
    for pin; do
      printf ', {"name": "%s", "git": "file://%s", "commit": "%s", "path": "%s"}' "${pin#*:}" "$cells" "${pin%%:*}" "${pin#*:}"
    done
    printf ']}\n'
  } > "$repo/index.json"
}
index one "$one:c1" "$one:c2" "$one:c3" "$one:c4" "$one:c5"
index two "$one:c1" "$one:c2" "$one:c3" "$one:c4" "$one:c5" "$two:c6"
index three "$one:c1" "$one:c2" "$one:c3" "$one:c4" "$one:c5" "$two:c6" "$three:c7"

# publish NAME COMMIT RUN --- publish the fixture repository NAME as COMMIT;
# RUN-output, RUN-errors and RUN-reads (the edge's log of this run) are kept.
publish() {
  : > "$root/edge-log"
  status=0
  PATH="$fake_bin:$PATH" \
  NODECODE_PUBLISH_CONFIG=/dev/null \
  NODECODE_PUBLISH_HOST=testhost \
  NODECODE_PUBLISH_REMOTE_DIR="$host" \
  NODECODE_PUBLISH_BASE_URL="$base" \
  NODECODE_HUB_REPO=test/cells \
  NODECODE_HUB_CHECK_BINARY="$fake_bin/nodecode" \
  NODECODE_HUB_READ_TIMEOUT="${PUBLISH_HUB_TEST_TIMEOUT:-3}" \
  PUBLISH_HUB_TEST_REPO="$root/repo-$1" \
  PUBLISH_HUB_TEST_COMMIT="$2" \
    bash "$here/publish-hub.sh" > "$root/$3-output" 2> "$root/$3-errors" || status=$?
  cp "$root/edge-log" "$root/$3-reads"
  return "$status"
}
reads() { grep -c "/$1.tar.gz\$" "$root/$2-reads" || true; }
named() { python3 -c 'import json, sys; print(sum(1 for e in json.load(open(sys.argv[1]))["cells"] if e.get("archive")))' "$host/cells/index.json"; }

# --- a read that stalls once is read again, and the publish serves -----------
echo stall-once > "$faults/c2.tar.gz"
publish one "$one" first || { cat "$root/first-errors" >&2; fail 'one stalled read stopped the publish'; }
[ "$(reads c2 first)" = 2 ] || fail "the stalled archive was read $(reads c2 first) times, not twice"
for n in 1 3 4 5; do
  [ "$(reads "c$n" first)" = 1 ] || fail "c$n was read $(reads "c$n" first) times, not once"
done
[ "$(named)" = 5 ] || fail "the served index names $(named) archives, not 5"
grep -q 'serves test/cells at' "$root/first-output" || fail 'the publish did not say what it serves'

# --- an archive the served index proved is not read again --------------------
publish two "$two" second || { cat "$root/second-errors" >&2; fail 'the second publish failed'; }
[ "$(reads c6 second)" = 1 ] || fail 'the new archive was not read back'
[ "$(grep -c 'tar.gz$' "$root/second-reads")" = 1 ] \
  || fail "a publish read back archives the served index already proved: $(grep 'tar.gz$' "$root/second-reads" | tr '\n' ' ')"
[ "$(named)" = 6 ] || fail "the served index names $(named) archives, not 6"
grep -q '1 archives read back, 5 named as the served index proved them' "$root/second-errors" \
  || fail "the publish did not say what it read back: $(cat "$root/second-errors")"
publish two "$two" again || { cat "$root/again-errors" >&2; fail 'a republish failed'; }
! grep -q 'tar.gz$' "$root/again-reads" || fail 'a republish of the same commit read archives back'

# --- an archive that never answers, or answers other bytes, serves nothing ---
cp "$host/cells/index.json" "$root/index-before"
echo stall > "$faults/c7.tar.gz"
if publish three "$three" silent; then fail 'an archive that never answered was served'; fi
[ "$(reads c7 silent)" = 4 ] || fail "the silent archive was read $(reads c7 silent) times, not 4"
grep -q "c7.tar.gz gave no bytes in four reads" "$root/silent-errors" \
  || fail "the silent archive was not named: $(cat "$root/silent-errors")"
cmp -s "$root/index-before" "$host/cells/index.json" || fail 'a failed publish replaced the index'
echo lie > "$faults/c7.tar.gz"
if publish three "$three" lying; then fail 'an archive served with other bytes was named'; fi
[ "$(reads c7 lying)" = 1 ] || fail 'wrong bytes were read again, as if the network had dropped them'
grep -q "c7.tar.gz serves .*, not the host's " "$root/lying-errors" \
  || fail "the wrong bytes were not named: $(cat "$root/lying-errors")"
cmp -s "$root/index-before" "$host/cells/index.json" || fail 'a failed publish replaced the index'
rm "$faults/c7.tar.gz"
publish three "$three" healed || { cat "$root/healed-errors" >&2; fail 'the publish failed once the edge answered'; }
[ "$(named)" = 7 ] || fail "the served index names $(named) archives, not 7"

echo 'publish-hub-test: ok'
