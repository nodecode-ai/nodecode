#!/usr/bin/env bash
# discord-roundtrip.sh --- a built nodecode answers one Discord message end to
# end, against stand-ins for Discord and the model provider.
#
# SPDX-License-Identifier: MIT
#
# usage: discord-roundtrip.sh TARGET [--tool-call-without-id]
#   TARGET  a release tarball (nodecode-*.tar.gz) or a nodecode binary
#
# The shipped binary, not the source tree: it is unpacked, given a scratch
# NODECODE_HOME whose config points the Discord cell at fake-apis.py's gateway
# and REST API and the default model at its OpenAI-compatible endpoint, and
# run as `nodecode serve'. The fake sends one MESSAGE_CREATE; the run passes
# when nodecode identified with the configured token, the provider saw the
# message, and the provider's reply was posted back to that channel. With
# --tool-call-without-id the provider's first answer is a tool call carrying
# no id (issue #32): the turn must run the tool and send the history back with
# an id on it, or the fake refuses it as a strict endpoint does.
# Then serve must stop on SIGTERM. Nothing leaves 127.0.0.1.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
target="${1:?usage: discord-roundtrip.sh TARGET [--tool-call-without-id]}"
mode="${2:-}"
wait_secs="${E2E_WAIT_SECS:-240}"

scratch="$(mktemp -d "${TMPDIR:-/tmp}/nodecode-e2e.XXXXXX")"
fake_pid=''
serve_pid=''
cleanup() {
  [ -n "$serve_pid" ] && kill -9 "$serve_pid" 2>/dev/null || true
  [ -n "$fake_pid" ] && kill "$fake_pid" 2>/dev/null || true
  if [ -n "${E2E_KEEP:-}" ]; then echo "e2e: kept $scratch" >&2; else rm -rf "$scratch"; fi
}
trap cleanup EXIT
fail() {
  echo "e2e: FAIL: $*" >&2
  echo '--- serve.log (tail)' >&2; tail -n 60 "$scratch/serve.log" >&2 2>/dev/null || true
  echo '--- events' >&2; cut -c1-300 "$scratch/events.jsonl" >&2 2>/dev/null || true
  exit 1
}

if [ -f "$target" ] && [ "${target%.tar.gz}" != "$target" ]; then
  mkdir -p "$scratch/unpacked"
  tar -xzf "$target" -C "$scratch/unpacked"
  bin="$(find "$scratch/unpacked" -maxdepth 3 -type f -name nodecode -perm -u+x | head -n 1)"
  [ -n "$bin" ] || fail "no nodecode binary in $target"
else
  bin="$target"
fi
[ -x "$bin" ] || fail "$bin is not executable"

read -r http_port ws_port < <(python3 -c '
import socket
socks = [socket.socket() for _ in range(2)]
for s in socks: s.bind(("127.0.0.1", 0))
print(*[s.getsockname()[1] for s in socks])')
reply="e2e-pong-$$-$RANDOM"
events="$scratch/events.jsonl"
fake_args=(--http-port "$http_port" --ws-port "$ws_port" --log "$events" --reply "$reply")
[ "$mode" = --tool-call-without-id ] && fake_args+=(--tool-call-without-id)
python3 "$here/fake-apis.py" "${fake_args[@]}" > "$scratch/fake.log" 2>&1 &
fake_pid=$!
for _ in $(seq 1 50); do grep -q '"kind": "ready"' "$events" 2>/dev/null && break; sleep 0.1; done
grep -q '"kind": "ready"' "$events" 2>/dev/null || { cat "$scratch/fake.log" >&2; fail 'the fakes never came up'; }

home="$scratch/home"
mkdir -p "$home/cells"
# A cell runs when its folder is in the home's cells folder (presence is
# consent): the shipped Discord cell and the kit under it, linked the way
# installing a shipped cell links it.
shipped="$(dirname "$bin")/src/cells"
for cell in channels/kit:nodecode-channel-kit channels/discord:nodecode-channel-discord; do
  [ -d "$shipped/${cell%%:*}" ] || fail "the release ships no $shipped/${cell%%:*}"
  ln -s "$shipped/${cell%%:*}/" "$home/cells/${cell#*:}"
done
printf 'e2e-fake-token\n' > "$home/discord-token"
chmod 600 "$home/discord-token"
cat > "$home/config.jsonc" <<EOF
{
  "providers": {
    "e2e": {"base_url": "http://127.0.0.1:$http_port/v1", "sdk": "openai-completions",
            "api_key": "e2e-fake-key", "models": [{"id": "fake-model"}]}
  },
  "general": {"default_model": {"provider": "e2e", "model": "fake-model"},
              "auxiliary_model": {"provider": "e2e", "model": "fake-model"}},
  "channels": {
    "discord": {
      "enabled": true,
      "gateway_url": "ws://127.0.0.1:$ws_port/",
      "api_base": "http://127.0.0.1:$http_port/api/v10",
      "bot_token_file": "$home/discord-token",
      "bot_user_id": "999",
      "owner": ["42"],
      "allowed_channels": ["123"],
      "require_mention": false,
      "request_timeout_seconds": 10
    }
  }
}
EOF

echo "e2e: $bin serve, Discord at 127.0.0.1:$ws_port, provider at 127.0.0.1:$http_port"
NODECODE_HOME="$home" XDG_CACHE_HOME="$scratch/cache" \
  "$bin" serve --store "$scratch/history.db" > "$scratch/serve.log" 2>&1 &
serve_pid=$!

posted() {
  python3 - "$events" "$reply" <<'PY'
import json, sys
reply = sys.argv[2]
for line in open(sys.argv[1], encoding='utf-8'):
    e = json.loads(line)
    b = e.get('body')
    if e['kind'] == 'rest' and e['method'] in ('POST', 'PATCH') and '/messages' in e['path'] \
            and isinstance(b, dict) and reply in (b.get('content') or ''):
        sys.exit(0)
sys.exit(1)
PY
}
started=$SECONDS
until posted; do
  kill -0 "$serve_pid" 2>/dev/null || fail 'serve exited before answering'
  if grep -q '"empty_tool_ids": [1-9]' "$events"; then
    fail 'nodecode sent a tool call with no id back to the provider, which refused the history (issue #32)'
  fi
  [ $((SECONDS - started)) -lt "$wait_secs" ] || fail "no answer posted within ${wait_secs}s"
  sleep 1
done
echo "e2e: answer posted after $((SECONDS - started))s"

python3 - "$events" "$reply" "$mode" <<'PY' || fail 'the run did not hold'
import json, sys
events = [json.loads(l) for l in open(sys.argv[1], encoding='utf-8')]
reply, mode = sys.argv[2], sys.argv[3]
problems = []
identify = [e for e in events if e['kind'] == 'identify']
if not identify or identify[0].get('token') != 'e2e-fake-token':
    problems.append('IDENTIFY did not carry the configured token: %r' % identify[:1])
if not any(e['kind'] == 'rest' and e.get('auth') == 'Bot ' for e in events):
    problems.append('no REST call was authorized as a bot')
turns = [e for e in events if e['kind'] == 'provider' and e.get('path') == '/v1/chat/completions']
if not any(e.get('saw_message') for e in turns):
    problems.append('the provider never saw the Discord message')
if any(e.get('empty_tool_ids') for e in turns):
    problems.append('a request carried a tool call with no id (issue #32)')
if mode == '--tool-call-without-id' and not any(e.get('tool_results') for e in turns):
    problems.append('the id-less tool call never came back with its result')
posts = [e for e in events if e['kind'] == 'rest' and e['method'] in ('POST', 'PATCH')
         and '/messages' in e['path'] and isinstance(e.get('body'), dict)
         and reply in (e['body'].get('content') or '')]
first = posts[0] if posts else {}
ref = (first.get('body') or {}).get('message_reference') or {}
threaded = any(e['kind'] == 'rest' and e['path'] == '/channels/123/messages/1001/threads' for e in events)
if not (first.get('path', '').startswith('/channels/123/') and ref.get('message_id') == '1001') and not threaded:
    problems.append('the answer was neither a reply to the ask nor in its thread: %r' % first)
for p in problems:
    print('e2e:', p, file=sys.stderr)
print('e2e: %d provider request(s), %d REST call(s), answer at %s'
      % (len(turns), sum(e['kind'] == 'rest' for e in events), first.get('path')))
sys.exit(1 if problems else 0)
PY

kill -TERM "$serve_pid"
for _ in $(seq 1 30); do kill -0 "$serve_pid" 2>/dev/null || break; sleep 0.5; done
if kill -0 "$serve_pid" 2>/dev/null; then fail 'serve did not stop on SIGTERM within 15s'; fi
serve_pid=''
echo "e2e: ok${mode:+ ($mode)}"
