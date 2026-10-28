#!/usr/bin/env bash
# discord-live.sh --- a built nodecode answers a message on real Discord, with
# a real bot token, in a test channel; only the model provider is a stand-in.
#
# SPDX-License-Identifier: MIT
#
# usage: discord-live.sh TARGET     (a release tarball or a nodecode binary)
#   NODECODE_LIVE_DISCORD_TOKEN    the test bot's token (never the product's
#                                  bot: the run sets the application's slash
#                                  commands to this build's)
#   NODECODE_LIVE_DISCORD_CHANNEL  a text channel in a private test server
#                                  the bot can read, send, manage webhooks and
#                                  create threads in
# The bot needs the Message Content intent. What it proves, in order: the
# token authenticates; nodecode serve connects to Discord's real gateway with
# it; a message from a webhook (a bot author, admitted by allow_bots
# "mentions") that mentions the bot reaches the turn; the stand-in provider's
# reply is posted back on real Discord, read back with the token; serve stops
# on SIGTERM. Every message it made is deleted afterwards, the webhook kept
# for the next night. The provider is fake-apis.py on 127.0.0.1: no model is
# paid for, and a red run is Discord or nodecode, never a model's mood.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
target="${1:?usage: discord-live.sh TARGET}"
token="${NODECODE_LIVE_DISCORD_TOKEN:-}"
channel="${NODECODE_LIVE_DISCORD_CHANNEL:-}"
wait_secs="${E2E_WAIT_SECS:-180}"
api=https://discord.com/api/v10
[ -n "$token" ] || { echo 'live: NODECODE_LIVE_DISCORD_TOKEN is not set: the live check cannot run' >&2; exit 1; }
[ -n "$channel" ] || { echo 'live: NODECODE_LIVE_DISCORD_CHANNEL is not set: the live check cannot run' >&2; exit 1; }

scratch="$(mktemp -d "${TMPDIR:-/tmp}/nodecode-live.XXXXXX")"
fake_pid=''
serve_pid=''
made=()   # paths to DELETE afterwards, with the bot's token
ask_url=''
cleanup() {
  [ -n "$serve_pid" ] && kill -9 "$serve_pid" 2>/dev/null || true
  [ -n "$fake_pid" ] && kill "$fake_pid" 2>/dev/null || true
  for path in "${made[@]}"; do discord DELETE "$path" > /dev/null 2>&1 || true; done
  [ -n "$ask_url" ] && curl -sS -X DELETE "$ask_url" > /dev/null 2>&1 || true
  rm -rf "$scratch"
}
trap cleanup EXIT
fail() {
  echo "live: FAIL: $*" >&2
  echo '--- serve.log (tail)' >&2; tail -n 60 "$scratch/serve.log" >&2 2>/dev/null || true
  echo '--- provider' >&2; cut -c1-300 "$scratch/events.jsonl" >&2 2>/dev/null || true
  exit 1
}
# discord METHOD PATH [JSON]: one REST call as the bot; the body on stdout,
# the status in $scratch/status. The token never reaches a command line.
discord() {
  local method="$1" path="$2" body="${3:-}"
  curl -sS -X "$method" -o "$scratch/body" -w '%{http_code}' \
    -H @"$scratch/auth" -H 'Content-Type: application/json' \
    -H 'User-Agent: DiscordBot (https://github.com/nodecode-ai/nodecode, ci)' \
    ${body:+--data "$body"} "$api$path" > "$scratch/status"
  cat "$scratch/body"
}
status() { cat "$scratch/status"; }
jget() { python3 -c 'import json,sys; v=json.load(sys.stdin)
for k in sys.argv[1:]: v=v[int(k)] if isinstance(v,list) else (v or {}).get(k)
print("" if v is None else v)' "$@"; }
( umask 077; printf 'Authorization: Bot %s\n' "$token" > "$scratch/auth" )

me="$(discord GET /users/@me)"
[ "$(status)" = 200 ] || fail "the token does not authenticate: GET /users/@me answered $(status)"
bot_id="$(jget id <<< "$me")"
echo "live: token authenticates as $(jget username <<< "$me") ($bot_id)"
room="$(discord GET "/channels/$channel")"
[ "$(status)" = 200 ] || fail "the bot cannot read the test channel: GET /channels/<channel> answered $(status)"

# The driver: a webhook in the test channel, made once and kept.
hooks="$(discord GET "/channels/$channel/webhooks")"
[ "$(status)" = 200 ] || fail "the bot cannot list the channel's webhooks ($(status)): it needs Manage Webhooks there"
hook="$(python3 -c 'import json,sys
for h in json.load(sys.stdin):
    if h.get("name") == "nodecode-ci-driver" and h.get("token"): print(h["id"], h["token"]); break' <<< "$hooks")"
if [ -z "$hook" ]; then
  made_hook="$(discord POST "/channels/$channel/webhooks" '{"name":"nodecode-ci-driver"}')"
  [ "$(status)" = 200 ] || fail "the bot cannot make a webhook ($(status))"
  hook="$(jget id <<< "$made_hook") $(jget token <<< "$made_hook")"
fi
read -r hook_id hook_token <<< "$hook"

if [ -f "$target" ] && [ "${target%.tar.gz}" != "$target" ]; then
  mkdir -p "$scratch/unpacked"
  tar -xzf "$target" -C "$scratch/unpacked"
  bin="$(find "$scratch/unpacked" -maxdepth 3 -type f -name nodecode -perm -u+x | head -n 1)"
else
  bin="$target"
fi
[ -n "$bin" ] && [ -x "$bin" ] || fail "no nodecode binary in $target"

read -r http_port ws_port < <(python3 -c '
import socket
socks = [socket.socket() for _ in range(2)]
for s in socks: s.bind(("127.0.0.1", 0))
print(*[s.getsockname()[1] for s in socks])')
nonce="live-$(date +%s)-$RANDOM"
reply="nightly-pong-$nonce"
events="$scratch/events.jsonl"
python3 "$here/fake-apis.py" --http-port "$http_port" --ws-port "$ws_port" --log "$events" \
  --reply "$reply" --message "$nonce" > "$scratch/fake.log" 2>&1 &
fake_pid=$!
for _ in $(seq 1 50); do grep -q '"kind": "ready"' "$events" 2>/dev/null && break; sleep 0.1; done

home="$scratch/home"
mkdir -p "$home/cells"
shipped="$(dirname "$bin")/src/cells"
for cell in channels/kit:nodecode-channel-kit channels/discord:nodecode-channel-discord; do
  [ -d "$shipped/${cell%%:*}" ] || fail "the release ships no $shipped/${cell%%:*}"
  ln -s "$shipped/${cell%%:*}/" "$home/cells/${cell#*:}"
done
( umask 077; printf '%s\n' "$token" > "$home/discord-token" )
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
      "bot_token_file": "$home/discord-token",
      "bot_user_id": "$bot_id",
      "allowed_channels": ["$channel"],
      "require_mention": true,
      "allow_bots": "mentions"
    }
  }
}
EOF

NODECODE_HOME="$home" XDG_CACHE_HOME="$scratch/cache" \
  "$bin" serve --store "$scratch/history.db" > "$scratch/serve.log" 2>&1 &
serve_pid=$!
started=$SECONDS
until grep -q 'discord: running, connected' "$scratch/serve.log"; do
  kill -0 "$serve_pid" 2>/dev/null || fail 'serve exited before Discord connected'
  [ $((SECONDS - started)) -lt 90 ] || fail 'serve never said discord: running, connected within 90s'
  sleep 1
done
echo "live: connected to Discord's gateway after $((SECONDS - started))s"

ask_body="$(python3 -c 'import json,sys; print(json.dumps({"content": "<@%s> %s" % (sys.argv[1], sys.argv[2]),
  "username": "nodecode nightly", "allowed_mentions": {"users": [sys.argv[1]]}}))' "$bot_id" "$nonce")"
ask="$(curl -sS -H 'Content-Type: application/json' --data "$ask_body" "$api/webhooks/$hook_id/$hook_token?wait=true")"
ask_id="$(jget id <<< "$ask")"
[ -n "$ask_id" ] || fail "the webhook did not post the ask: $(head -c 200 <<< "$ask")"
ask_url="$api/webhooks/$hook_id/$hook_token/messages/$ask_id"
echo "live: asked in the test channel"

# The answer: a reply in the channel, or in the thread opened on the ask.
found=''
where=''
thread=''
started=$SECONDS
while [ -z "$found" ]; do
  [ $((SECONDS - started)) -lt "$wait_secs" ] || fail "no answer on Discord within ${wait_secs}s"
  sleep 3
  rooms=("$channel")
  thread="$(discord GET "/channels/$channel/messages/$ask_id" | jget thread id)"
  [ -n "$thread" ] && rooms+=("$thread")
  for room in "${rooms[@]}"; do
    hit="$(discord GET "/channels/$room/messages?limit=20" | python3 -c 'import json,sys
msgs = json.load(sys.stdin)
for m in msgs if isinstance(msgs, list) else []:
    if m.get("author", {}).get("id") == sys.argv[1] and sys.argv[2] in (m.get("content") or ""):
        print(m["id"]); break' "$bot_id" "$reply")"
    if [ -n "$hit" ]; then
      found="$room/$hit"
      made+=("/channels/$room/messages/$hit")
      [ "$room" = "$channel" ] && where='in the channel' || where='in the thread opened on the ask'
    fi
  done
done
# A thread opened on the ask goes with everything in it.
[ -n "$thread" ] && made+=("/channels/$thread")
echo "live: answer read back on Discord after $((SECONDS - started))s, $where"
grep -q '"saw_message": true' "$events" || fail 'the provider never saw the Discord message'

kill -TERM "$serve_pid"
for _ in $(seq 1 30); do kill -0 "$serve_pid" 2>/dev/null || break; sleep 0.5; done
kill -0 "$serve_pid" 2>/dev/null && fail 'serve did not stop on SIGTERM within 15s'
serve_pid=''
echo 'live: ok'
