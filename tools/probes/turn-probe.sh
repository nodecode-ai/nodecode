#!/usr/bin/env bash
# turn-probe.sh LABEL PROMPT [WORKDIR] --- one measured turn on the repo's
# ./nodecode (release binary) with --ephemeral, driven through tmux: the
# engine's phase log (NODECODE_PHASE_LOG), the screen sampled at 10 Hz, the
# process's CPU at 5 Hz, and the ephemeral store read back after /exit.
# Writes $PROBE_OUT/<stamp>-LABEL/{phase.log,frames.log,frames/,cpu.log,
# meta.json,nodecode-ephemeral-<pid>.db*}; read it with turn-probe-analyze.py.
# PROBE_OUT defaults to /tmp/nodecode-turn-probe; WORKDIR defaults to a
# nine-file scratch tree made under it. PROBE_PRELUDE, when set, is a prompt
# sent first and waited out before PROMPT (a form to evaluate, a setting to
# change in the image); the analyzer selects the measured turn by PROMPT.
# Build first: just release.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
S="${PROBE_OUT:-/tmp/nodecode-turn-probe}"; mkdir -p "$S"
LABEL="$1"; PROMPT="$2"; WS="${3:-$S/ws}"
if [ ! -d "$WS" ]; then mkdir -p "$WS"; for i in 1 2 3 4 5 6; do printf 'line one of file %d\nline two\n' $i > "$WS/note$i.txt"; done; printf '(defun hello () "hi")\n' > "$WS/hello.lisp"; printf '(defun bye () "bye")\n' > "$WS/bye.lisp"; printf 'x = 1\n' > "$WS/a.py"; git -C "$WS" init -q; fi
BIN="$(cd "$HERE/../.." && pwd)/nodecode"
STAMP=$(date -u +%Y%m%dT%H%M%SZ)
RUN="$S/$STAMP-$LABEL"; mkdir -p "$RUN/frames"
SESS="ncprobe-$STAMP"
PHASE="$RUN/phase.log"
now() { date +%s.%N; }
echo "run=$RUN"
tmux new-session -d -s "$SESS" -x 120 -y 40 -c "$WS" \
  "env NODECODE_PHASE_LOG=$PHASE TMPDIR=$RUN NCPROBE=$STAMP $BIN --ephemeral; sleep 5"
T_LAUNCH=$(now)
# wait for the Ready placeholder; the shell (PID, which names the ephemeral
# store) runs the organism (ORG, where the turn runs) in a process of its own,
# the one whose environment carries NODECODE_FRONT
PID=""; ORG=""
for i in $(seq 1 600); do
  pane=$(tmux capture-pane -p -t "$SESS" 2>/dev/null || true)
  if [ -z "$PID" ] || [ -z "$ORG" ]; then
    for p in /proc/[0-9]*; do
      [ -r "$p/environ" ] || continue
      env=$(tr '\0' '\n' < "$p/environ" 2>/dev/null || true)
      if printf '%s\n' "$env" | grep -q "^NCPROBE=$STAMP$"; then
        exe=$(readlink "$p/exe" 2>/dev/null || true)
        case "$exe" in *nodecode*)
          if printf '%s\n' "$env" | grep -q '^NODECODE_FRONT='; then ORG=${p#/proc/}; else PID=${p#/proc/}; fi;;
        esac
      fi
    done
  fi
  if [ -n "$ORG" ] && printf '%s' "$pane" | grep -q 'Ready'; then break; fi
  sleep 0.1
done
T_READY=$(now)
[ -n "$PID" ] && [ -n "$ORG" ] || { echo "no probe pids found (shell $PID, organism $ORG)" >&2; tmux kill-session -t "$SESS"; exit 1; }
echo "pid=$PID org=$ORG exe=$(readlink /proc/$ORG/exe) ready_after=$(echo "$T_READY - $T_LAUNCH" | bc)s"
# cpu sampler, on the organism: the turn runs there
( while [ -d /proc/$ORG ]; do
    read -r _ _ _ _ _ _ _ _ _ _ _ _ _ ut st _ < /proc/$ORG/stat 2>/dev/null || break
    rss=$(awk '/VmRSS/{print $2}' /proc/$ORG/status 2>/dev/null || echo 0)
    echo "$(now) $ut $st $rss" >> "$RUN/cpu.log"; sleep 0.2; done ) &
CPU_SAMPLER=$!
# settle 2 s so the boot finishes (gateway, cells)
sleep 2
DB="$RUN/nodecode-ephemeral-$PID.db"
# terminal facts of the turns whose input is exactly $1
turn_done() {
  local q="${1//\'/\'\'}"
  sqlite3 "file:$DB?mode=ro" "select count(*) from events where kind in ('turn.completed','turn.failed','turn.cancelled') and turn_id in (select turn_id from events where kind='turn.input_committed' and json_extract(payload,'$.message')='$q')" 2>/dev/null || echo 0
}
if [ -n "${PROBE_PRELUDE:-}" ]; then
  tmux send-keys -t "$SESS" -l "$PROBE_PRELUDE"; sleep 0.3; tmux send-keys -t "$SESS" Enter
  for i in $(seq 1 3000); do
    [ -f "$DB" ] && [ "$(turn_done "$PROBE_PRELUDE")" -ge 1 ] && break
    sleep 0.1
  done
  echo "prelude done after $i ticks"
  sleep 1
fi
tmux send-keys -t "$SESS" -l "$PROMPT"
sleep 0.3
T_ENTER=$(now)
tmux send-keys -t "$SESS" Enter
echo "t_enter=$T_ENTER"
# frame sampler + completion poll
last=""; n=0; T_DONE=""
for i in $(seq 1 6000); do
  pane=$(tmux capture-pane -p -t "$SESS" 2>/dev/null || true)
  h=$(printf '%s' "$pane" | md5sum | cut -c1-12)
  if [ "$h" != "$last" ]; then
    n=$((n+1)); last=$h
    printf '%s %s\n' "$(now)" "$h" >> "$RUN/frames.log"
    printf '%s' "$pane" > "$RUN/frames/$(printf '%04d' $n).txt"
  fi
  if [ -f "$DB" ] && [ $((i % 5)) -eq 0 ]; then
    if [ "$(turn_done "$PROMPT")" -ge 1 ]; then T_DONE=$(now); break; fi
  fi
  if [ $((i % 50)) -eq 0 ] && [ -f "$DB" ]; then
    pr=$(sqlite3 "file:$DB?mode=ro" "select count(*) from events where kind='turn.provider_request'" 2>/dev/null || echo 0)
    el=$(echo "$(now) - $T_ENTER" | bc)
    if [ "${pr:-0}" -eq 0 ] && [ "${el%.*}" -ge 300 ]; then echo "stall: no provider round after ${el}s; aborting"; break; fi
  fi
  sleep 0.1
done
echo "t_done=$T_DONE frames=$n"
# let the divider settle, capture the final frame, then exit
sleep 1.5
tmux capture-pane -p -t "$SESS" > "$RUN/final-frame.txt"
tmux send-keys -t "$SESS" -l "/exit"; sleep 0.2; tmux send-keys -t "$SESS" Enter
for i in $(seq 1 100); do [ -d /proc/$PID ] || break; sleep 0.1; done
[ -d /proc/$PID ] && { echo "pid $PID still alive after /exit; leaving it"; }
kill $CPU_SAMPLER 2>/dev/null || true
tmux kill-session -t "$SESS" 2>/dev/null || true
for ext in "" -wal -shm; do [ -f "$DB$ext" ] && cp "$DB$ext" "$RUN/store.db$ext"; done; true
cat > "$RUN/meta.json" <<JSON
{"label":"$LABEL","prompt":$(printf '%s' "$PROMPT" | python3 -c 'import json,sys;print(json.dumps(sys.stdin.read()))'),
 "workdir":"$WS","binary":"$BIN","binary_mtime":"$(stat -c %y $BIN)","git_head":"$(git -C "$HERE/../.." rev-parse --short HEAD)",
 "pid":$PID,"t_launch":$T_LAUNCH,"t_ready":$T_READY,"t_enter":$T_ENTER,"t_done":${T_DONE:-null}}
JSON
echo "done: $RUN"
