#!/usr/bin/env python3
"""exec-stress.py -- background evaluations under load, watched the way the
web page watches them.

Usage: exec-stress.py BINARY [--sessions N] [--budget SECONDS] [--serve] [--keep]

BINARY is the nodecode executable under proof. It serves headless (`serve') in
a scratch home -- updates off, the web cell alone -- against a chat provider
played here. Each session is sent one prompt; the provider answers it with one
eval call per workload below, each handed off after half a second
(yield_time_ms), so every workload runs as a background evaluation while the
session goes idle. SESSIONS sessions run the same mix at once.

A WebSocket client subscribes every session's rows -- session_rows, what the
page draws -- and holds the proof to what a person watching would expect:

  - every workload shows as a running exec, and the ones that write show what
    they wrote while they run (the preview), silent ones included as running;
  - the long sleep is interrupted from the client and leaves within seconds;
  - every exec is gone once its command ends, and each exit wakes its session
    as a tool result -- the wake turn's (eval-status ID) reads -- never as a
    user message;
  - the frames stay bounded (size, rate) and the gateway stays up.

Exits 1 when a claim fails, printing what the client saw; 2 when the proof
cannot run. --serve keeps the scratch gateway up afterwards and prints the
web page's link, so the same load can be watched in a browser.
"""

import argparse
import base64
import http.server
import json
import os
import re
import select
import shutil
import socket
import struct
import subprocess
import sys
import tempfile
import threading
import time
import urllib.request

# Name -> shell command. Each is one kind of load a band has to carry.
WORKLOADS = {
    "steady": "for i in $(seq 1 40); do echo \"steady $i\"; sleep 0.25; done",
    "burst": "seq 1 60000; sleep 6",
    "flood": "yes flood-line-of-output | head -c 20000000",
    "ansi": "for i in $(seq 1 16); do printf '\\033[3%dmcolour %d\\033[0m \\342\\234\\223 \\303\\274n\\303\\257c\\303\\266d\\303\\251\\n' $((i % 7)) $i;"
            " printf '\\rprogress %d%%' $((i * 6)); sleep 0.5; done; echo",
    "stderr": "for i in $(seq 1 30); do echo \"out $i\"; echo \"err $i\" >&2; sleep 0.3; done",
    "fails": "sleep 3; echo boom >&2; exit 3",
    "silent": "sleep 12",
    "interrupt": "sleep 600",
}
# What names each workload in the band's command preview, which may be cut.
MARKS = {"steady": "steady", "burst": "seq 1 60000", "flood": "yes flood", "ansi": "colour",
         "stderr": "err $i", "fails": "boom", "silent": "sleep 12", "interrupt": "sleep 600"}
# The workloads that write while they run: their preview must show it.
WRITERS = ("steady", "burst", "ansi", "stderr")
FRAME_LIMIT = 256 * 1024
RATE_LIMIT = 60  # session_rows frames a second, per session, at the worst second


def die(message, code=2):
    print("exec-stress: " + message, file=sys.stderr)
    sys.exit(code)


class Provider(http.server.ThreadingHTTPServer):
    """The chat-completions endpoint the gateway is pointed at."""
    daemon_threads = True

    def __init__(self):
        super().__init__(("127.0.0.1", 0), Handler)
        self.calls = 0
        self.lock = threading.Lock()


class Handler(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, *args):
        pass

    def do_GET(self):
        body = json.dumps({"object": "list", "data": [{"id": "mock-1", "object": "model"}]}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_POST(self):
        request = json.loads(self.rfile.read(int(self.headers.get("Content-Length", 0))))
        messages = request.get("messages", [])
        last = messages[-1] if messages else {}
        prompt = next((m for m in reversed(messages) if m.get("role") == "user"), {}).get("content") or ""
        if isinstance(prompt, list):
            prompt = " ".join(part.get("text", "") for part in prompt if isinstance(part, dict))
        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.send_header("Connection", "close")
        self.end_headers()

        def chunk(delta, finish=None):
            obj = {"id": "c", "object": "chat.completion.chunk", "model": "mock-1",
                   "choices": [{"index": 0, "delta": delta, "finish_reason": finish}]}
            self.wfile.write(b"data: " + json.dumps(obj).encode() + b"\n\n")
            self.wfile.flush()

        chunk({"role": "assistant", "content": ""})
        if last.get("role") != "tool" and "exec-stress" in prompt:
            calls = []
            for index, (name, command) in enumerate(WORKLOADS.items()):
                with self.server.lock:
                    self.server.calls += 1
                    call_id = "call_%d" % self.server.calls
                form = "(sh %s)" % json.dumps(command)
                calls.append({"index": index, "id": call_id, "type": "function",
                              "function": {"name": "eval",
                                           "arguments": json.dumps({"form": form, "yield_time_ms": 500})}})
            chunk({"tool_calls": calls})
            chunk({}, "tool_calls")
        else:
            chunk({"content": "Noted."})
            chunk({}, "stop")
        usage = {"id": "c", "object": "chat.completion.chunk", "model": "mock-1", "choices": [],
                 "usage": {"prompt_tokens": 1000, "completion_tokens": 10, "total_tokens": 1010}}
        self.wfile.write(b"data: " + json.dumps(usage).encode() + b"\n\ndata: [DONE]\n\n")
        self.wfile.flush()


class Socket:
    """The least WebSocket client the gateway speaks to: text frames, masked
    out, fragments joined, pings answered and sent."""

    KEEPALIVE = 20

    def __init__(self, port, path="/gateway"):
        self.sock = socket.create_connection(("127.0.0.1", port))
        key = base64.b64encode(os.urandom(16)).decode()
        self.sock.sendall(("GET %s HTTP/1.1\r\nHost: 127.0.0.1:%d\r\nUpgrade: websocket\r\n"
                           "Connection: Upgrade\r\nSec-WebSocket-Key: %s\r\nSec-WebSocket-Version: 13\r\n\r\n"
                           % (path, port, key)).encode())
        head = b""
        while b"\r\n\r\n" not in head:
            data = self.sock.recv(4096)
            if not data:
                die("the gateway closed the upgrade")
            head += data
        if b" 101 " not in head.split(b"\r\n", 1)[0]:
            die("upgrade refused: " + head.split(b"\r\n", 1)[0].decode(errors="replace"))
        self.buffer = head.split(b"\r\n\r\n", 1)[1]
        self.bytes_in = 0
        self.sent_at = time.time()

    def _read(self, n):
        while len(self.buffer) < n:
            data = self.sock.recv(65536)
            if not data:
                raise EOFError
            self.buffer += data
        out, self.buffer = self.buffer[:n], self.buffer[n:]
        return out

    def _send(self, opcode, payload):
        mask = os.urandom(4)
        head = bytes([0x80 | opcode])
        n = len(payload)
        if n < 126:
            head += bytes([0x80 | n])
        elif n < 65536:
            head += bytes([0x80 | 126]) + struct.pack(">H", n)
        else:
            head += bytes([0x80 | 127]) + struct.pack(">Q", n)
        self.sock.sendall(head + mask + bytes(b ^ mask[i % 4] for i, b in enumerate(payload)))
        self.sent_at = time.time()

    def send(self, obj):
        self._send(1, json.dumps(obj).encode())

    def recv(self, timeout):
        """The next text message as JSON, or None when TIMEOUT passes before
        one begins. A message once begun is read whole. Without this, a timeout
        inside a frame would drop the header already read, and the stream after
        it would parse out of step. And the client pings every KEEPALIVE seconds,
        as the shell's client does: the gateway reads each connection under a
        75 s timeout and pings only a peer whose stream went quiet, so a
        client that sends nothing while turns streamed on would be hung up on."""
        if time.time() - self.sent_at > self.KEEPALIVE:
            self._send(9, b"keepalive")
        if not self.buffer and not select.select([self.sock], [], [], timeout)[0]:
            return None
        message = b""
        while True:
            b0, b1 = self._read(2)
            n = b1 & 0x7F
            if n == 126:
                n = struct.unpack(">H", self._read(2))[0]
            elif n == 127:
                n = struct.unpack(">Q", self._read(8))[0]
            payload = self._read(n)
            opcode = b0 & 0x0F
            if opcode == 9:
                self._send(10, payload)
                continue
            if opcode == 10:
                continue
            if opcode == 8:
                raise EOFError
            message += payload
            if b0 & 0x80:
                self.bytes_in += len(message)
                return json.loads(message)


def wake_reads(rows):
    """The exit wakes' reads among ROWS: (eval-status ID) calls the harness
    issued, each a tool row carrying what the exited evaluation left."""
    return [row for row in rows.values()
            if row.get("kind") == "tool_call" and "(eval-status" in ((row.get("tool") or {}).get("args") or "")]


def rss_mb(pid):
    try:
        with open("/proc/%d/status" % pid) as status:
            for line in status:
                if line.startswith("VmRSS:"):
                    return int(line.split()[1]) / 1024.0
    except OSError:
        return None


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("binary")
    parser.add_argument("--sessions", type=int, default=2)
    parser.add_argument("--budget", type=float, default=90.0, help="seconds the load may take")
    parser.add_argument("--serve", action="store_true", help="keep the gateway up afterwards")
    parser.add_argument("--keep", action="store_true", help="leave the scratch home behind")
    args = parser.parse_args()
    binary = os.path.abspath(args.binary)
    if not os.access(binary, os.X_OK):
        die("no executable at " + binary)
    home = tempfile.mkdtemp(prefix="nodecode-exec-stress-")
    work = os.path.join(home, "work")
    os.makedirs(os.path.join(home, ".nodecode"))
    os.makedirs(work)
    provider = Provider()
    threading.Thread(target=provider.serve_forever, daemon=True).start()
    env = {k: v for k, v in os.environ.items()
           if k not in ("NODECODE_HOME", "XDG_CACHE_HOME", "XDG_CONFIG_HOME", "XDG_DATA_HOME")}
    env.update(HOME=home, XDG_CACHE_HOME=os.path.join(home, ".cache"), LANG="C.UTF-8",
               DISPLAY="", WAYLAND_DISPLAY="")
    with open(os.path.join(home, ".nodecode", "config.jsonc"), "w") as config:
        json.dump({"general": {"default_model": {"provider": "mock", "model": "mock-1"}},
                   "providers": {"mock": {"base_url": "http://127.0.0.1:%d/v1" % provider.server_port,
                                          "sdk": "openai-completions", "api_key": "exec-stress",
                                          "models": [{"id": "mock-1", "context_window": 200000}]}},
                   "update": {"mode": "off"}}, config)
    serve = None
    try:
        form = ('(let ((root (merge-pathnames ".nodecode/cells/" (user-homedir-pathname)))) '
                '(dolist (entry (nlk::shipped-cells)) (when (equal "nodecode-web" (nlk:offer-name entry)) '
                '(nlk::install-cell (nlk:offer-name entry) :root root))) '
                '(length (directory (merge-pathnames "*/" root))))')
        out = subprocess.run([binary, "trial"], input=form + "\n", env=env, cwd=home,
                             capture_output=True, text=True, timeout=300).stdout
        if "nodecode-trial-score 1" not in out:
            die("the web cell did not install:\n" + out[-1500:])
        serve = subprocess.Popen([binary, "serve"], env=env, cwd=work,
                                 stdout=open(os.path.join(home, "serve.log"), "w"),
                                 stderr=subprocess.STDOUT, start_new_session=True)
        link = None
        for _ in range(240):
            time.sleep(1)
            if serve.poll() is not None:
                die("the gateway exited: see " + os.path.join(home, "serve.log"))
            web = subprocess.run([binary, "web"], env=env, cwd=work, capture_output=True, text=True, timeout=60)
            link = next((word for word in web.stdout.split() if "#t=" in word), None)
            if link:
                break
        if not link:
            die("no web link: " + web.stdout[-500:] + web.stderr[-500:])
        port = int(re.search(r"127\.0\.0\.1:(\d+)", link).group(1))
        token = link.split("#t=", 1)[1]
        print("exec-stress: gateway pid %d on :%d, %d sessions x %d workloads"
              % (serve.pid, port, args.sessions, len(WORKLOADS)))

        sessions = ["s-stress%d" % i for i in range(args.sessions)]
        for sid in sessions:
            request = urllib.request.Request(
                "http://127.0.0.1:%d/api/gateway/sessions" % port,
                data=json.dumps({"session_id": sid, "cwd": work}).encode(), method="POST",
                headers={"Authorization": "Bearer " + token, "Content-Type": "application/json"})
            urllib.request.urlopen(request, timeout=30).read()

        ws = Socket(port)
        challenge = ws.recv(30)
        ws.send({"type": "gateway_connect", "protocol_min": challenge["protocol_min"],
                 "protocol_max": challenge["protocol_max"], "challenge_nonce": challenge["challenge_nonce"],
                 "client": {"id": "exec-stress", "version": "0", "mode": "web", "platform": "script",
                            "instance_id": "exec-stress-1"},
                 "auth": {"operator_token": token}, "requested_scopes": ["sync.read", "sync.write"],
                 "transport": {"forwarded": False, "secure_channel_observed": False}})
        ack = ws.recv(30)
        if not ack or ack.get("type") != "gateway_connect_ack":
            die("handshake refused: %r" % ack)
        ws.send({"type": "client_command", "command": {"type": "set_session_subscriptions",
                                                       "subscriptions": [{"session_id": s, "rows": True}
                                                                         for s in sessions]}})
        for sid in sessions:
            command_id = "command-stress-" + sid
            ws.send({"type": "client_command", "command": {
                "type": "start_turn", "command_id": command_id, "correlation_id": command_id,
                "request": {"session_id": sid, "generate_response": True,
                            "content": [{"type": "text", "text": "exec-stress: run the workloads"}]}}})

        state = {sid: {"execs": [], "turn": {}, "rows": {}, "seen": {}, "frames": 0, "bytes": 0,
                       "max_frame": 0, "per_second": {}, "max_execs": 0} for sid in sessions}
        interrupts = {}  # sid -> (sent_at, exec_id, gone_at)
        results = []
        started = time.time()
        rss_peak = 0.0
        while time.time() - started < args.budget:
            message = ws.recv(1.0)
            now = time.time() - started
            rss = rss_mb(serve.pid) or 0.0
            rss_peak = max(rss_peak, rss)
            if message is None:
                pass
            else:
                payload = message.get("payload") or {}
                kind = payload.get("type")
                if kind == "gateway_turn_command_result":
                    results.append(payload.get("result", {}))
                elif kind == "session_rows" and payload.get("session_id") in state:
                    s = state[payload["session_id"]]
                    size = len(json.dumps(payload))
                    s["frames"] += 1
                    s["bytes"] += size
                    s["max_frame"] = max(s["max_frame"], size)
                    second = int(now)
                    s["per_second"][second] = s["per_second"].get(second, 0) + 1
                    if payload.get("reset"):
                        s["rows"] = {}
                    for row in payload.get("rows", []):
                        held = s["rows"].get(row["id"], {})
                        if "append" in row:
                            row["text"] = held.get("text", "") + row["append"]
                        s["rows"][row["id"]] = row
                    for gone in payload.get("gone", []):
                        s["rows"].pop(gone, None)
                    if "turn" in payload:
                        s["turn"] = payload["turn"]
                    if "execs" in payload:
                        s["execs"] = payload["execs"]
                        s["max_execs"] = max(s["max_execs"], len(s["execs"]))
                        for exec_ in s["execs"]:
                            name = next((n for n, mark in MARKS.items() if mark in exec_.get("command", "")), None)
                            if not name:
                                continue
                            seen = s["seen"].setdefault(name, {"first": now, "tail": False, "id": None})
                            seen["last"] = now
                            seen["id"] = exec_.get("exec_id", seen["id"])
                            if exec_.get("tail"):
                                seen["tail"] = True
                                seen["tail_text"] = exec_["tail"][-160:]
            # The long sleep is interrupted the moment the client can name it.
            for sid in sessions:
                s = state[sid]
                target = s["seen"].get("interrupt")
                live = [e for e in s["execs"] if e.get("exec_id") == (target or {}).get("id")]
                if target and target["id"] is not None and sid not in interrupts and live:
                    command_id = "command-interrupt-" + sid
                    ws.send({"type": "client_command", "command": {
                        "type": "interrupt_exec", "command_id": command_id, "correlation_id": command_id,
                        "session_id": sid, "exec_id": target["id"]}})
                    interrupts[sid] = [now, target["id"], None]
                if sid in interrupts and interrupts[sid][2] is None and not any(
                        e.get("exec_id") == interrupts[sid][1] for e in s["execs"]):
                    interrupts[sid][2] = now
            wakes = {sid: len(wake_reads(state[sid]["rows"])) for sid in sessions}
            done = all(not state[sid]["execs"] and not state[sid]["turn"].get("active")
                       and len(state[sid]["seen"]) == len(WORKLOADS) and wakes[sid] >= 1
                       and sid in interrupts for sid in sessions)
            if done and now > 20:
                break
        elapsed = time.time() - started

        failures = []
        print("\nexec-stress: %.1fs, gateway peak RSS %.0f MB, %.1f MB read"
              % (elapsed, rss_peak, ws.bytes_in / 1e6))
        for sid in sessions:
            s = state[sid]
            worst = max(s["per_second"].values() or [0])
            wakes = len(wake_reads(s["rows"]))
            posed = [row for row in s["rows"].values() if row.get("kind") == "text" and row.get("role") == "user"
                     and "background eval" in (row.get("text") or "")]
            print("\n%s: %d frames, %.0f KB, largest %.1f KB, worst %d/s, most at once %d, exits read %d"
                  % (sid, s["frames"], s["bytes"] / 1024, s["max_frame"] / 1024, worst, s["max_execs"], wakes))
            for name in WORKLOADS:
                seen = s["seen"].get(name)
                if not seen:
                    print("  %-9s never shown" % name)
                    failures.append("%s: %s never showed as running" % (sid, name))
                    continue
                print("  %-9s shown %5.1fs-%5.1fs  preview %-3s %s" % (
                    name, seen["first"], seen.get("last", seen["first"]), "yes" if seen["tail"] else "no",
                    (seen.get("tail_text") or "").replace("\n", " | ")[-70:]))
                if name in WRITERS and not seen["tail"]:
                    failures.append("%s: %s wrote but its preview stayed empty" % (sid, name))
            if s["execs"]:
                failures.append("%s: %d execs still shown at the end" % (sid, len(s["execs"])))
            if s["max_frame"] > FRAME_LIMIT:
                failures.append("%s: a rows frame of %d bytes" % (sid, s["max_frame"]))
            if worst > RATE_LIMIT:
                failures.append("%s: %d rows frames in one second" % (sid, worst))
            if wakes < 1:
                failures.append("%s: no exit woke the session" % sid)
            if posed:
                failures.append("%s: an exit reached the transcript as a user message" % sid)
            interrupt = interrupts.get(sid)
            if not interrupt:
                failures.append("%s: the long sleep was never interrupted" % sid)
            elif interrupt[2] is None or interrupt[2] - interrupt[0] > 5:
                failures.append("%s: the interrupted exec stayed %s" % (
                    sid, "shown" if interrupt[2] is None else "%.1fs" % (interrupt[2] - interrupt[0])))
            else:
                print("  interrupt left in %.1fs" % (interrupt[2] - interrupt[0]))
        refused = [r for r in results if r.get("status") == "rejected"]
        if refused:
            failures.append("commands refused: %s" % [(r.get("command"), r.get("code")) for r in refused])
        if serve.poll() is not None:
            failures.append("the gateway exited during the load")
        if failures:
            print("\nexec-stress: FAILED")
            for failure in failures:
                print("  - " + failure)
        else:
            print("\nexec-stress: every claim held")
        if args.serve:
            print("\nexec-stress: serving; open %s  (send `exec-stress' in a session to run the mix again; Ctrl+C ends)"
                  % link)
            try:
                serve.wait()
            except KeyboardInterrupt:
                pass
        return 1 if failures else 0
    finally:
        if serve and serve.poll() is None:
            os.killpg(serve.pid, 15)
            try:
                serve.wait(timeout=20)
            except subprocess.TimeoutExpired:
                os.killpg(serve.pid, 9)
        provider.shutdown()
        if args.keep or args.serve:
            print("exec-stress: scratch home kept at " + home)
        else:
            shutil.rmtree(home, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
