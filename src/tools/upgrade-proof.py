#!/usr/bin/env python3
"""upgrade-proof.py -- a running gateway of the last release lands on the next one.

Usage: upgrade-proof.py CANDIDATE [--from PREVIOUS] [--host URL] [--budget SECONDS] [--keep]

CANDIDATE is the release tarball under proof, nodecode-<label>-<arch>-<os>.tar.gz
(pack.sh's output), its .sha256 beside it or made here. PREVIOUS is the build a
user runs when the candidate is published: a tarball, a label, or `stable'
(the default) -- the label the release host's stable pointer names now, which
before a publish is the release the candidate replaces. A label is fetched,
with its sidecar, from --host.

A fresh install fails the way no user does. Users run a gateway of the last
release, and the update reaches it while it serves. So here, in a scratch home
and a scratch account (no unit, no service manager it can reach):

  1. PREVIOUS is installed from a release host that is a folder (file://), with
     every shipped cell named, two model providers played here on localhost
     (an OpenAI-compatible one and an Anthropic one), and `update' on auto,
     looking every minute.
  2. Its gateway is started by hand -- `gateway run', no unit -- and a turn on
     each provider completes on it: the control.
  3. The folder's stable pointer moves to CANDIDATE.
  4. (a) Within the budget the gateway serving the store reports CANDIDATE's
     label (/api/gateway/identity): it took the release live, or restarted
     itself onto `current'. A gateway that refused the release and stays on
     PREVIOUS fails here.
     (b) A shell of CANDIDATE (`nodecode rpc') attaches and a turn on each
     provider completes.
     (c) One organism serves the home: no second gateway is left beside it.

Exits 0 when all three hold (or PREVIOUS is CANDIDATE: nothing to upgrade), 1
when one does not -- printing the timeline, the gateway's notices and the tail
of its log -- and 2 when the proof cannot run. Every process it started is
found by its scratch environment and stopped by pid: through /proc on Linux,
`ps -E' elsewhere (the release runs it on Linux only)."""

import argparse
import hashlib
import http.server
import json
import os
import platform
import re
import shutil
import signal
import subprocess
import sys
import tarfile
import tempfile
import threading
import time
import urllib.request

REPLY = "upgrade-proof ok"
LABEL = re.compile(r"^nodecode-([0-9a-f]{7,})-([a-z0-9_]+)-([a-z]+)\.tar\.gz$")


def say(text):
    print(f"upgrade-proof: {text}", flush=True)


class Timeline:
    def __init__(self):
        self.start = time.monotonic()
        self.rows = []

    def note(self, text):
        at = time.monotonic() - self.start
        self.rows.append((at, text))
        say(f"{at:6.1f}s  {text}")


# --- the providers -------------------------------------------------------------

class Provider(http.server.ThreadingHTTPServer):
    """One port, both wires: /v1/chat/completions streams an OpenAI-compatible
    answer, /v1/messages an Anthropic one; /v1/models lists one model each."""

    daemon_threads = True

    def __init__(self):
        super().__init__(("127.0.0.1", 0), Handler)
        self.served = {"openai": 0, "anthropic": 0}


class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass

    def send_stream(self):
        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.send_header("Cache-Control", "no-store")
        self.send_header("Connection", "close")
        self.end_headers()

    def do_GET(self):
        body = json.dumps({"object": "list", "data": [{"id": "mock-1", "object": "model"},
                                                     {"id": "mock-claude", "type": "model"}]}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_POST(self):
        length = int(self.headers.get("Content-Length") or 0)
        self.rfile.read(length)
        if self.path.rstrip("/").endswith("/messages"):
            self.server.served["anthropic"] += 1
            self.anthropic()
        else:
            self.server.served["openai"] += 1
            self.openai()

    def openai(self):
        self.send_stream()

        def chunk(obj):
            self.wfile.write(b"data: " + json.dumps(obj).encode() + b"\n\n")

        base = {"id": "c1", "object": "chat.completion.chunk", "model": "mock-1"}
        chunk(dict(base, choices=[{"index": 0, "delta": {"role": "assistant", "content": REPLY}}]))
        chunk(dict(base, choices=[{"index": 0, "delta": {}, "finish_reason": "stop"}]))
        chunk(dict(base, choices=[], usage={"prompt_tokens": 10, "completion_tokens": 3,
                                            "total_tokens": 13}))
        self.wfile.write(b"data: [DONE]\n\n")

    def anthropic(self):
        self.send_stream()

        def event(kind, obj):
            obj["type"] = kind
            self.wfile.write(f"event: {kind}\ndata: ".encode() + json.dumps(obj).encode() + b"\n\n")

        event("message_start", {"message": {"id": "msg_1", "type": "message", "role": "assistant",
                                            "model": "mock-claude", "content": [],
                                            "stop_reason": None, "stop_sequence": None,
                                            "usage": {"input_tokens": 10, "output_tokens": 1}}})
        event("content_block_start", {"index": 0, "content_block": {"type": "text", "text": ""}})
        event("content_block_delta", {"index": 0, "delta": {"type": "text_delta", "text": REPLY}})
        event("content_block_stop", {"index": 0})
        event("message_delta", {"delta": {"stop_reason": "end_turn", "stop_sequence": None},
                                "usage": {"output_tokens": 3}})
        event("message_stop", {})


PROVIDERS = (("mock", "mock-1", "openai"), ("mockclaude", "mock-claude", "anthropic"))


def provider_config(port):
    base = f"http://127.0.0.1:{port}/v1"
    return {"mock": {"base_url": base, "sdk": "openai-completions", "api_key": "upgrade-proof",
                     "models": [{"id": "mock-1", "context_window": 200000}]},
            "mockclaude": {"base_url": base, "sdk": "anthropic", "api_key": "upgrade-proof",
                           "models": [{"id": "mock-claude", "context_window": 200000}]}}


# --- releases -------------------------------------------------------------------

def platform_name():
    system = platform.system().lower()
    machine = platform.machine().lower()
    arch = {"x86_64": "x86_64", "amd64": "x86_64", "arm64": "arm64", "aarch64": "arm64"}.get(machine)
    if system not in ("linux", "darwin") or arch is None:
        return None
    return f"{arch}-{system}"


def sha256_of(path):
    digest = hashlib.sha256()
    with open(path, "rb") as f:
        for block in iter(lambda: f.read(1 << 20), b""):
            digest.update(block)
    return digest.hexdigest()


def take_tarball(path, mirror):
    """Copy a release tarball and its sidecar into the mirror => its label."""
    name = os.path.basename(path)
    match = LABEL.match(name)
    if not match:
        raise RuntimeError(f"{name} is not named nodecode-<label>-<arch>-<os>.tar.gz")
    shutil.copy(path, os.path.join(mirror, name))
    sidecar = path + ".sha256"
    if os.path.exists(sidecar):
        shutil.copy(sidecar, os.path.join(mirror, name + ".sha256"))
    else:
        with open(os.path.join(mirror, name + ".sha256"), "w") as f:
            f.write(f"{sha256_of(path)}  {name}\n")
    return match.group(1)


def fetch(url, path=None):
    with urllib.request.urlopen(url, timeout=120) as answer:
        body = answer.read()
    if path is None:
        return body.decode().strip()
    with open(path, "wb") as f:
        f.write(body)
    return path


def take_previous(spec, host, plat, mirror, scratch):
    if os.path.exists(spec):
        return take_tarball(spec, mirror)
    label = fetch(f"{host}/stable-{plat}") if spec == "stable" else spec
    if not re.fullmatch(r"[0-9a-f]{7,}", label):
        raise RuntimeError(f"{spec} named no release label: {label!r}")
    name = f"nodecode-{label}-{plat}.tar.gz"
    local = os.path.join(scratch, name)
    fetch(f"{host}/{name}", local)
    fetch(f"{host}/{name}.sha256", local + ".sha256")
    return take_tarball(local, mirror)


def installer_of(tarball, scratch):
    """The install.sh the tarball itself carries: the one a user of it had."""
    with tarfile.open(tarball) as tar:
        member = next(m for m in tar.getmembers() if m.name.endswith("/install.sh"))
        member.name = os.path.basename(member.name)
        tar.extract(member, scratch)
    return os.path.join(scratch, "install.sh")


# --- the scratch account --------------------------------------------------------

def scratch_env(root):
    """A whole account of its own: no HOME, cache, config or service manager of
    the box's. The bus and runtime dir lead nowhere, so no `systemctl --user'
    or launchctl a build runs can reach a real unit."""
    env = {k: v for k, v in os.environ.items()
           if not k.startswith(("NODECODE_", "XDG_", "DBUS_", "SBCL"))}
    user = os.path.join(root, "user")
    env.update(HOME=user, XDG_CONFIG_HOME=os.path.join(user, ".config"),
               XDG_CACHE_HOME=os.path.join(root, "cache"),
               XDG_DATA_HOME=os.path.join(user, ".local", "share"),
               XDG_RUNTIME_DIR=os.path.join(root, "run"),
               DBUS_SESSION_BUS_ADDRESS="unix:path=" + os.path.join(root, "run", "no-bus"),
               LANG="C.UTF-8")
    for d in (env["XDG_CONFIG_HOME"], env["XDG_CACHE_HOME"], env["XDG_DATA_HOME"], env["XDG_RUNTIME_DIR"]):
        os.makedirs(d, mode=0o700, exist_ok=True)
    return env


def ours(root):
    """=> {pid: argv} of every process whose environment names this run's
    scratch cache: everything the proof started, and what those started."""
    marker = ("XDG_CACHE_HOME=" + os.path.join(root, "cache")).encode()
    found = {}
    if os.path.isdir("/proc"):
        for name in os.listdir("/proc"):
            if not name.isdigit() or int(name) == os.getpid():
                continue
            try:
                with open(f"/proc/{name}/environ", "rb") as f:
                    if marker not in f.read().split(b"\0"):
                        continue
                with open(f"/proc/{name}/cmdline", "rb") as f:
                    argv = [a.decode(errors="replace") for a in f.read().split(b"\0") if a]
            except OSError:
                continue
            found[int(name)] = argv
    else:
        # macOS: ps shows the environment of one's own processes with -E.
        out = subprocess.run(["ps", "-E", "-ww", "-o", "pid=,command=", "-U", str(os.getuid())],
                             capture_output=True, text=True).stdout
        for line in out.splitlines():
            pid, _, command = line.strip().partition(" ")
            if pid.isdigit() and int(pid) != os.getpid() and marker.decode() in command:
                found[int(pid)] = command.split(" ")[:4]
    return found


def organisms(root):
    """The processes serving a store: `gateway run' and `serve'."""
    return {pid: argv for pid, argv in ours(root).items()
            if "serve" in argv[1:3] or argv[1:3] == ["gateway", "run"]}


def stop_all(root):
    for sig in (signal.SIGTERM, signal.SIGKILL):
        pids = ours(root)
        for pid in pids:
            try:
                os.kill(pid, sig)
            except OSError:
                pass
        deadline = time.monotonic() + 15
        while pids and time.monotonic() < deadline:
            time.sleep(0.5)
            pids = ours(root)
        if not pids:
            return


# --- the gateway, seen from outside -----------------------------------------------

def identity(home):
    """The serving gateway's /api/gateway/identity, or None."""
    try:
        with open(os.path.join(home, "history.db.owner")) as f:
            port = json.load(f)["port"]
        with open(os.path.join(home, "operator-token")) as f:
            token = f.read().strip()
        request = urllib.request.Request(f"http://127.0.0.1:{port}/api/gateway/identity",
                                         headers={"Authorization": f"Bearer {token}"})
        with urllib.request.urlopen(request, timeout=5) as answer:
            return json.load(answer)
    except (OSError, ValueError, KeyError):
        return None


def label_of(version):
    return (version or "").rpartition("+")[2]


def shipped_cells(release):
    """The cell names a release folder ships (its src/cells/bundle.json)."""
    with open(os.path.join(release, "src", "cells", "bundle.json")) as f:
        paths = json.load(f)["cells"].values()
    return [os.path.basename(p) for p in paths if os.path.basename(p).startswith("nodecode-")]


def cell_releases(home, releases):
    """=> the release labels the home's shipped-cell links point into."""
    labels = set()
    folder = os.path.join(home, "cells")
    for name in sorted(os.listdir(folder)) if os.path.isdir(folder) else ():
        target = os.path.realpath(os.path.join(folder, name))
        if target.startswith(os.path.realpath(releases) + os.sep):
            labels.add(os.path.relpath(target, os.path.realpath(releases)).split(os.sep)[0])
    return sorted(labels)


def turn(binary, env, cwd, provider, model, server, wire, timeout=120):
    """One turn through `BINARY rpc' => (ok, detail): ended completed, with
    the reply the provider played, after a call on WIRE."""
    before = server.served[wire]
    line = json.dumps({"type": "start_turn",
                       "request": {"prompt": "Say ok.", "provider": provider, "model": model}})
    proc = subprocess.Popen([binary, "rpc"], env=env, cwd=cwd, stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    outcome = {}

    def read():
        for frame in proc.stdout:
            for kind in ("turn.completed", "turn.failed", "turn.cancelled"):
                if f'"{kind}"' in frame:
                    outcome.setdefault("kind", kind)
                    outcome.setdefault("frame", frame)
            if REPLY in frame:
                outcome["reply"] = True
            if "error" in frame and "text" not in outcome:
                found = re.search(r'"(?:message|error|reason)":"([^"]{8,300})"', frame)
                if found:
                    outcome["text"] = found.group(1)
            if "kind" in outcome:
                return

    reader = threading.Thread(target=read, daemon=True)
    reader.start()
    try:
        proc.stdin.write(line + "\n")
        proc.stdin.flush()
    except OSError:
        pass
    reader.join(timeout)
    try:
        proc.stdin.close()
    except OSError:
        pass
    try:
        proc.wait(timeout=30)
    except subprocess.TimeoutExpired:
        proc.kill()
        proc.wait()
    err = proc.stderr.read().strip().splitlines()
    kind = outcome.get("kind")
    if kind == "turn.completed" and outcome.get("reply") and server.served[wire] > before:
        return True, "completed"
    if kind == "turn.completed":
        return False, f"completed without the {wire} provider's reply"
    if kind is None:
        return False, ("no turn ended within %ds" % timeout) + (f"; rpc said: {err[-1]}" if err else "")
    return False, f"{kind}: {outcome.get('text') or outcome['frame'][:300]}"


def turns(binary, env, cwd, server, timeline, when):
    ok = True
    for provider, model, wire in PROVIDERS:
        passed, detail = turn(binary, env, cwd, provider, model, server, wire)
        timeline.note(f"{when} turn on {provider} ({model}): {detail}")
        ok = ok and passed
    return ok


# --- the proof ----------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("candidate", help="the release tarball under proof")
    parser.add_argument("--from", dest="previous", default="stable",
                        help="the build users run: a tarball, a label, or stable (default)")
    parser.add_argument("--host", default="https://nodecode.ai/dl", help="where a label is fetched")
    parser.add_argument("--budget", type=float, default=180.0,
                        help="seconds from the pointer's move for the gateway to report the candidate")
    parser.add_argument("--keep", action="store_true", help="keep the scratch directory")
    args = parser.parse_args()

    plat = platform_name()
    if plat is None:
        say(f"no release arm for {platform.system()} {platform.machine()}")
        return 2
    root = tempfile.mkdtemp(prefix="upgrade-proof-")
    mirror, home, work = (os.path.join(root, d) for d in ("mirror", "home", "work"))
    releases = os.path.join(root, "releases")
    for d in (mirror, home, work):
        os.makedirs(d)
    env = scratch_env(root)
    provider = Provider()
    threading.Thread(target=provider.serve_forever, daemon=True).start()
    timeline = Timeline()
    log_path = os.path.join(root, "gateway.log")
    try:
        try:
            candidate = take_tarball(os.path.abspath(args.candidate), mirror)
            previous = take_previous(args.previous, args.host, plat, mirror, root)
        except (RuntimeError, OSError, ValueError) as error:
            say(f"cannot lay out the two releases: {error}")
            return 2
        if previous == candidate:
            # A rehearsal of the commit already published: no user runs an
            # older build of it.
            say(f"SKIP: the previous release is the candidate itself ({candidate}): nothing to upgrade")
            return 0
        pointer = os.path.join(mirror, f"stable-{plat}")
        with open(pointer, "w") as f:
            f.write(previous + "\n")
        timeline.note(f"previous {previous}, candidate {candidate}, {plat}")

        # 1. The previous release, installed as its own installer installs it.
        installer = installer_of(os.path.join(mirror, f"nodecode-{previous}-{plat}.tar.gz"), root)
        install = subprocess.run(["sh", installer, "--no-modify-path"], cwd=work,
                                 env=dict(env, NODECODE_BASE_URL="file://" + mirror, NODECODE_HOME=releases),
                                 capture_output=True, text=True, timeout=600)
        if install.returncode != 0:
            say("installing the previous release failed:\n" + (install.stdout + install.stderr)[-2000:])
            return 2
        binary = os.path.join(releases, "current", "nodecode")
        organism_env = dict(env, NODECODE_HOME=home)
        with open(os.path.join(home, "config.jsonc"), "w") as f:
            json.dump({"general": {"default_model": {"provider": "mock", "model": "mock-1"}},
                       "providers": provider_config(provider.server_port),
                       "update": {"mode": "auto", "check_minutes": 1, "feed": "off"}}, f)
        # Every shipped cell on, as setup leaves a home a person runs: each one
        # a link into the release folder that installed it.
        cells = shipped_cells(os.path.join(releases, previous))
        for name in cells:
            added = subprocess.run([binary, "add", f"nc://{name}"], cwd=work, env=organism_env,
                                   stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=300)
            if added.returncode != 0:
                say(f"`add nc://{name}' failed on the previous release:\n" + (added.stdout + added.stderr)[-1000:])
                return 2
        timeline.note(f"installed {previous} with its {len(cells)} shipped cells on")

        # 2. Its gateway, by hand.
        with open(log_path, "w") as log:
            gateway = subprocess.Popen([binary, "gateway", "run"], cwd=work, env=organism_env,
                                       stdin=subprocess.DEVNULL, stdout=log, stderr=subprocess.STDOUT,
                                       start_new_session=True)
        deadline = time.monotonic() + 180
        seen = None
        while time.monotonic() < deadline and seen is None:
            if gateway.poll() is not None:
                say(f"the previous gateway exited ({gateway.returncode}) before it served")
                return 2
            time.sleep(1)
            seen = identity(home)
        if seen is None or label_of(seen["version"]) != previous:
            say(f"the previous gateway never served as {previous}: {seen}")
            return 2
        timeline.note(f"gateway pid {seen['pid']} serves as {seen['version']} (no unit); "
                      f"shipped-cell links point into {cell_releases(home, releases)}")
        if not turns(binary, organism_env, work, provider, timeline, "control:"):
            say("a turn failed on the previous release itself: the providers here, not the upgrade")
            return 2

        # 3. The release lands.
        with open(pointer, "w") as f:
            f.write(candidate + "\n")
        timeline.note(f"stable pointer moved to {candidate}")
        moved = time.monotonic()
        last = None
        said = set(seen.get("notices") or ())
        landed = False
        while time.monotonic() - moved < args.budget:
            time.sleep(2)
            seen = identity(home)
            state = None if seen is None else (seen["pid"], seen["version"], tuple(seen.get("notices") or ()))
            if state != last:
                current = os.path.basename(os.path.realpath(os.path.join(releases, "current")))
                fresh = [n for n in (seen or {}).get("notices") or () if n not in said]
                said.update(fresh)
                timeline.note("no gateway answers" if seen is None else
                              f"gateway pid {seen['pid']} reports {seen['version']} (build {seen['build']}); "
                              f"current -> {current}" + "".join(f"\n          notice: {n}" for n in fresh))
                last = state
            if seen is not None and label_of(seen["version"]) == candidate:
                landed = True
                break

        # 4. What a user meets.
        timeline.note(f"shipped-cell links point into {cell_releases(home, releases)}")
        failures = []
        if not landed:
            failures.append(f"(a) {args.budget:.0f}s after the pointer moved the gateway still reports "
                            f"{seen and seen['version']}, not {candidate}")
        if not turns(binary, organism_env, work, provider, timeline, "candidate shell:"):
            failures.append("(b) a turn through a candidate shell did not complete")
        serving = organisms(root)
        timeline.note("organisms serving the home: " +
                      (", ".join(f"{pid} ({' '.join(argv[1:3])})" for pid, argv in sorted(serving.items())) or "none"))
        if len(serving) != 1:
            failures.append(f"(c) {len(serving)} organisms serve the home, not one")
        if failures:
            with open(log_path, errors="replace") as log:
                tail = log.readlines()[-30:]
            say("gateway log, last lines:\n" + "".join(tail).rstrip())
            for failure in failures:
                say("FAIL: " + failure)
            return 1
        say(f"PASS: {previous} -> {candidate}: the running gateway reports the candidate, "
            f"a candidate shell's turns complete, one organism serves")
        return 0
    finally:
        stop_all(root)
        provider.shutdown()
        if args.keep:
            say(f"kept {root}")
        else:
            shutil.rmtree(root, ignore_errors=True)


if __name__ == "__main__":
    signal.signal(signal.SIGTERM, lambda signum, frame: sys.exit(128 + signum))
    sys.exit(main())
