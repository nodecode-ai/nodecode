#!/usr/bin/env python3
"""footprint.py -- what a working shell and its organism hold in memory, held
to ceilings.

Usage: footprint.py BINARY [--turns N] [--max-boot-mb MB] [--max-turn-mb MB]
                           [--max-growth-mb MB] [--max-idle-mb MB] [--keep]

BINARY is the nodecode executable under proof (Linux). It runs as a person
runs it: the interactive shell in a terminal (tmux), in a scratch HOME with
every shipped cell installed through the product's own INSTALL-CELL, from
the warm image its first launch publishes, talking to a local
OpenAI-compatible provider played here -- each answer a streamed reasoning
block and a long markdown reply, so the request grows turn by turn as a real
conversation's does. Nothing reaches a paid model.

The scratch HOME is made under $TMPDIR, or /var/tmp: on disk, as a person's
home is. The file system decides how much of the image a turn holds -- on
disk the kernel maps each fault's cached neighbours with it, and a turn held
35-43 MB of image pages where the same turn from tmpfs held 13-18.

Memory is read from outside every 100 ms, the shell's and its organism's
together -- the shell runs the organism in a process of its own, named by
the store's holder sidecar -- as proportional resident memory (Pss): the
two map one image file, and its pages the machine holds once are counted
once, where a VmRSS sum counted them twice. Read through four phases: the
boot, the quiet after it, N back-to-back turns, and the quiet after them.
Held to:
  --max-boot-mb    the most it holds in the twenty seconds after launch;
  --max-turn-mb    the most it holds at any instant while turns run, past the
                   first five (the boot's own pages are given back by then);
  --max-idle-mb    what it holds once a quiet minute follows the turns;
  --max-growth-mb  how much more the last ten turns hold than turns six to
                   fifteen -- what a conversation keeps, turn over turn.
It exits 1 when a ceiling is broken, naming each, and 2 when it cannot
measure (no tmux, no warm image, a turn never answered).
"""

import argparse
import glob
import http.server
import json
import os
import random
import shutil
import subprocess
import sys
import tempfile
import threading
import time

WORDS = ("memory heap collector nursery generation thread stack image page "
         "session transcript provider gateway organism layer catalog").split()


def text(n_words, seed):
    """A markdown answer of N_WORDS, paragraphs of twelve-word lines."""
    rand = random.Random(seed)
    lines, line = [], []
    for _ in range(n_words):
        line.append(rand.choice(WORDS))
        if len(line) == 12:
            lines.append(" ".join(line))
            line = []
            if rand.random() < 0.15:
                lines.append("")
    return "## Answer\n\n" + "\n".join(lines) + "\n"


class Provider(http.server.ThreadingHTTPServer):
    """The chat-completions endpoint the shell is pointed at."""
    daemon_threads = True

    def __init__(self):
        super().__init__(("127.0.0.1", 0), Handler)
        self.served = 0
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
        raw = self.rfile.read(int(self.headers.get("Content-Length", 0)))
        with self.server.lock:
            k = self.server.served + 1
        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.send_header("Connection", "close")
        self.end_headers()

        def chunk(delta, finish=None):
            obj = {"id": "c%d" % k, "object": "chat.completion.chunk", "model": "mock-1",
                   "choices": [{"index": 0, "delta": delta, "finish_reason": finish}]}
            self.wfile.write(b"data: " + json.dumps(obj).encode() + b"\n\n")

        chunk({"role": "assistant", "content": ""})
        reasoning = text(300, k * 7)
        for i in range(0, len(reasoning), 400):
            chunk({"reasoning_content": reasoning[i:i + 400]})
        answer = text(1400, k)
        for i in range(0, len(answer), 300):
            chunk({"content": answer[i:i + 300]})
            self.wfile.flush()
            time.sleep(0.002)
        chunk({}, "stop")
        usage = {"id": "c%d" % k, "object": "chat.completion.chunk", "model": "mock-1", "choices": [],
                 "usage": {"prompt_tokens": max(1, len(raw) // 4), "completion_tokens": len(answer) // 4,
                           "total_tokens": max(1, len(raw) // 4) + len(answer) // 4}}
        self.wfile.write(b"data: " + json.dumps(usage).encode() + b"\n\ndata: [DONE]\n\n")
        self.wfile.flush()
        with self.server.lock:
            self.server.served = k


def breakdown(pid):
    """Where PID's proportional resident MB are: its image, the libraries, the Lisp heap
    (the one anonymous mapping of gigabytes), the rest anonymous."""
    exe = os.path.realpath("/proc/%d/exe" % pid)
    parts, part = {}, None
    try:
        with open("/proc/%d/smaps" % pid) as smaps:
            for line in smaps:
                fields = line.split()
                if "-" in fields[0] and not fields[0].endswith(":"):
                    low, high = (int(x, 16) for x in fields[0].split("-"))
                    path = fields[5] if len(fields) > 5 else ""
                    part = ("image" if path == exe else "libraries" if ".so" in path
                            else "heap" if not path and high - low > (1 << 30)
                            else "anonymous" if not path or path.startswith("[") else "files")
                elif fields[0] == "Pss:":
                    parts[part] = parts.get(part, 0) + int(fields[1]) / 1024
    except OSError:
        pass
    return ", ".join("%s %.0f" % item for item in sorted(parts.items(), key=lambda item: -item[1]) if item[1] >= 0.5)


def pss_mb(pid):
    """PID's proportional resident memory (Pss) in MB, or None when it is gone."""
    try:
        with open("/proc/%d/smaps_rollup" % pid) as rollup:
            for line in rollup:
                if line.startswith("Pss:"):
                    return int(line.split()[1]) / 1024
    except OSError:
        return None


def owner_pid(home):
    """The pid the store's holder sidecar names: the organism serving HOME's
    store, or None while none does."""
    try:
        with open(os.path.join(home, ".nodecode", "history.db.owner")) as sidecar:
            return json.load(sidecar).get("pid")
    except (OSError, ValueError):
        return None


def breakdowns(pid, home):
    """BREAKDOWN of the shell PID and of the organism HOME's store names."""
    organism = owner_pid(home)
    return "shell: %s; organism: %s" % (breakdown(pid), breakdown(organism) if organism else "none")


class Sampler(threading.Thread):
    """Pss of the shell PID and of its organism every 100 ms, summed, as
    (seconds, MB) pairs, and the breakdown at the highest of them."""

    def __init__(self, pid, home):
        super().__init__(daemon=True)
        self.pid, self.home, self.samples, self.stopped = pid, home, [], False
        self.highest, self.highest_breakdown = 0.0, ""

    def run(self):
        while not self.stopped:
            shell = pss_mb(self.pid)
            if shell is None:
                return
            organism = owner_pid(self.home)
            mb = shell + ((organism and organism != self.pid and pss_mb(organism)) or 0)
            self.samples.append((time.monotonic(), mb))
            if mb > self.highest:
                self.highest, self.highest_breakdown = mb, breakdowns(self.pid, self.home)
            time.sleep(0.1)

    def reset_highest(self):
        self.highest, self.highest_breakdown = 0.0, ""

    def peak(self, since, until=None):
        until = until or time.monotonic()
        values = [mb for at, mb in self.samples if since <= at <= until]
        return max(values) if values else 0.0

    def now(self):
        return self.samples[-1][1] if self.samples else 0.0


def die(message):
    print("footprint: " + message, file=sys.stderr)
    sys.exit(2)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("binary")
    parser.add_argument("--turns", type=int, default=40)
    parser.add_argument("--max-boot-mb", type=float, default=0)
    parser.add_argument("--max-turn-mb", type=float, default=0)
    parser.add_argument("--max-idle-mb", type=float, default=0)
    parser.add_argument("--max-growth-mb", type=float, default=0)
    parser.add_argument("--keep", action="store_true", help="leave the scratch HOME behind")
    args = parser.parse_args()
    if not shutil.which("tmux"):
        die("tmux is not installed, and the shell needs a terminal")
    binary = os.path.abspath(args.binary)
    home = tempfile.mkdtemp(prefix="nodecode-footprint-",
                            dir=os.environ.get("TMPDIR") or ("/var/tmp" if os.path.isdir("/var/tmp") else None))
    # A tmux server of its own: a new session in a server already running
    # takes that server's environment -- the operator's HOME.
    tmux = ["tmux", "-L", "footprint-%d" % os.getpid()]
    session = "footprint"
    provider = Provider()
    threading.Thread(target=provider.serve_forever, daemon=True).start()
    env = dict(os.environ, HOME=home, TERM="xterm-256color", LANG="C.UTF-8")
    env.pop("NODECODE_HOME", None)
    os.makedirs(os.path.join(home, ".nodecode"))
    with open(os.path.join(home, ".nodecode", "config.jsonc"), "w") as config:
        json.dump({"general": {"default_model": {"provider": "mock", "model": "mock-1"}},
                   "providers": {"mock": {"base_url": "http://127.0.0.1:%d/v1" % provider.server_port,
                                          "sdk": "openai-completions", "api_key": "footprint",
                                          "models": [{"id": "mock-1", "context_window": 400000}]}},
                   "update": {"mode": "off"}}, config)
    try:
        # Every shipped cell, as /setup installs them: the organism a person
        # runs, not the bare core a fresh home boots.
        form = ("(let ((root (merge-pathnames \".nodecode/cells/\" (user-homedir-pathname)))) "
                "(dolist (entry (nlk::shipped-cells)) (nlk::install-cell (nlk:offer-name entry) :root root)) "
                "(length (directory (merge-pathnames \"*/\" root))))")
        out = subprocess.run([binary, "trial"], input=form + "\n", env=env, cwd=home,
                             capture_output=True, text=True, timeout=300).stdout
        installed = [line.split()[-1] for line in out.splitlines() if "nodecode-trial-score" in line]
        if not installed or installed[-1] == "0":
            die("the shipped cells did not install:\n" + out[-2000:])
        print("footprint: %s cells installed" % installed[-1])

        def launch():
            subprocess.run(tmux + ["new-session", "-d", "-s", session, "-x", "200", "-y", "50",
                                   "exec env HOME='%s' TERM=xterm-256color '%s'" % (home, binary)],
                           env=env, cwd=home, check=True)
            return int(subprocess.run(tmux + ["display", "-p", "-t", session, "#{pane_pid}"],
                                      capture_output=True, text=True).stdout.strip())

        def stop(pid):
            # The organism goes with its shell, a moment after it.
            organism = owner_pid(home)
            subprocess.run(tmux + ["kill-session", "-t", session], capture_output=True)
            for each in (pid, organism):
                if not each:
                    continue
                for _ in range(100):
                    if not os.path.exists("/proc/%d" % each):
                        break
                    time.sleep(0.1)
                else:
                    os.kill(each, 9)

        # The first launch loads the folder and publishes the warm image every
        # later launch runs from; that image is what is measured.
        started = time.time()
        pid = launch()
        images = []
        for _ in range(600):
            images = [path for path in glob.glob(os.path.join(home, ".cache/nodecode/warm/**/nodecode"), recursive=True)
                      if os.path.getmtime(path) >= started]
            failed = glob.glob(os.path.join(home, ".cache/nodecode/warm/**/failed"), recursive=True)
            if images or failed:
                break
            time.sleep(0.5)
        stop(pid)
        if not images:
            die("no warm image was published: " + " ".join(
                open(path).read().strip() for path in glob.glob(os.path.join(home, ".cache/nodecode/warm/**/failed"), recursive=True)))

        pid = launch()
        for _ in range(100):
            if os.path.realpath("/proc/%d/exe" % pid) in map(os.path.realpath, images):
                break
            time.sleep(0.1)
        else:
            die("the launch did not run from the warm image (%s)" % os.path.realpath("/proc/%d/exe" % pid))
        sampler = Sampler(pid, home)
        sampler.start()
        time.sleep(20)
        boot_peak, boot_quiet = sampler.peak(0), sampler.now()
        boot_breakdown, quiet_breakdown = sampler.highest_breakdown, breakdowns(pid, home)
        sampler.reset_highest()

        def screen():
            return subprocess.run(tmux + ["capture-pane", "-p", "-t", session],
                                  capture_output=True, text=True).stdout

        def settle(limit=30):
            # The answer is served before the shell has painted it: the next
            # prompt waits for a screen that has stopped changing.
            last, since, deadline = None, time.monotonic(), time.monotonic() + limit
            while time.monotonic() < deadline:
                now = screen()
                if now != last:
                    last, since = now, time.monotonic()
                elif time.monotonic() - since >= 1.0:
                    return
                time.sleep(0.2)

        turn_marks = []
        for turn in range(1, args.turns + 1):
            before = provider.served
            began = time.monotonic()
            subprocess.run(tmux + ["send-keys", "-t", session,
                            "turn %d: write the next part of the report" % turn, "Enter"], check=True)
            deadline = began + 120
            while provider.served <= before:
                if time.monotonic() > deadline:
                    die("turn %d was never answered; the screen:\n%s" % (turn, screen()))
                time.sleep(0.1)
            settle()
            turn_marks.append((began, time.monotonic()))
        turns_end = time.monotonic()
        time.sleep(60)
        idle_after, idle_breakdown = sampler.now(), breakdowns(pid, home)
        sampler.stopped = True
        stop(pid)

        def window(first, last):
            return sampler.peak(turn_marks[first - 1][0], turn_marks[last - 1][1])

        n = len(turn_marks)
        turn_peak = window(min(6, n), n)
        growth = window(n - 9, n) - window(6, 15) if n >= 25 else 0.0
        print("footprint: boot peak %.0f MB (%s), %.0f MB 20 s after launch (%s)"
              % (boot_peak, boot_breakdown, boot_quiet, quiet_breakdown))
        for first in range(1, n + 1, 10):
            print("footprint: turns %2d-%2d: at most %.0f MB" % (first, min(first + 9, n), window(first, min(first + 9, n))))
        print("footprint: during turns 6-%d at most %.0f MB; last ten over turns 6-15 %+.0f MB; "
              "a quiet minute after, %.0f MB (%s)" % (n, turn_peak, growth, idle_after, idle_breakdown))
        print("footprint: the most while turns ran was %.0f MB (%s)" % (sampler.highest, sampler.highest_breakdown))
        sys.stdout.flush()
        broken = 0
        for label, value, ceiling in (("at boot", boot_peak, args.max_boot_mb),
                                      ("during turns", turn_peak, args.max_turn_mb),
                                      ("idle after the turns", idle_after, args.max_idle_mb),
                                      ("growth over the turns", growth, args.max_growth_mb)):
            if ceiling and value > ceiling:
                print("footprint: FAIL %s: %.0f MB, over the ceiling of %.0f" % (label, value, ceiling),
                      file=sys.stderr)
                broken += 1
        sys.exit(1 if broken else 0)
    finally:
        subprocess.run(tmux + ["kill-server"], capture_output=True)
        provider.shutdown()
        if args.keep:
            print("footprint: scratch HOME kept at %s" % home)
        else:
            shutil.rmtree(home, ignore_errors=True)


if __name__ == "__main__":
    main()
