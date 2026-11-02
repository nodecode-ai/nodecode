#!/usr/bin/env python3
"""latency.py -- how long a person waits on the shell, base against head.

Usage: latency.py BASE HEAD [HEAD...] [--rounds N] [--json FILE] [--keep]
       latency.py BINARY [--rounds N]          one build: print what it measures

Each BINARY is a nodecode executable (Linux). It runs as a person runs it:
the interactive shell in a terminal -- a pty this script reads, drawn by a
terminal emulator (pyte) -- in a scratch HOME with every shipped cell
installed, from the warm image its first launch publishes, talking to a
local OpenAI-compatible provider played here. Nothing reaches a paid model.

Counters -- instructions, bytes consed -- miss what a person feels most: a
wait. A sleep, a slower frame clock, a poll interval, a lock held across a
read: each costs no instructions and conses nothing. So every number here
is wall-clock time between something the person does (or the provider
sends) and the frame that shows its effect, read off the screen:

  boot_ms        launch to the composer with its session's model shown
  key_ms         a typed character to the frame that shows it
  submit_ms      Enter to the provider receiving the request
                 (shell -> gateway -> turn -> provider client)
  first_ms       the provider's first answer word sent to it painted
  last_ms        the provider's last answer word sent to it painted
  turn_ms        Enter to the turn ended on screen
  wheel_ms       a wheel step over the transcript to the frame that moves it
  sessions_ms    opening the session list (Left) to this session's row painted
  pss_mb         the shell's and its organism's proportional resident memory
                 after the round's turns (Pss: the image both map counts once)
  peak_mb        the most either held at once (VmHWM), summed

And what the round cost the machine, from the first typed key to the end
(the boot's cost is boot_ms's): CPU time of every thread, which a spin in a
hot path raises before any latency shows it --

  shell_cpu_ms, organism_cpu_ms   apart
  frame_cpu_ms   the shell's per frame it painted (one DEC 2026 update each)

A round is one launch: boot, typing, three turns of a token-by-token
stream, the wheel, the session list, then quit. Each build first runs one
round that is not kept (cold page caches, a first collection), then rounds
alternate between the builds, each round in a new order, and are compared
in pairs -- round N of the head against round N of the base, run next to
it -- so a stretch where the whole machine was slow cancels out. Per metric a round gives one
number (its median); a head fails a metric when its paired ratio to the
base is above the metric's tolerance with 95% confidence (a bootstrap over
the rounds' ratios) and its median is past the base's by the metric's floor.
A launch that fails its round (no footer, a turn never served) is run again,
up to three times; a head fails when it lost two launches more than the
base did, or could not finish a round at all.
Exits 1 when any head fails, 2 when it cannot measure (the base cannot start).
"""

import argparse
import json
import os
import pty
import random
import re
import select
import shutil
import signal
import statistics
import struct
import subprocess
import sys
import tempfile
import termios
import threading
import time
import fcntl
import glob
import http.server

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from footprint import owner_pid, pss_mb  # noqa: E402

try:
    import pyte
except ImportError:
    pyte = None

# Per metric: (unit, tolerance as a fraction, floor in the unit). A head fails
# a metric only past both, so a 1 ms wobble on a 4 ms keystroke never fails it.
METRICS = {
    "boot_ms": ("ms", 0.20, 40.0),
    "key_ms": ("ms", 0.25, 1.5),
    "submit_ms": ("ms", 0.25, 15.0),
    "first_ms": ("ms", 0.25, 15.0),
    "last_ms": ("ms", 0.25, 15.0),
    "turn_ms": ("ms", 0.20, 20.0),
    "wheel_ms": ("ms", 0.25, 1.5),
    "sessions_ms": ("ms", 0.25, 5.0),
    "pss_mb": ("MB", 0.06, 8.0),
    "peak_mb": ("MB", 0.08, 15.0),
    "shell_cpu_ms": ("ms", 0.15, 40.0),
    "frame_cpu_ms": ("ms", 0.15, 0.3),
    "organism_cpu_ms": ("ms", 0.15, 40.0),
}

WORDS = ("memory heap collector nursery generation thread stack image page "
         "catalog provider gateway session transcript organism layer").split()
COLS, ROWS = 200, 50
# A launch that fails (no footer, a turn never served) is run again, at most
# this many times per round; a build that fails them all cannot start.
ATTEMPTS = 3
# A head fails when it lost this many more launches than the base did: one
# lost launch in a run is a flake either build may have, two more is a pattern.
LAUNCH_FAILURES = 2
TURNS = 3
KEYS = 24


def cpu_ms(pid):
    """The CPU time (user and system, every thread) PID has used, in ms, or None."""
    try:
        with open("/proc/%d/stat" % pid) as stat:
            fields = stat.read().rsplit(")", 1)[1].split()
        return (int(fields[11]) + int(fields[12])) * 1000.0 / os.sysconf("SC_CLK_TCK")
    except (OSError, ValueError, IndexError):
        return None


def peak_mb(pid):
    """PID's peak resident memory (VmHWM) in MB, or None."""
    try:
        with open("/proc/%d/status" % pid) as status:
            for line in status:
                if line.startswith("VmHWM:"):
                    return int(line.split()[1]) / 1024
    except (OSError, ValueError):
        return None


def die(message):
    print("latency: " + message, file=sys.stderr)
    sys.exit(2)


# --- the provider ---------------------------------------------------------------

class Provider(http.server.ThreadingHTTPServer):
    """A chat-completions endpoint that notes when each request arrived and
    when its first and last answer words left."""
    daemon_threads = True

    def __init__(self):
        super().__init__(("127.0.0.1", 0), Handler)
        self.lock = threading.Lock()
        self.served = 0
        self.marks = {}

    def mark(self, k, key):
        with self.lock:
            self.marks.setdefault(k, {})[key] = time.monotonic()


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
        # The turn's number rides the prompt: "... turn K".
        found = re.findall(rb"zturn(\d+)z", raw)
        k = int(found[-1]) if found else 0
        self.server.mark(k, "arrived")
        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.send_header("Connection", "close")
        self.end_headers()

        def chunk(delta, finish=None):
            obj = {"id": "c%d" % k, "object": "chat.completion.chunk", "model": "mock-1",
                   "choices": [{"index": 0, "delta": delta, "finish_reason": finish}]}
            self.wfile.write(b"data: " + json.dumps(obj).encode() + b"\n\n")
            self.wfile.flush()

        rand = random.Random(k)
        chunk({"role": "assistant", "content": ""})
        chunk({"reasoning_content": " ".join(rand.choice(WORDS) for _ in range(40))})
        chunk({"content": "zopen%dz " % k})
        self.server.mark(k, "first")
        # A token or two a delta, as a model streams: the gateway's cost per
        # event is paid per delta.
        words = [rand.choice(WORDS) for _ in range(240)]
        for i in range(0, len(words), 2):
            time.sleep(0.001)
            chunk({"content": " " + " ".join(words[i:i + 2])})
        time.sleep(0.003)
        chunk({"content": " zclose%dz" % k})
        self.server.mark(k, "last")
        chunk({}, "stop")
        usage = {"id": "c%d" % k, "object": "chat.completion.chunk", "model": "mock-1", "choices": [],
                 "usage": {"prompt_tokens": max(1, len(raw) // 4), "completion_tokens": 400,
                           "total_tokens": max(1, len(raw) // 4) + 400}}
        self.wfile.write(b"data: " + json.dumps(usage).encode() + b"\n\ndata: [DONE]\n\n")
        self.wfile.flush()


# --- the terminal ---------------------------------------------------------------

# Strings a terminal swallows whole -- DCS, SOS, PM, APC (the shell's
# capability probes, kitty graphics) -- which pyte would print as text.
STRING_START = re.compile(rb"\x1b[P_^X]")
STRING_END = b"\x1b\\"


class Terminal:
    """BINARY in a COLSxROWS pty, its output drawn by pyte; every wait answers
    the monotonic time of the read that made its condition true."""

    def __init__(self, binary, home, env):
        self.started = time.monotonic()
        pid, fd = pty.fork()
        if pid == 0:
            fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", ROWS, COLS, 0, 0))
            os.chdir(home)
            os.execve(binary, [binary], env)
        self.pid, self.fd = pid, fd
        self.screen = pyte.Screen(COLS, ROWS)
        self.stream = pyte.ByteStream(self.screen)
        self.carry = b""
        self.text = ""
        self.closed = False
        self.frames = 0

    def feed(self, data):
        # Each frame the shell paints is one synchronized update (DEC 2026).
        self.frames += data.count(b"\x1b[?2026h")
        data = self.carry + data
        self.carry = b""
        out = []
        while data:
            start = STRING_START.search(data)
            if not start:
                # An ESC at the very end may open a string the next read finishes.
                if data.endswith(b"\x1b"):
                    out.append(data[:-1])
                    self.carry = b"\x1b"
                else:
                    out.append(data)
                break
            out.append(data[:start.start()])
            end = data.find(STRING_END, start.end())
            if end < 0:
                self.carry = data[start.start():]
                break
            data = data[end + 2:]
        self.stream.feed(b"".join(out))
        self.text = "\n".join(self.screen.display)

    def pump(self, timeout):
        """Read what the shell wrote within TIMEOUT seconds; the time of the
        last read, or None when nothing came."""
        if self.closed:
            return None
        ready, _, _ = select.select([self.fd], [], [], max(0.0, timeout))
        if not ready:
            return None
        at = time.monotonic()
        try:
            data = os.read(self.fd, 1 << 20)
        except OSError:
            self.closed = True
            return None
        if not data:
            self.closed = True
            return None
        self.feed(data)
        return at

    def wait(self, test, limit, what):
        """The time of the read after which TEST(text) holds; dies past LIMIT."""
        if test(self.text):
            return time.monotonic()
        deadline = time.monotonic() + limit
        while time.monotonic() < deadline:
            at = self.pump(deadline - time.monotonic())
            if at is not None and test(self.text):
                return at
            if self.closed:
                break
        raise Unmeasured("%s: not on screen within %.0f s; the screen:\n%s" % (what, limit, self.tail()))

    def change(self, limit):
        """The time of the first read that changes the screen, or None."""
        before = self.text
        deadline = time.monotonic() + limit
        while time.monotonic() < deadline:
            at = self.pump(deadline - time.monotonic())
            if at is not None and self.text != before:
                return at
        return None

    def quiet(self, still, limit):
        """Read until the screen has not changed for STILL seconds (or LIMIT)."""
        deadline = time.monotonic() + limit
        since = time.monotonic()
        while time.monotonic() < deadline:
            before = self.text
            at = self.pump(min(still, deadline - time.monotonic()))
            if at is not None and self.text != before:
                since = at
            elif time.monotonic() - since >= still:
                return

    def send(self, data):
        os.write(self.fd, data)
        return time.monotonic()

    def footer(self):
        lines = [line for line in self.screen.display if line.strip()]
        return lines[-1] if lines else ""

    def tail(self):
        return "\n".join(line.rstrip() for line in self.screen.display if line.strip())[-3000:]

    def close(self, home):
        """Quit as a closed terminal does, and wait for the organism to follow."""
        organism = owner_pid(home)
        try:
            os.close(self.fd)
        except OSError:
            pass
        self.closed = True
        for each in (self.pid, organism):
            if not each:
                continue
            try:
                os.kill(each, signal.SIGHUP)
            except OSError:
                pass
        for each in (self.pid, organism):
            if not each:
                continue
            for _ in range(150):
                if each == self.pid:
                    try:
                        if os.waitpid(each, os.WNOHANG)[0]:
                            break
                    except ChildProcessError:
                        break
                elif not os.path.exists("/proc/%d" % each):
                    break
                time.sleep(0.05)
            else:
                try:
                    os.kill(each, signal.SIGKILL)
                except OSError:
                    pass
        if self.pid:
            try:
                os.waitpid(self.pid, 0)
            except ChildProcessError:
                pass


class Unmeasured(Exception):
    pass


# --- one build ------------------------------------------------------------------

class Arm:
    """One build under measurement: its binary and a scratch HOME of its own."""

    def __init__(self, label, binary, provider_port, root):
        self.label = label
        self.binary = os.path.abspath(binary)
        self.home = tempfile.mkdtemp(prefix="nodecode-latency-", dir=root)
        self.env = dict(os.environ, HOME=self.home, TERM="xterm-256color", LANG="C.UTF-8",
                        COLORTERM="truecolor")
        for name in ("NODECODE_HOME", "TMUX", "XDG_CACHE_HOME", "XDG_CONFIG_HOME", "XDG_DATA_HOME"):
            self.env.pop(name, None)
        os.makedirs(os.path.join(self.home, ".nodecode"))
        with open(os.path.join(self.home, ".nodecode", "config.jsonc"), "w") as config:
            json.dump({"general": {"default_model": {"provider": "mock", "model": "mock-1"}},
                       "providers": {"mock": {"base_url": "http://127.0.0.1:%d/v1" % provider_port,
                                              "sdk": "openai-completions", "api_key": "latency",
                                              "models": [{"id": "mock-1", "context_window": 400000}]}},
                       "update": {"mode": "off"}}, config)
        self.rounds = []
        self.error = None
        self.failures = []  # "round N: why", one per launch that never finished its round
        self.broken = None  # why this build could not finish a round in ATTEMPTS launches

    def measure(self, provider, number, what):
        """Run one round, launching again when a launch fails: the round's
        values, or None when ATTEMPTS launches in a row failed (self.broken)."""
        for attempt in range(ATTEMPTS):
            try:
                # Each launch its own number: its markers never match an earlier one's.
                return self.round(provider, number + attempt * 100000)
            except Unmeasured as error:
                why = str(error).split("; the screen:")[0]
                self.failures.append("%s: %s" % (what, why))
                print("latency: %s %-6s launch %d failed: %s" % (what, self.label, attempt + 1, error),
                      file=sys.stderr, flush=True)
        self.broken = "%d launches in a row failed in %s" % (ATTEMPTS, what)
        return None

    def prepare(self):
        """Every shipped cell installed, and the warm image the first launch
        publishes: what every measured launch runs from."""
        try:
            form = ("(let ((root (merge-pathnames \".nodecode/cells/\" (user-homedir-pathname)))) "
                    "(dolist (entry (nlk::shipped-cells)) (nlk::install-cell (nlk:offer-name entry) :root root)) "
                    "(length (directory (merge-pathnames \"*/\" root))))")
            out = subprocess.run([self.binary, "trial"], input=form + "\n", env=self.env, cwd=self.home,
                                 capture_output=True, text=True, timeout=600).stdout
            installed = [line.split()[-1] for line in out.splitlines() if "nodecode-trial-score" in line]
            if not installed or installed[-1] == "0":
                raise Unmeasured("the shipped cells did not install:\n" + out[-2000:])
            started = time.time()
            term = Terminal(self.binary, self.home, self.env)
            pattern = os.path.join(self.home, ".cache/nodecode/warm/**/")
            deadline = time.monotonic() + 600
            while time.monotonic() < deadline:
                term.pump(0.5)
                images = [path for path in glob.glob(pattern + "nodecode", recursive=True)
                          if os.path.getmtime(path) >= started]
                failed = glob.glob(pattern + "failed", recursive=True)
                if images or failed:
                    break
            term.close(self.home)
            if not images:
                raise Unmeasured("no warm image was published")
            self.image = os.path.realpath(images[0])
            self.cells = installed[-1]
        except Unmeasured as error:
            self.error = str(error)
        except Exception as error:  # noqa: BLE001 -- reported, then the run stops
            self.error = "%s: %s" % (type(error).__name__, error)

    def round(self, provider, number):
        """One launch, measured: a dict of metric -> this round's value."""
        samples = {name: [] for name in METRICS}
        term = Terminal(self.binary, self.home, self.env)
        try:
            ready = term.wait(lambda text: "\u2190 sessions" in text and "mock-1" in text, 60, "the session's footer")
            samples["boot_ms"].append((ready - term.started) * 1000)
            exe = os.path.realpath("/proc/%d/exe" % term.pid)
            if exe != self.image:
                raise Unmeasured("the launch ran %s, not the warm image %s" % (exe, self.image))
            term.quiet(0.4, 8)
            # Typing, a character at a time, each its own frame.
            rand = random.Random(number)
            typed = "zkey%dz " % number
            term.send(typed.encode())
            term.wait(lambda text: typed.strip() in text, 10, "the typed prefix")
            term.quiet(0.2, 3)
            organism = owner_pid(self.home)
            cpu_before = (cpu_ms(term.pid) or 0.0, (organism and cpu_ms(organism)) or 0.0)
            frames_before = term.frames
            letters = "".join(rand.choice("abcdefghijklmnopqrstuvwxyz") for _ in range(KEYS))
            for letter in letters:
                typed += letter
                expect = typed.split()[-1]
                sent = term.send(letter.encode())
                painted = term.wait(lambda text, expect=expect: expect in text, 10, "a typed key")
                samples["key_ms"].append((painted - sent) * 1000)
                term.quiet(0.03, 0.5)
            # Three turns.
            for turn in range(TURNS):
                k = number * 100 + turn + 1
                if turn:
                    prompt = "write the next part zturn%dz" % k
                    term.send(prompt.encode())
                    term.wait(lambda text, p=prompt: p in text, 10, "the prompt")
                else:
                    prompt = " zturn%dz" % k
                    term.send(prompt.encode())
                    term.wait(lambda text, p=prompt.strip(): p in text, 10, "the prompt")
                term.quiet(0.1, 2)
                entered = term.send(b"\r")
                opened = term.wait(lambda text, k=k: "zopen%dz" % k in text, 60, "turn %d's first word" % k)
                closed = term.wait(lambda text, k=k: "zclose%dz" % k in text, 60, "turn %d's last word" % k)
                ended = term.wait(lambda text, k=k: "zclose%dz" % k in text and "running" not in term.footer(),
                                  60, "turn %d's end" % k)
                marks = provider.marks.get(k, {})
                if not {"arrived", "first", "last"} <= set(marks):
                    raise Unmeasured("the provider never served turn %d" % k)
                samples["submit_ms"].append((marks["arrived"] - entered) * 1000)
                samples["first_ms"].append((opened - marks["first"]) * 1000)
                samples["last_ms"].append((closed - marks["last"]) * 1000)
                samples["turn_ms"].append((ended - entered) * 1000)
                term.quiet(0.3, 5)
            # The wheel over the transcript: up, then back down.
            def step(button):
                sent = term.send(b"\x1b[<%d;60;20M" % button)
                moved = term.change(0.3)
                if moved is not None:
                    samples["wheel_ms"].append((moved - sent) * 1000)
                term.quiet(0.03, 1)
                return moved is not None
            up = 0
            while up < 10 and step(64):
                up += 1
            for _ in range(up):
                step(65)
            # The memory the round left, once it is quiet.
            term.quiet(0.5, 5)
            organism = owner_pid(self.home)
            shell = pss_mb(term.pid) or 0.0
            samples["pss_mb"].append(shell + ((organism and organism != term.pid and pss_mb(organism)) or 0.0))
            # The session list (Left on an empty composer), to this round's
            # session's row: the gateway's session_rows, painted.
            mark = "zkey%dz" % number
            sent = term.send(b"\x1b[D")
            listed = term.wait(lambda text: "This folder" in text and any(
                mark in line and "\u258c" in line for line in text.split("\n")), 10, "the session list")
            samples["sessions_ms"].append((listed - sent) * 1000)
            term.quiet(0.1, 3)
            # What the round cost the machine: every thread's CPU time from the
            # first typed key to here (the boot's is boot_ms's business), the
            # shell's and its organism's apart, and the most either held.
            organism = owner_pid(self.home)
            shell_cpu = (cpu_ms(term.pid) or 0.0) - cpu_before[0]
            samples["shell_cpu_ms"].append(shell_cpu)
            if term.frames > frames_before:
                samples["frame_cpu_ms"].append(shell_cpu / (term.frames - frames_before))
            if organism and organism != term.pid and cpu_ms(organism) is not None:
                samples["organism_cpu_ms"].append(cpu_ms(organism) - cpu_before[1])
            samples["peak_mb"].append((peak_mb(term.pid) or 0.0)
                                       + ((organism and organism != term.pid and peak_mb(organism)) or 0.0))
        finally:
            term.close(self.home)
        return {name: statistics.median(values) for name, values in samples.items() if values}


# --- the comparison -------------------------------------------------------------

def compare(base, head, name, rand):
    """(ratio, low, high, verdict) for HEAD over BASE on NAME, from the rounds."""
    unit, tolerance, floor = METRICS[name]
    # Round N of each build ran next to the other's: compare them in pairs,
    # so a stretch where the whole machine was slow cancels out.
    pairs = [(b[name], h[name]) for b, h in zip(base.rounds, head.rounds) if name in b and name in h]
    if len(pairs) < 3:
        return None
    mb = statistics.median(b for b, _ in pairs)
    mh = statistics.median(h for _, h in pairs)
    ratios = [h / b if b > 0 else (1.0 if h <= 0 else float("inf")) for b, h in pairs]
    ratio = statistics.median(ratios)
    boots = sorted(statistics.median(rand.choice(ratios) for _ in ratios) for _ in range(4000))
    low, high = boots[int(0.025 * len(boots))], boots[int(0.975 * len(boots)) - 1]
    slower = low > 1 + tolerance and mh - mb > floor
    faster = high < 1 - tolerance and mb - mh > floor
    return {"base": mb, "head": mh, "ratio": ratio, "low": low, "high": high,
            "verdict": "FAIL" if slower else "faster" if faster else "ok"}


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("binaries", nargs="+", help="BASE then each HEAD; one binary just measures it")
    parser.add_argument("--rounds", type=int, default=15)
    parser.add_argument("--json", help="write every round's numbers and the verdicts here")
    parser.add_argument("--keep", action="store_true", help="leave the scratch HOMEs behind")
    args = parser.parse_args()
    if pyte is None:
        die("pyte is not installed (pip install pyte): it draws the shell's screen")
    if sys.platform != "linux":
        die("Linux only: the memory is read from /proc")
    for binary in args.binaries:
        if not os.access(binary, os.X_OK):
            die("%s is no executable" % binary)
    root = tempfile.mkdtemp(prefix="nodecode-latency-",
                            dir=os.environ.get("TMPDIR") or ("/var/tmp" if os.path.isdir("/var/tmp") else None))
    provider = Provider()
    threading.Thread(target=provider.serve_forever, daemon=True).start()
    labels = ["base"] + ["head" if len(args.binaries) == 2 else "head%d" % i
                         for i in range(1, len(args.binaries))]
    if len(args.binaries) == 1:
        labels = ["build"]
    arms = [Arm(label, binary, provider.server_port, root) for label, binary in zip(labels, args.binaries)]
    try:
        began = time.monotonic()
        threads = [threading.Thread(target=arm.prepare) for arm in arms]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join()
        for arm in arms:
            if arm.error:
                die("%s (%s): %s" % (arm.label, arm.binary, arm.error))
        print("latency: %d build(s) ready in %.0f s, %s cells each; %d rounds"
              % (len(arms), time.monotonic() - began, arms[0].cells, args.rounds), flush=True)
        # One round each first, not kept: a build's first launch pays for cold
        # page caches and a first collection the later rounds never see again.
        for arm in arms:
            arm.measure(provider, arms.index(arm), "the warm-up")
        order = list(arms)
        for number in range(1, args.rounds + 1):
            if arms[0].broken:
                break
            # Each round in a new order: no build always runs first after a pause.
            order = order[1:] + order[:1]
            for arm in order:
                if arm.broken:
                    continue
                values = arm.measure(provider, number * len(arms) + arms.index(arm), "round %d" % number)
                if values is None:
                    continue
                arm.rounds.append(values)
                print("latency: round %d %-6s %s" % (number, arm.label, "  ".join(
                    "%s %.1f" % (name.rsplit("_", 1)[0], value) for name, value in values.items())), flush=True)
        if arms[0].broken:
            die("%s (%s) cannot be measured: %s; the last: %s"
                % (arms[0].label, arms[0].binary, arms[0].broken, arms[0].failures[-1]))
        report = {"rounds": {arm.label: arm.rounds for arm in arms},
                  "binaries": {arm.label: arm.binary for arm in arms},
                  "failed_launches": {arm.label: arm.failures for arm in arms}, "verdicts": {}}
        failed = []
        rand = random.Random(1)
        lines = []
        if len(arms) == 1:
            lines.append("| metric | median | spread (min-max of rounds) |")
            lines.append("|---|---|---|")
            for name, (unit, _, _) in METRICS.items():
                values = [r[name] for r in arms[0].rounds if name in r]
                if values:
                    lines.append("| %s | %.1f %s | %.1f-%.1f |" % (name, statistics.median(values), unit,
                                                                 min(values), max(values)))
            lines.append("| launches lost | %d | %s |" % (len(arms[0].failures), "; ".join(arms[0].failures) or "-"))
        for head in arms[1:]:
            lines.append("")
            lines.append("**%s against base** (%d rounds each, interleaved)" % (head.label, args.rounds))
            lines.append("")
            lines.append("| metric | base | %s | ratio | 95%% interval | gate (tolerance, floor) | verdict |" % head.label)
            lines.append("|---|---|---|---|---|---|---|")
            report["verdicts"][head.label] = {}
            # Launches that never finished their round: a shell that fails to
            # start, or a turn never served, is the worst slowdown there is.
            lost, base_lost = len(head.failures), len(arms[0].failures)
            if head.broken or lost - base_lost >= LAUNCH_FAILURES:
                failed.append("%s launches" % head.label)
                for why in head.failures[-3:]:
                    print("latency: %s lost a launch, %s" % (head.label, why), file=sys.stderr)
            lines.append("| launches lost | %d | %d | | | +%d | %s |"
                         % (base_lost, lost, LAUNCH_FAILURES - 1,
                            "FAIL (%s)" % head.broken if head.broken
                            else "FAIL" if lost - base_lost >= LAUNCH_FAILURES else "ok"))
            for name, (unit, tolerance, floor) in METRICS.items():
                result = compare(arms[0], head, name, rand)
                if result is None:
                    continue
                report["verdicts"][head.label][name] = result
                if result["verdict"] == "FAIL":
                    failed.append("%s %s" % (head.label, name))
                lines.append("| %s | %.1f %s | %.1f %s | %.2fx | %.2f-%.2f | +%d%%, %g %s | %s |"
                             % (name, result["base"], unit, result["head"], unit, result["ratio"],
                                result["low"], result["high"], tolerance * 100, floor, unit,
                                result["verdict"]))
        text = "\n".join(lines)
        print(text)
        summary = os.environ.get("GITHUB_STEP_SUMMARY")
        if summary:
            with open(summary, "a") as out:
                out.write("### Latency, base against head\n" + text + "\n")
        if args.json:
            with open(args.json, "w") as out:
                json.dump(report, out, indent=1)
        if failed:
            print("latency: FAIL past the gate: " + ", ".join(failed), file=sys.stderr)
            sys.exit(1)
    finally:
        provider.shutdown()
        if args.keep:
            print("latency: scratch HOMEs kept under %s" % root)
        else:
            shutil.rmtree(root, ignore_errors=True)


if __name__ == "__main__":
    main()
