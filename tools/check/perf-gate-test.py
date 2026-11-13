#!/usr/bin/env python3
"""perf-gate-test.py --- perf-gate.py's handling of a measured run that hangs.

No SBCL and no perf: the run is a stand-in process that hangs (or, the
second time, writes its spans), started where perf-gate.py starts a
workload's SBCL. A hung run must be killed whole, its own children too, well
inside the limit's reach; it must say what it was doing, the output it had
written and what it answered SIGUSR1 with; the gate measures it again once,
and a second hang cannot measure.

Usage: python3 tools/check/perf-gate-test.py (ci-linux.sh runs it).

SPDX-License-Identifier: MIT
"""

import contextlib
import importlib.util
import io
import os
import shutil
import signal
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))
LIMIT = 3
# Past this a run_one has not given up on its hung run: a fail, not a wait.
PATIENCE = 30

STAND_IN = r'''
import os, signal, subprocess, sys, time
mode, state = sys.argv[1], sys.argv[2]
if mode == "flaky":
    mode = "ok" if os.path.exists(state) else "hang"
    open(state, "a").close()
if mode == "ok":
    with open("spans.sexp", "w") as f:
        f.write("(:WALL-NS 1000 :CPU-NS 900 :CONSED 4096 :SPANS 1)\n")
    sys.exit(0)
child = subprocess.Popen(["sleep", "600"])
with open(state + ".children", "a") as f:
    f.write("%d\n" % child.pid)
signal.signal(signal.SIGUSR1, lambda *_: print("stand-in backtrace: waiting forever", flush=True))
print("stand-in started", flush=True)
while True:
    time.sleep(1)
'''


class TooSlow(Exception):
    pass


def on_alarm(*_):
    raise TooSlow("still waiting on a hung run after %d s, with a %d s limit" % (PATIENCE, LIMIT))


def load_gate(home):
    os.environ["PERF_HOME"] = home
    os.environ["PERF_RUN_LIMIT"] = str(LIMIT)
    spec = importlib.util.spec_from_file_location("perf_gate", os.path.join(HERE, "perf-gate.py"))
    gate = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(gate)
    return gate


def alive(pid):
    try:
        with open("/proc/%d/stat" % pid) as f:
            return f.read().split(")")[-1].split()[0] != "Z"
    except OSError:
        return False


class Case:
    def __init__(self, gate, root, mode):
        self.gate, self.mode = gate, mode
        self.scratch = tempfile.mkdtemp(dir=root)
        self.tree = tempfile.mkdtemp(dir=root)
        self.state = os.path.join(self.scratch, "state")
        stand_in = os.path.join(self.scratch, "stand-in.py")
        with open(stand_in, "w") as f:
            f.write(STAND_IN)
        gate.sbcl_command = lambda tree, forms: [sys.executable, stand_in, self.mode, self.state]
        self.out, self.err = io.StringIO(), io.StringIO()

    def children(self):
        try:
            with open(self.state + ".children") as f:
                return [int(line) for line in f if line.strip()]
        except OSError:
            return []

    def call(self, fn, *args):
        signal.signal(signal.SIGALRM, on_alarm)
        signal.alarm(PATIENCE)
        started = time.time()
        try:
            with contextlib.redirect_stdout(self.out), contextlib.redirect_stderr(self.err):
                return fn(*args)
        finally:
            signal.alarm(0)
            self.seconds = time.time() - started

    def cleanup(self):
        for pid in self.children():
            with contextlib.suppress(OSError):
                os.kill(pid, signal.SIGKILL)


def a_hung_run_is_killed_whole_and_says_what_it_was_doing(gate, root):
    case = Case(gate, root, "hang")
    try:
        try:
            case.call(gate.run_one, case.tree, "turns", case.scratch, False)
            return "run_one answered a run that never finished"
        except gate.CannotMeasure as e:
            said = str(e)
        problems = []
        for words in ("turns", "stand-in started", "stand-in backtrace: waiting forever"):
            if words not in said:
                problems.append("what it said lacks %r:\n%s" % (words, said))
        time.sleep(0.5)
        left = [pid for pid in case.children() if alive(pid)]
        if not case.children():
            problems.append("the stand-in never started its child")
        if left:
            problems.append("the run's own child outlived it: pid %s" % left)
        return "\n".join(problems)
    finally:
        case.cleanup()


def one_hang_is_named_and_measured_again(gate, root):
    case = Case(gate, root, "flaky")
    os.environ["GITHUB_ACTIONS"] = "true"
    try:
        hangs = []
        result = case.call(gate.run_counted, case.tree, "turns", case.scratch, False, hangs)
        problems = []
        if result.get("wall") != 1000:
            problems.append("the second run's spans were not its answer: %r" % result)
        if len(hangs) != 1 or hangs[0]["workload"] != "turns":
            problems.append("the hang was not counted once: %r" % hangs)
        if "::warning" not in case.out.getvalue() or "turns" not in case.out.getvalue():
            problems.append("no warning on the run's page naming the workload: %r" % case.out.getvalue())
        if "again" not in case.err.getvalue():
            problems.append("the log does not say it measured again:\n%s" % case.err.getvalue())
        return "\n".join(problems)
    finally:
        del os.environ["GITHUB_ACTIONS"]
        case.cleanup()


def a_second_hang_cannot_measure(gate, root):
    case = Case(gate, root, "hang")
    try:
        hangs = []
        try:
            case.call(gate.run_counted, case.tree, "turns", case.scratch, False, hangs)
            return "run_counted answered a run that hung twice"
        except gate.CannotMeasure as e:
            said = str(e)
        problems = []
        if len(hangs) != 2:
            problems.append("two hangs, counted %d" % len(hangs))
        if "hung" not in said:
            problems.append("it does not say the runs hung: %s" % said)
        left = [pid for pid in case.children() if alive(pid)]
        if left:
            problems.append("a hung run's child outlived it: pid %s" % left)
        return "\n".join(problems)
    finally:
        case.cleanup()


def main():
    root = tempfile.mkdtemp(prefix="perf-gate-test-")
    failed = 0
    try:
        gate = load_gate(root)
        for test in (a_hung_run_is_killed_whole_and_says_what_it_was_doing,
                     one_hang_is_named_and_measured_again,
                     a_second_hang_cannot_measure):
            try:
                problem = test(gate, root)
            except Exception as e:
                problem = "%s: %s" % (type(e).__name__, e)
            name = test.__name__.replace("_", "-")
            if problem:
                failed += 1
                print("FAIL %s\n  %s" % (name, problem.replace("\n", "\n  ")))
            else:
                print("ok   %s" % name)
    finally:
        shutil.rmtree(root, ignore_errors=True)
    print("perf-gate-test: %d failed" % failed if failed else "perf-gate-test: all passed")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
