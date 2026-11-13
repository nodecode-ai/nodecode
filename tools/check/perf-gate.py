#!/usr/bin/env python3
"""perf-gate.py BASE --- does this checkout cost more than BASE, where a
person waits?

Usage: perf-gate.py BASE [--reps N] [--workloads tui,boot,turns] [--json OUT]

Run from a nodecode checkout (`just perf BASE'). The checkout as it stands is
the head; BASE is any revision of its repository (CI passes the pull
request's merge base, or the commit a push moved main from). Both trees run
one bench file, the head's tools/check/perf-bench.lisp, each in an SBCL that
loaded its own :nodecode/test, so the two differ only in the code under
review. The workloads, each its own process:

  tui    a 120x40 shell over a 4000-line transcript: a sentence typed into the
         composer and 40 pages of scrolling, each key one batch through the
         event loop's own fold-then-paint (HANDLE-MESSAGES); then 3 s still,
         the frame clock ticking, outside the measured spans
  boot   the gateway started on a fresh store, five times
  turns  eight turns sent to the gateway as start_turn, each streamed back as
         200 deltas from a provider whose HTTP answer is played

Each process is measured four ways, over the workload's own spans only:
  instructions  user-space instructions of every thread (perf stat, when
                perf may count this user's processes): what the work costs,
                whatever else the machine runs -- the tight gate
  consed        bytes allocated: garbage is GC time later
  cpu           CPU time of every thread
  wall          the time a person waits, sleeps and blocking waits included,
                which cost no instructions

A workload may also report counts beside its spans, which no machine moves:
the tui workload counts what it writes to its terminal, per phase (keys,
scroll, idle), as PHASE-bytes and PHASE-frames. They catch what a fast runner
hides in time: a frame that repaints the whole screen where one line moved,
or a clock that paints while nothing changes.

Base and head runs alternate, REPS of each (default 5). A metric regressed
when the head's median exceeds the base's by more than its floor AND every
head run costs more than every base run, so one noisy run never fails it;
a flagged workload is then measured again, a fresh round of both sides, and
fails only when that round flags the same metric:
  instructions 3%   consed 3% + 256 KiB   cpu 20% + 5 ms   wall 25% + 20 ms
  *-bytes 1%   *-frames 0 (an idle frame where the base paints none fails)
PERF_FLOORS="instructions=0.05,wall=0.4,bytes=0.1" overrides floors (fractions).

Exits 0 when the head holds, 1 when a metric regressed, 2 when it cannot
measure (a tree that does not load, a workload that fails). Prints a table;
under GitHub Actions the table also goes to the job summary.

A run takes seconds; one still going after PERF_RUN_LIMIT seconds (default
120) hung. The gate reads its threads' kernel states, asks its SBCL for every
thread's backtrace (SIGUSR1, which perf-bench.lisp answers), kills the run's
whole process group, prints all of it, and measures that run again. One hung
run in a gate is a flake, named as a warning on the run's page; a second
cannot measure.

Trees and their compiled code are kept under PERF_HOME (default
/var/tmp/nodecode-perf): a base is extracted once per commit (git archive)
and compiled once, so a second run against the same base pays only the
measuring. A base no run used for 7 days goes, and so does compiled code
whose source directory is gone. SBCL is `sbcl' on PATH; PERF_SBCL_USERINIT, when set, is its
--userinit (CI's quicklisp).
"""

import argparse
import fcntl
import json
import os
import shutil
import signal
import statistics
import subprocess
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(HERE, "..", ".."))
BENCH = os.path.join(HERE, "perf-bench.lisp")
WORKLOADS = ["tui", "boot", "turns"]
METRICS = ["instructions", "consed", "cpu", "wall"]
# (fraction, absolute) per metric; absolute in the metric's own unit
FLOORS = {"instructions": (0.03, 0), "consed": (0.03, 256 * 1024),
          "cpu": (0.20, 5e6), "wall": (0.25, 20e6),
          # counts: the same work writes the same bytes in every run
          "bytes": (0.01, 0), "frames": (0.0, 0)}
SPAN_KEYS = {"WALL-NS", "CPU-NS", "CONSED", "SPANS"}
PERF_HOME = os.environ.get("PERF_HOME", "/var/tmp/nodecode-perf")
# A measured run takes seconds; one still going after this many has hung.
RUN_LIMIT = int(os.environ.get("PERF_RUN_LIMIT", "120"))


class CannotMeasure(Exception):
    pass


class Hung(CannotMeasure):
    """A run still going after RUN_LIMIT seconds, killed; says what it was doing."""


def git(*args, cwd=REPO):
    return subprocess.run(["git", *args], cwd=cwd, check=True, capture_output=True,
                          text=True).stdout.strip()


def base_tree(rev):
    """BASE's tree under PERF_HOME, extracted and its cells laid down once."""
    try:
        sha = git("rev-parse", "--verify", rev + "^{commit}")
    except subprocess.CalledProcessError:
        raise CannotMeasure("no revision %s in %s" % (rev, REPO))
    path = os.path.join(PERF_HOME, "trees", sha)
    ready = os.path.join(path, ".ready")
    if os.path.exists(ready):
        os.utime(ready)
    else:
        shutil.rmtree(path, ignore_errors=True)
        os.makedirs(path)
        archive = subprocess.Popen(["git", "archive", sha], cwd=REPO, stdout=subprocess.PIPE)
        subprocess.run(["tar", "-x", "-C", path], stdin=archive.stdout, check=True)
        if archive.wait() != 0:
            raise CannotMeasure("git archive %s failed" % sha)
        cells(path)
        open(os.path.join(path, ".ready"), "w").close()
    return sha, path


def prune(days=7):
    """Drop base trees no run used for DAYS, and compiled code whose source is gone."""
    cutoff = time.time() - days * 86400
    trees = os.path.join(PERF_HOME, "trees")
    for name in os.listdir(trees) if os.path.isdir(trees) else []:
        path = os.path.join(trees, name)
        ready = os.path.join(path, ".ready")
        if os.path.getmtime(ready if os.path.exists(ready) else path) < cutoff:
            shutil.rmtree(path, ignore_errors=True)
    # The fasl cache mirrors each source path (asdf's default translation):
    # a mirror whose source directory no longer exists is never loaded again.
    cache = os.path.join(PERF_HOME, "cache", "common-lisp")
    for impl in os.listdir(cache) if os.path.isdir(cache) else []:
        top = os.path.join(cache, impl)
        for dirpath, dirnames, _ in os.walk(top):
            source = "/" + os.path.relpath(dirpath, top) if dirpath != top else "/"
            if not os.path.isdir(source):
                shutil.rmtree(dirpath, ignore_errors=True)
                dirnames[:] = []


def system_root(tree):
    """Where TREE keeps nodecode.asd and its tools: the root since the flat
    layout, src/ in a base from before it."""
    if os.path.exists(os.path.join(tree, "nodecode.asd")):
        return tree
    return os.path.join(tree, "src")


def cells(tree):
    # A base from before tools/ took one folder per job keeps it at the top.
    script = os.path.join(system_root(tree), "tools", "build", "pull-cells.sh")
    if not os.path.exists(script):
        script = os.path.join(system_root(tree), "tools", "pull-cells.sh")
    r = subprocess.run([script], cwd=tree,
                       capture_output=True, text=True)
    if r.returncode != 0:
        raise CannotMeasure("pull-cells in %s: %s" % (tree, r.stderr.strip()[-400:]))


def sbcl_command(tree, forms):
    command = ["sbcl", "--dynamic-space-size", "4096", "--control-stack-size", "8MB",
               "--noinform", "--non-interactive"]
    if os.environ.get("PERF_SBCL_USERINIT"):
        command += ["--userinit", os.environ["PERF_SBCL_USERINIT"]]
    command += ["--eval", "(require :asdf)",
                "--eval", '(push (truename "%s/") asdf:*central-registry*)' % system_root(tree),
                "--eval", "(asdf:load-system :nodecode/test)"]
    for form in forms:
        command += ["--eval", form]
    return command


def environment(scratch):
    env = dict(os.environ)
    env["NODECODE_HOME"] = os.path.join(scratch, "nodecode-home")
    env["XDG_CACHE_HOME"] = os.path.join(PERF_HOME, "cache")
    env.pop("NODECODE_PHASE_LOG", None)
    return env


def compile_tree(tree, scratch):
    """Load TREE's :nodecode/test once, so its compile is never a measured run's."""
    r = subprocess.run(sbcl_command(tree, ["(values)"]), cwd=scratch,
                       env=environment(scratch), capture_output=True, text=True, timeout=1800)
    if r.returncode != 0:
        raise CannotMeasure("%s does not load :nodecode/test:\n%s" % (tree, (r.stdout + r.stderr)[-1500:]))


def perf_usable():
    if not shutil.which("perf"):
        return False
    r = subprocess.run(["perf", "stat", "-x", ",", "-e", "instructions:u", "true"],
                       capture_output=True, text=True)
    return r.returncode == 0 and "instructions" in r.stderr and "<not" not in r.stderr


def lisp_string(text):
    return '"%s"' % text.replace("\\", "\\\\").replace('"', '\\"')


def run_one(tree, workload, scratch, use_perf):
    d = tempfile.mkdtemp(prefix="%s-" % workload, dir=scratch)
    out = os.path.join(d, "spans.sexp")
    forms = ['(load %s)' % lisp_string(BENCH)]
    command = None
    if use_perf:
        ctl, ack, counts = (os.path.join(d, n) for n in ("ctl", "ack", "perf.csv"))
        os.mkfifo(ctl)
        os.mkfifo(ack)
        forms.append('(nodecode.test::perf-run %s %s :ctl %s :ack %s)'
                     % (lisp_string(workload), lisp_string(out), lisp_string(ctl), lisp_string(ack)))
        command = ["perf", "stat", "-x", ",", "-e", "instructions:u", "--delay=-1",
                   "--control=fifo:%s,%s" % (ctl, ack), "-o", counts, "--"]
    else:
        forms.append('(nodecode.test::perf-run %s %s)' % (lisp_string(workload), lisp_string(out)))
        command = []
    command += sbcl_command(tree, forms)
    # From D, never from the tree: a turn reads every AGENTS.md from its
    # folder up, and a tree's parents differ (a checkout under a home with its
    # own AGENTS.md against a base under PERF_HOME), so the same work would
    # carry a different prompt in each.
    output_path = os.path.join(d, "output")
    with open(output_path, "w") as output:
        # Its own session: perf, its SBCL and whatever that SBCL starts are
        # one process group, killed as one when the run hangs.
        run = subprocess.Popen(command, cwd=d, env=environment(d), stdout=output,
                               stderr=subprocess.STDOUT, start_new_session=True)
    try:
        returncode = run.wait(timeout=RUN_LIMIT)
    except subprocess.TimeoutExpired:
        raise Hung("workload %s of the %s (%s) was still running after %d s; killed it.\n%s"
                   % (workload, side(tree), tree, RUN_LIMIT, stalled(run, use_perf, output_path)))
    except BaseException:
        kill_group(run)
        raise
    if returncode != 0 or not os.path.exists(out):
        raise CannotMeasure("workload %s in %s exited %s:\n%s"
                            % (workload, tree, returncode, tail(output_path, 2000)))
    spans = read_plist(open(out).read())
    result = {"consed": spans["CONSED"], "cpu": spans["CPU-NS"], "wall": spans["WALL-NS"]}
    # every other key is a count the workload reported (PERF-COUNT)
    result.update({k.lower(): v for k, v in spans.items() if k not in SPAN_KEYS})
    if use_perf:
        for line in open(counts):
            fields = line.strip().split(",")
            if len(fields) > 2 and fields[2].startswith("instructions") and fields[0].isdigit():
                result["instructions"] = int(fields[0])
        if "instructions" not in result:
            raise CannotMeasure("perf stat counted no instructions for %s:\n%s"
                                % (workload, open(counts).read()[-800:]))
    shutil.rmtree(d, ignore_errors=True)
    return result


def side(tree):
    return "head" if tree == REPO else "base"


def tail(path, chars):
    with open(path, errors="replace") as f:
        return f.read()[-chars:]


def proc_fields(pid):
    """/proc/PID/stat's fields after the name: state, ppid, ...; None when gone."""
    try:
        with open("/proc/%s/stat" % pid) as f:
            stat = f.read()
    except OSError:
        return None
    return stat[stat.rindex(")") + 2:].split()


def children(pid):
    found = []
    for entry in os.listdir("/proc") if os.path.isdir("/proc") else []:
        fields = entry.isdigit() and proc_fields(entry)
        if fields and int(fields[1]) == pid:
            found.append(int(entry))
    return sorted(found)


def threads(pid):
    """Each thread of PID as the kernel sees it: tid, name, state, and the
    kernel function it sleeps in."""
    lines = []
    try:
        tids = sorted(os.listdir("/proc/%d/task" % pid), key=int)
    except OSError:
        return ""
    for tid in tids:
        task = "/proc/%d/task/%s/" % (pid, tid)
        fields = proc_fields("%d/task/%s" % (pid, tid))
        try:
            with open(task + "comm") as f:
                name = f.read().strip()
            with open(task + "wchan") as f:
                wchan = f.read().strip()
        except OSError:
            continue
        if fields:
            lines.append("  %s %s %s %s\n" % (tid, name, fields[0], wchan or "-"))
    return "".join(lines)


def settle(path, quiet=1.5, most=10.0):
    """Wait until PATH has not grown for QUIET seconds, at most MOST."""
    deadline = time.time() + most
    size, since = -1, time.time()
    while time.time() < deadline:
        now = os.path.getsize(path)
        if now != size:
            size, since = now, time.time()
        elif time.time() - since >= quiet:
            return
        time.sleep(0.1)


def kill_group(run):
    """Kill RUN's whole process group; RUN not yet reaped, so the group is still its own."""
    try:
        os.killpg(run.pid, signal.SIGKILL)
    except (ProcessLookupError, PermissionError):
        pass


def stalled(run, use_perf, output_path):
    """What the hung RUN was doing, then RUN killed: each thread of perf and
    of its SBCL as the kernel sees it, the run's output, and the backtrace of
    every Lisp thread, which perf-bench.lisp prints when sent SIGUSR1."""
    try:
        lisp = run.pid
        if use_perf:
            lisp = next(iter(children(run.pid)), None)
        states = "".join("process %d:\n%s" % (pid, threads(pid))
                         for pid in dict.fromkeys([run.pid, lisp]) if pid)
        before = os.path.getsize(output_path)
        if lisp:
            try:
                os.kill(lisp, signal.SIGUSR1)
            except ProcessLookupError:
                pass
            settle(output_path)
    finally:
        kill_group(run)
    run.wait()
    with open(output_path, errors="replace") as f:
        output = f.read()
    return ("Its threads (tid name state wchan):\n%s"
            "Its output, the last of it:\n%s\n"
            "Its backtraces, asked for with SIGUSR1:\n%s"
            % (states or "  (no /proc here)\n", output[:before][-3000:] or "  (none)",
               output[before:][:60000] or "  (none: it did not answer)"))


def annotate(message):
    """A warning on the run's page, under GitHub Actions."""
    if os.environ.get("GITHUB_ACTIONS"):
        print("::warning title=perf-gate::%s"
              % message.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A"), flush=True)


def run_counted(tree, workload, scratch, use_perf, hangs):
    """RUN_ONE, measured again when it hangs. One hung run in a gate is a
    flake, named as a warning; a second, of any workload on either side, is
    no flake and cannot measure. HANGS collects every hang of the gate."""
    while True:
        try:
            return run_one(tree, workload, scratch, use_perf)
        except Hung as hung:
            hangs.append({"workload": workload, "side": side(tree), "report": str(hung)})
            print("perf-gate: %s" % hung, file=sys.stderr)
            if len(hangs) > 1:
                raise CannotMeasure(
                    "%d runs hung past %d s (%s): a hang that comes back is no flake"
                    % (len(hangs), RUN_LIMIT, ", ".join("%s of the %s" % (h["workload"], h["side"])
                                                         for h in hangs)))
            annotate("workload %s of the %s hung past %d s; killed it and measured it again. "
                     "Its threads and backtraces are in this step's log."
                     % (workload, side(tree), RUN_LIMIT))
            print("perf-gate: measuring %s of the %s again" % (workload, side(tree)), file=sys.stderr)


def read_plist(text):
    """The flat (:KEY integer ...) plist PERF-RUN prints."""
    words = text.strip().strip("()").split()
    return {words[i].lstrip(":"): int(words[i + 1]) for i in range(0, len(words) - 1, 2)}


def floors():
    table = dict(FLOORS)
    for item in filter(None, os.environ.get("PERF_FLOORS", "").split(",")):
        name, value = item.split("=")
        table[name] = (float(value), table[name][1])
    return table


def verdicts(base, head, use_perf):
    """One row per workload and metric: medians, the change, regressed or not."""
    rows = []
    table = floors()
    for workload in base:
        counts = sorted(set(base[workload][0]) - set(METRICS))
        for metric in METRICS + counts:
            if metric == "instructions" and not use_perf:
                continue
            if any(metric not in run for run in base[workload] + head[workload]):
                continue
            b = [run[metric] for run in base[workload]]
            h = [run[metric] for run in head[workload]]
            mb, mh = statistics.median(b), statistics.median(h)
            fraction, absolute = table[metric if metric in table else metric.rsplit("-", 1)[-1]]
            regressed = mh > mb * (1 + fraction) + absolute and min(h) > max(b)
            rows.append({"workload": workload, "metric": metric, "base": mb, "head": mh,
                         "change": (mh - mb) / mb if mb else (0.0 if mh == mb else None),
                         "spread": (max(b) - min(b)) / mb if mb else 0.0,
                         "regressed": regressed})
    return rows


def human(metric, value):
    if metric in ("cpu", "wall"):
        return "%.1f ms" % (value / 1e6)
    if metric == "consed":
        return "%.1f MiB" % (value / 1048576)
    if metric.endswith("-bytes"):
        return "%d B" % value
    if metric.endswith("-frames"):
        return "%d" % value
    return "%.1f M" % (value / 1e6)


def change(row):
    return "from 0" if row["change"] is None else "%+.1f%%" % (100 * row["change"])


def table(rows, base_sha, head_label, reps):
    lines = ["perf gate: %s against base %s, %d runs each" % (head_label, base_sha[:12], reps), "",
             "| workload | metric | base | head | change | base spread | |",
             "|---|---|---:|---:|---:|---:|---|"]
    for row in rows:
        lines.append("| %s | %s | %s | %s | %s | %.1f%% | %s |"
                     % (row["workload"], row["metric"], human(row["metric"], row["base"]),
                        human(row["metric"], row["head"]), change(row),
                        100 * row["spread"],
                        "REGRESSED" if row["regressed"]
                        else "ok (not confirmed)" if row.get("unconfirmed") else "ok"))
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("base")
    parser.add_argument("--reps", type=int, default=int(os.environ.get("PERF_REPS", "5")))
    parser.add_argument("--workloads", default=",".join(WORKLOADS))
    parser.add_argument("--json")
    args = parser.parse_args()
    workloads = [w for w in args.workloads.split(",") if w]
    os.makedirs(PERF_HOME, exist_ok=True)
    if not os.access(PERF_HOME, os.W_OK | os.X_OK):
        print("perf-gate: cannot measure: %s is not writable by this user (it belongs to uid %d);"
              " remove it or set PERF_HOME" % (PERF_HOME, os.stat(PERF_HOME).st_uid), file=sys.stderr)
        return 2
    # One run at a time per PERF_HOME: two runs share its base trees and
    # compiled code, and on one box each would be the other's noise.
    lock = open(os.path.join(PERF_HOME, ".lock"), "w")
    try:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        print("perf-gate: another run holds %s; waiting for it" % PERF_HOME, file=sys.stderr)
        fcntl.flock(lock, fcntl.LOCK_EX)
    prune()
    scratch = tempfile.mkdtemp(prefix="run-", dir=PERF_HOME)
    try:
        base_sha, base_path = base_tree(args.base)
        head_sha = git("rev-parse", "HEAD")
        dirty = bool(git("status", "--porcelain", "--untracked-files=no"))
        head_label = "HEAD %s%s" % (head_sha[:12], " (with uncommitted changes)" if dirty else "")
        cells(REPO)
        use_perf = perf_usable()
        if not use_perf:
            print("perf-gate: perf cannot count this user's instructions here; "
                  "gating consed, cpu and wall only", file=sys.stderr)
        started = time.time()
        for tree in (base_path, REPO):
            compile_tree(tree, scratch)
        hangs = []
        def measure(names):
            base, head = {w: [] for w in names}, {w: [] for w in names}
            for rep in range(args.reps):
                order = [(base_path, base), (REPO, head)]
                if rep % 2:
                    order.reverse()
                for workload in names:
                    for tree, sink in order:
                        sink[workload].append(run_counted(tree, workload, scratch, use_perf, hangs))
            return base, head

        base, head = measure(workloads)
        rows = verdicts(base, head, use_perf)
        # A flagged workload is measured again, a fresh round of both sides,
        # and a metric fails only when the second round flags it too: one
        # round's chance of every head run landing above every base run is
        # what a confirmation squares away.
        flagged = sorted({r["workload"] for r in rows if r["regressed"]}, key=workloads.index)
        if flagged:
            print("perf-gate: %s flagged; measuring again to confirm" % ", ".join(flagged), file=sys.stderr)
            again_base, again_head = measure(flagged)
            again = {(r["workload"], r["metric"]): r for r in verdicts(again_base, again_head, use_perf)}
            for row in rows:
                second = again.get((row["workload"], row["metric"]))
                if row["regressed"] and not (second and second["regressed"]):
                    row["regressed"] = False
                    row["unconfirmed"] = True
            for w in flagged:
                base[w + " (again)"], head[w + " (again)"] = again_base[w], again_head[w]
        report = table(rows, base_sha, head_label, args.reps)
        if hangs:
            report += ("\n\nA run hung past %d s, was killed and measured again: %s of the %s."
                       % (RUN_LIMIT, hangs[0]["workload"], hangs[0]["side"]))
        print(report)
        print("\n%.0f s" % (time.time() - started))
        if os.environ.get("GITHUB_STEP_SUMMARY"):
            with open(os.environ["GITHUB_STEP_SUMMARY"], "a") as summary:
                summary.write(report + "\n")
        if args.json:
            with open(args.json, "w") as f:
                json.dump({"base": base_sha, "head": head_sha, "rows": rows, "hangs": hangs,
                           "runs": {"base": base, "head": head}}, f, indent=1)
        regressed = [r for r in rows if r["regressed"]]
        for r in regressed:
            print("perf-gate: %s %s regressed %s (%s -> %s)"
                  % (r["workload"], r["metric"], change(r),
                     human(r["metric"], r["base"]), human(r["metric"], r["head"])), file=sys.stderr)
        return 1 if regressed else 0
    except CannotMeasure as e:
        print("perf-gate: cannot measure: %s" % e, file=sys.stderr)
        return 2
    finally:
        shutil.rmtree(scratch, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
