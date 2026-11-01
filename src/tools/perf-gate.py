#!/usr/bin/env python3
"""perf-gate.py BASE --- does this checkout cost more than BASE, where a
person waits?

Usage: perf-gate.py BASE [--reps N] [--workloads tui,boot,turns] [--json OUT]

Run from a nodecode checkout (`just perf BASE'). The checkout as it stands is
the head; BASE is any revision of its repository (CI passes the pull
request's merge base, or the commit a push moved main from). Both trees run
one bench file, the head's src/tools/perf/bench.lisp, each in an SBCL that
loaded its own :nodecode/test, so the two differ only in the code under
review. The workloads, each its own process:

  tui    a 120x40 shell over a 4000-line transcript: a sentence typed into the
         composer and 40 pages of scrolling, each key one batch through the
         event loop's own fold-then-paint (HANDLE-MESSAGES)
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

Base and head runs alternate, REPS of each (default 5). A metric regressed
when the head's median exceeds the base's by more than its floor AND every
head run costs more than every base run, so one noisy run never fails it:
  instructions 3%   consed 3% + 256 KiB   cpu 20% + 5 ms   wall 25% + 20 ms
PERF_FLOORS="instructions=0.05,wall=0.4" overrides floors (fractions).

Exits 0 when the head holds, 1 when a metric regressed, 2 when it cannot
measure (a tree that does not load, a workload that fails). Prints a table;
under GitHub Actions the table also goes to the job summary.

Trees and their compiled code are kept under PERF_HOME (default
/var/tmp/nodecode-perf): a base is extracted once per commit (git archive)
and compiled once, so a second run against the same base pays only the
measuring. SBCL is `sbcl' on PATH; PERF_SBCL_USERINIT, when set, is its
--userinit (CI's quicklisp).
"""

import argparse
import json
import os
import shutil
import statistics
import subprocess
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(HERE, "..", ".."))
BENCH = os.path.join(HERE, "perf", "bench.lisp")
WORKLOADS = ["tui", "boot", "turns"]
METRICS = ["instructions", "consed", "cpu", "wall"]
# (fraction, absolute) per metric; absolute in the metric's own unit
FLOORS = {"instructions": (0.03, 0), "consed": (0.03, 256 * 1024),
          "cpu": (0.20, 5e6), "wall": (0.25, 20e6)}
PERF_HOME = os.environ.get("PERF_HOME", "/var/tmp/nodecode-perf")


class CannotMeasure(Exception):
    pass


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
    if not os.path.exists(os.path.join(path, ".ready")):
        shutil.rmtree(path, ignore_errors=True)
        os.makedirs(path)
        archive = subprocess.Popen(["git", "archive", sha], cwd=REPO, stdout=subprocess.PIPE)
        subprocess.run(["tar", "-x", "-C", path], stdin=archive.stdout, check=True)
        if archive.wait() != 0:
            raise CannotMeasure("git archive %s failed" % sha)
        cells(path)
        open(os.path.join(path, ".ready"), "w").close()
    return sha, path


def cells(tree):
    r = subprocess.run([os.path.join(tree, "src", "tools", "pull-cells.sh")], cwd=tree,
                       capture_output=True, text=True)
    if r.returncode != 0:
        raise CannotMeasure("pull-cells in %s: %s" % (tree, r.stderr.strip()[-400:]))


def sbcl_command(tree, forms):
    command = ["sbcl", "--dynamic-space-size", "4096", "--control-stack-size", "8MB",
               "--noinform", "--non-interactive"]
    if os.environ.get("PERF_SBCL_USERINIT"):
        command += ["--userinit", os.environ["PERF_SBCL_USERINIT"]]
    command += ["--eval", "(require :asdf)",
                "--eval", '(push (truename "%s/") asdf:*central-registry*)' % os.path.join(tree, "src"),
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
    r = subprocess.run(command, cwd=d, env=environment(d),
                       capture_output=True, text=True, timeout=900)
    if r.returncode != 0 or not os.path.exists(out):
        raise CannotMeasure("workload %s in %s exited %s:\n%s"
                            % (workload, tree, r.returncode, (r.stdout + r.stderr)[-2000:]))
    spans = read_plist(open(out).read())
    result = {"consed": spans["CONSED"], "cpu": spans["CPU-NS"], "wall": spans["WALL-NS"]}
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
        for metric in METRICS:
            if metric == "instructions" and not use_perf:
                continue
            b = [run[metric] for run in base[workload]]
            h = [run[metric] for run in head[workload]]
            mb, mh = statistics.median(b), statistics.median(h)
            fraction, absolute = table[metric]
            regressed = mh > mb * (1 + fraction) + absolute and min(h) > max(b)
            rows.append({"workload": workload, "metric": metric, "base": mb, "head": mh,
                         "change": (mh - mb) / mb if mb else 0.0,
                         "spread": (max(b) - min(b)) / mb if mb else 0.0,
                         "regressed": regressed})
    return rows


def human(metric, value):
    if metric in ("cpu", "wall"):
        return "%.1f ms" % (value / 1e6)
    if metric == "consed":
        return "%.1f MiB" % (value / 1048576)
    return "%.1f M" % (value / 1e6)


def table(rows, base_sha, head_label, reps):
    lines = ["perf gate: %s against base %s, %d runs each" % (head_label, base_sha[:12], reps), "",
             "| workload | metric | base | head | change | base spread | |",
             "|---|---|---:|---:|---:|---:|---|"]
    for row in rows:
        lines.append("| %s | %s | %s | %s | %+.1f%% | %.1f%% | %s |"
                     % (row["workload"], row["metric"], human(row["metric"], row["base"]),
                        human(row["metric"], row["head"]), 100 * row["change"],
                        100 * row["spread"], "REGRESSED" if row["regressed"] else "ok"))
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
        base, head = {w: [] for w in workloads}, {w: [] for w in workloads}
        for rep in range(args.reps):
            order = [(base_path, base), (REPO, head)]
            if rep % 2:
                order.reverse()
            for workload in workloads:
                for tree, sink in order:
                    sink[workload].append(run_one(tree, workload, scratch, use_perf))
        rows = verdicts(base, head, use_perf)
        report = table(rows, base_sha, head_label, args.reps)
        print(report)
        print("\n%.0f s" % (time.time() - started))
        if os.environ.get("GITHUB_STEP_SUMMARY"):
            with open(os.environ["GITHUB_STEP_SUMMARY"], "a") as summary:
                summary.write(report + "\n")
        if args.json:
            with open(args.json, "w") as f:
                json.dump({"base": base_sha, "head": head_sha, "rows": rows,
                           "runs": {"base": base, "head": head}}, f, indent=1)
        regressed = [r for r in rows if r["regressed"]]
        for r in regressed:
            print("perf-gate: %s %s regressed %+.1f%% (%s -> %s)"
                  % (r["workload"], r["metric"], 100 * r["change"],
                     human(r["metric"], r["base"]), human(r["metric"], r["head"])), file=sys.stderr)
        return 1 if regressed else 0
    except CannotMeasure as e:
        print("perf-gate: cannot measure: %s" % e, file=sys.stderr)
        return 2
    finally:
        shutil.rmtree(scratch, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
