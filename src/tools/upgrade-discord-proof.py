#!/usr/bin/env python3
"""upgrade-discord-proof.py -- a Discord bot keeps answering through an update.

Usage: upgrade-discord-proof.py CANDIDATE [--from PREVIOUS] [--host URL] [--budget SECONDS] [--keep]

CANDIDATE and PREVIOUS are what upgrade-proof.py takes: the release tarball
under proof, and the build users run when it is published (a tarball, a
label, or `stable', the default).

A person's bot is a gateway serving a channel, and an update is where it went
silent: the build that came up beside the old one loaded the old build's
channel kit, its packages clashed, and every channel was refused until a
restart. So here, in a scratch account, against a Discord and a model played
on 127.0.0.1 (discord-qa.py's), with every shipped cell on:

  1. PREVIOUS's gateway serves by hand (no unit) and the bot answers an
     @mention: the control.
  2. The release host's stable pointer moves to CANDIDATE. The gateway takes
     it live, restarts itself onto it, or -- when PREVIOUS cannot, which
     upgrade-proof.py gates -- is restarted by hand, as its person would.
     (a) Once CANDIDATE serves, the bot answers.
  3. The handoff the incident had: a gateway of PREVIOUS holds the store, a
     gateway of CANDIDATE boots and waits for it, a process of PREVIOUS points
     the home's shipped-cell links back into its own release, and the old
     gateway lets go.
     (b) CANDIDATE serves, and the bot answers.
  4. (c) One organism serves the home, and no notice says a channel was
     refused.

Exits 0 when all three hold (or PREVIOUS is CANDIDATE), 1 when one does not,
printing the timeline, the notices and the gateway log's tail, and 2 when
the proof cannot run. Every process it started is stopped by pid."""

import argparse
import importlib.util
import json
import os
import platform
import shutil
import signal
import subprocess
import sys
import tempfile
import threading
import time

HERE = os.path.dirname(os.path.abspath(__file__))


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


up = load("upgrade_proof", "upgrade-proof.py")
qa = load("discord_qa", "discord-qa.py")


def say(text):
    print(f"upgrade-discord-proof: {text}", flush=True)


up.say = say


class Run:
    def __init__(self, root, home, work, releases, env, log_path):
        self.root, self.home, self.work, self.releases = root, home, work, releases
        self.env, self.log_path = env, log_path
        self.timeline = up.Timeline()
        self.said = set()

    def note(self, text):
        self.timeline.note(text)

    def start(self, binary):
        """`BINARY gateway run', by hand, logging beside the others."""
        with open(self.log_path, "a") as log:
            log.write(f"\n==== {binary} gateway run\n")
            log.flush()
            return subprocess.Popen([binary, "gateway", "run"], cwd=self.work, env=self.env,
                                    stdin=subprocess.DEVNULL, stdout=log, stderr=subprocess.STDOUT,
                                    start_new_session=True)

    def serving(self, label, timeout, pid=None):
        """Wait for the gateway answering the home to report LABEL (and be
        PID, when given) => its identity, or None."""
        deadline = time.monotonic() + timeout
        seen = None
        while time.monotonic() < deadline:
            seen = up.identity(self.home)
            if seen is not None:
                fresh = [n for n in seen.get("notices") or () if n not in self.said]
                for n in fresh:
                    self.said.add(n)
                    self.note(f"notice: {n}")
                if up.label_of(seen["version"]) == label and (pid is None or seen["pid"] == pid):
                    return seen
            time.sleep(1)
        return None

    def stop(self, proc):
        """SIGTERM PROC, as its person's stop would, and wait it out."""
        if proc.poll() is None:
            proc.send_signal(signal.SIGTERM)
            try:
                proc.wait(timeout=30)
            except subprocess.TimeoutExpired:
                proc.kill()
                proc.wait()

    def links(self):
        return up.cell_releases(self.home, self.releases)

    def log_has(self, text, since):
        try:
            with open(self.log_path, errors="replace") as log:
                log.seek(since)
                return text in log.read()
        except OSError:
            return False

    def log_size(self):
        try:
            return os.path.getsize(self.log_path)
        except OSError:
            return 0

    def tail(self, n=40):
        try:
            with open(self.log_path, errors="replace") as log:
                return "".join(log.readlines()[-n:]).rstrip()
        except OSError:
            return ""


def ask(dc, model, n, timeout=120):
    """Mike @mentions the bot in the flat channel => (answered, detail)."""
    key, marker = f"qa-ping{n}", f"PING-{n}-OK"
    model.script(key, text=f"{marker} still here.")
    message = dc.say("mike", f"{key} are you there", channel=qa.FLAT)
    began = time.monotonic()
    try:
        qa.answer_in(dc, qa.FLAT, marker, after=message["id"], timeout=timeout)
        return True, f"answered in {time.monotonic() - began:.1f}s"
    except qa.Failure as failure:
        return False, str(failure)


def identified(dc):
    with dc.cv:
        return len([c for c in dc.connects if c[0] == "identify"])


def reconnected(dc, before, timeout=120):
    """Wait for the bot to identify again past BEFORE identifies => whether it did."""
    try:
        dc.wait(lambda: len([c for c in dc.connects if c[0] == "identify"]) > before,
                "the bot to identify again", timeout=timeout)
        return True
    except qa.Failure:
        return False


def channel_refusals(notices):
    words = ("discord", "channel", "slack", "telegram", "email", "matrix")
    return [n for n in notices if "refused" in n.lower() and any(w in n.lower() for w in words)]


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("candidate", help="the release tarball under proof")
    parser.add_argument("--from", dest="previous", default="stable",
                        help="the build users run: a tarball, a label, or stable (default)")
    parser.add_argument("--host", default="https://nodecode.ai/dl", help="where a label is fetched")
    parser.add_argument("--budget", type=float, default=120.0,
                        help="seconds from the pointer's move for the gateway to take the candidate itself")
    parser.add_argument("--keep", action="store_true", help="keep the scratch directory")
    args = parser.parse_args()

    plat = up.platform_name()
    if plat is None:
        say(f"no release arm for {platform.system()} {platform.machine()}")
        return 2
    root = tempfile.mkdtemp(prefix="upgrade-discord-proof-")
    mirror, home, work, files = (os.path.join(root, d) for d in ("mirror", "home", "work", "discord-files"))
    releases = os.path.join(root, "releases")
    for d in (mirror, home, work, files):
        os.makedirs(d)
    env = up.scratch_env(root)
    dc, model = qa.Discord(files), qa.Model()
    for server in (dc, model):
        threading.Thread(target=server.serve_forever, daemon=True).start()
    organism_env = dict(env, NODECODE_HOME=home, DISCORD_BOT_TOKEN="proof-token-not-a-real-one")
    run = Run(root, home, work, releases, organism_env, os.path.join(root, "gateway.log"))
    failures = []
    try:
        try:
            candidate = up.take_tarball(os.path.abspath(args.candidate), mirror)
            previous = up.take_previous(args.previous, args.host, plat, mirror, root)
        except (RuntimeError, OSError, ValueError) as error:
            say(f"cannot lay out the two releases: {error}")
            return 2
        if previous == candidate:
            say(f"SKIP: the previous release is the candidate itself ({candidate}): nothing to upgrade")
            return 0
        pointer = os.path.join(mirror, f"stable-{plat}")
        with open(pointer, "w") as f:
            f.write(previous + "\n")
        run.note(f"previous {previous}, candidate {candidate}, {plat}; Discord :{dc.port}, model :{model.server_port}")

        # The previous release, installed as its own installer installs it,
        # every shipped cell on, the bot pointed at the Discord played here.
        installer = up.installer_of(os.path.join(mirror, f"nodecode-{previous}-{plat}.tar.gz"), root)
        install = subprocess.run(["sh", installer, "--no-modify-path"], cwd=work,
                                 env=dict(env, NODECODE_BASE_URL="file://" + mirror, NODECODE_HOME=releases),
                                 capture_output=True, text=True, timeout=600)
        if install.returncode != 0:
            say("installing the previous release failed:\n" + (install.stdout + install.stderr)[-2000:])
            return 2
        current = os.path.join(releases, "current", "nodecode")
        old = os.path.join(releases, previous, "nodecode")
        mock = {"base_url": f"http://127.0.0.1:{model.server_port}/v1", "sdk": "openai-completions",
                "api_key": "upgrade-discord-proof", "models": [{"id": "mock-fast", "context_window": 200000}]}
        with open(os.path.join(home, "config.jsonc"), "w") as f:
            json.dump({"general": {"default_model": {"provider": "mock", "model": "mock-fast"},
                                   "auxiliary_model": {"provider": "mock", "model": "mock-fast"}},
                       "providers": {"mock": mock},
                       "update": {"mode": "auto", "check_minutes": 1, "feed": "off"},
                       "channels": {"discord": {
                           "bot_token_env": "DISCORD_BOT_TOKEN",
                           "api_base": f"http://127.0.0.1:{dc.port}/api/v10",
                           "gateway_url": f"ws://127.0.0.1:{dc.port}/gateway",
                           "allowed_users": [qa.USERS["mike"]], "owner": [qa.USERS["mike"]],
                           "require_mention": True, "flat_channels": [qa.FLAT],
                           "catch_up_minutes": 60}}}, f, indent=1)
        cells = up.shipped_cells(os.path.join(releases, previous))
        for name in cells:
            added = subprocess.run([current, "add", f"nc://{name}"], cwd=work, env=organism_env,
                                   stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=300)
            if added.returncode != 0:
                say(f"`add nc://{name}' failed on the previous release:\n" + (added.stdout + added.stderr)[-1000:])
                return 2
        run.note(f"installed {previous} with its {len(cells)} shipped cells on")

        # 1. The control: the previous release's bot answers.
        gateway = run.start(current)
        seen = run.serving(previous, 180)
        if seen is None:
            say(f"the previous gateway never served as {previous}:\n{run.tail()}")
            return 2
        try:
            qa.connected(dc)
        except qa.Failure as failure:
            say(f"the previous release's bot never reached Discord: {failure}\n{run.tail()}")
            return 2
        started = seen.get("started_at")
        answered, detail = ask(dc, model, 1)
        run.note(f"control: gateway pid {seen['pid']} serves {seen['version']} (no unit); bot {detail}")
        if not answered:
            say("the previous release's bot did not answer: the stand-ins here, not the upgrade\n" + run.tail())
            return 2

        # 2. The release lands.
        connects = identified(dc)
        with open(pointer, "w") as f:
            f.write(candidate + "\n")
        run.note(f"stable pointer moved to {candidate}")
        seen = run.serving(candidate, args.budget)
        if seen is not None:
            how = "in place" if seen["pid"] == gateway.pid else "as a new process"
            run.note(f"the gateway took {candidate} itself, {how} (pid {seen['pid']})")
        else:
            stuck = up.identity(home)
            run.note(f"the gateway still reports {stuck and stuck['version']} after {args.budget:.0f}s "
                     f"(upgrade-proof.py gates that); restarting it by hand, as its person would")
            run.stop(gateway)
            connects = identified(dc)
            gateway = run.start(current)
            seen = run.serving(candidate, 180)
            if seen is None:
                failures.append(f"(a) a gateway started on `current' never served {candidate}")
        if seen is not None:
            # A restart, in place or by hand, reconnects the bot; a release
            # taken live keeps the connection it had.
            if seen.get("started_at") != started:
                try:
                    reconnected(dc, connects)
                except qa.Failure:
                    pass
            answered, detail = ask(dc, model, 2)
            run.note(f"(a) {candidate} serves (pid {seen['pid']}); links into {run.links()}; bot {detail}")
            if not answered:
                failures.append(f"(a) once {candidate} served, the bot did not answer: {detail}")

        # 3. The handoff: an old gateway holds the store while a new one boots
        # and waits, and an old process points the links back at its release.
        run.stop(gateway)
        holder = run.start(old)
        if run.serving(previous, 180, pid=holder.pid) is None:
            say(f"a gateway of {previous} never served for the handoff:\n{run.tail()}")
            return 2
        run.note(f"handoff: a gateway of {previous} (pid {holder.pid}) holds the store")
        since = run.log_size()
        gateway = run.start(current)
        deadline = time.monotonic() + 180
        while time.monotonic() < deadline and not run.log_has("waiting: pid", since):
            if gateway.poll() is not None:
                break
            time.sleep(1)
        if not run.log_has("waiting: pid", since):
            say(f"the candidate's gateway never booted to wait for the store:\n{run.tail()}")
            return 2
        run.note(f"handoff: a gateway of {candidate} (pid {gateway.pid}) booted and waits; links into {run.links()}")
        relink = subprocess.run([old, "trial"], input="(progn (nlk::refresh-installed-cells) :relinked)\n",
                                cwd=work, env=organism_env, capture_output=True, text=True, timeout=300)
        run.note(f"handoff: a process of {previous} pointed the links into {run.links()}"
                 + ("" if relink.returncode == 0 else f" (trial exited {relink.returncode})"))
        run.stop(holder)
        connects = identified(dc)
        seen = run.serving(candidate, 180, pid=gateway.pid)
        if seen is None:
            failures.append(f"(b) after the handoff no gateway of {candidate} served")
        else:
            try:
                reconnected(dc, connects)
            except qa.Failure:
                pass
            answered, detail = ask(dc, model, 3)
            run.note(f"(b) {candidate} serves (pid {seen['pid']}) after the handoff; bot {detail}")
            if not answered:
                failures.append(f"(b) after the handoff the bot did not answer: {detail}")

        # 4. What is left.
        serving = up.organisms(root)
        run.note("organisms serving the home: " +
                 (", ".join(f"{pid} ({' '.join(argv[1:3])})" for pid, argv in sorted(serving.items())) or "none"))
        if len(serving) != 1:
            failures.append(f"(c) {len(serving)} organisms serve the home, not one")
        refused = channel_refusals((up.identity(home) or {}).get("notices") or ())
        if refused:
            failures.append("(c) a channel was refused: " + "; ".join(refused))
        if failures:
            say("gateway log, last lines:\n" + run.tail())
            for failure in failures:
                say("FAIL: " + failure)
            return 1
        say(f"PASS: {previous} -> {candidate}: the bot answered after the update and after the handoff, "
            f"one organism serves, no channel was refused")
        return 0
    finally:
        up.stop_all(root)
        for server in (dc, model):
            server.shutdown()
        if args.keep:
            say(f"kept {root}")
        else:
            shutil.rmtree(root, ignore_errors=True)


if __name__ == "__main__":
    signal.signal(signal.SIGTERM, lambda signum, frame: sys.exit(128 + signum))
    sys.exit(main())
