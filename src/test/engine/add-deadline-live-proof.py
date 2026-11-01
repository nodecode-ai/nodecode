#!/usr/bin/env python3
"""End-to-end proof that adding a cell ends by the wait it was given.

proofs/AddDeadlines.lean's `answer_lands': a switch told it is waited on for
W seconds keeps its reads and installs under W less a margin, so its answer
reaches the waiter while it still waits and no install runs on after it.

Each case starts `nodecode gateway run` in a scratch account of its own (HOME,
NODECODE_HOME, XDG folders, no service manager), Nodecode's and the community's
cell indexes served on loopback. The account's git config leads the hub's
repositories elsewhere: `hang' to a loopback port that accepts a connection
and never answers (a stalled network: git waits in its TLS handshake for
libcurl's 300 s), `good' to a local repository holding a one-file cell. No
real HOME, network, credential or provider is used. The last line is
PASSED/TOTAL.
"""
import argparse
import http.server
import json
import os
import shutil
import signal
import socket
import subprocess
import sys
import threading
import time
import urllib.error
import urllib.request
from pathlib import Path

HANG_COMMIT = '5f0e3c2a9b8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f'


def scratch_env(root):
    """A whole account of its own: no HOME, cache, config, proxy or service
    manager of the box's."""
    env = {k: v for k, v in os.environ.items()
           if not k.startswith(('NODECODE_', 'XDG_', 'DBUS_', 'SBCL', 'GIT_'))
           and not k.lower().endswith('_proxy')}
    user = root / 'user'
    env.update(HOME=str(user), XDG_CONFIG_HOME=str(user / '.config'),
               XDG_CACHE_HOME=str(root / 'cache'), XDG_DATA_HOME=str(user / '.local/share'),
               XDG_RUNTIME_DIR=str(root / 'run'), NODECODE_HOME=str(root / 'nc'),
               DBUS_SESSION_BUS_ADDRESS='unix:path=' + str(root / 'run/no-bus'),
               LANG='C.UTF-8', TERM='dumb')
    for d in ('user/.config', 'cache', 'user/.local/share', 'run', 'nc', 'work'):
        (root / d).mkdir(mode=0o700, parents=True, exist_ok=True)
    return env


def ours(root):
    """{pid: argv} of every process whose environment names this case's
    scratch cache: what the case started, and what those started."""
    marker = ('XDG_CACHE_HOME=' + str(root / 'cache')).encode()
    found = {}
    for name in os.listdir('/proc'):
        if not name.isdigit() or int(name) == os.getpid():
            continue
        try:
            with open(f'/proc/{name}/environ', 'rb') as f:
                if marker not in f.read().split(b'\0'):
                    continue
            with open(f'/proc/{name}/cmdline', 'rb') as f:
                argv = [a.decode(errors='replace') for a in f.read().split(b'\0') if a]
        except OSError:
            continue
        if argv:
            found[int(name)] = argv
    return found


def gits(root):
    """The git runs alive in this case: git and its remote helpers."""
    return {pid: argv for pid, argv in ours(root).items()
            if os.path.basename(argv[0]).startswith('git')}


def stop_all(root):
    for sig in (signal.SIGTERM, signal.SIGKILL):
        pids = ours(root)
        for pid in pids:
            try:
                os.kill(pid, sig)
            except OSError:
                pass
        deadline = time.monotonic() + 10
        while pids and time.monotonic() < deadline:
            time.sleep(0.3)
            pids = ours(root)
        if not pids:
            return


class Indexes(http.server.BaseHTTPRequestHandler):
    documents = {}

    def log_message(self, *_):
        pass

    def do_GET(self):
        body = self.documents.get(self.path.split('?')[0])
        if body is None:
            self.send_response(404)
            self.end_headers()
            return
        data = json.dumps(body).encode()
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(data)))
        self.end_headers()
        self.wfile.write(data)


def black_hole():
    """A loopback port that accepts every connection and never says a word."""
    server = socket.socket()
    server.bind(('127.0.0.1', 0))
    server.listen(64)
    held = []

    def accept():
        while True:
            try:
                held.append(server.accept()[0])
            except OSError:
                return
    threading.Thread(target=accept, daemon=True).start()
    return server, held


def git(*args, cwd):
    return subprocess.run(['git', '-c', 'user.name=nl', '-c', 'user.email=nl@nl',
                           '-c', 'commit.gpgsign=false', *args],
                          cwd=cwd, check=True, capture_output=True, text=True).stdout.strip()


def good_repository(repos):
    """repos/nl-good.git: the one-file peripheral nl-good, committed. => its commit."""
    folder = repos / 'nl-good.git'
    folder.mkdir(parents=True)
    (folder / 'nl-good.asd').write_text('(defsystem "nl-good" :description "a one-file cell" :serial t :components ((:file "cell")))\n')
    (folder / 'cell.lisp').write_text('(defpackage #:nl-good (:use #:cl) (:export #:start-cell))\n'
                                      '(in-package #:nl-good)\n'
                                      '(defun start-cell (config) (declare (ignore config)) nil)\n')
    git('init', '--quiet', cwd=folder)
    git('add', '-A', cwd=folder)
    git('commit', '--quiet', '-m', 'nl-good', cwd=folder)
    return git('rev-parse', 'HEAD', cwd=folder)


class Account:
    """One case's scratch account, its indexes, its hub and its gateway."""

    def __init__(self, binary, root):
        self.binary, self.root = binary, root
        root.mkdir(parents=True)
        self.env = scratch_env(root)
        self.home = root / 'nc'
        self.hole, self.held = black_hole()
        good = good_repository(root / 'repos')
        (root / 'user/.gitconfig').write_text(
            f'[url "https://127.0.0.1:{self.hole.getsockname()[1]}/"]\n\tinsteadOf = https://hub.invalid/hang/\n'
            f'[url "file://{root / "repos"}/"]\n\tinsteadOf = https://hub.invalid/good/\n')
        hang = {'git': 'https://hub.invalid/hang/cells.git', 'commit': HANG_COMMIT}
        Indexes.documents = {
            '/index.json': {'format': 1, 'cells': [
                dict(hang, name='nl-hang', path='cells/nl-hang', description='a cell on a hub that never answers'),
                dict(hang, name='nl-hang-b', path='cells/nl-hang-b', description='its sibling in that repository'),
                {'name': 'nl-good', 'git': 'https://hub.invalid/good/nl-good.git', 'commit': good,
                 'description': 'a one-file cell'}]},
            '/community.json': {'format': 1, 'cells': []}}
        self.indexes = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Indexes)
        threading.Thread(target=self.indexes.serve_forever, daemon=True).start()
        base = f'http://127.0.0.1:{self.indexes.server_port}'
        self.env.update(NODECODE_HUB_INDEX=base + '/index.json',
                        NODECODE_COMMUNITY_INDEX=base + '/community.json')
        (self.home / 'config.jsonc').write_text(json.dumps({'update': {'mode': 'off'}}))
        self.log = open(root / 'gateway.log', 'wb')
        self.gateway = subprocess.Popen([binary, 'gateway', 'run'], cwd=root / 'work', env=self.env,
                                        stdin=subprocess.DEVNULL, stdout=self.log, stderr=subprocess.STDOUT,
                                        start_new_session=True)
        deadline = time.monotonic() + 180
        while self.identity() is None:
            if self.gateway.poll() is not None or time.monotonic() > deadline:
                raise RuntimeError(f'the gateway never served: {self.tail()}')
            time.sleep(0.5)

    def tail(self):
        try:
            return (self.root / 'gateway.log').read_text(errors='replace')[-1500:]
        except OSError:
            return ''

    def credentials(self):
        port = json.loads((self.home / 'history.db.owner').read_text())['port']
        return port, (self.home / 'operator-token').read_text().strip()

    def identity(self):
        try:
            port, token = self.credentials()
            request = urllib.request.Request(f'http://127.0.0.1:{port}/api/gateway/identity',
                                             headers={'Authorization': f'Bearer {token}'})
            with urllib.request.urlopen(request, timeout=5) as answer:
                return json.load(answer)
        except (OSError, ValueError, KeyError):
            return None

    def switch(self, body, wait):
        """POST /api/gateway/cells as a waiter that waits WAIT seconds.
        => (seconds it took, the answer's text), the text None when none came."""
        port, token = self.credentials()
        request = urllib.request.Request(f'http://127.0.0.1:{port}/api/gateway/cells',
                                         data=json.dumps(body).encode(), method='POST',
                                         headers={'Authorization': f'Bearer {token}',
                                                  'Content-Type': 'application/json'})
        started = time.monotonic()
        try:
            with urllib.request.urlopen(request, timeout=wait) as answer:
                text = answer.read().decode(errors='replace')
        except urllib.error.HTTPError as refused:
            text = refused.read().decode(errors='replace')
        except (OSError, ValueError) as error:
            text = None if 'timed out' in str(error) else f'no answer: {error}'
        return time.monotonic() - started, text

    def add(self, *words, timeout=240):
        """`nodecode add WORDS...` => (seconds, exit code, its output)."""
        started = time.monotonic()
        try:
            run = subprocess.run([self.binary, 'add', *words], cwd=self.root / 'work', env=self.env,
                                 stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=timeout)
            code, out = run.returncode, run.stdout + run.stderr
        except subprocess.TimeoutExpired as late:
            code, out = None, f'still running after {timeout} s: {late.stdout or ""}'
        return time.monotonic() - started, code, out

    def installed(self, name):
        return [str(p.relative_to(self.home)) for p in self.home.rglob(name)
                if p.is_dir() and p.relative_to(self.home).parts[0] not in ('hub', '.cell-staging')]

    def staging(self):
        staging = self.home / '.cell-staging'
        return sorted(p.name for p in staging.iterdir()) if staging.is_dir() else []

    def settled(self, names, problems, grace=3):
        """After a refused switch: no git left running, no staging, nothing of NAMES installed."""
        deadline = time.monotonic() + grace
        while gits(self.root) and time.monotonic() < deadline:
            time.sleep(0.3)
        left = gits(self.root)
        if left:
            problems.append(f'git still runs after the answer: {sorted(" ".join(a[:4]) for a in left.values())}')
        if self.staging():
            problems.append(f'staging left: {self.staging()}')
        for name in names:
            if self.installed(name):
                problems.append(f'{name} installed anyway: {self.installed(name)}')

    def close(self):
        stop_all(self.root)
        self.indexes.shutdown()
        self.hole.close()
        for each in self.held:
            each.close()
        self.log.close()


def add_hang_wait(account, problems):
    took, code, out = account.add('nc://nl-hang', '--yes', '--wait', '12', timeout=60)
    if not took < 12:
        problems.append(f'add --wait 12 took {took:.1f} s')
    if code in (0, None):
        problems.append(f'exit {code}')
    for phrase in ('nl-hang did not come on within 6 s: the time ran out while fetching nl-hang from '
                   'https://hub.invalid/hang/cells.git; nothing was installed',):
        if phrase not in out:
            problems.append(f'not said: {phrase!r}')
    account.settled(['nl-hang'], problems)
    return f'{took:.1f} s, exit {code}: {out.strip()[-300:]}'


def post_hang_seconds(account, problems):
    took, text = account.switch({'name': 'nl-hang', 'on': True, 'seconds': 10}, wait=10)
    if text is None:
        problems.append(f'no answer within the 10 s it was told ({took:.1f} s)')
    elif ('nl-hang did not come on within 5 s: the time ran out while fetching nl-hang from '
          'https://hub.invalid/hang/cells.git; nothing was installed') not in text:
        problems.append(f'answered without the deadline: {text[:300]}')
    account.settled(['nl-hang'], problems)
    return f'{took:.1f} s: {(text or "none")[:300]}'


def second_behind_stalled_fetch(account, problems):
    first = {}
    thread = threading.Thread(target=lambda: first.update(
        zip(('took', 'text'), account.switch({'name': 'nl-hang', 'on': True, 'seconds': 40}, wait=45))))
    thread.start()
    deadline = time.monotonic() + 20
    while not any('fetch' in argv for argv in gits(account.root).values()):
        if time.monotonic() > deadline:
            problems.append('the first switch never started its fetch')
            break
        time.sleep(0.2)
    took, text = account.switch({'name': 'nl-hang-b', 'on': True, 'seconds': 8}, wait=8)
    if text is None:
        problems.append(f'the second switch had no answer within the 8 s it was told ({took:.1f} s)')
    elif ('nl-hang-b did not come on within 4 s: the time ran out while waiting for another cell\'s fetch '
          'to finish; nothing was installed') not in text:
        problems.append(f'the second switch answered without its deadline: {text[:300]}')
    thread.join(50)
    if first.get('text') is None:
        problems.append(f'the first switch had no answer within the 40 s it was told')
    elif ('nl-hang did not come on within 20 s: the time ran out while fetching nl-hang from '
          'https://hub.invalid/hang/cells.git; nothing was installed') not in first['text']:
        problems.append(f'the first switch answered without its deadline: {first["text"][:300]}')
    account.settled(['nl-hang', 'nl-hang-b'], problems)
    return (f'second {took:.1f} s: {(text or "none")[:200]} | '
            f'first {first.get("took", 0):.1f} s: {(first.get("text") or "none")[:200]}')


def add_good(*extra):
    def case(account, problems):
        took, code, out = account.add('nc://nl-good', '--yes', *extra)
        if code != 0:
            problems.append(f'exit {code}')
        if 'nl-good is on' not in out:
            problems.append('not said: "nl-good is on"')
        if not account.installed('nl-good'):
            problems.append('nl-good is not installed')
        if account.staging():
            problems.append(f'staging left: {account.staging()}')
        return f'{took:.1f} s, exit {code}: {out.strip()[-300:]}'
    return case


CASES = [
    ('add --wait 12 on a stalled hub ends within 12 s, says so, and leaves no git running',
     add_hang_wait),
    ('a switch POSTed as waited on for 10 s answers within 10 s, and leaves no git running',
     post_hang_seconds),
    ('a second switch behind a stalled fetch answers within its own 8 s, the first within its 40 s',
     second_behind_stalled_fetch),
    ('a reachable cell installs and comes on (control, no --wait)', add_good()),
    ('a reachable cell installs and comes on under add --wait 60', add_good('--wait', '60')),
]


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument('--binary', required=True)
    parser.add_argument('--root', required=True)
    parser.add_argument('--only', type=int, nargs='*', help='the cases to run, 1-based')
    args = parser.parse_args()
    binary = str(Path(args.binary).resolve())
    root = Path(args.root)
    if root.exists():
        shutil.rmtree(root)
    passed = 0
    chosen = [i for i in range(len(CASES)) if not args.only or i + 1 in args.only]
    for i in chosen:
        name, case = CASES[i]
        problems = []
        account = None
        try:
            account = Account(binary, root / f'case-{i + 1}')
            said = case(account, problems)
        except Exception as error:
            problems.append(f'{type(error).__name__}: {error}')
            said = ''
        finally:
            if account:
                if problems:
                    said += f' | gateway log: {account.tail()[-600:]!r}'
                account.close()
            else:
                stop_all(root / f'case-{i + 1}')
        if problems:
            print(f'FAIL {name}: {"; ".join(problems)}')
            print(f'     {said}')
        else:
            passed += 1
            print(f'PASS {name}')
            print(f'     {said}')
        sys.stdout.flush()
    print(f'{passed}/{len(chosen)}')


if __name__ == '__main__':
    main()
