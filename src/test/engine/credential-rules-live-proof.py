#!/usr/bin/env python3
"""End-to-end proof of the credential chain's trim rule and the rules files' budget.

Each case runs `nodecode --ephemeral rpc` in a fresh isolated HOME, NODECODE_HOME
and XDG_CACHE_HOME against a loopback chat-completions fixture, starts one turn,
and reads what the fixture received: the Authorization header the credential
chain resolved, and whether a rules file's marker rode in the prompt.
No real HOME, credential, cell or provider is used. The last line is PASSED/TOTAL.
"""
import argparse
import http.server
import json
import os
import queue
import shutil
import signal
import subprocess
import sys
import threading
import time
from pathlib import Path

LIMIT = 65536

# name, auth.json keys (None: no file), environment, workspace files, expectation
CASES = [
    ('a blank auth.json key is absent: the environment answers',
     {'fixture': '   '}, {'FIXTURE_API_KEY': 'sk-env-good'}, {},
     ('auth', 'Bearer sk-env-good')),
    ('a padded auth.json key is sent trimmed',
     {'fixture': '  sk-auth-good  '}, {}, {},
     ('auth', 'Bearer sk-auth-good')),
    ('a blank NODECODE_API_KEY is absent: the environment answers',
     None, {'NODECODE_API_KEY': '  ', 'FIXTURE_API_KEY': 'sk-env-good'}, {},
     ('auth', 'Bearer sk-env-good')),
    ('a padded NODECODE_API_KEY is sent trimmed',
     None, {'NODECODE_API_KEY': ' sk-flag-good '}, {},
     ('auth', 'Bearer sk-flag-good')),
    ('a clean auth.json key wins over the environment (control)',
     {'fixture': 'sk-auth-good'}, {'FIXTURE_API_KEY': 'sk-env-good'}, {},
     ('auth', 'Bearer sk-auth-good')),
    ('a blank AGENTS.md passes to the folder\'s CLAUDE.md',
     None, {'FIXTURE_API_KEY': 'sk-env-good'},
     {'AGENTS.md': '\n  \n', 'CLAUDE.md': 'RULE-CLAUDE-7731 answer in haiku\n'},
     ('in', 'RULE-CLAUDE-7731')),
    ('an unreadable AGENTS.md passes to the folder\'s CLAUDE.md',
     None, {'FIXTURE_API_KEY': 'sk-env-good'},
     {'AGENTS.md': None, 'CLAUDE.md': 'RULE-CLAUDE-8842 answer in haiku\n'},
     ('in', 'RULE-CLAUDE-8842')),
    ('a non-blank AGENTS.md still hides CLAUDE.md (control)',
     None, {'FIXTURE_API_KEY': 'sk-env-good'},
     {'AGENTS.md': 'RULE-AGENTS-1203\n', 'CLAUDE.md': 'RULE-CLAUDE-1204\n'},
     ('in-not', 'RULE-AGENTS-1203', 'RULE-CLAUDE-1204')),
    ('a file over 64 KiB of UTF-8 is not read, though under 64 Ki characters',
     None, {'FIXTURE_API_KEY': 'sk-env-good'},
     {'AGENTS.md': 'RULE-BYTES-4410\n' + '\u65e5\u672c\u8a9e' * 8000},
     ('not', 'RULE-BYTES-4410')),
    ('a file of exactly 64 KiB of UTF-8 is read (control)',
     None, {'FIXTURE_API_KEY': 'sk-env-good'},
     {'AGENTS.md': 'RULE-ASCII-5520\n' + 'a' * (LIMIT - 17) + '\n'},
     ('in', 'RULE-ASCII-5520')),
]


def run_case(binary, root, case):
    name, keys, environment, files, expect = case
    for sub in ('home', 'nc', 'cache', 'tmp', 'workspace'):
        (root / sub).mkdir(parents=True)
    seen = []

    class Fixture(http.server.BaseHTTPRequestHandler):
        def log_message(self, *_):
            pass

        def do_GET(self):
            data = json.dumps({'data': [{'id': 'proof-fixture'}]}).encode()
            self.send_response(200)
            self.send_header('Content-Type', 'application/json')
            self.send_header('Content-Length', str(len(data)))
            self.end_headers()
            self.wfile.write(data)

        def do_POST(self):
            body = json.loads(self.rfile.read(int(self.headers['Content-Length'])))
            agent = any(t.get('function', {}).get('name') == 'eval' for t in body.get('tools', []))
            if agent:
                seen.append({'authorization': self.headers.get('Authorization'), 'body': body})
            self.send_response(200)
            self.send_header('Content-Type', 'text/event-stream')
            self.send_header('Connection', 'close')
            self.end_headers()
            for delta, reason in (({'role': 'assistant', 'content': 'proof ok'}, None), ({}, 'stop')):
                event = {'id': 'proof', 'object': 'chat.completion.chunk', 'model': 'proof-fixture',
                         'choices': [{'index': 0, 'delta': delta, 'finish_reason': reason}]}
                self.wfile.write(('data: ' + json.dumps(event) + '\n\n').encode())
            self.wfile.write(b'data: [DONE]\n\n')
            self.wfile.flush()
            self.close_connection = True

    server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Fixture)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    config = {'update': {'mode': 'off'},
              'general': {'default_model': {'provider': 'fixture', 'model': 'proof-fixture'}},
              'providers': {'fixture': {'sdk': 'openai-completions',
                                        'base_url': f'http://127.0.0.1:{server.server_port}/v1'}}}
    (root / 'nc/config.jsonc').write_text(json.dumps(config))
    if keys is not None:
        auth = {'api_keys': {p: {'provider': p, 'key': k} for p, k in keys.items()}}
        (root / 'nc/auth.json').write_text(json.dumps(auth))
        os.chmod(root / 'nc/auth.json', 0o600)
    for file, text in files.items():
        path = root / 'workspace' / file
        if text is None:
            path.write_text('RULE-UNREADABLE-0001\n')
            os.chmod(path, 0)
        else:
            path.write_text(text, encoding='utf-8')
    env = {'PATH': os.environ['PATH'], 'LANG': 'C.UTF-8', 'TERM': 'xterm-256color',
           'HOME': str(root / 'home'), 'NODECODE_HOME': str(root / 'nc'),
           'XDG_CACHE_HOME': str(root / 'cache'), 'TMPDIR': str(root / 'tmp')}
    env.update(environment)
    frames = queue.Queue()
    with (root / 'stderr.log').open('w') as stderr:
        process = subprocess.Popen([str(binary), '--ephemeral', 'rpc'], cwd=root / 'workspace',
                                   env=env, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                   stderr=stderr, text=True, start_new_session=True)

        def read_frames():
            with (root / 'frames.jsonl').open('w') as output:
                for line in process.stdout:
                    output.write(line)
                    try:
                        frames.put(json.loads(line))
                    except json.JSONDecodeError:
                        pass
        threading.Thread(target=read_frames, daemon=True).start()
        process.stdin.write(json.dumps({'type': 'start_turn', 'request': {'prompt': 'hi'}}) + '\n')
        process.stdin.flush()
        ended, deadline = None, time.monotonic() + 120
        try:
            while ended is None and time.monotonic() < deadline:
                frame = frames.get(timeout=max(0.1, deadline - time.monotonic()))
                payload = frame.get('payload', {})
                if payload.get('type') == 'session_stream_event':
                    event = payload.get('frame', {}).get('event', {})
                    kind = event.get('type') or event.get('kind')
                    if kind in ('turn.completed', 'turn.failed', 'turn.cancelled'):
                        ended = kind
        except queue.Empty:
            pass
        finally:
            process.stdin.close()
            try:
                process.wait(timeout=20)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait()
            server.shutdown()
    for file, text in files.items():
        if text is None:
            os.chmod(root / 'workspace' / file, 0o600)
    request = seen[0] if seen else None
    text = json.dumps(request['body'], ensure_ascii=False) if request else ''
    authorization = request['authorization'] if request else None
    kind = expect[0]
    if request is None:
        passed, got = False, f'no agent request reached the fixture (turn: {ended})'
    elif kind == 'auth':
        passed, got = authorization == expect[1], f'Authorization {authorization!r}'
    elif kind == 'in':
        passed, got = expect[1] in text, f'{expect[1]} in prompt: {expect[1] in text}'
    elif kind == 'not':
        passed, got = expect[1] not in text, f'{expect[1]} in prompt: {expect[1] in text}'
    else:
        passed = expect[1] in text and expect[2] not in text
        got = f'{expect[1]} in prompt: {expect[1] in text}, {expect[2]} in prompt: {expect[2] in text}'
    (root / 'result.json').write_text(json.dumps({'case': name, 'passed': passed, 'got': got,
                                                  'turn': ended}, indent=2))
    return passed, got


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--binary', required=True)
    parser.add_argument('--root', required=True)
    args = parser.parse_args()
    binary = Path(args.binary).resolve()
    root = Path(args.root).resolve()
    root.mkdir(parents=True, exist_ok=False)
    passed = 0
    for index, case in enumerate(CASES, 1):
        ok, got = run_case(binary, root / f'case-{index:02d}', case)
        passed += ok
        print(f"{'PASS' if ok else 'FAIL'} {index:2d} {case[0]} - {got}", flush=True)
    print(f'{passed}/{len(CASES)}')


if __name__ == '__main__':
    main()
