#!/usr/bin/env python3
"""Live proof of the message board on a released binary.

Four sessions on one organism, each driven by its own `nodecode rpc`, against a
loopback-only OpenAI fixture; the board cell is installed in a scratch home.

  A posts while B's tool runs: B's next round carries the post, mid-turn, after
  an unchanged prefix.  D, idle and @mentioned, is woken and its first request
  carries the post.  C, idle and not named, runs nothing until it is prompted,
  and its next turn's first request carries the post.  A is never told its own.

usage, from a Nodecode release built with `just release` (the :round point):
  python3 test/live-proof.py --binary ./nodecode --cell tools/nodecode-board \
      --root /tmp/board-proof-new-directory --cache /tmp/board-proof-cache
The root must not exist. HOME, NODECODE_HOME and TMPDIR are scratch under it;
--cache is XDG_CACHE_HOME (a build's own, so its warm image is the one run).
"""
import argparse
import http.server
import json
import os
from pathlib import Path
import queue
import subprocess
import threading
import time

SESSIONS = ['s-BOARDA', 's-BOARDB', 's-BOARDC', 's-BOARDD']
POST = 'the build is green; @s-BOARDD, run the tests.'


def content_text(message):
    content = message.get('content')
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        return '\n'.join(str(part.get('text', '')) for part in content if isinstance(part, dict))
    return ''


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--binary', required=True)
    parser.add_argument('--cell', required=True)
    parser.add_argument('--root', required=True)
    parser.add_argument('--cache', required=True)
    args = parser.parse_args()
    binary = Path(args.binary).resolve()
    root = Path(args.root).resolve()
    root.mkdir(parents=True, exist_ok=False)
    for name in ('home', 'nc', 'nc/cells', 'tmp', 'workspace'):
        (root / name).mkdir()
    (root / 'nc/cells/nodecode-board').symlink_to(Path(args.cell).resolve())
    flag = root / 'posted'

    lock = threading.Lock()
    served = []                      # (label, body) for every agent request
    b_waiting = threading.Event()
    tokens = ['A-JOIN', 'B-JOIN', 'C-JOIN', 'D-JOIN', 'B-WORK', 'A-POST', 'C-AGAIN']

    def classify(body):
        msgs = body.get('messages', [])
        best = None
        for i, m in enumerate(msgs):
            if m.get('role') != 'user':
                continue
            text = content_text(m)
            for token in tokens:
                if token in text:
                    best = (i, token)
            if 'mentioned you' in text:
                best = (i, 'WAKE')
        if best is None:
            return None, False, msgs
        i, token = best
        done = any(m.get('role') == 'tool' for m in msgs[i + 1:])
        if token == 'WAKE':
            joined = [t for t in ('A-JOIN', 'B-JOIN', 'C-JOIN', 'D-JOIN')
                      if any(t in content_text(m) for m in msgs)]
            token = 'WAKE-' + (joined[-1][0] if joined else '?')
        return token, done, msgs

    def eval_call(call_id, form, yield_ms=None):
        arguments = {'form': form}
        if yield_ms:
            arguments['yield_time_ms'] = yield_ms
        return {'role': 'assistant', 'tool_calls': [{
            'index': 0, 'id': call_id, 'type': 'function',
            'function': {'name': 'eval', 'arguments': json.dumps(arguments)}}]}

    class Fixture(http.server.BaseHTTPRequestHandler):
        def log_message(self, *_):
            pass

        def do_GET(self):
            data = json.dumps({'data': [{'id': 'board-fixture'}]}).encode()
            self.send_response(200)
            self.send_header('Content-Type', 'application/json')
            self.send_header('Content-Length', str(len(data)))
            self.end_headers()
            self.wfile.write(data)

        def do_POST(self):
            body = json.loads(self.rfile.read(int(self.headers['Content-Length'])))
            agent = any(t.get('function', {}).get('name') == 'eval' for t in body.get('tools', []))
            delta = {'role': 'assistant', 'content': 'Fixture Session'}
            label = 'aux'
            if agent:
                token, done, _ = classify(body)
                label = f'{token}:{"after-tool" if done else "first"}'
                if token and token.endswith('-JOIN'):
                    delta = ({'role': 'assistant', 'content': 'joined'} if done else
                             eval_call(f'join-{token[0]}', '(board:join "dev")'))
                elif token == 'B-WORK':
                    if done:
                        delta = {'role': 'assistant', 'content': 'done'}
                    else:
                        delta = eval_call('wait-B', '(progn (loop repeat 600 until (probe-file "%s") '
                                          'do (sleep 0.05)) (if (probe-file "%s") :seen :timeout))'
                                          % (flag, flag), 60000)
                elif token == 'A-POST':
                    delta = ({'role': 'assistant', 'content': 'posted'} if done else
                             eval_call('post-A', '(let ((answer (board:post "%s"))) '
                                       '(with-open-file (o "%s" :direction :output :if-exists :supersede) '
                                       '(write-line "posted" o)) answer)' % (POST, flag)))
                elif token and token.startswith('WAKE'):
                    delta = {'role': 'assistant', 'content': 'on it'}
                elif token == 'C-AGAIN':
                    delta = {'role': 'assistant', 'content': 'ok'}
                with lock:
                    served.append((label, body))
                    (root / 'requests.json').write_text(
                        json.dumps([{'label': l, 'body': b} for l, b in served], indent=1))
                if label == 'B-WORK:first':
                    b_waiting.set()
            finish = 'tool_calls' if 'tool_calls' in delta else 'stop'
            self.send_response(200)
            self.send_header('Content-Type', 'text/event-stream')
            self.send_header('Connection', 'close')
            self.end_headers()
            for d, reason in ((delta, None), ({}, finish)):
                event = {'id': 'fixture', 'object': 'chat.completion.chunk', 'model': 'board-fixture',
                         'choices': [{'index': 0, 'delta': d, 'finish_reason': reason}]}
                self.wfile.write(('data: ' + json.dumps(event) + '\n\n').encode())
            self.wfile.write(b'data: [DONE]\n\n')
            self.wfile.flush()
            self.close_connection = True

    server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Fixture)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    config = {'update': {'mode': 'off'},
              'general': {'default_model': {'provider': 'fixture', 'model': 'board-fixture'}},
              'providers': {'fixture': {'sdk': 'openai-completions', 'api_key': 'fixture-only',
                                        'base_url': f'http://127.0.0.1:{server.server_port}/v1'}}}
    (root / 'nc/config.jsonc').write_text(json.dumps(config))
    env = {'PATH': os.environ['PATH'], 'LANG': 'C.UTF-8', 'TERM': 'xterm-256color',
           'HOME': str(root / 'home'), 'NODECODE_HOME': str(root / 'nc'),
           'XDG_CACHE_HOME': str(Path(args.cache).resolve()), 'TMPDIR': str(root / 'tmp')}

    class Shell:
        def __init__(self, session):
            self.session = session
            self.frames = queue.Queue()
            self.kinds = []
            self.stderr = (root / f'{session}.stderr.log').open('w')
            self.process = subprocess.Popen([str(binary), 'rpc', session], cwd=root / 'workspace', env=env,
                                            stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                            stderr=self.stderr, text=True, start_new_session=True)
            threading.Thread(target=self.read, daemon=True).start()

        def read(self):
            with (root / f'{self.session}.frames.jsonl').open('w') as output:
                for line in self.process.stdout:
                    output.write(line)
                    output.flush()
                    try:
                        frame = json.loads(line)
                    except json.JSONDecodeError:
                        continue
                    payload = frame.get('payload', {})
                    if payload.get('type') == 'session_stream_event':
                        event = payload.get('frame', {}).get('event', {})
                        kind = event.get('type') or event.get('kind')
                        self.kinds.append(kind)
                        self.frames.put(kind)

        def send(self, prompt):
            self.process.stdin.write(json.dumps({'type': 'start_turn', 'request': {'prompt': prompt}}) + '\n')
            self.process.stdin.flush()

        def completed(self, seconds=120):
            deadline = time.monotonic() + seconds
            while time.monotonic() < deadline:
                try:
                    kind = self.frames.get(timeout=max(0.1, deadline - time.monotonic()))
                except queue.Empty:
                    break
                if kind == 'turn.completed':
                    return
                if kind in ('turn.failed', 'turn.cancelled'):
                    raise AssertionError(f'{self.session}: {kind}')
            raise TimeoutError(f'{self.session}: no turn.completed; see its frames and stderr')

        def close(self):
            try:
                self.process.stdin.close()
                self.process.wait(timeout=30)
            except Exception:
                self.process.terminate()
            self.stderr.close()

    def labelled(prefix):
        with lock:
            return [b for l, b in served if l.startswith(prefix)]

    shells = {}
    try:
        a = shells['A'] = Shell('s-BOARDA')
        # The organism starts its peripherals on a thread behind the listener,
        # after a shell's replay or three seconds: give it that before a turn.
        time.sleep(10)
        a.send('A-JOIN please')
        a.completed(300)
        for key, session in (('B', 's-BOARDB'), ('C', 's-BOARDC'), ('D', 's-BOARDD')):
            shell = shells[key] = Shell(session)
            shell.send(f'{key}-JOIN please')
            shell.completed()
        c_turns_before = shells['C'].kinds.count('turn.started')
        d_turns_before = shells['D'].kinds.count('turn.started')
        shells['B'].send('B-WORK now')
        assert b_waiting.wait(60), 'B never reached its waiting tool'
        a.send('A-POST now')
        a.completed()
        shells['B'].completed()
        shells['D'].completed()            # the woken turn
        time.sleep(2)
        c_turns_idle = shells['C'].kinds.count('turn.started')
        shells['C'].send('C-AGAIN please')
        shells['C'].completed()

        # every join took: its answer is the board
        joins = [[content_text(m) for m in b['messages'] if m.get('role') == 'tool'][-1]
                 for l, b in served if l.endswith('-JOIN:after-tool')]
        assert len(joins) == 4 and all('\nboard dev: ' in t for t in joins), joins
        # B: its round after the tool carries the post, after an unchanged prefix
        b_first, b_after = labelled('B-WORK:first'), labelled('B-WORK:after-tool')
        assert len(b_first) == 1 and len(b_after) == 1, (len(b_first), len(b_after))
        before, after = b_first[0]['messages'], b_after[0]['messages']
        assert after[:len(before)] == before, 'the round after the tool changed what came before'
        reminders = [i for i, m in enumerate(after) if '[board dev:' in content_text(m)]
        assert len(reminders) == 1, reminders
        tool_at = max(i for i, m in enumerate(after) if m.get('role') == 'tool')
        assert reminders[0] > tool_at, 'the reminder stands after the tool result'
        b_reminder = after[reminders[0]]
        assert 's-BOARDA: ' + POST in content_text(b_reminder)
        assert not any('[board dev:' in content_text(m) for m in before), 'nothing before the post'
        # D: woken, its first request carries the post
        wakes = labelled('WAKE-D')
        assert len(wakes) == 1, [l for l, _ in served]
        assert shells['D'].kinds.count('turn.started') == d_turns_before + 1
        assert any('s-BOARDA mentioned you' in content_text(m) for m in wakes[0]['messages'])
        assert any('s-BOARDA: ' + POST in content_text(m) for m in wakes[0]['messages'])
        # C: idle until prompted, then told
        assert c_turns_idle == c_turns_before, 'C ran a turn it was never given'
        again = labelled('C-AGAIN')
        assert len(again) == 1
        assert any('s-BOARDA: ' + POST in content_text(m) for m in again[0]['messages'])
        # A: never told its own post
        a_requests = [b for l, b in served if l.startswith('A-')]
        assert a_requests and not any('[board dev:' in content_text(m) for b in a_requests for m in b['messages'])
        # the post's answer to A named the wake
        post_results = [content_text(m) for b in labelled('A-POST:after-tool') for m in b['messages']
                        if m.get('role') == 'tool']
        assert any('woke s-BOARDD' in t for t in post_results), post_results
        # the file
        files = list((root / 'nc/boards').glob('*/dev.txt'))
        assert len(files) == 1 and ('s-BOARDA: ' + POST) in files[0].read_text(), files
        # every process on the built binary or its warm image
        pids = [a.process.pid]
        for pid in pids:
            children = Path(f'/proc/{pid}/task/{pid}/children')
            if children.exists():
                pids.extend(int(p) for p in children.read_text().split())
        executables = {str(pid): os.readlink(f'/proc/{pid}/exe') for pid in pids if Path(f'/proc/{pid}/exe').exists()}
        nodes = [exe for exe in executables.values() if 'nodecode' in exe]
        assert len(nodes) >= 2, executables
        assert all('(deleted)' not in exe and (exe == str(binary) or '/nodecode/warm/' in exe) for exe in nodes), nodes
        result = {'passed': True, 'requests': [l for l, _ in served], 'executables': executables,
                  'reminder_role': b_reminder.get('role'), 'reminder': content_text(b_reminder),
                  'board_file': str(files[0]), 'board_file_text': files[0].read_text(),
                  'checks': ['a post lands in a running turn at its next round, after the tool result',
                             'the round before is an unchanged prefix of the round after',
                             'an idle, @mentioned member is woken and its first request carries the post',
                             'an idle member not named runs nothing, and its next turn is told',
                             'the author is never told its own post',
                             "the post's answer names the wake",
                             'the board file has the line',
                             'shell and organism on the built binary']}
        (root / 'result.json').write_text(json.dumps(result, indent=2))
        print(json.dumps(result, indent=2))
    finally:
        for key in ('D', 'C', 'B', 'A'):
            if key in shells:
                shells[key].close()
        server.shutdown()


if __name__ == '__main__':
    main()
