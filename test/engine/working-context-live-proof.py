#!/usr/bin/env python3
"""Prove a released binary via RPC + a loopback-only OpenAI fixture.
No real HOME, credentials, cells or provider is used. Artifacts stay in --root.
"""
import argparse
import http.server
import json
import os
from pathlib import Path
import queue
import re
import subprocess
import threading
import time


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--binary', required=True)
    parser.add_argument('--root', required=True)
    parser.add_argument('--pristine', action='store_true')
    args = parser.parse_args()
    binary = Path(args.binary).resolve()
    root = Path(args.root).resolve()
    root.mkdir(parents=True, exist_ok=False)
    for name in ('home', 'nc', 'cache', 'tmp', 'workspace'):
        (root / name).mkdir()
    requests = []
    task_checks = '''(let ((task-symbol (find-symbol "TASK" :nodecode.evolved)))
    (assert (not (eq :external (nth-value 1 (find-symbol "TASK" :nodecode.evolved)))))
    (assert (not (and task-symbol (fboundp task-symbol))))
    (assert (not (assoc :task nle::*help-topics*)))
    (assert (not (find "TASK" nle::*help-verbs* :key #'symbol-name :test #'equal)))
    (assert (not (search "  :task -" (help))))
    (assert (not (loop for kind being the hash-keys of nlk::*owned-kind-owners*
                       thereis (uiop:string-prefix-p "task." kind))))
    (assert (notany (lambda (kind) (uiop:string-prefix-p "task." kind))
                    nlk::*replay-suppressed-kinds*)))'''
    # Round 1: append a note, keeping every block.
    append_code = ''';; Append one note to the context file; keep every block.
(let* ((path (context))
       (old (uiop:read-file-string path :external-format :utf-8))
       (first-line (subseq old 0 (position #\\Newline old))))
  ''' + task_checks + '''
  (assert (eql 0 (search "[[context " first-line)))
  (assert (search "context 4.3k" first-line))
  (assert (search "[[m:" old))
  (assert (search "ORIGINAL RPC HISTORY" old))
  (assert (search "OLD RPC ANSWER" old))
  (assert (not (search "PROTECTED RPC INPUT" old)))
  (assert (not (search "During substantial tool work" old)))
  (write-file path (format nil "~aLIVE WORKING NOTE~%" old)))'''
    # Round 2: read the file and keep the read; round 3 edits what this read.
    read_code = ''';; Read the context file now; the next round edits this read.
(progn (defparameter cl-user::*context-path* (context))
       (defparameter cl-user::*context-read*
         (uiop:read-file-string cl-user::*context-path* :external-format :utf-8))
       (assert (search "LIVE WORKING NOTE" cl-user::*context-read*))
       :read)'''
    # Round 3: remove the old answer's block from that earlier read.
    write_code = ''';; Remove the old answer's block from the earlier read and write it back.
(let* ((text cl-user::*context-read*)
       (at (search "OLD RPC ANSWER" text))
       (start (search "[[m:" text :from-end t :end2 at))
       (end (search "[[m:" text :start2 at)))
  (write-file cl-user::*context-path*
              (concatenate 'string (subseq text 0 start) (if end (subseq text end) ""))))'''
    calls = {2: ('edit-append', append_code), 3: ('read-later', read_code), 4: ('write-stale', write_code)}

    class Fixture(http.server.BaseHTTPRequestHandler):
        def log_message(self, *_):
            pass

        def do_GET(self):
            data = json.dumps({'data': [{'id': 'context-fixture'}]}).encode()
            self.send_response(200)
            self.send_header('Content-Type', 'application/json')
            self.send_header('Content-Length', str(len(data)))
            self.end_headers()
            self.wfile.write(data)

        def do_POST(self):
            body = json.loads(self.rfile.read(int(self.headers['Content-Length'])))
            # Auxiliary naming requests are not agent rounds.
            agent = any(t.get('function', {}).get('name') == 'eval' for t in body.get('tools', []))
            if agent:
                requests.append(body)
                (root / 'requests.json').write_text(json.dumps(requests, indent=2))
            index = len(requests) if agent else 0
            finish = 'stop'
            delta = {'role': 'assistant', 'content': 'Fixture Session'}
            if index == 1:
                delta['content'] = 'OLD RPC ANSWER'
            elif index in calls:
                call_id, form = calls[index]
                delta = {'role': 'assistant', 'tool_calls': [{
                    'index': 0, 'id': call_id, 'type': 'function',
                    'function': {'name': 'eval', 'arguments': json.dumps({'form': form})}}]}
                finish = 'tool_calls'
            elif index >= 5:
                delta['content'] = 'workspace proof ok'
            self.send_response(200)
            self.send_header('Content-Type', 'text/event-stream')
            self.send_header('Connection', 'close')
            self.end_headers()
            for d, reason in ((delta, None), ({}, finish)):
                event = {'id': f'fixture-{index}', 'object': 'chat.completion.chunk',
                         'model': 'context-fixture',
                         'choices': [{'index': 0, 'delta': d, 'finish_reason': reason}]}
                self.wfile.write(('data: ' + json.dumps(event) + '\n\n').encode())
            # What the prompt filled, as a provider reports it: the file's size line reads it.
            usage = {'id': f'fixture-{index}', 'object': 'chat.completion.chunk', 'model': 'context-fixture',
                     'choices': [], 'usage': {'prompt_tokens': 4321, 'completion_tokens': 7, 'total_tokens': 4328}}
            self.wfile.write(('data: ' + json.dumps(usage) + '\n\n').encode())
            self.wfile.write(b'data: [DONE]\n\n')
            self.wfile.flush()
            self.close_connection = True

    server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Fixture)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    config = {'update': {'mode': 'off'},
              'general': {'default_model': {'provider': 'fixture', 'model': 'context-fixture'}},
              'providers': {'fixture': {'sdk': 'openai-completions', 'api_key': 'fixture-only',
                                        'base_url': f'http://127.0.0.1:{server.server_port}/v1'}}}
    (root / 'nc/config.jsonc').write_text(json.dumps(config))
    env = {'PATH': os.environ['PATH'], 'LANG': 'C.UTF-8', 'TERM': 'xterm-256color',
           'HOME': str(root / 'home'), 'NODECODE_HOME': str(root / 'nc'),
           'XDG_CACHE_HOME': str(root / 'cache'), 'TMPDIR': str(root / 'tmp')}
    frames = queue.Queue()
    stderr = (root / 'stderr.log').open('w')
    command = [str(binary)] + (['--pristine'] if args.pristine else []) + ['--ephemeral', 'rpc']
    process = subprocess.Popen(command, cwd=root / 'workspace',
                               env=env, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                               stderr=stderr, text=True, start_new_session=True)
    def read_frames():
        with (root / 'frames.jsonl').open('w') as output:
            for line in process.stdout:
                output.write(line)
                output.flush()
                try:
                    frames.put(json.loads(line))
                except json.JSONDecodeError:
                    pass
    threading.Thread(target=read_frames, daemon=True).start()

    def send(prompt):
        process.stdin.write(json.dumps({'type': 'start_turn', 'request': {'prompt': prompt}}) + '\n')
        process.stdin.flush()

    def completed():
        deadline = time.monotonic() + 120
        while time.monotonic() < deadline:
            frame = frames.get(timeout=max(0.1, deadline-time.monotonic()))
            payload = frame.get('payload', {})
            if payload.get('type') == 'session_stream_event':
                event = payload.get('frame', {}).get('event', {})
                kind = event.get('type') or event.get('kind')
                if kind == 'turn.completed':
                    return
                if kind in ('turn.failed', 'turn.cancelled'):
                    raise AssertionError(frame)
        raise TimeoutError('no turn.completed; inspect frames.jsonl and stderr.log')

    try:
        send('ORIGINAL RPC HISTORY')
        completed()
        send('PROTECTED RPC INPUT')
        completed()
        assert len(requests) == 5, len(requests)
        rule = None
        for request in requests:
            text = '\n'.join(str(m.get('content', '')) for m in request['messages'])
            assert '<harness key="task"' not in text, 'retired task harness on HTTP request'
            assert 'No task started yet.' not in text
            blocks = re.findall(r'<harness key="context">\n(.*?)\n</harness>', text, re.S)
            assert len(blocks) == 1, blocks
            rule = rule or blocks[0]
            assert blocks[0] == rule
            assert text.count(rule) == 1
        assert 'During substantial tool work' in rule and 'at useful checkpoints' in rule
        assert 'before your final answer' in rule and 'not after every tool call' in rule
        assert len(rule) <= 650
        assert not (root / 'nc/cells/nodecode-context').exists(), 'proof must not rely on an installed cell'
        # An appended note: every message the round before sent is sent again byte for byte.
        before, after = requests[1]['messages'], requests[2]['messages']
        assert after[:len(before)] == before, 'appending a note changed the messages before it'
        note = after[len(before)]
        assert note.get('role') == 'assistant' and str(note.get('content')).startswith('[context note]\nLIVE WORKING NOTE'), note
        # An edit written a round after its read: the read's rounds follow it, the removed block is gone.
        final = requests[-1]['messages']
        wire = json.dumps(final)
        # The eval forms name these texts too, so messages are read by role and content.
        assert not any(m.get('role') == 'assistant' and m.get('content') == 'OLD RPC ANSWER' for m in final), \
            'a removed block reached the request'
        assert any(m.get('role') == 'user' and 'ORIGINAL RPC HISTORY' in str(m.get('content')) for m in final)
        assert any(m.get('role') == 'assistant' and str(m.get('content')).startswith('[context note]\nLIVE WORKING NOTE')
                   for m in final)
        # The provider may prepend standing harness blocks to the first user message.
        assert sum(str(m.get('content', '')).splitlines().count('PROTECTED RPC INPUT')
                   for m in final if m.get('role') == 'user') == 1
        for call_id in ('edit-append', 'read-later', 'write-stale'):
            assert sum(m.get('role') == 'tool' and m.get('tool_call_id') == call_id for m in final) == 1, call_id
        assert wire.count('context edit accepted') == 2, wire.count('context edit accepted')
        # The operator's row rides the stream as the durable fact it reads.
        edits = []
        for line in (root / 'frames.jsonl').read_text().splitlines():
            try:
                frame = json.loads(line)
            except json.JSONDecodeError:
                continue
            event = frame.get('payload', {}).get('frame', {}).get('event', {})
            if (event.get('type') or event.get('kind')) == 'context.edited':
                edits.append(event.get('payload', {}))
        assert len(edits) == 2, edits
        assert edits[0].get('notes') == 1 and edits[0].get('removed') == 0, edits[0]
        assert edits[1].get('removed') == 1, edits[1]
        pids = [process.pid]
        for pid in pids:
            children = Path(f'/proc/{pid}/task/{pid}/children')
            if children.exists():
                pids.extend(int(p) for p in children.read_text().split())
        executables = {str(pid): os.readlink(f'/proc/{pid}/exe') for pid in pids if Path(f'/proc/{pid}/exe').exists()}
        nodes = [exe for exe in executables.values() if 'nodecode' in exe]
        assert len(nodes) >= 2, executables
        assert all('(deleted)' not in exe and (exe == str(binary) or '/nodecode/warm/' in exe) for exe in nodes), nodes
        result = {'passed': True, 'provider_requests': len(requests), 'executables': executables,
                  'pristine': args.pristine, 'context_skill': rule, 'skill_words': len(rule.split()),
                  'checks': ['RPC handshake and turn completion',
                             'no task API, export, help, ownership or replay policy',
                             'no task harness on any serialized HTTP request',
                             'headed blocks and the size line in context.txt',
                             'an appended note leaves every earlier message byte-identical on the wire',
                             'an edit written a round after its read is taken, newer rounds follow it',
                             'a removed block leaves the serialized request',
                             'protected input exactly once', 'each native tool result exactly once',
                             'model-authored note in the assistant role',
                             'context.edited facts on the RPC stream with their counts',
                             'shell and gateway on required binary',
                             'full built-in skill on every request without an installed cell']}
        (root / 'result.json').write_text(json.dumps(result, indent=2))
        print(json.dumps(result, indent=2))
    finally:
        process.stdin.close()
        try:
            process.wait(timeout=20)
        except subprocess.TimeoutExpired:
            process.terminate()
            process.wait(timeout=10)
        server.shutdown()
        stderr.close()


if __name__ == '__main__':
    main()
