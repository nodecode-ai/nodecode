#!/usr/bin/env python3
# fake-apis.py --- stand-ins for Discord and an OpenAI-compatible provider, so
# a built nodecode answers a Discord message end to end with no network.
#
# SPDX-License-Identifier: MIT
#
# One process, standard library only, two ports:
#   --http-port  Discord's REST API under /api/v10, and an OpenAI-compatible
#                chat endpoint under /v1 (streamed or not)
#   --ws-port    Discord's gateway: HELLO, READY after IDENTIFY, then one
#                MESSAGE_CREATE (--message, from user 42 in channel 123 of
#                guild g1), heartbeats acknowledged
# Every request is one JSON line in --log (kind: identify, rest, provider), so
# the driver asserts on what nodecode sent, never on what it printed.
# --tool-call-without-id makes the first streamed completion a tool call that
# carries no id (issue #32's shape); the next request must carry an id on it.

import argparse
import base64
import hashlib
import itertools
import json
import socket
import socketserver
import struct
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

GUID = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'
BOT_ID = '999'
USER_ID = '42'
CHANNEL_ID = '123'
GUILD_ID = 'g1'
ASK_ID = '1001'

args = None
log_lock = threading.Lock()
ids = itertools.count(5000)
sent_message = threading.Event()
provider_calls = itertools.count(1)


def record(kind, **fields):
    fields['kind'] = kind
    fields['at'] = round(time.time(), 3)
    with log_lock, open(args.log, 'a', encoding='utf-8') as out:
        out.write(json.dumps(fields) + '\n')


# --- the gateway: just enough RFC 6455 for one JSON text conversation -------

def recv_exact(sock, n):
    buf = b''
    while len(buf) < n:
        chunk = sock.recv(n - len(buf))
        if not chunk:
            raise ConnectionError('closed')
        buf += chunk
    return buf


def ws_recv(sock):
    """(opcode, payload) of one message; a fragmented text is joined."""
    first, message = None, b''
    while True:
        b1, b2 = recv_exact(sock, 2)
        fin, op, n = b1 & 0x80, b1 & 0x0F, b2 & 0x7F
        if n == 126:
            n = struct.unpack('!H', recv_exact(sock, 2))[0]
        elif n == 127:
            n = struct.unpack('!Q', recv_exact(sock, 8))[0]
        mask = recv_exact(sock, 4) if b2 & 0x80 else b'\0\0\0\0'
        data = bytes(c ^ mask[i % 4] for i, c in enumerate(recv_exact(sock, n)))
        if op >= 8:
            return op, data
        first = op if first is None else first
        message += data
        if fin:
            return first, message


def ws_send(sock, op, data):
    n = len(data)
    if n < 126:
        head = struct.pack('!BB', 0x80 | op, n)
    elif n < 65536:
        head = struct.pack('!BBH', 0x80 | op, 126, n)
    else:
        head = struct.pack('!BBQ', 0x80 | op, 127, n)
    sock.sendall(head + data)


def ws_json(sock, obj):
    ws_send(sock, 1, json.dumps(obj).encode())


class Gateway(socketserver.BaseRequestHandler):
    asked = threading.Lock()  # the ask goes out once, on the first IDENTIFY

    def handle(self):
        sock = self.request
        head = b''
        while b'\r\n\r\n' not in head:
            chunk = sock.recv(4096)
            if not chunk:
                return
            head += chunk
        lines = head.split(b'\r\n\r\n')[0].decode('latin-1').split('\r\n')
        headers = {k.strip().lower(): v.strip() for k, _, v in (l.partition(':') for l in lines[1:])}
        key = headers.get('sec-websocket-key', '')
        accept = base64.b64encode(hashlib.sha1((key + GUID).encode()).digest()).decode()
        sock.sendall(('HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\n'
                      'Connection: Upgrade\r\nSec-WebSocket-Accept: %s\r\n\r\n' % accept).encode())
        record('connect', path=lines[0].split(' ')[1] if ' ' in lines[0] else lines[0])
        ws_json(sock, {'op': 10, 'd': {'heartbeat_interval': 41250}})
        seq = itertools.count(1)
        try:
            while True:
                op, data = ws_recv(sock)
                if op == 8:
                    ws_send(sock, 8, data[:2])
                    return
                if op == 9:
                    ws_send(sock, 10, data)
                    continue
                if op != 1:
                    continue
                payload = json.loads(data)
                if payload.get('op') == 1:
                    ws_json(sock, {'op': 11})
                elif payload.get('op') in (2, 6):
                    d = payload.get('d') or {}
                    record('identify', op=payload['op'], token=d.get('token'), intents=d.get('intents'))
                    ws_json(sock, {'op': 0, 't': 'READY', 's': next(seq), 'd': {
                        'v': 10, 'session_id': 'e2e-session',
                        'resume_gateway_url': 'ws://127.0.0.1:%d/' % args.ws_port,
                        'user': {'id': BOT_ID, 'username': 'e2e-bot', 'bot': True},
                        'application': {'id': 'app1', 'flags': 0},
                        'guilds': [{'id': GUILD_ID, 'unavailable': True}]}})
                    if Gateway.asked.acquire(blocking=False):
                        threading.Timer(args.ask_delay, self.ask, (sock, next(seq))).start()
        except (ConnectionError, OSError, ValueError):
            return

    def ask(self, sock, s):
        try:
            ws_json(sock, {'op': 0, 't': 'MESSAGE_CREATE', 's': s, 'd': {
                'id': ASK_ID, 'channel_id': CHANNEL_ID, 'guild_id': GUILD_ID, 'type': 0,
                'content': args.message, 'mentions': [], 'attachments': [], 'embeds': [],
                'timestamp': time.strftime('%Y-%m-%dT%H:%M:%S+00:00', time.gmtime()),
                'author': {'id': USER_ID, 'username': 'e2e-user'},
                'member': {'roles': [], 'nick': None}}})
            record('asked', content=args.message)
        except OSError as error:
            record('ask-failed', error=str(error))


# --- REST and the provider ---------------------------------------------------

class Http(BaseHTTPRequestHandler):
    protocol_version = 'HTTP/1.1'

    def log_message(self, *_):
        pass

    def body(self):
        n = int(self.headers.get('Content-Length') or 0)
        raw = self.rfile.read(n) if n else b''
        kind = self.headers.get('Content-Type') or ''
        if 'json' in kind and raw:
            try:
                return json.loads(raw)
            except ValueError:
                return raw.decode('utf-8', 'replace')
        return raw.decode('utf-8', 'replace') if raw else None

    def reply(self, status, obj=None):
        data = b'' if obj is None else json.dumps(obj).encode()
        self.send_response(status)
        if obj is not None:
            self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_GET(self):
        self.route('GET')

    def do_POST(self):
        self.route('POST')

    def do_PUT(self):
        self.route('PUT')

    def do_PATCH(self):
        self.route('PATCH')

    def do_DELETE(self):
        self.route('DELETE')

    def route(self, method):
        path = self.path.split('?')[0]
        body = self.body()
        if path.startswith('/v1/'):
            return self.provider(method, path, body)
        if path.startswith('/api/v10/'):
            return self.discord(method, path[len('/api/v10'):], body)
        record('unknown', method=method, path=self.path)
        self.reply(404, {'message': 'Unknown route'})

    def discord(self, method, path, body):
        record('rest', method=method, path=path, body=body,
               auth=(self.headers.get('Authorization') or '')[:4])
        parts = path.strip('/').split('/')
        message = lambda mid, channel, extra=None: dict(
            {'id': mid, 'channel_id': channel, 'type': 0, 'content': '',
             'author': {'id': BOT_ID, 'username': 'e2e-bot', 'bot': True},
             'timestamp': time.strftime('%Y-%m-%dT%H:%M:%S+00:00', time.gmtime())},
            **(extra or {}))
        if path == '/users/@me':
            return self.reply(200, {'id': BOT_ID, 'username': 'e2e-bot', 'bot': True})
        if path == '/users/@me/guilds':
            return self.reply(200, [{'id': GUILD_ID, 'name': 'e2e'}])
        if path == '/applications/@me':
            return self.reply(200, {'id': 'app1', 'name': 'e2e', 'flags': 0})
        if path == '/gateway/bot' or path == '/gateway':
            return self.reply(200, {'url': 'ws://127.0.0.1:%d' % args.ws_port, 'shards': 1})
        if method in ('PUT', 'DELETE'):
            return self.reply(200, []) if path.endswith('/commands') else self.reply(204)
        if parts[0] == 'channels' and len(parts) >= 2:
            channel = parts[1]
            if len(parts) == 2:
                return self.reply(200, {'id': channel, 'type': 0, 'guild_id': GUILD_ID, 'name': 'e2e'})
            if parts[2] == 'typing':
                return self.reply(204)
            if parts[2] == 'messages' and len(parts) == 3:
                if method == 'POST':
                    content = body.get('content', '') if isinstance(body, dict) else ''
                    if args.reply in content:
                        sent_message.set()
                    return self.reply(200, message(str(next(ids)), channel,
                                                   {'content': content}))
                return self.reply(200, [])
            if parts[2] == 'messages' and len(parts) >= 4:
                if len(parts) >= 5 and parts[4] == 'threads':
                    return self.reply(201, {'id': str(next(ids)), 'type': 11, 'parent_id': channel,
                                            'guild_id': GUILD_ID, 'name': 'e2e'})
                if method == 'PATCH':
                    content = body.get('content', '') if isinstance(body, dict) else ''
                    if args.reply in content:
                        sent_message.set()
                    return self.reply(200, message(parts[3], channel, {'content': content}))
                return self.reply(200, message(parts[3], channel))
        if parts[0] == 'guilds' and len(parts) >= 3 and parts[2] == 'channels':
            return self.reply(200, [{'id': CHANNEL_ID, 'type': 0, 'guild_id': GUILD_ID, 'name': 'e2e'}])
        if parts[0] == 'applications' and parts[-1] == 'emojis':
            return self.reply(200, {'items': []})
        return self.reply(200, {})

    def provider(self, method, path, body):
        if path == '/v1/models':
            record('provider', method=method, path=path)
            return self.reply(200, {'object': 'list', 'data': [{'id': args.model, 'object': 'model'}]})
        if path != '/v1/chat/completions' or not isinstance(body, dict):
            record('provider', method=method, path=path)
            return self.reply(404, {'error': {'message': 'unknown route'}})
        n = next(provider_calls)
        messages = body.get('messages') or []
        text = json.dumps(messages)
        empty_ids = sum(1 for m in messages for c in (m.get('tool_calls') or []) if not c.get('id')) \
            + sum(1 for m in messages if m.get('role') == 'tool' and not m.get('tool_call_id'))
        tool_names = [t.get('function', {}).get('name') for t in body.get('tools') or []]
        record('provider', method=method, path=path, n=n, model=body.get('model'),
               stream=bool(body.get('stream')), messages=len(messages),
               saw_message=args.message in text, empty_tool_ids=empty_ids,
               tool_results=sum(1 for m in messages if m.get('role') == 'tool'),
               tools=len(tool_names))
        if empty_ids:
            # what a strict endpoint answers (issue #32): the whole history refused
            return self.reply(400, {'error': {'message': 'Invalid tool_call_id: empty', 'type': 'invalid_request_error'}})
        call_tool = (args.tool_call_without_id and args.tool_name in tool_names
                     and not any(m.get('role') == 'tool' for m in messages)
                     and args.message in text)
        if not body.get('stream'):
            return self.reply(200, {
                'id': 'chatcmpl-e2e-%d' % n, 'object': 'chat.completion', 'model': args.model,
                'choices': [{'index': 0, 'finish_reason': 'stop',
                             'message': {'role': 'assistant', 'content': args.reply}}],
                'usage': {'prompt_tokens': 10, 'completion_tokens': 3, 'total_tokens': 13}})
        self.send_response(200)
        self.send_header('Content-Type', 'text/event-stream')
        self.send_header('Cache-Control', 'no-cache')
        self.send_header('Connection', 'close')
        self.end_headers()
        base = {'id': 'chatcmpl-e2e-%d' % n, 'object': 'chat.completion.chunk',
                'created': int(time.time()), 'model': args.model}

        def chunk(delta, finish=None):
            obj = dict(base, choices=[{'index': 0, 'delta': delta, 'finish_reason': finish}])
            self.wfile.write(b'data: ' + json.dumps(obj).encode() + b'\n\n')
            self.wfile.flush()
        chunk({'role': 'assistant', 'content': ''})
        if call_tool:
            # no "id" on the call: the provider shape issue #32 names
            chunk({'tool_calls': [{'index': 0, 'type': 'function',
                                   'function': {'name': args.tool_name, 'arguments': ''}}]})
            chunk({'tool_calls': [{'index': 0, 'function': {'arguments': args.tool_arguments}}]})
            chunk({}, 'tool_calls')
        else:
            for piece in (args.reply[:len(args.reply) // 2], args.reply[len(args.reply) // 2:]):
                chunk({'content': piece})
            chunk({}, 'stop')
        usage = dict(base, choices=[], usage={'prompt_tokens': 10, 'completion_tokens': 3, 'total_tokens': 13})
        self.wfile.write(b'data: ' + json.dumps(usage).encode() + b'\n\ndata: [DONE]\n\n')
        self.wfile.flush()
        self.close_connection = True


class ThreadingTCP(socketserver.ThreadingTCPServer):
    daemon_threads = True
    allow_reuse_address = True


def main():
    global args
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--http-port', type=int, required=True)
    parser.add_argument('--ws-port', type=int, required=True)
    parser.add_argument('--log', required=True)
    parser.add_argument('--message', default='ping e2e')
    parser.add_argument('--reply', default='e2e-pong')
    parser.add_argument('--model', default='fake-model')
    parser.add_argument('--ask-delay', type=float, default=2.0)
    parser.add_argument('--tool-call-without-id', action='store_true')
    parser.add_argument('--tool-name', default='eval')
    parser.add_argument('--tool-arguments', default='{"form":"(+ 1 2)"}')
    args = parser.parse_args()
    http = ThreadingHTTPServer(('127.0.0.1', args.http_port), Http)
    http.daemon_threads = True
    gateway = ThreadingTCP(('127.0.0.1', args.ws_port), Gateway)
    threading.Thread(target=gateway.serve_forever, daemon=True).start()
    record('ready', http_port=args.http_port, ws_port=args.ws_port)
    http.serve_forever()


if __name__ == '__main__':
    main()
