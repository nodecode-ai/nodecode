#!/usr/bin/env python3
"""body-relay.py -- take request bodies as deltas in front of a model server.

Usage: body-relay.py [--listen HOST:PORT] [--upstream HOST:PORT ...] [--cache-mb N] [--heartbeat S] [--xargs JSON] [--log PATH]

A conversation's next request repeats the one before it but for what the
round added, and over a slow, distant link the repeat is most of the wait.
This relay sits beside the server and keeps the bodies it forwarded, by SHA-256;
a client then sends a request as a delta against one of them (Nodecode's
request_bodies encoding, store/bodies.lisp ENCODE-DELTA), and the relay rebuilds
the body, checks it, and forwards it.

  X-Body-Delta: <sha256 hex of the base body> | empty
  X-Body-Sha256: <sha256 hex of the body the delta rebuilds>

A request without X-Body-Delta passes through as it came. While a streamed
answer waits on its first byte (queued behind other requests) the relay writes
it an SSE comment every --heartbeat seconds, so a client's idle cut reads a
queue as a live stream; a client that hangs up has its request closed at the
server at once. --xargs merges server switches into every chat request's
vllm_xargs, on the forwarded copy only (e.g. {"ee_nohit": 1} keeps
the server's exact-end hits off). Each request's log line names its
body's SHA-256 (Nodecode's request_bodies key) and the server's response id. Every response
carries `X-Body-Delta: 1', which is how a client learns it may send deltas.
Given several --upstream servers (one per GPU), a conversation stays
on one of them, since each holds its own prefix cache: a delta goes where its
base went, a new conversation (keyed by its first two messages) to the server
with the fewest requests in flight, and a server that refuses the connection
hands the request to the next.
A base the relay does not hold answers 412; a delta that does not rebuild to
its SHA-256 answers 422. Responses stream back chunked on a kept-alive
connection, so a client's pooled socket keeps its window open."""
import argparse, collections, hashlib, http.client, http.server, json, select, socket, socketserver, sys, threading, time

HOP = {"connection", "keep-alive", "proxy-connection", "transfer-encoding", "te", "trailer",
       "upgrade", "content-length", "host", "x-body-delta", "x-body-sha256", "expect"}


class Bodies:
    """The bodies forwarded, newest kept, up to a byte budget."""

    def __init__(self, budget):
        self.budget, self.size = budget, 0
        self.items = collections.OrderedDict()
        self.lock = threading.Lock()

    def get(self, sha):
        with self.lock:
            body = self.items.get(sha)
            if body is not None:
                self.items.move_to_end(sha)
            return body

    def put(self, sha, body):
        with self.lock:
            if sha in self.items:
                self.items.move_to_end(sha)
                return
            self.items[sha] = body
            self.size += len(body)
            while self.size > self.budget and len(self.items) > 1:
                _, old = self.items.popitem(last=False)
                self.size -= len(old)


def apply_delta(delta, base):
    """The body DELTA makes of BASE: ops opening with a varint N, an even N
    inserting the N/2 bytes after it, an odd N copying (N-1)/2 bytes from the
    offset in the varint after it, in BASE followed by the body so far."""
    out = bytearray()
    i, n, blen = 0, len(delta), len(base)

    def varint():
        nonlocal i
        value = shift = 0
        while True:
            byte = delta[i]
            i += 1
            value |= (byte & 127) << shift
            if byte < 128:
                return value
            shift += 7

    while i < n:
        op = varint()
        length = op >> 1
        if op & 1 == 0:
            if i + length > n:
                raise ValueError("an insert of %d bytes runs past the delta" % length)
            out += delta[i:i + length]
            i += length
        else:
            frm = varint()
            if frm + length <= blen:
                out += base[frm:frm + length]
            elif blen <= frm and frm - blen + length <= len(out):
                start = frm - blen
                out += out[start:start + length]
            else:
                raise ValueError("a copy of %d bytes from %d is out of bounds" % (length, frm))
    return bytes(out)


class Router:
    """Which upstream serves a request: a conversation's, else the least busy."""

    def __init__(self, upstreams):
        self.upstreams = upstreams
        self.inflight = [0] * len(upstreams)
        self.by_sha = collections.OrderedDict()
        self.by_conv = collections.OrderedDict()
        self.lock = threading.Lock()

    @staticmethod
    def conversation(doc):
        messages = doc.get("messages") if isinstance(doc, dict) else None
        if not messages:
            return None
        return hashlib.sha256(json.dumps(messages[:2], sort_keys=True).encode()).hexdigest()

    def pick(self, base_sha, conv):
        """The upstream for a request, counted in flight from here: a conversation
        that arrives with its siblings at once must not see them all at zero."""
        with self.lock:
            for table, key in ((self.by_sha, base_sha), (self.by_conv, conv)):
                if key is not None and key in table:
                    table.move_to_end(key)
                    i = table[key]
                    break
            else:
                held = collections.Counter(self.by_conv.values())
                i = min(range(len(self.upstreams)), key=lambda j: (self.inflight[j], held[j], j))
                if conv is not None:
                    self.by_conv[conv] = i
            self.inflight[i] += 1
            return i

    def move(self, old, new):
        with self.lock:
            self.inflight[old] -= 1
            self.inflight[new] += 1

    def remember(self, i, sha, conv):
        with self.lock:
            for table, key in ((self.by_sha, sha), (self.by_conv, conv)):
                if key is not None:
                    table[key] = i
                    table.move_to_end(key)
                    while len(table) > 100000:
                        table.popitem(last=False)

    def enter(self, i, step):
        with self.lock:
            self.inflight[i] += step


class Relay(socketserver.ThreadingMixIn, http.server.HTTPServer):
    daemon_threads = True
    allow_reuse_address = True
    request_queue_size = 128


class Handler(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"
    server_version = "body-relay"

    def log_message(self, *args):
        pass

    def note(self, **fields):
        log = self.server.log
        if log:
            fields["t"] = round(time.time(), 3)
            with self.server.log_lock:
                log.write(json.dumps(fields) + "\n")
                log.flush()

    def read_body(self):
        if "chunked" in (self.headers.get("Transfer-Encoding") or "").lower():
            parts = []
            while True:
                size = int(self.rfile.readline().split(b";")[0].strip() or b"0", 16)
                if size == 0:
                    while self.rfile.readline().strip():
                        pass
                    return b"".join(parts)
                parts.append(self.rfile.read(size))
                self.rfile.readline()
        length = int(self.headers.get("Content-Length") or 0)
        return self.rfile.read(length) if length else b""

    def answer(self, status, payload):
        body = json.dumps(payload).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("X-Body-Delta", "1")
        self.end_headers()
        self.wfile.write(body)

    def watch(self, upstream, done, sse):
        """Until DONE: close UPSTREAM the moment the client hangs up, and while
        an event stream has sent nothing yet, write it an SSE comment every
        --heartbeat seconds. A request still queued at the server writes
        nothing back; without this, a client hanging up would never be noticed
        and the server would run a request nobody reads, and a client's idle
        cut would see a queue as a dead stream and re-send to its back."""
        client = self.connection
        beat = self.server.heartbeat
        quiet = 0.0
        while not done.wait(0.5):
            quiet += 0.5
            if sse and beat and quiet >= beat:
                quiet = 0.0
                with self.write_lock:
                    if self.waiting:
                        try:
                            self.wfile.write(b"b\r\n: waiting\n\n\r\n")
                            self.wfile.flush()
                        except OSError:
                            pass
            try:
                if not select.select([client], [], [], 0)[0]:
                    continue
                if client.recv(1, socket.MSG_PEEK):
                    return  # the client's next request: it is still there
            except OSError:
                pass
            self.abandoned = True
            try:
                upstream.sock.shutdown(socket.SHUT_RDWR)
            except (OSError, AttributeError):
                pass
            return

    def relay(self, method):
        t0 = time.time()
        body = self.read_body() if method in ("POST", "PUT", "PATCH") else None
        sent = len(body) if body is not None else 0
        base_sha = self.headers.get("X-Body-Delta")
        if base_sha is not None:
            want = (self.headers.get("X-Body-Sha256") or "").lower()
            base = b"" if base_sha == "empty" else self.server.bodies.get(base_sha.lower())
            if base is None:
                self.note(path=self.path, delta=sent, base="missing")
                return self.answer(412, {"error": {"type": "body_delta_base_missing",
                                                   "message": "this relay does not hold base " + base_sha}})
            try:
                body = apply_delta(body, base)
            except (IndexError, ValueError) as e:
                body = None
                error = "the delta does not parse: %s" % e
            else:
                error = None if hashlib.sha256(body).hexdigest() == want else \
                    "the delta rebuilt %d bytes that are not %s" % (len(body), want)
            if error:
                self.note(path=self.path, delta=sent, base=base_sha[:12], error=error)
                return self.answer(422, {"error": {"type": "body_delta_mismatch", "message": error}})
        sha = hashlib.sha256(body).hexdigest() if body else None
        if body:
            self.server.bodies.put(sha, body)
        chat = bool(body) and self.path.endswith("/chat/completions")
        doc = json.loads(body) if chat else None
        router = self.server.router
        conv = router.conversation(doc) if chat else None
        home = router.pick(base_sha.lower() if base_sha and base_sha != "empty" else None, conv) if chat else None
        if chat and self.server.xargs:
            # The forwarded copy only: the kept body, and so every delta, is the client's.
            doc["vllm_xargs"] = dict(doc.get("vllm_xargs") or {}, **self.server.xargs)
            body = json.dumps(doc).encode()
        headers = {k: v for k, v in self.headers.items() if k.lower() not in HOP}
        if base_sha is not None:
            headers["Content-Type"] = "application/json"
        if body is not None:
            headers["Content-Length"] = str(len(body))
        n = len(router.upstreams)
        if home is None:
            home = 0
            router.enter(0, 1)
        for k in range(n):
            up = (home + k) % n
            if k:
                router.move((home + k - 1) % n, up)
            upstream = http.client.HTTPConnection(*router.upstreams[up], timeout=900)
            try:
                upstream.request(method, self.path, body=body, headers=headers)
                response = upstream.getresponse()
                break
            except OSError as e:
                upstream.close()
                self.note(path=self.path, up=router.upstreams[up][1], error="upstream: %s" % e)
                if k == n - 1:
                    router.enter(up, -1)
                    return self.answer(502, {"error": {"type": "upstream_unreachable", "message": str(e)}})
        if chat:
            router.remember(up, sha, conv)
        self.send_response(response.status, response.reason)
        for k, v in response.getheaders():
            if k.lower() not in HOP:
                self.send_header(k, v)
        self.send_header("X-Body-Delta", "1")
        self.send_header("Transfer-Encoding", "chunked")
        self.end_headers()
        first = None
        rid = None
        out = 0
        self.abandoned = False
        self.waiting = True
        self.write_lock = threading.Lock()
        done = threading.Event()
        sse = "text/event-stream" in (response.getheader("Content-Type") or "")
        threading.Thread(target=self.watch, args=(upstream, done, sse), daemon=True).start()
        try:
            while True:
                data = response.read1(65536)
                if not data:
                    break
                if first is None:
                    first = time.time()
                    if b'"id":"' in data:
                        rid = data.split(b'"id":"', 1)[1].split(b'"', 1)[0].decode(errors="replace")
                out += len(data)
                with self.write_lock:
                    self.waiting = False
                    self.wfile.write(b"%x\r\n%s\r\n" % (len(data), data))
                    self.wfile.flush()
            with self.write_lock:
                self.waiting = False
                self.wfile.write(b"0\r\n\r\n")
                self.wfile.flush()
        except (OSError, http.client.HTTPException):
            # The client went away, or the server did mid-stream: closing
            # upstream is how the server learns to stop.
            self.close_connection = True
        finally:
            done.set()
            upstream.close()
            router.enter(up, -1)
        if method == "POST":
            self.note(path=self.path, up=router.upstreams[up][1], status=response.status, sent=sent,
                      abandoned=self.abandoned, rid=rid, sha=sha,
                      body=len(body) if body is not None else 0,
                      delta=base_sha[:12] if base_sha else None,
                      first=round(first - t0, 3) if first else None, total=round(time.time() - t0, 3), out=out)

    def do_GET(self):
        self.relay("GET")

    def do_POST(self):
        self.relay("POST")

    def do_DELETE(self):
        self.relay("DELETE")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--listen", default="0.0.0.0:8081")
    parser.add_argument("--upstream", action="append",
                        help="HOST:PORT of a model server; repeat for several (default 127.0.0.1:8080)")
    parser.add_argument("--cache-mb", type=int, default=2048)
    parser.add_argument("--heartbeat", type=float, default=15,
                        help="seconds between SSE comments to a stream still waiting on its first byte; 0 sends none")
    parser.add_argument("--xargs", type=json.loads, default={},
                        help="JSON merged into every chat request's vllm_xargs (the server's per-request switches)")
    parser.add_argument("--log")
    args = parser.parse_args()
    host, port = args.listen.rsplit(":", 1)
    server = Relay((host, int(port)), Handler)
    server.router = Router([(h, int(p)) for h, p in (u.rsplit(":", 1) for u in args.upstream or ["127.0.0.1:8080"])])
    server.bodies = Bodies(args.cache_mb * 1024 * 1024)
    server.log = open(args.log, "a") if args.log else None
    server.log_lock = threading.Lock()
    server.heartbeat = args.heartbeat
    server.xargs = args.xargs
    print("body-relay: %s -> %s, %d MB of bodies" % (
        args.listen, " ".join("%s:%d" % u for u in server.router.upstreams), args.cache_mb), flush=True)
    server.serve_forever()


if __name__ == "__main__":
    main()
