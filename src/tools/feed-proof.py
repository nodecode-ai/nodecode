#!/usr/bin/env python3
"""feed-proof.py -- a release feed gone silent is left, and the next label lands.

Usage: feed-proof.py BINARY [--budget SECONDS] [--keep]

BINARY is the nodecode executable under proof. It serves headless (`serve') in
a scratch home whose config points its release host at a TLS host played here
on localhost, under a certificate made for the run and trusted through
SSL_CERT_FILE. TLS on purpose: over TLS dexador's :read-timeout bounds nothing.

The host's feed answers its first connection with the build's own label and
then says nothing and never closes -- to the reader, a connection whose far
end is gone. The gateway must leave it at the feed's idle limit, connect
again, and act on the label the second connection names: a read of the
channel pointer after it. The pointer names the build's own label throughout,
so nothing is ever staged.

Exits 1 when the gateway holds the silent connection, leaves it sooner than a
healthy feed's ping could arrive, never returns, or never acts on the next
label -- printing what the host saw; 2 when the proof cannot run."""

import argparse
import json
import os
import re
import shutil
import signal
import socket
import ssl
import subprocess
import sys
import tempfile
import threading
import time

# The label the second connection names: never a release, never this build's.
NEXT = "feed-proof-next"
# The host pings every twenty seconds, so an idle limit under this would drop
# healthy feeds; the proof calls that a failure too.
EARLIEST_LEAVE = 25.0


def say(text):
    print(f"feed-proof: {text}", flush=True)


class Host:
    """The release host: /dl/stable-* answers the build's label; the feed's
    first connection goes silent, every later one names NEXT and pings."""

    def __init__(self, label, cert, key):
        self.label = label
        self.events = []
        self.lock = threading.Lock()
        self.feeds = 0
        self.done = threading.Event()
        self.context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        self.context.load_cert_chain(cert, key)
        # Both families: the organism dials localhost however it resolves.
        self.socket = socket.socket(socket.AF_INET6, socket.SOCK_STREAM)
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.setsockopt(socket.IPPROTO_IPV6, socket.IPV6_V6ONLY, 0)
        self.socket.bind(("::", 0))
        self.socket.listen(16)
        self.port = self.socket.getsockname()[1]
        self.start = time.monotonic()

    def note(self, kind, detail=""):
        with self.lock:
            self.events.append((time.monotonic() - self.start, kind, detail))

    def first(self, kind, detail=None, after=-1.0):
        with self.lock:
            for at, k, d in self.events:
                if k == kind and at > after and (detail is None or d == detail):
                    return at
        return None

    def serve(self):
        while not self.done.is_set():
            try:
                raw, _ = self.socket.accept()
            except OSError:
                return
            threading.Thread(target=self.answer, args=(raw,), daemon=True).start()

    def answer(self, raw):
        try:
            conn = self.context.wrap_socket(raw, server_side=True)
        except (ssl.SSLError, OSError) as error:
            self.note("tls-refused", str(error))
            raw.close()
            return
        try:
            head = b""
            while b"\r\n\r\n" not in head:
                chunk = conn.recv(4096)
                if not chunk:
                    return
                head += chunk
            path = head.split(b" ", 2)[1].decode("latin-1")
            if path.startswith("/api/release/feed"):
                self.feed(conn)
            elif path.startswith("/dl/stable-"):
                self.note("pointer-read", path.split("?")[0])
                body = (self.label + "\n").encode()
                conn.sendall(b"HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n"
                             b"Content-Length: %d\r\nConnection: close\r\n\r\n" % len(body) + body)
            else:
                self.note("other", path)
                conn.sendall(b"HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n")
        except OSError:
            pass
        finally:
            try:
                conn.close()
            except OSError:
                pass

    def feed(self, conn):
        with self.lock:
            self.feeds += 1
            n = str(self.feeds)
        self.note("feed-open", n)
        # The production route's shape: an event stream, chunked, never cached.
        conn.sendall(b"HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n"
                     b"Cache-Control: no-store\r\nTransfer-Encoding: chunked\r\n\r\n")

        def event(text):
            data = text.encode()
            conn.sendall(b"%x\r\n" % len(data) + data + b"\r\n")

        if n == "1":
            event(f"data: {self.label}\n\n")
            self.note("feed-silent", n)
            # Nothing more and never a close: only the reader can end it.
            try:
                while conn.recv(4096):
                    pass
            except OSError:
                pass
            self.note("feed-left", n)
            return
        event(f"data: {NEXT}\n\n")
        self.note("label-sent", n)
        while not self.done.wait(10):
            try:
                event(": ping\n\n")
            except OSError:
                self.note("feed-left", n)
                return

    def timeline(self):
        with self.lock:
            return "\n".join(f"  {at:7.1f}s  {kind} {detail}".rstrip()
                             for at, kind, detail in self.events)


def make_certificate(root):
    openssl = os.environ.get("OPENSSL") or shutil.which("openssl")
    if not openssl:
        raise RuntimeError("no openssl to make the host's certificate")
    cert, key = os.path.join(root, "cert.pem"), os.path.join(root, "key.pem")
    subprocess.run([openssl, "req", "-x509", "-newkey", "rsa:2048", "-nodes",
                    "-keyout", key, "-out", cert, "-days", "1", "-subj", "/CN=localhost",
                    "-addext", "subjectAltName=DNS:localhost,IP:127.0.0.1,IP:::1"],
                   check=True, capture_output=True)
    return cert, key


def verdict(host, budget_left):
    """None while the proof is still running; else (code, text)."""
    silent = host.first("feed-silent", "1")
    left = host.first("feed-left", "1")
    second = host.first("feed-open", "2")
    sent = host.first("label-sent", "2")
    acted = host.first("pointer-read", after=sent) if sent is not None else None
    if left is not None and silent is not None and left - silent < EARLIEST_LEAVE:
        return 1, (f"left the feed {left - silent:.1f}s into its silence: under the host's "
                   f"ping interval, so a healthy feed would be dropped too")
    if acted is not None:
        return 0, (f"left the silent feed after {left - silent:.1f}s, reconnected at "
                   f"{second:.1f}s, and read the pointer {acted - sent:.1f}s after the next label")
    if budget_left > 0:
        return None
    if host.first("tls-refused") is not None and silent is None:
        return 2, "the gateway never agreed on TLS with the host (trust, not the feed)"
    if silent is None:
        return 1, "the gateway never opened its release feed"
    if left is None:
        return 1, (f"the gateway held the silent feed for {time.monotonic() - host.start - silent:.0f}s "
                   f"and never left it: no label after it can reach this box")
    if second is None:
        return 1, "the gateway left the silent feed but never connected again"
    return 1, "the next label arrived but the gateway never read the channel pointer after it"


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("binary")
    parser.add_argument("--budget", type=float, default=240.0,
                        help="seconds from launch the whole proof may take")
    parser.add_argument("--keep", action="store_true", help="keep the scratch directory")
    args = parser.parse_args()
    binary = os.path.abspath(args.binary)

    version = subprocess.run([binary, "--version"], capture_output=True, text=True,
                             timeout=300).stdout.strip()
    # The label as VERSION-LABEL reads it: what follows `+', else the version
    # itself (a build from a tree with no git stamps a bare number).
    match = re.search(r"([0-9A-Za-z._+-]+)$", version)
    if not match:
        say(f"{binary} --version named no version: {version!r}")
        return 2
    label = match.group(1).rpartition("+")[2]

    root = tempfile.mkdtemp(prefix="feed-proof-")
    proc = None
    host = None
    try:
        try:
            cert, key = make_certificate(root)
        except (RuntimeError, subprocess.CalledProcessError) as error:
            say(f"cannot make a certificate: {error}")
            return 2
        host = Host(label, cert, key)
        threading.Thread(target=host.serve, daemon=True).start()

        home = os.path.join(root, "home")
        os.makedirs(home)
        with open(os.path.join(home, "config.jsonc"), "w") as config:
            json.dump({"update": {"mode": "auto", "url": f"https://localhost:{host.port}/dl"}}, config)
        # A scratch cache as well as a scratch home: a shared binary's warm
        # image lives under the cache, and a proof must not prune the one a
        # live gateway on this box runs from.
        env = dict(os.environ, NODECODE_HOME=home, XDG_CACHE_HOME=os.path.join(root, "cache"),
                   SSL_CERT_FILE=cert)
        serve_log = os.path.join(root, "serve.log")
        say(f"{version}; host on localhost:{host.port}; budget {args.budget:.0f}s")
        with open(serve_log, "w") as out:
            proc = subprocess.Popen([binary, "serve", "--store", os.path.join(home, "history.db")],
                                    env=env, stdout=out, stderr=subprocess.STDOUT)
        deadline = time.monotonic() + args.budget
        result = None
        while result is None:
            if proc.poll() is not None:
                result = (1, f"serve exited ({proc.returncode}) before the proof ended")
                break
            time.sleep(1)
            result = verdict(host, deadline - time.monotonic())
        code, text = result
        print(host.timeline(), flush=True)
        if code:
            with open(serve_log, errors="replace") as log:
                tail = log.readlines()[-30:]
            say("serve log, last lines:\n" + "".join(tail).rstrip())
        say(("PASS: " if code == 0 else "FAIL: ") + text)
        return code
    finally:
        if proc is not None and proc.poll() is None:
            proc.terminate()
            try:
                proc.wait(timeout=15)
            except subprocess.TimeoutExpired:
                proc.kill()
                proc.wait()
        if host is not None:
            host.done.set()
            host.socket.close()
        if args.keep:
            say(f"kept {root}")
        else:
            shutil.rmtree(root, ignore_errors=True)


if __name__ == "__main__":
    # A driver that stops the proof early (mac-build.sh, when a step beside it
    # fails) sends SIGTERM: exit through main's finally, so the serve it
    # started stops and its scratch goes, rather than dying around them.
    signal.signal(signal.SIGTERM, lambda signum, frame: sys.exit(128 + signum))
    sys.exit(main())
