#!/usr/bin/env python3
"""discord-qa.py -- the Discord cell driven end to end, the way people in a
server drive it.

Usage: discord-qa.py BINARY [--only NAME[,NAME...]] [--list] [--keep]

BINARY is the nodecode executable under proof. It serves headless (`serve') in
a scratch home -- updates off, every shipped cell installed as /setup
installs them, so the bot's menu is the one a person's puts up -- against two
stand-ins played here, on 127.0.0.1 and nowhere else:

  - Discord: the gateway (v10, JSON over a WebSocket) and every REST v10 call
    the adapter makes -- messages, edits, reactions, threads, uploads, the
    command menu, interaction callbacks -- holding a server the way Discord
    does: a guild with #general (a thread per ask) and #flat (the reply
    chain), two people (mike, the owner; kim, whom no list names), their DMs
    and the files they attach. What Discord refuses it refuses too: a message
    over 2000 characters, a component past its limits, a second answer to
    one interaction.
  - the model: a chat provider that answers each ask from a script keyed on a
    word in it -- plain words, eval calls first, a stall, a refusal.

Each scenario says something into the server the way a person would and holds
the bot to what that person would then see; the scenarios run in order, on
one bot, which `catch-up' restarts. --only runs the named ones (the boot
always runs). Exits 1 when a scenario fails, printing what the server held;
2 when the proof cannot run.
"""

import argparse
import base64
import hashlib
import http.server
import io
import json
import math
import os
import re
import shutil
import socket
import sqlite3
import struct
import subprocess
import sys
import tempfile
import threading
import time
import traceback
import urllib.parse
import wave
import zlib
from email import policy as email_policy
from email.parser import BytesParser

EPOCH = 1420070400000  # the instant a snowflake counts its milliseconds from
BOT, APP, GUILD = "1100000000000000001", "1100000000000000002", "1100000000000000003"
GENERAL, FLAT = "1100000000000000004", "1100000000000000005"
LOUNGE, LOCKED = "1100000000000000006", "1100000000000000007"  # voice; the bot may not sit in LOCKED
VOICE_STATES = 1 << 7  # GUILD_VOICE_STATES: without it no voice state is dispatched
USERS = {"mike": "1100000000000000010", "kim": "1100000000000000011"}
DMS = {"mike": "1100000000000000020", "kim": "1100000000000000021"}
CURSOR = " \u2589"     # what ends, on a card, the words the model is still writing
VOICE_MESSAGE = 1 << 13  # IS_VOICE_MESSAGE: the one attachment plays as a voice message
SUPPRESS_EMBEDS = 1 << 2  # a message flagged so draws no embed, its own included
COMPONENTS_V2 = 1 << 15   # IS_COMPONENTS_V2: the message is its components alone, for good
WS_GUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
# GUILDS, GUILD_MESSAGES, GUILD_MESSAGE_REACTIONS, DIRECT_MESSAGES, MESSAGE_CONTENT
INTENTS = {"GUILDS": 1 << 0, "GUILD_MESSAGES": 1 << 9, "DIRECT_MESSAGES": 1 << 12,
           "MESSAGE_CONTENT": 1 << 15}


class Failure(Exception):
    pass


def die(message, code=2):
    print("discord-qa: " + message, file=sys.stderr)
    sys.exit(code)


def check(condition, message):
    if not condition:
        raise Failure(message)
    return condition


# --- the WebSocket the gateway speaks ----------------------------------------

class Socket:
    """One server end of a WebSocket, upgraded from an HTTP request: frames
    read from the client (masked), written unmasked, a lock around writes."""

    def __init__(self, handler):
        key = handler.headers.get("Sec-WebSocket-Key", "")
        accept = base64.b64encode(hashlib.sha1((key + WS_GUID).encode()).digest()).decode()
        handler.send_response(101)
        handler.send_header("Upgrade", "websocket")
        handler.send_header("Connection", "Upgrade")
        handler.send_header("Sec-WebSocket-Accept", accept)
        handler.end_headers()
        handler.wfile.flush()
        self.rfile, self.sock = handler.rfile, handler.connection
        self.lock = threading.Lock()
        self.open = True

    def _write(self, opcode, data):
        n = len(data)
        head = bytes([0x80 | opcode])
        if n < 126:
            head += bytes([n])
        elif n < 65536:
            head += bytes([126]) + struct.pack(">H", n)
        else:
            head += bytes([127]) + struct.pack(">Q", n)
        with self.lock:
            if self.open:
                self.sock.sendall(head + data)

    def send(self, obj):
        try:
            self._write(1, json.dumps(obj).encode())
        except OSError:
            self.open = False

    def close(self, code):
        try:
            self._write(8, struct.pack(">H", code))
        except OSError:
            pass
        self.open = False
        try:
            self.sock.shutdown(socket.SHUT_RDWR)
        except OSError:
            pass

    def _read(self, n):
        data = self.rfile.read(n)
        if len(data) < n:
            raise EOFError
        return data

    def recv(self):
        """The next text message, or None once the connection ends."""
        message = b""
        try:
            while True:
                b0, b1 = self._read(2)
                opcode, n = b0 & 0x0F, b1 & 0x7F
                if n == 126:
                    n = struct.unpack(">H", self._read(2))[0]
                elif n == 127:
                    n = struct.unpack(">Q", self._read(8))[0]
                mask = self._read(4) if b1 & 0x80 else b"\0\0\0\0"
                data = bytes(b ^ mask[i % 4] for i, b in enumerate(self._read(n)))
                if opcode == 8:
                    self.close(1000)
                    return None
                if opcode == 9:
                    self._write(10, data)
                    continue
                if opcode in (0, 1, 2):
                    message += data
                    if b0 & 0x80:
                        return message.decode()
        except (EOFError, OSError, ValueError):
            self.open = False
            return None


# --- the Discord stand-in ----------------------------------------------------

def user(name):
    if name == "bot":
        return {"id": BOT, "username": "nodecode", "global_name": "Nodecode", "bot": True}
    return {"id": USERS[name], "username": name, "global_name": name.capitalize()}


def iso(ms):
    return time.strftime("%Y-%m-%dT%H:%M:%S", time.gmtime(ms / 1000)) + ".000000+00:00"


def snowflake_ms(sid):
    return (int(sid) >> 22) + EPOCH


def body_of(content_type, raw):
    """A request's JSON body and its uploaded files ({name: (filename, bytes, type)})."""
    if content_type.startswith("multipart/"):
        parsed = BytesParser(policy=email_policy.HTTP).parsebytes(
            b"Content-Type: " + content_type.encode() + b"\r\n\r\n" + raw)
        body, files = {}, {}
        for part in parsed.iter_parts():
            name = part.get_param("name", header="content-disposition")
            data = part.get_payload(decode=True) or b""
            if name == "payload_json":
                body = json.loads(data or b"{}")
            elif name:
                files[name] = (part.get_filename() or name, data, part.get_content_type())
        return body, files
    return (json.loads(raw) if raw.strip() else None), {}


def component_problems(rows):
    """What Discord would refuse in a message's components."""
    problems = []
    if len(rows or []) > 5:
        problems.append("more than 5 rows")
    for row in rows or []:
        for c in row.get("components") or []:
            if len(c.get("custom_id") or "") > 100:
                problems.append("custom_id over 100: " + c["custom_id"][:40])
            if len(c.get("label") or "") > 80:
                problems.append("label over 80")
            if c.get("type") == 3 and not 1 <= len(c.get("options") or []) <= 25:
                problems.append("a select with %d options" % len(c.get("options") or []))
            for o in c.get("options") or []:
                if len(o.get("value") or "") > 100 or len(o.get("label") or "") > 100:
                    problems.append("an option over 100")
    return problems


def embed_problems(embeds, flags=0):
    """What Discord would refuse, or hide, in a message's embeds."""
    problems = []
    if embeds and flags & SUPPRESS_EMBEDS:
        problems.append("embeds on a message flagged SUPPRESS_EMBEDS are never shown")
    if len(embeds or []) > 10:
        problems.append("more than 10 embeds")
    for e in embeds or []:
        sizes = {"title": len(e.get("title") or ""), "description": len(e.get("description") or ""),
                 "author.name": len((e.get("author") or {}).get("name") or ""),
                 "footer.text": len((e.get("footer") or {}).get("text") or "")}
        for key, cap in (("title", 256), ("description", 4096), ("author.name", 256),
                         ("footer.text", 2048)):
            if sizes[key] > cap:
                problems.append("%s over %d" % (key, cap))
        fields = e.get("fields") or []
        if len(fields) > 25:
            problems.append("more than 25 fields")
        for f in fields:
            if not 1 <= len(f.get("name") or "") <= 256 or not 1 <= len(f.get("value") or "") <= 1024:
                problems.append("a field's name or value empty or over its cap")
        total = sum(sizes.values()) + sum(len(f.get("name") or "") + len(f.get("value") or "")
                                          for f in fields)
        if total > 6000:
            problems.append("an embed over 6000 characters")
        if not 0 <= (e.get("color") or 0) <= 0xFFFFFF:
            problems.append("a colour out of range")
    return problems


def embed_text(message):
    """Every word a message's embeds show, in the order Discord draws them."""
    out = []
    for e in message.get("embeds") or []:
        out += [(e.get("author") or {}).get("name") or "", e.get("title") or "",
                e.get("description") or ""]
        for f in e.get("fields") or []:
            out += [f.get("name") or "", f.get("value") or ""]
        out.append((e.get("footer") or {}).get("text") or "")
    return "\n".join(x for x in out if x)


def walk(components):
    """Every component in COMPONENTS, in the order Discord draws them: a
container's and a row's children and a section's accessory included."""
    for c in components or []:
        yield c
        yield from walk(c.get("components"))
        if c.get("accessory"):
            yield from walk([c["accessory"]])


def v2_text(message):
    """Every word a Components V2 message's text displays show, in order."""
    return "\n".join(c.get("content") or "" for c in walk(message.get("components")) if c.get("type") == 10)


def shown(message):
    """What a message says in words: its content, its embeds' or its text displays'."""
    return message.get("content") or embed_text(message) or v2_text(message)


def container_of(message):
    """The container a Components V2 message draws, or None."""
    return next((c for c in message.get("components") or [] if c.get("type") == 17), None)


V2_TOP = {1, 9, 10, 12, 13, 14, 17}       # what a V2 message may hold at its top
V2_INSIDE = {1, 9, 10, 12, 13, 14}        # what a container may hold


def v2_problems(body, names, listed):
    """What Discord would refuse in a Components V2 message BODY: NAMES the
files the message holds once the request lands, by which attachment://
resolves; LISTED whether the request says which those are (its attachments
field), without which a reference is not found (discord-api-docs#7529)."""
    problems = []
    if body.get("content") not in (None,):
        problems.append("content on a Components V2 message")
    if body.get("embeds"):
        problems.append("embeds on a Components V2 message")
    every = list(walk(body.get("components")))
    if len(every) > 40:
        problems.append("%d components, over 40" % len(every))
    if sum(len(c.get("content") or "") for c in every if c.get("type") == 10) > 4000:
        problems.append("over 4000 characters of text")
    ids = [c.get("custom_id") for c in every if c.get("custom_id") is not None]
    if len(ids) != len(set(ids)):
        problems.append("a custom_id twice")
    for c in body.get("components") or []:
        if c.get("type") not in V2_TOP:
            problems.append("a top-level component of type %s" % c.get("type"))
    for c in every:
        kind = c.get("type")
        if kind == 17:
            if not 0 <= (c.get("accent_color") or 0) <= 0xFFFFFF:
                problems.append("an accent out of range")
            for child in c.get("components") or []:
                if child.get("type") not in V2_INSIDE:
                    problems.append("a container holding type %s" % child.get("type"))
        elif kind == 9:
            inner = c.get("components") or []
            if not 1 <= len(inner) <= 3 or any(x.get("type") != 10 for x in inner):
                problems.append("a section of other than one to three text displays")
            if (c.get("accessory") or {}).get("type") not in (2, 11):
                problems.append("a section without a button or thumbnail beside it")
        elif kind == 10:
            if not c.get("content"):
                problems.append("an empty text display")
        elif kind == 14:
            if c.get("spacing", 1) not in (1, 2):
                problems.append("a separator's spacing %s" % c.get("spacing"))
        elif kind == 1:
            inner = c.get("components") or []
            buttons = [x for x in inner if x.get("type") == 2]
            if not (1 <= len(buttons) == len(inner) <= 5 or (len(inner) == 1 and inner[0].get("type") in (3, 5, 6, 7, 8))):
                problems.append("a row of other than one to five buttons or one select")
        elif kind == 2:
            if c.get("style") != 5 and not 1 <= len(c.get("custom_id") or "") <= 100:
                problems.append("a button whose custom_id is empty or over 100")
            if len(c.get("label") or "") > 80:
                problems.append("label over 80")
        elif kind == 12:
            items = c.get("items") or []
            if not 1 <= len(items) <= 10:
                problems.append("a gallery of %d items" % len(items))
            for item in items:
                url = (item.get("media") or {}).get("url") or ""
                if url.startswith("attachment://") and (not listed or url[len("attachment://"):] not in names):
                    problems.append("UNFURLED_MEDIA_ITEM_REFERENCED_ATTACHMENT_NOT_FOUND: %s" % url)
                if len(item.get("description") or "") > 1024:
                    problems.append("a picture's description over 1024")
    return problems


class Discord(http.server.ThreadingHTTPServer):
    daemon_threads = True

    def __init__(self, files_dir):
        super().__init__(("127.0.0.1", 0), DiscordHandler)
        self.port = self.server_port
        self.files_dir = files_dir
        self.cv = threading.Condition()
        self.last_ms, self.counter, self.gw_seq = 0, 0, 0
        self.channels = {
            GENERAL: {"id": GENERAL, "type": 0, "name": "general", "guild_id": GUILD},
            FLAT: {"id": FLAT, "type": 0, "name": "flat", "guild_id": GUILD},
            LOUNGE: {"id": LOUNGE, "type": 2, "name": "lounge", "guild_id": GUILD},
            LOCKED: {"id": LOCKED, "type": 2, "name": "locked", "guild_id": GUILD},
            DMS["mike"]: {"id": DMS["mike"], "type": 1, "recipients": [user("mike")]},
            DMS["kim"]: {"id": DMS["kim"], "type": 1, "recipients": [user("kim")]},
        }
        self.messages, self.order = {}, []
        self.calls = []            # every REST call: {t, method, path, body, status}
        self.unknown = []          # calls this stand-in has no answer for
        self.refused = []          # calls refused as Discord would refuse them
        self.reaction_log = []     # (message id, "+"/"-", emoji)
        self.history = {}          # message id -> every content it carried, oldest first
        self.interactions = {}     # token -> {id, user, channel, message, callbacks, original}
        self.app_emojis = {}       # name -> the application's emoji
        self.sockets = []
        self.connects = []         # ("identify" | "resume", intents)
        self.voice = {}            # user id -> the voice channel they sit in
        self.voice_ops = []        # every op 4 the bot sent: its d
        self.commands = None

    # Every mutation happens under the condition, and wakes whoever waits.
    def snowflake(self):
        ms = max(int(time.time() * 1000), self.last_ms)
        self.last_ms, self.counter = ms, self.counter + 1
        return str(((ms - EPOCH) << 22) | (self.counter & 0x3FFFFF))

    def post(self, channel_id, author, content="", **extra):
        mid = self.snowflake()
        channel = self.channels[channel_id]
        msg = {"id": mid, "channel_id": channel_id, "author": author, "content": content or "",
               "timestamp": iso(snowflake_ms(mid)), "edited_timestamp": None, "type": 0,
               "mentions": [], "attachments": [], "embeds": [], "components": [], "flags": 0,
               "pinned": False, "tts": False, "mention_everyone": False}
        if channel.get("guild_id"):
            msg["guild_id"] = GUILD
        msg.update(extra)
        if msg.get("message_reference"):
            msg["type"] = 19
            ref = self.messages.get(msg["message_reference"].get("message_id"))
            if ref:
                msg["referenced_message"] = {k: v for k, v in ref.items() if k != "referenced_message"}
        self.messages[mid] = msg
        self.history[mid] = [msg["content"]]
        self.order.append(mid)
        channel["last_message_id"] = mid
        self.cv.notify_all()
        return msg

    def dispatch(self, event, data):
        with self.cv:
            self.gw_seq += 1
            payload = {"op": 0, "t": event, "s": self.gw_seq, "d": data}
            sockets = list(self.sockets)
        for ws in sockets:
            ws.send(payload)

    def gateway_message(self, msg, event="MESSAGE_CREATE"):
        data = dict(msg)
        if msg.get("guild_id") and not msg["author"].get("bot"):
            data["member"] = {"roles": [], "nick": None, "joined_at": iso(EPOCH)}
        self.dispatch(event, data)

    # --- the gateway --------------------------------------------------------

    def gateway(self, handler):
        ws = Socket(handler)
        with self.cv:
            self.sockets.append(ws)
        ws.send({"op": 10, "d": {"heartbeat_interval": 41250}})
        try:
            while True:
                text = ws.recv()
                if text is None:
                    return
                payload = json.loads(text)
                op, d = payload.get("op"), payload.get("d") or {}
                if op == 1:
                    ws.send({"op": 11})
                elif op == 2:
                    self.ready(ws, d)
                elif op == 4:
                    self.voice_op(d)
                elif op == 6:
                    with self.cv:
                        self.connects.append(("resume", None))
                        self.gw_seq += 1
                        seq = self.gw_seq
                        self.cv.notify_all()
                    ws.send({"op": 0, "t": "RESUMED", "s": seq, "d": {}})
        finally:
            with self.cv:
                if ws in self.sockets:
                    self.sockets.remove(ws)
                self.cv.notify_all()

    def ready(self, ws, identify):
        with self.cv:
            self.connects.append(("identify", identify.get("intents")))
            self.gw_seq += 2
            seq = self.gw_seq
            guild = {"id": GUILD, "name": "QA", "unavailable": False, "member_count": 3,
                     "roles": [{"id": GUILD, "name": "@everyone", "permissions": "0"}],
                     "channels": [dict(c) for c in self.channels.values() if c["type"] in (0, 2)],
                     "voice_states": [self.voice_state(u, c) for u, c in self.voice.items()]
                     if identify.get("intents", 0) & VOICE_STATES else [],
                     "threads": [dict(c) for c in self.channels.values()
                                 if c["type"] == 11 and not c.get("deleted")],
                     "members": [], "joined_at": iso(EPOCH)}
            self.cv.notify_all()
        ws.send({"op": 0, "t": "READY", "s": seq - 1, "d": {
            "v": 10, "user": user("bot"), "session_id": "qa-session",
            "resume_gateway_url": "ws://127.0.0.1:%d/gateway" % self.port,
            "guilds": [{"id": GUILD, "unavailable": True}],
            "application": {"id": APP, "flags": 0}}})
        ws.send({"op": 0, "t": "GUILD_CREATE", "s": seq, "d": guild})

    def voice_state(self, uid, channel):
        return {"guild_id": GUILD, "channel_id": channel, "user_id": uid,
                "session_id": "qa-voice-" + uid, "deaf": False, "mute": False,
                "self_deaf": False, "self_mute": False, "suppress": False,
                "member": {"user": user("bot") if uid == BOT else {"id": uid}, "roles": []}}

    def voice_moved(self, uid, channel):
        """UID now sits in CHANNEL (None: nowhere), said to a bot that asked
        for voice states."""
        with self.cv:
            if channel:
                self.voice[uid] = channel
            else:
                self.voice.pop(uid, None)
            intents = next((c[1] for c in reversed(self.connects) if c[0] == "identify"), 0)
            self.cv.notify_all()
        if intents & VOICE_STATES:
            self.dispatch("VOICE_STATE_UPDATE", self.voice_state(uid, channel))

    def voice_op(self, d):
        """The bot's op 4: it sits down where it asks to, unless the channel
        is LOCKED -- Discord answers a join it will not grant with nothing --
        and gets up on a null channel. No voice server is dialled."""
        with self.cv:
            self.voice_ops.append(d)
            self.cv.notify_all()
        channel = d.get("channel_id")
        if channel == LOCKED:
            return
        if channel or BOT in self.voice:
            self.voice_moved(BOT, channel)

    def drop(self, code):
        """Close every gateway connection from the server's end with CODE."""
        with self.cv:
            sockets = list(self.sockets)
        for ws in sockets:
            ws.close(code)

    # --- REST ---------------------------------------------------------------

    def rest(self, method, path, query, body, files):
        with self.cv:
            status, answer = self._rest(method, path, query, body, files)
            record = {"t": time.time(), "method": method, "path": path, "body": body, "status": status,
                      "files": len(files or {}),
                      "made": answer.get("id") if method == "POST" and isinstance(answer, dict) else None}
            self.calls.append(record)
            if status == 404 and isinstance(answer, dict) and answer.get("code") == 0:
                self.unknown.append(record)
            elif status == 400:
                self.refused.append(dict(record, answer=answer))
            self.cv.notify_all()
        return status, answer

    def _rest(self, method, path, query, body, files):
        seg = path.strip("/").split("/")
        body = body if isinstance(body, (dict, list)) else {}
        bad = lambda message: (400, {"code": 50035, "message": message})
        if path == "users/@me":
            return 200, user("bot")
        if path == "users/@me/guilds":
            return 200, [{"id": GUILD, "name": "QA", "owner": False, "permissions": "0"}]
        if path == "applications/@me":
            return 200, {"id": APP, "name": "nodecode", "bot": user("bot")}
        if path == "gateway/bot":
            return 200, {"url": "ws://127.0.0.1:%d/gateway" % self.port, "shards": 1}
        if seg[0] == "guilds" and len(seg) == 4 and seg[2] == "members":
            return 200, {"user": user("bot") if seg[3] == BOT else {"id": seg[3]}, "roles": []}
        if seg[0] == "guilds" and len(seg) == 3 and seg[2] == "channels":
            return 200, [c for c in self.channels.values() if c.get("guild_id") == GUILD]
        if seg[0] == "applications" and seg[-1] == "emojis" and seg[1] == APP:
            # The application's own emojis: listed, and made from a data URI
            # Discord takes -- a name of 2 to 32 word characters, an image of
            # at most 256 KB.
            if method == "GET":
                return 200, {"items": list(self.app_emojis.values())}
            name, image = body.get("name") or "", body.get("image") or ""
            head, _, data = image.partition(";base64,")
            if not re.fullmatch(r"\w{2,32}", name) or not head.startswith("data:image/") or not data:
                return bad("Invalid Form Body: name or image")
            if len(base64.b64decode(data)) > 256 * 1024:
                return bad("image: File cannot be larger than 256.0 kb.")
            if name in self.app_emojis:
                return bad("name: An emoji with this name already exists")
            self.app_emojis[name] = {"id": self.snowflake(), "name": name,
                                     "animated": head == "data:image/gif"}
            return 201, self.app_emojis[name]
        if seg[0] == "applications" and seg[-1] == "commands" and method == "PUT":
            names = [c.get("name", "") for c in body]
            wrong = [n for n in names if not re.fullmatch(r"[-_a-z0-9]{1,32}", n)]
            if wrong or len(set(names)) != len(names) or len(names) > 100 or any(
                    not 1 <= len(c.get("description", "")) <= 100 for c in body):
                return bad("Invalid Form Body: commands " + ", ".join(wrong))
            self.commands = body
            return 200, [dict(c, id=self.snowflake(), application_id=APP) for c in body]
        if seg[0] == "channels" and seg[1] in self.channels:
            return self._channel(method, seg, query, body, files, bad)
        if seg[0] == "channels":
            return 404, {"code": 10003, "message": "Unknown Channel"}
        if seg[0] == "interactions" and len(seg) == 4 and seg[3] == "callback":
            return self._callback(seg[2], body, bad)
        if seg[0] == "webhooks" and len(seg) >= 3:
            return self._webhook(method, seg, body, bad)
        return 404, {"code": 0, "message": "404: Not Found"}

    def _channel(self, method, seg, query, body, files, bad):
        cid, channel = seg[1], self.channels[seg[1]]
        if len(seg) == 2:
            if method == "PATCH":
                channel.update({k: body[k] for k in ("name",) if k in body})
                self.dispatch_later("THREAD_UPDATE" if channel["type"] == 11 else "CHANNEL_UPDATE",
                                    dict(channel))
            elif method == "DELETE":
                channel["deleted"] = True
                self.dispatch_later("THREAD_DELETE", {"id": cid, "type": channel["type"],
                                                      "guild_id": GUILD, "parent_id": channel.get("parent_id")})
            return 200, channel
        if seg[2] == "typing":
            return 204, None
        if seg[2] == "threads" or (len(seg) == 5 and seg[4] == "threads"):
            starter = seg[3] if len(seg) == 5 else None
            if starter and starter not in self.messages:
                return 404, {"code": 10008, "message": "Unknown Message"}
            if starter and self.messages[starter].get("thread"):
                return 400, {"code": 160004, "message": "A thread has already been created for this message"}
            # A thread opened off a message takes that message's id, as Discord's do.
            tid = starter or self.snowflake()
            thread = {"id": tid, "type": 11, "name": body.get("name", ""), "parent_id": cid,
                      "guild_id": GUILD, "owner_id": BOT, "message_count": 0,
                      "thread_metadata": {"archived": False, "locked": False,
                                          "auto_archive_duration": 1440}}
            self.channels[tid] = thread
            if starter:
                self.messages[starter]["thread"] = dict(thread)
            self.dispatch_later("THREAD_CREATE", dict(thread, newly_created=True))
            return 201, thread
        if seg[2] != "messages":
            return 404, {"code": 0, "message": "404: Not Found"}
        if len(seg) == 3 and method == "GET":
            after, before = int(query.get("after", 0)), int(query.get("before", 0) or 1 << 63)
            limit = int(query.get("limit", 50))
            found = [self.messages[i] for i in reversed(self.order)
                     if self.messages[i]["channel_id"] == cid and not self.messages[i].get("deleted")
                     and after < int(i) < before]
            if after:
                found = found[-limit:]
            return 200, found[:limit]
        if len(seg) == 3 and method == "POST":
            content = body.get("content") or ""
            v2 = body.get("flags", 0) & COMPONENTS_V2
            if len(content) > 2000:
                return bad("content: Must be 2000 or fewer in length.")
            # What the payload declares of each file, by its files[i] index.
            declared = {str(a.get("id")): a for a in body.get("attachments") or []}
            misnamed = [n for n, (filename, _, _) in files.items()
                        if declared.get(n[len("files["):-1], {}).get("filename", filename) != filename]
            if misnamed:
                return bad("attachments: a file named otherwise than its upload: %s" % misnamed)
            if v2:
                problems = v2_problems(body, {f[0] for f in files.values()}, "attachments" in body)
                if problems:
                    return bad("components: " + "; ".join(problems))
            else:
                problems = component_problems(body.get("components"))
                if any(c.get("type") != 1 for c in body.get("components") or []):
                    problems.append("Components V2 parts without the IS_COMPONENTS_V2 flag")
                if problems:
                    return bad("components: " + "; ".join(problems))
                problems = embed_problems(body.get("embeds"), body.get("flags", 0))
                if problems:
                    return bad("embeds: " + "; ".join(problems))
            if not content and not files and not body.get("components") and not body.get("embeds"):
                return bad("Cannot send an empty message")
            if body.get("flags", 0) & VOICE_MESSAGE:
                voice = list(declared.values())
                if content or len(files) != 1 or len(voice) != 1:
                    return bad("A voice message is one audio attachment and no content")
                if not isinstance(voice[0].get("duration_secs"), (int, float)) \
                        or not voice[0].get("waveform"):
                    return bad("attachments.0: A voice message declares duration_secs and waveform")
            attachments = []
            for name, (filename, data, kind) in files.items():
                aid = self.snowflake()
                with open(os.path.join(self.files_dir, aid), "wb") as out:
                    out.write(data)
                meta = declared.get(name[len("files["):-1], {})
                attachments.append(dict({"id": aid, "filename": filename, "size": len(data),
                                         "content_type": kind,
                                         "url": "http://127.0.0.1:%d/attachments/%s/%s/%s"
                                                % (self.port, cid, aid, filename)},
                                        **{k: meta[k] for k in ("duration_secs", "waveform")
                                           if k in meta}))
            ref = body.get("message_reference")
            msg = self.post(cid, user("bot"), content, components=body.get("components") or [],
                            embeds=body.get("embeds") or [], embed_history=[body.get("embeds") or []],
                            components_history=[body.get("components") or []] if v2 else [],
                            flags=body.get("flags", 0), attachments=attachments,
                            allowed_mentions=body.get("allowed_mentions"),
                            **({"message_reference": {"message_id": ref["message_id"], "channel_id": cid}}
                               if ref else {}))
            self.dispatch_later("MESSAGE_CREATE", msg)
            return 200, msg
        msg = self.messages.get(seg[3])
        if msg is None or msg.get("deleted") or msg["channel_id"] != cid:
            return 404, {"code": 10008, "message": "Unknown Message"}
        if len(seg) >= 6 and seg[4] == "reactions":
            emoji = urllib.parse.unquote(seg[5])
            mine = msg.setdefault("my_reactions", [])
            if method == "PUT" and emoji not in mine:
                mine.append(emoji)
                self.reaction_log.append((msg["id"], "+", emoji))
            elif method == "DELETE" and emoji in mine:
                mine.remove(emoji)
                self.reaction_log.append((msg["id"], "-", emoji))
            msg["reactions"] = [{"emoji": {"id": None, "name": e}, "count": 1, "me": True} for e in mine]
            return 204, None
        if method == "PATCH":
            if len(body.get("content") or "") > 2000:
                return bad("content: Must be 2000 or fewer in length.")
            was_v2 = msg.get("flags", 0) & COMPONENTS_V2
            if was_v2 and "flags" in body and not body["flags"] & COMPONENTS_V2:
                return bad("flags: IS_COMPONENTS_V2 cannot be removed from a message")
            v2 = was_v2 or body.get("flags", 0) & COMPONENTS_V2
            # The files the message holds once the edit lands: with an
            # attachments field, those it lists -- kept by id, new by files[i]
            # index -- and none other; without one, all it held and every
            # upload appended.
            uploads = {}
            for name, (filename, data, kind) in files.items():
                aid = self.snowflake()
                with open(os.path.join(self.files_dir, aid), "wb") as out:
                    out.write(data)
                uploads[name[len("files["):-1]] = {
                    "id": aid, "filename": filename, "size": len(data), "content_type": kind,
                    "url": "http://127.0.0.1:%d/attachments/%s/%s/%s" % (self.port, cid, aid, filename)}
            if "attachments" in body:
                held = {a["id"]: a for a in msg.get("attachments") or []}
                kept, wrong = [], []
                for a in body["attachments"]:
                    key = str(a.get("id"))
                    if key in held:
                        kept.append(held[key])
                    elif key in uploads and a.get("filename", uploads[key]["filename"]) == uploads[key]["filename"]:
                        kept.append(uploads[key])
                    else:
                        wrong.append(key)
                if wrong:
                    return bad("attachments: no such file or one named otherwise than its upload: %s" % wrong)
            else:
                kept = list(msg.get("attachments") or []) + list(uploads.values())
            if v2:
                problems = v2_problems(body, {a["filename"] for a in kept}, "attachments" in body)
                if problems:
                    return bad("components: " + "; ".join(problems))
            else:
                problems = component_problems(body.get("components"))
                if any(c.get("type") != 1 for c in body.get("components") or []):
                    problems.append("Components V2 parts without the IS_COMPONENTS_V2 flag")
                if problems:
                    return bad("components: " + "; ".join(problems))
                problems = embed_problems(body.get("embeds"), body.get("flags", msg.get("flags", 0)))
                if problems:
                    return bad("embeds: " + "; ".join(problems))
            for key in ("content", "components", "flags", "embeds"):
                if key in body:
                    msg[key] = body[key]
            if v2:
                msg["content"], msg["flags"] = "", msg.get("flags", 0) | COMPONENTS_V2
                if "components" in body:
                    msg.setdefault("components_history", []).append(body["components"])
            msg["attachments"] = kept
            if "content" in body:
                self.history.setdefault(msg["id"], []).append(msg["content"])
            if "embeds" in body:
                msg.setdefault("embed_history", []).append(body["embeds"])
            msg["edited_timestamp"] = iso(time.time() * 1000)
            self.dispatch_later("MESSAGE_UPDATE", msg)
            return 200, msg
        if method == "DELETE":
            msg["deleted"] = True
            self.dispatch_later("MESSAGE_DELETE", {"id": msg["id"], "channel_id": cid,
                                                   "guild_id": msg.get("guild_id")})
            return 204, None
        return 200, msg

    def _callback(self, token, body, bad):
        rec = self.interactions.get(token)
        if rec is None:
            return 404, {"code": 10062, "message": "Unknown interaction"}
        if rec["callbacks"] and body.get("type") != 8:
            return 400, {"code": 40060, "message": "Interaction has already been acknowledged."}
        if body.get("type") == 8:
            choices = (body.get("data") or {}).get("choices")
            if not isinstance(choices, list) or len(choices) > 25 or any(
                    not 1 <= len(c.get("name", "")) <= 100 or len(str(c.get("value", ""))) > 100
                    for c in choices):
                return bad("Invalid Form Body: data.choices")
        rec["callbacks"].append(body)
        kind, data = body.get("type"), body.get("data") or {}
        if kind in (4, 5):
            flags = data.get("flags", 0)
            msg = self.post(rec["channel"], user("bot"), data.get("content", ""),
                            components=data.get("components") or [], flags=flags | (128 if kind == 5 else 0),
                            interaction={"id": rec["id"], "type": rec["type"], "name": rec.get("name"),
                                         "user": user(rec["user"])},
                            ephemeral_for=rec["user"] if flags & 64 else None)
            rec["original"] = msg["id"]
        elif kind in (6, 7):
            rec["original"] = rec.get("message")
            if kind == 7:
                self.messages[rec["original"]].update(
                    {k: data[k] for k in ("content", "components") if k in data})
        return 204, None

    def _webhook(self, method, seg, body, bad):
        rec = self.interactions.get(seg[2])
        if rec is None or seg[1] != APP:
            return 404, {"code": 10015, "message": "Unknown Webhook"}
        if len(body.get("content") or "") > 2000:
            return bad("content: Must be 2000 or fewer in length.")
        problems = component_problems(body.get("components")) + embed_problems(body.get("embeds"))
        if problems:
            return bad("components: " + "; ".join(problems))
        if seg[-1] == "@original" and method == "PATCH":
            if not rec.get("original"):
                return 404, {"code": 10008, "message": "Unknown Message"}
            msg = self.messages[rec["original"]]
            for key in ("content", "components", "embeds"):
                if key in body:
                    msg[key] = body[key]
            msg["flags"] = msg.get("flags", 0) & ~128
            msg["edited_timestamp"] = iso(time.time() * 1000)
            rec.setdefault("edits", []).append(body)
            return 200, msg
        if len(seg) == 3 and method == "POST":
            flags = body.get("flags", 0)
            msg = self.post(rec["channel"], user("bot"), body.get("content", ""),
                            components=body.get("components") or [], flags=flags,
                            ephemeral_for=rec["user"] if flags & 64 else None)
            return 200, msg
        return 404, {"code": 0, "message": "404: Not Found"}

    def dispatch_later(self, event, data):
        """Send EVENT once the REST answer that caused it is on its way, as
        Discord's gateway follows its REST."""
        threading.Timer(0.05, self.dispatch, (event, dict(data))).start()

    # --- what a person does -------------------------------------------------

    def say(self, who, text, channel=GENERAL, mention=True, attachments=(), reply_to=None):
        """WHO types TEXT in CHANNEL, @mentioning the bot first unless not."""
        with self.cv:
            content = ("<@%s> " % BOT if mention else "") + text
            files = []
            for filename, data in attachments:
                aid = self.snowflake()
                with open(os.path.join(self.files_dir, aid), "wb") as out:
                    out.write(data)
                files.append({"id": aid, "filename": filename, "size": len(data),
                              "content_type": "text/plain; charset=utf-8",
                              "url": "http://127.0.0.1:%d/attachments/%s/%s/%s"
                                     % (self.port, channel, aid, filename)})
            msg = self.post(channel, user(who), content, attachments=files,
                            mentions=[user("bot")] if mention else [],
                            **({"message_reference": {"message_id": reply_to, "channel_id": channel}}
                               if reply_to else {}))
        self.gateway_message(msg)
        return msg

    def edit(self, msg, text):
        with self.cv:
            msg["content"] = text
            msg["mentions"] = [user("bot")] if "<@%s>" % BOT in text else []
            msg["edited_timestamp"] = iso(time.time() * 1000)
        self.gateway_message(msg, "MESSAGE_UPDATE")

    def interact(self, who, kind, data, channel, message=None, name=None):
        with self.cv:
            iid, token = self.snowflake(), "qa-token-" + self.snowflake()
            self.interactions[token] = {"id": iid, "type": kind, "user": who, "channel": channel,
                                        "message": message["id"] if message else None,
                                        "name": name, "callbacks": []}
        ch = self.channels[channel]
        d = {"id": iid, "token": token, "type": kind, "application_id": APP, "channel_id": channel,
             "channel": {k: ch[k] for k in ("id", "type", "guild_id", "parent_id", "name") if k in ch},
             "data": data, "version": 1, "locale": "en-US", "app_permissions": "0"}
        if ch.get("guild_id"):
            d["guild_id"] = GUILD
            d["member"] = {"user": user(who), "roles": [], "nick": None}
        else:
            d["user"] = user(who)
        if message:
            d["message"] = {k: message.get(k) for k in ("id", "channel_id", "content", "components",
                                                         "flags", "author", "timestamp")}
        self.dispatch("INTERACTION_CREATE", d)
        return token

    def slash(self, who, name, args=None, channel=GENERAL):
        data = {"id": self.snowflake(), "name": name, "type": 1}
        if args:
            data["options"] = [{"name": "args", "type": 3, "value": args}]
        return self.interact(who, 2, data, channel, name=name)

    def complete(self, who, name, typed, channel=GENERAL):
        """The choices the bot offers while WHO types TYPED into /NAME's args
        option -- asked only of an option the menu marks autocomplete, as
        Discord asks."""
        entry = next((c for c in self.commands or [] if c["name"] == name), None)
        check(entry is not None, "/%s is not in the menu" % name)
        check(any(o.get("autocomplete") for o in entry.get("options") or []),
              "the menu does not complete /%s" % name)
        data = {"id": self.snowflake(), "name": name, "type": 1,
                "options": [{"name": "args", "type": 3, "value": typed, "focused": True}]}
        rec = self.interactions[self.interact(who, 4, data, channel, name=name)]
        self.wait(lambda: rec["callbacks"], "the choices for /%s %r" % (name, typed))
        check(rec["callbacks"][0].get("type") == 8, "/%s was not answered with choices: %s"
              % (name, rec["callbacks"][0]))
        return rec["callbacks"][0]["data"]["choices"]

    def sit(self, who, channel):
        """WHO sits in the voice channel CHANNEL, or gets up (None)."""
        self.voice_moved(USERS[who], channel)

    def press(self, who, msg, custom_id=None, value=None):
        """WHO presses the button CUSTOM_ID on MSG, or picks VALUE in its menu,
        a person's reaction time after it showed: never before Discord's
        answer to the post that made it has reached the bot."""
        time.sleep(0.3)
        for c in walk(msg.get("components")):
            if custom_id is not None and c.get("custom_id") == custom_id:
                check(not c.get("disabled"), "the button %r is disabled" % custom_id)
                return self.interact(who, 3, {"custom_id": custom_id, "component_type": 2},
                                     msg["channel_id"], message=msg)
            if value is not None and c.get("type") == 3 and any(
                    o.get("value") == value for o in c.get("options") or []):
                return self.interact(who, 3, {"custom_id": c["custom_id"], "component_type": 3,
                                              "values": [value]}, msg["channel_id"], message=msg)
        raise Failure("no %s on message %s: %s" % (
            "button %r" % custom_id if custom_id else "option %r" % value, msg["id"],
            json.dumps(msg.get("components"))[:400]))

    # --- what a person sees -------------------------------------------------

    def wait(self, probe, what, timeout=30.0):
        """PROBE's first truthy answer within TIMEOUT seconds, else a failure
        naming WHAT."""
        deadline = time.time() + timeout
        with self.cv:
            while True:
                found = probe()
                if found:
                    return found
                left = deadline - time.time()
                if left <= 0:
                    raise Failure("timed out after %.0fs waiting for %s" % (timeout, what))
                self.cv.wait(min(left, 0.25))

    def still(self, probe, what, seconds):
        """PROBE stays falsy for SECONDS, else a failure naming WHAT."""
        deadline = time.time() + seconds
        with self.cv:
            while time.time() < deadline:
                found = probe()
                if found:
                    raise Failure("%s: %s" % (what, json.dumps(found, default=str)[:400]))
                self.cv.wait(0.25)

    def bot_messages(self, channel, after=None):
        return [self.messages[i] for i in self.order
                if self.messages[i]["channel_id"] == channel and self.messages[i]["author"]["id"] == BOT
                and not self.messages[i].get("deleted") and (after is None or int(i) > int(after))]

    def reactions(self, mid):
        return list(self.messages[mid].get("my_reactions", []))

    def reactions_seen(self, mid):
        return [(sign, emoji) for m, sign, emoji in self.reaction_log if m == mid]

    def threads_off(self, mid):
        return [c for c in self.channels.values() if c["type"] == 11 and c["id"] == mid]


class DiscordHandler(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, *args):
        pass

    def handle_one(self, method):
        url = urllib.parse.urlsplit(self.path)
        if url.path.startswith("/gateway") and self.headers.get("Upgrade", "").lower() == "websocket":
            self.close_connection = True
            return self.server.gateway(self)
        n = int(self.headers.get("Content-Length") or 0)
        raw = self.rfile.read(n) if n else b""
        if url.path.startswith("/attachments/"):
            path = os.path.join(self.server.files_dir, url.path.split("/")[3])
            if not os.path.exists(path):
                return self.answer(404, {"message": "not found"})
            with open(path, "rb") as f:
                data = f.read()
            with self.server.cv:
                self.server.calls.append({"t": time.time(), "method": "GET", "path": url.path,
                                          "body": None, "status": 200})
                self.server.cv.notify_all()
            self.send_response(200)
            self.send_header("Content-Type", "text/plain; charset=utf-8")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)
            return
        if not url.path.startswith("/api/v10/"):
            return self.answer(404, {"code": 0, "message": "404: Not Found"})
        try:
            body, files = body_of(self.headers.get("Content-Type", ""), raw)
        except ValueError:
            return self.answer(400, {"code": 50109, "message": "The request body contains invalid JSON."})
        query = dict(urllib.parse.parse_qsl(url.query))
        status, answer = self.server.rest(method, url.path[len("/api/v10/"):], query, body, files)
        self.answer(status, answer)

    def answer(self, status, obj):
        data = b"" if obj is None else json.dumps(obj).encode()
        self.send_response(status)
        if obj is not None:
            self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_GET(self):
        self.handle_one("GET")

    def do_POST(self):
        self.handle_one("POST")

    def do_PUT(self):
        self.handle_one("PUT")

    def do_PATCH(self):
        self.handle_one("PATCH")

    def do_DELETE(self):
        self.handle_one("DELETE")


# --- the model ---------------------------------------------------------------

def text_of(message):
    content = message.get("content")
    if isinstance(content, str):
        return content
    return " ".join(p.get("text", "") for p in content or [] if isinstance(p, dict))


def square_png(size=16):
    """A picture a turn can look at: a SIZE-pixel square, half blurple, as PNG bytes."""
    rows = b"".join(b"\x00" + b"".join(b"\x58\x65\xf2" if x < size // 2 else b"\xff\xff\xff"
                                        for x in range(size)) for _ in range(size))

    def chunk(kind, data):
        return struct.pack(">I", len(data)) + kind + data + struct.pack(">I", zlib.crc32(kind + data) & 0xFFFFFFFF)
    return (b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", struct.pack(">IIBBBBB", size, size, 8, 2, 0, 0, 0))
            + chunk(b"IDAT", zlib.compress(rows)) + chunk(b"IEND", b""))


def tone_wav(seconds=1.5, rate=24000):
    """Words said as a speech API sends them: 16-bit mono WAV, a tone that
    swells from nothing, so its waveform rises."""
    n = int(seconds * rate)
    frames = b"".join(struct.pack("<h", int(12000 * i / n * math.sin(2 * math.pi * 440 * i / rate)))
                      for i in range(n))
    out = io.BytesIO()
    with wave.open(out, "wb") as w:
        w.setnchannels(1)
        w.setsampwidth(2)
        w.setframerate(rate)
        w.writeframes(frames)
    return out.getvalue()


class Model(http.server.ThreadingHTTPServer):
    """A chat provider on the openai-completions wire. An ask's script is
    picked by the key (a `qa-' word) that comes last in the newest user
    message carrying one (TITLE_DELAY holds a naming call that long): CALLS are eval forms called a round each, the words
    in SAID spoken in the round of the same place, THINK reasoning streamed
    before it, then TEXT answers, PACE
    seconds between each eight words the model writes; STATUS
    refuses the request outright; STALL holds the round after
    the calls open for that many seconds, a keepalive every quarter second. A
    request offering no tools (a thread's title) is answered TITLE; one with
    no key (the organism's note) is said back as QA-SAID and the note. It is the
    speech and transcription API too: a recording is heard as TRANSCRIPT, and
    words are said as a tone that swells for a second and a half."""
    daemon_threads = True

    def __init__(self):
        super().__init__(("127.0.0.1", 0), ModelHandler)
        self.cv = threading.Condition()
        self.scripts = {}
        self.requests = []   # {t, model, tools, key, ask, raw}
        self.cuts = []       # keys whose stalled stream the client cut
        self.transcript = ""
        self.spoken = []     # each /audio/speech request's JSON
        self.titled = {}     # key -> when its title answer was written

    def script(self, key, **plan):
        with self.cv:
            self.scripts[key] = plan

    def asked(self, key):
        with self.cv:
            return [r for r in self.requests if r["key"] == key and r["tools"]]


class ModelHandler(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, *args):
        pass

    def json(self, status, obj):
        data = json.dumps(obj).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_GET(self):
        self.json(200, {"object": "list", "data": [{"id": m, "object": "model", "owned_by": "qa"}
                                                   for m in ("mock-fast", "mock-deep")]})

    def do_POST(self):
        raw = self.rfile.read(int(self.headers.get("Content-Length") or 0))
        if self.path.endswith("/audio/transcriptions"):
            return self.json(200, {"text": self.server.transcript})
        if self.path.endswith("/audio/speech"):
            with self.server.cv:
                self.server.spoken.append(json.loads(raw or b"{}"))
                self.server.cv.notify_all()
            data = tone_wav()
            self.send_response(200)
            self.send_header("Content-Type", "audio/wav")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)
            return
        req = json.loads(raw or b"{}")
        messages = req.get("messages") or []
        tools = bool(req.get("tools"))
        server = self.server
        key, ask, done = None, "", 0
        with server.cv:
            keys = list(server.scripts)
        for i in range(len(messages) - 1, -1, -1):
            if messages[i].get("role") != "user":
                continue
            said = text_of(messages[i])
            hits = [(said.rfind(k), k) for k in keys if k in said]
            if hits:
                key, ask = max(hits)[1], said
                done = sum(1 for m in messages[i + 1:] if m.get("role") == "tool")
                break
        plan = server.scripts.get(key, {}) if key else {}
        with server.cv:
            server.requests.append({"t": time.time(), "model": req.get("model"), "tools": tools,
                                    "key": key, "ask": ask, "raw": raw.decode("utf-8", "replace")})
            server.cv.notify_all()
        if tools and plan.get("status"):
            return self.json(plan["status"], {"error": {"message": "QA refuses this request",
                                                        "type": "invalid_request_error"}})
        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.send_header("Cache-Control", "no-cache")
        self.send_header("Transfer-Encoding", "chunked")
        self.end_headers()
        base = {"id": "chatcmpl-qa", "object": "chat.completion.chunk", "created": int(time.time()),
                "model": req.get("model", "mock-fast")}

        def chunk(obj):
            data = ("data: " + (obj if isinstance(obj, str) else json.dumps(obj)) + "\n\n").encode()
            self.wfile.write(b"%x\r\n" % len(data) + data + b"\r\n")
            self.wfile.flush()

        def delta(d, finish=None):
            chunk(dict(base, choices=[{"index": 0, "delta": d, "finish_reason": finish}]))

        def write(text):
            words = text.split(" ")
            for i in range(0, len(words), 8):
                if i:
                    time.sleep(plan.get("pace", 0))
                delta({"content": " ".join(words[i:i + 8]) + (" " if i + 8 < len(words) else "")})

        calls = plan.get("calls") or []
        try:
            delta({"role": "assistant", "content": ""})
            if not tools:
                time.sleep(plan.get("title_delay", 0))
                note = next((text_of(m) for m in reversed(messages) if m.get("role") == "user"), "")
                text = plan.get("title", "QA title") if key else "QA-SAID " + note
                with server.cv:
                    server.titled[key] = time.time()
                    server.cv.notify_all()
            elif done < len(calls):
                args = json.dumps({"form": calls[done]})
                for thought in (plan.get("think") or [])[done:done + 1]:
                    delta({"reasoning_content": thought})
                    time.sleep(0.6)
                said = (plan.get("said") or [])[done:done + 1]
                if said and said[0]:
                    write(said[0])
                delta({"tool_calls": [{"index": 0, "id": "call_qa_%d_%d" % (time.time() * 1000, done),
                                       "type": "function", "function": {"name": "eval", "arguments": ""}}]})
                for i in range(0, len(args), 64):
                    delta({"tool_calls": [{"index": 0, "function": {"arguments": args[i:i + 64]}}]})
                delta({}, "tool_calls")
                text = None
            elif plan.get("stall"):
                deadline = time.time() + plan["stall"]
                while time.time() < deadline:
                    delta({"content": ""})
                    time.sleep(0.25)
                text = "QA stalled to the end."
            else:
                text = plan.get("text", "QA reply.")
            if text is not None:
                write(text)
                delta({}, "stop")
            tokens = max(len(raw) // 4, 1)
            # Half the prompt served from cache, so a card's meter has a c mark to show.
            chunk(dict(base, choices=[], usage={"prompt_tokens": tokens, "completion_tokens": 20,
                                                "total_tokens": tokens + 20,
                                                "prompt_tokens_details": {"cached_tokens": tokens // 2}}))
            chunk("[DONE]")
            self.wfile.write(b"0\r\n\r\n")
            self.wfile.flush()
        except (BrokenPipeError, ConnectionResetError):
            with server.cv:
                server.cuts.append(key)
                server.cv.notify_all()


# --- the bot under proof ------------------------------------------------------

class Bot:
    def __init__(self, binary, home, env):
        self.binary, self.home, self.env = binary, home, env
        self.proc = None
        self.log = os.path.join(home, "serve.log")

    def start(self):
        self.proc = subprocess.Popen([self.binary, "serve"], env=self.env, cwd=os.path.join(self.home, "work"),
                                     stdout=open(self.log, "a"), stderr=subprocess.STDOUT,
                                     start_new_session=True)

    def alive(self):
        return self.proc is not None and self.proc.poll() is None

    def stop(self):
        if not self.alive():
            return
        os.killpg(self.proc.pid, 15)
        try:
            self.proc.wait(timeout=30)
        except subprocess.TimeoutExpired:
            os.killpg(self.proc.pid, 9)
            self.proc.wait()

    def tail(self, n=30):
        try:
            with open(self.log, errors="replace") as f:
                return "".join(f.readlines()[-n:])
        except OSError:
            return ""


# --- the scenarios -----------------------------------------------------------

def connected(dc, after=0):
    """Wait for the gateway to identify for the (AFTER+1)th time."""
    dc.wait(lambda: len([c for c in dc.connects if c[0] == "identify"]) > after,
            "the bot to identify on the gateway", timeout=300)


def answer_in(dc, channel, word, after=None, timeout=30):
    """The first bot message after AFTER carrying WORD."""
    return dc.wait(lambda: next((m for m in dc.bot_messages(channel, after) if word in m["content"]), None),
                   "an answer carrying %s in %s" % (word, channel), timeout=timeout)


def boot(qa):
    """The bot identifies with the intents it needs and puts its command menu."""
    dc = qa.dc
    _, intents = next(c for c in dc.connects if c[0] == "identify")
    for name, bit in INTENTS.items():
        check(intents & bit, "identify left out the %s intent (intents %s)" % (name, intents))
    commands = dc.wait(lambda: dc.commands, "the command menu (PUT applications/%s/commands)" % APP)
    names = {c["name"] for c in commands}
    for name in ("help", "new", "stop", "models", "channels"):
        check(name in names, "/%s is not in the command menu: %s" % (name, sorted(names)))
    qa.note("%d commands, intents %d" % (len(names), intents))


def canary(qa):
    """An @mention in #general opens a thread off the ask, answers there, and
    the ask carries the eye while it runs, then a check."""
    dc, model = qa.dc, qa.model
    model.script("qa-canary", text="QA-CANARY-OK the marker came back.")
    ask = dc.say("mike", "qa-canary say the marker")
    dc.wait(lambda: "👀" in dc.reactions(ask["id"]), "the eye on the ask")
    thread = dc.wait(lambda: dc.threads_off(ask["id"]), "a thread off the ask")[0]
    answer = answer_in(dc, thread["id"], "QA-CANARY-OK")
    check("---" not in answer["content"], "the answer opens with a divider: %r" % answer["content"][:60])
    check(not [m for m in dc.bot_messages(GENERAL, ask["id"]) if "QA-CANARY-OK" in m["content"]],
          "the answer also landed in #general")
    dc.wait(lambda: dc.reactions(ask["id"]) == ["✅"], "the check on the ask, the eye gone")
    check(("+", "👀") in dc.reactions_seen(ask["id"]), "the ask never carried the eye")
    qa.thread = thread["id"]
    qa.note("thread %r" % dc.channels[thread["id"]]["name"])


def follow_up(qa):
    """A line typed in the bot's thread needs no mention, and the turn reads
    the thread's earlier exchange."""
    dc, model = qa.dc, qa.model
    check(qa.thread, "no thread from the canary")
    model.script("qa-follow", text="QA-FOLLOW-OK still here.")
    dc.say("mike", "qa-follow and again", channel=qa.thread, mention=False)
    answer_in(dc, qa.thread, "QA-FOLLOW-OK")
    req = model.asked("qa-follow")[-1]
    check("QA-CANARY-OK" in req["raw"], "the follow-up's turn did not read the thread's first answer")


def mention_gate(qa):
    """An unmentioned line in #general is heard, not answered."""
    dc, model = qa.dc, qa.model
    model.script("qa-gate", text="QA-GATE-WRONG")
    ask = dc.say("mike", "qa-gate nobody asked you", mention=False)
    dc.still(lambda: model.asked("qa-gate") or dc.reactions(ask["id"]) or dc.threads_off(ask["id"]),
             "the bot answered an unmentioned line", 5)


def stranger(qa):
    """kim, whom no list names, @mentions the bot: nothing happens, and
    nothing of it reaches the model."""
    dc, model = qa.dc, qa.model
    model.script("qa-stranger", text="QA-STRANGER-WRONG")
    ask = dc.say("kim", "qa-stranger let me in")
    dc.still(lambda: model.asked("qa-stranger") or dc.reactions(ask["id"]) or dc.threads_off(ask["id"]),
             "the bot acted on a stranger's ask", 5)
    check(not [r for r in model.requests if "qa-stranger" in r["raw"]],
          "a stranger's words reached the model")


def edit_wakes(qa):
    """An unmentioned line edited to @mention the bot is answered."""
    dc, model = qa.dc, qa.model
    model.script("qa-edit", text="QA-EDIT-OK woken by the edit.")
    ask = dc.say("mike", "qa-edit are you there", channel=FLAT, mention=False)
    time.sleep(1.5)
    check(not model.asked("qa-edit"), "the unmentioned line was answered before the edit")
    dc.edit(ask, "<@%s> qa-edit are you there" % BOT)
    answer_in(dc, FLAT, "QA-EDIT-OK")


def reply_in_place(qa):
    """In a flat channel the answer is a reply to the ask, and no thread opens."""
    dc, model = qa.dc, qa.model
    model.script("qa-flat", text="QA-FLAT-OK in place.")
    ask = dc.say("mike", "qa-flat answer here", channel=FLAT)
    answer = answer_in(dc, FLAT, "QA-FLAT-OK")
    check((answer.get("message_reference") or {}).get("message_id") == ask["id"],
          "the answer is no reply to the ask: %s" % answer.get("message_reference"))
    check(not dc.threads_off(ask["id"]), "a thread opened in a flat channel")
    dc.wait(lambda: dc.reactions(ask["id"]) == ["✅"], "the check on the ask")


def commentary(qa):
    """Words the model says on its way show on its turn's card, never as a
    message of their own, and the answer posts alone."""
    dc, model = qa.dc, qa.model
    model.script("qa-comment", calls=["(sleep 3)"], said=["QA-COMMENT-SAID looking first."],
                 text="QA-COMMENT-OK done looking.")
    ask = dc.say("mike", "qa-comment think out loud", channel=FLAT)
    answer = answer_in(dc, FLAT, "QA-COMMENT-OK")
    check(answer["content"] == "QA-COMMENT-OK done looking.",
          "the answer is not its words alone: %r" % answer["content"][:80])
    posts = dc.bot_messages(FLAT, ask["id"])
    check(not [m for m in posts if "QA-COMMENT-SAID" in m["content"]],
          "the words said on the way posted as a message of their own")
    card = next((m for m in posts if container_of(m)), None)
    check(card is not None, "the turn showed no card")
    check(any("QA-COMMENT-SAID looking first." in v2_text({"components": v})
              for v in card.get("components_history") or []),
          "no version of the card carried the words said on the way")
    dc.wait(lambda: v2_text(card).startswith("-# done"), "the card settled", timeout=15)
    check("QA-COMMENT-SAID" not in v2_text(card), "the settled card still shows the words the answer says more than")


def streaming(qa):
    """The words a round writes show on its card as the model writes them, the
    cursor at their end: a round that goes on to a tool keeps them there as
    what it said, and the answer's words, once written, leave the card for
    the answer, which posts fresh as a reply to the ask. No message carries
    words on their way."""
    dc, model = qa.dc, qa.model
    said = "QA-STREAM-SAID " + " ".join("looking%02d" % i for i in range(64))
    text = "QA-STREAM-OK " + " ".join("word%03d" % i for i in range(120))
    model.script("qa-stream", calls=["(sleep 1)", "(sleep 3)"], said=["", said], text=text, pace=0.5)
    ask = dc.say("mike", "qa-stream write it out", channel=FLAT)
    answer = answer_in(dc, FLAT, "QA-STREAM-OK", timeout=60)
    check(answer["content"] == text, "the answer reads %r" % answer["content"][-60:])
    check((answer.get("message_reference") or {}).get("message_id") == ask["id"],
          "the answer is no reply to the ask: %s" % answer.get("message_reference"))
    card = next((m for m in dc.bot_messages(FLAT, ask["id"]) if container_of(m)), None)
    check(card is not None, "the turn showed no card")
    others = [m for m in dc.messages.values() if m["channel_id"] == FLAT and int(m["id"]) > int(ask["id"])
              and m["author"]["id"] == BOT and m["id"] not in (card["id"], answer["id"])
              and not m.get("ephemeral_for")]
    check(not others, "a message carried words on their way: %s" % [m["content"][:40] for m in others])
    texts = [v2_text({"components": v}) for v in card.get("components_history") or []]
    check(any("QA-STREAM-SAID" in t and CURSOR in t for t in texts), "the words said never streamed on the card")
    check(any(said in t and said + CURSOR not in t for t in texts),
          "the words said on the way did not stay on the card, the cursor gone")
    writing = [t for t in texts if "QA-STREAM-OK" in t]
    check(len(writing) >= 2 and all(CURSOR in t for t in writing),
          "the answer's words did not grow on the card: %d versions" % len(writing))
    dc.wait(lambda: v2_text(card).startswith("-# done"), "the card settled", timeout=15)
    check("QA-STREAM" not in v2_text(card), "the settled card still shows words: %r" % v2_text(card)[:200])
    qa.note("the answer grew on the card over %d versions" % len(writing))


def long_answer(qa):
    """An answer past 2000 characters arrives whole, in order, in messages
    Discord accepts."""
    dc, model = qa.dc, qa.model
    lines = ["L%03d %s" % (i, "lorem ipsum dolor sit amet " * 2) for i in range(1, 91)]
    model.script("qa-long", text="\n".join(lines))
    ask = dc.say("mike", "qa-long write a lot", channel=FLAT)
    dc.wait(lambda: any("L090" in m["content"] for m in dc.bot_messages(FLAT, ask["id"])),
            "the last line of the long answer")
    parts = [m for m in dc.bot_messages(FLAT, ask["id"]) if re.search(r"L\d{3}", m["content"])]
    check(len(parts) >= 3, "%d messages for %d characters" % (len(parts), len("\n".join(lines))))
    whole = "\n".join(m["content"] for m in parts)
    found = [int(n) for n in re.findall(r"L(\d{3})", whole)]
    check(found == list(range(1, 91)), "lines missing or out of order: %s" % found[:12])
    check((parts[0].get("message_reference") or {}).get("message_id") == ask["id"],
          "the first part is no reply to the ask")
    check(not dc.refused, "Discord refused a call: %s" % dc.refused[-1:])
    qa.note("%d messages" % len(parts))


def long_thread_answer(qa):
    """A long answer in the thread its ask opened arrives whole, the asker
    named ahead of it inside Discord's 2000 characters. Lines of twenty put
    the cut within a line of 2000 before the name is counted."""
    dc, model = qa.dc, qa.model
    lines = [" ".join("w%03d" % i for i in range(n, n + 4)) for n in range(0, 720, 4)]
    model.script("qa-longthread", text="QA-LT-START\n" + "\n".join(lines) + "\nQA-LT-END")
    ask = dc.say("mike", "qa-longthread write a lot")
    thread = dc.wait(lambda: dc.threads_off(ask["id"]), "a thread off the ask")[0]
    dc.wait(lambda: any("QA-LT-END" in m["content"] for m in dc.bot_messages(thread["id"])),
            "the end of the long answer in the thread")
    parts = [m for m in dc.bot_messages(thread["id"]) if re.search(r"QA-LT-|w\d{3}", m["content"])]
    check(parts[0]["content"].startswith("<@%s> " % USERS["mike"]),
          "the first part does not name the asker: %r" % parts[0]["content"][:40])
    found = [int(n) for n in re.findall(r"w(\d{3})", " ".join(m["content"] for m in parts))]
    check(found == list(range(720)), "words missing or out of order: %s" % found[:12])
    check(not dc.refused, "Discord refused a call: %s" % dc.refused[-1:])
    dc.wait(lambda: dc.reactions(ask["id"]) == ["✅"], "the check on the ask")
    qa.note("%d messages, the first %d characters" % (len(parts), len(parts[0]["content"])))


def failed_turn(qa):
    """A turn the provider refuses ends with a cross on the ask and a word in
    the room, never silence."""
    dc, model = qa.dc, qa.model
    model.script("qa-fail", status=400)
    ask = dc.say("mike", "qa-fail this will not work", channel=FLAT)
    dc.wait(lambda: dc.reactions(ask["id"]) == ["❌"], "the cross on the failed ask", timeout=60)
    said = dc.wait(lambda: [m for m in dc.bot_messages(FLAT, ask["id"]) if shown(m).strip()],
                   "a notice of the failure in the room")
    qa.note(shown(said[-1])[:80].replace("\n", " "))


def stop_button(qa):
    """A running turn's card carries Stop; pressing it ends the turn at once,
    the Stop leaves, and the card says who stopped it."""
    dc, model = qa.dc, qa.model
    model.script("qa-stall", calls=["(+ 1 2)"], stall=120)
    ask = dc.say("mike", "qa-stall take your time", channel=FLAT)

    def stop_line():
        for m in dc.bot_messages(FLAT, ask["id"]):
            for c in walk(m.get("components")):
                if (c.get("custom_id") or "").startswith("nck:stop:"):
                    return m, c
    line, button = dc.wait(stop_line, "a card with a Stop button", timeout=40)
    check(button.get("style") == 4, "Stop is not a danger button: %s" % button)
    dc.wait(lambda: model.asked("qa-stall") and len(model.asked("qa-stall")) >= 2, "the stalled round")
    pressed = time.time()
    token = dc.press("mike", line, custom_id=button["custom_id"])
    dc.wait(lambda: dc.interactions[token]["callbacks"], "the press acknowledged")
    check(dc.interactions[token]["callbacks"][0].get("type") == 6,
          "the press was not held as a deferred update: %s" % dc.interactions[token]["callbacks"])
    dc.wait(lambda: "qa-stall" in model.cuts, "the stalled stream cut", timeout=15)
    dc.wait(lambda: not stop_line(), "the Stop button gone", timeout=15)
    dc.wait(lambda: "👀" not in dc.reactions(ask["id"]), "the eye off the stopped ask", timeout=15)
    dc.wait(lambda: "stopped by Mike" in v2_text(line) and v2_text(line).startswith("-# stopped · "),
            "the card saying Mike stopped it")
    check((container_of(line) or {}).get("accent_color") == 0x80848E,
          "a stopped card is not grey: %s" % container_of(line))
    qa.note("stopped %.1fs after the press" % (time.time() - pressed))


def card(qa):
    """A turn that calls tools shows a card: a container of components titled
    its ask's task -- the ask's first words, then the title a model writes,
    which the turn never waits on: held past the answer, it retitles the
    settled card -- its thought's title in italics, a row per step, each one
    landed with its Output button; no call posts a message of its own. The
    answer posts below, its words alone, and the card settles green above it,
    `done', with Details. Details and a step's Output answer their presser
    privately and refuse anyone else."""
    dc, model = qa.dc, qa.model
    model.script("qa-card", calls=["(sleep 3)", "(+ 1 2)"],
                 think=["**Checking the clock**\n\nA pause comes first.", "Then the sum."],
                 text="QA-CARD the sum is 3.", title="Add one and two after a pause", title_delay=8)
    ask = dc.say("mike", "qa-card add one and two after a pause", channel=FLAT)

    def card_msg():
        return next((m for m in dc.bot_messages(FLAT, ask["id"]) if container_of(m)), None)

    def parts(components):
        return (container_of({"components": components}) or {}).get("components") or []
    msg = dc.wait(card_msg, "a card", timeout=40)
    answer = answer_in(dc, FLAT, "QA-CARD", after=ask["id"], timeout=60)
    check("qa-card" not in model.titled, "the turn waited on its title: the answer came after it")
    check(answer["content"] == "QA-CARD the sum is 3.",
          "the answer carries more than its words: %r" % answer["content"])
    check(int(msg["id"]) < int(answer["id"]), "the card is not above the answer")
    check(msg["flags"] & COMPONENTS_V2 and not msg["content"] and not msg.get("embeds"),
          "the card is not components alone: %r" % {k: msg.get(k) for k in ("flags", "content", "embeds")})
    dc.wait(lambda: v2_text(msg).startswith("-# done · "), "the card settled to done", timeout=15)
    dc.wait(lambda: "### Add one and two after a pause" in v2_text(msg),
            "the settled card retitled once its late title landed", timeout=20)
    done = container_of(msg)
    check(done.get("accent_color") == 0x23A55A, "a done card is not green: %s" % done.get("accent_color"))
    rows = [c for c in parts(msg["components"]) if c.get("type") == 9]
    done_mark = "<:nc_done:%s>" % dc.app_emojis.get("nc_done", {}).get("id")
    check(len(rows) == 2 and all(v2_text(r).startswith(done_mark) for r in rows),
          "the done card's two steps do not both wear the green check: %r" % [v2_text(r) for r in rows])
    outputs = [r["accessory"].get("custom_id", "") for r in rows]
    check(all(o.startswith("nck:step:") for o in outputs) and all(r["accessory"].get("label") == "Output" for r in rows),
          "a landed step carries no Output button: %s" % [r.get("accessory") for r in rows])
    check(re.search(r"\n-# mock/mock-fast · 2 steps\n-# ↑\S+ c\d+\.\d+% ↓60 · ctx \d+%", v2_text(msg)),
          "the card's footer names no model or run line: %r" % v2_text(msg)[-200:])
    seen = msg.get("components_history") or []
    texts = [v2_text({"components": v}) for v in seen]
    check(any("### qa-card add one and two" in t for t in texts),
          "the card did not open under its ask's first words: %s" % [t[:80] for t in texts])
    running = [v for v in seen if (container_of({"components": v}) or {}).get("accent_color") == 0x5865F2]
    check(any("*Checking the clock*" in v2_text({"components": v}) for v in running),
          "no running view named the thought's title: %s" % json.dumps(running)[:600])
    spinner = "<a:nc_running:%s>" % dc.app_emojis.get("nc_running", {}).get("id")
    check(any(len(parts(v)) > 1 and parts(v)[1].get("type") == 10 and parts(v)[1]["content"].startswith(spinner)
              for v in running), "no running view opened on the spinner")

    def said(c):
        return c.get("content") or "" if c.get("type") == 10 else v2_text(c)
    check(any(said(c).startswith(spinner) and " · " in said(c) for v in running for c in parts(v)[2:]),
          "no running view showed its running step spinning")
    others = [m for m in dc.bot_messages(FLAT, ask["id"])
              if m["id"] not in (msg["id"], answer["id"]) and not m.get("ephemeral_for")]
    check(not others, "a call posted a message of its own: %r" % [m["content"][:60] for m in others])
    ids = [c.get("custom_id") or "" for c in walk(msg["components"]) if c.get("type") == 2]
    check(any(i.startswith("nck:details:") for i in ids) and not any(i.startswith("nck:stop:") for i in ids),
          "a settled card keeps Details alone: %s" % ids)
    details = next(i for i in ids if i.startswith("nck:details:"))
    for who, press, expect in (("mike", details, "**Steps** · 2"),
                               ("kim", details, "Details are for the person who asked and the room's operators."),
                               ("mike", outputs[-1], "```\n3\n```"),
                               ("kim", outputs[-1], "Details are for the person who asked and the room's operators.")):
        token = dc.press(who, msg, custom_id=press)
        dc.wait(lambda: dc.interactions[token]["callbacks"], "%s's %s answered" % (who, press))
        got = dc.interactions[token]["callbacks"][0]
        check(got.get("type") == 4 and (got.get("data") or {}).get("flags", 0) & 64,
              "%s did not answer %s privately: %s" % (press, who, got))
        check(expect in got["data"].get("content", ""),
              "%s's %s said %r" % (who, press, got["data"].get("content", "")[:200]))
    qa.note("%d card versions, steps with Output" % len(seen))


def card_pictures(qa):
    """A picture the turn looks at joins its card as a gallery: uploaded once,
    with the edit that first shows it, then kept by its id on every later
    edit, the settled card included."""
    dc, model = qa.dc, qa.model
    shot = os.path.join(qa.home, "qa-shot.png")
    with open(shot, "wb") as f:
        f.write(square_png())
    model.script("qa-look", calls=["(look %s)" % json.dumps(shot), "(sleep 4)"], text="QA-LOOK it is a square.")
    ask = dc.say("mike", "qa-look look at this square", channel=FLAT)
    msg = dc.wait(lambda: next((m for m in dc.bot_messages(FLAT, ask["id"]) if container_of(m)), None),
                  "a card", timeout=40)
    answer_in(dc, FLAT, "QA-LOOK", after=ask["id"], timeout=90)
    dc.wait(lambda: v2_text(msg).startswith("-# done · "), "the card settled to done", timeout=15)

    def gallery(components):
        return next((c for c in walk(components) if c.get("type") == 12), None)
    shown_in = [v for v in msg.get("components_history") or [] if gallery(v)]
    check(len(shown_in) >= 2, "the picture rode fewer than two card versions: %d" % len(shown_in))
    urls = {i["media"]["url"] for v in shown_in for i in gallery(v)["items"]}
    check(urls == {"attachment://step-1.png"}, "the gallery named %s" % urls)
    check(gallery(msg["components"]), "the settled card lost its picture")
    names = [a["filename"] for a in msg.get("attachments") or []]
    check(names == ["step-1.png"], "the card holds %s" % names)
    with open(os.path.join(dc.files_dir, msg["attachments"][0]["id"]), "rb") as f:
        check(f.read() == square_png(), "the picture uploaded is not the one looked at")
    uploads = [c for c in dc.calls if c["status"] == 200 and c["files"]
               and (c["path"].endswith("/messages/" + msg["id"]) or c["made"] == msg["id"])]
    check(len(uploads) == 1, "the picture was sent %d times, not once" % len(uploads))
    check(len([c for c in dc.calls if c["path"].endswith("/messages/" + msg["id"])]) >= 2,
          "no edit after the picture came to keep it")
    qa.note("%d versions carried the picture, sent once" % len(shown_in))


def clarify(qa):
    """A clarifying question carries its options as buttons; a press answers
    it in place and the turn goes on with the pick."""
    dc, model = qa.dc, qa.model
    labels = ["Zero setup", "Relational queries", "Realtime sync", "Scale past one box"]
    form = "(nck:answer-choices '(%s) :platform \"discord\" :channel \"%s\")" % (
        " ".join('"%s"' % label for label in labels), FLAT)
    model.script("qa-clarify", calls=[form], text="QA-QUESTION what matters most for this project?")
    model.script("Zero setup", text="QA-PICKED-SQLITE one file, no server.")
    ask = dc.say("mike", "qa-clarify pick me a database", channel=FLAT)
    card = answer_in(dc, FLAT, "QA-QUESTION")
    dc.wait(lambda: dc.reactions(ask["id"]) == ["✅"], "the question's turn settled")
    buttons = [c for row in card["components"] for c in row["components"]]
    check([b.get("label") for b in buttons] == labels, "the buttons read %s" % [b.get("label") for b in buttons])
    check(all(b["custom_id"] == "nck:say:" + b["label"] for b in buttons), "a button says something else")
    token = dc.press("mike", card, custom_id="nck:say:Zero setup")
    dc.wait(lambda: dc.interactions[token]["callbacks"], "the press acknowledged")
    check(dc.interactions[token]["callbacks"][0].get("type") == 6, "the press was not held in place")
    dc.wait(lambda: "Answered: Zero setup" in card["content"] and not card["components"],
            "the question marked answered, its buttons gone")
    check("QA-QUESTION" in card["content"], "the question lost its words: %r" % card["content"])
    answer_in(dc, FLAT, "QA-PICKED-SQLITE", after=ask["id"])
    check(any("qa-clarify" in r["raw"] for r in model.asked("Zero setup")),
          "the pick ran without the question's lane")


def models_picker(qa):
    """/models is a private card: a provider menu, then that provider's
    models; the pick moves the room, a stranger's press is refused, and Reset
    goes back."""
    dc, model = qa.dc, qa.model
    token = dc.slash("mike", "models", channel=FLAT)
    rec = dc.interactions[token]
    dc.wait(lambda: rec.get("edits"), "the picker card")
    check(rec["callbacks"][0].get("data", {}).get("flags", 0) & 64, "the picker is not private")
    card = dc.messages[rec["original"]]
    check("Model Picker" in card["content"], "the card reads %r" % card["content"][:200])
    menu = card["components"][0]["components"][0]
    check(menu["type"] == 3, "the card's first control is no menu")
    providers = [o["value"] for o in menu["options"]]
    check("nck:say:/models mock" in providers and "nck:say:/models lab" in providers,
          "the provider menu offers %s" % providers)
    pick = dc.press("mike", card, value="nck:say:/models mock")
    dc.wait(lambda: "mock-deep" in json.dumps(card["components"]), "the mock provider's models")
    check(dc.interactions[pick]["callbacks"][0].get("type") == 6, "the provider pick did not edit in place")
    before = json.dumps([card["content"], card["components"]])
    stranger_press = dc.press("kim", card, value="nck:say:/models mock mock-deep")
    refusal = dc.wait(lambda: next((m for m in dc.bot_messages(FLAT) if m.get("ephemeral_for") == "kim"
                                    and (m.get("interaction") or {}).get("id")
                                    == dc.interactions[stranger_press]["id"]), None),
                      "a private refusal for the stranger")
    check(dc.interactions[stranger_press]["callbacks"][0].get("type") == 5,
          "the stranger's refusal was not a message of its own")
    check(json.dumps([card["content"], card["components"]]) == before, "the stranger's press changed the card")
    dc.press("mike", card, value="nck:say:/models mock mock-deep")
    dc.wait(lambda: "runs mock/mock-deep" in card["content"], "the card confirming mock-deep")
    qa.note("refusal %r" % refusal["content"][:60])
    model.script("qa-which", text="QA-WHICH-OK")
    dc.say("mike", "qa-which model now", channel=FLAT)
    answer_in(dc, FLAT, "QA-WHICH-OK")
    check(model.asked("qa-which")[-1]["model"] == "mock-deep",
          "the next ask ran on %s" % model.asked("qa-which")[-1]["model"])
    back = dc.slash("mike", "models", args="mock", channel=FLAT)
    dc.wait(lambda: dc.interactions[back].get("edits"), "the mock models card")
    models_card = dc.messages[dc.interactions[back]["original"]]
    dc.press("mike", models_card, custom_id="nck:say:/models default")
    dc.wait(lambda: "runs the organism's default" in models_card["content"], "the reset confirmed")
    model.script("qa-reset", text="QA-RESET-OK")
    dc.say("mike", "qa-reset model now", channel=FLAT)
    answer_in(dc, FLAT, "QA-RESET-OK")
    check(model.asked("qa-reset")[-1]["model"] == "mock-fast",
          "after Reset the ask ran on %s" % model.asked("qa-reset")[-1]["model"])


def thread_reset(qa):
    """In a thread running its channel's model, the picker's Reset is closed;
    once the thread picks its own, Reset takes it back to the channel's."""
    dc = qa.dc
    check(qa.thread, "no thread from the canary")

    def models(args, channel):
        token = dc.slash("mike", "models", args=args, channel=channel)
        rec = dc.interactions[token]
        dc.wait(lambda: rec.get("edits"), "/models %s in %s" % (args, dc.channels[channel]["name"]))
        return dc.messages[rec["original"]]

    def reset(card):
        return next(c for row in card["components"] for c in row["components"]
                    if c.get("label") == "Reset to default")
    check("this channel runs mock/mock-deep" in models("mock mock-deep", GENERAL)["content"],
          "#general did not pick mock-deep")
    card = models("mock", qa.thread)
    check(reset(card).get("disabled"), "Reset is open in a thread running its channel's model")
    dc.press("mike", card, value="nck:say:/models mock mock-fast")
    dc.wait(lambda: "this thread runs mock/mock-fast" in card["content"], "the thread's own pick")
    card = models("mock", qa.thread)
    check(not reset(card).get("disabled"), "Reset is closed on the thread's own pick")
    dc.press("mike", card, custom_id="nck:say:/models default")
    dc.wait(lambda: "this thread runs mock/mock-deep from its next ask" in card["content"],
            "the reset back to the channel's model")
    models("default", GENERAL)


def agents(qa):
    """#flat is routed to the agent scribe: an ask there runs in scribe's
    folder, under its AGENTS.md, with its persona and its standing prompt. An
    ask in #general, which no route takes, carries none of them, until /agent
    scribe hands #general to scribe; /agent default gives it back."""
    dc, model = qa.dc, qa.model
    marks = ("QA-SCRIBE-RULES", "QA-SCRIBE-SOUL", "QA-SCRIBE-PROMPT")

    def ask(key, channel):
        model.script(key, text="QA-AGENT-OK %s." % key)
        said = dc.say("mike", "%s who answers here" % key, channel=channel)
        where = (FLAT if channel == FLAT
                 else dc.wait(lambda: dc.threads_off(said["id"]), "a thread off the ask")[0]["id"])
        answer_in(dc, where, "QA-AGENT-OK %s" % key)
        return [m for m in marks if m in model.asked(key)[0]["raw"]]

    def agent(args):
        token = dc.slash("mike", "agent", args=args, channel=GENERAL)
        rec = dc.interactions[token]
        dc.wait(lambda: rec.get("edits"), "/agent %s" % args)
        return dc.messages[rec["original"]]["content"]

    check(ask("qa-routed", FLAT) == list(marks), "the routed ask did not run as scribe")
    check(ask("qa-unrouted", GENERAL) == [], "an ask no route takes ran as scribe")
    said = agent("")
    check("this channel is answered by the channel itself" in said and "agents: scribe" in said,
          "bare /agent said %r" % said)
    said = agent("scribe")
    check("this channel is answered by agent scribe from its next ask" in said, "/agent scribe said %r" % said)
    check(ask("qa-handed", GENERAL) == list(marks), "the handed channel's ask did not run as scribe")
    said = agent("default")
    check("this channel is answered by the channel itself from its next ask" in said,
          "/agent default said %r" % said)
    check(ask("qa-handback", GENERAL) == [], "the channel given back still ran as scribe")
    qa.note("#flat by its route, #general by /agent and back")


def completions(qa):
    """While the owner types a command's argument, the menu offers what it
    can become: /agent the room's agents, scribe beside its folder, then
    default; /voice the verbs that would do something now -- no seat, so join
    and not leave. Someone who may not run the command is offered nothing."""
    dc = qa.dc

    def offered(name, typed, who="mike"):
        return [(c["name"], c["value"]) for c in dc.complete(who, name, typed)]

    agents = offered("agent", "")
    check([v for _, v in agents] == ["scribe", "default"], "/agent offered %r" % agents)
    check(agents[0][0].startswith("scribe — ") and agents[0][0].rstrip("/").endswith("scribe"),
          "scribe is not offered beside its folder: %r" % agents[0][0])
    check(offered("agent", "SC") == agents[:1], "/agent SC offered %r" % offered("agent", "SC"))
    verbs = [v for _, v in offered("voice", "")]
    check(verbs == ["join", "status", "off", "on", "tts"], "/voice offered %r" % verbs)
    check(offered("agent", "", who="kim") == [], "a stranger was offered the agents")
    qa.note("/agent: %s; /voice: %s" % (", ".join(v for _, v in agents), ", ".join(verbs)))


def file_in(qa):
    """A file attached to an ask is fetched and reaches the turn."""
    dc, model = qa.dc, qa.model
    model.script("qa-file-in", text="QA-FILE-IN-OK read it.")
    ask = dc.say("mike", "qa-file-in what does the note say", channel=FLAT,
                 attachments=[("notes.txt", b"QA-ATTACHED-PAYLOAD-7 is the secret line.\n")])
    answer_in(dc, FLAT, "QA-FILE-IN-OK")
    check([c for c in dc.calls if c["path"].startswith("/attachments/")], "the attachment was never fetched")
    raw = model.asked("qa-file-in")[0]["raw"]
    check("notes.txt" in raw or "QA-ATTACHED-PAYLOAD-7" in raw, "the turn never heard of notes.txt")
    qa.note("the note's words %s the prompt" % ("are in" if "QA-ATTACHED-PAYLOAD-7" in raw else "are not in"))


def file_out(qa):
    """A file the turn hands its answer rides the answer's own message."""
    dc, model = qa.dc, qa.model
    path = os.path.join(qa.home, "work", "qa-report.txt")
    form = ('(progn (with-open-file (s "%s" :direction :output :if-exists :supersede) '
            '(write-string "QA-FILE-BODY 42" s)) '
            '(nck:answer-file "%s" :platform "discord" :channel "%s"))' % (path, path, FLAT))
    model.script("qa-file-out", calls=[form], text="QA-FILE-OUT here is the report.")
    dc.say("mike", "qa-file-out send me the report", channel=FLAT)
    answer = answer_in(dc, FLAT, "QA-FILE-OUT")
    files = answer.get("attachments") or []
    check([f["filename"] for f in files] == ["qa-report.txt"], "the answer carries %s" % files)
    with open(os.path.join(dc.files_dir, files[0]["id"]), "rb") as f:
        check(f.read() == b"QA-FILE-BODY 42", "the uploaded file's bytes differ")


def voice_replies(qa):
    """/voice on answers a voice note with a voice message under the words,
    /voice tts every ask, /voice off none: each a flagged, drawn Ogg Opus
    reply to the ask."""
    dc, model, dm = qa.dc, qa.model, DMS["mike"]

    def voice_after(mid):
        return [m for m in dc.bot_messages(dm, after=mid) if m.get("flags", 0) & VOICE_MESSAGE]

    def mode(word, needle):
        rec = dc.interactions[dc.slash("mike", "voice", args=word, channel=dm)]
        dc.wait(lambda: rec.get("edits"), "/voice %s answered" % word)
        text = dc.messages[rec["original"]]["content"]
        check(needle in text, "/voice %s answered %r" % (word, text[:200]))

    mode("on", "said in a voice message is answered in one")
    # A voice note, as Discord delivers one: flagged, one Ogg recording that
    # declares its length and waveform, no words; the transcriber hears it.
    model.transcript = "qa-voice-note what time is it"
    model.script("qa-voice-note", text="QA-VOICE-NOTE-OK it is noon.")
    with dc.cv:
        aid = dc.snowflake()
        with open(os.path.join(dc.files_dir, aid), "wb") as out:
            out.write(b"OggS" + bytes(200))
        note = dc.post(dm, user("mike"), "", flags=VOICE_MESSAGE, attachments=[{
            "id": aid, "filename": "voice-message.ogg", "size": 204, "content_type": "audio/ogg",
            "duration_secs": 2.0, "waveform": base64.b64encode(bytes(256)).decode(),
            "url": "http://127.0.0.1:%d/attachments/%s/%s/voice-message.ogg" % (dc.port, dm, aid)}])
    dc.gateway_message(note)
    answer = answer_in(dc, dm, "QA-VOICE-NOTE-OK")
    voice = dc.wait(lambda: voice_after(note["id"]), "a voice message answering the voice note")[0]
    check(int(voice["id"]) > int(answer["id"]), "the voice message came before the words")
    check((voice.get("message_reference") or {}).get("message_id") == note["id"],
          "the voice message answers %s, not the voice note" % voice.get("message_reference"))
    att = voice["attachments"]
    check(len(att) == 1 and att[0]["filename"] == "voice-message.ogg" and not voice["content"],
          "the voice message carries %r and %s" % (voice["content"], att))
    check(att[0]["content_type"] == "audio/ogg", "the recording is uploaded as %s" % att[0]["content_type"])
    with open(os.path.join(dc.files_dir, att[0]["id"]), "rb") as f:
        data = f.read()
    check(data[:4] == b"OggS" and b"OpusHead" in data, "the recording is not Ogg Opus")
    bars = base64.b64decode(att[0]["waveform"])
    check(len(bars) == 256 and max(bars) == 255 and bars[0] < bars[-1],
          "the waveform does not draw the swelling tone: %s" % list(bars[:8]))
    check(1.0 < att[0]["duration_secs"] < 2.0, "it says it plays %ss" % att[0]["duration_secs"])
    check(model.spoken and "it is noon" in model.spoken[-1].get("input", ""),
          "the speech API was asked to say %s" % model.spoken[-1:])
    # On: a typed ask is answered in words alone.
    model.script("qa-voice-typed", text="QA-VOICE-TYPED-OK words only.")
    typed = dc.say("mike", "qa-voice-typed", channel=dm, mention=False)
    answer_in(dc, dm, "QA-VOICE-TYPED-OK")
    dc.still(lambda: voice_after(typed["id"]), "a typed ask got a voice message under /voice on", 3)
    mode("tts", "every answer here comes with a voice message")
    model.script("qa-voice-tts", text="QA-VOICE-TTS-OK said too.")
    typed = dc.say("mike", "qa-voice-tts", channel=dm, mention=False)
    answer_in(dc, dm, "QA-VOICE-TTS-OK")
    dc.wait(lambda: voice_after(typed["id"]), "a voice message under /voice tts")
    mode("off", "words alone")
    model.script("qa-voice-off", text="QA-VOICE-OFF-OK words again.")
    typed = dc.say("mike", "qa-voice-off", channel=dm, mention=False)
    answer_in(dc, dm, "QA-VOICE-OFF-OK")
    dc.still(lambda: voice_after(typed["id"]), "a voice message under /voice off", 3)
    qa.note("%.2fs, %d bytes of Ogg Opus" % (att[0]["duration_secs"], len(data)))


def voice_join(qa):
    """With nothing about voice configured, /voice join typed by someone in no
    voice channel says to sit in one first; typed by mike sitting in #lounge,
    it seats the bot beside him, in his server, talking through #general where
    it was typed, and says so there once; /voice leave gets up. In #locked, where
    Discord will not seat the bot, the join says so and names Connect and
    Speak and the invite that grants them."""
    dc = qa.dc

    def voice(args, timeout=30):
        rec = dc.interactions[dc.slash("mike", "voice", args=args, channel=GENERAL)]
        dc.wait(lambda: rec.get("edits"), "/voice %s answered" % args, timeout=timeout)
        return dc.messages[rec["original"]]["content"]

    intents = next(c[1] for c in reversed(dc.connects) if c[0] == "identify")
    check(intents & VOICE_STATES, "identify left out the voice states intent (intents %s)" % intents)
    said = voice("join")
    check("sit in a voice channel first" in said, "/voice join from nobody in voice said %r" % said)
    check(not dc.voice_ops, "the bot asked to sit with nobody to sit beside: %s" % dc.voice_ops)

    dc.sit("mike", LOUNGE)
    said = voice("join")
    check("voice: joining <#%s>" % LOUNGE in said and "answer here" in said,
          "/voice join beside mike said %r" % said)
    op = dc.voice_ops[-1]
    check(op.get("guild_id") == GUILD and op.get("channel_id") == LOUNGE, "the bot asked for %s" % op)
    check(dc.voice.get(BOT) == LOUNGE, "the bot is not seated in #lounge: %s" % dc.voice)
    joined = [m for m in dc.bot_messages(GENERAL) if "voice: joining" in m["content"]]
    check(len(joined) == 1, "#general was told the bot sat down %d times" % len(joined))
    status = voice("status")
    check("in <#%s>, talking through <#%s>" % (LOUNGE, GENERAL) in status, "/voice status said %r" % status)
    said = voice("leave")
    check("voice: leaving" in said, "/voice leave said %r" % said)
    # The gateway lap sends what the bot asks for within half a second.
    dc.wait(lambda: BOT not in dc.voice and dc.voice_ops[-1].get("channel_id") is None,
            "the bot got up from #lounge")

    dc.sit("mike", LOCKED)
    began = time.time()
    said = voice("join", timeout=40)
    waited = time.time() - began
    check("Discord did not seat me in <#%s>" % LOCKED in said and "Connect and Speak" in said
          and "client_id=%s" % APP in said, "/voice join in #locked said %r" % said)
    check(BOT not in dc.voice, "the bot sits in %s" % dc.voice.get(BOT))
    dc.sit("mike", None)
    qa.note("beside mike in #lounge through #general, and up again; #locked refused after %.0f s" % waited)


def voice_card(qa):
    """/voice bare is a card in the room, an embed: grey, how to start, Join
    and Refresh, and a menu of the channel's voice messages showing Words
    only. With mike in #lounge, Join seats the bot and the card, edited in
    place, turns green with where it sits, Leave and Take notes; picking Voice
    for voice notes moves the channel and the menu shows it; Leave gets up and
    the card offers Join again."""
    dc = qa.dc

    def buttons(card):
        return [c.get("custom_id") for row in card["components"] for c in row["components"]
                if c.get("type") == 2]

    def current(card):
        menu = next(c for row in card["components"] for c in row["components"] if c.get("type") == 3)
        return [o["value"] for o in menu["options"] if o.get("default")]

    def embed(card):
        check(not card["content"] and len(card.get("embeds") or []) == 1,
              "the card is not one embed and no words: %r" % card["content"][:200])
        return card["embeds"][0]

    def field(card, name):
        return next((f["value"] for f in embed(card).get("fields") or [] if f["name"] == name), None)

    rec = dc.interactions[dc.slash("mike", "voice", channel=GENERAL)]
    dc.wait(lambda: rec.get("edits"), "the voice card")
    card = dc.messages[rec["original"]]
    check(embed(card).get("title") == "Voice" and "press Join" in embed_text(card),
          "the card reads %r" % embed_text(card)[:200])
    check("~" not in embed_text(card), "a format directive leaked into the card: %r" % embed_text(card))
    check(embed(card).get("color") == 0x80848E, "an unseated card is not grey: %s" % embed(card).get("color"))
    check(buttons(card) == ["nck:say:/voice join", "nck:say:/voice"], "the card's buttons: %s" % buttons(card))
    check(current(card) == ["nck:say:/voice off"], "the menu shows %s" % current(card))
    check(field(card, "Voice messages here") == "Words only", "the card's mode: %s" % embed(card))

    dc.sit("mike", LOUNGE)
    press = dc.press("mike", card, custom_id="nck:say:/voice join")
    dc.wait(lambda: field(card, "Sitting in") == "<#%s>" % LOUNGE, "the card showing the seat")
    check(dc.interactions[press]["callbacks"][0].get("type") == 6, "Join did not edit the card in place")
    check("joining" in embed_text(card), "the card does not say it joined: %r" % embed_text(card)[:200])
    check(embed(card).get("color") == 0x23A55A, "a seated card is not green: %s" % embed(card).get("color"))
    check(field(card, "Talking through") == "<#%s>" % GENERAL, "the card's room: %s" % embed(card))
    check(buttons(card) == ["nck:say:/voice leave", "nck:say:/voice notes", "nck:say:/voice"],
          "the seated card's buttons: %s" % buttons(card))
    dc.press("mike", card, value="nck:say:/voice on")
    dc.wait(lambda: current(card) == ["nck:say:/voice on"], "the menu showing Voice for voice notes")
    check("in one too" in embed_text(card), "the card does not say what on does: %r" % embed_text(card)[:200])
    dc.press("mike", card, custom_id="nck:say:/voice leave")
    dc.wait(lambda: "press Join" in embed_text(card), "the card offering Join again")
    dc.wait(lambda: BOT not in dc.voice, "the bot got up from #lounge")
    dc.press("mike", card, value="nck:say:/voice off")
    dc.wait(lambda: current(card) == ["nck:say:/voice off"], "the menu back on Words only")
    dc.sit("mike", None)
    qa.note("an embed, grey then green; Join, Voice for voice notes, Leave and Words only, each in place")


def help_command(qa):
    """/help answers the person who asked with the command list."""
    dc = qa.dc
    token = dc.slash("mike", "help", channel=FLAT)
    rec = dc.interactions[token]
    dc.wait(lambda: rec.get("edits"), "the /help answer")
    check(rec["callbacks"][0].get("type") == 5, "/help was not held with a thinking message")
    text = dc.messages[rec["original"]]["content"]
    for name in ("/models", "/stop", "/channels"):
        check(name in text, "/help leaves out %s: %r" % (name, text[:300]))


def every_command(qa):
    """Every command in the menu, run bare by the owner, answers -- in words
    Discord accepts, or a card's embed -- never a stuck "thinking"."""
    dc = qa.dc
    answered = []
    for name in sorted(c["name"] for c in dc.commands):
        token = dc.slash("mike", name, channel=FLAT)
        rec = dc.interactions[token]
        dc.wait(lambda: rec.get("edits"), "an answer to /%s" % name, timeout=60)
        msg = dc.messages[rec["original"]]
        text = msg["content"] or embed_text(msg)
        check(text.strip(), "/%s answered nothing" % name)
        check(not re.search(r"is not of type|unbound|undefined function", text),
              "/%s answered a fault: %r" % (name, text[:200]))
        answered.append(name)
    qa.note("%d commands: %s" % (len(answered), " ".join(answered)))


def dm_pairing(qa):
    """A DM from someone no list names gets a pairing code; the owner's
    /channels pair CODE lets them in, and their next DM is answered."""
    dc, model = qa.dc, qa.model
    model.script("qa-dm", text="QA-DM-OK welcome.")
    dc.say("kim", "hello qa-dm-first", channel=DMS["kim"], mention=False)
    notice = dc.wait(lambda: next((m for m in dc.bot_messages(DMS["kim"]) if "pairing code" in m["content"]), None),
                     "a pairing code in kim's DM")
    code = re.search(r"pairing code is ([A-Z0-9]+)", notice["content"])
    check(code, "no code in %r" % notice["content"])
    check(not [r for r in model.requests if "qa-dm-first" in r["raw"]], "the stranger's DM reached the model")
    token = dc.slash("mike", "channels", args="pair " + code.group(1), channel=FLAT)
    dc.wait(lambda: dc.interactions[token].get("edits"), "the pair answer")
    check(dc.interactions[token]["callbacks"][0].get("data", {}).get("flags", 0) & 64,
          "/channels pair was not private")
    dc.say("kim", "qa-dm am I in now", channel=DMS["kim"], mention=False)
    answer_in(dc, DMS["kim"], "QA-DM-OK")


def reconnect(qa):
    """A connection Discord closes is resumed on its session, and the bot
    answers after it."""
    dc, model = qa.dc, qa.model
    resumes = len([c for c in dc.connects if c[0] == "resume"])
    dc.drop(4000)
    dc.wait(lambda: len([c for c in dc.connects if c[0] == "resume"]) > resumes, "a resume", timeout=60)
    model.script("qa-after-drop", text="QA-AFTER-DROP-OK")
    dc.say("mike", "qa-after-drop still there", channel=FLAT)
    answer_in(dc, FLAT, "QA-AFTER-DROP-OK")


def catch_up(qa):
    """An ask made while the bot was down is answered once it is back."""
    dc, model = qa.dc, qa.model
    model.script("qa-missed", text="QA-MISSED-OK sorry for the wait.")
    identifies = len([c for c in dc.connects if c[0] == "identify"])
    qa.bot.stop()
    ask = dc.say("mike", "qa-missed are you up", channel=FLAT)
    qa.bot.start()
    connected(dc, identifies)
    answer = answer_in(dc, FLAT, "QA-MISSED-OK", timeout=90)
    check((answer.get("message_reference") or {}).get("message_id") == ask["id"],
          "the late answer is no reply to the missed ask")


def release_note(qa):
    """A release the bot runs for the first time is said once in its home
    channel, in the model's words, with the restart that brought it: its
    version, and what changed since the release this home ran before, from
    the list the publisher writes beside the archives. The
    release host is a folder whose channel names the release the bot runs, so
    nothing is staged; the release before is written into the store."""
    dc, bot, home = qa.dc, qa.bot, qa.home
    token = dc.slash("mike", "sethome", channel=FLAT)
    dc.wait(lambda: dc.interactions[token].get("edits"), "the /sethome answer")
    version = subprocess.run([bot.binary, "--version"], env=bot.env, cwd=home, capture_output=True,
                             text=True, timeout=120).stdout.split()[-1]
    label = version.split("+", 1)[-1]
    host = os.path.join(home, "release-host")
    os.makedirs(host, exist_ok=True)
    arch = "arm64" if os.uname().machine in ("aarch64", "arm64") else "x86_64"
    with open(os.path.join(host, "stable-%s-linux" % arch), "w") as out:
        out.write(label + "\n")
    with open(os.path.join(host, "changes-" + label), "w") as out:
        out.write("%s qa: the release itself\nqamid0001 qa: a change before it\nqamid0002\n"
                  "qaold0001 qa: the release before\nqaold0000 qa: older still\n" % label)
    identifies = len([c for c in dc.connects if c[0] == "identify"])
    bot.stop()
    path = os.path.join(home, ".nodecode", "config.jsonc")
    with open(path) as f:
        config = json.load(f)
    config["update"] = {"mode": "ask", "url": "file://" + host, "feed": "off", "check_minutes": 1440}
    with open(path, "w") as f:
        json.dump(config, f, indent=1)
    db = sqlite3.connect(os.path.join(home, ".nodecode", "history.db"))
    with db:
        now, value = time.strftime("%Y-%m-%dT%H:%M:%S.000Z", time.gmtime()), json.dumps({"label": "qaold0001"})
        ver = db.execute("insert into session_state_events (session_id, ws, key, value_json, log_seq, occurred_at)"
                         " values ('gateway', 'main', 'release', ?, 0, ?)", (value, now)).lastrowid
        db.execute("insert into session_state (session_id, ws, key, ver, value_json, updated_log_seq, updated_at)"
                   " values ('gateway', 'main', 'release', ?, ?, 0, ?) on conflict (session_id, ws, key) do update"
                   " set ver = excluded.ver, value_json = excluded.value_json, updated_at = excluded.updated_at",
                   (ver, value, now))
    db.close()
    bot.start()
    connected(dc, identifies)
    note = dc.wait(lambda: next((m for m in dc.bot_messages(FLAT) if "nodecode updated to" in m["content"]), None),
                   "the release note in the home channel", timeout=120)
    text = note["content"]
    check(text.startswith("QA-SAID nodecode is running again"),
          "the home channel heard the note itself, not the model's words of it with the restart: %r" % text)
    check("nodecode updated to %s (from qaold0001)" % version in text,
          "the note does not name the version: %r" % text)
    for line in ("- qa: the release itself", "- qa: a change before it"):
        check(line in text, "the note leaves out %r: %r" % (line, text))
    check("the release before" not in text and "qamid0002" not in text,
          "the note reaches past the release before, or names a change that shipped nothing: %r" % text)
    qa.note(text.splitlines()[0])


def back_brief(qa):
    """A restart that cuts a turn off is back in the home channel in the
    model's words of the boot's note -- how long it was gone, the turn it
    picks up -- and the turn resumes with the model told to say it is back
    before it carries on."""
    dc, model, bot = qa.dc, qa.model, qa.bot
    token = dc.slash("mike", "sethome", channel=FLAT)
    dc.wait(lambda: dc.interactions[token].get("edits"), "the /sethome answer")
    model.script("qa-cutoff", calls=["(+ 1 2)"], stall=120, title="Keep the cutoff going")
    ask = dc.say("mike", "qa-cutoff keep at it until the restart", channel=FLAT)
    dc.wait(lambda: len(model.asked("qa-cutoff")) >= 2, "the round the restart cuts off", timeout=40)
    identifies = len([c for c in dc.connects if c[0] == "identify"])
    bot.stop()
    model.script("qa-cutoff", text="QA-CUTOFF-OK done.", title="Keep the cutoff going")
    cut = len(model.requests)
    bot.start()
    connected(dc, identifies)
    brief = dc.wait(lambda: next((m for m in dc.bot_messages(FLAT, ask["id"])
                                  if m["content"].startswith("QA-SAID nodecode is running again")
                                  and "It picks up the turn" in m["content"]), None),
                    "the model's words of the note in the home channel", timeout=60)
    check('"Keep the cutoff going"' in brief["content"],
          "the brief does not name the turn by its title: %r" % brief["content"])
    answer_in(dc, FLAT, "QA-CUTOFF-OK", timeout=90)
    told = [r for r in model.requests[cut:] if r["key"] == "qa-cutoff" and r["tools"]]
    check(told and "you are back and what you were doing" in told[0]["raw"],
          "the resumed turn's first request does not tell the model it is back")
    qa.note(brief["content"])


SCENARIOS = [canary, follow_up, mention_gate, stranger, edit_wakes, reply_in_place, commentary,
             streaming, long_answer, long_thread_answer,
             failed_turn, stop_button, card, card_pictures, clarify, models_picker, thread_reset, agents,
             completions,
             file_in, file_out,
             voice_replies, voice_join, voice_card, help_command, every_command, dm_pairing, reconnect,
             catch_up, release_note, back_brief]


def name_of(fn):
    return fn.__name__.replace("_", "-")


class QA:
    def __init__(self, dc, model, bot, home):
        self.dc, self.model, self.bot, self.home = dc, model, bot, home
        self.thread = None
        self.notes = []

    def note(self, text):
        self.notes.append(text)


def show(dc, n=14):
    with dc.cv:
        lines = []
        for mid in dc.order[-n:]:
            m = dc.messages[mid]
            where = dc.channels[m["channel_id"]].get("name") or m["channel_id"][-4:]
            lines.append("    %s %-8s %-9s %s%s%s" % (
                mid[-5:], where[:8], m["author"]["username"][:9],
                "[deleted] " if m.get("deleted") else "",
                m["content"][:90].replace("\n", " | "),
                " %s" % m.get("my_reactions") if m.get("my_reactions") else ""))
        for c in dc.unknown[-5:]:
            lines.append("    unknown call: %s /%s" % (c["method"], c["path"]))
        for c in dc.refused[-5:]:
            lines.append("    refused: %s /%s %s" % (c["method"], c["path"], c["answer"]))
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("binary")
    parser.add_argument("--only", default="", help="comma-separated scenario names")
    parser.add_argument("--list", action="store_true", help="name the scenarios and stop")
    parser.add_argument("--keep", action="store_true", help="leave the scratch home behind")
    args = parser.parse_args()
    if args.list:
        for fn in [boot] + SCENARIOS:
            print("%-16s %s" % (name_of(fn), " ".join(fn.__doc__.split())))
        return 0
    chosen = [s for s in args.only.split(",") if s]
    unknown = [s for s in chosen if s not in map(name_of, SCENARIOS)]
    if unknown:
        die("no scenario named " + ", ".join(unknown))
    binary = os.path.abspath(args.binary)
    if not os.access(binary, os.X_OK):
        die("no executable at " + binary)
    home = tempfile.mkdtemp(prefix="nodecode-discord-qa-")
    os.makedirs(os.path.join(home, ".nodecode"))
    os.makedirs(os.path.join(home, "work"))
    os.makedirs(os.path.join(home, "discord-files"))
    scribe = os.path.join(home, "scribe")
    os.makedirs(scribe)
    for name, text in (("AGENTS.md", "QA-SCRIBE-RULES: keep the minutes."),
                       ("SOUL.md", "QA-SCRIBE-SOUL: you are the scribe.")):
        with open(os.path.join(scribe, name), "w") as out:
            out.write(text + "\n")
    dc, model = Discord(os.path.join(home, "discord-files")), Model()
    for server in (dc, model):
        threading.Thread(target=server.serve_forever, daemon=True).start()
    env = {k: v for k, v in os.environ.items()
           if k not in ("NODECODE_HOME", "XDG_CACHE_HOME", "XDG_CONFIG_HOME", "XDG_DATA_HOME")
           and not k.startswith("DISCORD_")}
    env.update(HOME=home, XDG_CACHE_HOME=os.path.join(home, ".cache"), LANG="C.UTF-8",
               DISPLAY="", WAYLAND_DISPLAY="", DISCORD_BOT_TOKEN="qa-token-not-a-real-one",
               QA_SPEECH_KEY="qa-speech")
    mock = {"base_url": "http://127.0.0.1:%d/v1" % model.server_port, "sdk": "openai-completions",
            "api_key": "discord-qa", "models": [{"id": "mock-fast", "context_window": 200000},
                                                {"id": "mock-deep", "context_window": 200000}]}
    with open(os.path.join(home, ".nodecode", "config.jsonc"), "w") as config:
        json.dump({"update": {"mode": "off"},
                   "general": {"default_model": {"provider": "mock", "model": "mock-fast"},
                               "auxiliary_model": {"provider": "mock", "model": "mock-fast"}},
                   "speech": {"base_url": mock["base_url"], "model": "qa-voice", "voice": "qa",
                              "api_key_env": "QA_SPEECH_KEY"},
                   "transcription": {"base_url": mock["base_url"], "model": "qa-ears"},
                   "providers": {"mock": mock,
                                 "lab": dict(mock, models=[{"id": "mock-fast", "context_window": 200000}])},
                   "channels": {"discord": {
                       "bot_token_env": "DISCORD_BOT_TOKEN",
                       "api_base": "http://127.0.0.1:%d/api/v10" % dc.port,
                       "gateway_url": "ws://127.0.0.1:%d/gateway" % dc.port,
                       "allowed_users": [USERS["mike"]], "owner": [USERS["mike"]],
                       "pairing": True, "require_mention": True, "reactions": True,
                       "thread_behavior": "create_per_message", "flat_channels": [FLAT],
                       "catch_up_minutes": 60,
                       "agents": {"scribe": {"folder": scribe, "soul_file": os.path.join(scribe, "SOUL.md"),
                                             "prompt": "QA-SCRIBE-PROMPT: write it down."}},
                       "routes": [{"agent": "scribe", "channel": FLAT}]}}}, config, indent=1)
    bot = Bot(binary, home, env)
    failed = []
    try:
        # Every shipped cell, as /setup installs them: the menu a person's
        # bot puts up, cells' commands with it.
        form = ('(let ((root (merge-pathnames ".nodecode/cells/" (user-homedir-pathname)))) '
                '(dolist (entry (nlk::shipped-cells)) (nlk::install-cell (nlk:offer-name entry) :root root)) '
                '(length (directory (merge-pathnames "*/" root))))')
        out = subprocess.run([binary, "trial"], input=form + "\n", env=env, cwd=home,
                             capture_output=True, text=True, timeout=300).stdout
        if not all(os.path.isdir(os.path.join(home, ".nodecode", "cells", name))
                   for name in ("nodecode-channel-kit", "nodecode-channel-discord")):
            die("the channel cells did not install:\n" + out[-1500:])
        bot.start()
        started = time.time()
        try:
            connected(dc)
        except Failure as failure:
            die("%s\n%s" % (failure, bot.tail()))
        qa = QA(dc, model, bot, home)
        print("discord-qa: bot pid %d on Discord :%d, model :%d, identified in %.0fs"
              % (bot.proc.pid, dc.port, model.server_port, time.time() - started))
        for fn in [boot] + [s for s in SCENARIOS if not chosen or name_of(s) in chosen]:
            qa.notes = []
            began, refused = time.time(), len(dc.refused)
            try:
                if not bot.alive():
                    raise Failure("the bot exited:\n" + bot.tail())
                fn(qa)
                check(len(dc.refused) == refused, "Discord would have refused: %s" % dc.refused[refused:])
                print("  ok    %-15s %5.1fs  %s" % (name_of(fn), time.time() - began, "; ".join(qa.notes)))
            except Failure as failure:
                failed.append(name_of(fn))
                print("  FAIL  %-15s %5.1fs  %s\n%s" % (name_of(fn), time.time() - began, failure, show(dc)))
            except Exception:
                failed.append(name_of(fn))
                print("  FAIL  %-15s crashed:\n%s" % (name_of(fn), traceback.format_exc()))
        if dc.unknown:
            print("discord-qa: calls the stand-in had no answer for: "
                  + ", ".join(sorted({"%s /%s" % (c["method"], c["path"]) for c in dc.unknown})))
        if failed:
            print("\ndiscord-qa: FAILED: " + ", ".join(failed))
        else:
            print("\ndiscord-qa: every scenario held")
        return 1 if failed else 0
    finally:
        bot.stop()
        for server in (dc, model):
            server.shutdown()
        if args.keep or failed:
            with open(os.path.join(home, "discord-state.json"), "w") as out:
                json.dump({"messages": [dc.messages[i] for i in dc.order], "calls": dc.calls,
                           "channels": dc.channels, "model": model.requests}, out, indent=1, default=str)
            print("discord-qa: scratch home kept at %s (serve.log, discord-state.json)" % home)
        else:
            shutil.rmtree(home, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
