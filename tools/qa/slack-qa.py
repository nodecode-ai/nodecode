#!/usr/bin/env python3
"""slack-qa.py -- the Slack cell driven end to end, the way people in a
workspace drive it.

Usage: slack-qa.py BINARY [--only NAME[,NAME...]] [--list] [--keep]

BINARY is the nodecode executable under proof. It serves headless (`serve') in
a scratch home -- updates off, every shipped cell installed as /setup
installs them -- against two stand-ins played here, on 127.0.0.1 and nowhere
else:

  - Slack: the Web API the adapter calls (auth.test, chat.postMessage and its
    edits, ephemerals, reactions by name, the three upload steps,
    conversations.replies and .history, users.info, chat.getPermalink, the
    status line) and the Socket Mode link apps.connections.open names, which
    carries events, slash commands and presses and takes every
    acknowledgement; a command's or a press's response_url. It holds the
    conversations the way Slack does: mike's and kim's DMs with the bot,
    #general and #board; mike the owner, kim whom no list names. What Slack
    refuses it refuses too: blocks past Slack's limits, a reaction by glyph,
    an edit of a message that is gone or not the bot's.
  - the model: discord-qa.py's chat provider, the same scripts keyed on a
    word in the ask.

Each scenario says something the way a person would and holds the bot to
what that person would then see; the scenarios run in order, on one bot,
which `catch-up' restarts. --only runs the named ones (the boot always
runs). Exits 1 when a scenario fails, printing what the conversations held;
2 when the proof cannot run.
"""

import argparse
import importlib.util
import http.server
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import time
import traceback
import urllib.parse

# The model stand-in, the bot under proof, the checks and the WebSocket are
# discord-qa.py's: one chat provider for every channel's QA.
_spec = importlib.util.spec_from_file_location(
    "discord_qa", os.path.join(os.path.dirname(os.path.abspath(__file__)), "discord-qa.py"))
dq = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(dq)
Failure, check, Model, Bot = dq.Failure, dq.check, dq.Model, dq.Bot

BOT_TOKEN, APP_TOKEN = "xoxb-qa-not-a-real-token", "xapp-qa-not-a-real-token"
TEAM, APP, BOT_USER, BOT_ID = "T0QA", "A0QA", "U0BOT", "B0BOT"
USERS = {"mike": "U0MIKE", "kim": "U0KIM"}
DMS = {"mike": "D0MIKE", "kim": "D0KIM"}
GENERAL, BOARD = "C0GENERAL", "C0BOARD"
CONVERSATIONS = {GENERAL: "general", BOARD: "board", DMS["mike"]: None, DMS["kim"]: None}
BLOCKS = {"markdown", "section", "actions", "context", "image", "header", "divider", "rich_text"}
EMOJI_NAME = re.compile(r"^[a-z0-9_+'-]+$")


def die(message, code=2):
    print("slack-qa: " + message, file=sys.stderr)
    sys.exit(code)


def words_of(message):
    """What a message the bot sent says: its markdown, its sections' and its
    small print's words, else its notification text."""
    parts = []
    for block in message.get("blocks") or []:
        kind = block.get("type")
        if kind == "markdown":
            parts.append(block.get("text") or "")
        elif kind == "section":
            parts.append((block.get("text") or {}).get("text") or "")
        elif kind == "context":
            parts.extend(e.get("text") or "" for e in block.get("elements") or [] if isinstance(e, dict))
    return "\n".join(p for p in parts if p) or message.get("text") or ""


def buttons_of(message):
    out = []
    for block in message.get("blocks") or []:
        if block.get("type") == "actions":
            out.extend(e for e in block.get("elements") or [] if e.get("type") == "button")
        elif block.get("type") == "section" and (block.get("accessory") or {}).get("type") == "button":
            out.append(block["accessory"])
    return out


def blocks_problem(blocks):
    """Why Slack would refuse BLOCKS (invalid_blocks), or None."""
    if not isinstance(blocks, list):
        return "blocks is not an array"
    if len(blocks) > 50:
        return "%d blocks, past 50" % len(blocks)
    markdown = 0
    for block in blocks:
        kind = block.get("type")
        if kind not in BLOCKS:
            return "a %r block" % kind
        if kind == "markdown":
            markdown += len(block.get("text") or "")
        elif kind == "actions":
            elements = block.get("elements") or []
            if not 0 < len(elements) <= 25:
                return "an actions block of %d elements" % len(elements)
            for e in elements:
                if e.get("type") == "button":
                    if len((e.get("text") or {}).get("text") or "") > 75:
                        return "a button's words past 75 characters"
                    if len(e.get("value") or "") > 2000:
                        return "a button's value past 2000 characters"
                elif e.get("type") == "static_select":
                    options = e.get("options") or []
                    if not 0 < len(options) <= 100:
                        return "a select of %d options" % len(options)
                    if any(len(o.get("value") or "") > 150 for o in options):
                        return "an option's value past 150 characters"
        elif kind == "context":
            if any(len(e.get("text") or "") > 3000 for e in block.get("elements") or []):
                return "small print past 3000 characters"
        elif kind == "image":
            source = block.get("slack_file") or {}
            if not (block.get("image_url") or source.get("id") or source.get("url")):
                return "an image with no source"
            if not block.get("alt_text") or len(block["alt_text"]) > 2000:
                return "an image's alt_text missing or past 2000 characters"
    if markdown > 12000:
        return "markdown of %d characters, past 12000" % markdown
    return None


class Slack(http.server.ThreadingHTTPServer):
    """The Web API and the Socket Mode link, for one app in one workspace."""
    daemon_threads = True

    def __init__(self):
        super().__init__(("127.0.0.1", 0), SlackHandler)
        self.port = self.server_address[1]
        self.cv = threading.Condition()
        self.messages = {}        # (channel, ts) -> message
        self.order = []           # (channel, ts), in the order they were made
        self.calls = []           # (method, args)
        self.unknown = []
        self.refused = []         # what Slack would have refused
        self.sockets = []
        self.opened = 0           # apps.connections.open answers
        self.acks = set()
        self.ephemerals = []
        self.responses = {}       # response_url id -> what was posted to it
        self.targets = {}         # response_url id -> (channel, ts or None, user)
        self.uploads = {}         # file id -> {name, size, shared}
        self.reactions = {}       # (channel, ts) -> the names the bot left
        self.clock = 0
        self.serial = 0

    def next(self):
        with self.cv:
            self.serial += 1
            return self.serial

    def ts(self, ago=0):
        with self.cv:
            self.clock += 1
            return "%d.%06d" % (int(time.time() - ago), self.clock)

    def keep(self, message):
        with self.cv:
            self.messages[(message["channel"], message["ts"])] = message
            self.order.append((message["channel"], message["ts"]))
            self.cv.notify_all()

    # --- the people's side ---------------------------------------------------

    def envelope(self, kind, payload):
        eid = "env-%d" % self.next()
        with self.cv:
            sockets = [s for s in self.sockets if s.open]
        frame = {"envelope_id": eid, "type": kind, "payload": payload,
                 "accepts_response_payload": kind == "slash_commands"}
        for s in sockets:
            s.send(frame)
        return eid

    def event(self, event):
        return self.envelope("events_api", {"type": "event_callback", "team_id": TEAM, "api_app_id": APP,
                                            "event_id": "Ev%d" % self.next(), "event_time": int(time.time()),
                                            "event": event})

    def say(self, who, text, channel, thread=None, ago=0):
        """WHO says TEXT in CHANNEL, in the thread THREAD starts, AGO seconds
        back; the socket carries it when one is open. => the message."""
        message = {"type": "message", "channel": channel, "user": USERS[who], "text": text,
                   "ts": self.ts(ago), "team": TEAM, "user_team": TEAM,
                   "channel_type": "im" if channel.startswith("D") else "channel"}
        if thread:
            root = self.messages.get((channel, thread)) or {}
            message.update(thread_ts=thread, parent_user_id=root.get("user"))
        self.keep(dict(message, people=True))
        self.event(message)
        return message

    def edit(self, message, text):
        edited = {k: v for k, v in message.items() if k not in ("people", "channel", "channel_type")}
        edited.update(text=text, edited={"user": message["user"], "ts": self.ts()})
        with self.cv:
            self.messages[(message["channel"], message["ts"])].update(text=text)
        self.event({"type": "message", "subtype": "message_changed", "hidden": True,
                    "channel": message["channel"], "channel_type": message["channel_type"],
                    "ts": self.ts(), "message": edited,
                    "previous_message": {k: v for k, v in message.items() if k in ("type", "user", "text", "ts")}})

    def react(self, who, message, name):
        self.event({"type": "reaction_added", "user": USERS[who], "reaction": name,
                    "item_user": message.get("user"), "event_ts": self.ts(),
                    "item": {"type": "message", "channel": message["channel"], "ts": message["ts"]}})

    def response_url(self, channel, ts, who):
        rid = "r%d" % self.next()
        with self.cv:
            self.targets[rid] = (channel, ts, USERS[who])
            self.responses[rid] = []
        return rid, "http://127.0.0.1:%d/response/%s" % (self.port, rid)

    def command(self, who, text, channel):
        """WHO types /nodecode TEXT in CHANNEL. => the response_url's id."""
        rid, url = self.response_url(channel, None, who)
        self.envelope("slash_commands", {"token": "qa", "team_id": TEAM, "team_domain": "qa",
                                         "channel_id": channel, "channel_name": CONVERSATIONS.get(channel) or "directmessage",
                                         "user_id": USERS[who], "user_name": who, "command": "/nodecode",
                                         "text": text, "api_app_id": APP, "response_url": url,
                                         "trigger_id": "trig%d" % self.next()})
        return rid

    def press(self, who, message, button):
        """WHO presses BUTTON on MESSAGE. => the response_url's id."""
        rid, url = self.response_url(message["channel"], message["ts"], who)
        container = {"type": "message", "message_ts": message["ts"], "channel_id": message["channel"],
                     "is_ephemeral": False}
        if message.get("thread_ts"):
            container["thread_ts"] = message["thread_ts"]
        self.envelope("interactive", {
            "type": "block_actions", "api_app_id": APP, "token": "qa", "trigger_id": "trig%d" % self.next(),
            "user": {"id": USERS[who], "username": who, "name": who, "team_id": TEAM},
            "team": {"id": TEAM, "domain": "qa"}, "container": container,
            "channel": {"id": message["channel"], "name": CONVERSATIONS.get(message["channel"]) or "directmessage"},
            "message": {k: v for k, v in message.items() if k in ("type", "user", "bot_id", "ts", "text", "blocks", "thread_ts")},
            "response_url": url,
            "actions": [{"type": "button", "action_id": button.get("action_id"), "block_id": "b1",
                         "text": button.get("text"), "value": button.get("value"), "action_ts": self.ts()}]})
        return rid

    # --- reading what the bot did ---------------------------------------------

    def bot_messages(self, channel):
        with self.cv:
            return [m for key in self.order for m in [self.messages[key]]
                    if key[0] == channel and not m.get("people") and not m.get("deleted")]

    def called(self, method):
        with self.cv:
            return [args for name, args in self.calls if name == method]

    def live_sockets(self):
        with self.cv:
            return [s for s in self.sockets if s.open]

    def wait(self, probe, what, timeout=30.0):
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
        deadline = time.time() + seconds
        with self.cv:
            while time.time() < deadline:
                found = probe()
                if found:
                    raise Failure("%s: %s" % (what, found))
                self.cv.wait(0.25)

    # --- the Web API ------------------------------------------------------------

    def refuse(self, method, error, why):
        with self.cv:
            self.refused.append("%s %s: %s" % (method, error, why))
        return {"ok": False, "error": error}

    def call(self, method, args, token):
        with self.cv:
            self.calls.append((method, args))
            self.cv.notify_all()
        if token != (APP_TOKEN if method == "apps.connections.open" else BOT_TOKEN):
            return self.refuse(method, "invalid_auth", "the wrong token")
        ok = lambda **kw: dict(ok=True, **kw)
        if method == "auth.test":
            return ok(user_id=BOT_USER, bot_id=BOT_ID, team_id=TEAM, team="QA", user="nodecode",
                      url="https://qa.slack.com/")
        if method == "apps.connections.open":
            with self.cv:
                self.opened += 1
                return ok(url="ws://127.0.0.1:%d/link?ticket=%d&app_id=%s" % (self.port, self.opened, APP))
        if method == "chat.postMessage":
            return self.post(method, args)
        if method == "chat.update":
            return self.update(method, args)
        if method == "chat.delete":
            with self.cv:
                message = self.messages.get((args.get("channel"), args.get("ts")))
                if not message or message.get("deleted"):
                    return {"ok": False, "error": "message_not_found"}
                message["deleted"] = True
                self.cv.notify_all()
            return ok(channel=args.get("channel"), ts=args.get("ts"))
        if method == "chat.postEphemeral":
            problem = blocks_problem(args.get("blocks") or [])
            if problem:
                return self.refuse(method, "invalid_blocks", problem)
            with self.cv:
                self.ephemerals.append(args)
                self.cv.notify_all()
            return ok(message_ts=self.ts())
        if method == "chat.getPermalink":
            channel, ts = args.get("channel"), args.get("message_ts")
            if (channel, ts) not in self.messages:
                return {"ok": False, "error": "message_not_found"}
            return ok(channel=channel, permalink="https://qa.slack.com/archives/%s/p%s" % (channel, ts.replace(".", "")))
        if method in ("reactions.add", "reactions.remove"):
            return self.reaction(method, args)
        if method == "conversations.replies":
            return self.replies(args)
        if method == "conversations.history":
            return self.history(args)
        if method == "users.info":
            uid = args.get("user")
            name = next((n for n, i in USERS.items() if i == uid), "nodecode" if uid == BOT_USER else None)
            if not name:
                return {"ok": False, "error": "user_not_found"}
            return ok(user={"id": uid, "name": name, "is_bot": uid == BOT_USER,
                            "profile": {"display_name": name, "real_name": name.capitalize()}})
        if method == "users.conversations":
            return ok(channels=[{"id": c, "name": n, "is_im": n is None} for c, n in CONVERSATIONS.items()])
        if method == "users.list":
            return ok(members=[{"id": i, "name": n, "profile": {"display_name": n}} for n, i in USERS.items()]
                      + [{"id": BOT_USER, "name": "nodecode", "is_bot": True}])
        if method == "assistant.threads.setStatus":
            return ok()
        if method == "files.getUploadURLExternal":
            if not args.get("filename") or not str(args.get("length", "")).isdigit():
                return self.refuse(method, "invalid_arguments", "filename and length are required")
            fid = "F0QA%d" % self.next()
            with self.cv:
                self.uploads[fid] = {"name": args["filename"], "length": int(args["length"]), "size": None,
                                     "shared": None}
            return ok(upload_url="http://127.0.0.1:%d/upload/%s" % (self.port, fid), file_id=fid)
        if method == "files.completeUploadExternal":
            return self.complete(method, args)
        with self.cv:
            self.unknown.append(method)
        return ok()

    def post(self, method, args):
        channel = args.get("channel")
        if channel not in CONVERSATIONS:
            return self.refuse(method, "channel_not_found", channel)
        blocks = args.get("blocks") or []
        problem = blocks_problem(blocks)
        if problem:
            return self.refuse(method, "invalid_blocks", problem)
        if not (args.get("text") or blocks):
            return self.refuse(method, "no_text", "nothing to say")
        message = {"type": "message", "channel": channel, "user": BOT_USER, "bot_id": BOT_ID, "app_id": APP,
                   "text": args.get("text") or "", "blocks": blocks, "ts": self.ts(), "team": TEAM}
        if args.get("thread_ts"):
            message["thread_ts"] = args["thread_ts"]
        self.keep(message)
        # Slack hands an app its own posts as events too: the lane must not
        # take them for anyone's words.
        self.event(dict(message, channel_type="im" if channel.startswith("D") else "channel"))
        return {"ok": True, "channel": channel, "ts": message["ts"],
                "message": {k: v for k, v in message.items() if k != "channel"}}

    def update(self, method, args):
        key = (args.get("channel"), args.get("ts"))
        with self.cv:
            message = self.messages.get(key)
        if not message or message.get("deleted"):
            return {"ok": False, "error": "message_not_found"}
        if message.get("user") != BOT_USER:
            return self.refuse(method, "cant_update_message", "not the bot's message")
        blocks = args.get("blocks") or []
        problem = blocks_problem(blocks)
        if problem:
            return self.refuse(method, "invalid_blocks", problem)
        with self.cv:
            message.update(text=args.get("text") or "", blocks=blocks, edited=True)
            self.cv.notify_all()
        return {"ok": True, "channel": key[0], "ts": key[1], "text": message["text"],
                "message": {k: v for k, v in message.items() if k != "channel"}}

    def reaction(self, method, args):
        key, name = (args.get("channel"), args.get("timestamp")), args.get("name") or ""
        if not EMOJI_NAME.match(name):
            return self.refuse(method, "invalid_name", "%r is no emoji name" % name)
        with self.cv:
            if key not in self.messages:
                return {"ok": False, "error": "message_not_found"}
            names = self.reactions.setdefault(key, [])
            if method == "reactions.add":
                if name in names:
                    return {"ok": False, "error": "already_reacted"}
                names.append(name)
            else:
                if name not in names:
                    return {"ok": False, "error": "no_reaction"}
                names.remove(name)
            self.cv.notify_all()
        return {"ok": True}

    def replies(self, args):
        channel, ts = args.get("channel"), args.get("ts")
        oldest, limit = float(args.get("oldest") or 0), int(args.get("limit") or 1000)
        with self.cv:
            if (channel, ts) not in self.messages:
                return {"ok": False, "error": "thread_not_found"}
            thread = [self.messages[k] for k in self.order
                      if k[0] == channel and (k[1] == ts or self.messages[k].get("thread_ts") == ts)]
        thread.sort(key=lambda m: float(m["ts"]))
        thread = [m for m in thread if m["ts"] == ts or float(m["ts"]) > oldest][:limit]
        return {"ok": True, "has_more": False, "messages": [self.public(m) for m in thread]}

    def history(self, args):
        channel = args.get("channel")
        oldest, limit = float(args.get("oldest") or 0), int(args.get("limit") or 100)
        with self.cv:
            if channel not in CONVERSATIONS:
                return {"ok": False, "error": "channel_not_found"}
            top = [self.messages[k] for k in self.order
                   if k[0] == channel and self.messages[k].get("thread_ts") in (None, k[1])
                   and not self.messages[k].get("deleted")]
        top = sorted((m for m in top if float(m["ts"]) > oldest), key=lambda m: -float(m["ts"]))[:limit]
        return {"ok": True, "has_more": False, "messages": [self.public(m) for m in top]}

    @staticmethod
    def public(message):
        return {k: v for k, v in message.items() if k not in ("people", "channel", "channel_type", "deleted")}

    def complete(self, method, args):
        files = args.get("files")
        if isinstance(files, str):
            files = json.loads(files)
        channel, thread = args.get("channel_id"), args.get("thread_ts")
        with self.cv:
            for f in files or []:
                upload = self.uploads.get(f.get("id"))
                if not upload or upload["size"] is None:
                    return self.refuse(method, "file_not_found", "%s was never sent" % f.get("id"))
        for f in files:
            with self.cv:
                self.uploads[f["id"]]["shared"] = {"channel": channel, "thread": thread}
                self.cv.notify_all()
            if channel:
                message = {"type": "message", "channel": channel, "user": BOT_USER, "bot_id": BOT_ID,
                           "text": args.get("initial_comment") or "", "ts": self.ts(), "team": TEAM,
                           "files": [{"id": f["id"], "name": self.uploads[f["id"]]["name"]}]}
                if thread:
                    message["thread_ts"] = thread
                self.keep(message)
        return {"ok": True, "files": [{"id": f["id"], "title": f.get("title")} for f in files]}

    def respond(self, rid, body):
        """What a response_url carried: the message pressed replaced or
        deleted, an answer to the presser alone, or a post in the channel."""
        with self.cv:
            if rid not in self.targets:
                return 404
            channel, ts, user = self.targets[rid]
            self.responses[rid].append(body)
            message = self.messages.get((channel, ts)) if ts else None
            if body.get("delete_original") and message:
                message["deleted"] = True
            elif body.get("replace_original") and message:
                message.update(text=body.get("text") or "", blocks=body.get("blocks") or [], edited=True)
            elif body.get("response_type") == "ephemeral":
                self.ephemerals.append(dict(body, channel=channel, user=user))
            else:
                self.clock += 1
                ts = "%d.%06d" % (int(time.time()), self.clock)
                self.messages[(channel, ts)] = dict(body, channel=channel, ts=ts, user=BOT_USER, bot_id=BOT_ID)
                self.order.append((channel, ts))
            self.cv.notify_all()
        return 200


class SlackHandler(http.server.BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass

    def answer(self, status, obj):
        data = json.dumps(obj).encode() if not isinstance(obj, bytes) else obj
        self.send_response(status)
        self.send_header("Content-Type", "application/json" if not isinstance(obj, bytes) else "text/plain")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def token(self):
        auth = self.headers.get("Authorization", "")
        return auth[7:] if auth.lower().startswith("bearer ") else None

    def raw(self):
        n = int(self.headers.get("Content-Length") or 0)
        return self.rfile.read(n) if n else b""

    def do_GET(self):
        url = urllib.parse.urlparse(self.path)
        if url.path == "/link" and self.headers.get("Upgrade", "").lower() == "websocket":
            return self.link()
        if url.path.startswith("/api/"):
            args = {k: v[-1] for k, v in urllib.parse.parse_qs(url.query).items()}
            return self.answer(200, self.server.call(url.path[5:], args, self.token()))
        self.answer(404, {"ok": False, "error": "unknown_method"})

    def do_POST(self):
        url = urllib.parse.urlparse(self.path)
        sl = self.server
        body = self.raw()
        if url.path.startswith("/upload/"):
            fid = url.path[len("/upload/"):]
            with sl.cv:
                upload = sl.uploads.get(fid)
                if upload is None:
                    return self.answer(404, b"no such upload")
                # The bytes ride a multipart body: the file part is what is
                # counted, near enough as the body less its framing.
                upload["size"] = upload["length"] if len(body) >= upload["length"] else len(body)
                sl.cv.notify_all()
            return self.answer(200, ("OK - %d" % len(body)).encode())
        if url.path.startswith("/response/"):
            try:
                obj = json.loads(body or b"{}")
            except ValueError:
                obj = {}
            return self.answer(sl.respond(url.path[len("/response/"):], obj), {"ok": True})
        if url.path.startswith("/api/"):
            kind = self.headers.get("Content-Type", "")
            if "json" in kind:
                try:
                    args = json.loads(body or b"{}")
                except ValueError:
                    return self.answer(200, sl.refuse(url.path[5:], "invalid_json", body[:80]))
            else:
                args = {k: v[-1] for k, v in urllib.parse.parse_qs(body.decode("utf-8", "replace")).items()}
            args.update({k: v[-1] for k, v in urllib.parse.parse_qs(url.query).items()})
            return self.answer(200, sl.call(url.path[5:], args, self.token()))
        self.answer(404, {"ok": False, "error": "unknown_method"})

    def link(self):
        """The Socket Mode link: hello on open, every frame the app sends read
        as an acknowledgement, pings answered."""
        sl = self.server
        sock = dq.Socket(self)
        with sl.cv:
            sl.sockets.append(sock)
            sl.cv.notify_all()
        sock.send({"type": "hello", "num_connections": 1, "connection_info": {"app_id": APP},
                   "debug_info": {"host": "slack-qa", "approximate_connection_time": 3600}})
        while True:
            text = sock.recv()
            if text is None:
                break
            try:
                frame = json.loads(text)
            except ValueError:
                continue
            if frame.get("envelope_id"):
                with sl.cv:
                    sl.acks.add(frame["envelope_id"])
                    if frame.get("payload"):
                        sl.responses.setdefault("ack:" + frame["envelope_id"], []).append(frame["payload"])
                    sl.cv.notify_all()
        with sl.cv:
            sock.open = False
            sl.cv.notify_all()
        self.close_connection = True


# --- the scenarios ---------------------------------------------------------------

def answer_in(sl, channel, word, timeout=30):
    """The first message the bot left in CHANNEL carrying WORD."""
    return sl.wait(lambda: next((m for m in sl.bot_messages(channel) if word in words_of(m)), None),
                   "an answer carrying %s in %s" % (word, channel), timeout=timeout)


def answered(sl, channel, word):
    return [m for m in sl.bot_messages(channel) if word in words_of(m)]


def mention(text):
    return "<@%s> %s" % (BOT_USER, text)


def boot(qa):
    """The bot says who it is with its bot token, opens Slack's socket with
    its app token, and acknowledges what the socket carries."""
    sl = qa.sl
    check(sl.called("auth.test"), "the bot never called auth.test")
    eid = sl.event({"type": "member_joined_channel", "user": USERS["kim"], "channel": GENERAL,
                    "channel_type": "C", "team": TEAM})
    sl.wait(lambda: eid in sl.acks, "the socket's acknowledgement of an event")
    qa.note("%d socket(s)" % len(sl.live_sockets()))


def dm(qa):
    """mike writes to the bot in his DM: the answer is the DM's next line,
    in no thread."""
    sl, model = qa.sl, qa.model
    model.script("qa-dm", text="QA-DM-OK in your DM.")
    sl.say("mike", "qa-dm are you there", DMS["mike"])
    answer = answer_in(sl, DMS["mike"], "QA-DM-OK")
    check(not answer.get("thread_ts"), "the DM's answer went into a thread")
    qa.dm_answer = answer


def mention_gate(qa):
    """In #general an unmentioned line is heard, not answered; one naming the
    bot is answered in a thread under it, and wears 👀, then ✅."""
    sl, model = qa.sl, qa.model
    model.script("qa-gate", text="QA-GATE-OK named, answered.")
    sl.say("mike", "qa-gate nobody asked you", GENERAL)
    sl.still(lambda: model.asked("qa-gate"), "the bot answered an unmentioned line", 4)
    ask = sl.say("mike", mention("qa-gate now I am asking"), GENERAL)
    answer = answer_in(sl, GENERAL, "QA-GATE-OK")
    check(answer.get("thread_ts") == ask["ts"], "the answer is not in the ask's thread: %r" % answer.get("thread_ts"))
    sl.wait(lambda: sl.reactions.get((GENERAL, ask["ts"])) == ["white_check_mark"],
            "the ask marked white_check_mark, its eyes gone")
    check(any(r.get("name") == "eyes" for r in sl.called("reactions.add")), "the ask never wore eyes")
    qa.thread = ask["ts"]


def follow_up(qa):
    """In the thread the bot answers in, a reply naming nobody is answered
    there, by the same conversation."""
    sl, model = qa.sl, qa.model
    check(qa.thread, "no thread to go on in (mention-gate did not run)")
    model.script("qa-follow", text="QA-FOLLOW-OK same conversation.")
    sl.say("mike", "qa-follow and the tests?", GENERAL, thread=qa.thread)
    answer = answer_in(sl, GENERAL, "QA-FOLLOW-OK")
    check(answer.get("thread_ts") == qa.thread, "the follow-up's answer left the thread")
    check(any("qa-gate" in r["raw"] for r in model.asked("qa-follow")),
          "the follow-up ran without the thread's conversation")


def stranger(qa):
    """kim, whom no list names, names the bot: nothing of it reaches the model."""
    sl, model = qa.sl, qa.model
    model.script("qa-stranger", text="QA-STRANGER-WRONG")
    sl.say("kim", mention("qa-stranger let me in"), GENERAL)
    sl.still(lambda: model.asked("qa-stranger") or answered(sl, GENERAL, "QA-STRANGER-WRONG"),
             "the bot acted on a stranger's ask", 4)


def edit_wakes(qa):
    """An unmentioned line edited to name the bot is answered."""
    sl, model = qa.sl, qa.model
    model.script("qa-edit", text="QA-EDIT-OK woken by the edit.")
    ask = sl.say("mike", "qa-edit are you there", GENERAL)
    time.sleep(1.5)
    check(not model.asked("qa-edit"), "the unmentioned line was answered before the edit")
    sl.edit(ask, mention("qa-edit are you there"))
    answer = answer_in(sl, GENERAL, "QA-EDIT-OK")
    check(answer.get("thread_ts") == ask["ts"], "the edit's answer is not under the edited line")


def thread_context(qa):
    """Named inside a thread of mike's own, the bot reads what the thread is
    about: its first message reaches the model."""
    sl, model = qa.sl, qa.model
    model.script("qa-context", text="QA-CONTEXT-OK read the root.")
    root = sl.say("mike", "the deploy plan is BLUE-GREEN-QA", GENERAL)
    sl.say("mike", mention("qa-context what is the plan?"), GENERAL, thread=root["ts"])
    answer = answer_in(sl, GENERAL, "QA-CONTEXT-OK")
    check(answer.get("thread_ts") == root["ts"], "the answer left the thread")
    check(any("BLUE-GREEN-QA" in r["raw"] for r in model.asked("qa-context")),
          "the thread's first message never reached the model")


def reaction(qa):
    """A reaction on the bot's answer reaches the conversation as a line of it."""
    sl, model = qa.sl, qa.model
    check(qa.dm_answer, "no DM answer to react to (dm did not run)")
    sl.react("mike", qa.dm_answer, "fire")
    time.sleep(1.5)
    model.script("qa-react", text="QA-REACT-OK you reacted.")
    sl.say("mike", "qa-react which emoji did I react with", DMS["mike"])
    answer_in(sl, DMS["mike"], "QA-REACT-OK")
    check(any(":fire:" in r["raw"] for r in model.asked("qa-react")), "the reaction never reached the model")


def choices(qa):
    """A question's options are buttons; a press answers it in place, its
    buttons gone, and the turn goes on with the pick."""
    sl, model = qa.sl, qa.model
    labels = ["Zero setup", "Relational queries"]
    form = "(nck:answer-choices '(%s) :platform \"slack\" :channel \"%s\")" % (
        " ".join('"%s"' % label for label in labels), DMS["mike"])
    model.script("qa-clarify", calls=[form], text="QA-QUESTION what matters most?")
    model.script("Zero setup", text="QA-PICKED-SQLITE one file, no server.")
    sl.say("mike", "qa-clarify pick me a database", DMS["mike"])
    question = answer_in(sl, DMS["mike"], "QA-QUESTION")
    buttons = buttons_of(question)
    check([(b.get("text") or {}).get("text") for b in buttons] == labels,
          "the buttons read %s" % [(b.get("text") or {}).get("text") for b in buttons])
    sl.press("mike", question, buttons[0])
    sl.wait(lambda: "Answered: Zero setup" in words_of(question) and not buttons_of(question),
            "the question marked answered, its buttons gone")
    check("QA-QUESTION" in words_of(question), "the question lost its words: %r" % words_of(question))
    answer_in(sl, DMS["mike"], "QA-PICKED-SQLITE")
    check(any("qa-clarify" in r["raw"] for r in model.asked("Zero setup")), "the pick ran without the question's lane")


def command(qa):
    """/nodecode help is answered through the command's response_url;
    /nodecode models answers mike alone."""
    sl = qa.sl
    rid = sl.command("mike", "help", GENERAL)
    said = sl.wait(lambda: [b for b in sl.responses[rid] if words_of(b)], "/nodecode help answered")
    check("/new" in words_of(said[-1]) or "/stop" in words_of(said[-1]), "help reads %r" % words_of(said[-1])[:200])
    rid = sl.command("mike", "models", GENERAL)
    said = sl.wait(lambda: sl.responses[rid], "/nodecode models answered")
    check(all(b.get("response_type") == "ephemeral" for b in said),
          "/nodecode models answered in the open: %s" % [b.get("response_type") for b in said])
    qa.note("%d response(s) to /models" % len(said))


def files(qa):
    """A file the turn hands its answer goes in Slack's three upload steps and
    is shared under the answer, in the DM."""
    sl, model = qa.sl, qa.model
    path = os.path.join(qa.home, "work", "qa-chart.png")
    with open(path, "wb") as out:
        out.write(dq.square_png(24))
    form = '(nck:answer-file "%s" :platform "slack" :channel "%s")' % (path, DMS["mike"])
    model.script("qa-file", calls=[form], text="QA-FILE-OK here is the chart.")
    sl.say("mike", "qa-file draw me a chart", DMS["mike"])
    answer_in(sl, DMS["mike"], "QA-FILE-OK")
    shared = sl.wait(lambda: [u for u in sl.uploads.values() if (u["shared"] or {}).get("channel") == DMS["mike"]],
                     "the file shared in the DM")
    check(shared[-1]["name"] == "qa-chart.png", "the file is named %r" % shared[-1]["name"])
    check(shared[-1]["length"] == os.path.getsize(path), "the upload said %d bytes, not %d"
          % (shared[-1]["length"], os.path.getsize(path)))


def card_picture(qa):
    """A picture the turn looks at shows on its running card: uploaded once,
    shared nowhere, then named on every edit."""
    sl, model = qa.sl, qa.model
    path = os.path.join(qa.home, "work", "qa-look.png")
    with open(path, "wb") as out:
        out.write(dq.square_png(32))
    model.script("qa-look", calls=["(look %s)" % json.dumps(path), "(sleep 4)"], text="QA-LOOK it is a square.")
    before = set(sl.uploads)
    sl.say("mike", mention("qa-look look at this square"), GENERAL)
    answer_in(sl, GENERAL, "QA-LOOK", timeout=60)
    new = [fid for fid in list(sl.uploads) if fid not in before]
    check(len(new) == 1, "the picture was uploaded %d times" % len(new))
    check(not (sl.uploads[new[0]]["shared"] or {}).get("channel"), "the card's picture was shared into a channel")
    shown = [args for args in sl.called("chat.update")
             if any(b.get("type") == "image" and (b.get("slack_file") or {}).get("id") == new[0]
                    for b in args.get("blocks") or [])]
    check(shown, "no edit of the card carried the picture")
    qa.note("%d edit(s) named the picture, uploaded once" % len(shown))


def board(qa):
    """The board in #board lists the #general thread by its ask, linked, in
    one message edited in place; Show all answers mike alone."""
    sl = qa.sl
    check(qa.thread, "no thread to look for (mention-gate did not run)")
    link = "https://qa.slack.com/archives/%s/p%s" % (GENERAL, qa.thread.replace(".", ""))
    message = sl.wait(lambda: next((m for m in sl.bot_messages(BOARD)
                                    if "**Threads**" in words_of(m) and link in words_of(m)), None),
                      "the board naming the thread", timeout=90)
    check("[qa-gate now I am asking](%s)" % link in words_of(message),
          "the thread is not named by its ask: %r" % words_of(message)[:300])
    check(len(sl.bot_messages(BOARD)) == 1, "the board posted %d messages, not one" % len(sl.bot_messages(BOARD)))
    show = next((b for b in buttons_of(message) if (b.get("text") or {}).get("text") == "Show all"), None)
    check(show, "the board has no Show all")
    before = len(sl.ephemerals)
    sl.press("mike", message, show)
    sl.wait(lambda: [e for e in sl.ephemerals[before:] if e.get("user") == USERS["mike"] and link in words_of(e)],
            "Show all's list, to mike alone")


def catch_up(qa):
    """While the bot is away mike asks in his DM: the ask is answered once it
    is back; one from hours before is not."""
    sl, model, bot = qa.sl, qa.model, qa.bot
    bot.stop()
    time.sleep(12)  # away past the gap a reconnect reads back
    model.script("qa-late", text="QA-LATE-OK answered after the restart.")
    model.script("qa-stale", text="QA-STALE-WRONG")
    sl.say("mike", "qa-stale from this morning", DMS["mike"], ago=3 * 3600)
    sl.say("mike", "qa-late while you were away", DMS["mike"])
    opened = sl.opened
    bot.start()
    sl.wait(lambda: sl.opened > opened and sl.live_sockets(), "the restarted bot's socket", timeout=300)
    answer_in(sl, DMS["mike"], "QA-LATE-OK", timeout=90)
    check(not model.asked("qa-stale"), "an ask from hours before was answered")


SCENARIOS = [dm, mention_gate, follow_up, stranger, edit_wakes, thread_context, reaction, choices,
             command, files, card_picture, board, catch_up]


def name_of(fn):
    return fn.__name__.replace("_", "-")


class QA:
    def __init__(self, sl, model, bot, home):
        self.sl, self.model, self.bot, self.home = sl, model, bot, home
        self.thread = None
        self.dm_answer = None
        self.notes = []

    def note(self, text):
        self.notes.append(text)


def show(sl, n=14):
    with sl.cv:
        lines = []
        for key in sl.order[-n:]:
            m = sl.messages[key]
            who = next((name for name, uid in USERS.items() if uid == m.get("user")), "bot")
            lines.append("    %-10s %-18s %-6s %s%s" % (
                (CONVERSATIONS.get(key[0]) or key[0])[:10], key[1][:18], who,
                "[deleted] " if m.get("deleted") else "",
                (m.get("text") if m.get("people") else words_of(m))[:90].replace("\n", " | ")))
        for method in sl.unknown[-5:]:
            lines.append("    unknown call: " + method)
        for refusal in sl.refused[-5:]:
            lines.append("    refused: " + refusal)
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
    home = tempfile.mkdtemp(prefix="nodecode-slack-qa-")
    os.makedirs(os.path.join(home, ".nodecode"))
    os.makedirs(os.path.join(home, "work"))
    sl, model = Slack(), Model()
    for server in (sl, model):
        threading.Thread(target=server.serve_forever, daemon=True).start()
    env = {k: v for k, v in os.environ.items()
           if k not in ("NODECODE_HOME", "XDG_CACHE_HOME", "XDG_CONFIG_HOME", "XDG_DATA_HOME")
           and not k.startswith(("SLACK_", "TELEGRAM_", "DISCORD_"))}
    env.update(HOME=home, XDG_CACHE_HOME=os.path.join(home, ".cache"), LANG="C.UTF-8",
               DISPLAY="", WAYLAND_DISPLAY="", SLACK_BOT_TOKEN=BOT_TOKEN, SLACK_APP_TOKEN=APP_TOKEN)
    mock = {"base_url": "http://127.0.0.1:%d/v1" % model.server_port, "sdk": "openai-completions",
            "api_key": "slack-qa", "models": [{"id": "mock-fast", "context_window": 200000},
                                              {"id": "mock-deep", "context_window": 200000}]}
    with open(os.path.join(home, ".nodecode", "config.jsonc"), "w") as config:
        json.dump({"update": {"mode": "off"},
                   "general": {"default_model": {"provider": "mock", "model": "mock-fast"},
                               "auxiliary_model": {"provider": "mock", "model": "mock-fast"}},
                   "providers": {"mock": mock},
                   "channels": {"slack": {
                       "bot_token_env": "SLACK_BOT_TOKEN", "app_token_env": "SLACK_APP_TOKEN",
                       "api_base": "http://127.0.0.1:%d/api" % sl.port,
                       "allowed_channels": [GENERAL, DMS["mike"]],
                       "allowed_users": [USERS["mike"]], "owner": [USERS["mike"]],
                       "allowed_workspaces": [TEAM],
                       "require_mention": True, "reactions": True,
                       "board_channel": BOARD, "catch_up_minutes": 60}}}, config, indent=1)
    # `serve' ends when its stdin does: the bot reads a pipe this run holds
    # open, so it lives as long as the run, from a terminal or not, and ends
    # with it.
    hold_read, hold_write = os.pipe()
    os.dup2(hold_read, 0)
    bot = Bot(binary, home, env)
    failed = []
    try:
        form = ('(let ((root (merge-pathnames ".nodecode/cells/" (user-homedir-pathname)))) '
                '(dolist (entry (nlk::shipped-cells)) (nlk::install-cell (nlk:offer-name entry) :root root)) '
                '(length (directory (merge-pathnames "*/" root))))')
        out = subprocess.run([binary, "trial"], input=form + "\n", env=env, cwd=home,
                             capture_output=True, text=True, timeout=300).stdout
        if not all(os.path.isdir(os.path.join(home, ".nodecode", "cells", name))
                   for name in ("nodecode-channel-kit", "nodecode-channel-slack")):
            die("the channel cells did not install:\n" + out[-1500:])
        bot.start()
        started = time.time()
        try:
            sl.wait(lambda: sl.live_sockets(), "the bot to open Slack's socket", timeout=300)
        except Failure as failure:
            die("%s; the Web API heard: %s\n%s" % (failure, sorted({m for m, _ in sl.calls}) or "nothing", bot.tail()))
        qa = QA(sl, model, bot, home)
        print("slack-qa: bot pid %d on Slack :%d, model :%d, socket open in %.0fs"
              % (bot.proc.pid, sl.port, model.server_port, time.time() - started))
        for fn in [boot] + [s for s in SCENARIOS if not chosen or name_of(s) in chosen]:
            qa.notes = []
            began, refused = time.time(), len(sl.refused)
            try:
                if not bot.alive():
                    raise Failure("the bot exited:\n" + bot.tail())
                fn(qa)
                check(len(sl.refused) == refused, "Slack would have refused: %s" % sl.refused[refused:])
                print("  ok    %-15s %5.1fs  %s" % (name_of(fn), time.time() - began, "; ".join(qa.notes)))
            except Failure as failure:
                failed.append(name_of(fn))
                print("  FAIL  %-15s %5.1fs  %s\n%s" % (name_of(fn), time.time() - began, failure, show(sl)))
            except Exception:
                failed.append(name_of(fn))
                print("  FAIL  %-15s crashed:\n%s" % (name_of(fn), traceback.format_exc()))
        if sl.unknown:
            print("slack-qa: methods the stand-in had no answer for: " + ", ".join(sorted(set(sl.unknown))))
        if failed:
            print("\nslack-qa: FAILED: " + ", ".join(failed))
        else:
            print("\nslack-qa: every scenario held")
        return 1 if failed else 0
    finally:
        bot.stop()
        for server in (sl, model):
            server.shutdown()
        if args.keep or failed:
            with open(os.path.join(home, "slack-state.json"), "w") as out:
                json.dump({"messages": [sl.messages[k] for k in sl.order], "calls": sl.calls,
                           "ephemerals": sl.ephemerals, "responses": sl.responses,
                           "uploads": sl.uploads, "model": model.requests}, out, indent=1, default=str)
            print("slack-qa: scratch home kept at %s (serve.log, slack-state.json)" % home)
        else:
            shutil.rmtree(home, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
