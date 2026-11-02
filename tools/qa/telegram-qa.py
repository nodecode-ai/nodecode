#!/usr/bin/env python3
"""telegram-qa.py -- the Telegram cell driven end to end, the way people in a
chat drive it.

Usage: telegram-qa.py BINARY [--only NAME[,NAME...]] [--list] [--keep]

BINARY is the nodecode executable under proof. It serves headless (`serve') in
a scratch home -- updates off, every shipped cell installed as /setup
installs them -- against two stand-ins played here, on 127.0.0.1 and nowhere
else:

  - Telegram: the Bot API the adapter calls -- getUpdates as a long poll,
    rich messages and their edits, deletes, reactions, forum topics, the
    command menu, callback answers, a group's admins -- holding the chats the
    way Telegram does: mike's and kim's private chats with the bot, a group
    (no forum) and a forum group, mike its creator and the owner, kim whom no
    list names. What Telegram refuses it refuses too: an edit that changes
    nothing, a post to a topic that is not there, a topic in a chat that is
    no forum.
  - the model: discord-qa.py's chat provider, the same scripts keyed on a
    word in the ask.

Each scenario says something the way a person would and holds the bot to
what that person would then see; the scenarios run in order, on one bot,
which `catch-up' restarts. --only runs the named ones (the boot always
runs). Exits 1 when a scenario fails, printing what the chats held; 2 when
the proof cannot run.
"""

import argparse
import importlib.util
import http.server
import json
import os
import shutil
import subprocess
import sys
import tempfile
import threading
import time
import traceback
import urllib.parse
from email import policy as email_policy
from email.parser import BytesParser

# The model stand-in, the bot under proof and the checks are discord-qa.py's:
# one chat provider for every channel's QA.
_spec = importlib.util.spec_from_file_location(
    "discord_qa", os.path.join(os.path.dirname(os.path.abspath(__file__)), "discord-qa.py"))
dq = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(dq)
Failure, check, Model, Bot = dq.Failure, dq.check, dq.Model, dq.Bot

TOKEN = "4242:qa-token-not-a-real-one"
BOT = {"id": 1000, "is_bot": True, "first_name": "QA bot", "username": "qa_bot"}
USERS = {"mike": 1001, "kim": 1002}
GROUP, FORUM = -1001000000001, -1001000000002
CHATS = {
    USERS["mike"]: {"id": USERS["mike"], "type": "private", "first_name": "mike", "username": "mike"},
    USERS["kim"]: {"id": USERS["kim"], "type": "private", "first_name": "kim", "username": "kim"},
    GROUP: {"id": GROUP, "type": "supergroup", "title": "QA group"},
    FORUM: {"id": FORUM, "type": "supergroup", "title": "QA forum", "is_forum": True},
}


def die(message, code=2):
    print("telegram-qa: " + message, file=sys.stderr)
    sys.exit(code)


def user(name):
    return {"id": USERS[name], "is_bot": False, "first_name": name, "username": name}


def words_of(message):
    """What a message the bot sent says: its rich Markdown, its HTML, its
    text or its caption."""
    rich = message.get("rich_message") or {}
    return rich.get("markdown") or rich.get("html") or message.get("text") or message.get("caption") or ""


def buttons_of(message):
    markup = message.get("reply_markup") or {}
    return [b for row in markup.get("inline_keyboard") or [] for b in row]


class Telegram(http.server.ThreadingHTTPServer):
    """The Bot API, for one bot, over the chats above."""
    daemon_threads = True

    def __init__(self):
        super().__init__(("127.0.0.1", 0), TelegramHandler)
        self.port = self.server_address[1]
        self.cv = threading.Condition()
        self.updates = []          # every update ever made, update_id order
        self.next_update = 1
        self.next_id = {}          # chat -> its next message id
        self.messages = {}         # (chat, id) -> message
        self.order = []            # (chat, id), in the order they were made
        self.topics = {}           # (chat, thread) -> {"name", "closed"}
        self.calls = []            # (method, body)
        self.unknown = []
        self.refused = []          # what Telegram would have refused
        self.commands = None
        self.answers = []          # answerCallbackQuery bodies
        self.reactions = []        # setMessageReaction bodies
        self.polls = 0

    # --- the people's side ---------------------------------------------------

    def message_id(self, chat):
        self.next_id[chat] = self.next_id.get(chat, 100) + 1
        return self.next_id[chat]

    def push(self, **update):
        with self.cv:
            update["update_id"] = self.next_update
            self.next_update += 1
            self.updates.append(update)
            self.cv.notify_all()
        return update

    def say(self, who, text, chat=None, thread=None, ago=0, reply_to=None):
        """WHO says TEXT in CHAT (their own chat with the bot unless named),
        in forum topic THREAD, AGO seconds back. => the message."""
        chat = USERS[who] if chat is None else chat
        with self.cv:
            message = {"message_id": self.message_id(chat), "from": user(who), "chat": CHATS[chat],
                       "date": int(time.time() - ago), "text": text}
            if thread is not None:
                message.update(message_thread_id=thread, is_topic_message=True)
                # A message in a topic replies to the topic's creation, as
                # Telegram sends it: from whoever opened the topic.
                topic = self.topics.get((chat, thread)) or {}
                reply_to = reply_to or {"message_id": thread, "from": topic.get("creator", user(who)),
                                        "chat": CHATS[chat], "date": int(time.time()),
                                        "message_thread_id": thread, "is_topic_message": True,
                                        "forum_topic_created": {"name": topic.get("name", ""),
                                                                "icon_color": 7322096}}
            if reply_to is not None:
                message["reply_to_message"] = reply_to
            self.messages[(chat, message["message_id"])] = dict(message, people=True)
            self.order.append((chat, message["message_id"]))
        self.push(message=message)
        return message

    def edit(self, message, text):
        edited = dict(message, text=text, edit_date=int(time.time()))
        self.push(edited_message=edited)
        return edited

    def react(self, who, message, emoji):
        chat = message["chat"]["id"]
        self.push(message_reaction={"chat": CHATS[chat], "message_id": message["message_id"],
                                    "user": user(who), "date": int(time.time()),
                                    "old_reaction": [], "new_reaction": [{"type": "emoji", "emoji": emoji}]})

    def press(self, who, message, data):
        self.push(callback_query={"id": "cb%d" % self.next_update, "from": user(who), "data": data,
                                  "chat_instance": "qa",
                                  "message": {k: v for k, v in message.items()
                                              if k in ("message_id", "chat", "date", "message_thread_id")}})

    # --- reading what the bot did ---------------------------------------------

    def bot_messages(self, chat, thread="any", after=None):
        with self.cv:
            out = []
            for key in self.order:
                m = self.messages[key]
                if m.get("people") or m.get("deleted") or key[0] != chat:
                    continue
                if thread != "any" and m.get("message_thread_id") != thread:
                    continue
                if after is not None and m["date_ms"] <= after:
                    continue
                out.append(m)
            return out

    def called(self, method):
        with self.cv:
            return [body for name, body in self.calls if name == method]

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

    # --- the Bot API ------------------------------------------------------------

    def refuse(self, method, description):
        self.refused.append("%s: %s" % (method, description))
        return 400, {"ok": False, "error_code": 400, "description": "Bad Request: " + description}

    def post(self, method, body, files):
        """A sent message of the bot's, kept as Telegram keeps it."""
        chat = int(body["chat_id"])
        if chat not in CHATS:
            return self.refuse(method, "chat not found")
        thread = body.get("message_thread_id")
        if thread is not None:
            thread = int(thread)
            if (chat, thread) not in self.topics:
                return self.refuse(method, "message thread not found")
        message = {"message_id": self.message_id(chat), "from": BOT, "chat": CHATS[chat],
                   "date": int(time.time()), "date_ms": time.time() * 1000}
        if thread is not None:
            message.update(message_thread_id=thread, is_topic_message=True)
        for key in ("rich_message", "text", "caption", "reply_markup", "reply_parameters", "disable_notification"):
            if body.get(key) is not None:
                value = body[key]
                message[key] = json.loads(value) if isinstance(value, str) and key in ("rich_message", "reply_markup", "reply_parameters") and value[:1] in "{[" else value
        if files:
            message["files"] = sorted(files)
        self.messages[(chat, message["message_id"])] = message
        self.order.append((chat, message["message_id"]))
        return 200, {"ok": True, "result": {k: v for k, v in message.items() if k != "date_ms"}}

    def call(self, method, body, files):
        """(STATUS, ANSWER) of one Bot API METHOD called with BODY."""
        ok = lambda result=True: (200, {"ok": True, "result": result})
        if method == "getMe":
            return ok(BOT)
        if method == "getUpdates":
            return None  # answered by the handler: it parks
        if method in ("setMyCommands",):
            self.commands = body.get("commands")
            return ok()
        if method in ("deleteMyCommands", "sendChatAction", "deleteWebhook", "setMyDescription"):
            return ok()
        if method == "getMyCommands":
            return ok(self.commands or [])
        if method in ("sendRichMessage", "sendMessage", "sendPhoto", "sendDocument", "sendVoice"):
            return self.post(method, body, files)
        if method == "editMessageText":
            key = (int(body["chat_id"]), int(body["message_id"]))
            message = self.messages.get(key)
            if not message or message.get("people"):
                return self.refuse(method, "message to edit not found")
            new = dict(message)
            for field in ("rich_message", "text", "reply_markup"):
                if body.get(field) is not None:
                    new[field] = body[field]
            if (words_of(new), new.get("reply_markup")) == (words_of(message), message.get("reply_markup")):
                return 400, {"ok": False, "error_code": 400,
                             "description": "Bad Request: message is not modified: specified new message content and reply markup are exactly the same as a current content and reply markup of the message"}
            new["edits"] = message.get("edits", 0) + 1
            self.messages[key] = new
            return ok({k: v for k, v in new.items() if k != "date_ms"})
        if method == "deleteMessage":
            key = (int(body["chat_id"]), int(body["message_id"]))
            if key not in self.messages:
                return self.refuse(method, "message to delete not found")
            self.messages[key]["deleted"] = True
            return ok()
        if method == "setMessageReaction":
            self.reactions.append(body)
            return ok()
        if method == "answerCallbackQuery":
            self.answers.append(body)
            return ok()
        if method == "createForumTopic":
            chat = int(body["chat_id"])
            if not CHATS.get(chat, {}).get("is_forum"):
                return self.refuse(method, "the chat is not a forum")
            thread = self.message_id(chat)
            self.topics[(chat, thread)] = {"name": body.get("name"), "closed": False, "creator": BOT}
            return ok({"message_thread_id": thread, "name": body.get("name"), "icon_color": 7322096})
        if method in ("editForumTopic", "closeForumTopic", "reopenForumTopic", "deleteForumTopic"):
            key = (int(body["chat_id"]), int(body["message_thread_id"]))
            topic = self.topics.get(key)
            if topic is None:
                return self.refuse(method, "message thread not found")
            if method == "editForumTopic":
                topic["name"] = body.get("name", topic["name"])
            elif method == "closeForumTopic":
                if topic["closed"]:
                    return 400, {"ok": False, "error_code": 400, "description": "Bad Request: TOPIC_NOT_MODIFIED"}
                topic["closed"] = True
            elif method == "reopenForumTopic":
                topic["closed"] = False
            else:
                del self.topics[key]
            return ok()
        if method == "getChatAdministrators":
            return ok([{"status": "creator", "user": user("mike")}, {"status": "administrator", "user": BOT}])
        if method == "getChat":
            chat = CHATS.get(int(body.get("chat_id", 0)))
            return ok(chat) if chat else self.refuse(method, "chat not found")
        if method == "getChatMember":
            return ok({"status": "member", "user": user("kim")})
        self.unknown.append(method)
        return ok()


class TelegramHandler(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, *args):
        pass

    def answer(self, status, obj):
        data = json.dumps(obj).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def body(self):
        raw = self.rfile.read(int(self.headers.get("Content-Length") or 0))
        kind = self.headers.get("Content-Type") or ""
        files = {}
        if kind.startswith("multipart/"):
            parsed = BytesParser(policy=email_policy.HTTP).parsebytes(
                b"Content-Type: " + kind.encode() + b"\r\n\r\n" + raw)
            body = {}
            for part in parsed.iter_parts():
                name = part.get_param("name", header="content-disposition")
                data = part.get_payload(decode=True) or b""
                if part.get_filename():
                    files[name] = (part.get_filename(), len(data))
                elif name:
                    body[name] = data.decode("utf-8", "replace")
            return body, files
        if kind.startswith("application/x-www-form-urlencoded"):
            return dict(urllib.parse.parse_qsl(raw.decode())), files
        query = dict(urllib.parse.parse_qsl(urllib.parse.urlsplit(self.path).query))
        return (json.loads(raw) if raw.strip() else query), files

    def do_GET(self):
        self.do_POST()

    def do_POST(self):
        tg = self.server
        path = urllib.parse.urlsplit(self.path).path
        prefix = "/bot%s/" % TOKEN
        if not path.startswith(prefix):
            return self.answer(404, {"ok": False, "error_code": 404, "description": "Not Found"})
        method = path[len(prefix):]
        body, files = self.body()
        with tg.cv:
            tg.calls.append((method, body))
            tg.cv.notify_all()
        if method == "getUpdates":
            offset = int(body.get("offset") or 0)
            deadline = time.time() + min(float(body.get("timeout") or 0), 2.0)
            with tg.cv:
                tg.polls += 1
                # An offset confirms every update before it: Telegram keeps
                # those no more.
                tg.updates = [u for u in tg.updates if u["update_id"] >= offset]
                tg.cv.notify_all()
                while True:
                    pending = [u for u in tg.updates if u["update_id"] >= offset]
                    if pending or time.time() >= deadline:
                        break
                    tg.cv.wait(0.1)
            return self.answer(200, {"ok": True, "result": pending[:100]})
        with tg.cv:
            status, answer = tg.call(method, body, files)
            tg.cv.notify_all()
        self.answer(status, answer)


# --- the scenarios -----------------------------------------------------------

def answer_in(tg, chat, word, thread="any", after=None, timeout=30):
    """The first bot message in CHAT (topic THREAD) after AFTER carrying WORD."""
    return tg.wait(lambda: next((m for m in tg.bot_messages(chat, thread, after) if word in words_of(m)), None),
                   "an answer carrying %s in %s" % (word, chat), timeout=timeout)


def now_ms():
    return time.time() * 1000


def boot(qa):
    """The bot polls, and puts its command menu, /threads in it."""
    tg = qa.tg
    commands = tg.wait(lambda: tg.commands, "the command menu (setMyCommands)")
    names = {c["command"] for c in commands}
    for name in ("help", "new", "stop", "models", "channels", "threads"):
        check(name in names, "/%s is not in the command menu: %s" % (name, sorted(names)))
    polls = tg.called("getUpdates")
    allowed = set(polls[-1].get("allowed_updates") or [])
    for kind in ("message", "edited_message", "callback_query", "message_reaction"):
        check(kind in allowed, "the poll leaves out %s updates: %s" % (kind, sorted(allowed)))
    qa.note("%d commands" % len(names))


def private_chat(qa):
    """mike writes to the bot in his own chat with it: the answer comes back there."""
    tg, model = qa.tg, qa.model
    model.script("qa-dm", text="QA-DM-OK in your own chat.")
    tg.say("mike", "qa-dm are you there")
    answer_in(tg, USERS["mike"], "QA-DM-OK")


def mention_gate(qa):
    """In a group, an unmentioned line is heard, not answered; one naming
    @qa_bot is answered as a reply to it."""
    tg, model = qa.tg, qa.model
    model.script("qa-gate", text="QA-GATE-OK named, answered.")
    tg.say("mike", "qa-gate nobody asked you", chat=GROUP)
    tg.still(lambda: model.asked("qa-gate"), "the bot answered an unmentioned line", 4)
    ask = tg.say("mike", "@qa_bot qa-gate now I am asking", chat=GROUP)
    answer = answer_in(tg, GROUP, "QA-GATE-OK")
    replied = (answer.get("reply_parameters") or {}).get("message_id")
    check(replied == ask["message_id"], "the answer replies to %s, not to the ask %s" % (replied, ask["message_id"]))


def stranger(qa):
    """kim, whom no list names, names the bot: nothing of it reaches the model."""
    tg, model = qa.tg, qa.model
    model.script("qa-stranger", text="QA-STRANGER-WRONG")
    tg.say("kim", "@qa_bot qa-stranger let me in", chat=GROUP)
    tg.still(lambda: model.asked("qa-stranger") or tg_answered(tg, GROUP, "QA-STRANGER-WRONG"),
             "the bot acted on a stranger's ask", 4)


def tg_answered(tg, chat, word):
    return [m for m in tg.bot_messages(chat) if word in words_of(m)]


def edit_wakes(qa):
    """An unmentioned line edited to name the bot is answered."""
    tg, model = qa.tg, qa.model
    model.script("qa-edit", text="QA-EDIT-OK woken by the edit.")
    ask = tg.say("mike", "qa-edit are you there", chat=GROUP)
    time.sleep(1.5)
    check(not model.asked("qa-edit"), "the unmentioned line was answered before the edit")
    tg.edit(ask, "@qa_bot qa-edit are you there")
    answer_in(tg, GROUP, "QA-EDIT-OK")


def topic_per_ask(qa):
    """In the forum, an ask outside a topic opens a topic of its own and is
    answered there; a follow-up in it needs no mention."""
    tg, model = qa.tg, qa.model
    model.script("qa-topic", text="QA-TOPIC-OK answered in its topic.", title="QA build fix")
    tg.say("mike", "@qa_bot qa-topic the build is red", chat=FORUM)
    created = tg.wait(lambda: tg.called("createForumTopic"), "a topic for the ask (createForumTopic)")
    thread = tg.wait(lambda: next((t for (c, t) in tg.topics if c == FORUM), None), "the topic")
    answer = answer_in(tg, FORUM, "QA-TOPIC-OK", thread=thread)
    qa.thread = thread
    model.script("qa-follow", text="QA-FOLLOW-OK same conversation.")
    tg.say("mike", "qa-follow and the tests?", chat=FORUM, thread=thread)
    answer_in(tg, FORUM, "QA-FOLLOW-OK", thread=thread)
    check(any("Close" in b.get("text", "") for m in tg.bot_messages(FORUM, thread) for b in buttons_of(m)),
          "no Close button under the topic's answers")
    qa.note("topic %d named %r" % (thread, tg.topics[(FORUM, thread)]["name"]))


def ping(qa):
    """The room's instructions say how to ping someone on Telegram, and an
    answer that does carries the link Telegram tags them by."""
    tg, model = qa.tg, qa.model
    model.script("qa-ping", text="QA-PING-OK [mike](tg://user?id=%d) look here." % USERS["mike"])
    tg.say("mike", "@qa_bot qa-ping tell me when it is done", chat=GROUP)
    answer = answer_in(tg, GROUP, "QA-PING-OK")
    check("tg://user?id=%d" % USERS["mike"] in words_of(answer), "the answer lost the mention: %r" % words_of(answer))
    check(not answer.get("disable_notification"), "the answer was sent silently")
    request = model.asked("qa-ping")[-1]["raw"]
    check("tg://user?id=" in request, "the model was never told how to ping someone on Telegram")


def reaction(qa):
    """A reaction on the bot's answer reaches the conversation as a line of it."""
    tg, model = qa.tg, qa.model
    answer = answer_in(tg, USERS["mike"], "QA-DM-OK")
    tg.react("mike", answer, "\U0001F525")
    time.sleep(1.5)
    model.script("qa-react", text="QA-REACT-OK you reacted.")
    tg.say("mike", "qa-react which emoji did I react with")
    answer_in(tg, USERS["mike"], "QA-REACT-OK")
    request = model.asked("qa-react")[-1]["raw"]
    check("\U0001F525" in request or "\\ud83d\\udd25" in request.lower(), "the reaction never reached the model")


def private_command(qa):
    """/models picked from the group's command menu is answered in mike's own
    chat with the bot, never in the group."""
    tg = qa.tg
    before = now_ms()
    # A group's menu sends the command addressed: /models@<bot>.
    tg.say("mike", "/models@qa_bot", chat=GROUP)
    dm = tg.wait(lambda: [m for m in tg.bot_messages(USERS["mike"], after=before) if words_of(m)],
                 "/models answered in mike's own chat")
    check(not [m for m in tg.bot_messages(GROUP, after=before) if "mock-fast" in words_of(m)],
          "/models answered in the group")
    qa.note("%d message(s) in the DM" % len(dm))


def board(qa):
    """The thread board in the forum lists the topic the bot opened, as a link;
    Show all sends mike the list in his own chat."""
    tg = qa.tg
    check(qa.thread, "no topic to look for (topic-per-ask did not run)")
    name = tg.topics[(FORUM, qa.thread)]["name"]
    link = "https://t.me/c/1000000002/%d" % qa.thread
    message = tg.wait(lambda: next((m for m in tg.bot_messages(FORUM, thread=None)
                                    if "**Threads**" in words_of(m) and link in words_of(m)), None),
                      "the board naming the topic", timeout=90)
    labels = [b["text"] for b in buttons_of(message)]
    check("Show all" in labels, "the board has no Show all: %s" % labels)
    before = now_ms()
    tg.press("mike", message, "nct:board:all")
    tg.wait(lambda: [m for m in tg.bot_messages(USERS["mike"], after=before) if link in words_of(m)],
            "Show all's list in mike's chat")
    qa.note("board lists %r: %s" % (name, words_of(message).split("\n")[2] if words_of(message).count("\n") > 2 else ""))
    qa.board = message


def close(qa):
    """Close on the topic's answer closes the topic, and the board lets it go."""
    tg = qa.tg
    check(qa.thread, "no topic to close (topic-per-ask did not run)")
    answer = next((m for m in reversed(tg.bot_messages(FORUM, qa.thread))
                   for b in buttons_of(m) if "Close" in b.get("text", "")), None)
    check(answer, "no answer in the topic carries Close")
    data = next(b["callback_data"] for b in buttons_of(answer) if "Close" in b.get("text", ""))
    tg.press("mike", answer, data)
    tg.wait(lambda: tg.topics[(FORUM, qa.thread)]["closed"], "the topic closed (closeForumTopic)")
    link = "https://t.me/c/1000000002/%d" % qa.thread
    tg.wait(lambda: next((m for m in tg.bot_messages(FORUM, thread=None)
                          if "**Threads**" in words_of(m) and link not in words_of(m)), None),
            "the board letting the closed topic go", timeout=90)


def catch_up(qa):
    """While the bot is away, mike asks twice: the ask from minutes ago is
    answered when it is back, the one from hours ago is not."""
    tg, model, bot = qa.tg, qa.model, qa.bot
    bot.stop()
    model.script("qa-late", text="QA-LATE-OK answered after the restart.")
    model.script("qa-stale", text="QA-STALE-WRONG")
    tg.say("mike", "qa-stale from this morning", ago=3 * 3600)
    tg.say("mike", "qa-late while you were away", ago=120)
    polls = tg.polls
    bot.start()
    tg.wait(lambda: tg.polls > polls, "the restarted bot to poll", timeout=300)
    answer_in(tg, USERS["mike"], "QA-LATE-OK", timeout=90)
    check(not model.asked("qa-stale"), "an ask from hours before was answered")


SCENARIOS = [private_chat, mention_gate, stranger, edit_wakes, topic_per_ask, ping, reaction,
             private_command, board, close, catch_up]


def name_of(fn):
    return fn.__name__.replace("_", "-")


class QA:
    def __init__(self, tg, model, bot, home):
        self.tg, self.model, self.bot, self.home = tg, model, bot, home
        self.thread = None
        self.board = None
        self.notes = []

    def note(self, text):
        self.notes.append(text)


def show(tg, n=14):
    with tg.cv:
        lines = []
        for key in tg.order[-n:]:
            m = tg.messages[key]
            where = CHATS[key[0]].get("title") or CHATS[key[0]].get("first_name")
            lines.append("    %5d %-9s %-6s %-6s %s%s" % (
                key[1], where[:9], ("t%s" % m["message_thread_id"]) if m.get("message_thread_id") else "",
                m["from"]["first_name"][:6], "[deleted] " if m.get("deleted") else "",
                (m.get("text") if m.get("people") else words_of(m))[:90].replace("\n", " | ")))
        for method in tg.unknown[-5:]:
            lines.append("    unknown call: " + method)
        for refusal in tg.refused[-5:]:
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
    home = tempfile.mkdtemp(prefix="nodecode-telegram-qa-")
    os.makedirs(os.path.join(home, ".nodecode"))
    os.makedirs(os.path.join(home, "work"))
    tg, model = Telegram(), Model()
    for server in (tg, model):
        threading.Thread(target=server.serve_forever, daemon=True).start()
    env = {k: v for k, v in os.environ.items()
           if k not in ("NODECODE_HOME", "XDG_CACHE_HOME", "XDG_CONFIG_HOME", "XDG_DATA_HOME")
           and not k.startswith("TELEGRAM_") and not k.startswith("DISCORD_")}
    env.update(HOME=home, XDG_CACHE_HOME=os.path.join(home, ".cache"), LANG="C.UTF-8",
               DISPLAY="", WAYLAND_DISPLAY="", TELEGRAM_BOT_TOKEN=TOKEN)
    mock = {"base_url": "http://127.0.0.1:%d/v1" % model.server_port, "sdk": "openai-completions",
            "api_key": "telegram-qa", "models": [{"id": "mock-fast", "context_window": 200000},
                                                 {"id": "mock-deep", "context_window": 200000}]}
    with open(os.path.join(home, ".nodecode", "config.jsonc"), "w") as config:
        json.dump({"update": {"mode": "off"},
                   "general": {"default_model": {"provider": "mock", "model": "mock-fast"},
                               "auxiliary_model": {"provider": "mock", "model": "mock-fast"}},
                   "providers": {"mock": mock},
                   "channels": {"telegram": {
                       "token_env": "TELEGRAM_BOT_TOKEN",
                       "api_base": "http://127.0.0.1:%d" % tg.port,
                       "allowed_chats": [str(USERS["mike"]), str(GROUP), str(FORUM)],
                       "allowed_users": [str(USERS["mike"])], "owner": [str(USERS["mike"])],
                       "require_mention": True, "reactions": True,
                       "board_chat": str(FORUM), "catch_up_minutes": 60,
                       "poll_interval_ms": 50, "get_updates_timeout_seconds": 1}}}, config, indent=1)
    bot = Bot(binary, home, env)
    failed = []
    try:
        form = ('(let ((root (merge-pathnames ".nodecode/cells/" (user-homedir-pathname)))) '
                '(dolist (entry (nlk::shipped-cells)) (nlk::install-cell (nlk:offer-name entry) :root root)) '
                '(length (directory (merge-pathnames "*/" root))))')
        out = subprocess.run([binary, "trial"], input=form + "\n", env=env, cwd=home,
                             capture_output=True, text=True, timeout=300).stdout
        if not all(os.path.isdir(os.path.join(home, ".nodecode", "cells", name))
                   for name in ("nodecode-channel-kit", "nodecode-channel-telegram")):
            die("the channel cells did not install:\n" + out[-1500:])
        bot.start()
        started = time.time()
        try:
            tg.wait(lambda: tg.polls > 0, "the bot to poll (getUpdates)", timeout=300)
        except Failure as failure:
            die("%s; the Bot API heard: %s\n%s" % (failure, sorted({m for m, _ in tg.calls}) or "nothing", bot.tail()))
        qa = QA(tg, model, bot, home)
        print("telegram-qa: bot pid %d on Telegram :%d, model :%d, polling in %.0fs"
              % (bot.proc.pid, tg.port, model.server_port, time.time() - started))
        for fn in [boot] + [s for s in SCENARIOS if not chosen or name_of(s) in chosen]:
            qa.notes = []
            began, refused = time.time(), len(tg.refused)
            try:
                if not bot.alive():
                    raise Failure("the bot exited:\n" + bot.tail())
                fn(qa)
                check(len(tg.refused) == refused, "Telegram would have refused: %s" % tg.refused[refused:])
                print("  ok    %-15s %5.1fs  %s" % (name_of(fn), time.time() - began, "; ".join(qa.notes)))
            except Failure as failure:
                failed.append(name_of(fn))
                print("  FAIL  %-15s %5.1fs  %s\n%s" % (name_of(fn), time.time() - began, failure, show(tg)))
            except Exception:
                failed.append(name_of(fn))
                print("  FAIL  %-15s crashed:\n%s" % (name_of(fn), traceback.format_exc()))
        if tg.unknown:
            print("telegram-qa: methods the stand-in had no answer for: " + ", ".join(sorted(set(tg.unknown))))
        if failed:
            print("\ntelegram-qa: FAILED: " + ", ".join(failed))
        else:
            print("\ntelegram-qa: every scenario held")
        return 1 if failed else 0
    finally:
        bot.stop()
        for server in (tg, model):
            server.shutdown()
        if args.keep or failed:
            with open(os.path.join(home, "telegram-state.json"), "w") as out:
                json.dump({"messages": [tg.messages[k] for k in tg.order], "calls": tg.calls,
                           "topics": {"%s:%s" % k: v for k, v in tg.topics.items()},
                           "model": model.requests}, out, indent=1, default=str)
            print("telegram-qa: scratch home kept at %s (serve.log, telegram-state.json)" % home)
        else:
            shutil.rmtree(home, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
