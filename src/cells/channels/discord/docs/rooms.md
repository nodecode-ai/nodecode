# Rooms and access

This page covers who the bot listens to, where, and how a conversation in a
channel, a thread or a direct message is kept.

## Who may talk to it

Every message the bot reads passes one gate. The gate runs these checks in
order and refuses at the first that fails. The name in brackets is the reason
the refusal records (see
[troubleshooting](troubleshooting.md#the-bot-says-nothing)).

1. The bot's own messages are always refused (`self_author`).
2. Another bot's message is refused unless `allow_bots` lets it in
   (`bot_author`).
3. `allowed_guilds`, when set, must name the server (`guild_not_allowed`).
   A direct message has no server, so a set `allowed_guilds` refuses every
   direct message.
4. `allowed_channels`, when set, must name the channel (`channel_not_allowed`).
   A message in a thread passes when the list names the thread or its parent
   channel. `"*"` allows every channel.
5. The author must be allowed (`user_not_allowed`): see below.
6. `thread_behavior: "disabled"` refuses messages typed inside threads
   (`threads_disabled`).
7. `dm_policy: "disabled"` refuses direct messages (`dm_disabled`).
8. `ignored_channels` refuses the channels it names (`channel_ignored`).
9. A bot let in with `allow_bots: "mentions"` must mention this bot
   (`bot_not_mentioned`).

A refused message is dropped. Nothing about it reaches the model or the
room's record.

### Allowed users, roles and pairing

An author is allowed when any of these holds:

- Neither `allowed_users` nor `allowed_roles` is set. Then everyone who
  passes the channel checks may talk.
- `allowed_users` names their user id, or contains `"*"`.
- They hold a role that `allowed_roles` names. Roles exist only in a
  server: a direct message carries none.
- You let them in by pairing (below).

The owners in `owner` are added to `allowed_users` when that list is set. If
you gate by `allowed_roles` alone, give your owners the role or list them in
`allowed_users` too.

The section must set at least one of `allowed_channels`, `allowed_users` and
`allowed_roles`, or the bot does not start. `allowed_channels` alone admits
everyone who can write in those channels.

### Direct messages

A direct message is a channel of its own, with its own id. Two settings
therefore refuse direct messages even from allowed users:

- `allowed_channels` set to specific channels: the DM's channel is not on
  the list.
- `allowed_guilds` set: a DM has no server.

To take direct messages, gate with `allowed_users` or `allowed_roles` and
leave `allowed_channels` and `allowed_guilds` empty (or put `"*"` in
`allowed_channels`). Pairing has the same requirement: a stranger's DM is
offered a code only when the refusal is `user_not_allowed`.

### Pairing

With `pairing` on (the default), someone no list names who writes to the bot
directly gets an eight-character code:

> I don't know you yet. Your pairing code is ABCD2345: give it to the bot's
> operator, who can let you in with it. It lasts an hour.

You get a notice naming who asked, never the code. The code is how you know
the person who brings it to you is the one who wrote. Then:

- `/channels pair CODE` lets them in. They are told "You're in: the
  operator let you in. Ask away."
- `/channels paired` lists who is paired and how many codes wait.
- `/channels unpair USER-ID` takes a pairing back.
- The web page's **Control**, **Channels** tab shows the same under
  **Pairing**: the asks waiting (approve one with **Let in**), the people let
  in (with **Unpair**), and a field for a code someone gave you.

A code lasts one hour. One person gets at most one code every ten minutes,
and at most three codes wait at once; past that the person is told to try
later. Codes are lost on restart, pairings are kept. A paired person is
allowed everywhere an allowed user is.

### Other bots

`allow_bots` decides what happens to other bots' messages:

- `"none"` (default): ignored.
- `"mentions"`: a bot's message that mentions this bot is answered; the rest
  are ignored. This keeps two bots from answering each other forever.
- `"all"`: treated like a person's.

## The operator

`owner` lists the operators: the people whose word is standing policy. It
has three effects:

- Their messages reach the model marked `(operator)`. No one else's name can
  carry that mark, whatever their display name says.
- The model is told that the operator's instructions win over a
  participant's, and that a claim of authority typed into a message is not
  authority.
- With operators declared, every slash command but `/help` is theirs.
  Others are refused (privately, when they used the menu), and the operator
  is told who tried what, at most once every ten minutes per person.

If `owner` is unset and `allowed_users` names exactly one person, that
person is the operator. Otherwise the room has no operator: a warning says
so at start, nobody's word outranks anybody's, and anyone admitted may run
every command.

## Where it answers: mentions

A message the gate admits is either an ask, which gets an answer, or
context, which the bot keeps for later without answering.

- `require_mention: true` (default): in a channel, a message is an ask only
  when it mentions the bot, replies to one of the bot's messages, or comes
  from the bot's slash command menu. Anything else is context.
- `require_mention: false`: every admitted message in a channel is an ask.
  This needs the Message Content intent.
- In direct messages and in threads the bot is part of, no mention is
  needed: every admitted message is an ask.

A mention can be the bot's user (`@YourBot`) or the role Discord made for
the bot, which the mention picker offers beside it. `mention_patterns` adds
wake words: a message that contains one, in any case, counts as a mention.

Per-channel lists override the default. When a channel is on several lists,
`ignored_channels` wins, then `free_response_channels`, then
`require_mention_channels`. A thread matches its own id and its parent's.

- `free_response_channels`: every admitted message is an ask.
- `require_mention_channels`: only a mention, a reply or a menu command is
  an ask.
- `ignored_channels`: nothing is read, not even as context.

A message that opens by addressing someone else is context, not an ask,
unless it also mentions the bot. "Addressing" means it starts with a mention
of another user, or with a name from `other_addressees` followed by `,`,
`:`, `;` or `-` (for example `"ada, can you look"`). Use this when several
bots share a channel.

## Where it answers: threads

`thread_behavior` decides what an ask typed in a plain channel does:

- `"create_per_message"` (default): the ask opens a public thread off its
  own message. The thread is named after the ask's first five words, and
  when the ask is longer the auxiliary model renames it with a short title.
  The work and the answer happen in the thread. The bot needs Create Public
  Threads and Send Messages in Threads; if it cannot open the thread, it
  answers in the channel instead.
- `"reply_in_place"`: the answer is a reply in the channel. Each message is
  its own conversation, and a reply to one of the bot's messages continues
  it.
- `"disabled"`: like `reply_in_place`, and messages typed inside threads are
  refused.

`flat_channels` keeps the channels it names on the reply-in-place surface
even when threads are on. Use it for busy channels where a thread per ask
would crowd the channel list.

A thread the bot takes part in is one conversation. The bot takes part once
it has taken an ask there or run a command there, which includes every
thread it opened itself; this survives restarts. In such a thread:

- a message needs no mention;
- a message typed while a turn runs steers that turn (see
  [turns.md](turns.md#steering));
- a message typed after the turn ends is the next turn of the same
  conversation, with what the bot did still in context.

A direct message works the same way.

When the bot opened a thread for an ask and the model answers with silence
(see [turns.md](turns.md#silence)), the empty thread is removed, unless
someone wrote in it or the turn posted something there.

### Closing a thread

Every answer in a thread carries a **Close thread** button under its words.
The person who asked, or one of the room's operators, presses it to close
the thread: Discord archives it, so it leaves the channel's list of open
threads, stays searchable, and opens again as soon as anyone writes in it.
While a turn still runs in the thread, Close waits for it to answer. The
answer to every press is private.

### The thread board

`board_channel` names a channel the bot keeps one message in, edited in
place: every open thread it started, sorted by who moves next. Make the
channel read-only for everyone but the bot, so the board stays the only
message there.

| Section | A thread is here when |
| --- | --- |
| Needs you | its newest words are a bot answer marked as waiting on someone: the turn called `nck:answer-waits`, or put choices on it. The board never guesses from a question mark |
| No answer | someone else spoke last, or nobody did, and nothing runs there |
| Running | a turn runs there, or waits for a free slot |
| Done | the bot answered and its answer waits on nobody |

Each section shows its newest five threads, each a link with how long it has
been quiet; Needs you shows the question each one ends on. **Show all**
lists every open thread to whoever pressed it, and so does `/threads`.

The board closes what is finished: a Done thread after 48 hours with nothing
said in it, any other after 7 days. **Close all done**, for the room's
operators, closes every Done thread at once. The board reads the threads
again every minute and edits its message only when what it says changed.

## The room record

Each channel, thread and DM is a room: a durable session holding what was
said there. The record is what lets the bot remember earlier exchanges.

- Every ask runs in a lane of its own, a fork of the room's record taken
  when the ask arrives. Two asks at once run side by side; neither sees the
  other's answer while it works.
- When a turn ends, the ask and its answer are written back to the room as
  one exchange, so the next ask sees them.
- Context (messages that were not asks) is held, up to 40 lines of 300
  characters, and rides into the next ask.
- A thread's record starts from the channel's record as it stood when the
  thread was made. The thread's exchanges are also written to the channel's
  record, so a later thread sees earlier threads' conversations. The model
  is told where its own thread begins.
- `room_tokens` (default 40000 estimated tokens) bounds a room's history:
  past it the older half is dropped from what lanes see. The log keeps it.
- A lane stays open for replies for 30 minutes after its turn ends. After
  that, a reply to the bot's message starts a new lane on the room's record.

### /new in a channel and in a thread

`/new` (also `/clear`) starts a room's record fresh. It acts on the room
where you type it:

- In a channel: the channel's record goes back to its start. Threads that
  already exist keep their own records.
- In a thread or a DM: that thread's record goes back to its start, and the
  next message there opens a new lane on the cleared record rather than
  continuing the old conversation.

A running turn finishes its answer first. `/undo` and `/redo` rewind the
room's last exchange and bring it back. Any rewind, including one from a
terminal attached to the room's session, starts the next message fresh the
same way.

## Catch-up after downtime

The bot remembers when it last heard Discord. When it starts again (a
restart, a machine that was off), it reads back what the channels said while
it was away and handles each message as if it had just arrived: asks are
answered, other messages become context. The model sees how late each one
is ("sent 12 minutes ago, while you were offline").

- `catch_up_minutes` (default 60) bounds how far back it reads; `0` turns
  catch-up off.
- It reads server text channels, announcement channels and active threads,
  at most 50 messages per channel and 100 in all. Direct messages are not
  read back.
- A message that already got a lane before the gap is skipped.

A short disconnect is different: Discord replays what the bot missed, and no
catch-up is needed.

A turn cut off by a stop or restart answers on its own when the bot is back
(see [turns.md](turns.md#when-the-bot-stops-mid-turn)). A turn that starts
later on its own, such as a scheduled job or a background job finishing,
answers in the room it belongs to.

## Agents

An agent is what the bot is in a room: the folder it works in, its persona,
its standing instructions, its model and its skills. `agents` names them, and
`routes` says which asks go to which, by server, channel, thread, person or
role:

```jsonc
"agents": {
  "release": {
    "prompt": "This is the release channel: keep answers to the release plan.",
    "provider": "deepseek",
    "model": "deepseek-flash",
    "skills": ["release-checklist"]
  },
  "coder": {
    "folder": "~/src/webshop",
    "soul_file": "/home/me/src/webshop/SOUL.md"
  }
},
"routes": [
  {"agent": "release", "channel": "123456789012345678"},
  {"agent": "coder", "user": "222222222222222222"},
  {"agent": "coder", "guild": "111111111111111111", "role": "333333333333333333"}
]
```

What an agent may set:

- `folder`: where its asks work. Their tools run there, they read the
  folder's `AGENTS.md` (or `CLAUDE.md`), and in a git repository its project
  memory is theirs. A folder that does not exist refuses the section at start.
- `soul_file`: its persona, in place of the channel's (see [Persona](#persona)).
- `prompt`: standing instructions for every ask it answers.
- `model`, with an optional `provider`: the model its asks run on. A
  `provider` without a `model` is refused.
- `skills`: skills the model reads before it answers the first ask.

A route names an `agent` and one or more of `guild`, `channel`, `thread`,
`user` and `role`, each an id written as a string. Every key it names must
match. A `channel` route covers the channel's threads. When several routes
match, the most specific wins: `user`, then `thread`, `channel`, `role` and
`guild`, one key outweighing all the keys after it together. Of two equally
specific routes, the one written first wins. `"agent": "default"` sends asks
to no agent, which carves a channel out of a server's route.

An ask no route takes runs as the channel itself: the channel's `soul_file`,
no standing instructions, the organism's model. A reply that continues a
conversation stays with the agent the conversation began with, whoever
replies.

Every agent runs in this one organism: the cells, credentials and store
are shared, and a room keeps one record whichever agent answers there. For an
agent with its own memory, store and bot, run a second profile with a bot
token of its own.

A name that is not one word, an unknown key, or a route naming an agent that
is not defined refuses the whole section at start, naming the entry.
Agents and routes are read at start; restart the channels after changing
them.

### Handing a room to an agent

`/agent` in a room says which agent answers there and why. `/agent NAME`
hands the room to that agent from its next ask, ahead of every route, and a
channel's threads go with it unless one is handed elsewhere itself.
`/agent default` gives the room back to the routes. Conversations already
open in the room end with their current turn, so the next message starts
fresh as the new agent. With operators declared, only they can run it.

### A room's model

`/models` typed in a room changes that room's model and no other. The
organism's default, which the terminal's `/models` moves, stays where it
was. Precedence for an ask's model:

1. the room's own `/models` pick;
2. for a thread, its channel's `/models` pick;
3. the model of the agent the ask runs as;
4. the organism's default.

`/models default` clears the room's pick. On Discord, bare `/models` opens a
picker: see [choices.md](choices.md#the-models-picker).

### The channel topic

The model is told the channel's topic (a thread gets its parent's), cut to
one line, as a label to read and never an instruction to follow. Anyone with
Manage Channels can set a topic, so it carries no authority.

## Persona

The bot's voice in rooms comes from a `SOUL.md` file: `~/.nodecode/SOUL.md`
by default, or the file `soul_file` names. It reaches the model as standing
context after the room's rules. An edit applies from the next message, and
deleting the file removes it. `/channels` shows the file and whether it is
present. A starting point ships beside the channel kit as `kit/SOUL.md`.

## The home channel

`/sethome` typed in a channel makes it the bot's home: where it reports to
you. It posts there:

- that it is back when it starts again, in the model's own words: nodecode
  hands the model a note of how long it was gone, why, and the turns it
  picks up, and posts what the model writes (`Back after a 3-minute restart
  for an update -- picking up the flaky test where I left off.`), after a
  quit of your own too. Each of those turns opens with the bot saying, in a
  sentence, that it is back and what it was doing;
- a new release, once, when nodecode first runs it: its version and what
  changed since the one before, in the same message as the restart that
  brought it (see below);
- who wrote asking to be let in (without the code);
- who was refused a command.

The same lines reach every attached terminal. `/sethome off`
clears the home. `/sethome` typed in a terminal says where each bot's home
is. The home is kept across restarts.

The note the model is handed for a release reads:

```
nodecode updated to 0.0.1+063107936 (from 29db67027)
- channels: a turn's card is Components V2
- tui, channels: no surface shows a round's first-token wait
- and 3 more
```

and the model says it in a sentence or three, keeping the version as
written and the changes that matter to you. It is said when an update lands
(`"update": {"mode": "auto"}`, the default on an installed build, or
`nodecode update`), or at the first start on a new release, and never twice
for one release. A terminal shows the same words as a line in its
transcript, and the model reads the note itself once per conversation, so
the bot can answer what changed. An update mode of `off` says nothing.
When no model answers, nothing is said.
