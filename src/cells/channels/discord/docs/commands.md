# Slash commands

The bot's slash commands are Nodecode's own commands: the ones that can run
without a terminal, plus the commands of the cells you have loaded. `/help`
in a room lists exactly what that bot can run.

## The menu

When the bot connects, it publishes its commands as the application's global
commands, so they show when you type `/` in a server or a DM with the bot.

- The publish replaces the application's whole command list. Commands that
  another tool registered for the same application are removed.
- It publishes again whenever the list changes shape, for example when a
  cell that brings a command is loaded later.
- A command's aliases are listed as commands of their own (`/new` beside
  `/clear`).
- A name Discord would refuse is left out of the menu. Descriptions are cut
  to 100 characters.
- A command that takes arguments has one optional text option, `args`, which
  holds everything after the command name. For `/models`,
  `/usage`, `/think`, `/agent` and `/voice` the option offers completions
  while you type, up to 25, and only to people allowed to run the command.
  `/agent` offers the agents of the room it is typed in, each with its
  folder, then `default`. `/voice` offers the verbs that would do something
  now: `join` while the bot sits nowhere, `leave`, `notes` and `say` while
  it sits in a voice channel, `notes stop` while notes are taken, and
  always `status`, `on`, `tts` and `off`.

The bot needs the `applications.commands` scope from its invite for the menu
to appear (see [setup.md](setup.md#4-permissions-and-the-invite)).

## Running a command

From the menu, the bot first acknowledges the command (Discord shows the bot
thinking), then replaces that with the answer. Holding first means a slow
command such as `/doctor` still answers in place. A long answer's first part
replaces the acknowledgement and the rest follow as messages in the channel.

You can also type a command as a message. A message whose text, with the
bot's mention removed, starts with `/` and a letter is a command, never a
prompt. In a channel that needs a mention, mention the bot first:

```
@YourBot /models
```

In a DM or a thread the bot takes part in, no mention is needed. A typed
command is answered as a reply in the room, never privately. Note that a
prompt that starts with a slash, such as `/etc/hosts is broken`, is also read
as a command and answered as an unknown one.

A command acts on the room you run it in: a thread is its own room, separate
from its channel.

## Who may run commands

With operators declared (`owner`, or a single `allowed_users` entry), every
command but `/help` is the operators'. Anyone else is refused, privately,
with "Only this room's operators can run /NAME" and an **Ask for access**
button. The operators' terminals (and the home channel, if set) hear who was
refused what, at most once every ten minutes per person.

**Ask for access** posts one request into the channel, mentioning the
operators, with a **Make NAME an operator** button (at most once every ten
minutes per person). An operator's press adds that person to `owner`, live at
once and saved in the config, and tells them in the channel; anyone else's
press changes nothing.

With no operator declared, everyone admitted to the room may run every
command. See [rooms.md](rooms.md#the-operator).

Someone the room does not admit at all gets "not allowed here: REASON" when
they use the menu. A typed command from them gets no answer, except the
pairing code a stranger's DM is offered.

## Private answers

The answers of `/stop`, `/channels`, `/models` and `/threads` run from the menu
are private: only the person who ran them sees them. So is every refusal from the
menu. A private answer is one message; text past 2,000 characters is not
sent. Every other answer is visible to the room.

## The commands

### Commands of the room

| Command | What it does in a room |
| --- | --- |
| `/help` | Lists the commands this bot can run, one per line. Open to everyone. |
| `/stop` | Stops the newest turn running in this room. In a thread, that thread's turn. Private. |
| `/clear`, `/new` | Starts the room's record fresh. See [rooms.md](rooms.md#new-in-a-channel-and-in-a-thread). |
| `/undo` | Rewinds the room's last exchange. |
| `/redo` | Returns to the exchange the last rewind left. |
| `/rename [name \| default]` | Names the room's session, the name the terminal's `/sessions` and the web sidebar show. Bare, shows the name. |
| `/models [provider] [model]` | The model this room runs on. See below. Private. |
| `/context` | The room session's context window and usage. |
| `/evict [percent]`, `/compact` | Drops the oldest part of the room session's history from the model's context (default 50%). |
| `/sethome [off]` | Makes this channel the bot's home channel; `off` clears it. See [rooms.md](rooms.md#the-home-channel). |
| `/agent [NAME \| default]` | Which agent answers in this room and why; `NAME` hands the room to that agent, `default` gives it back to the routes. See [rooms.md](rooms.md#handing-a-room-to-an-agent). |

### `/models` in a room

`/models` in a room is the room's own: a pick changes this room's model and
no other, and the organism's default stays where it was.

| Form | Effect |
| --- | --- |
| `/models` | On Discord, the model picker card. See [choices.md](choices.md#the-models-picker). |
| `/models PROVIDER` | On Discord, the picker's list of that provider's models. |
| `/models MODEL` | Picks MODEL for this room, when exactly one configured provider lists it. |
| `/models PROVIDER MODEL` | Picks PROVIDER's MODEL for this room. |
| `/models default` (or `reset`) | Clears this room's pick. |
| `/models list [PROVIDER]` | The catalog as text: every provider with its model count, or one provider's models. |
| `/models aux [PROVIDER] [MODEL]` | Shows or changes the organism's auxiliary model, which naming threads, recaps and summaries run on. A pick changes it for the whole organism, not the room. |
| `/models aux default` | Gives that work back to each session's own model. |

A pick takes effect from the room's next ask. A thread with no pick of its
own follows its channel's. A model no provider lists is refused, and so is
one that two providers list (name the provider then).

### Commands of the whole organism

These act on Nodecode itself, not on the room. They are the same commands
the terminal has.

| Command | What it does |
| --- | --- |
| `/think [rung]`, `/effort` | Shows the reasoning effort and the rungs the model offers; with a rung, sets it on the room's own session and makes it the default every session opened after it starts at, so the lanes asks open from then on run at it too. |
| `/usage [today \| 7d \| 30d \| all]` | Spend across every session, as text. |
| `/doctor` | What this organism is missing, and what to do about it. |
| `/cells` | The cell folders loaded and how each stands. |
| `/backup` | Writes a backup archive of the whole home under `~/.nodecode/backups/` on the machine running Nodecode. It holds your keys. |
| `/profile [verb]`, `/profiles` | Profiles: the roster, or one of its verbs. |
| `/gc [reset]` | Garbage collection counters. |

### Commands of the channel cells

| Command | What it does |
| --- | --- |
| `/channels` | The adapters' status lines, as in the terminal. Private. |
| `/channels pair CODE` | Lets in the person a pairing code was given to. |
| `/channels unpair USER-ID` | Takes a pairing back. |
| `/channels paired` | Who is paired, and how many codes wait. |
| `/threads` | Every open thread the bot started, by who moves next, when `board_channel` is set. See [rooms.md](rooms.md#the-thread-board). Private. |
| `/voice [join \| leave \| status \| on \| tts \| off \| notes [stop] \| say WORDS]` | The voice channel, voice-message answers in the room it is typed in, and meeting notes. Bare, a card with Join or Leave, notes, and a menu for the room's voice messages. See [voice.md](voice.md#the-voice-card). |

### Commands of other cells

Each loaded cell that declares a command adds it to the menu, beside the
core's `/index`: for example `/cron`, `/team`, `/link`, `/qa`, `/prs`,
`/import`, `/experience` and `/chrome`. What they do is each cell's own;
`/help` lists the ones present. One that opens a panel in the terminal (the
`/index` picker, the `/link` code) answers in a room with its words alone.

### Not available in a room

Commands that need a terminal (pickers and shell moves such as `/sessions`,
`/tree`, `/fork`, `/history`, `/themes`, `/details`, `/export`, `/execs`,
`/gateway`, `/web`, `/connect`, `/setup`, `/exit`) are not in the menu.
Typed as a message, they answer "/NAME needs an interactive shell". An
unknown name answers "unknown command /NAME; /help lists them".
