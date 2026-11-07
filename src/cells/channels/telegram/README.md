# Telegram

This cell puts Nodecode in Telegram as a bot. People you allow can message
it in a private chat or a group, and it answers there with the same agent
you run in the terminal: same tools, same models, same store.

Each ask runs on its own, so two asks in one chat run at the same time.
Reply to one of the bot's messages to keep talking about that answer. While
a longer turn runs you see a card of its steps with Stop and Details
buttons, and it stays above the answer as the turn's record.

The shared machinery (rooms, lanes, admission, the turn's card, commands,
voice) is the channel kit's, in
[`../nodecode-channel-kit/`](../nodecode-channel-kit). This folder holds
what is Telegram's own: the getUpdates long poll, the Bot API calls, the
config section and the setup probe.

Channel messages run with your machine's full authority. The bot refuses to
start until you name who may talk to it.

## Quick start

With the token (step 1) and a message sent to the bot (step 3),
`nodecode gateway setup telegram` does the rest in the terminal: it turns the
cell on, keeps the token you paste in a 0600 file, checks it, offers the
chats by name and starts the bot. By hand:

1. Message [@BotFather](https://t.me/BotFather), send `/newbot`, and pick a
   name. Save the token it answers with to a file only you can read, for
   example `~/.nodecode/secrets/telegram.token` with mode 600, or put it in
   an environment variable.
2. Turn the cell on: `nodecode add channel-telegram`, or `/setup` in the
   terminal. It ships with Nodecode, so nothing is downloaded.
3. Send your new bot a message. Telegram doesn't let a bot list its chats,
   so a chat only shows up for setup once it has written to the bot.
4. Add a `channels.telegram` section to `~/.nodecode/config.jsonc`:

   ```jsonc
   {
     "channels": {
       "telegram": {
         "token_file": "/home/you/.nodecode/secrets/telegram.token",
         "allowed_users": ["<your user id>"],
         "owner": ["<your user id>"]
       }
     }
   }
   ```

   Or tell Nodecode "set up telegram; the token is in
   /home/you/.nodecode/secrets/telegram.token". It checks the token and
   lists the chats and people that have written to the bot, so you pick ids
   by name. `/setup` in the terminal offers the same panel.
5. Restart the channels (the web page's **Restart channels**, or ask
   Nodecode to run `(restart-cells)`). `/channels` in the terminal should
   read `telegram: running, connected`.
6. Message the bot `hello`.

### In a group

Add the bot to the group and put the group's chat id (it is negative) in
`allowed_chats`, or keep `allowed_users` as the gate. With
`require_mention` on (the default) the bot answers when you @mention it or
reply to one of its messages.

Telegram hides most group messages from bots. Mentions and replies to the
bot still reach it, which is all `require_mention` needs. To let it read
every message, send @BotFather `/setprivacy`, choose the bot, pick
**Disable**, then remove the bot from the group and add it again.

In a forum group each topic is its own room. `allowed_threads` limits the
bot to some topics; empty admits all of them.

### A topic per ask

In a forum group where the bot is an admin with **Manage Topics**, an ask
typed outside a topic opens a topic of its own, named from its first words,
and the answer lands there. Inside a topic the bot opened, follow-ups need no
mention and a line typed while a turn runs steers it; topics people opened
keep the mention rule. An answer in a topic carries **Close thread**: the
asker or an owner closes the topic (Telegram locks it until an admin
reopens it from its menu). A turn that answers nothing removes the topic it
opened. Groups that are no forum, and private chats, answer where the ask
was typed. `thread_behavior: "reply_in_place"` keeps a forum flat too, and
`flat_channels` names forums to keep flat.

### The thread board

`board_chat` names a chat the bot keeps one message in, edited in place:
every open topic it opened, by who moves next. **Needs you**: its answer
waits on you (the turn marked it, or it asked with buttons). **No answer**:
someone spoke after the bot, or nobody has. **Running**: a turn runs or
waits there. **Done**: the bot answered and asked nothing. Each topic is a
link that opens it. A done topic closes after 48 hours quiet, any other after
7 days. **Show all** sends you every open topic in your chat with the bot;
**Close all done**, an owner's, closes every done topic; `/threads` lists
them too. Telegram lets a bot list neither a group's topics nor their
messages, so the board knows the topics it opened while it ran, as their
updates and its own posts showed them, kept across restarts.

### Pings

A mention the bot writes, `[name](tg://user?id=…)`, tags and notifies that
person; the room's instructions tell the model so. The bot's own notices to
its owners (an access ask, a raised limit) tag them the same way.

### Who else may talk, and where it answers freely

- `allowed_roles`: `"administrator"` lets every admin of a group drive the
  bot as a listed user does (its creator too); `"creator"` only the
  creator. Read off the group's admin list every five minutes.
- `free_response_chats`: chats where every allowed message opens a turn,
  no mention needed (the bot must read every message: `/setprivacy`
  Disable). `require_mention_chats` asks for a mention in chats even when
  `require_mention` is off. `ignored_chats` are never read.
- `mention_patterns`: wake words that address the bot as its @handle does
  (`"nodecode"`), anywhere in a line, case aside.
- `other_addressees`: names that, opening a line (`"ada, ..."`), make it
  someone else's: observed, never answered.

## What you see

- **The card.** A turn that takes a while posts one message that grows as
  it works: a pill (blue working, green done, red failed), the task, what it
  is doing now, its steps ticked off as they land, and its numbers in the
  footer. **Stop** ends the turn; **Details** shows whoever pressed it
  what the turn did.
- **Words as they're written.** With `stream` on (the default) the answer
  appears in a message that is edited as the model writes. When it is done
  the answer posts fresh as a reply to your ask, so it notifies you, and the
  draft is deleted.
- **Markdown.** Answers are Telegram rich messages: bold, lists, code,
  quotes and tables read as they do anywhere else. A very long answer comes
  in parts of up to 16,384 characters.
- **Reactions.** With `reactions` on, your ask carries 👀 while its turn is
  active, then 👍 when the answer lands or 👎 if the turn fails. A
  reaction you leave on one of the bot's messages is never an ask: it
  becomes a line of context ("reacted 🔥") that the next ask carries. In a
  group Telegram tells only an admin bot about reactions.
- **Edits.** Editing a message so that it mentions the bot, or says one of
  its `mention_patterns`, turns it into an ask. Other edits are ignored, as
  is an edit to a message the bot already took.
- **Mentions.** When the bot pings someone, such as an owner it needs, it
  names them as a link to the person, which Telegram notifies.
- **Private answers.** A command whose answer is private (`/models`,
  `/think`, `/effort`, `/stop`, `/channels`, `/threads`), typed in a group, is answered in your own chat
  with the bot, so the group doesn't read it. If you never opened that chat
  (Telegram refuses a bot's first word), it answers in the group.
- **Commands.** The bot's `/` menu is Nodecode's command list: `/new` starts
  the chat fresh, `/stop` ends a running turn, `/models` shows or changes
  the chat's model, and so on. When `owner` is set, every command but
  `/help` is the owners' alone.

## Files and voice

- **Files in.** The bot reads photos (the largest size sent), documents,
  videos, audio files, voice notes and round video notes. A caption counts
  as the ask's text. A file in the message you reply to comes along too.
- **Voice in.** A voice note reaches the agent as its transcript. Locally by
  default, so the audio never leaves your machine; the `transcription`
  section can point at an OpenAI-compatible server instead.
- **Files out.** The agent can send files back: a picture shows inline,
  anything else comes as a document.
- **Voice out.** `voice_replies` adds a voice message under an answer: `on`
  answers a voice note with one, `tts` answers every ask with one, `off`
  (the default) never does. It speaks with the `speech` section's voice and
  needs ffmpeg.

The agent can also call any Bot API method as the bot from its eval tool
(`nct:request`): pin a message, react, read a chat or a member, change the
command menu. The token never appears in what it reads back.

## After the bot was away

Telegram keeps what the bot missed for 24 hours and hands it over when the
bot polls again. A message from the last `catch_up_minutes` (60 by default)
is answered as if it had just arrived, and the model reads how late it is
("sent 12 minutes ago, while you were offline"). Older ones, and any that
already got an answer before the gap, are skipped; `0` answers none of
them.

## Who may talk to it

- `allowed_users` and `allowed_chats` are the gate. At least one of them
  must name someone or somewhere, or the bot won't start.
- `owner` names whose word is standing policy in the chat. An owner may
  always talk to the bot.
- With `pairing` on (the default), someone no list names who writes to the
  bot in a private chat gets a pairing code, and you are told.
  `/channels pair CODE` lets them in; `/channels paired` lists who is in and
  `/channels unpair USER-ID` takes someone out.

## Configuration

Every key of `channels.telegram`. A present section is enabled;
`"enabled": false` turns it off.

| Key | Default | What it does |
| --- | --- | --- |
| `token_env` | | Environment variable holding the bot token. Use this or `token_file`. |
| `token_file` | | File holding the bot token. The token never goes in the config itself. |
| `allowed_chats` | | Chat ids the bot answers in. A group's id is negative; a private chat's is the user's id. |
| `allowed_users` | | User ids allowed to drive the bot. |
| `owner` | | User ids whose word is standing policy. |
| `pairing` | `true` | Answer unknown private chats with a pairing code. |
| `require_mention` | `true` | In groups, answer only when @mentioned or replied to. |
| `allowed_roles` | | `"administrator"`, `"creator"`: a group member with that standing may drive the bot. |
| `allowed_threads` | all | Forum topic ids the bot answers in. |
| `free_response_chats` | | Chats where every allowed message opens a turn, no mention needed. |
| `require_mention_chats` | | Chats that need a mention even when `require_mention` is off. |
| `ignored_chats` | | Chats the bot never reads. |
| `mention_patterns` | | Wake words that address the bot as its @handle does. |
| `other_addressees` | | Names that make a line someone else's when it opens with one. |
| `thread_behavior` | `"create_per_message"` | In a forum group, open a topic per ask; `"reply_in_place"` or `"disabled"` answer where it was typed. |
| `flat_channels` | | Forum chat ids where asks are answered in place. |
| `board_chat` | | A chat the bot keeps the [thread board](#the-thread-board) in. |
| `catch_up_minutes` | `60` | How old a message from while the bot was away may be and still be answered. `0` answers none. |
| `allow_private_chats` | `true` | `false` ignores private chats. |
| `allow_group_chats` | `true` | `false` ignores groups. |
| `allow_forum_topics` | `true` | `false` ignores forum topics. |
| `bot_username` | from getMe | The bot's @handle, for the mention check. |
| `reactions` | `false` | Mark each ask with 👀, then 👍 or 👎. |
| `stream` | `true` | Show the answer as it is written. |
| `voice_replies` | `"off"` | `"on"`, `"tts"` or `"off"`; see [Files and voice](#files-and-voice). |
| `turn_budget_minutes` | `0` | Minutes a turn may run before it must wrap up with what it has. `0` sets no limit. |
| `room_tokens` | `40000` | The most history a chat keeps, in estimated tokens. Past it the older half is dropped from the chat's record (recall can still find it). `0` keeps everything. |
| `soul_file` | | A `SOUL.md` whose text is the chat's standing persona. |
| `agents` | | Named agents a chat's asks may run as: a folder to work in, a prompt, a model, skills. |
| `routes` | | Which asks run as which agent, by chat, topic or user id. |
| `allowed_updates` | `["message", "edited_message", "callback_query", "message_reaction"]` | Update kinds the poll asks Telegram for. A list that leaves one out turns that feature off; widen it for a layer that watches membership changes. |
| `max_concurrent_turns` | `4` | Turns that may run at once; more wait their turn. |
| `status_update_ms` | `2000` | How often a running turn's card is edited, in ms. |
| `text_chunk_limit` | `16384` | The most characters one message carries. |
| `delivery_workers` | `4` | Threads that post to Telegram at once. |
| `api_base` | `https://api.telegram.org` | The Bot API server, for a local one. |
| `request_timeout_seconds` | `60` | How long one Bot API call may take. |
| `get_updates_timeout_seconds` | `2` | The long poll's window. Short on purpose: it is also how long a stop waits. |
| `poll_interval_ms` | `1000` | Pause between polls. |

[`config.example.jsonc`](config.example.jsonc) is a commented section to
copy.

## Troubleshooting

- **`unauthorized (401)`:** the token is wrong or was revoked. Ask
  @BotFather for a new one (`/token`) and save it where the config points.
- **A 409 conflict in the log:** another program is polling with the same
  token, for example a second Nodecode or an old bot script. Telegram lets
  only one poll at a time. Stop the other one.
- **Setup lists no chats:** nobody has written to the bot yet, or the
  running lane is holding the update stream. Message the bot, or read the
  chat ids off `/channels`.
- **The bot ignores a group:** check that the group's id is in
  `allowed_chats` (or that you are in `allowed_users`), and mention the bot
  or reply to it. If the log says it could not read the bot's username, set
  `bot_username`.
- **No reactions:** some groups forbid them. Turn `reactions` off there.

## Testing

- From a Nodecode checkout, `just channels-test` runs the kit's and the
  adapters' tests in one image with no network.
- From this repository, `.github/test-cell.sh rooms/nodecode-channel-telegram`
  runs this cell's tests alone against a pinned Nodecode tree, offline.

The tests drive the adapter through a scripted Bot API, so they need no
Telegram account.
