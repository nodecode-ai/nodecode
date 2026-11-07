# Discord

This cell puts Nodecode in a Discord server as a bot. People in the
channels you allow can ask it for work, and it answers there with the same
agent you run in the terminal: same tools, same models, same store.

By default each ask typed in a channel opens a thread off the asker's
message, and the answer lands in that thread. Inside a thread the bot is
part of, follow-ups need no mention. Direct messages work the same way.
While a longer turn runs you see a card of its steps with Stop and Details
buttons, and it stays above the answer as the turn's record.

The shared machinery (rooms, lanes, admission, the turn's card, slash
commands) is the channel kit's, in `../kit/`. This folder holds what is
Discord's own: the gateway connection, the REST calls, the config section
and voice.

Channel messages run with your machine's full authority. The bot refuses to
start until you name who may talk to it.

## Quick start

1. In the [developer portal](https://discord.com/developers/applications),
   create an application, reset the bot token on its Bot page, and save the
   token to a file only you can read, for example
   `~/.nodecode/secrets/discord.token` with mode 600.
2. On the same page, turn on **Message Content Intent** (optional; see
   [setup](docs/setup.md#3-intents)).
3. Invite the bot with this URL, your application id in place:

   ```
   https://discord.com/oauth2/authorize?client_id=APPLICATION_ID&scope=bot+applications.commands&permissions=309237763136
   ```

4. Install the cell: `nodecode add channel-discord`, or `/setup` in the
   terminal.
5. Add a `channels.discord` section to `~/.nodecode/config.jsonc`:

   ```jsonc
   {
     "channels": {
       "discord": {
         "bot_token_file": "/home/you/.nodecode/secrets/discord.token",
         "allowed_users": ["<your user id>"],
         "owner": ["<your user id>"]
       }
     }
   }
   ```

   Give the token file's absolute path. Or tell Nodecode "set up discord;
   the token is in /home/you/.nodecode/secrets/discord.token" and it writes
   the section with you.
6. Restart the channels (the web page's **Restart channels**, or ask
   Nodecode to run `(restart-cells)`). `/channels` in the terminal should
   read `discord: running, connected` (`degraded` without Message Content).
7. In a server channel, write `@YourBot hello`.

[setup.md](docs/setup.md) walks each step in full.

## Guides

- [Setup](docs/setup.md): the application, the bot token, intents,
  permissions, the invite, installing, first run, checking the connection.
- [Rooms and access](docs/rooms.md): who the bot listens to and where,
  mentions, direct messages, pairing, threads, the room record, catch-up
  after downtime, `/new`, agents and routes, the home channel.
- [Turns](docs/turns.md): what you see while the bot works: reactions, the
  card, Stop and Details, what the turn says on its way, the answer, long
  answers, pings, private replies, steering.
- [Commands](docs/commands.md): every slash command in the bot's menu, who
  may run it, and which answers are private.
- [Buttons and pickers](docs/choices.md): clarifying questions with their
  answers on buttons, and the `/models` picker.
- [Files](docs/files.md): attachments the bot reads, files it posts, size
  limits.
- [Voice](docs/voice.md): answers as voice messages; joining a voice
  channel, following people, listening, speaking, meeting notes; Discord's
  end-to-end encryption (DAVE).
- [Configuration](docs/config.md): every key of `channels.discord`, with
  type, default and effect.
- [Troubleshooting](docs/troubleshooting.md): the messages the bot and the
  terminal show when something is wrong, and what to change.

[`config.example.jsonc`](config.example.jsonc) is a commented section to
copy.

## Testing

- `just channels-test`: the kit's and the adapters' tests, in one image
  with no network.
- `just discord-qa` (after `just release`): the built `./nodecode` in a
  scratch home against a loopback Discord (gateway and REST, refusing what
  Discord refuses) and a scripted model. Each scenario says something in
  the server and checks what a person would then see: the thread and the
  reactions, the mention and stranger gates, an edit, long answers, a failed
  turn, Stop, question buttons, the `/models` picker, files both ways, every
  command in the menu, DM pairing, a resume and the catch-up after a
  restart, the note a new release says in the home channel, and the brief
  a restart that cut a turn off says there. `just discord-qa --list` names
  them; `--only NAME,...` runs some.
  It runs in the Linux CI workflow.
