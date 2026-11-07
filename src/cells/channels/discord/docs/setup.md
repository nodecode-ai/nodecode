# Setting up the Discord bot

This page takes you from nothing to a bot that answers in your server. You
need a Discord account that can manage the server you add the bot to, and a
Nodecode install with a working model (`/connect` and `/models` in the
terminal).

## 1. Create the application

1. Open the [developer portal](https://discord.com/developers/applications)
   and choose **New Application**. The name you give it is the bot's name.
2. On **General Information**, copy the **Application ID**. The invite URL
   needs it.

## 2. Get the bot token

1. Open the **Bot** page and choose **Reset Token**. Copy the token.
2. Save it where only you can read it. Nodecode reads the token from a file
   or an environment variable, never from the config itself:

   ```sh
   mkdir -p ~/.nodecode/secrets
   printf '%s' 'PASTE-THE-TOKEN' > ~/.nodecode/secrets/discord.token
   chmod 600 ~/.nodecode/secrets/discord.token
   ```

   Or export it in the environment the organism runs in:

   ```sh
   export DISCORD_BOT_TOKEN='PASTE-THE-TOKEN'
   ```

Prefer the file if the organism runs in the background (`/gateway on`). The
background service is started with only a few variables (`NODECODE_HOME`,
`HOME`, the `XDG_*` folders and `PATH`), so a variable you exported in a
shell is not there, and the bot refuses to start with `bot_token_env names
DISCORD_BOT_TOKEN, which is unset or empty`.

Leading and trailing whitespace in the file or variable is ignored. Never
paste the token into a chat with the bot or into `config.jsonc`.

## 3. Intents

Intents decide which gateway events Discord sends the bot. The adapter asks
for these by default:

| Intent | Bit | Privileged | Why |
| --- | --- | --- | --- |
| Guilds | 1 << 0 | no | channels, threads, topics |
| Guild Messages | 1 << 9 | no | messages in servers |
| Guild Message Reactions | 1 << 10 | no | reactions on the bot's messages |
| Direct Messages | 1 << 12 | no | direct messages |
| Direct Message Reactions | 1 << 13 | no | reactions there |
| Message Content | 1 << 15 | yes | the words of messages that do not mention the bot |
| Guild Voice States | 1 << 7 | no | always: `/voice join` finds you by it |

Message Content is privileged: turn it on under **Privileged Gateway
Intents** on the Bot page.

Without it the bot still runs. Discord then sends the words of a message
only when it mentions the bot, replies to the bot, or is a direct message.
What changes:

- `/channels` reads `discord: degraded — message content intent not
  granted in the Developer Portal: answering mentions, replies and DMs only`.
- In a channel the bot answers mentions and replies only, whatever
  `require_mention` says.
- Inside a thread the bot is part of, a follow-up needs a mention or a
  reply too.
- Chatter that does not mention the bot reaches it without its words, so
  it adds nothing to the next ask's context.

When Discord refuses Message Content (gateway close code 4014), the adapter
drops that intent and connects again without it, so the bot keeps running.

`intents` in the section replaces the whole set with the number you give;
leave it unset unless you know you need it.

## 4. Permissions and the invite

The bot needs these permissions in the channels it works in:

| Permission | Bit | Why |
| --- | --- | --- |
| Add Reactions | 1 << 6 | the marks on an ask, when `reactions` is on |
| View Channels | 1 << 10 | see the channels it is allowed in |
| Send Messages | 1 << 11 | answer |
| Attach Files | 1 << 15 | tables drawn as pictures, files an answer carries |
| Embed Links | 1 << 14 | each turn's card, an embed |
| Read Message History | 1 << 16 | replies, reactions, catch-up after downtime |
| Create Public Threads | 1 << 35 | a thread per ask (the default) |
| Send Messages in Threads | 1 << 38 | answer in that thread |
| Connect | 1 << 20 | voice only |
| Speak | 1 << 21 | voice only |

The sum for text is `309237763136`; with voice it is `309240908864`. Embed
Links draws each turn's card; every other message the bot sends has its link
previews suppressed.

Open this URL with your application id in place of `APPLICATION_ID`, pick
the server, and authorize:

```
https://discord.com/oauth2/authorize?client_id=APPLICATION_ID&scope=bot+applications.commands&permissions=309237763136
```

With voice, use `permissions=309240908864`. The `applications.commands`
scope is what lets the bot's slash commands appear in the server. If you
invited the bot without it, open the URL again.

After step 4, `nodecode gateway setup discord` does steps 5 to 8 in the
terminal: it turns the cell on, keeps the token you paste in a 0600 file,
checks it with Discord, offers the channels by name and starts the bot.
The steps below are the same walk by hand.

## 5. Install the cell

The Discord cell ships with Nodecode but is not installed in a new home.
Install it, with the channel kit it depends on, in one of these ways:

- `nodecode add channel-discord` from a shell.
- `/setup` in the terminal, then the cells step.
- The web page (`nodecode web`): **Control**, then the **Cells** tab.

Until the section exists, the terminal shows a standing notice that
`channels.discord` is absent.

## 6. Write the section

Add `channels.discord` to `~/.nodecode/config.jsonc`. A present section is
enabled; `"enabled": false` turns it off.

```jsonc
{
  "channels": {
    "discord": {
      "bot_token_file": "/home/you/.nodecode/secrets/discord.token",
      "allowed_users": ["123456789012345678"],
      "owner": ["123456789012345678"]
    }
  }
}
```

The section must name at least one of `allowed_channels`, `allowed_users`
or `allowed_roles`, or the bot refuses to start. Ids are strings. Exactly
one of `bot_token_file` (an absolute path) and `bot_token_env` (a variable
name) is set.

To find ids, you can:

- Ask Nodecode to run `(nck:probe "discord")`. It checks the token and
  lists the bot's identity, each server it is in with its text channels and
  their ids, and whether Message Content is granted. It never prints the
  token.
- Turn on Developer Mode in Discord (User Settings, Advanced). Right-click a
  user, channel or server and choose **Copy ID**.

You can also let Nodecode write the section. In the terminal, say "set up
discord; the token is in /home/you/.nodecode/secrets/discord.token". It
writes the members through `config-set`, probes the token, offers channels
by name, and restarts the cells. The web page's **Control**, **Channels**
tab has a **Set up** panel for the same section.

[config.md](config.md) lists every key. [rooms.md](rooms.md) explains how
the lists combine.

## 7. Start it

Restart the channels so they read the new section:

- the web page: **Control**, **Channels**, **Restart channels**;
- or ask Nodecode to run `(restart-cells)`;
- or `nodecode gateway restart` when the gateway runs in the background.

The bot answers whether or not a terminal is open. When a bot is running,
the background gateway turns itself on once, unless you turned it off with
`/gateway off`.

A throwaway organism (`nodecode --ephemeral`) never starts a channel: the
status reads `refused — not started: this organism is ephemeral`.

One bot token runs in one organism at a time. A second profile that
configures the same token is refused and told which profile and process
hold it.

## 8. Check the connection

1. In the terminal, run `/channels`. A healthy bot reads:

   ```
   discord: running, connected, 3 delivered, soul /home/you/.nodecode/SOUL.md (absent)
   ```

   Every state change also shows as a notice on attached shells.
2. In Discord the bot shows as online, and its commands appear when you type
   `/` in the server. The menu is published shortly after the bot connects.
3. In a channel the bot may use, write `@YourBot hello`. By default a thread
   opens off your message and the answer lands there.

If any step fails, see [troubleshooting.md](troubleshooting.md).

## Next steps

- Decide who may talk to the bot and where: [rooms.md](rooms.md).
- Hand a channel, a person or a role to an agent with its own folder,
  persona, instructions, model and skills: [rooms.md](rooms.md#agents).
- Give the bot a voice in rooms with a `SOUL.md` file:
  [rooms.md](rooms.md#persona).
- Choose a home channel for the bot's reports: type `/sethome` there.
