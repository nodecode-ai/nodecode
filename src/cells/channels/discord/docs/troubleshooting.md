# Troubleshooting

## Where to look

- **`/channels`**, in the terminal or in Discord: one status line per
  channel adapter, for example
  `discord: running, connected, 12 delivered, soul /home/you/.nodecode/SOUL.md (present)`.
  The part after `—` is the latest error, when there is one.
- **Notices** in attached terminals: every change of the bot's state shows
  as a notice, with the same line.
- **The web page** (`nodecode web`): **Control**, **Channels** tab. Each
  channel shows its state, when it last heard a message, **Last error**,
  and **Last refused** (the reason the newest refused message was refused).
  **Check** asks Discord what the token can see.
- **`(nck:probe "discord")`**: ask Nodecode to run it. It checks the token
  and lists the bot's servers, their text channels with ids, and whether
  Message Content is granted.
- **`(nck:channel-status "discord")`**: the raw status, including
  `:last-rejection`, which the terminal's `/channels` line does not show.
- **The log**: warnings (refused calls, failed threads, failed reactions)
  go to the organism's log. When the gateway runs in the background,
  `nodecode gateway logs` shows its recent lines.

## Status states

| State | Meaning |
| --- | --- |
| `starting` | The bot is starting. Transient. |
| `running` | Connected and working. |
| `degraded` | Running without Message Content, or stopped by a fatal close code (`connected` absent). The detail says which. |
| `stopped` | The cell was stopped. |
| `refused` | The section is present but the bot would not start. The detail says why. |
| `unconfigured` | The cell is installed but `channels.discord` is absent. |

## The bot does not start

`/channels` reads `discord: refused — ...`. The reasons, as written:

| Message | What to change |
| --- | --- |
| `channels.discord refuses to start open to the world: populate at least one of allowed_channels, allowed_users, allowed_roles (channel messages run with full host authority)` | Name who may talk to it. See [rooms.md](rooms.md#who-may-talk-to-it). |
| `one of bot_token_env / bot_token_file is required (secrets are never inline)` | Add one of them. |
| `exactly one of bot_token_env / bot_token_file may be set, both are` | Remove one. |
| `bot_token_env names DISCORD_BOT_TOKEN, which is unset or empty` | Set the variable in the environment the organism runs in. The background gateway does not see variables exported in a shell; use `bot_token_file` instead. |
| `bot_token_file names PATH, which is empty` | Put the token in the file. |
| `bot_token_file: cannot read PATH: ...` | Fix the path (use an absolute one) or the file's permissions. |
| `channels.discord: this bot token is held by profile NAME, pid N — one organism per token; stop that one, or give this profile a token of its own` | Another profile's organism runs this bot. Stop it, or make a second application for this profile. |
| `not started: this organism is ephemeral (--ephemeral); the live organism keeps the channel` | Expected for `--ephemeral`. Run the bot from your normal organism. |
| `... must be one of ..., got ...`, `... must be a string ...`, `... must be >= ...` | A key has the wrong type or value. See [config.md](config.md). |
| `channels.discord.agents.NAME: no setting "..."` | A misspelled member of an agent. See [rooms.md](rooms.md#agents). |
| `channels.discord.agents.NAME: a provider needs its model` | Add `model` beside `provider`. |
| `channels.discord.agents.NAME: folder: ...` | The agent's folder is not there. Create it or fix the path. |
| `channels.discord.agents.NAME: an agent's name is one word, and not default` | Rename the agent. |
| `channels.discord.routes[N]: no agent "..."; ...` | The route's `agent` is misspelled, or the agent is not defined. Routes count from 0. |
| `channels.discord.routes[N]: matches nothing: ...` | Add a `guild`, `channel`, `thread`, `user` or `role` to the route. |
| `channels.discord.routes[N]: user is an id, written as a string` | Quote the id: a Discord id is too long for a JSON number. |

After fixing the section, restart the channels.

## The bot does not connect

| Line | Meaning | What to change |
| --- | --- | --- |
| `discord: degraded — discord_gateway_fatal_close_4004` | Discord rejected the token. | Reset the token on the Bot page, save the new one, restart the channels. |
| `discord: degraded — discord_gateway_fatal_close_4013` | The `intents` number is not valid. | Fix or remove `intents`. |
| `discord: degraded — discord_gateway_fatal_close_4014` | An intent you asked for in `intents` is privileged and not granted. (A missing Message Content alone does not stop the bot.) | Grant it on the Bot page, or remove it from `intents`. |
| `discord: degraded — discord_gateway_fatal_close_4010`, `4011`, `4012` | Discord's shard and API version errors. The adapter does not shard. | If you set `gateway_url`, remove it. |
| `discord: degraded — message content intent not granted in the Developer Portal: answering mentions, replies and DMs only` | The bot runs without Message Content. | Turn on Message Content Intent on the Bot page, then restart the channels. Or keep `require_mention: true` and accept that it answers mentions, replies and DMs only. See [setup.md](setup.md#3-intents). |

A fatal close stops the connection for good: after fixing the cause, restart
the channels. Other disconnects are retried on their own within seconds, and
the bot resumes the session when Discord allows it. A connection that stops
acknowledging heartbeats for 15 seconds, or that delivers nothing for 4 hours,
is reconnected.

If `(nck:probe "discord")` answers
`token: unauthorized (401): the token is wrong or revoked`, the token is
wrong. If it answers `guilds: none — invite the bot with the OAuth2 URL from
the developer portal`, the bot is in no server yet.

## The bot says nothing

First check that `/channels` reads `running, connected`. Then look at **Last
refused** on the web page, or `:last-rejection` in
`(nck:channel-status "discord")`. A refused message leaves one of these
reasons:

| Reason | Meaning | What to change |
| --- | --- | --- |
| `user_not_allowed` | The author is not in `allowed_users`, holds no role from `allowed_roles`, and is not paired. | Add them, or pair them. |
| `channel_not_allowed` | The channel (or its parent, for a thread) is not in `allowed_channels`. Also every DM when `allowed_channels` names specific channels. | Add the channel. For DMs, see [rooms.md](rooms.md#direct-messages). |
| `guild_not_allowed` | The server is not in `allowed_guilds`. Also every DM when `allowed_guilds` is set. | Add the server, or empty `allowed_guilds`. |
| `channel_ignored` | The channel is in `ignored_channels`. | Remove it there. |
| `dm_disabled` | `dm_policy` is `"disabled"`. | Set it to `"allow"`. |
| `threads_disabled` | `thread_behavior` is `"disabled"` and the message was in a thread. | Change `thread_behavior`. |
| `bot_author` | Another bot wrote it and `allow_bots` is `"none"`. | Set `allow_bots`. |
| `bot_not_mentioned` | Another bot wrote it without mentioning this bot, under `allow_bots: "mentions"`. | Expected. |
| `self_author` | The bot's own message. | Expected. |
| `mention_required_without_bot_user` | The channel needs a mention, but the bot does not know its own user id. | See [below](#mentions-are-not-recognized). |

A message can also pass the checks and still not be an ask. These are not
recorded as refusals: the message becomes context for the next ask instead.

- The channel needs a mention and the message has none (no mention, no reply
  to the bot, no wake word). Mention the bot, reply to one of its messages,
  or list the channel in `free_response_channels`.
- The message opens by addressing someone else (another user's mention, or a
  name from `other_addressees`).

Other causes:

- Without Message Content, Discord sends the bot no words for messages that
  do not mention it, even in its own threads.
- A message with no text and no attachments is ignored.
- The model may have chosen silence (`NO_REPLY`). Nothing is posted then;
  see [turns.md](turns.md#silence).
- The ask may be waiting: its card reads `Queued · N ahead`. An operator's
  press on its **Raise limit** doubles `max_concurrent_turns` and lets the
  queue through.

### Mentions are not recognized

The adapter needs the bot's own user id to recognize a mention. It reads it
from Discord at start when `require_mention` is true or
`require_mention_channels` is set. If that read fails, the log says
`discord: could not hydrate bot user id (...); mention-gated guild messages
will be rejected until channels.discord.bot_user_id is set`. Set
`bot_user_id` to the id the probe prints.

A mention of the bot's role (the one Discord made for the bot) counts as a
mention once the adapter has read which roles the bot holds. If that read
fails, the log says `discord: could not read the bot's guilds (...); a
<@&role> mention of the bot will read as mention_required`.

## Commands

| Symptom | Cause and fix |
| --- | --- |
| No commands when typing `/` | The invite lacked the `applications.commands` scope: open the invite URL again. Or the publish failed: the log says `discord command menu not published: ...`. |
| "/NAME is the operator's; /help lists what you can run" | Operators are declared and you are not one. See [rooms.md](rooms.md#the-operator). |
| "not allowed here: REASON" | The room's checks refused you; REASON is from the table above. |
| "/NAME needs an interactive shell" | The command only works in a terminal. |
| "unknown command /NAME; /help lists them" | No such command, or its cell is not loaded. A prompt that starts with `/` is read as a command too. |
| The answer appears in the room instead of privately | The bot could not hold the interaction open in time; the log says `discord: the command's answer could not be held open; it replies in the room`. |

## Threads, reactions and posts

| Log line | Cause and fix |
| --- | --- |
| `discord: ask ID runs in the channel, no thread opened: ...` | The bot could not open a thread, usually a missing Create Public Threads permission. The ask was answered in the channel. |
| `discord: thread ID keeps its first words: ...` | Renaming the thread failed (the model call or Discord's rename). Harmless. |
| `discord reaction on ID for SESSION refused, the lane stops reacting: ...` | Add Reactions or Read Message History is missing. |
| `Discord REST METHOD LABEL forbidden; verify the bot's guild membership, channel permissions, and privileged intents (status 403: ...)` | Discord refused a call: check the bot's permissions in that channel. This line also shows as the `/channels` detail when an answer could not be posted. |
| `Discord REST METHOD LABEL rate limited; retry later (status 429: ...)` | Discord's rate limit. Answers, final edits, deletions and reactions are retried up to three times before this shows. |
| `discord: an attachment at HOST is not fetched: not a Discord host` | A message carried a file URL outside Discord's file hosts. It was dropped on purpose. |

## Messages the bot posts in the room

| Message | Meaning |
| --- | --- |
| `Failed at ... > DETAIL` (the turn's card, red) | The turn failed. DETAIL is the error. |
| `The answer could not be posted > DETAIL` (the turn's card, red) | The turn answered and Discord refused the answer's message. DETAIL is Discord's reason; the card keeps the steps, Details and each step's Output. |
| `Stopped at ... > REASON` (grey) | The turn was stopped; REASON says by whom. |
| `Paused at ... the bot stopped; this picks up where it left off when it is back` | The bot shut down mid-turn. The turn resumes when it starts again. |
| `cancelled · ... the bot stopped before this ran: ask again when it is back` | The ask was still queued when the bot shut down. Ask again. |
| `the turn ended without an answer — nothing was posted.` | The model replied with no words. Ask again, or try another model. |
| `turn failed: DETAIL` | A turn failed after its conversation had closed. |
| `[the recording "NAME" could not be transcribed: ...]` (in the model's prompt) | See [files.md](files.md#transcription). The usual cause is `no transcriber is installed`: run `(nck:install-transcriber)`, or set a hosted `transcription.base_url`. |

## Voice

| Message | What to change |
| --- | --- |
| `voice: sit in a voice channel first, then /voice join` | Join a voice channel yourself, then type it again. Or set `voice_channel_id` for a channel of the bot's own. |
| `voice: Discord did not seat me in #channel. The bot needs the Connect and Speak permissions there ...` | Grant Connect and Speak to the bot's role in that channel, or invite the bot again with the link it gives. A full channel answers the same. |
| `voice: #channel is no voice channel of a server the bot is in` | `voice_channel_id` names a channel the bot cannot see. |
| `voice: nobody is allowed to speak (channels.discord.voice_speakers)` | Set `voice_speakers` or `allowed_users`. |
| `voice: the bot does not know its own user id yet` | Reading it from Discord failed at start (see the log). Set `bot_user_id`. |
| `voice: the Discord gateway is still connecting; try again in a moment` | Wait for `running, connected`. |
| `voice: Discord requires its end-to-end encryption library for voice, and it is not here: ...` | `libdave` could not be downloaded or has no build for this machine. |
| `encryption: waiting for the group — nobody else is in the channel yet` (in `/voice status`) | Normal while the bot is alone in the voice channel. Join it yourself. |
| `voice: could not say that out loud (...). The answer above is the answer.` | Speaking failed: often `ffmpeg` missing or no voice installed (`(nck:install-speaker)`). |

See [voice.md](voice.md).
