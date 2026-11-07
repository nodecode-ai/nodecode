# Configuration reference

The bot reads the `channels.discord` section of `~/.nodecode/config.jsonc`
when the channel cells start. After an edit, restart them: the web page's
**Control**, **Channels**, **Restart channels**, or ask Nodecode to run
`(restart-cells)`.

A present section is enabled. Ids (users, channels, servers, roles) are
strings, not numbers. Entries in id lists are trimmed. A value of the wrong
type, a choice outside its options, or a number under its minimum refuses
the whole section at start, with the reason in `/channels`.

The section must set at least one of `allowed_channels`, `allowed_users` and
`allowed_roles`, and exactly one of `bot_token_env` and `bot_token_file`.

[`config.example.jsonc`](../config.example.jsonc) is a commented copy of
the common keys.

## The token and the connection

| Key | Type | Default | Effect |
| --- | --- | --- | --- |
| `enabled` | boolean | `true` | `false` keeps the section but does not start the bot. |
| `bot_token_env` | variable name | | The environment variable holding the bot token. |
| `bot_token_file` | file path | | A file holding the bot token. Use an absolute path. Surrounding whitespace is ignored. |
| `intents` | integer | `46721` | The gateway intents bitmask. Replaces the whole default set. See [setup.md](setup.md#3-intents). |
| `bot_user_id` | string | read from Discord | The bot's own user id, read at start. Set it only if that read fails. |
| `request_timeout_seconds` | integer, at least 1 | `30` | How long one call to Discord's REST API may take. |
| `gateway_url` | string | `wss://gateway.discord.gg/?v=10&encoding=json` | The gateway to connect to. For a test stand-in only. |
| `api_base` | string | `https://discord.com/api/v10` | The REST API root. For a test stand-in only; files are also fetched from its host. |

The token is never written in the config, and Nodecode never prints it. A
token can run in one organism at a time.

## Who may talk to it

See [rooms.md](rooms.md#who-may-talk-to-it) for how these combine.

| Key | Type | Default | Effect |
| --- | --- | --- | --- |
| `allowed_channels` | list of channel ids | `[]` (any) | Channels the bot reads. A thread passes when its parent is listed. `"*"` allows all. When set to specific channels, direct messages are refused. |
| `allowed_users` | list of user ids | `[]` | People who may talk to it. `"*"` allows everyone. The owners are added when this list is set. |
| `allowed_roles` | list of role ids | `[]` | A server member holding any of these roles may talk as a listed user does. Not in DMs. |
| `allowed_guilds` | list of server ids | `[]` (any) | Servers the bot reads. When set, direct messages are refused. |
| `owner` | list of user ids | the single `allowed_users` entry, if there is exactly one | The operators: their word is standing policy, and with operators declared, every command but `/help` is theirs. |
| `pairing` | boolean | `true` | A direct message from someone no list allows is answered with a pairing code. |
| `allow_bots` | `"none"`, `"mentions"`, `"all"` | `"none"` | Other bots' messages: ignored, answered when they mention this bot, or treated like people's. The bot's own are always ignored. |
| `dm_policy` | `"allow"`, `"disabled"` | `"allow"` | `"disabled"` refuses direct messages. |
| `group_policy` | `"allow"`, `"disabled"` | `"allow"` | Group chats. Discord messages never arrive as group chats, so this has no effect here. |

## Where it answers

See [rooms.md](rooms.md#where-it-answers-mentions).

| Key | Type | Default | Effect |
| --- | --- | --- | --- |
| `require_mention` | boolean | `true` | In channels not listed below: answer only a mention, a reply to the bot, or a menu command; other messages become context. `false` answers every message and needs the Message Content intent. |
| `free_response_channels` | list of channel ids | `[]` | Channels where every admitted message is an ask. |
| `require_mention_channels` | list of channel ids | `[]` | Channels where only a mention, a reply or a menu command is an ask. |
| `ignored_channels` | list of channel ids | `[]` | Channels the bot does not read at all. Wins over the two lists above. |
| `mention_patterns` | list of strings | `[]` | Wake words: a message containing one, in any case, counts as a mention. |
| `other_addressees` | list of names | `[]` | Names other than the bot's. A message that opens by addressing one (`"ada, ..."`) is context, not an ask, unless it also mentions the bot. |

In each list, a thread matches its own id and its parent channel's.

## Threads and rooms

See [rooms.md](rooms.md#where-it-answers-threads).

| Key | Type | Default | Effect |
| --- | --- | --- | --- |
| `thread_behavior` | `"create_per_message"`, `"reply_in_place"`, `"disabled"` | `"create_per_message"` | An ask in a plain channel opens a thread off its message; or the answer is a reply in the channel; or as `reply_in_place`, with messages inside threads refused. |
| `flat_channels` | list of channel ids | `[]` | Channels that keep the reply-in-place surface when threads are on. |
| `catch_up_minutes` | integer, at least 0 | `60` | After downtime, how many minutes back the bot reads missed messages. `0` never. |
| `board_channel` | channel id | unset | A channel the bot keeps one message in: every open thread it started, by who moves next. Done threads close after 48 hours quiet, any other after 7 days. See [rooms.md](rooms.md#the-thread-board). |
| `room_tokens` | integer, at least 0 | `40000` | The most history a room keeps for its asks, in estimated tokens (four characters each; providers count about a quarter more). Past it, the older half is dropped from what asks see. `0` keeps everything. |
| `soul_file` | file path | `SOUL.md` in your Nodecode home | The persona file every room's asks read. See [rooms.md](rooms.md#persona). |
| `agents` | object | `{}` | Named agents a room's asks may run as: `folder`, `soul_file`, `prompt`, `model`, `provider`, `skills`. See below. |
| `routes` | list | `[]` | Which asks run as which agent, by `guild`, `channel`, `thread`, `user` or `role`. See below. |

### agents and routes

```jsonc
"agents": {
  "release": {
    "prompt": "This is the release channel: keep answers to the release plan.",
    "provider": "deepseek",
    "model": "deepseek-flash",
    "skills": ["release-checklist"]
  },
  "coder": {"folder": "~/src/webshop"}
},
"routes": [
  {"agent": "release", "channel": "123456789012345678"},
  {"agent": "coder", "user": "222222222222222222"}
]
```

An agent's members:

| Member | Type | Effect |
| --- | --- | --- |
| `folder` | folder path | Where its asks work: their tools run there, and they read its `AGENTS.md` and, in a git repository, its project memory. Must exist. |
| `soul_file` | file path | Its persona, in place of the section's `soul_file`. |
| `prompt` | string | Standing instructions for every ask it answers. |
| `model` | string | The model its asks run on, unless a room's `/models` pick says otherwise. |
| `provider` | string | The provider of `model`. Not allowed without `model`. |
| `skills` | list of skill names | Skills the model reads before it answers the first ask. |

A route's members:

| Member | Type | Matches |
| --- | --- | --- |
| `agent` | agent name, or `"default"` | Required: the agent its asks run as; `"default"` is none. |
| `user` | user id | Asks this person sends. |
| `thread` | thread id | Asks in this thread. |
| `channel` | channel id | Asks in this channel and its threads. |
| `role` | role id | Asks from a member holding this role. |
| `guild` | server id | Asks in this server. |

Every member a route names must match. The most specific route wins, in the
table's order from `user` down to `guild`, and of two equally specific routes
the first written. `/agent` in a room overrides the routes there. See
[rooms.md](rooms.md#agents).

## Turns and what the room shows

See [turns.md](turns.md).

| Key | Type | Default | Effect |
| --- | --- | --- | --- |
| `reactions` | boolean | `false` | Mark each ask with a reaction: eyes while it runs, a check mark when answered, a cross mark when it fails. Needs Add Reactions. |
| `stream` | boolean | `true` | Show the model's words on the turn's card as it writes them. The answer is still posted fresh at the end. See [turns.md](turns.md#streaming). |
| `voice_replies` | `"off"`, `"on"` or `"tts"` | `"off"` | Post an answer as a voice message too, below its words: `on` for an ask that was a voice message, `tts` for every ask. `/voice on`, `tts` or `off` sets it for one room. See [voice.md](voice.md#voice-messages). |
| `turn_budget_minutes` | integer, at least 0 | `0` | Minutes an ask's turn may run. Past them its tool calls are refused and it answers with what it has. `0` caps nothing. |
| `max_concurrent_turns` | integer, at least 1 | `4` | Turns running at once across the whole bot. Others queue. |
| `status_update_ms` | integer, at least 0 | `2000` | The shortest gap between edits of a running turn's card. See [turns.md](turns.md#the-card). |
| `text_chunk_limit` | integer, at least 1 | `2000` | Characters per message when an answer is split. Discord refuses more than 2000. |
| `delivery_workers` | integer, at least 1 | `4` | Threads that make the bot's calls to Discord. |

## Voice

See [voice.md](voice.md#configuration).

| Key | Type | Default | Effect |
| --- | --- | --- | --- |
| `voice_channel_id` | string | (unset) | The bot's own voice channel: where `voice_autojoin` sits, and where `/voice join` sits when its asker is in no voice channel. `/voice join` needs no voice key. |
| `voice_text_channel_id` | string | the voice channel's own chat | The channel voice talks through. |
| `voice_speakers` | list of user ids | `allowed_users`, then `owner`, then whoever typed `/voice join` | Whose speech opens a turn. `"*"` admits everyone. |
| `voice_autojoin` | boolean | `false` | Sit in `voice_channel_id` whenever somebody is in it: join when the first person comes in, leave when the last one goes. |
| `voice_follow` | list of user ids | (none) | Follow these people into voice: join the channel the first of them is in, move when they move, leave when they leave. |
| `voice_idle_minutes` | integer, at least 0 | `5` | Leave a channel joined with `/voice join` after this many minutes with nobody speaking to the bot. `0` stays. |

## Related top-level sections

These are outside `channels.discord` and apply to every channel:

- `transcription`: how voice messages and recordings are turned into text.
  See [files.md](files.md#transcription).
- `speech`: how answers are said, in a voice channel or as a voice
  message. See [voice.md](voice.md#speech).

## A fuller example

```jsonc
{
  "channels": {
    "discord": {
      "bot_token_file": "/home/you/.nodecode/secrets/discord.token",
      "owner": ["111111111111111111"],
      "allowed_users": ["111111111111111111", "222222222222222222"],
      "allowed_roles": ["333333333333333333"],
      "require_mention": true,
      "free_response_channels": ["444444444444444444"],
      "mention_patterns": ["hey bot"],
      "thread_behavior": "create_per_message",
      "flat_channels": ["555555555555555555"],
      "reactions": true,
      "turn_budget_minutes": 20,
      "agents": {
        "ops": {
          "prompt": "This is the ops channel. Prefer short answers.",
          "skills": ["deploy-checklist"]
        }
      },
      "routes": [{"agent": "ops", "channel": "444444444444444444"}]
    }
  }
}
```

This bot answers the two listed people and anyone holding the role, in any
channel of any server it is in. In DMs it answers the two listed people only,
since a DM carries no roles. In channel
`444...` it answers every message, as the `ops` agent; elsewhere it needs a
mention, a reply or "hey bot". Channel `555...` keeps answers inline.
