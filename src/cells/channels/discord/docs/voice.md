# Voice

Voice is two things:

- **Voice messages.** In any room, the bot can post an answer as a Discord
  voice message too, below its words. See [Voice messages](#voice-messages).
- **A voice channel.** The bot can sit in one voice channel, listen to the
  people you allow, say its answers out loud, follow people between channels,
  and take meeting notes. A voice channel is a second way into a text
  channel, never a separate conversation: every spoken ask is posted as text
  in that channel, and the answer is there in text too.

The bot sits in a voice channel only when asked: sit in one yourself and
type `/voice join`, and it sits beside you. Nothing needs configuring for
that. Otherwise it never joins or listens.

## Voice messages

`/voice on`, `/voice tts` and `/voice off`, typed in a room, decide what that
room's answers carry besides their words:

| Form | Answers |
| --- | --- |
| `/voice on` | An ask sent as a voice message is answered with one too. |
| `/voice tts` | Every ask is answered with a voice message too. |
| `/voice off` | Words alone. The default. |

A thread follows its channel's setting until `/voice` is typed in the
thread itself. `voice_replies` sets the default for every room (see
[config.md](config.md)).

The voice message is posted after the answer, as a reply to the ask. It is a
real Discord voice message: it plays in place and shows its waveform. It says
the answer without its markdown, and a long answer only up to
`speech.max_characters` (default 1200), cut at a sentence. The words above
are always the whole answer.

It is spoken by the [speech](#speech) section's voice and encoded with
`ffmpeg`. If that fails, the bot posts one line, for example "voice: no voice
message (ffmpeg is not installed ...). The answer above is the answer."

An ask spoken in the voice channel is answered out loud there, not with a
voice message.

## What you need

- **Nothing in the config.** `/voice join` sits beside whoever typed it, in
  the voice channel they are in, and talks through that channel's own chat,
  where the people in the call already read and type. The keys
  [below](#configuration) only change where it talks,
  whom it hears, and where it sits on its own.
- **Permissions**: Connect and Speak in the voice channel, on top of the
  text set (the invite integer with voice is `309240908864`). Without them
  Discord never seats the bot, and `/voice join` says so after ten seconds,
  with an invite link that grants them.
- **DAVE**: Discord requires its end-to-end encryption for voice. The first
  join downloads Discord's `libdave` library (about 4 MB, checked against a
  pinned digest) into `nodecode/libdave/` in your cache folder. Builds exist
  for Linux x64 and arm64, macOS arm64 and x64, and Windows x64.
- **A transcriber** to hear speech: local by default (`(nck:install-transcriber)`,
  about 510 MB, needs `ffmpeg`), or a hosted one. See
  [files.md](files.md#transcription).
- **A voice** to speak answers: local by default (`(nck:install-speaker)`,
  about 95 MB), or a hosted one ([below](#speech)). Spoken answers are
  encoded with `ffmpeg`, which must be on the PATH.

## Joining and leaving

Sit in a voice channel and type `/voice join` in a text channel of the same
bot. The bot joins your voice channel, in whichever server it is. If you are
in no voice channel, it joins `voice_channel_id` when one is set, and
otherwise answers "sit in a voice channel first, then /voice join".

It talks through `voice_text_channel_id` when set, else the voice channel's
own chat (`/voice room` picks another for this call; a call you are moved to
brings its own chat, a channel you picked comes along). It hears
`voice_speakers`, else `allowed_users`, else
`owner`, else you. It answers once Discord has seated it, and says the same
in the text channel when that is not where you typed:

```
voice: joining #voice-room — I will listen to @kim, @ana and answer here.
```

The bot can also take a seat on its own:

- **`voice_autojoin: true`**: it sits in `voice_channel_id` whenever somebody
  is in it. It joins when the first person comes in (or at start, if somebody
  is already there) and leaves when the last one goes.
- **`voice_follow`**: a list of user ids. The bot joins the voice channel the
  first of them is in, moves when they move, and leaves when they leave. If
  another person on the list is still in voice, it goes to them instead.
  Following someone gives them no rights: whose speech is answered is still
  `voice_speakers`.

It leaves on its own too:

- When the last person leaves its channel (bots do not count).
- After `voice_idle_minutes` (default 5) with nobody speaking to it, in a
  channel it joined with `/voice join`. A seat `voice_autojoin` or
  `voice_follow` holds is not given up this way, nor one where it is taking
  notes. `0` turns this off.

Each join, move and leave is said in the text channel, with the reason. If an
automatic join fails, `/voice status` and `/voice join` say why.

`/voice leave` leaves, stops listening and stops speaking. The bot also
leaves when someone disconnects it in Discord, and when the cell stops. If
someone drags it into another channel, it starts over there.

The bot sits in one voice channel at a time. `/voice join` while it is
already in one answers "already sitting in" that channel.

## Listening

The bot listens to the speakers you allowed and to no one else, and never to
itself.

- An utterance ends after 0.9 seconds of silence, or at 60 seconds.
- An utterance shorter than 0.4 seconds is ignored.
- The utterance is transcribed, and the transcript is posted in the text
  channel with a microphone mark and the speaker's mention. That message is
  the ask: it is admitted and answered like a typed one, with the same rules
  for threads, the turn's card and the answer.
- The speaker must also pass the text channel's checks as a person typing
  there would. They are matched by user id; a spoken line carries no roles,
  so `allowed_roles` does not admit a speaker.
- If the text channel refuses the transcript post, no turn starts.

With `thread_behavior: "create_per_message"`, each spoken ask opens a thread
off its transcript, like a typed ask. Put the text channel in
`flat_channels` to keep the voice conversation in the channel itself.

`voice_speakers: ["*"]` listens to everyone in the voice channel; the text
channel's checks still decide whose words get an answer.

## Speaking

Every answer that lands in the voice text channel, or in a thread under it, is
said out loud while the bot is in voice and the encryption group has formed
(see [below](#encryption-dave)), whether the ask was spoken or typed. Each
turn's answer is spoken once. Commentary posted while the turn works is not
spoken.

- Markdown is stripped before speaking.
- An answer longer than `speech.max_characters` (default 1200) is cut at a
  sentence, and the bot posts "voice: said the first part out loud; the rest
  is above."
- When an allowed speaker starts talking, the bot stops speaking at once.
  Silence does not interrupt it.
- `/voice say WORDS` speaks the words now, with no turn behind them. It is a
  quick test of the speaking path.

If speaking fails, the text answer stands and the bot posts one line, for
example "voice: could not say that out loud (ffmpeg is not installed, and a
spoken answer is encoded with it). The answer above is the answer."

## Meeting notes

`/voice notes` turns the bot into a note taker: it listens to everyone in
the voice channel, not only `voice_speakers`, writes down what each person
says, and answers nothing.

`/voice notes stop` ends the notes. The bot then:

1. posts the transcript in the text channel as a file, one line per thing
   said, with who said it and when (minutes and seconds from the start);
2. asks the model, in the name of the person who started the notes, to write
   them up: what the meeting was about, what was decided, who is doing what,
   and what is still open. The notes arrive as the answer to the transcript's
   message, in text only.

Leaving the channel, for any reason, stops the notes the same way. A move
(following someone, or being dragged) carries them along.

Start the notes from Discord: they are written up as an ask, and an ask needs
someone who asked. `/voice status` shows how many lines are taken so far.

## Encryption (DAVE)

Discord encrypts voice end to end with DAVE, and refuses a client that does
not speak it. The bot joins the channel's encryption group using `libdave`.

Discord forms no group while the bot is alone in the channel. Until someone
else joins, the bot can neither hear nor be heard, and `/voice status` reads
"waiting for the group — nobody else is in the channel yet". This is normal.
Once the group forms it reads "end-to-end, DAVE v1".

## /voice

| Form | Effect |
| --- | --- |
| `/voice` | In a room, the voice card (below). From a terminal, as `/voice status`. |
| `/voice status` | Where voice stands, in words. |
| `/voice join` | Join your voice channel, or the configured one. |
| `/voice leave` | Leave it. |
| `/voice on`, `tts`, `off` | Voice messages in this room. See [above](#voice-messages). |
| `/voice notes` | Take meeting notes. `/voice notes stop` writes them up. |
| `/voice room [CHANNEL]` | The channel it talks through: the call's own chat or a text channel. |
| `/voice hear [everyone \| default]` | Whose speech opens a turn. |
| `/voice hears [MODEL]` | The model speech is heard with: those the transcription server lists. Saved to `transcription.model`. |
| `/voice speaks [VOICE]` | The voice answers are said in: those the speech server names. Saved to `speech.voice`. |
| `/voice say WORDS` | Say WORDS now. |

With operators declared, `/voice` is theirs, like every command but `/help`.

### The voice card

`/voice` with nothing after it, in a room, answers with a card everyone in
the room can see: an embed, green while the bot sits in a voice channel and
grey while it does not.

- **What it says:** how to start, or where the bot sits, the text channel
  it talks through and whom it hears, each a field. Then the room's voice
  messages.
- **Buttons:** **Join** while the bot sits nowhere. **Leave** and **Take
  notes** (or **Stop notes**) while it sits in a voice channel. **Refresh**
  always.
- **Menu:** the room's voice messages: Words only (`off`), Voice for voice
  notes (`on`), Voice for every answer (`tts`). The room's current pick is
  shown.
- **Models:** three menus, each showing the one in use: **Hears with** (the
  transcription server's models), **Speaks with** (the speech server's
  voices, 25 at most), **Thinks with** (the room's model, the default and
  the failover chain; a pick is the room's `/models`). The built-in engines
  have one pick each.

Each press runs the `/voice` command it names, as if you typed it, and the
card updates in place: a change answers the card again, so its buttons stay. Where no embed is drawn
(a terminal, Telegram), `/voice` answers the same in words. A press from someone who may
not run `/voice` is refused privately, and the card stays as it was.

`/voice status` when joined:

```
voice: in #voice-room, talking through #general
· encryption: end-to-end, DAVE v1
· speakers: 123456789012345678, 234567890123456789
· heard 4 utterances, sent 1830 frames
· last trouble: ...
```

When not joined, it says how to start, and where `voice_autojoin` and
`voice_follow` would seat it.

## Configuration

These keys go in `channels.discord`:

| Key | Type | Default | Effect |
| --- | --- | --- | --- |
| `voice_channel_id` | string | (unset) | The bot's own voice channel: where `voice_autojoin` sits, and where `/voice join` sits when you are in no voice channel. |
| `voice_text_channel_id` | string | the voice channel's own chat | The channel voice talks through. |
| `voice_speakers` | list of user ids | `allowed_users`, then `owner`, then whoever typed `/voice join` | Whose speech opens a turn. `"*"` admits everyone. |
| `voice_autojoin` | boolean | `false` | Sit in the voice channel whenever somebody is in it. |
| `voice_follow` | list of user ids | (none) | Follow these people from channel to channel. |
| `voice_idle_minutes` | integer | `5` | Leave a `/voice join` seat after this long with nobody speaking to the bot. `0` stays. |
| `voice_replies` | `"off"`, `"on"`, `"tts"` | `"off"` | Every room's [voice messages](#voice-messages) until `/voice` sets its own. |

### Speech

A top-level `speech` section chooses how answers are spoken, in the voice
channel and in voice messages:

```jsonc
{
  "speech": {
    "base_url": "https://api.openai.com/v1",
    "model": "gpt-4o-mini-tts",
    "voice": "alloy",
    "api_key_env": "OPENAI_API_KEY"
  }
}
```

| Key | Default | Effect |
| --- | --- | --- |
| `enabled` | `true` | `false` keeps answers in text only. |
| `max_characters` | `1200` | The longest answer spoken; longer is cut at a sentence. |
| `speed_percent` | `100` | Pace of the local voice, at least 50. |
| `base_url` | (unset: local) | An OpenAI-compatible API root ending in `/v1`. |
| `model` | | The model `base_url` serves; required with it. |
| `voice` | | The voice `base_url` should use. |
| `api_key_env` / `api_key_file` | | Where the API key is. |

Restart the channels after changing these.
