# Files

This page covers what the bot does with files people send, and how it sends
files back.

## Files people send

The bot reads the attachments on:

- the message itself;
- a forwarded message, which it reads whole: the forward's text and files
  are what the person said;
- the message being replied to, so "what does this say?" in reply to a voice
  note or a picture works.

It reads up to 4 files per message. Past that, the model is told how many
more arrived and were not read.

Files are fetched only from Discord's own file hosts (`cdn.discordapp.com`
and `media.discordapp.net`, plus the host of `api_base` when you point it at
a stand-in). An attachment URL anywhere else is dropped, with a warning in the
log.

Each file is fetched (20 seconds at most) and recognized by its bytes, not by
its name or the type Discord reports:

| What it is | Limit | What the model gets |
| --- | --- | --- |
| An image: PNG, JPEG, GIF or WebP | 5 MB | The image itself, marked `[Image #1]` in the prompt where the message sits. |
| A recording: a voice message or an audio file | 25 MB, and `transcription.max_seconds` (default 300 s) | Its transcript: `[Audio #1, 5.5s, transcribed] "..."`. |
| A text file | 100 KB | Its whole text, in a code block under `[File "name"]`. |
| Any other file, or one over the limits above | 32 MB | The file saved on the machine, and its path: `[File "name" (type, N bytes) saved at PATH: not read in; open it with your tools]`. |
| A file over 32 MB | | A note saying it was not fetched. |

Details:

- A text file is one Discord types as text or a structured text format (JSON,
  YAML, XML, CSV and the like), or whose extension is a common text or source
  format (`.md`, `.log`, `.py`, `.lisp`, `.sh` and many more). It must decode
  as UTF-8.
- A video is a file to open, not a recording: it is saved, not transcribed.
- At most 4 recordings are transcribed for one ask. Each takes seconds of
  CPU with the local transcriber.
- Saved files go to `nodecode/channel-files/` in your cache folder
  (`$XDG_CACHE_HOME`, else `~/.cache` on Linux and macOS, `%LOCALAPPDATA%`
  on Windows). The model opens them with its own tools.
- A message with an image and no words still counts as something said.
- Voice messages sent as chatter (not addressed to the bot) wait with the
  rest of the room's context and are transcribed when the next ask carries
  them.
- Any failure leaves one bracketed note in the prompt in place of the file,
  saying why, so the model never pretends it saw the file.

Without the Message Content intent, Discord sends no attachments for
messages that do not mention the bot.

### Transcription

Recordings are transcribed by the local transcriber by default: the audio
never leaves the machine. It is not installed with Nodecode. Ask Nodecode to
run `(nck:install-transcriber)`, which fetches the engine and its model once
(about 510 MB) into the cache. It needs `ffmpeg` on the PATH.

Or use a hosted, OpenAI-compatible service with a top-level `transcription`
section:

```jsonc
{
  "transcription": {
    "base_url": "https://api.groq.com/openai/v1",
    "model": "whisper-large-v3-turbo",
    "api_key_env": "GROQ_API_KEY"
  }
}
```

| Key | Default | Effect |
| --- | --- | --- |
| `enabled` | `true` | `false` leaves a note in place of each recording. |
| `max_seconds` | `300` | The longest recording transcribed. |
| `base_url` | (unset: local) | An OpenAI-compatible API root ending in `/v1`. |
| `model` | | The model `base_url` serves; required with it. |
| `api_key_env` / `api_key_file` | | Where the API key is. |

These settings are read when the channel cells start.

## Files the bot sends

The model has three calls for files. They are part of its instructions in a
room; you do not call them yourself.

- `nck:answer-file` hands a file to the answer the running turn is about to
  post. The file rides the answer's own message (its first part), so a
  picture shows with the answer and not beside it.
- `nck:post-file` posts a file as a message of its own, with an optional
  caption and reply.
- `nck:fetch-image` downloads an image from the web (5 MB at most, checked
  by its bytes, a page or video refused) so the model can attach it.

The model can also make any other upload through Discord's API with
`ncd:request`.

The bot needs the Attach Files permission. Nodecode does not check sizes
before an upload; Discord's own upload limit for the server applies, and a
refused upload is reported back to the model. An upload may take up to a
minute.

### Tables

Discord does not render Markdown tables. When an answer contains one, the bot
draws it as a PNG, takes the table's rows out of the text, and attaches the
picture to the answer's message. Drawing
needs `python3` with Pillow and a monospace font on the machine. A table
over 60 rows, 14 columns or 200 characters in a cell, or a machine that
cannot draw, gets the table as a code block instead. The room's record keeps
the table as the model wrote it.
