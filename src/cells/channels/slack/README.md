# nodecode-channel-slack

Lets people talk to your Nodecode through a Slack app. It connects over
Socket Mode, a websocket the app opens itself, so your machine needs no
public address. It ships with Nodecode, beside Discord and Telegram, on the
same channel kit.

- **DMs:** every message is an ask, answered in the DM.
- **Channels:** mention the bot and it answers in a thread under your message.
  Inside that thread, keep talking without mentioning it; each reply
  continues the same conversation. Mention it inside someone else's thread
  and it reads the thread's first message as what you are answering.
- **Edits:** edit a message to mention the bot and it answers, as if you had
  sent it that way.
- **Reactions:** with `reactions` on, an ask shows 👀 while it runs, then ✅
  once answered or ❌ if it failed. A reaction you leave on the bot's message
  reaches the conversation too.
- **Answers:** written in Markdown, which Slack renders; a table comes as
  Slack's own table (one bigger than Slack draws comes as a picture). While a
  turn runs, a status line with Stop and Details buttons sits in the thread.
- **Buttons and menus:** offered answers come as buttons; `/nodecode models`,
  `/nodecode context`, `/nodecode think` and the other dialogs come as cards
  with menus and buttons. A pick runs its line and the card updates in
  place.
- **Commands:** `/nodecode help`, `/nodecode models`, `/nodecode stop`, and
  so on, run a Nodecode command. Slack doesn't allow slash commands inside
  threads; mention the bot with the command instead (`@nodecode /stop`).
- **Files:** Nodecode reads images and recordings people post, and posts
  pictures, files and voice replies back (needs `files:write`).
- **Missed messages:** after Nodecode was away (a restart, a machine that
  was off), it reads what was said in the last hour of it and answers the
  asks it missed, marked as late. `catch_up_minutes` sets the hour.

## Make the Slack app

1. Go to [api.slack.com/apps](https://api.slack.com/apps) and choose
   **Create New App** → **From a manifest**.
2. Pick your workspace and paste [`manifest.json`](manifest.json). It turns on
   Socket Mode, the bot user, the Messages tab (so people can DM the bot), and
   the scopes, events and `/nodecode` command the cell uses.
3. Choose **Install to Workspace**. Copy the **Bot User OAuth Token**
   (`xoxb-…`) from **OAuth & Permissions**.
4. Under **Basic Information** → **App-Level Tokens**, choose **Generate**,
   add the `connections:write` scope, and copy the token (`xapp-…`).
5. Keep both tokens in files or environment variables, never in the config.
6. `/invite @nodecode` into a channel, or open a DM with it.

An app made from an older manifest lacks `files:write`, `reactions:read` and
the reaction events: add them under **OAuth & Permissions** and **Event
Subscriptions** (or paste the manifest again), then reinstall the app.

Slack's free plan allows custom apps (up to ten per workspace).

## Configure

`nodecode gateway setup slack` walks it in the terminal. Or add a
`channels.slack` section to `~/.nodecode/config.jsonc`, or tell Nodecode
"set up slack" and it walks you through it. It checks both tokens
and lists your channels and people by name, so you pick ids from a list.

```jsonc
{
  "channels": {
    "slack": {
      "bot_token_env": "SLACK_BOT_TOKEN",   // or bot_token_file
      "app_token_env": "SLACK_APP_TOKEN",   // or app_token_file
      "allowed_users": ["U0123ABCD"],       // who may drive it
      "owner": ["U0123ABCD"],               // whose word is standing policy
      "allowed_workspaces": ["T0123ABCD"],  // your own workspace only
      "require_mention": true
    }
  }
}
```

The bot refuses to start unless `allowed_users` or `allowed_channels` names
someone or somewhere, because a message it answers runs with your machine's
full authority. In a channel shared with another organization (Slack
Connect), `allowed_channels` alone lets its people drive the bot too; set
`allowed_workspaces` to your own workspace id to keep them out.
`mention_patterns` (wake words) and `mention_groups` (user group ids) count
as mentioning the bot. See [`config.example.jsonc`](config.example.jsonc)
for every key.

### The thread board

Set `board_channel` to a channel id and the bot keeps one message there,
edited in place: every thread it answers in, sorted by who moves next
(Needs you, No answer, Running, Done). Each thread is named by the first
line of the message it hangs off and links to it. Slack can't close a
thread, so a finished one leaves the board instead: a Done thread after 48
hours quiet, any other after 7 days. **Show all** lists every open thread
in a message only the presser sees, **Clear done** (owners only) takes the
Done threads off, and `/threads` answers the same list. DM threads never go
on the board. The rest do, with their first lines, so pick a channel whose
members may read every channel the bot answers in.

## Test

From a Nodecode checkout: `just channels-test` runs this cell's tests with
the kit's, Discord's and Telegram's. From a checkout of the cells
repository: `.github/test-cell.sh rooms/nodecode-channel-slack`.

The tests run the cell against a fake Slack on 127.0.0.1 that speaks the
Web API and Socket Mode, so they need no Slack account.

MIT licensed.
