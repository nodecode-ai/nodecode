# What you see during a turn

A turn is the bot's work on one ask, from the moment it is admitted to the
answer. This page describes each thing the bot shows in Discord along the
way, in the order you meet them.

## Typing and reactions

The typing indicator starts as soon as the ask is admitted and is renewed
every 8 seconds while the turn runs. It stops when the turn ends, or when the
turn has shown nothing for 5 minutes. Several turns in one channel share one
indicator.

With `reactions: true` the ask itself carries a mark:

- an eyes reaction from admission until the turn ends, including while it
  waits in the queue;
- a check mark when the answer lands;
- a cross mark when the turn fails;
- no mark when the turn was stopped or answered with silence.

Reactions are off by default. They need Add Reactions and Read Message
History. If Discord refuses one, the bot stops reacting for that
conversation and logs why.

## The card

A quick turn shows only the typing indicator and then the answer. A turn that
takes longer earns a card: a message posted as a silent reply to the ask (it
pings no one) and edited in place. It appears when:

- the ask has to wait for a free slot (see [the queue](#the-queue));
- the turn calls its first tool;
- a message is waiting behind the turn;
- the model fell back to another model;
- the turn has run for 8 seconds while the model only thinks, or for 20
  seconds in any case.

The card is a box of Discord's message components (Components V2) whose
accent bar says the phase: blurple while the turn works, green once it is
done, grey when it was stopped or waits in the queue, red when it failed. A
running card reads like this:

```
working · 40s                                    (small type: state and time)
Fix the red lint on main                         (the heading: what the ask is)
◌ An extra paren in digest-status-text           (the spinner, then the thought in italics)
Found it: a stray paren in DIGEST-STATUS-TEXT.   (what the model said on its way)
+2 earlier
✓ Read kit/digest.lisp 860–880 · 1s     [Output] (a green check, and its button)
✓ Edited kit/digest.lisp · 1s           [Output]
◌ Running just lint · 3s                         (the spinner, turning)
[the pictures the turn looked at]
──────────
deepseek/deepseek-flash (high) · 4 steps         (small type: the numbers)
↑48k c93.9% ↓2.1k r310 · ctx 24% · $0.012
[Stop] [Details]
```

- The title is what the ask is: the title a model writes for it, three to
  seven words, the same one its thread is named (one call names both; a DM
  or a flat channel's ask is named when its card first posts). Until it is
  written the card shows the ask's first words; an ask of five words or fewer
  is its own title.
- What the turn is doing now is its running step, in the words the terminal
  uses, with the spinner. Between steps the description opens with it:
  `Thinking` while the model only thinks, `Writing` while it writes its words,
  `Waiting on the provider` after 20 seconds with no output, `Waiting on a
  background evaluation` while the turn waits on work it started.
- The line in italics is the model's newest thought: the bold title a
  reasoning summary opens each step with, or else the newest whole sentence
  of its thinking. A sentence still being written is never shown.
- The newest four tool calls are a row each, with how long it took; older
  ones are counted. A step done wears a green check, the one running a
  spinner that turns, and a step a stop or a failure cut short a grey cross.
  A step that answered something carries an **Output** button.
  The spinner also opens the thought while the turn works, so a working card
  always moves. These are the bot's own emojis: on connecting it uploads
  them to its application from the cell's `emoji/` folder (nothing more
  than the token is needed) and uses them in every server. If Discord refuses
  the upload, the marks are text (`✓`, `›`, `×`) and the bot logs why.
- `fell back to MODEL (reason)` appears under the thought when the provider
  failed over.
- What the model says on its way shows under the thought, in its own words:
  see [what the turn says](#what-the-turn-says-on-its-way).
- A picture the model looked at (`look`) joins the card's gallery, the
  newest four. Each is uploaded once, with the edit that first shows it, and
  kept on every edit after.
- The numbers are the terminal's run line in two. The first line names the
  provider, model and reasoning effort the newest round ran on (`→served`
  after the model when a relay served another one) and counts the tool
  calls. The second is the turn's tokens, summed over its rounds as the
  terminal's meter sums them: `↑` read (`c` the share served from cache,
  `w` cache writes), `↓` written (`r` of it reasoning), `~` in front when a
  count was estimated; `ctx` the share of the model's window the newest
  round filled, or its size when the window is unknown; the price when
  the provider reports one; and once it ended, `finish length` (or the
  provider's other reason) when the answer did not end cleanly.

The card is edited at most every 2 seconds (`status_update_ms`), and right
away when a button changes or a waiting message joins it.

A running card carries two buttons on one row at its foot:

- **Stop** (red) cancels the turn. Only the person who asked and the
  operators can stop it; the press takes effect at once, the model's current
  round is cut off, and the card says who stopped it.
- **Details** answers the person who pressed it, and no one else, with every
  step of the turn, the last lines each one answered, and the model's newest
  thought in whole sentences. The asker and the operators can press it.

A step's **Output** answers the person who pressed it, and no one else, with
the end of what that step answered: up to its last 1,500 characters, for the
newest eight steps (older ones keep the last lines Details shows). Who can
press it is Details' rule.

When the answer lands, the card stays above it as the turn's record: it turns
green, its small type reads `done · 1m02s` over its last steps, and it keeps
**Details** and each step's **Output**.
The bot keeps the details of its last 500 settled cards while it runs; after
a restart a press says the steps are no longer held.

### Messages waiting behind a turn

A message sent while a turn runs on the same conversation is shown on the
card, under Waiting:

```
Waiting
⌎ also check the tests — after this round
⌎ and then push
```

The first row says when it runs: "after this round" for a steer (see
[steering](#steering)), "after this turn" for a message that waits for the
turn to end. At most five rows show; the rest are counted.

## What the turn says on its way

When the model says something and keeps working (a round that speaks and
then calls a tool), its words stay on the card, under the thought: the
newest of them, up to their last 1,200 characters, in the model's own
Markdown. They are never posted as a message of their own, so the room
holds the card and the answer and nothing between them. **Details** shows
everything the turn said, under Said.

A settled card shows what the turn said only when the turn was stopped or
failed; when it answered, the answer below says more. A turn waiting on its
own background work shows the word it said before waiting.

## Streaming

With `stream: true` (the default), the card shows the words the model is
writing as it writes them, with `▉` at their end, edited at most every 2
seconds. A turn that only writes earns its card after 8 seconds, as one that
only thinks does; a quicker answer comes without one. The model's thinking
is not streamed; its newest thought shows on the card.

- A round that goes on to call a tool keeps its words on the card as what it
  said, the `▉` gone.
- The round that ends the turn wrote the answer. The answer is then posted as
  a new message, as described below, and its words leave the card. Only a
  new message pings the asker and marks the channel unread; an edit does
  neither.

With `stream: false` the card shows a round's words once the round ends.

## The answer

The answer is one message, or several when it is long, and opens with its
words.

- The work it took, and what the turn said on its way, are on the card above
  it, never under the answer.
- Each answer replies to the message it answers and pings that person once.
  In a thread the bot opened for the ask, the first answer cannot reply to
  the ask, which sits in the parent channel, so it starts with a mention of
  the asker instead.
- Link previews are suppressed on every message the bot sends.
- Nothing in the answer text can ping anyone: `@everyone`, `@here`, role and
  user mentions written by the model render without notifying.
- A Markdown table in the answer is posted as a picture attached to the
  answer, because Discord does not render tables. If the picture cannot be
  drawn, the table is posted as a code block. See
  [files.md](files.md#tables).

### Long answers

Discord allows 2,000 characters per message. A longer answer is split into
several messages of at most 2,000 characters each. A code block that a cut
lands in is closed at the end of one message and opened again, with the same
language, at the start of the next. Only the first message replies and pings;
files ride the first message, buttons the last.

### Silence

The model may decide a message needs no answer, for example a reply that
talks to someone else. It answers with `NO_REPLY` (or `SILENT`, `[SILENT]`,
`NO REPLY`, in any case), and the bot posts nothing: no answer, no ping. The
card is removed. If the bot opened a thread for the ask and nothing
else was said in it, the thread is removed too, when Discord allows it.

A turn whose model replied with no words at all posts "the turn ended
without an answer — nothing was posted." so the silence is not mistaken for
a choice.

### Failure and cancellation

A failed or stopped turn does not post a separate message. Its card becomes
the notice, red for a failure:

```
failed
Failed at 2m14s
> DETAIL
Steps
✓ Read kit/digest.lisp · 1s
× Running just lint · 40s
3 steps
```

`DETAIL` is the error, cut to 200 characters; `×` marks the call it cut
short. A stopped turn's card is grey and reads `Stopped at 15s` with who
stopped it. A turn that fails before it earned a card still gets one, so a
fast failure is never silent. Both keep **Details**.
The ask and the notice stay in the room's record, so a later ask can see
what happened.

When a turn fails after its conversation was closed, the bot posts
`turn failed: DETAIL` in the room it belongs to.

## Steering

A message that reaches a running turn steers it. That happens when you:

- reply (with Discord's Reply) to anything the turn posted;
- write in a thread or DM where the bot is mid-turn.

The running turn ends at its next round boundary and your message runs as
the next turn of the same conversation, with everything the first turn did
still in context. The card shows the steer as a waiting row until then. Marks on the first ask carry over to the new turn.

A reply to a message from a turn that has ended starts that conversation's
next turn. After 30 minutes without a reply, the conversation closes, and a
reply opens a new one on the room's record.

## Edits and reactions

Editing a message so that it mentions the bot, or contains one of its
`mention_patterns`, turns it into an ask. Other edits are ignored, as is an
edit to a message the bot already took.

A reaction someone leaves on one of the bot's messages, in a conversation that
is still open, is never an ask. It becomes a line of context ("reacted ..." or
"took back ...") that the next ask in the room carries. Reactions on other
people's messages are not read.

## The queue

At most `max_concurrent_turns` asks (default 4) run at once across the whole
bot. Others wait, each with a grey card reading `Queued · 2 ahead` and a
**Raise limit** button. An operator's press doubles the limit at once, writes
it to `max_concurrent_turns` so a restart keeps it, and lets the queue
through as far as the new limit reaches; anyone else's press is told,
privately, to ask an operator. When a slot frees, the next ask is picked
fairly by author: the person with the fewest running turns, then the one
served longest ago. The queue lives in
memory; a restart drops waiting asks (see below).

## Turn budget

`turn_budget_minutes` caps how long an ask's turn may run. Past it, the
turn's tool calls are refused and the model answers with what is done and
what is left. The default, `0`, caps nothing; the model sees the elapsed time
on every tool result instead.

## Stopping a turn

- Press **Stop** on the card (the asker or an operator).
- Run `/stop` in the room (an operator, when operators are declared). It
  stops the newest turn running in that room and answers privately:
  "stopping the running turn…", "no turn is running in this room", or "the
  turn already settled". In a channel where asks open threads, run `/stop`
  inside the thread: the channel's `/stop` only reaches turns that run in
  the channel itself.

## When the bot stops mid-turn

When the bot shuts down (a restart, a stop of the cells), every live card
is settled:

- a running turn's card reads `Paused at ...` with `the bot stopped; this
  picks up where it left off when it is back`. When the bot starts again,
  the turn resumes and answers under the same card.
- a queued ask's card reads `Stopped at ...` with `the bot stopped before
  this ran: ask again when it is back`.

If you set a home channel, the bot says it is back there when it starts,
in the model's words of a note on how long it was gone and the turns it
picks up. Each of those turns opens with the bot saying, in a sentence,
that it is back and what it was doing.

## Private replies

Some answers are shown only to the person who asked, as Discord ephemeral
messages:

- the answers to `/stop`, `/channels` and `/models` run from the slash
  menu;
- every refusal of a command run from the menu or a button ("/X is the
  operator's; /help lists what you can run", "not allowed here: REASON");
- a card's **Details**.

A private answer is one message: text past 2,000 characters is not sent.
The same commands typed as a message (for example `@YourBot /channels`)
answer in the room as a normal reply. See [commands.md](commands.md).

## Notes to the operators

Some background work (a reflection the model recorded and chose to announce)
posts one note headed "needs your attention" that mentions the operators.
It is the only bot message besides the answer that pings anyone.
