# Buttons and pickers

Some of the bot's messages carry buttons or menus. Pressing one is the same
as typing a line: the bot treats the press as that person saying the line in
the room, as a reply to the message pressed, and admits and answers it the
way it would a typed message. Three features use this: clarifying questions,
the `/models` picker, and the card any command with a dialog answers with.

A button or menu option carries a short id, not the line itself; the bot
keeps what each id says, so a line of any length fits, and a card posted
before the bot restarted still works. A press on a card older than that
answers you alone "This button is out of date: run the command again."

The Stop and Details buttons on a turn's card are different: they act at
once and say nothing in the room. See [turns.md](turns.md#the-card).

## Clarifying questions

When the bot has to ask before it can answer, and the answer is one of a few
options, it can put the options on its question as buttons:

```
---

Which matters most for the first release?

[ Zero setup ] [ Fast startup ] [ Small binary ]
```

The model does this by calling, during its turn:

```lisp
(nck:answer-choices '("Zero setup" "Fast startup" "Small binary")
                    :platform "discord" :channel "<channel id>" :thread "<thread id, or omit>")
```

then ending the turn with the question as its answer. The model is told about
this in its Discord instructions; you do not need to configure anything.

- Two to five labels, each 1 to 80 characters, all different. Anything else
  is refused with an error the model sees.
- The labels ride the answer's last message as one row of blurple buttons.
- A second call in the same turn replaces the first set.
- It works only while a turn is running in that room.

### Pressing a button

A press is the presser's reply to the question, with the label as its words.

- The conversation that asked the question takes the label as its next
  turn, with everything it did before still in context. If that
  conversation is mid-turn, the press steers it.
- The question is edited in place: its words stay, its buttons go, and a
  small line under it says which answer it took:

  ```
  Which matters most for the first release?

  -# Answered: Zero setup
  ```

- Anyone the room admits may press, not only the person who asked. The press
  passes the same checks as a typed message from that person.
- Typing a reply instead still works: the buttons are a shortcut.

### A question whose conversation closed

The conversation that asked stays open until 30 minutes after its turn ended
with no further reply, and not across a restart of the bot. A press after
that still works, as a typed reply to the question would: the question is
marked answered and loses its buttons, and the label starts a new
conversation that sees everything the room has said, the question included.
Its answer replies to the question. Only the tool work the old conversation
did is out of reach.

## The /models picker

On Discord, `/models` with no arguments answers with a picker card instead of
text.

### The providers view

```
## Model Picker
Current model: deepseek/deepseek-flash
Select a provider (3 available).

[ Select provider          v ]
```

The menu lists every configured provider with its model count, the
provider of the current model shown as selected. "Current model" is what
this room's next ask runs on: the room's `/models` pick, its channel's (for a
thread), the model of the agent the room runs as, or the organism's default.

### The models view

Picking a provider edits the card in place:

```
## Model Picker
Current model: deepseek/deepseek-flash
Default: deepseek/deepseek-flash
Select a openrouter model.

[ Select openrouter model  v ]
[ Providers ] [ Reset to default ]
```

- The menu lists that provider's models. The room's current model is shown
  as selected.
- **Providers** goes back to the first view.
- **Reset to default** runs `/models default`, which clears the pick of the
  room you are in. It is greyed out unless that room picked its own: a
  thread running its channel's pick has nothing to reset.
- If the provider's model list could not be fetched, the card says so on a
  `listing:` line.

Picking a model picks it for this room. Where the model has a reasoning
effort to pick, the card is replaced by that ladder, over the words "this
channel runs PROVIDER/MODEL from its next ask; the organism's default is
unchanged" (or "this thread runs ...") -- the same effort card `/think`
opens, so a rung picked here is the same press either way; `/think` stays
the quick way to change it later without going back through `/models`. A
model the catalog says does not reason gets no such card; the words alone
answer.

### Long lists

A menu shows at most 25 entries, Discord's limit. A longer list gets a row of
**Prev**, **Page N/M** and **Next** buttons under the menu.

### What each step is

Every press is a `/models` line, run exactly as if you had typed it:

| Press | Line |
| --- | --- |
| a provider | `/models PROVIDER` |
| a model | `/models PROVIDER MODEL` |
| Providers | `/models` |
| Reset to default | `/models default` |
| Prev / Next | `/models page N` or `/models PROVIDER page N` |

So the same rules apply: with operators declared, only they can press. Anyone
else is refused privately ("/models is the operator's; /help lists what you
can run") and the card stays as it was.

### Who sees the card

Run from the slash menu, `/models` answers privately: only you see the card,
and your presses edit it. Typed as a message (`@YourBot /models`), the card is
a normal reply that the room can see, and any operator can work it.

See [commands.md](commands.md#models-in-a-room) for the text forms of
`/models`.

## A command's card

A command that opens a dialog in the terminal (`/cron`, `/link`, any cell's
command that lists things) answers with the same dialog as a card when you
run it bare in a room:

```
## Cron (28)
2 active · 4 paused · 22 done

[ Pick one of 28           v ]
[ Prev ] [ Page 1/2 ] [ Next ]
[ Refresh ]
```

- The menu lists the rows that do something. Picking one runs what Enter runs
  on that row in the terminal: `/cron show NAME` here.
- A row the terminal only puts in the message box for you to read first
  (`/link remove ID`) asks before it runs: **Run** or **Back**.
- Rows that do nothing are listed in the card's words.
- A dialog with tabs gets a row of tab buttons, the open one blue.
- Long lists page 25 at a time, as `/models` does.
- Whatever you pick answers in the card's place, with **Back** to the card
  you came from. An answer that is only words is shown above the card you
  pressed, read again, the way the terminal shows a notice over its dialog.
- **Refresh** reads the card again.

Telegram draws the same cards as inline buttons: a menu's options are one
button each, a disabled button is left out, and a press edits the card in
place.
