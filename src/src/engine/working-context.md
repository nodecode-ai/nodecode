# Editable working context

`(context)` writes the current session's `context.txt` from the context as it
stands and returns its path. The first line names the export (`[[context
NONCE]]`) and says how full the window is when a round has reported it
(`context 87k of 410k`). Below it, each message of the retained history is a
block under a `[[m:NONCE ID KIND]]` line (a tool block adds its tool's
name), `ID` the log position the message
stands for: a user ask, an assistant's text, a tool block (its call on a
`→ ARGS` first line, then its result) or a note. Ordinary `edit`, `write-file`,
shell commands or Lisp edit it. There is no summarization API and no
size-reduction requirement: growth, deletion, notes and restoration are all
valid.

## What an edit means

The next model-request boundary compares the file with the export record
beside it (`context.export.json`), block by block:

- a block left as it is keeps its message native: the very object the fold
  built, byte-identical on the wire, so the provider's cached prefix holds up
  to the first change;
- a block deleted drops its message; a tool block takes its call with it;
- an assistant's text or a tool result edited stays in its slot (an edited
  result ends with the recall line that reads the original); an edited call
  line replaces the call's arguments with a stand-in naming the original;
- an edited user ask, text under a `[[note]]` line, text above the first block,
  or text added after a block's own text becomes a note: an assistant message
  labelled `[context note]`, the model's own words, never the operator's and
  never an executed call;
- a removed block's header written back, anywhere, returns the original message
  native, where it stood;
- harness lines and the active request are never in the file and stay where
  they stand.

Kept messages stay in log order; one moved behind a later one is read as a note
where it was moved to. Every call stays answered: a call whose result left
answers that it did, and a note or harness line never splits a call from its
answers.

## Edit by seq

Every tool result the model reads ends with a line naming the log position of
its fact, `[seq 57]` (under a turn budget the spend facts follow it in the same
bracket, meter.lisp `tool-result-text`); the file shows the result without the
line, since its header names the block. `(context :drop SPANS :keep IDS :note
TEXT)` is the same edit without the file: `:drop` names the blocks that go — `t`
for every block, a `(from . to)` span of seqs, or a list of seqs, ids (`"n1"`,
`"57c"`) and spans — `:keep` the seqs held back from the drop, and `:note` the
model's note at the end, alone when nothing else changes. The verb writes the
edit into `context.txt` over the export (or over an edit still waiting there, so
edits in one round compose) and returns what the next request will take; that
request takes it through the same acceptance as a file edit, with the same
receipt and the same `context.edited` fact. The file stays the general path.

## Authority and publication

The event log is the only authority. One `context.edited` fact (contract in
`*working-context-contract*`) records the covered span (`start`..`source`),
the items it now reads (`{id}`, `{id text calls args}`, `{note}`), the first
item that differs from the export and the counts the operator's row reads.
Acceptance runs under the store lock against the newest visible edit: the
export record names the edit it was written over, and another edit taken since,
or a branch move, refuses it. Newer turns appended after the export do not: they
follow the edited history, so a file read rounds ago still edits. The file and
its record are disposable working state, written by `(context)` and after every
boundary that took or refused an edit; a file holding an edit not yet taken is
never overwritten. Acceptance or refusal is both a durable prompt receipt and
`context.receipt.txt`; refused bytes are saved in `context.rejected.txt`.

The fold names every message by the fact it stands for (`MESSAGE-SOURCE` in
`context.lisp`): the log position, or for a message the fold makes itself, the
position it answers for and a suffix (`c` a call flush, `o` an unrecorded
answer, `s` an eviction stub).

## One projection

`history-turns-for-session` applies the newest visible edit after the raw
history cache (`:raw t` reads the fold as made). Each item lands in the unit of
the message it names, a note in the unit of what stands before it, so turns
keep their units: floor eviction drops a note with the turn it rides, and a note
never opens a round, so within-turn relief still aligns with its facts.
Reasoning stays on messages before the first change; from it on, and on rounds
generated between the export and the edit, signed reasoning is not replayed.
Hydration, compile, estimates, image withholding, operator eviction and
overflow recovery all consume the same projected units. Resume rebuilds the
view from SQLite; forks inherit the edit visible at their anchor and edit their
own files; rewinding restores the older branch's view. Original conversation
facts and lossless recall are never rewritten.

`context.edited` is persisted history: the stream carries it and the shell
folds each one into one transcript row (`context-edit-line`).

## Built-in editing skill

The `nodecode-context` cell is compiled into `nodecode/bin`, including pristine
boots and homes with no installed cells. Its short rule always rides in the
protected `context` harness block, not the ranked knowledge index or editable
history. The wording lives only in the cell's `cell.lisp`.

## Proof and limits

`working-context-request-replay-and-forks` runs the synchronous private-store
proof in `working-context-proof.lisp`: actual provider-attempt inputs, an
appended note that leaves every earlier message byte-identical, a surgical
deletion, restore by header, an edit written after newer rounds, protected
input and harness, images, reasoning, refusal, forks, rewind, relief alignment,
overflow, the size line, the operator row and database close/reopen.

After `just release`, the process/HTTP proof is:

```sh
python3 src/test/engine/working-context-live-proof.py \
  --binary ./nodecode --root /tmp/context-proof-new-directory
```

It creates isolated HOME, NODECODE_HOME and XDG_CACHE_HOME directories, uses only
a loopback fixture, drives `./nodecode --ephemeral rpc`, and captures
`requests.json`, `frames.jsonl`, and `result.json`. It checks both the shell and
gateway executables, an appended note's byte-identical prefix on the serialized
HTTP request, an edit written a round after its read, and the `context.edited`
facts on the stream. The root must not already exist.

Middle edits still re-send everything after the first change: no provider here
reuses a cached suffix. No learned editing policy or cost improvement is
claimed; benchmark matched tasks before claiming a win.
