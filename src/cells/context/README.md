# Context skill

A built-in, prompt-only cell, compiled into `nodecode/bin` even with an empty
home or `--pristine`. Its one short rule asks the model to edit its
editable context at useful checkpoints during substantial tool work (after
settling a subproblem: a level cleared, a fix proved) and
before a final answer, and says how: one call,
`(context :drop SPANS :keep IDS :note TEXT)`, naming blocks by the `[seq N]` line
every tool result ends with, with no export, read or print round. Keep what
matters; window headroom is not the test: kept blocks ride every request, so
settled ones the model will not reread go, and a drop is lossless (`(recall-entry
seq)` reads one back). Under the older "none when unnecessary" wording, Claude on a
1M window edited in 8 of 33 long turns, its reasoning citing the
free window, "unnecessary" and fear of losing an answer. Still not after every tool
call, and not while a subproblem is still open (a wipe in
the middle of a level was followed by 15 calls and one game action).

The full rule is always in the protected `context` harness block. It is not a
ranked knowledge entry: index budgets, aging, context-file edits and history
eviction cannot prune it. The normal prompt pin keeps its bytes stable across
rounds; forks and resumed sessions receive it too. This is logical prompt
retention, not a promise that a provider never evicts its physical KV cache.

No configuration, tools, workers, automatic deletion or forced extra round.
The cell is a library: loading its folder installs one ordinary function hook.
The wording has one owner, `*skill*` in `cell.lisp`.

Run `.github/test-cell.sh tools/nodecode-context` from the cells repository,
or `just context-test` from Nodecode. Tests cover full-text inclusion, repeated
load, an empty index, deletion of the entire editable body, history eviction,
cache loss, forks and database reopen. The released RPC/HTTP proof is
Nodecode's `src/test/engine/working-context-live-proof.py`.
