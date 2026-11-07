# nodecode-team

team@N ("Scaling Discovery through Test-Time Communication", arXiv
2609.21032): N identical full-tool sessions work one task in parallel from
one directory, and the directory is the whole protocol. No messages between
nodes, no roles, no controller process.

## The standing team

`/team TEXT` in a session's shell seats that session's standing team: the
first one mints `team.nodes` nodes under it, working from
`~/.nodecode/team/<session>/standing/`, and gives each node TEXT behind its
own marker. Every later `/team TEXT` goes to the same nodes -- a steer of the
turn a node is running, a follow-up to a node running none -- and between
inputs they sit idle. `/team` alone reads where the team stands; `/team stop`
stops every node. Nothing else the operator types reaches the team: the first
real runs seated every input, a status question included, and the nodes
billed three and a half times what their session did.

A node whose tokens are spent (a twentieth or less left) is never prompted
again: at the next `/team` a fresh node, the next free K, takes its seat with
that input and the paragraph a first input carries, so the team is back at
`team.nodes`; the spent node stays listed in `team.sexp`, still capped. A
node cannot seat a team, and `/team` in a node's own shell is refused.

A standing team has no scorer. Its product is `LOG`, `best/` and `work-K/`
in that directory; nothing of it is relayed back into the session, and
`(team:best DIR)` reads it.

## What a node keeps

Nothing past the image. A node's definitions are live for its task but are
never filed into the operator's layer (advice on `nlk:record-definitions`):
the first real runs filed four there, one of them appending to a dead
team's LOG from every boot.

## The verbs

Called through `eval`; every one answers a string, every refusal is
`ERROR: TEAM-ERROR` with the reason. While the cell runs, a node is a
session named `team-<team>-node-<k>`: TEAM is the identifier of the session
it hangs under — its standing team and every team it opens count K up
together — and K is its own number inside that team, minted as it is needed.

- `(team:open "task" &key score dir)` makes the directory (under
  `~/.nodecode/team/<session>/<tag>/` unless `:dir` says otherwise), starts
  the configured number of nodes (`team.nodes`, 1 to 5) on one shared
  prompt — each node's own `[team-<team>-node-<k>]` marker leading it — and
  answers the directory. A relative `:dir`, like every verb's DIR, is
  under the calling session's own directory. Call it again for more nodes. `:score` names the
  scorer; without it the team is unscored, like the standing one: no
  `./score`, `./verify`, `SCORES` or `slots/`, `best` answers none, and
  `watch` never prompts an idle node again.
- `(team:watch "dir" &key (poll 15))` blocks until `SOLVED` exists, every
  node has settled, or the deadline passes, then answers the best. The
  eval substrate backgrounds it and its exit wakes the session.
- `(team:best "dir")` the best now: the `SCORES` maximum, confirmed by one
  more run of `./verify` (a claim that no longer verifies loses to the next).
- `(team:stop "dir")` stops every node; the directory stays.

## The directory

```
TASK.md        the task, identical for all; a standing team's first input
team.sexp      every node minted, the budget, when the team opened (a standing team:
               the latest /team), the session it hangs under
LOG            append-only broadcast: [slot K HH:MM:SSZ] kind: text ([node K ...] unscored)
DISCONFIRMED   negative results and the command that showed them
ADOPTED        who adopted what from whom, and why
work-K/        K's scratch (made by its node)
best/ best.lock   the team's one artifact, replaced only under flock
score          scored only: the scorer, ./score CANDIDATE prints one number, exits 0 iff met
verify         scored only: ./verify K CANDIDATE runs ./score, appends the line to SCORES under flock
SCORES         scored only: append-only, written by ./verify only
slots/slot-K/  scored only: claimed by an atomic mkdir; approach inside
SOLVED         scored only: written once, by the node whose ./verify exited 0
```

A scored team's K is the slot its node claimed; an unscored team's K is the
number that ends the node's marker.

## The budget

A node's turn opens under what its session has left: `budget_tokens` minus
its summed `turn.usage`, `budget_seconds` minus the time since the team
opened -- for a standing team, since the latest `/team`: its clock re-bases
at every input, so the seconds are the current task's window. The kernel enforces it (`nle:turn-budget`): past it tool calls are
refused and the turn answers with what it has. An idle node with more than
a twentieth of both left is prompted again in its own session, three times at
most, and only in a scored team. A standing node's tokens are a lifetime cap
over every input it takes.
