# nodecode

Nodecode is one SBCL process that runs its own gateway, sessions, turn loop
and storage. The model has one tool, `eval`, which runs Lisp inside that
process, and whatever the model defines is kept. This file is for working on
the source. For using Nodecode, read the [docs](https://nodecode.ai/docs).

## Build and test

From the repository root:

```sh
just build          # pull the pinned cells, compile the system
just test           # the test suite
just check          # lint, line-count ceiling, build and test
just release        # dump ./nodecode
just <cell>-check   # one cell's lint, build and tests (just mcp-check, ...)
```

By hand, from `src/`:

```sh
sbcl --dynamic-space-size 8192 \
     --eval '(push (truename ".") asdf:*central-registry*)' \
     --eval '(asdf:load-system :nodecode)'     # or (asdf:test-system :nodecode)
```

Give any image that will serve a big heap. SBCL's default 1 GB ran out in
production under a 418 MB core. `just release` bakes the heap size into the
binary.

For a proof that touches nothing real, `HOME=$(mktemp -d) ./nodecode` isolates
the whole organism. `--ephemeral` swaps only the store.

## Layout

```
src/
  nodecode.asd     the system; its component list is the load order
  CELLS.md         the cell contract
  UPGRADE.md       how a new build reaches a running organism
  src/
    package.lisp   the two packages
    term/          terminal core under both shells (from cl-tuition, see NOTICE)
    waist/         the declaration macros everything reads through: errors,
                   records, json, contracts, config sections, paths
    wire/          invocation provenance and the gateway sync protocol
    store/         store lifecycle, event log, state KV
    session/       sessions, turns, workspace snapshots, checkpoints, recall
    organism/      self-modification: gc, hooks, the layer, cells, knowledge,
                   launch, profiles, upgrade
    engine/        provider lanes, context engine, turn loop, background exec
    import/        bringing in another agent's home and sessions
    tui/           the shell: theme, editor, slash, keys
    surface/       gateway server, TUI client, rpc, control routes, notify
  test/            mirrors src/; support.lisp at the root
  cells/           bundle.json pins the nodecode-ai/cells commit; just cells-pull
                   fetches each cell into its own git-ignored folder
  tools/           lint, bloat ledger, install and release scripts
```

Each concern has one owner and each owner one file. The `.asd` lists every
file in load order. A file's folder is its address, not its place in that
order, so a new file goes in by taking its slot in the list.

`src/` is the product and reads nothing outside itself except `resources/`.

## One surface

Every client attaches through the gateway's HTTP routes and WebSocket sync:
the TUI, the web page, and programs through `nodecode rpc`
(`src/surface/rpc.lisp`), which takes commands on stdin and writes frames to
stdout. `src/wire/protocol.schema.json` defines every command and frame, next
to its owner `src/wire/protocol.lisp`, and a test holds the two to one list of
kinds.

## Two packages

`nodecode.kernel` is the floor: what has to run before any layer and survive
any layer. That is transport, durable history, GC scheduling, the lock, the
hook primitive, the folder loader, the fresh-process seams and the scribe.
Boot locks it with `sb-ext:lock-package`.

`nodecode.evolved` is what the organism changes: the prompt, the tool table,
the turn loop, projections and UI methods. The turn loop lives here on
purpose, because it is the thing the organism rewrites most.

The organism can move that line itself: `eval` reaches every definition, and
`save-lisp-and-die` saves compiled code.

## The layer

Every definition the model evaluates is kept as one file under
`~/.nodecode/cells/<git-identity>/`, committed with the session id and loaded
over the shipped source at the next boot of every process. A toplevel
`fmakunbound`, `makunbound`, `unhook`, `remove-method` or `unintern` removes
the name and its file. `git revert` in that folder undoes a change, and `git
push` / `git clone` share one.

Only people edit `src/`. The binary carries the source, so the organism reads
itself with `(definition-text 'name)` and `(definition-location 'name)`. After
an upgrade, boot skips and names any layer file whose target moved or no
longer exists, and the shipped definition stands until the organism records
it again or forgets it.

Prefer advice to overrides: `(define-hook fn "key" (next &rest args) ...)`
wraps a shipped function and survives a rewrite of its body.

Memories and skills are definitions too (`define-memory`, `define-skill`, in
`src/src/organism/knowledge.lisp`). They live in a knowledge cell that the
loader records but never loads, and one index of everything kept opens each
conversation, ranked by `~/.nodecode/usage.jsonl`.

More: [Eval and the layer](https://nodecode.ai/docs/eval.md).

## Profiles

A profile is a separate home, `~/.nodecode/profiles/<name>/`, laid out like
`~/.nodecode/` with its own layer, store and gateway. Every path resolves
through `(nlk:home ...)` (`src/src/waist/paths.lisp`), and the verbs live in
`src/src/organism/profile.lisp`. A new profile links its parent's `secrets/`,
`auth.json` and knowledge cells, and leaves the channels behind, because a bot
token belongs to one home. More: [Profiles](https://nodecode.ai/docs/profiles.md).

## Cells

Cells are optional ASDF systems, and nothing in `src/` names one. They live in
[nodecode-ai/cells](https://github.com/nodecode-ai/cells), pinned by
`cells/bundle.json`. At boot every process loads the folders under
`~/.nodecode/cells/`, and the gateway calls each cell's `START-CELL`. The
channel kit runs one shared room per chat and one lane per ask
(`cells/channels/kit/host.lisp` and `room.lisp`). Each cell documents itself:
the Discord guide is `cells/channels/discord/README.md`, and chrome, websearch
and mcp each have a README in their folder.

More: [Cells](https://nodecode.ai/docs/cells.md),
[Cell API](https://nodecode.ai/docs/cell-api.md),
[Channels](https://nodecode.ai/docs/channels.md). Providers and model pins:
[Models and providers](https://nodecode.ai/docs/models.md),
[Config](https://nodecode.ai/docs/config.md).

## Design constraints

A child is a fresh process, never a fork. `fork()` copies only the calling
thread, and SBCL refuses it once a second thread runs. The organism's
definitions are on disk, so a fresh process is the whole organism, and every
child runs this same binary with a verb. The launch cache builds and checks
its image that way (`warm`, `warm-dump`, `trial` in `launch.lisp`).

GC pauses matter and throughput does not. Measured: a gen0 collection takes
10.0 ms and a full GC over a populated heap 86.0 ms, against a 16.6 ms frame
at 60 fps. A typed f64 sum runs in 53.0 ms on SBCL against 44.1 ms on Zig, and
a JSON frame decodes in 7.0 us. So history lives in SQLite, the image holds
the active turn's working set, and full GC runs while the provider's reply is
in flight.

No Zig FFI until `sb-sprof` names a hot spot. A measured FFI call costs 8 ns,
so the option stays cheap and unused.

## Containment

`eval` has full host authority, and so does any folder under
`~/.nodecode/cells/`: being there is consent. A fresh process isolates
crashes, not damage to files or the network. Run Nodecode in a container with
seccomp before you let it change itself unattended.
