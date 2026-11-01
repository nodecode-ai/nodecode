# Upgrading a running organism

How a published build reaches an organism that is already running, on a box
nobody is sitting at. The decision and the runbook; the
code is `src/src/organism/upgrade.lisp` (the mechanism over the image),
`src/src/surface/update.lisp` (the policy around it) and
`src/tools/install/install.sh` (the layout on disk).

## The decision

An upgrade classifies itself before it touches anything, then takes one of
two paths. Both end with `current` naming the new version.

- **Hot.** Every changed definition can be evaluated into the live image, so
  it is, in load order, all or nothing, with no restart. The organism keeps
  its peripherals, its MCP sessions, its adapters and timers, its shells.
- **Restart.** Something in the diff cannot go live (a struct, a package
  past its exports, a constant, a store kind, a form outside any
  definition, a system the image never loaded). The organism moves
  `current`, waits for no turn to be in flight, and restarts itself through
  its unit. There is no watcher: if the new build does not serve, the
  next launch runs pristine -- generation 0, the binary alone, nothing of the
  folder loaded -- and `nodecode rollback N` restores a sealed generation.

The classes were each proved in a bare SBCL first, and the classifier is
measured, not asserted: one published build to the next day's
pin was hot (39 definitions across 7 files, one added export); the same
build to a week of refactoring was a restart (9 structs, 1 package, 9 files
of boot state) with 1271 definitions that would have been hot.

No quicklisp is involved at any point. The dumped image carries every
dependency and the compiler; a hot load is `eval` of definition forms read
from the new tree. A new `depends-on` in the `.asd` is by definition a new
binary, which is the restart path.

## Who triggers it

| Trigger | What happens |
| --- | --- |
| The gateway's poller (config `update.mode` = `auto`, the default) | Thirty seconds after the gateway starts, and every `update.check_minutes` (360) after that, it reads the channel pointer. A new label is staged, classified and applied: hot now, or a restart at the next idle moment. |
| The release feed (the default; `update.feed` names another host, `off` holds none) | One held event-stream connection to the release host — `<host>/api/release/feed?pointer=<channel>-<arch>-<os>` — answers the pointer's label on connect and again every time it moves. Each new label runs the same check the poller runs, so a box is on a release seconds after `publish.sh` writes it. The poller stays as the backstop: a feed that never lands costs one reconnect a minute (five seconds doubling to sixty) and nothing else. |
| `update.mode` = `ask` | The same, but it stops after staging: the standing board says `update X is staged: … · nodecode update applies it`. |
| `update.mode` = `off` | It never looks. |
| `nodecode update [VERSION] [--check]` | The verb: the organism serving this home takes the update through its route (`POST /api/gateway/update`). With no organism running, the release is staged and linked so the next launch runs it. `--check` stages and classifies only. |
| `curl … install.sh \| sh` | The one-liner still works: stage, link, restart every active gateway unit, as before. |

The channel pointer is one line at `<release-url>/<channel>-<arch>-<os>`
(`stable-x86_64-linux`) beside the tarballs; `publish.sh` writes it with
every release (`NODECODE_PUBLISH_CHANNEL=beta` for the other channel).
The web app beside that path serves the same fact pushed, at
`/api/release/feed?pointer=<name>`: an event stream that answers the label
now and on every move. That is what the feed holds open.

## What moves, in order

1. **Stage.** The organism runs the installer it shipped with
   (`install.sh --stage`, `NODECODE_VERSION=<label>`): fetch, sha256, unpack
   under `<release-directory>/<label>/`, probe the staged binary with
   `--help`. `current` does not move. A staging failure ends here, with the
   installer's last line as the reason.
2. **Classify.** `CLASSIFY-UPGRADE` reads the staged `src/src/` against the
   tree this image loaded from, and the staged `src/cells/` against the
   shipped cells when both sides carry one. Files of a package this image
   never loaded (a cell not enabled here) are left alone.
3. **Apply, hot.** `CONVERGE` takes an undo log of every name about to move,
   lowers the kernel floor, evaluates each hot definition from the staged
   file through `LOAD-SOURCE :ONLY` (a changed macro or inline function
   drags its callers along), raises the floor, moves the image's self-view
   (`*SOURCE-ROOT*`) and its effective version to the new label. Any
   definition that will not evaluate puts everything back and refuses,
   naming it. Then `current` moves, an `organism.upgraded` fact lands in the
   log, and every attached shell receives an `organism.upgraded` frame and
   converges its own image on the same tree.
4. **Apply, restart.** The board says `update X needs a restart (reason); it
   restarts at the next idle moment`; a fact lands. A thread waits until no
   session has a turn without a terminal fact, then `current` moves and the
   gateway takes the one idle exit (`RESTART-WHEN-IDLE`, gateway.lisp, the
   exit a `rollback` takes too) for its unit to start it again. With no
   unit to come back through, the move stops at the link and the board says
   so.

## What is kept

| Fact | Hot | Restart |
| --- | --- | --- |
| Store rows, sessions, transcripts | kept | kept |
| A turn in flight | untouched | never interrupted: the restart waits for idle; the boot-resume guard remains the net if the process dies anyway |
| Attached shells | converge in place, one system row each | reconnect on the ladder, one build-skew line if their binary is older |
| Peripherals, MCP sessions, channel adapters, cron timers | kept | restart with the process |
| The layer | kept; overrides the new build skips are on the board | same |
| Page and undo state in a shell | kept | kept (the shell does not restart) |

## What is told

- The standing board (every shell, `gateway status`, the identity route)
  carries one line under `update`: staged, needs a restart, updated to X,
  pinned at a release by a restore, or the channel did not answer.
- The identity route and the connect ack report the **effective** version;
  the identity route also carries `build` (the binary's own) and `runtime`
  (the folder it runs from). `gateway status` prints `build X (binary Y)`
  when the two differ.
- Layer files the new build skipped are one standing line under `layer`,
  not only the boot line.
- Every upgrade is an `organism.upgraded` fact in the event log: from, to,
  mode, the plan's line, the held reasons.

## What rolls back

- **A hot load that fails mid-way** restores its undo log; the image is as
  it was and the refusal names the definition. If restoring itself fails,
  `current` never moved, so a restart on the old build is the way back.
- **A staged build that does not run** (`--help` does not answer) is refused
  at staging; nothing moved.
- **A restart onto a build that does not serve** has no watcher: a launch
  whose last self died before serving runs pristine -- generation 0, the
  binary alone, nothing of the folder loaded -- and hands the `layer`
  session the sealed generations; `nodecode rollback N` (or `(rollback N)`)
  restores one, every folder and the release it names, and the next launch
  runs it (`src/src/organism/generation.lisp`).
- **A rollback across releases** relinks `current` at the release generation
  N names and pins it (`~/.nodecode/release-pin`): the poller leaves the
  channel alone and says so on the board until `nodecode update` lifts the
  pin.
- **The store across versions.** A changed event kind or schema holds the
  upgrade to the restart path, where the new build's migrations run at
  open. Rolling back after a forward migration is the one move this runbook
  does not automate: take a copy of the store before a restart-class
  upgrade you may want to reverse.

## The proofs

- `just test` subsets `upgrade` and `update-`: every class hot or restart
  by test; converge all-or-nothing and under a locked floor; the installer's
  stage and link modes against a release host on disk; the route hot and
  restart-scheduled; the release pin honoured by the poller and lifted by an
  update; a shell's fold; the
  feed's URL, its event reader and its reconnect loop, offline against a
  stub opener.
- The live proof recipe (scratch `HOME`, a release host on `python3 -m
  http.server`, three derived tarballs: a definition-only change, a struct
  change, a build whose gateway exits) is in the issue's closing comment.
