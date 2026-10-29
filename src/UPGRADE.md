# Upgrading a running organism

How a published build reaches an organism that is already running, on a box
nobody is sitting at. The decision and the runbook; the
code is `src/src/organism/upgrade.lisp` (the mechanism over the image),
`src/src/surface/update.lisp` (the policy around it) and
`src/tools/install.sh` -- `install.ps1` on Windows -- (the layout on disk).

## The decision

An upgrade classifies itself before it touches anything, then takes one of
two paths. Both end with `current` naming the new version.

- **Hot.** Every changed definition can be evaluated into the live image, so
  it is, in load order, all or nothing, with no restart. The organism keeps
  its peripherals, its MCP sessions, its adapters and timers, its shells.
- **Restart.** Something in the diff cannot go live (a struct, a package
  past its exports, a constant, a store kind, a form outside any
  definition, a system the image never loaded). Every definition that can
  go live still does, now; `current` moves at once, and the held boot state
  lands at the next boot. A gateway running under a unit nodecode wrote
  makes that boot itself, exiting once no turn is in flight for the unit to
  start it again; anywhere else (a shell's own organism, every Windows box)
  it is the next launch. There is no watcher: if the new build does not
  serve, the next launch runs pristine -- generation 0, this binary alone,
  nothing of the folder loaded -- which recovers a folder that kills the
  boot. A release that itself does not serve is undone with
  `nodecode update PREVIOUS`, which works with no organism running.

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
| The gateway's poller (config `update.mode` = `auto`, the default on an installed build; `off` on a checkout's) | Thirty seconds after the gateway starts, and every `update.check_minutes` after that (360 where the release feed runs, else 30), it reads the channel pointer. A new label is staged, classified and applied the first moment no turn is running: whole if hot; for a restart-class release, what can go live, and the rest at the next boot. The poller runs in whichever process owns the organism: the background gateway, `nodecode serve`, or a shell that started its own (on Windows, which has no background gateway yet, only the last two). |
| The release feed (the default; `update.feed` names another host, `off` holds none) | One held event-stream connection to the release host — `<host>/api/release/feed?pointer=<channel>-<arch>-<os>` — answers the pointer's label on connect and again every time it moves. Each new label runs the same check the poller runs, so a box is on a release seconds after `publish.sh` writes it. The poller stays as the backstop: a feed that never lands costs one reconnect a minute (five seconds doubling to sixty) and nothing else. |
| `update.mode` = `ask` | The same, but it stops after staging: the standing board says `update X is staged: … · nodecode update applies it`. |
| `nodecode update VERSION` | Moves to VERSION -- older or newer -- and holds this home there (`~/.nodecode/release-pin`): the poller leaves the channel alone, and says so on the board, until a bare `nodecode update` lifts the hold. This is the way back after an update. |
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
   definition that will not evaluate -- or a provider this image served
   that stops resolving -- puts everything back and refuses, naming it; the
   release is then linked for the next boot instead, and the board says
   `X runs at …: this image refused it live`. Otherwise `current` moves, an
   `organism.upgraded` fact lands in the log, and every attached shell
   receives an `organism.upgraded` frame and converges its own image on the
   same tree.
4. **Apply, restart.** The same `CONVERGE`, under the same turn hold, takes
   every definition this image can; the held boot state stays on disk.
   `current` moves at once, a fact lands, and shells converge what went
   live. The board says `X is live: N definitions across M files; boot
   state waits for … (reason)` -- or `X runs at …: no definition of it can
   go live here` when nothing could. Under a unit nodecode wrote, a thread
   then waits until no turn is in flight and the gateway takes the one idle
   exit (`RESTART-WHEN-IDLE`, gateway.lisp, the exit a `rollback` takes
   too) for its unit to start it again on `current`. With no unit to come
   back through -- every Windows box, a shell's own organism -- the move
   stops at the link and the boot state waits for the next launch.

## What is kept

| Fact | Hot | Restart |
| --- | --- | --- |
| Store rows, sessions, transcripts | kept | kept |
| A turn in flight | untouched | never interrupted: the restart waits for idle; the boot-resume guard remains the net if the process dies anyway |
| Attached shells | converge in place, one system row each | converge in place what went live, then reconnect on the ladder after the restart, one build-skew line if their binary is older |
| Peripherals, MCP sessions, channel adapters, cron timers | kept | restart with the process |
| The layer | kept; overrides the new build skips are on the board | same |
| Page and undo state in a shell | kept | kept (the shell does not restart) |

## What is told

- The standing board (every shell, `gateway status`, the identity route)
  carries one line under `update`: staged, live with boot state waiting
  for a restart, runs at the next launch, held by `nodecode update X`, a
  newer or unpublished build staying put, or the channel did not answer. A
  hot release clears it; what the release changed is the `release` note,
  said once in the model's words.
- The identity route and the connect ack report the **effective** version;
  the identity route also carries `build` (the binary's own) and `runtime`
  (the folder it runs from). `gateway status` prints `build X (binary Y)`
  when the two differ.
- Layer files the new build skipped are one standing line under `layer`,
  not only the boot line.
- Every applied upgrade is an `organism.upgraded` fact in the event log:
  from, to, mode (`restart` only when the image refused it live), the
  plan's line, the held reasons. Staging alone records none.

## What rolls back

- **A hot load that fails mid-way** restores its undo log; the image is as
  it was and the refusal names the definition. `current` still moves to
  the release, which runs whole at the next boot; `nodecode update
  PREVIOUS` is the way back.
- **A staged build that does not run** (`--help` does not answer) is refused
  at staging; nothing moved.
- **A restart onto a build that does not serve** has no watcher: a launch
  whose last self died before serving runs pristine -- generation 0, this
  binary alone, nothing of the folder loaded -- and hands the `layer`
  session the sealed generations; `nodecode rollback N` (or `(rollback N)`)
  restores one -- the operator's folders; the binary and its shipped cells
  stay on the release it runs -- and the next launch runs it
  (`src/src/organism/generation.lisp`). When the release itself is what
  fails, the way back is `nodecode update PREVIOUS`.
- **Going back to an older release** is `nodecode update VERSION`: it
  stages and links VERSION and holds this home there
  (`~/.nodecode/release-pin`); the poller leaves the channel alone and
  says so on the board until a bare `nodecode update` lifts the hold. A
  restore holds nothing.
- **The store across versions.** A changed event kind or schema holds the
  upgrade to the restart path, where the new build's migrations run at
  open. Rolling back after a forward migration is the one move this runbook
  does not automate: take a copy of the store before a restart-class
  upgrade you may want to reverse.

## The proofs

- `just test` runs them, and `cd src && NEEDLES=upgrade,update- sbcl
  --non-interactive --load tools/test-subset.lisp` runs only these: every class hot or restart
  by test; converge all-or-nothing and under a locked floor; the installer's
  stage and link modes against a release host on disk; the route hot and
  restart-scheduled; the release pin honoured by the poller and lifted by an
  update; a shell's fold; the
  feed's URL, its event reader and its reconnect loop, offline against a
  stub opener.
- The live proof recipe (scratch `HOME`, a release host on `python3 -m
  http.server`, three derived tarballs: a definition-only change, a struct
  change, a build whose gateway exits) is in the issue's closing comment.
