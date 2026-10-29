# nodecode

A self-modifying agent organism in SBCL. It owns its own gateway, session,
turn loop, and persistence, and treats the running Lisp image as the material
it modifies.

This file is the orientation primer.

## Why this is not the Zig kernel

Nodecode was a Zig kernel with a V8-hosted extension lane before it was this;
that tree has since left this repo. The parting was not
a capability gap. That
kernel was a deliberately immovable substrate: it provided mechanism, and
extensions provided patterns, which capped runtime self-modification at the
extension layer by design.

This organism's entire value is that its floor is movable. `eval` reaches every
definition, `save-lisp-and-die` persists compiled code rather than replayed
source, and the kernel/evolved split is a decision the organism can revisit
rather than a property of the compiler. Merging the two would cap the organism
at the kernel's ceiling.

## Scope fence

`src/` is the product. It reads `resources/`, and nothing else outside itself.

- The root `justfile`'s plain recipes are this system's: `just lint`, `build`,
  `test`, `release`, `check`, and one `<cell>-{build,test,check}` lane each.
- Nothing here references the Zig tree; it is not in this repository.
- This was never a port of the Zig `src/gateway/` (29,402 non-test Zig lines).
  Snapshot codec, provider prewarm, extension trust/agency/secrets and tool
  policy were out of scope then and are not owed now.
- The gateway's HTTP routes and WebSocket sync transport are the one surface
  every consumer attaches through: the TUI, the web page, and a program
  through `nodecode rpc` (`src/surface/rpc.lisp`), which carries the
  gateway's commands in on stdin and its frames out on stdout. Every command
  and frame shape is `src/wire/protocol.schema.json`, beside its owner
  `src/wire/protocol.lisp`; a test holds the two to one list of kinds.
- Zig FFI stays unused until `sb-sprof` names a hot spot. Measured FFI overhead
  is 8ns/call, so the option stays cheap and unexercised.

## Layout

```
src/
  nodecode.asd    system definition; the component list is the load order
  CELLS.md             the cell contract: one folder, two shapes, advice
  src/package.lisp     the terminal core, the kernel/evolved boundary
  src/term/            the terminal core beneath both shells: messages and
                       the model protocol, the codepoint width table, the
                       cell painter, the tty and its modes, the byte
                       decoder, kitty graphics, the renderer, the program
                       loop (derived from cl-tuition and reduced to what the
                       shells run; NOTICE carries the attribution)
  src/waist/           the declaration waists everything reads through:
                       conditions (DEFINE-ERROR, DEFINE-RECORD), json (decoded
                       wire values, the lenient dialect), contract
                       (DEFINE-CONTRACT: one boundary payload, and the record,
                       decoder, encoder and JSON-Schema projection derived from
                       it), config (DEFINE-SECTION)
  src/wire/            invocation provenance and the typed gateway sync
                       protocol
  src/store/           durable substrate: store lifecycle, event log, state KV
  src/session/         the session domain: identity, turn lifecycle, session,
                       workspace snapshots, turn-tree checkpoints, recall
  src/organism/        self-modification: gc, hook, evolve (the floor),
                       scribe (the layer), cells (the folder), launch (the
                       warm cache and the fresh-process seams)
  src/engine/          the turn engine: provider lanes, context engine,
                       turn loop, background exec
  src/tui/             shell material: theme, editor, slash, keys, ui
  src/surface/         the network surfaces: gateway server, TUI client, rpc
  src/import/          another coding agent's home brought into this one:
                       readers (yaml, toml), worlds, the plan, the takeover
                       (package nodecode.import)
  src/warm.lisp        the release dump's warm-up, loaded by nodecode/bin only
  test/                mirrors the src/ grouping; support.lisp at the root
  cells/               bundle.json: the commit of nodecode-ai/cells every
                       shipped cell is pulled at (`just cells-pull`), one
                       git-ignored directory each carrying its own .asd:
                       channels/ (kit/, discord/, telegram/, slack/), guard/, chrome/,
                       websearch/, mcp/, cron/, ..., and template/, the
                       skeleton to copy (see below)
  tools/               structure lint, bloat ledger, install and release scripts
```

One owner per concern, one file per owner. The `.asd` names every component
explicitly in canonical load order — a directory is a file's address, never
its load position, so `session/checkpoint` still loads before
`engine/provider`. A new file lands by taking its place in that list.

## The two packages

`nodecode.kernel` is the floor: what must run before any layer or
survive any layer — transport, durable history, collection scheduling, the
lock, the hook primitive, the folder loader and its report, the
fresh-process seams, the scribe. The boot owner applies
`sb-ext:lock-package` to it.

`nodecode.evolved` is the mutable material: prompt, tool table, turn loop,
projections, UI methods.

Beneath both sits `nodecode.term` (`nlt`), the terminal core neither owns;
beside them, `nodecode.import` (`nik`) holds the import folder.

The turn loop sits in `evolved` deliberately. It is the primary mutation target,
and protecting it would defeat the point of choosing this substrate.

## The layer: what the organism keeps

Defining something is keeping it (ADR-0229). Every defining form the model
evaluates through `eval` is written to its layer,
`~/.nodecode/cells/<git-identity>/`, one file per definition in the
EVOLVED package, and committed with the session id. The folder loads over
the shipped source at the next boot in every process — owner, attached TUI,
trial child. There is no keep verb, no pending list, and no remove verb
either: `(fmakunbound 'name)`, `(makunbound '*name*)`, `(unhook 'name
"key")`, `(remove-method ...)` or `(unintern 'name)` evaluated as a
toplevel form takes the name out of the image and its file out of the
layer, committed as `forgot name`. A file deleted by hand is simply gone at
the next boot; the `.asd` is regenerated from the folder at the next write.
Undo of a whole change is `git revert` in the folder and a restart; share
is `git push`; install is `git clone`
into `~/.nodecode/cells/`. The shipped source under `src/` is edited by
humans only; the binary carries it (`src/src/` beside the executable), so
the organism can read itself — `(definition-text 'name)`,
`(definition-location 'name)` — on any machine, and an upgrade's boot line
names every layer file it skipped — an override the shipped source moved
under, or a definition naming a function or variable that nothing defines
any more — and a skipped file does not load: the shipped definition stands
until the organism records it again over the new source, or forgets it, the
work the boot hands the model once, in the session `layer`. Prefer
advice to override: `(define-hook compose-frame "mine" (next &rest args)
...)` wraps a shipped function and survives upstream rewriting its body —
and, being a defining form, it is kept in the layer and re-registered at
every boot, where a bare `(hook ...)` call lasts until restart. Layers load
after every peripheral, so advice on a function a cell provides finds
it. `(cells)` is the report; `/cells` renders it. `CELLS.md` is the
contract.

## Profiles: several homes, one binary

A profile is a home. `~/.nodecode/` is the default one; a named profile is
`~/.nodecode/profiles/<name>/`, laid out exactly the same — `config.jsonc`,
`secrets/`, `auth.json`, `SOUL.md`, `AGENTS.md` (the operator's rules, which
every session reads as its `<harness key="operator-rules">` section, in any
folder, ahead of a project's own AGENTS.md), the state entries flat at the root
(the store, the operator token, the use ledger `usage.jsonl`), `cells/` (the
layer, the knowledge cell of memories and skills, and the installed
peripherals). Every path the organism uses resolves through `(nlk:home
"...")` at process start (`src/src/waist/paths.lisp`), so a profile is a
different organism, not different settings: its own layer of definitions,
its own store, its own gateway on its own ephemeral port. What is shared
is the binary with its source, `~/.cache/nodecode/` (keyed by content,
never by home) and the operating system's home.

A new profile shares rather than copies what belongs to the operator and to
the project: `secrets/` and `auth.json` are links to the home it was made
from — the rows `+HOME-ENTRIES+` marks SHARED — and so is each knowledge cell
under `cells/`, so one correction lives in one place and a credential has
exactly one owner. The
channels are left behind (the copied `config.jsonc` goes without its
`channels` member), because a bot token belongs to one home: two homes
polling one bot answer each other's 409. The new home configures its own, or
runs with none. The home is built in a dot-named staging directory and
published with one rename, so nothing walking `profiles/` ever sees a
half-made home.
`config.jsonc`, `SOUL.md`, `AGENTS.md`, `themes.json` and the installed peripherals are
copies, and the store and layer are always fresh. Two homes on one folder
make two writers of it, so every write to a shared folder takes that
folder's lock — `~/.nodecode/cells/.locks/<identity>-memory.lock` for the
knowledge cell, resolved through the link, so both homes contend for one file
(`src/src/waist/os.lisp`). `--clone-all`
is a fork and copies even the shared rows, because an organism of its own
wants its own history.

Which home a launch boots, first found wins: `-p NAME` on the command
line, `$NODECODE_HOME` (any directory; a profile named after its folder),
`~/.nodecode/active-profile` (the sticky file `profile use` writes),
`~/.nodecode/` itself. The home a process ends up in is published to its
children as `NODECODE_HOME`, so a serve candidate, a trial or a shell the
model runs lands in the same home. The verbs are the kernel's
(`src/src/organism/profile.lisp`); `nodecode profile ...`, `/profile`, the
model's `(profiles)` and the web page's Profiles tab (its routes under
`/api/gateway/profiles`, `src/src/surface/control.lisp`) are shells over them:

```sh
nodecode help profile                      # the verbs; every command answers --help, and `profiles' is `profile'
nodecode profile list                      # * this process's, `boots by default', a live gateway's pid and port
nodecode profile create coder              # a home of its own — config, SOUL, themes, the peripherals, a fresh store and layer — sharing the knowledge cell, secrets and auth.json with the home it came from
nodecode profile create empty --blank      # an empty home; the setup walk runs at its first launch
nodecode profile create fork --clone-all   # a fork: the layer with its git history, and even the shared rows copied
nodecode profile use coder                 # the sticky file; `use default' removes it
nodecode -p coder                          # run it; `coder` too, a wrapper create wrote under ~/.local/bin/
nodecode profile delete coder              # asks first on a terminal, --yes in a script; refused while a gateway serves it
nodecode profile export coder coder.tgz    # without the state entries, secrets/, auth.json
nodecode profile export default home.tgz --whole   # the home to come back to: secrets, credentials and a copy of the store too (what /backup keeps)
nodecode profile import coder.tgz --name coder2
nodecode profile install git@host:team/coder.git   # a distribution: a home in a git repository with a profile.jsonc
nodecode profile update coder              # git pull --ff-only --autostash; what the organism keeps is excluded from git
```

`--ephemeral` still varies only the store; `HOME=$(mktemp -d) ./nodecode`
is the whole-organism isolation a proof wants. A channel's bot token is
leased per machine under `~/.cache/nodecode/leases/`: two profiles never
poll one bot.

## Load

```sh
sbcl --dynamic-space-size 8192 \
     --eval '(push #p"~/nodecode/nodecode/src/" asdf:*central-registry*)' \
     --eval '(asdf:load-system :nodecode)'
```

The `--dynamic-space-size` is not optional for an image that will serve:
SBCL's 1GB default heap died in production under a 418MB core
("Heap exhausted during garbage collection", 2026-08-18). The dumped
`./nodecode` carries its own ceiling, baked in by `just release`.

Tests: `just test`, which runs this from `src/`:

```sh
sbcl --eval '(push (truename ".") asdf:*central-registry*)' \
     --eval '(asdf:test-system :nodecode)'
```

## Providers: base_url, sdk, api_key

A provider in `~/.nodecode/config.jsonc` is an endpoint and the dialect it
speaks; which models it serves is the endpoint's own answer:

```jsonc
"providers": {
  "relay":   {"base_url": "https://relay.example/v1", "sdk": "openai-completions", "api_key": "sk-…"},
  "gpu-box": {"base_url": "http://100.101.102.103:8080/v1", "sdk": "openai-completions",
              "proxy": "socks5://localhost:1055"},
  "claude":  {"base_url": "https://claude-relay.example/v1", "sdk": "anthropic", "api_key": "sk-…"}
}
```

`sdk` is one of `openai-responses`, `anthropic`, `google`, `openai-completions`, and it
is required: the wire every model of the entry rides, declared, never learned
from the endpoint — an entry naming none is refused by name. A model that
needs another wire at the same endpoint is a second provider entry, or a
`models: [{"id": …, "sdk": …}]` pin.
`api_key` is optional (the chain falls through to `auth.json`, the
environment, then `"public"`), as are `proxy`, `display_name`, and the
`stream_*` retry tuning. `/models` fills itself from `GET <base_url>/models`
— `data[].id` on the openai-family and anthropic lanes, `models[].name` on google —
and caches the answers in one file (`~/.cache/nodecode/provider-models.json`,
ten-minute TTL, `/models refresh` forgets them). A window hint the listing
happens to carry (`max_model_len`, `inputTokenLimit`) shows as the row's
`ctx`. A provider whose listing fails keeps one `provider/` row naming the
reason.

`"models": [{"id": "…", "context_window": N}]` stays legal and optional: the
pinned aliases an endpoint serves but does not list, or an endpoint that
refuses to list at all. Configured rows merge with the fetched ones; a
configured window outranks the fetched hint on the same id.

## The two model pins: default and auxiliary

Which model answers is one object under `general`, and which model does
the side work — an agent's naming call, anything a layer or a cell
runs under `with-auxiliary-model` — is a second object of the same shape:

```jsonc
"general": {
  "default_model":   {"provider": "relay", "model": "claude-opus-5"},
  "auxiliary_model": {"provider": "relay", "model": "claude-haiku-4-5", "effort": "off"}
}
```

`provider` and `model` are both required in both — a model under an
unnamed lane is how a config once came to name a model no lane served —
and the object is the `session.model_selected` payload, so a pin and a
config entry are one value spelled one way. `effort` is optional: on the
auxiliary it is the whole answer (absent means the weakest rung the model
declares, and a model declaring none sends no effort at all); on the
default it is the effort a session that picked none runs at
(`NODECODE_REASONING_EFFORT`, when set, seeds it in its place).
The flat `default_provider` / `default_model` strings are retired and
refused by name on the first frame. A `/models` pick, in a shell or in
the web page's chat, pins its session and writes `default_model`, so every
session opened after it starts on the pick; a session keeps the model its
first turn ran, so the sessions already begun stay where they are. A
`/think` pick does the same with `default_model.effort`: it pins its
session and becomes the effort every session opened after it starts at,
while the sessions already begun keep the effort their first turn ran at
(one begun with no effort at all follows the default). The web page's Control ›
Settings writes `default_model` and pins no session; `/models aux` opens
the same catalog for the auxiliary model — the pick lands as
`auxiliary_model` and pins no session — and
`/models aux <provider> <model>` types it. Without an auxiliary model,
side work runs on each session's own model at its weakest declared rung.

A third pin of the same shape, `decider_model`, names the decider: no
chat model, but one that answers a map of named questions about a state as
probabilities (`POST <base>/v1/systemone`). DJev (`djev`, model `dgemma`)
is the one offered, at `http://127.0.0.1:8011` unless
`providers.djev.base_url` names another address. `/models decider` opens
its tab, `/models decider djev` pins it and `/models decider none` takes it
out. /models' Main tab also gathers, under Local, the models of every
provider whose base_url is a machine of your own: localhost, a private or
tailnet address, or a `.local` / `.ts.net` name.

## nodecode.ai's models: Cyber, Image, Decide

Nodecode's own models answer at `https://api.nodecode.ai/v1`, OpenAI's
wire, under the provider id `nodecode`. `nodecode login` (or `/login` in the
shell) signs in in a browser, a subscription or API credits, and saves the key
(`sk-nc-…`) at `auth.json` `api_keys.nodecode.key` and writes
`providers.nodecode` (`openai-completions`, that base URL); a key from
https://nodecode.ai/account/keys works the same, and any other key answers
401. The gate (nodecode-website `crates/gate`, its models in
`deploy/gate.toml`) serves `POST /v1/chat/completions`,
`POST /v1/images/generations` and `GET /v1/models`:

| Model | Id | Call |
| --- | --- | --- |
| Cyber v1, the security model | `foresight-cyber-v1` | chat; 1M context, 16K out |
| Image v1 | `foresight-image-v1` | images: `n` 1-4, `size` `WxH` or `auto`, `b64_json` only |
| Decide v1, the classifier | `foresight-decide-v1` | chat: the last user message is a decision request as JSON (`state`, named `questions`), the answer JSON in the reply's content |

The organism reads the same as `(help :models)`, a topic whose line rides
every request (`*HELP-TOPICS*`, `src/engine/turn.lisp`): which model answers
what, the key, and each call written out, a side call to any of them
through `COMPLETE` with `*AUXILIARY-MODEL*` bound. The decider pin above
offers DJev alone; Decide v1 is reached through the API.

## Sessions: Left leaves one running

A session runs whoever is attached to it: switching away, `/clear` or
closing the shell never stops a turn. **Left** on an idle composer (nothing
typed) is the gesture for it — Claude Code's: the shell goes home on a fresh
session with `/sessions` open and the highlight on the session it just
left. `/sessions` (Ctrl+S, `/agents` too) lists every session as a tree of
forks, this folder's first and every one a Tab away; a running session
carries a spinner and the turn's clock, read off the one running-session
map every shell holds (`src/surface/activity.lisp`, the `organism.activity`
frame). The highlighted session's transcript shows above the picker and,
once the highlight rests, runs live — the shell follows that session with a
second, live-only subscription, so its typing, tool calls and answers land
in the preview as they happen, with the running turn's bar beneath it.
Enter attaches it, F2 renames it, typing searches every conversation. The
footer advertises the gesture as `← sessions` beside the model name, or
`← sessions · 2 running` while sessions are at work.

The model reaches the same sessions through plain functions: `(nlk:list-sessions)`
lists them, `(submit "id" "text")` puts a prompt into any session and
`(recall-log :session-id "id")` reads one's log.

## Channel cells (Discord / Telegram / Slack)

Optional ASDF systems, not core: no system in `src/` depends on them (the
import and setup paths name them only to install or restart them). A cell is
a directory carrying its `.asd` under `~/.nodecode/cells/` (in development,
a symlink: `ln -s $PWD/src/cells/channels/kit ~/.nodecode/cells/nodecode-channel-kit`);
every process loads the folder at boot, and the gateway calls each
peripheral's `START-CELL (config) => stop-thunk` after recovery; the kit then starts every
enabled `channels.<id>` section (verbatim Zig channel pack keys, same
`~/.nodecode/config.jsonc`). An adapter lives inside the gateway image and
reaches the organism through its in-process seams — `(nle:hook :frame ...)`
to observe every live frame, `nle:submit` to put a prompt into a session,
the `nlk` session API to make sessions durable — never by dialling the
gateway it runs beside. Sessions are deterministic and durable, so you can
`resume` them from the TUI, and `/channels` shows lane status. Admission is
fail-closed: a lane
refuses to start unless at least one of `allowed_chats`/`allowed_channels`/
`allowed_users` (Discord: or `allowed_roles`) is populated (channel messages
run with full host authority), and secrets are only ever `*_env`/`*_file`,
never inline. A stranger's direct message is answered with a pairing code
the operator approves with `/channels pair CODE`; `/sethome` makes a room
where the bot reports to the operator; `agents` name what a room's asks run
as (a folder, a persona, a prompt, a model, skills), `routes` send asks to
them by server, channel, thread, person or role, and `/agent` and a room's
own `/models` set a room's at runtime (`kit/operator.lisp`).
`discord/README.md` opens the Discord guide, and `discord/docs/` holds the
rest: the portal walk, the permission integer, the intents, every key and the
room's commands. Proof lanes: `just channels-check`, and `just discord-qa`,
`just telegram-qa` and `just slack-qa`, the built shell against a loopback
Discord, Telegram or Slack; manual live smoke:
`rooms/live-smoke.lisp` in nodecode-ai/cells. The `just *` recipes register each cell
directory with ASDF the same way the folder loader does.

**Setting one up is a conversation.** Install the kit and an adapter folder
and launch: the boot line names both, and because the adapter is present
without a section the kit posts `discord: unconfigured — channels.discord
is absent; the adapter folder's config.example.jsonc shows the section` —
a standing notice (`nle:notice`) that every attached shell renders as a
system row and the model reads in the `notices` block at the tail of every
round's prompt — live material rides behind the cached prefix, never inside
it. Copy the
example in by hand, or say *set up discord; the token is in
DISCORD_BOT_TOKEN* to the agent: while an installed adapter is
unconfigured or refused, the kit's `(help :channels)` topic teaches the section's keys, the portal walk (application, bot token,
Message Content intent, invite URL; BotFather and `/setprivacy`), and the
four forms — `(config-get '("channels" "discord"))` to read the section
back, `(config-set '("channels" "discord") '(:bot_token_env
"DISCORD_BOT_TOKEN" ...))` to write it (references, never values; the
answer is the diff), `(nck:probe "discord")` to check the token against
the platform and list what the bot can see (its identity, the servers and
their text channels with ids, whether the intent is granted; Telegram:
the chats that have written to the bot) so ids are picked by name, and
`(restart-cells)` to start the lane without a relaunch. A section that
refuses — no allowlist, an unset variable — is `discord: refused — …` the
same way, and `discord: running, connected` the moment READY lands;
`/channels` shows the same rows. A present section is enabled;
`"enabled": false` vetoes.

Discord speaking is configurable per channel. `free_response_channels` admits
every allowed human message, `require_mention_channels` records unaddressed
messages as room context until a mention or bot reply arrives, and
`ignored_channels` drops them entirely. An ignored entry wins overlaps, then
free response, then mention required; unlisted channels use `require_mention`.
Discord threads inherit entries matching their active, parent, or thread id.
`mention_patterns` adds case-insensitive wake words to the entity mention.

Every channel is two tiers, because a chat is a room full of people rather
than one correspondent (a Telegram DM is a room of one). The kit's host
(`kit/host.lisp`, `kit/room.lisp`) runs the topology; an adapter contributes
its platform — the message, edit, delete and typing plans, the chunk limit,
the typing cadence, the mention, the session-id prefix — and its transport.
Discord names them `discord-…`, Telegram `telegram-<chat>[-t<topic>]`:

```
  ROOM   discord-<channel>[-t<thread>]     the shared record of the channel.
  ==[E1]======[E2]======[E3]==>            never runs a turn; head advances
     |  ^        |  ^                      as settled exchanges land
  fork  wb    fork  wb
     |  |        |  |
    [LANE]     [LANE]   discord-<channel>[-t<thread>]-m<message>
    alice      alice    one ask, one lane, forked at the room's head
```

Every ask forks its own lane, so asks run in parallel — including two from
the same person. The lane composes the room's history by reference, frozen
at its fork anchor, runs the turn, and writes the settled exchange back to
the room as one turn; a later lane inherits it, and the lane that produced
it cannot see it return. Chatter the bot was not addressed in buffers
live-only and rides the next ask's prompt, so the room's log stays one user
message and one assistant message per turn. Where the platform opens
threads (Discord, `thread_behavior` `create_per_message` by default), an ask
typed in a channel opens a thread off its own message and answers there
alone; the thread's first exchange also settles in the channel's record, so
the next ask typed there forks above it. A thread the bot takes part in — one
it opened, or one it answered in — is one conversation, as a DM is: a line a
person types there reaches the bot without a mention, and a line typed while
a turn runs steers it. Everywhere else a reply to anything a lane produced
continues that lane — steering it while it runs (the running turn ends at
its next round boundary and the reply runs as the lane's next turn), opening
its next turn once it has settled — so the reply chain carries the branch
structure. Concurrency
is rationed live-only by `max_concurrent_turns` (round-robin by author, a
queued ask says where it is in line) rather than by the gateway's durable
per-session queue, so a restart drops pending asks instead of resurrecting
a backlog. Settled lanes stay addressable for 30 idle minutes, then reap.
While a turn runs the surface shows a typing beat; a turn that calls a tool
or thinks long earns one card of its steps, edited in place, that settles
above the answer as the turn's record when the answer lands as a reply to the
ask — or, in a thread the kit opened, as its first answer naming the asker —
the one message that notifies. A failed or stopped turn settles its notice
into that card, and the card's Details answers its presser alone.

**SOUL.md** is the channel persona, the convention hermes-agent and openclaw
share: a file the operator edits holding the agent's voice — tone, stance,
boundaries — apart from operating rules. The kit reads `~/.nodecode/SOUL.md`
(or `channels.<id>.soul_file`, per channel) before every ask and keeps it
as the session's `<harness key="soul">` section, verbatim, after the base
prompt and after the room contract; edit the file and the next
message uses it, delete it and the section clears. Nothing is seeded —
`channels/kit/SOUL.md` is the boilerplate to copy and edit — and nothing is
trimmed: `/channels` shows `soul <path> (present|absent)`, and a
file that does not fit the 64KiB harness budget the room contract shares
is refused with a warning while the ask still runs. The operator's TUI
sessions do not read it. Operating rules are `~/.nodecode/AGENTS.md`'s,
which every session reads, a channel's included; `import` lands another
harness's CLAUDE.md, AGENTS.md, GEMINI.md or global_rules.md there, and a
SOUL.md or IDENTITY.md here.

**The platform API is open to the model.** The adapters use six Discord
endpoints and four Telegram methods; the model is not limited to them. Each
channel exports one function over the live, token-bearing executor —
`(ncd:request METHOD PATH &key body headers timeout retry)` and
`(nct:request ...)` — reached from `eval` like everything else (the
organism stays mono-tool), answering `(:status N :body VALUE [:error TEXT])`
with the decoded body as a keyword plist, arrays as vectors, null as `:NULL`;
a body is written the same way, `(:content "hi")`, or as an `nlk:json-object`
for keys a keyword cannot spell, or as the multipart alist the executor
already speaks for uploads. Any path the token's permissions and intents
allow — reactions, threads, pins, history, members, roles, DMs, webhooks —
is one call; nothing narrows it, and the token never appears in an answer.
The handles ride the record: every user line an adapter submits reads
`<speaker> [m<message id> u<user id>]: <text>` (ambient chatter too, so
anything the model has seen is addressable), and the room's fixed facts —
guild, channel, thread, the bot's own id — plus the primer teaching the
call live in the lane's `<harness key="discord-room">` section (Telegram:
`<harness key="telegram-chat">`), stable across messages so the prompt
prefix stays cacheable.

**The channel itself is open to the model.** Every function in the kit and
the adapters is plain and advisable, and the same contract lists the seams:
`nck:decide-inbound` (admission), `nck:execute-plan` (every outbound call),
`nck:digest-final-text` / `digest-status-text` (what gets posted), the send
functions, and one raw-event seam per platform — `ncd:discord-handle-dispatch
(adapter payload)` for every gateway event within `channels.discord.intents`,
`nct:telegram-handle-update (adapter update)` for every `getUpdates` entry
within `channels.telegram.allowed_updates` (default `["message"]`; list
`edited_message`, `message_reaction`, `callback_query`, `my_chat_member`, ...
to widen it — the Telegram twin of intents). Both run on the connection or
poll thread, so advice returns fast, and the error boundary sits outside the
seam: advice that signals costs that one event, never the connection. Advice
written as `(define-hook nct:telegram-handle-update "key" (next adapter
update) ...)` is kept in the layer and survives restarts.

## Chrome cell (pi-chrome's extension, a Lisp host)

`nodecode-chrome` gives the organism the user's real, signed-in Chrome as a
peripheral. `chrome/browser-extension/` is pi-chrome's MV3 companion
extension, vendored as the Nodecode Chrome Connector with the changes its
NOTICE lists (MIT); the cell copies it to `~/.nodecode/chrome-extension/`,
the folder `/chrome onboard` has the operator load. The pi host side is
re-implemented in `chrome/*.lisp`, minus pi's per-session authorization:
an HTTP long-poll bridge the extension polls on the port its manifest
hard-codes (`127.0.0.1:17318`), and the formatters that make a page snapshot
fit a tool result. The organism stays mono-tool: there is no `chrome` tool,
the model calls plain functions from `eval` — `(chrome:snapshot)`,
`(chrome:click "el-12" :snapshot t)`, `(chrome:navigate url)`, and
`(chrome:send "page.scroll" :delta-y 800)` for any of pi-chrome's thirty
actions — and learns that vocabulary from `(help :chrome)`, whose one-line
summary every session's `help` section carries while the cell runs.

There is no grant to give: loading the cell and the extension is the
consent, and every session may drive Chrome. `/chrome status`,
`/chrome doctor`, `/chrome onboard` (names the unpacked-extension folder to
load at `chrome://extensions`) and `/chrome background [on|off]` (one
session's default: whether actions keep Chrome's focus; the `background`
setting is everyone's) are the operator's controls. Screenshots go to
`~/.nodecode/chrome-screenshots/` and the model gets the path; images do
not yet ride a tool result. Config:

```jsonc
"chrome": {"enabled": true, "background": true,
           "screenshot_dir": "~/.nodecode/chrome-screenshots"}
```

installed as `~/.nodecode/cells/nodecode-chrome` (a symlink to
`src/cells/chrome` in development).

Threat model, as pi-chrome states it: loopback-only, non-extension browser
origins refused, but local processes are not authenticated (the extension
carries no token) — a speed bump, like the guard. Proof lane:
`just chrome-check`.

## Notify (a desktop notice when a turn ends)

A `notify` section in the shared config pops a desktop notification when a
turn completes or fails — pi's `notify.ts`, ported. pi writes OSC 777 from
the TUI process; a turn ends in the gateway image, which cannot reach the
terminal, so the port is a command spawned detached from the frames
`publish-event` lets out (`src/surface/notify.lisp`), and a command that
cannot start is said once, never failing the publish. With no `command`,
Linux gets `notify-send`, Windows and Linux under WSL a toast through
Windows PowerShell, a Mac `osascript`'s notice. The body names the
session's working directory — `Ready for input — nodecode`, or `Turn failed
— nodecode: <detail>`. A cancelled turn is silent (the operator pressed
Esc), and a turn republished after a peer hiccup does not pop twice.

`scope` decides which sessions count. The publish answers whether a
subscribed peer — a TUI — received the frame, which is the one origin
signal a frame carries (provenance is durable-log-only); the default
`attached` notifies only then, which is what keeps Discord and Telegram
turns quiet. `all` is the escape hatch for a turn that finishes after the
TUI closed. `{title}` and `{body}` are replaced inside every word of
`command`, so `--message={body}` shapes work as well as positionals. The
section's presence turns it on, and `"enabled": false` off:

```jsonc
"notify": {"command": ["notify-send", "--app-name=Nodecode", "{title}", "{body}"],
           "scope": "attached"}
```

Limits: the command runs with the gateway's environment, so a gateway
started outside the desktop session (no `DBUS_SESSION_BUS_ADDRESS`) pops
nothing, and a remote gateway notifies its own host.

## Websearch cell (search and fetch from eval)

`nodecode-websearch` gives the model the web as two functions in the `web:`
package, called through `eval` like everything else —
[pi-web-access](https://github.com/nicobailon/pi-web-access) reduced to its
kernel. No tool is registered and no slash command: the cell puts
`(help :web)` on the manual, so every session, TUI or channel lane, carries
one line naming the two calls in its `help` section and reads the rest when
it wants it; stopping the cell takes the topic away.
Both return a string; every failure is `ERROR: WEBSEARCH-ERROR: <detail>`
with any configured key already redacted.

```lisp
(web:search "query" &key provider n)    ; exa (keyless) | brave | tavily; n 1..10, default 5
(web:fetch "https://..." &key offset limit reader)
```

`search` works with no key at all: Exa's MCP endpoint (`mcp.exa.ai`, the
same keyless route pi-web-access falls back to) answers a JSON-RPC
`tools/call` with titles, urls and highlights. A key upgrades it — Exa's
`/answer` and Tavily's `include_answer` synthesise an answer, Brave is a
plain SERP — and Brave and Tavily are keyed only (their free plans still
hand out a key). The text is the provider's own answer when it gives one,
else one snippet per result (500 chars each, so ten results and the list
below always fit) followed by `Source: title (url)`, then a numbered
`Sources:` list. One provider answers one query — no fallback chain, no
fan-out. `fetch` answers `title`,
`url`, then up to `limit` characters (6000 default, 7000 cap, under the
8000 the eval snippet shows) of the page as text: HTML is reduced locally with
regular expressions — `<main>`/`<article>` preferred, script and style
dropped, absolute links kept as `text (url)`, entities decoded — other text
types come verbatim, binary and anything over `max_bytes` are refused. The
extracted page is cached for an hour, so a cut result's trailer, `Showing
A-B of N chars; (web:fetch "url" :offset B) for the next slice`, is a copy
the model makes without a second download. The URL is the id; pi's
responseId store is gone.

`reader: "jina"` — per call or as the default — sends the URL to
`r.jina.ai`, which renders JavaScript and answers markdown. Better text
for an app shell, and a third party that now knows what was read, which is
why it is an explicit choice; a local extraction that looks like a
JavaScript shell (little text, many scripts) says so and points at it.
Config:

```jsonc
"websearch": {"enabled": true,
              "provider": "brave",                      // optional: default is the first keyed, else exa keyless
              "providers": {"brave":  {"api_key": "..."},
                            "exa":    {"api_key": "..."},
                            "tavily": {"api_key": "..."}},
              "max_results": 5,
              "fetch": {"max_bytes": 5242880, "limit": 6000, "reader": "http"}}
```

installed as `~/.nodecode/cells/nodecode-websearch`. Keys are inline, as
the `providers` block has them, and never leave the process: each rides in a
request header, never a URL, and is replaced by `[redacted]` in any error
text. An empty section is the zero-config shape: fetch, and search through
Exa's keyless endpoint.

Limits: one network call per function on the turn worker, 30 s each, so a
slow site wants `yield_time_ms 40000` on the `eval` (`(help :web)` says
so); there is no retry. The extractor is regular expressions, not a DOM:
a page that hides its prose behind JavaScript reads as a shell. Proof lane:
`just websearch-check`.

## MCP cell (MCP servers as Lisp functions)

`nodecode-mcp` is the MCP client: it reads the same `mcp.servers` contract
the retired Zig client read, connects each server — a command over stdio,
or a URL over streamable HTTP — and makes every remote tool a function in
the `mcp:` package, called through `eval` like everything else. No
provider tool is registered. Each tool is generated from the server's
`tools/list` at connect time, named `SERVER/TOOL` in kebab-case, with a
real `&key` lambda list from its input schema and the description as its
docstring, over one chokepoint:

```lisp
(mcp:files/read-file :path "/etc/hosts")         ; a tool: (describe 'mcp:files/read-file)
(mcp:call "files" "read_file" :path "x")         ; the same by name; string keys pass verbatim
(mcp:tools)  (mcp:tools "files")                 ; every tool: call form, arguments (required *), description
(mcp:schema "files" "read_file")                 ; the full input schema
(mcp:status)                                     ; state, tool count, transport, last error, stderr log
(mcp:restart "files")                            ; reconnect one server
```

Keyword arguments fold to the schema's names (`:per-page` is `per_page`,
`:include-snapshot` is `includeSnapshot`); values cross as JSON (`t` true,
`:false` false, `nil` absent, a list an array, a keyword plist an object);
`:timeout` seconds and `:limit` characters are consumed by the call. A
result is the content blocks as text — images and resources as one-line
placeholders, as the Zig client rendered them — clipped near 7000
characters. `isError` arrives as `ERROR: MCP-TOOL-ERROR: server/tool:
<the tool's text>`; a server that is down after one reconnect is
`MCP-OFFLINE` naming its last failure and, for a process, its stderr log
under `~/.nodecode/mcp/<name>.stderr.log`; a deadline that passes is
`MCP-TIMEOUT` and keeps the connection (the late reply is skipped by the
next call). The model learns all of it from `(help :mcp)` — a static
preamble plus the live catalog, one line per tool — while every session's
`help` section carries one line naming the configured servers; nothing
durable is written, and removing the folder leaves no trace. `/mcp` answers one line:
`/mcp status` (the default), `/mcp restart NAME`, `/mcp tools NAME`. The
web page's Control > MCP tab adds, removes and restarts servers through
`/api/mcp`, and its Test lists what a server offers with how long the
connect and the list took, or the failure and what the process wrote to its
stderr: a configured server on the connection the model's calls use (one
that is down gets the reconnect a call would make), a server not yet added
on a connection of its own, closed before the answer. `catalog.json` in the
folder lists well-known servers in shapes this client speaks, each recipe
read from its project's own documentation and cited; the tab's Catalog
fills the Add form from one and asks only for the keys it needs.

```jsonc
"mcp": {
  "servers": {
    "files":  {"command": "mcp-files", "args": ["--root", "/tmp"], "env": {"K": "v"}, "timeout_ms": 30000},
    "remote": {"url": "https://host/mcp", "headers": {"Authorization": "Bearer ..."}},
    "off":    {"command": "x", "enabled": false}
  },
  "inherit_env_allowlist": ["PATH", "HOME"]      // optional; the default is the Zig list
}
```

installed as `~/.nodecode/cells/nodecode-mcp`. A url without a command is
HTTP, anything else stdio; `transport: "http"` overrides. Header values
never appear in any status or error — surfaces show a count. An entry that
fails to validate is one server in state `refused` while its siblings run.
Not supported, as the Zig client had none: `cwd`, `${VAR}` expansion,
per-tool allow and deny lists, OAuth, the legacy two-endpoint SSE transport.

Shape worth knowing: no reader thread. Every read happens on the calling
thread under that call's own deadline — which is also the whole
cancellation story, since Esc never reaches an eval thread — and a
server's `ping` or `tools/list_changed` is only seen while a call is
reading. One short-lived thread per server connects at start (the gateway
never waits); a call that lands mid-connect waits for the server's lock
against its own deadline. Proof lane: `just mcp-check` (48 tests
over an in-memory transport, the python fixture server in `mcp/test/`,
and a clack HTTP fixture).

## Knowledge: memories and skills are definitions, one index

What the organism keeps and does not run is a definition too, kept in a
knowledge cell beside the layer (`src/src/organism/knowledge.lisp`). `(define-memory
name "one declarative sentence" :type :feedback ;; why, how to apply ...)`
keeps a fact — `:type` is `:feedback` or `:user` for the operator's own word,
`:project`, `:reference` or `:lesson`; `:project "ROOT"` shows it only to
sessions under that repository; `:sources` keeps where it came from — and
`(define-skill name "one sentence" ;; when to use ;; the steps ...)` a
procedure in prose; the why and the steps ride as `;;` lines inside the
form, the closing paren on a line of its own. Evaluated at the eval seam,
each is one file in `~/.nodecode/cells/<git-identity>-memory/` (`:cell
"topic"` files it in a cell of that one subject instead), committed like the
layer under the cell's folder lock, since profiles share it. The cell's
`.asd` is a LAYER system carrying `(:knowledge . t)` in `:properties` and
listing no component: the loader records it (`cell you-memory: knowledge, N
definitions`) and never loads it, the launch key walks none of its files, so
a keep costs neither a boot nor the warm image. A `define-skill` with a
lambda list is code — a defun kept in the layer. A name the image already
uses is refused; a name has one home; `(unintern 'name)` as a toplevel form
forgets one, `(edit path old new)` on its file changes it, and `(help
'name)` reads it, docstring and prose.

One index lists everything kept — the layer's documented definitions and
every knowledge cell's — as the harness section `index` at the head of each
conversation, pinned on the conversation's root at its first request and
again only after an eviction or `/index reload`, never at a keep. It ranks by
the use ledger, `~/.nodecode/usage.jsonl` (a line per view, keep, call, and
each helped or harm sighting a reflection records): a memory never cools,
ranked by its evidence and the operator's word first; a skill or a layer
definition is hot while used inside `stale_days`. It lays out within
`hot_budget` — every hot one described when all fit, else the first described
within half of it and the next by name on one `also:` line — under one
`[label]` heading per cell, and its first line counts what `(index "text")`
finds beyond it. `(index)` is the surface now; `/index` answers `N hot · M
cold · K cells` and opens a picker that `$` at a word's start opens too,
Enter writing `$name` into the composer. The web page's Control > Index tab
lists, opens, saves and forgets through `/api/gateway/index`, every write
an eval of the definition's own text. `import` brings another agent's
memories and skills in as definitions with their use (`import nodecode`
converts this organism's own retired `memory/` and `skills/`, and unpins the
two indexes they had pinned on old sessions).

```jsonc
"index": {"hot_budget": 8192, "stale_days": 30}
```

## Two constraints that shape the design

**A child is a fresh process, never a fork.** `fork()` carries only the
calling thread and SBCL refuses it once a second thread is live. With the
organism's definitions on disk (the layer), a fresh process IS the composed
self, so every child execs this same binary with a verb — no ordering, no
pipe protocol, works from a threaded parent. The launch cache builds its
image that way (`warm`, `warm-dump`) and verifies it by asking the new image
one form through `trial`, the verb the release gate and the platform smoke
lanes ask through too (`launch.lisp`).

**GC pauses, not throughput.** Measured on this machine: gen0 collection 10.0ms,
full GC over a populated heap 86.0ms, against a 16.6ms frame budget at 60fps.
Throughput is a non-issue — SBCL runs a typed f64 sum in 53.0ms against Zig's
44.1ms, and JSON frames decode in 7.0us. So history lives in SQLite, the image
holds the active turn's working set, and full GC is forced into the provider's
network wait where hundreds of milliseconds of idle already exist.

## Containment

An environment gate, not a code owner. `eval` has full host authority; a
fresh process isolates crashes only — not filesystem or network damage. A
folder under `~/.nodecode/cells/` is full-authority code too: presence is
consent, the same trust as `eval`.
Run it in a container with seccomp before enabling unattended
mutation.
