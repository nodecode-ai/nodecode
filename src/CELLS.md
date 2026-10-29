# Cells

A cell is a directory under `~/.nodecode/cells/` that carries an `.asd`.
That is the whole contract. Presence is enabled: put the folder there and
every Nodecode Lisp process loads it at boot, before anything else runs;
remove it and it is gone. Nothing in config names cells.

```
~/.nodecode/cells/
  you/                         your layer: what your organism kept, one file per definition
    you.asd                    generated, :class nodecode.kernel:layer
    compose-frame.lisp
    _system-prompt_.lisp
  nodecode-guard/              a peripheral: a system that runs something
    nodecode-guard.asd
    cell.lisp                  exports START-CELL
  nodecode-channel-kit/        a peripheral with vendored dependencies
    nodecode-channel-kit.asd
    vendor/                    ql:bundle-systems output; bundle.lisp loads first
```

Three things you touch: the binary with its source, this folder, git.

## Two shapes, one convention

**A layer** is the organism's own accumulated definitions. You do not write
it; evaluating a defining form through `eval` writes it. Every `defun`,
`defparameter`, `defmethod`, `defclass`, `define-*` that evaluates
successfully lands as one file, `(in-package :nodecode.evolved)` at the
top so the bytes mean what they meant at the eval, and is committed to the
folder's git repository with the session id in the message. Its `.asd`
names the `layer` system class, which makes it load **from source** at every
boot — ASDF's fasl cache is keyed on the SBCL version, not on this image, so
a layer compiled against last release's macros would keep stale expansions
across an upgrade. A layer has no `START-CELL`: it defines, it does not run.

**A knowledge cell** is a layer whose `.asd` says so — `:properties
((:knowledge . t))` and no component — and holds what the organism keeps and
does not run: one `define-memory` or prose `define-skill` a file, written at
the eval seam like any other definition. The loader records it as kind
`:knowledge` and never loads it; the index (the `index` harness section,
`(index)`, `/index`) reads its files as data, `(help 'name)` reads one. The
organism's own is `<identity>-memory/`; a form's `:cell "topic"` files it in
a cell of that one subject, a folder that can be pushed and shared like any
other.

**A peripheral** is anything else — a channel host, a browser bridge, a
guard, a notifier. Bigger, dependency-heavy, rarely changing, compiled once:
a shipped one at the release, into `src/fasl/` beside the shipped folders, so
a launch only loads it; one of your own, or one from the hub, into
`~/.cache/nodecode/peripherals/<build stamp>/` at its first load under each
build, so no build runs the macro expansions another compiled. A peripheral
that runs threads exports one function:

```lisp
(defun start-cell (config)   ; the whole parsed ~/.nodecode/config.jsonc
  ...                         ; start threads, register hooks, take leases
  (lambda () ...))            ; the stop thunk, or NIL
```

That is the contract; you rarely write it out. **Declaring a cell**
below is the normal path — one `nle:define-cell` form declares the config
section, the typed settings, the tools, the slash command, the manual
topic and the hooks, and derives this function and its exact inverse from
them.

The gateway calls it after recovery, with the whole parsed config so the
core parses once and each cell picks its own section, and funcalls the
stop thunk first on the way down. A peripheral reaches the organism through
the in-process seams — `(nle:hook ...)`, `(nle:submit ...)`, the `nlk`
session API — never by dialling the gateway it runs beside, so it needs
neither the port nor the operator token. A folder with an `.asd` and no
`START-CELL` is a library: it loads, nothing starts.

A `START-CELL` that cannot honour its section **signals** — the typed
accessors do (`nlk:config-refusal`) — and that refusal is a fact, not a log
line: it lands on the folder's record (`REFUSED` on the `/cells` line),
on every attached shell as a toast, and in the notices block at the
tail of the model's prompt every round, under the folder's name. Siblings still
start; the gateway always boots. A present section is an enabled section;
an explicit `"enabled": false` vetoes it. A folder may carry
`config.example.jsonc` — its section, commented, secrets as references —
which is documentation, not config: the folder gets pushed and holds no
secret.

## Speaking, and trying again

```lisp
(nle:notice "chrome: extension never polled" :level :warning :key "nodecode-chrome")
(nle:notice nil :key "nodecode-chrome")     ; clear it
(restart-cells)                            ; every peripheral, from the config as it is now
(restart-cells "nodecode-channel-kit")     ; one folder
```

`nle:notice` is the one way a part says something to the operator: every
attached shell puts the text up as a toast, top right: `:level :info` and
`:warning` for five seconds, `:error` until it is closed. With a `:key` the
notice also **stands**: it replaces the board's entry under that key (and
the toast up under it), so a shell attaching later still receives it (right
after its first replay, toasted once per shell for what it says) and the model reads it in the
notices block at the tail of its prompt until a later notice under the key
replaces it or a `NIL` text clears it. Without a key it is said once. Where no shell is
attached and the process has none of its own — a headless `serve` — the
text goes to the serve log. It rides the `:frame` point like every live
frame, so an observer sees it; it is never durable.

`restart-cells` stops the started peripherals newest-first, re-reads
`~/.nodecode/config.jsonc`, and calls `START-CELL` again: the operator's
"I fixed it, try again" from the TUI or from a channel, without a relaunch.
It answers the report — every record's line with its start state, then the
board — so the eval snippet shows what happened. `(config-get '("channels"
"discord"))` reads the shared config at a path as a keyword plist (a
member named like a secret reads as `[set]`); `(config-set '("channels"
"discord") '(:bot_token_env "DISCORD_BOT_TOKEN"))` writes one — an object
value merges its members in, anything else replaces the member; comments
and every other byte are kept, the write is atomic — and answers the diff
it made. Together with the `cells` section, which lists every folder's
line and the board and names these forms, that is the onboarding surface
once a model is there to talk to: the refusal is the wizard, and the
conversation is the rest. Before one is — a new box — the setup wizard
below runs first.

## Declaring a section

A folder says what its section of the config is made of, once, as data:

```lisp
(nlk:define-section ("channels" "discord")
  (:guide "where to go: what to do; next step; ...") ; one step per ;, read by both
  (:check #'probe-channel)                         ; (table) => text, once complete
  (:one-of "bot_token_env" "bot_token_file")       ; exactly one set
  (:any-of "allowed_channels" "allowed_users")     ; at least one non-empty
  ("bot_token_env"    :env  :doc "environment variable holding the bot token")
  ("bot_token_file"   :path :doc "file holding the bot token")
  ("allowed_channels" :list :doc "channel ids" :choices #'probe-channel-choices)
  ("require_mention"  :boolean :default t :doc "answer when @mentioned ...")
  ("free_response_channels" :list :doc "channel ids that answer every message")
  ("require_mention_channels" :list :doc "channel ids that require a mention")
  ("ignored_channels" :list :doc "channel ids that are never read")
  ("mention_patterns" :list :doc "wake words for mention-gated channels"))
```

Field types: `:string`, `:secret` (a value written inline), `:env` (the
name of an environment variable holding a secret), `:path`, `:directory`
(`~/` expanded; a relative default is under the home), `:boolean`,
`:integer`, `:list` (an array of strings), `:choice` with `:options`. A
field may be `:required`; an `:integer` may carry `:min`; a `:list` may
carry `:choices`, a function of the section's table answering
`((id . label) ...)` — how ids get picked by name instead of typed.
`:owner` defaults to the declaring package's name, downcased, which by
convention is the folder's name.

Five readers derive from the one declaration and cannot disagree:
`(nlk:section-problems section table)` is what the folder refuses on at
start, in words that name the key; the `/settings` panel (and the page's
Settings tab) is the fields in order; the model's setup primer is `(nlk:section-text section)`; the
cells report's line for an unconfigured folder is
`(nlk:section-summary section table)`; and the typed, immutable values the
folder actually runs on are `(nlk:section-settings section table)` — the
settings struct and its parser that every folder used to write out beside
the declaration. `(nlk:section-needs-setup-p section config)` is the one
gate: a vetoed section (`"enabled": false`) needs nothing, a section with
no constraints is complete however absent.

## Declaring a boundary

A section is the config crossing into the folder. The other crossing is a
payload: a model's tool call, a command's params, an HTTP control request —
a foreign JSON object becoming a value the folder runs on. That one is
declared too, and by the same rule: once.

```lisp
(nlk:define-contract issue-report
  (:doc "Report a tool that misbehaved: it timed out, refused something it
should have done, answered the wrong shape, or lacked something you needed.")
  ("tool"    :text :required t :doc "the tool that misbehaved, as you called it")
  ("symptom" :text :required t :options +symptoms+ :doc "what went wrong")
  ("note"    :text :default "" :doc "one sentence, at most 280 characters"))
```

and the tool registers against it instead of against a schema written by
hand:

```lisp
(:tool "report_issue" (nlk:contract 'issue-report) 'report-issue)

(defun report-issue (report)
  (keep-note (issue-report-tool report)
             (issue-report-symptom report)
             (issue-report-note report))
  "noted")
```

**A boundary contract is not a Lisp type.** Every value in this image is
what it always was, and nothing above or below the boundary is checked.
What the declaration types is the one value crossing it, at the one moment
it crosses.

**What it derives.** The record `ISSUE-REPORT` with one read-only slot per
member and a reader each, `MAKE-ISSUE-REPORT`, `ISSUE-REPORT-P`,
`DECODE-ISSUE-REPORT`, `ENCODE-ISSUE-REPORT` — all exported where the
declaration stands, as `define-record` and `define-error` export theirs —
and the JSON-Schema projection `(nlk:contract-schema (nlk:contract 'name))`
that a catalog advertises. The schema the provider reads and the record the
body reads come off one `FIELDS` list, so a tool cannot advertise one shape
and dispatch another: the body never sees the hash table, and a member the
declaration does not carry has no reader.

**Member types** are `json-value`'s own, the eight `+JSON-TYPES+`: `:any`,
`:string`, `:text` (non-empty), `:integer`, `:number`, `:boolean`,
`:object`, `:array`. A member may be `:required`, carry a `:default` form
(refused beside `:required`), carry `:options` (a fixed set, advertised as
the schema's `enum`), and nest — `:of NAME` on an `:object` materializes
that contract's record, on an `:array` its elements, or `:of :string` for
an array of scalars. `:slot` names the record's slot where the derived one
(`max_results` → `MAX-RESULTS`) will not do, which is also the escape when
the derived name is one `COMMON-LISP` owns (`count`, `method`, `type`).

**What validates.** JSON null reads as **absent**, so null and a missing
key take the same default and refuse the same way. JSON false is
**present**, and materializes `NIL` on a `:boolean` — which is why an
absent boolean and a false one are the same value, and why the encoder
always writes a boolean. A member of the wrong type is **refused, never
coerced**: the string `"3"` is not an integer here. Members the
declaration does not carry are ignored, unless it says `(:unknown :refuse)`.

**Refusal** is one condition, `nlk:contract-refusal`, carrying the
contract, the member's wire path (`"session_id"`,
`"providers[0].name"`), what the declaration wanted and what arrived —
readable off the condition, not only in its text. A gateway route answers
it `400 contract_refused`; a tool call answers it as the preflight refusal
the model already repairs from, before any hook or body runs.

**What is statically checked, and what is not.** A malformed *declaration*
fails at macroexpansion: a type outside `+JSON-TYPES+`, a wire key that is
not a literal string, an unknown option, `:required` with `:default`, a
duplicate key, `:of` where nothing nests. A construction site that leaves
out a required member is a compile-time warning, because the slot has no
initform. Everything else is checked when a payload arrives. Nothing proves
a body correct, and nothing types the image.

**Live-only.** A contract carries no version and no hash: nothing durable
is written from one (the event log's own contract hash stays
`store/events.lisp`'s). Redefining one replaces the declaration in place,
and every boundary that resolves it by name follows without a restart.

## Declaring a cell

The section is one of the things a peripheral declares; the rest — what it
contributes, and how every bit of it comes back off — is the same
declaration. One form is the normal authoring path:

```lisp
(nlk:define-peripheral weather :not-running t)       ; the condition, FAIL, *WEATHER*,
                                                     ; RUNNING-SETTINGS and its refusal

(nle:define-cell weather
  (:section ("weather")
    (:guide "units is c or f; a key is optional — without one the public floor answers")
    ("units"   :choice :options '("c" "f") :default "c" :doc "degrees")
    ("cities"  :list :doc "what the primer names")
    ("timeout_seconds" :integer :default 10 :min 1 :doc "one lookup's ceiling"))
  (:tool "weather" +schema+ 'look-up)
  (:help :weather "weather: today's forecast for a city - (weather:today \"Oslo\")" +manual+)
  (:hook :frame #'observe)
  (:command "weather" 'run-slash
            :description "Today, where you are" :argument-hint "CITY")
  (:start (lambda ()
            (let ((worker (nlk:worker-start "weather" #'refresh :wake 900)))
              (nle:on-stop (lambda () (nlk:worker-stop worker)))))))
```

That is the whole peripheral. It defines the `START-CELL` the loader looks
for and the gateway calls, a `STOP-CELL` that is its exact inverse, and
`SETTING`:

```lisp
(setting :units)            ; "c" — typed and defaulted as the declaration says
(setting :timeout-seconds)  ; 10 — an underscore in the config is a dash here
```

The values are materialized once, at start, out of the section's own
declaration (`nlk:section-settings`, the fifth reader off it): there is no
second struct and no second parser to keep in step. A member of the wrong
shape, a `:choice` outside its options or an `:integer` under its `:min`
**refuses** — `NLK:CONFIG-REFUSAL`, which the gateway renders as the
folder's `REFUSED` line and one standing notice. A member that is merely
absent does not: `:required` is what `/settings` reports as still needed, and a folder whose
section is unanswered still runs whatever half of itself needs no answer.

Where the shape of a section outruns what the declaration carries — a
nested object, a default that depends on another member, a switch read off
the environment — `(:settings FUNCTION)` takes `(VALUES TABLE)`, the values
the declaration derived and the section's own object, and answers the plist
the cell runs on. `websearch` is the worked example: its `provider` and
`max_results` are declared, its `providers.<name>.api_key` and `fetch`
object are read there.

The clauses, in any order; contributions install in the order written and
come off in the reverse:

| Clause | Composes | Its inverse |
| --- | --- | --- |
| `(:section (PATH...) CLAUSE...)` | `nlk:define-section`, verbatim | — |
| `(:settings FUNCTION)` | `(lambda (values table) ...)` | — |
| `(:tool NAME SCHEMA FN)` | `nle:register-tool` | `nle:unregister-tool` |
| `(:command NAME RUN &key description argument-hint label catches session trigger complete)` | `nle:register-command`: `/NAME` in the one slash catalog, wherever it is typed; RUN takes `(args session-id)` and answers `(values TEXT DIALOG)`; LABEL and CATCHES name its failure line; SESSION NIL marks a command that acts on no session; COMPLETE takes `(text session-id)`, the argument typed so far, and answers `(:name :value)` plists, the choices a menu offers while it is typed | `nle:unregister-commands` |
| `(:help TOPIC SUMMARY TEXT)` | `nle:set-help-topic`: `(help TOPIC)` answers TEXT, every request's `help` section carries SUMMARY, one line | `(nle:set-help-topic TOPIC nil)` |
| `(:hook NAME FN)` | `nle:hook` on a point or a function name | `nle:unhook` |
| `(:route PATH FN &key public)` | `nle:route`: PATH answered on the gateway's own port, to the operator's token, to anyone under PUBLIC | `(nle:route PATH nil)` |
| `(:start FUNCTION)` | `(lambda () ...)`, with the settings already set | whatever it gives `nle:on-stop` |

Everything is filed under one key, the declaring package's name downcased,
which by the folder convention is the folder's name — `nodecode-weather`
registers its hooks and its commands under `"nodecode-weather"`, so two
cells advising one function both run and `STOP-CELL` removes exactly
one.

Three properties follow from the declaration rather than from the author
remembering them. **Stop is complete**: nothing is undone by a hand-written
list that can fall behind the install. **Stop is idempotent**: the inverses
empty as they run, so stopping twice, or stopping after a failed start,
does nothing the second time. **A start that fails halfway leaves
nothing**: the registrations made before the failure are undone before the
condition goes up, so the folder's next try finds nothing of the last one,
and its siblings still start. Starting twice stops the first start before
the second begins, so a second start leaves one of everything.

## Under it, and beside it

`define-cell` composes primitives that stay callable on their own. A
cell the declaration does not fit — a registration it must hold a handle
to, a contribution that starts with a lane rather than the cell — writes `START-CELL` by
hand and reaches for the same pieces:

```lisp
(nlk:define-section ("mine") ...)             ; config alone, no cell
(nle:register-tool NAME SCHEMA FN)            ; (nle:unregister-tool NAME)
(nle:register-command OWNER CMD RUN ...)      ; (nle:unregister-commands OWNER)
(nle:hook NAME KEY FN)                        ; (nle:unhook NAME KEY)
(nle:route PATH FN &key public)               ; (nle:route PATH nil)
(nlk:worker-start NAME STEP :wake N)          ; (nlk:worker-stop W), (nlk:worker-poke W)
```

`nle:route` answers PATH on the gateway's own port — a page, a webhook, a
sign-in callback — and a PATH ending in `/` answers everything under it. FN
takes the request's clack environment and returns a clack response or a JSON
value. A request without the operator token is refused before FN runs,
unless `:public t` -- which is for what cannot carry the token (a browser
page, a webhook, a sign-in callback), and whose FN owns its own trust; the
gateway has already refused any Host that is not loopback.

A boundary the declaration does not fit keeps the readers under it.
`register-tool` still takes the plist `(:description TEXT :parameters
OBJECT)` written by hand, which is what the `eval` snippet uses for its open
`strings` object — a shape no declaration carries. Under that,
`nlk:json-value` and `nlk:with-json` are the lenient readers for data a
peer produced (a wrong type reads `NIL`), and `nlk:json-object` builds one.
A contract is the strict dialect over the same types, not a replacement for
them, and `(nlk:contract-decode CONTRACT OBJECT)` /
`(nlk:contract-encode CONTRACT VALUE)` are callable directly when the
generated `DECODE-`/`ENCODE-` names are not what a caller has in hand.

The cleanup half is a primitive of its own, and nothing in it knows what a
cell is:

```lisp
(defun start-cell (config)
  (declare (ignore config))
  (nle:starting                                ; => the thunk that closes it
    (take-the-lease)
    (nle:on-stop #'release-the-lease)          ; one inverse, recorded here
    (let ((worker (nlk:worker-start "mine" #'step)))
      (nle:on-stop (lambda () (nlk:worker-stop worker))))))
```

`nle:starting` runs its body as one registration scope and answers the
thunk that undoes it, newest first; `nle:on-stop` records one inverse in
the innermost scope, and refuses outside one — an inverse nothing would run
is the drift the scope exists to remove. A body that leaves non-locally
takes its own prefix with it. It is what `define-cell` expands into, and
it is what a `:start` clause runs inside, which is why a worker started
there needs no `*worker*` anybody has to remember to clear.

## Turning a cell on, and setting it up

`nodecode setup`, `/setup`, and — unasked — the first frame of a shell
with no provider or no saved default model offer to import from other
coding agents found on the machine, then walk the provider, the model and
its reasoning effort, and nothing else: no cell is installed or asked about. A cell is turned on in `/cells`, a
sheet over the organism's listing: Home holds every folder this home holds
and every shipped one it does not, those that need a look first; Hub holds
the hub's entries once the organism has read its index; the chosen one is
laid open beside the list, and Space turns it on or off. Every switch — `/cells`,
the web page's Cells tab, `nodecode add` — is one request to the organism,
`POST /api/gateway/cells`, which installs the folder with the offered ones
it stands on, loads and starts it, and tells every attached shell to load it
too (`organism.cells`). `nodecode add` with no organism running installs
the folders itself, and they load at the next launch. What the folder's section declares is set in
`/settings` or on the page's Settings tab, both over `/api/gateway/config`,
whose write is refused when the section's own reader would refuse it; a
folder left unconfigured is the state the primer above teaches from, so
"set up discord" finishes it in the conversation.

No shipped folder is in any revision of this repository: `src/cells/bundle.json`
names a repository (`nodecode-ai/cells`), the commit it is read at, and every
folder the release builds in from it, each at the shipped path it takes here
(`guard` from `tools/nodecode-guard`, `channels/kit` from
`rooms/nodecode-channel-kit`, `template` from `template`, and so on).
`just cells-pull` (`src/tools/pull-cells.sh`) lays them down at that commit
before a build, a release, a pack or any cell lane, marks each with `.bundle`,
and does nothing while the marker names the pin; the paths are git-ignored, so
a shipped cell is edited in that repository (checked out at `~/nodecode-cells`;
`.github/test-cell.sh FOLDER` there runs its tests) and the pin moved here. A
hub index never lists a shipped name, so a shipped name always means the
shipped folder. A shipped cell is off until the operator turns it on, in
`/cells` or with `nodecode add`.

The binary ships its folders beside its source (`src/cells/`):
`(nlk:shipped-cells)` reads them as data — name, dependencies, and what
this image cannot find (such a folder is offered as unavailable, never
linked blind) — `(nlk:install-cell name)` puts one under
`~/.nodecode/cells/` (a symlink; a copy where the platform has none),
`(nlk:uninstall-cell name)` removes only what the installer put there,
and `(nlk:load-cell name ...)` runs the boot loader's per-folder load now
so the record lands on `(cells)` without a relaunch; `start-cells` then
starts it. The organism does all three when it turns a folder on; a shell
never installs into its own process. At boot, a home is told through a
standing info notice which shipped folders it does not have, and is given
`/cells` as the explicit choice. Each folder is named once per home (kept in
`~/.nodecode/cells-named`), so the notice goes once the folders are
installed or have been named. Cells are never
silently linked, because folder presence is consent. An install follows the
binary that boots it: before the folder
loads, a link into another tree's shipped folder — the release installed
before this one, a checkout — is pointed at this build's, and a copy
another build made (its `.shipped` marker carries that build's stamp) is
made again, so rerunning the installer moves every installed cell with
the executable. An install of a folder this build no longer ships, out of
another release's tree, is taken out, and the boot line says so; one out of
a checkout stays, a newer build's. A fork of a shipped folder is a folder of its own: a link
to one kept at `…/cells/<name>/` reads as another tree's and follows the
build. A secret pasted into `/settings` or the Settings tab rather than
named is kept in a 0600 file under `~/.nodecode/secrets/` and the section
names the file.

## The hub

Folders beyond the release are listed in two indexes, in one format.
Nodecode's own cells are in `index.json` in the repository
`nodecode-ai/cells`, and arrive there by pull request; `src/tools/publish-hub.sh`
puts that file, at a commit of the repository, where `*hub-index-url*`
reads it: `https://nodecode.ai/dl/cells/index.json`
(`$NODECODE_HUB_INDEX` names another). The ones other people publish are submitted at nodecode.ai: the
site pins the repository's commit, reads its `.asd`, and a person at
Nodecode reads the code at that commit before it is listed in the community
index the site serves at `https://nodecode.ai/cells/community.json`;
`*community-index-url*` names it (`$NODECODE_COMMUNITY_INDEX` another). A
name Nodecode's index lists is never the community's, and `nodecode add`
says `index   community` for an entry from there. An entry is a folder at
a pinned commit:

```json
{ "format": 1,
  "cells": [
    { "name": "nodecode-weather",
      "git": "https://github.com/someone/nodecode-weather",
      "commit": "3f9a1c7e2b8d4f60a5e19c0b7d2e4a6f8c1b3d5e",
      "description": "Current weather as a tool the model can call.",
      "license": "MIT",
      "depends_on": ["nodecode"],
      "platforms": ["linux", "macos", "windows"] } ] }
```

`name` is the primary system's, `git` is https only, `commit` is a full id,
`description` and `depends_on` are what the folder's `.asd` says, and
`platforms`, when present, narrows where the entry is offered. `path`, when
present, names the folder inside the repository the entry is, so one
repository can carry several: `"path": "nodecode-weather"` installs that
folder alone, with the path on the `.hub` marker's third line. An entry
stands on the core, the shipped folders and what it vendors, never on
another entry, and a name a release ships stays the shipped folder's.
Nodecode's index checks each entry by installing and loading it against a
release before a person reads the code and it merges.

`(nlk:hub-cells)` reads both indexes as offers beside `(nlk:shipped-cells)`,
refusing an index whole when an entry breaks the format; one index that
cannot be read leaves the other's entries on offer. Each index read is kept
under the home's `hub/`, and one that cannot be read is offered as last
read, saying why. `/cells` lists
them on its Hub tab and asks once more, in place, before installing one:
its repository and the commit read stand in view, and a second Space
installs it, because a hub folder runs inside the organism with its reach. `(nlk:install-cell name :offers ...)` clones the entry
beside the cell folder, proves the clone — `HEAD` is the pin, the primary
system is the entry's name with the index's description and `depends_on`,
no symbolic link, every dependency found — and
renames it in with a `.hub` marker (URL, commit) that `uninstall-cell`
honours. Nothing moves a hub folder afterwards: a release leaves it at its
commit.

## What a folder may do

- **Define** new names in `nodecode.evolved` or its own package.
- **Override** a shipped EVOLVED definition by redefining it. The loader
  snapshots EVOLVED before each folder loads, so an override is reported, and
  a layer file that overrides records the shipped text's hash in its header
  — after an upgrade the boot line lists every override whose shipped
  definition moved underneath it.
- **Advise** a shipped function instead of overriding it — preferred:

  ```lisp
  (hook 'compose-frame "mine"
        (lambda (next model width)
          (let ((frame (funcall next model width)))
            ...)))
  ```

  `fn` receives `(next &rest args)`; call `next` to run the rest of the chain
  and the function itself, or do not, to replace it. Advice survives the
  upstream body being rewritten as long as the signature holds, and two
  folders advising one function under different keys both run, in load
  order. `(unhook 'compose-frame "mine")` removes yours. The same primitive
  serves the named points, `:tool` (one tool dispatch), `:frame` (one
  live frame leaving the gateway), `:credential` and `:round`, whose hooks
  are `(lambda (op next))`. A
  `:tool` hook that refuses a call answers `(nle:failure "ERROR: ...")`, as
  a tool that knows its call failed does: the result fact is recorded
  failed, and every transcript draws the call failed from it.

  A hook that needs to know which turn it runs in reads `(nle:turn)`: a
  plist of the running turn — `:session-id`, `:turn-id`, `:parent` — and
  what it has spent so far — `:seconds`, `:rounds`, `:calls`, the token
  counts, `:context-tokens` the newest round filled against
  `:context-window`, `:quiet-seconds` since the operator last read a
  sentence — with
  the `:budget` it runs under, or NIL outside a turn. Every tool result
  shows the model the same spend. A folder that owns a session's turns gives
  them a budget by advising `nle:turn-budget`: return `(:seconds N :calls
  N :tokens N)`, any of the keys, or NIL for none, for your own sessions,
  and call `next` for every other. Unadvised, a turn runs uncapped:
  `nle::*turn-budget*` is NIL, and a layer may set it. What a budget means
  is the core's: a call past it is refused, so the model answers with what
  is done and what is left; a round that calls tools after that refusal
  ends the turn; and no wait outlasts it. `:tokens` are billable tokens
  (input, output, reasoning; cache reads free) of the turn's rounds.

  Through `eval`, write it as the defining form —
  `(define-hook compose-frame "mine" (next model width) ...)` — and the
  scribe keeps it as a layer file (`compose-frame-mine.lisp`, one per name
  and key) that re-registers at every boot; a bare `(hook ...)` call runs
  once and is gone at restart. A hook's name is what it advises, not what
  it defines: advising a kernel function is recorded, and
  `(definition-text 'name)` keeps answering the body, never the advice.
- **Add methods** to shipped generics. Reported as added; only a restart
  removes one.
- **Not** touch `nodecode.kernel`. The package is locked; a folder that
  redefines a kernel name fails its load and the boot line says so.

## Order, failure, report

Every folder is registered with ASDF before any loads, so an `.asd` may
`:depends-on` a sibling folder's system; dependencies load first, layers
after every peripheral (a layer's advice on a function a peripheral provides
must find it fbound and its package readable), everything else
alphabetically. Each folder loads under its own error boundary: a
folder that fails to compile is one record with the condition text, its
siblings load, the boot always completes. A layer's boundary is the file —
one definition per file — so a file that fails is named on the record
(`FAILED _dm-rev_.lisp: the discord channel is not running`, with SBCL's
form-at-line context beneath) and the files after it still load; the
landing screen's `failed` row names the same file.

```lisp
(cells)                 ; every folder: state, kind, definitions, overrides,
                         ; collisions, methods added, skipped files, failures,
                         ; started-p and refusal
(describe-cells)        ; the same as the boot line, plus `started` / `REFUSED`
```

`/cells` in the TUI lays the same facts open one cell at a time, read from
the organism's listing (`GET /api/gateway/cells`), every failure and skipped
file whole; headless, it prints these lines, then every standing notice.
A **collision** is two folders defining one name; the later loader wins,
deterministically, and the report names both.

## Transfer

Share is `git push` of the folder to any remote. Install is `git clone`
into `~/.nodecode/cells/`, or a sentence to the agent. Upgrade is a new
binary with its source; the folder loads over it; the boot line lists what
no longer compiles, what collides, and every layer file `SKIPPED` because it
would not run against the new image: an override whose shipped definition
changed since it was taken, or a definition naming a function or variable
that neither the image nor the layer's later files define — what its
compile found missing. A file naming what a later file defines loads behind
that file, once it has loaded, and is skipped when it never does. A skipped
file stays in the folder and does not
load; the shipped definition stands until the definition is recorded again
over the new source, or forgotten. The first boot that does not load a file
of the layer hands every such file, with why, to the model as a turn of its
own in the session `layer`, to record again or forget; a file is handed
again only after it has loaded and broken anew. A function only an optional cell
provides is called as `(funcall 'pkg::name ...)`, which is never judged
missing. Forgetting one definition is the act
in reverse, evaluated like the definition was: `(fmakunbound 'name)`,
`(makunbound '*name*)`, `(unhook 'name "key")`, `(remove-method ...)` or
`(unintern 'name)` removes its file from the layer and commits `forgot
name`; a file deleted by hand is gone at the next boot and pruned from the
`.asd` at the next write.

Undo is a generation. Once a fresh child of the organism has loaded the
folder and every override still takes its callers' calls, the launch-image
keeper seals the organism as it stands — the binary's version and every
folder at its revision — as a numbered manifest under
`~/.nodecode/generations/`, counted from 1 and never pruned; a folder that
does not prove is refused by name on the board, and no image is built for
it. `(generations)` lists them, `/generations` picks one, and `(rollback N)`
or `nodecode rollback N` puts every folder back to what N recorded: the
layer by one commit forward whose tree is N's (its history kept, so forward
is the same verb), a hub cell cloned again at N's pin; a shipped folder
stays with the release the binary runs, since only `nodecode update` moves
that side; then the organism exits once no turn is in flight and the next launch
runs N. The rule for a folder of your own: its git `HEAD` is its revision,
and a folder with no repository is known by its content and never restored
— the restore refuses by name before anything moves. A folder whose load
kills the process has a floor: the launch after it runs generation 0, the
binary alone (`--pristine` by hand), keeps nothing, and hands the `layer`
session the list.

Config stays outside every folder on purpose: the folder gets pushed, and
`~/.nodecode/config.jsonc` holds tokens.

A profile is another home — `~/.nodecode/profiles/<name>/`, run with
`nodecode -p NAME` — with a `cells/` of its own: its own layer, its own
installed peripherals. `profile create NAME` carries the settings and the
installed peripherals over a fresh store and layer (`--blank` for an empty
home); `--clone-all` clones the layer with its history. A cell reads its home through `(nlk:home "...")`, never a
literal `~/.nodecode/`, so it follows the profile without knowing.

## Vendoring dependencies

A peripheral that needs systems the image does not already carry ships them:

```lisp
(ql:bundle-systems '("some-dep" "other-dep")
                   :to #p"~/.nodecode/cells/my-peripheral/vendor/")
```

The loader loads `vendor/bundle.lisp` before the folder's systems, so the
operator's machine needs no Quicklisp. Check the `vendor/` tree in.

## Writing a peripheral

Copy `template/` (nodecode-ai/cells; `src/cells/template/` once pulled) to `~/.nodecode/cells/<name>/`, rename the
`.asd` and the system, and write the one `nle:define-cell` form
(**Declaring a cell**, above) over a `nlk:define-peripheral`. The
template carries one hook, a `config.example.jsonc`, and the `.asd` shape. The shipped peripherals
(`tools/` and `rooms/` of nodecode-ai/cells, pulled under `src/cells/`) are the
worked examples — in development they are installed by symlinking their pulled
directories into the folder:

```sh
ln -s ~/nodecode/nodecode/src/cells/guard ~/.nodecode/cells/nodecode-guard
```

Data over code: where the core's behaviour is really configuration —
prompts, keymaps, themes, text rules, model defaults — it is a
`defparameter`, because a layer rebinding data has zero fragility. Prefer
that to overriding a function.
