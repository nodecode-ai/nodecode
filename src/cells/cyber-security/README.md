# nodecode-cyber-security

The last two weeks' breaches, each with the fix its vendor shipped, and a check that
says whether *this* machine or site still needs it.

```
nodecode add nc://nodecode-cyber-security
```

Then ask nodecode to "check my site for the latest breaches", or:

```lisp
(security:report :dir "/var/www/site" :host "example.com")  ; what to patch first
(security:show "php2shell")                                  ; the fix, the stopgap, the signs
```

`/security`, `/security check /var/www/site example.com`, `/security show ID` do the same
from any shell or chat room.

## What it checks

Each advisory in [`advisories.json`](advisories.json) carries its own checks:

| kind | reads |
| --- | --- |
| `version` | a program's version (`php`, `node`, GitLab's VERSION file, Plex's package), judged per release line |
| `npm` | a package in the site folder's `package-lock.json` / `node_modules` |
| `composer` | a package in the site folder's `composer.lock` |
| `wp-plugin` | a WordPress plugin's `Version:` header |
| `command` | a command's output (e.g. the site certificate's issuer) |
| `files` | known signs of compromise on disk |

Advisories with no machine check (a platform breach: rotate your keys) are listed as
`READ`. Distro builds that backport a fix under the old version number are recognised
from the package changelog where the advisory says how.

## Staying current

An installed cell is pinned to a commit and never moves, so the advisories are not
baked in: the cell reads `advisories.json` from this repository's `main` branch at
start and every `refresh_hours` (6 by default). A fix to the feed reaches every
installed copy when it is committed. The copy shipped in the folder answers when the
network does not, and the report's first line always says which copy answered and
how old it is.

```jsonc
"cyber-security": { "feed_url": "https://...", "refresh_hours": 6 }  // both optional
```

## Kept current every 6 hours

The nodecode team's organism re-scans for new breaches and fixes every 6 hours (CISA's
known-exploited list, GitHub's reviewed advisories, vendor and news posts), edits
`advisories.json`, and pushes it only when `security:feed-problems` and this cell's tests
pass. [`UPDATING.md`](UPDATING.md) is the procedure it follows.

Each scan starts cheap: `(security:scan-report)` collects the window's items with no
model and has **Decide v1**, nodecode's decider model (`foresight-decide-v1` on
https://api.nodecode.ai/v1), sort them; the agent then reads only what it kept. News comes
from DeepSeek's own web search (its native `web_search` tool), or Exa's when the machine
has no DeepSeek key. Items on
CISA's exploited list always go on, malware advisories go on when the package has real
downloads, and when the decider cannot answer the scan says so and the agent scans in
full.

## Patching

The cell finds and explains; the person decides. Upgrades need sudo and restart
services, so nodecode states each command and asks before running it. A sign of
compromise under an advisory means stop and say so first: patching does not remove a
backdoor.

MIT.
