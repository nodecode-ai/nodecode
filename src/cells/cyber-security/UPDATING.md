# Updating the feed

The nodecode team's organism re-scans for new breaches every 6 hours and runs this
procedure (its cron job `nodecode-cyber-security`); a person can run it by hand the
same way. Every installed copy reads `advisories.json` from `main` within
`refresh_hours` of a push, so the push is the update: no release, no reinstall.

## 1. A clean copy

```sh
FEED=~/.nodecode/cyber-security-feed
[ -d "$FEED/.git" ] || git clone -q git@github.com:nodecode-ai/cells.git "$FEED"   # ssh: the push needs write access
cd "$FEED" && git fetch -q origin && git checkout -q -B main origin/main
```

This clone is the job's own: never edit the feed in a shared checkout.

## 2. Triage first: the scan sorts, you investigate

```lisp
(security:scan-report :seen "~/.nodecode/cyber-security-seen.json")
```

It collects everything published since the feed's `updated` time (minus a day of overlap)
with no model: CISA's exploited list, GitHub's reviewed critical/high and malware
advisories, and news through DeepSeek's own web search (its native `web_search` tool on
https://api.deepseek.com, on this image's `deepseek` provider key), or Exa's when there is no
DeepSeek key. The report names which search answered. It drops what the feed
already carries and what an earlier scan already sorted (the `:seen` ledger), and sorts
the rest with **Decide v1, nodecode's decider model** (`foresight-decide-v1` on provider
`nodecode`, https://api.nodecode.ai/v1, the key `nodecode login` saved). Never sort these
items with another model.

- `decider ... answered`, `keep: 0`, `expiring: none`: nothing to do. Answer exactly
  `SILENT` (section 7) and stop; do not search further.
- Items kept: investigate those only. Open each link, find the vendor's own advisory, and
  section 3 decides whether it goes in: the decider's call is a lead, never a verdict.
  `[exploited]` items are always read; `[popular-malware]` means the poisoned package has
  real downloads.
- `expiring` ids: drop them (section 4), unless attacks on one are still being reported.
- `decider UNAVAILABLE` (no nodecode key, the provider down): run the full scan below by
  hand over the same window, and the answer's first line says
  `decider unavailable: full scan`.
- A source `FAILED`: run that one source by hand.

### The full scan: only when the decider is unavailable

- **CISA KEV** (exploited, authoritative):
  `curl -s https://www.cisa.gov/sites/default/files/feeds/known_exploited_vulnerabilities.json`,
  entries whose `dateAdded` falls in the window.
- **GitHub reviewed advisories** (package ecosystems, with patched versions):
  `gh api '/advisories?type=reviewed&severity=critical&published=>=YYYY-MM-DD&per_page=100'`,
  the same with `severity=high`, and `type=malware` for poisoned npm/PyPI packages.
- **News and vendor posts**: `mcp:exa/web-search-advanced-exa` with `:category "news"`
  and `:start-published-date` at the window's start, for: actively exploited vulnerability;
  zero-day patch; critical security update web server / CMS / plugin; data breach API keys
  or environment variables exposed; supply-chain attack npm PyPI malicious package; hosting
  or SaaS platform breach. Researchers and vendors often announce on X first: cite the X
  post when a vendor or researcher made the announcement there.
- Never search X through a person's own logged-in account from this job (automation
  puts that account at risk); X posts arrive through the news and vendor pages above.

## 3. What goes in

An advisory belongs when someone running a website, a server, a dev machine or a hosted
service can act on it:

- web stacks and what runs beside them: PHP, Node, Python and Java app servers, CMSs and
  their plugins, web servers and proxies, databases, CI/CD and self-hosted apps;
- supply chain: poisoned npm, PyPI, Composer, Docker or GitHub releases;
- platform breaches that leak customers' secrets (rotate keys), TLS and CA failures,
  Linux kernel and virtualization bugs on hosts.

Critical or high severity; anything exploited in the wild always. Leave out phones,
desktop apps and anything a site operator cannot act on.

Every fact comes from the vendor's own advisory (`mcp:exa/web-fetch-exa` it), never
from a guess: fixed versions, affected range, stopgap. A news story is a lead, the vendor
page is the source. Add a machine `check` only when the version can be read reliably
(see the kinds in README.md); otherwise leave `checks` empty and the entry reads as READ.

## 4. Edit `tools/nodecode-cyber-security/advisories.json`

- New: an entry shaped like its neighbours (`id` a lowercase slug, `date` the
  disclosure day, `severity`, `exploited` true/false, `cves`, `summary`, `affected`,
  `fixed`, `checks`, `patch`, `mitigation`, `iocs`, `sources`), newest first.
- Changed: new fixed versions, exploitation confirmed, new signs of compromise: edit the
  entry in place and add the source.
- Dropped: an entry whose `date` is more than 14 days old, unless a source dated inside
  the 14 days says it is still being exploited (then keep it and add that source).
- Set the top-level `updated` to now (UTC, `2026-10-22T10:00:00Z`) and `window` to the
  14 days it covers. Write the file with a JSON tool, never by hand-splicing text.

## 5. The gate: nothing is pushed that fails it

```lisp
(security:feed-problems (uiop:read-file-string "~/.nodecode/cyber-security-feed/tools/nodecode-cyber-security/advisories.json"))
;; => NIL, or every reason it must not be published
```

```sh
cd "$FEED" && .github/test-cell.sh tools/nodecode-cyber-security cyber-feed
```

The test run needs a Nodecode checkout at the commit `.github/nodecode-rev` pins
(`~/nodecode/nodecode` or `NODECODE_SRC`). On a machine without one it stops at once with
`not a valid object name`: there a change to `advisories.json` alone is gated by
`feed-problems` alone (the shipped-feed test is that same check). Do not debug the test
run, and change nothing but `advisories.json` on such a machine.

## 6. Publish

```sh
git add tools/nodecode-cyber-security/advisories.json
git commit -q -m "cyber-security feed: +N new, ~M updated, -K dropped (ids)"
git push -q origin HEAD:main
```

A rejected push: `git pull --rebase -q origin main`, run the gate again, push again.
Then confirm what installed copies will read: the raw file at
`https://raw.githubusercontent.com/nodecode-ai/cells/main/tools/nodecode-cyber-security/advisories.json`
carries the new `updated` (GitHub's cache can lag up to 5 minutes), and `(security:refresh)`
here reports it.

## 7. The job's answer

- Nothing new, changed or dropped: exactly `SILENT`.
- A push: the first line `cyber-security feed: N new, M updated, K dropped (commit SHA)`,
  then one line per new or changed entry: severity, exploited or not, title, vendor link.
- The gate or the push failed: say so with the reasons. A failing feed is never pushed.
