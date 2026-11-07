# nodecode-qa

One page of counts a week, only if you say so. The whole page is
`report.schema.json` beside this file; `/qa show` prints the next one
exactly as it would go.

## What leaves

Once a week, when `qa.share` is `weekly`, one JSON page:

- the build and the platform (`0.0.1+abc123`, `x86_64-linux`)
- per provider and model: rounds, retries, failovers, tokens to the nearest
  thousand, the median time to first token
- turns completed, cancelled, failed by condition class
- per tool: calls, results that began with `ERROR:`, the median duration
- what the model reported through `report_issue`, by tool and symptom
- process-ending crashes by condition class

## What never leaves

No prompt, no answer, no path, no file, no hostname, no session id, and no
id that follows the install. A page carries a nonce minted for that page;
the collector keeps it two weeks to drop a redelivery and forgets it. Two
pages from one box cannot be tied together, by us or by anyone.

The counting starts when sharing is chosen, never before: the first page
covers the week after the choice.

## The model's notes

`report_issue` is a tool the model calls when another tool misbehaved: a
tool name, a symptom, one sentence. The note is kept on this machine,
`/qa notes` lists it, and the weekly page carries only its tool and
symptom as a count. The text leaves one way: `/qa push`, by hand,
after reading what `/qa notes` shows. A call to `(qa:push-notes)`
from a turn is refused.

## The switch

`/settings`, Diagnostics, asks it (Weekly report: weekly or never);
`(config-set '("qa") '(:share "never"))` answers it in a conversation,
and `qa.share` in `config.jsonc` by hand. `"enabled": false` vetoes the
cell, tool and all. `NODECODE_QA=0` in the environment turns sending off
whatever the config says, `push` included.

The answer is read when the cell starts. A cell already running when it
was answered keeps what it read at its start until
`(restart-cells "nodecode-qa")` or the next launch; until then `/qa`
says the answer waits, and `/qa send` refuses whichever way it went.

`/qa push` ships the model's notes, so it has something to send only
after the model filed one through `report_issue`; `/qa notes` lists
them.

```
/qa          one line: sharing or not, when the next page is due, notes kept
/qa show     the next page, as it would go
/qa send     send it now
/qa notes    the model's notes, newest first
/qa push     ship the notes' text, by hand
/qa clear    drop the notes
```

## How it works

The engine already journals every provider round, retry, failover, tool
result and turn end into `history.db`. The page is a fold over those facts
after a cursor, computed at the send; nothing is queued. One state row
holds the cursor (`qa/cursor`: log position, window start, last send,
the nonce while a send is open, the stdio log offset), one the notes
(`qa/notes`, 200 newest). A refused page keeps its nonce and is folded
again from the same cursor an hour later; on 2xx the cursor moves past it.

## The collector

`POST {url}/api/qa` with the page as the body, `content-type:
application/json`, the organism's user agent. Any 2xx is accepted; anything
else is retried under the same nonce. The collector upserts on the nonce
and keeps it fourteen days. `POST {url}/api/qa/notes` takes a by-hand
push: `{"report": "nodecode/notes/1", "version", "platform", "notes":
[{"tool", "symptom", "note", "provider"?, "model"?}]}`. `url` defaults to the
release host (`update.url`'s host) and is `qa.url` in the config.

The collector itself lives in the marketplace repo (`nodecode-marketplace`):
the two routes under `web/src/app/api/qa/`, the contract read strictly
in `web/src/lib/qa.ts`, the tables in `db/migrations/0029_telemetry.sql`
(they kept the cell's first name) and the desk board at `/admin/qa`.

## Tests

`just qa-test` — the page is counts only and carries no text from any
fact; a refused page keeps its nonce and cursor and the retry resends the same
page; the first look begins at the consent moment and sends nothing; never,
unanswered and the environment switch ship nothing; an answer written since
the start is named and ships nothing; the tool keeps a bounded
note and never fails; notes leave only by hand; the page's keys are the
schema's.
