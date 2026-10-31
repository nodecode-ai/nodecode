# Security

## Reporting a vulnerability

Report it privately, never in a public issue: open
[a private advisory](https://github.com/nodecode-ai/nodecode/security/advisories/new)
(the repository's Security tab, then "Report a vulnerability"). Only the
maintainers see it, and the fix is discussed there until it ships.

A report that names the commit or release you read, the file and line, and
what an attacker gains from it is the fastest to act on.

A vulnerability in a cell (the tools, rooms and providers under
[nodecode-ai/cells](https://github.com/nodecode-ai/cells)) goes to
[cells' advisories](https://github.com/nodecode-ai/cells/security/advisories/new).

## What is in scope

This repository: the Nodecode runtime, its shell, gateway and installer.

Some limits are by design and documented in
[Safety and trust](https://nodecode.ai/docs/security): Nodecode runs
with your user's rights and is not a sandbox. A report showing that
Nodecode crosses one of those documented lines is still welcome.

## How a fix reaches you

A fix ships in the next release; `nodecode update` takes it.
