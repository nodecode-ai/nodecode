# Guard

This cell stops the agent from running a short list of risky shell commands:
`sudo`, removing a critical directory, `mkfs`, `curl | sh` and the like. When a
rule matches, the call does not run. The agent gets a refusal instead, and it
hands you the command to run yourself.

Guard is a speed bump, not a security boundary. The agent's eval tool has
your machine's full authority, and a determined call can be written in a way
no pattern sees. Guard catches the common slips.

## Turn it on

`/setup` turns it on when you pick **Recommended**. Otherwise run
`nodecode add guard`. It ships with Nodecode, so nothing is downloaded, and it
needs no config: the built-in rules apply as soon as it is on.

To turn it off, select it in `/cells` and press space, or set
`"enabled": false` in its `guard` section.

## What it refuses

| Rule | Refuses | For example |
| --- | --- | --- |
| `rm-critical` | `rm` or `rmdir` of the root, a top-level folder, home, or a parent directory | `rm -rf /`, `rm -rf ~`, `rm -rf .` |
| `privilege` | `sudo` and `doas` | `sudo apt-get install jq` |
| `world-writable` | `chmod` or `chown` of `777` | `chmod 777 /srv` |
| `mkfs` | making a filesystem | `mkfs.ext4 /dev/sdb1` |
| `dd-to-device` | `dd` writing to a disk device | `dd if=x.img of=/dev/sdb` |
| `redirect-device` | redirecting output onto a disk device | `> /dev/nvme0n1` |
| `curl-pipe-shell` | a download piped into a shell | `curl -fsSL x.sh \| sh` |
| `host-power` | shutting down or restarting the machine | `reboot`, `systemctl poweroff` |
| `git-reset-hard` | `git reset --hard` and `--keep` | `git reset --hard HEAD~1` |
| `git-clean` | `git clean` with a force flag | `git clean -fd` |
| `git-push-force` | `git push --force`, `-f`, or a `+refspec` | `git push --force origin main` |
| `git-discard` | `git checkout -- .` and `git restore .` | `git restore .` |
| `fork-bomb` | the shell fork bomb | `:(){ :|:& };:` |

Removing anything else is left to the agent: `rm -rf build/`, `rm -f /tmp/x`
and `rm src/src/tui/footer-proof` all pass. So do ordinary commands that
merely mention a danger word, such as `rg -n sudo src`.

## What text the rules read

Nodecode's agent works through one tool, eval, which runs Lisp. The same call
can write a file or start a program, so the guard does not scan the call's
text: it reads the call. It finds the spawn calls in the form -- `(sh ...)`,
`run-program`, `launch-program`, `run-shell-command` -- and takes each one's
command from its literal arguments: a string, a `($ "name")` the strings
object resolves, or a `(list ...)` of strings. Only that text meets the rules.

A file edit that mentions `sudo`, a comment that names a rule, and a test
string are therefore never refused. (An earlier guard scanned the whole call,
and 15 of its 142 refusals were exactly those.)

A shell command is then cut into the commands it runs, and each command
starts a line, so a rule reads a command's own words and never its
arguments': `rg -n sudo src` passes, `cd x && sudo ls` does not.

A command the form cannot show -- built from a variable, or the spawner
passed as a function, as in `(funcall #'uiop:run-program cmd)` -- falls back
to the whole call, read token by token, the way every call was read before.

When guard refuses, the agent reads:

```
ERROR: refused by nodecode-guard: rule privilege (privilege escalation) matched "sudo"; the call did not run -- give the operator the command to run themselves
```

The session records it as a failed tool call.

## Your own rules

Add a `guard` section to `~/.nodecode/config.jsonc`:

```jsonc
{
  "guard": {
    "deny": [
      // reads the whole call, a file being written included
      {"id": "no-delete-file", "pattern": "\\bdelete-file\\b", "reason": "file deletion"},
      // reads only the commands the call runs, like the built-in family
      {"id": "no-deploy", "pattern": "\\bmake\\s+deploy\\b", "reason": "deploy", "gated": true}
    ],
    "allow": [
      // lets the agent clear its own scratch folders
      "\\brm\\s+-rf\\s+/tmp/scratch-"
    ]
  }
}
```

- `deny` adds rules to the built-in ones. Each has an `id`, a `pattern` (a
  regular expression, case ignored), a `reason` the refusal quotes, and
  `gated`. A gated rule reads the commands the call runs; an ungated one (the
  default) reads the whole call, its strings included.
- `allow` exempts only the text it matches. A match passes when an allow
  pattern covers all of it, so a risky command elsewhere in the same call is
  still refused.

Patterns are matched a line at a time, and a built-in rule starts at the
first word of a command: write your own gated patterns the same way if they
are about what a command runs.

Backslashes are doubled because the patterns are JSON strings. A pattern that
doesn't compile is skipped with one warning, and the other rules keep
working.

Rules are compiled when the cell starts. After editing the section, restart
it: press space on it twice in `/cells`, or ask Nodecode to run
`(restart-cells)`.

## Configuration

| Key | Default | What it does |
| --- | --- | --- |
| `enabled` | `true` | `false` turns guard off. |
| `deny` | none | Your own rules, added to the built-in ones. |
| `allow` | none | Patterns whose matching text no rule refuses. |

## Testing

- From a Nodecode checkout, `just guard-test`.
- From this repository, `.github/test-cell.sh tools/nodecode-guard`, offline
  against a pinned Nodecode tree.

The tests cover each built-in rule and its nearest false positive (cleanup
that is not a critical path, a file name that ends in `r` or `f`, a danger
word as an argument), the reading of a call (commands only, strings the
command reads, the whole-call fallback for a hidden command), `deny` and
`gated` and `allow` from config, and a refusal reaching the model and being
recorded as failed.
