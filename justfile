# Nodecode recipes. `just` lists them.
#
#   just build      compile the organism under src/
#   just test       run the core test suite
#   just lint       check Lisp structure: parens, layout, test prose
#   just check      lint, loc, build, test -- cheapest first
#   just release    dump ./nodecode and warm its launch image
#   just <cell>-build | <cell>-test | <cell>-check    one lane per shipped cell
#
# Needs SBCL, git, python3 and Quicklisp (README.md, "Build from source").
# Recipes for one machine only (publishing, a local Windows rig) live in an
# untracked local.just beside this file, read when it is there.

set shell := ["bash", "-uc"]

import? 'local.just'

# Every SBCL here gets a 4 GB heap (the 1 GB default ran out in a serving
# image) and an 8 MB control stack per thread (a thread's whole stack is
# resident on arm64; 8 MB still leaves the eval runaway probe room to unwind).
sbcl := "sbcl --dynamic-space-size 4096 --control-stack-size 8MB --noinform --non-interactive --eval '(require :asdf)'"

[private]
default:
    @just --list --unsorted

# --- core ---------------------------------------------------------------------

# Does nothing when every pinned cell is already in place.
# Fetch the shipped cells pinned in src/cells/bundle.json.
cells-pull:
    @src/tools/pull-cells.sh

# Compile the organism.
build: cells-pull
    @set -euo pipefail; cd src && {{sbcl}} \
        --eval '(push (truename ".") asdf:*central-registry*)' \
        --eval '(asdf:load-system :nodecode)' \
        --eval '(format t "~&just build complete~%")'

# Run the core test suite.
test:
    @set -euo pipefail; cd src && {{sbcl}} \
        --eval '(push (truename ".") asdf:*central-registry*)' \
        --eval '(asdf:test-system :nodecode)'

# Parens, layout, and IS prose on a line of its own; with no FILES, every
# .lisp and .asd under src/.
# Check Lisp structure.
lint *files:
    @set -euo pipefail; \
      files="{{files}}"; \
      if [ -z "$files" ]; then \
        files="$(find src -name '.*' -prune -o \( -name '*.lisp' -o -name '*.asd' \) -print | LC_ALL=C sort | tr '\n' ' ')"; \
      fi; \
      sbcl --script src/tools/lint-structure.lisp $files

# Hold HEAD's Common Lisp code lines (tokei) to this ceiling. Lower it when
# the code shrinks; raise it only with the reason in the commit. Cells pulled
# into src/cells/ are in no revision here and stay out of the count.
loc_ceiling := "104552"

# Fail when HEAD's Lisp code lines exceed loc_ceiling.
loc:
    @set -euo pipefail; \
      command -v tokei >/dev/null 2>&1 || { \
        printf 'loc: tokei is not installed, and it is the metric this reads.\n' >&2; exit 2; }; \
      git rev-parse --verify HEAD >/dev/null 2>&1 || { \
        printf 'loc: no HEAD to measure.\n' >&2; exit 2; }; \
      tree="$(mktemp -d)"; trap 'rm -rf "$tree"' EXIT; \
      git archive HEAD src | tar -x -C "$tree"; \
      measure() { dir="$1"; shift; tokei "$dir" --types 'Common Lisp' "$@" | awk '$1 == "Common" && $2 == "Lisp" { print $5 }'; }; \
      code="$(measure "$tree/src")"; \
      bundled=""; for marker in $(find src/cells -name .bundle 2>/dev/null); do bundled="$bundled -e ${marker%/.bundle}"; done; \
      live="$(measure src $bundled)"; \
      ceiling={{loc_ceiling}}; \
      if [ -z "$code" ]; then \
        printf 'loc: tokei found no Common Lisp under src/.\n' >&2; exit 2; fi; \
      if [ "$live" != "$code" ]; then \
        printf 'loc: the working tree reads %s, %s of that uncommitted.\n' \
          "$live" "$((live - code))"; fi; \
      if [ "$code" -gt "$ceiling" ]; then \
        printf 'loc: HEAD is %s code lines, %s over the ceiling of %s.\n' "$code" "$((code - ceiling))" "$ceiling" >&2; \
        printf '     Cut the growth, or raise loc_ceiling in the justfile with the reason in the commit.\n' >&2; \
        exit 1; \
      elif [ "$code" -lt "$ceiling" ]; then \
        printf 'loc: HEAD is %s code lines, %s under the ceiling of %s. Set loc_ceiling to %s in this commit.\n' \
          "$code" "$((ceiling - code))" "$ceiling" "$code"; \
      else \
        printf 'loc: HEAD is %s code lines, exactly at the ceiling.\n' "$code"; \
      fi

# Cheapest first: a paren defect fails in a second, not after the whole
# suite has run.
# Gate a change: lint, loc, build, test.
check: lint loc build test

# The heap set here is the binary's for good:
# the dump saves its runtime options, so the executable ignores them on its
# own command line. The dump is forced: a dump killed mid-write leaves an
# empty ./nodecode newer than every fasl, which ASDF would take as finished.
# Then, unless NODECODE_BUILD_COMMIT marks a tarball build (pack.sh compiles
# for the target box itself), the shipped cells compile into src/fasl and
# ./nodecode warms its launch image, so the next launch skips both. On macOS
# the gateway's launcher is compiled and signed beside the binary.
# Dump ./nodecode and warm its launch image.
release: cells-pull
    @set -euo pipefail; \
      cd src && sbcl --dynamic-space-size 8192 --control-stack-size 8MB --noinform --non-interactive \
        --eval '(require :asdf)' \
        --eval '(push (truename ".") asdf:*central-registry*)' \
        --eval '(push (truename "cells/context/") asdf:*central-registry*)' \
        --eval '(asdf:make :nodecode/bin :force (list "nodecode/bin"))'; \
      cd ..; \
      test -s ./nodecode && test -x ./nodecode || \
        { printf 'release: the dump left no executable ./nodecode\n' >&2; exit 1; }; \
      printf 'just release complete: ./nodecode\n'; \
      if [ "$(uname -s)" = Darwin ]; then \
        cc -O2 -Wall -o nodecode-launcher src/tools/launcher.c; \
        codesign --force --sign - --identifier ai.nodecode.launcher nodecode-launcher; \
      fi; \
      if [ -z "${NODECODE_BUILD_COMMIT:-}" ]; then \
        rm -rf src/fasl; \
        src/tools/compile-cells.sh ./nodecode src/cells src/fasl || \
          printf 'release: a shipped cell did not compile; a launch compiles what is missing\n' >&2; \
        ./nodecode warm; \
      fi

# Builds in a throwaway tree, never this one, and names the build for this
# box (x86_64-linux, arm64-darwin): SBCL cannot cross-compile.
# Pack a committed revision as a tarball that runs without SBCL.
tarball rev="HEAD" out="dist":
    @src/tools/pack.sh {{rev}} {{out}}

# Structural duplicates under src/, ranked by what collapsing each cluster
# would reclaim.
# Rank the code written twice.
bloat:
    @set -euo pipefail; cd src && {{sbcl}} \
        --eval '(push (truename ".") asdf:*central-registry*)' \
        --eval '(asdf:load-system :nodecode)' \
        --load tools/bloat.lisp \
        --eval '(princ (nodecode.bloat:report))'

# --- proofs against the built ./nodecode (CI runs the first three) -----------

# Resident memory ceilings in MB: at boot, at any turn of 40 past the fifth,
# growth over the last ten, and after a quiet minute. Lower them as the shell
# gets lighter; raise one only with the reason in the commit.
footprint_ceilings := "--max-boot-mb 95 --max-turn-mb 105 --max-growth-mb 15 --max-idle-mb 46"

# A scratch home with every shipped cell, a mock provider, the shell in tmux.
# Hold the shell to the memory ceilings (~3 min, Linux).
footprint:
    @python3 src/tools/footprint.py ./nodecode --turns 40 {{footprint_ceilings}}

# The gateway must leave a release feed that goes silent without closing,
# and act on the next label.
# Prove a silent release feed is left (~90 s).
feed-proof:
    @python3 src/tools/feed-proof.py ./nodecode

# TARBALL is `just tarball's output; --from takes a tarball, a label or
# stable (the default: what users run now), --keep the scratch.
# Upgrade a running gateway of the last release onto TARBALL (~1 min).
upgrade-proof tarball *args:
    @python3 src/tools/upgrade-proof.py {{tarball}} {{args}}

# Packs HEAD unless TARBALL is HEAD already packed, then packs the refused
# release itself (one commit in the object store, no branch).
# Upgrade a running gateway of HEAD onto a release it refuses live (~2 min).
upgrade-proof-refused *tarball:
    @src/tools/upgrade-proof-refused.sh {{tarball}}

# A loopback Discord, a scripted model and two installed releases.
# Keep a Discord bot answering through an upgrade onto TARBALL and a handoff (~3 min).
upgrade-discord-proof tarball *args:
    @python3 src/tools/upgrade-discord-proof.py {{tarball}} {{args}}

# A loopback Discord and a scripted model; --list names the scenarios,
# --only NAME,... runs some.
# Run the Discord cell end to end (a few minutes).
discord-qa *args:
    @python3 src/tools/discord-qa.py ./nodecode {{args}}

# A loopback Telegram Bot API and the same scripted model; --list names the
# scenarios, --only NAME,... runs some.
# Run the Telegram cell end to end (a few minutes).
telegram-qa *args:
    @python3 src/tools/telegram-qa.py ./nodecode {{args}}

# A loopback Slack -- the Web API and the Socket Mode link -- and the same
# scripted model; --list names the scenarios, --only NAME,... runs some.
# Run the Slack cell end to end (about a minute).
slack-qa *args:
    @python3 src/tools/slack-qa.py ./nodecode {{args}}

# Watched through the rows a browser reads; --sessions N for more, --serve
# keeps the gateway up.
# Load background evaluations (~30 s).
exec-stress *args:
    @python3 src/tools/exec-stress.py ./nodecode {{args}}

# --- cells --------------------------------------------------------------------

# Every directory under src/cells/ that carries an .asd registers with ASDF,
# the way the organism's folder loader registers ~/.nodecode/cells/<name>/.
cell_registry := "(progn (push (truename \".\") asdf:*central-registry*) (dolist (asd (directory \"cells/**/*.asd\")) (push (make-pathname :name nil :type nil :defaults asd) asdf:*central-registry*)))"

# Load SYSTEMS in a scratch image.
_cell-build +systems: cells-pull
    @set -euo pipefail; cd src; args=(); \
      for s in {{systems}}; do args+=(--eval "(asdf:load-system :$s)"); done; \
      {{sbcl}} --eval '{{cell_registry}}' "${args[@]}" --eval '(format t "~&built {{systems}}~%")'

# Load SYSTEMS (the cell's /test systems) and run RUNNER, the cell's filter
# over the shared test registry: the core suite loads for its fixtures, only
# the cell's tests run.
_cell-test runner +systems: cells-pull
    @set -euo pipefail; cd src; args=(); \
      for s in {{systems}}; do args+=(--eval "(asdf:load-system :$s)"); done; \
      {{sbcl}} --eval '{{cell_registry}}' "${args[@]}" --eval '(funcall (uiop:find-symbol* :{{runner}} :nodecode.test))'

board-build: (_cell-build "nodecode-board")
board-test: (_cell-test "run-board-tests" "nodecode-board/test")
board-check: lint board-build board-test

channels-build: (_cell-build "nodecode-channel-discord" "nodecode-channel-telegram" "nodecode-channel-slack")
channels-test: (_cell-test "run-channel-tests" "nodecode-channel-discord/test" "nodecode-channel-telegram/test" "nodecode-channel-slack/test")
channels-check: lint channels-build channels-test

chrome-build: (_cell-build "nodecode-chrome")
chrome-test: (_cell-test "run-chrome-tests" "nodecode-chrome/test")
chrome-check: lint chrome-build chrome-test

# The context cell ships inside the core; its tests run through ASDF's test-op.

context-build: (_cell-build "nodecode-context")
context-test: cells-pull
    @set -euo pipefail; cd src && {{sbcl}} --eval '{{cell_registry}}' --eval '(asdf:test-system :nodecode-context)'
context-check: lint context-build context-test

cron-build: (_cell-build "nodecode-cron")
cron-test: (_cell-test "run-cron-tests" "nodecode-cron/test")
cron-check: lint cron-build cron-test

cyber-security-build: (_cell-build "nodecode-cyber-security")
cyber-security-test: (_cell-test "run-cyber-security-tests" "nodecode-cyber-security/test")
cyber-security-check: lint cyber-security-build cyber-security-test

experience-build: (_cell-build "nodecode-experience")
experience-test: (_cell-test "run-experience-tests" "nodecode-experience/test")
experience-check: lint experience-build experience-test

goal-build: (_cell-build "nodecode-goal")
goal-test: (_cell-test "run-goal-tests" "nodecode-goal/test")
goal-check: lint goal-build goal-test

guard-build: (_cell-build "nodecode-guard")
guard-test: (_cell-test "run-guard-tests" "nodecode-guard/test")
guard-check: lint guard-build guard-test

link-build: (_cell-build "nodecode-link")
link-test: (_cell-test "run-link-tests" "nodecode-link/test")
link-check: lint link-build link-test

mcp-build: (_cell-build "nodecode-mcp")
mcp-test: (_cell-test "run-mcp-tests" "nodecode-mcp/test")
mcp-check: lint mcp-build mcp-test

prs-build: (_cell-build "nodecode-prs")
prs-test: (_cell-test "run-prs-tests" "nodecode-prs/test")
prs-check: lint prs-build prs-test

qa-build: (_cell-build "nodecode-qa")
qa-test: (_cell-test "run-qa-tests" "nodecode-qa/test")
qa-check: lint qa-build qa-test

team-build: (_cell-build "nodecode-team")
team-test: (_cell-test "run-team-tests" "nodecode-team/test")
team-check: lint team-build team-test

web-build: (_cell-build "nodecode-web")
web-test: (_cell-test "run-web-tests" "nodecode-web/test")
web-check: lint web-build web-test

websearch-build: (_cell-build "nodecode-websearch")
websearch-test: (_cell-test "run-websearch-tests" "nodecode-websearch/test")
websearch-check: lint websearch-build websearch-test
