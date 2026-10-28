#!/usr/bin/env python3
# cell-slices.py --- the shipped cells' test slices, one line each, for CI.
#
# SPDX-License-Identifier: MIT
#
# usage: cell-slices.py CELLS_DIR
#
# Every cell's NAME/test system names the runner its test-op calls
# (nodecode.test's RUN-<SLICE>-TESTS, from DEFINE-TEST-SLICE). Systems that
# share a runner -- the three channel adapters over the kit -- are one slice,
# so the shared tests run once. Each line is SLICE|RUNNER|SYSTEM..., sorted;
# a /test system naming no runner is refused, so a cell CI cannot run is
# said rather than skipped.
import pathlib
import re
import sys

root = pathlib.Path(sys.argv[1])
slices = {}
problems = []
for asd in sorted(root.rglob('*.asd')):
    text = asd.read_text(encoding='utf-8')
    starts = [m.start() for m in re.finditer(r'\(defsystem\s', text)] + [len(text)]
    for begin, end in zip(starts, starts[1:]):
        block = text[begin:end]
        name = re.match(r'\(defsystem\s+"([^"]+)"', block)
        if not name or not name.group(1).endswith('/test'):
            continue
        runner = re.search(r"'#:(run-[a-z0-9-]+-tests)", block)
        if not runner:
            problems.append('%s: %s names no run-*-tests runner' % (asd, name.group(1)))
            continue
        slices.setdefault(runner.group(1), []).append(name.group(1))
for problem in problems:
    print('cell-slices: ' + problem, file=sys.stderr)
if problems or not slices:
    sys.exit(1)
for runner in sorted(slices):
    print('%s|%s|%s' % (runner[len('run-'):-len('-tests')], runner, ' '.join(sorted(slices[runner]))))
