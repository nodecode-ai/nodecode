#!/usr/bin/env python3
"""Apply a bounded, revision-scoped formatting bridge to shipped cells.

Only ordinary unified text diffs changing existing files are supported. Each
file is independently pristine or already patched; conflicts fail before any
file is changed. This is not a general dependency patch format.
"""
import json
from pathlib import Path, PurePosixPath
import re
import subprocess
import sys


PATH = re.compile(r'[A-Za-z0-9_][A-Za-z0-9._-]*(/[A-Za-z0-9_][A-Za-z0-9._-]*)*')
HUNK = re.compile(r'@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@.*\n?')


def target(cells, names, value):
    """Reject traversal, undeclared cells and every symlink along a target."""
    if not PATH.fullmatch(value) or any(p in ('.', '..') for p in value.split('/')):
        raise ValueError(f'unsafe patch target: {value}')
    path = PurePosixPath(value)
    if not any(value.startswith(str(name) + '/') for name in names):
        raise ValueError(f'patch target outside declared cells: {value}')
    current = cells
    for part in path.parts:
        current /= part
        if current.is_symlink():
            raise ValueError(f'symlink patch target: {value}')
    return value


def file_patches(text, cells, names):
    """Read only same-path unified diffs, honoring hunk counts, not content."""
    lines = text.splitlines(keepends=True)
    index = 0
    while index < len(lines):
        if not lines[index].strip():
            index += 1
            continue
        start = index
        if not lines[index].startswith('--- a/') or index + 1 >= len(lines):
            raise ValueError('expected ordinary unified diff old target')
        old = target(cells, names, lines[index][6:].rstrip('\n'))
        index += 1
        if not lines[index].startswith('+++ b/'):
            raise ValueError('expected ordinary unified diff new target')
        new = target(cells, names, lines[index][6:].rstrip('\n'))
        if old != new:
            raise ValueError('formatting bridge cannot rename files')
        index += 1
        hunks = 0
        while index < len(lines) and lines[index].startswith('@@ '):
            match = HUNK.fullmatch(lines[index])
            if not match:
                raise ValueError('invalid unified diff hunk')
            before = int(match[2]) if match[2] is not None else 1
            after = int(match[4]) if match[4] is not None else 1
            index += 1
            while before or after:
                if index >= len(lines):
                    raise ValueError('truncated unified diff hunk')
                prefix = lines[index][:1]
                if prefix not in (' ', '-', '+'):
                    raise ValueError('invalid unified diff body')
                before -= prefix in (' ', '-')
                after -= prefix in (' ', '+')
                if before < 0 or after < 0:
                    raise ValueError('invalid unified diff counts')
                index += 1
            hunks += 1
        if not hunks:
            raise ValueError('formatting bridge needs a text hunk')
        yield old, ''.join(lines[start:index])


def main():
    mode, name = sys.argv[1:]
    if mode not in ('check', 'apply'):
        raise ValueError(f'unknown mode: {mode}')
    manifest = Path(name).resolve()
    cells = manifest.parent
    bundle = json.loads(manifest.read_text())
    names = []
    for local in bundle['cells']:
        if not PATH.fullmatch(local) or any(p in ('.', '..') for p in local.split('/')):
            raise ValueError(f'unsafe declared cell: {local}')
        names.append(PurePosixPath(local))
    patches = []
    seen = set()
    for entry in bundle.get('patches', []):
        if entry['commit'] != bundle['commit']:
            raise ValueError(f"patch revision mismatch: {entry['file']}; review patch for new pin")
        patch = (cells / entry['file']).resolve()
        if patch.parent != cells or not patch.is_file():
            raise ValueError(f"patch must be a file beside bundle.json: {entry['file']}")
        files = list(file_patches(patch.read_text(), cells, names))
        if not files:
            raise ValueError(f'empty patch: {patch.name}')
        for name, text in files:
            if name in seen:
                raise ValueError(f'duplicate patch target: {name}')
            seen.add(name)
            patches.append((name, text))
    if mode == 'check':
        return
    # Running inside the target tree works for ignored checkout cells and
    # release staging alike, without disabling Git's own safe-path checks.
    command = ['git', 'apply']
    pending = []
    for name, text in patches:
        if not (cells / name).is_file():
            raise ValueError(f'formatting target is not an existing file: {name}')
        def check(reverse=False):
            return subprocess.run(command + ['--check'] + (['--reverse'] if reverse else []),
                                  input=text, capture_output=True, text=True, cwd=cells)
        if check(reverse=True).returncode == 0:
            print(f'cell-patches: {name} already applied')
        elif check().returncode == 0:
            pending.append((name, text))
        else:
            raise ValueError(f'patch conflict: {name}; preserve edits and review the pinned sources')
    if pending:
        # A single application keeps the preflighted pristine subset atomic.
        subprocess.run(command, input=''.join(text for _, text in pending), text=True, check=True, cwd=cells)
        for name, _ in pending:
            print(f'cell-patches: applied {name}')


if __name__ == '__main__':
    try:
        main()
    except (ValueError, KeyError, OSError, subprocess.CalledProcessError) as error:
        sys.exit(f'cell-patches: {error}')
