"""Offline integration tests for cell materialization (see cell-patches.feature)."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[3]
PULL = ROOT / 'src/tools/pull-cells.sh'
PATCH = ROOT / 'src/tools/patch-cells.py'


def run(*args, **kw):
    return subprocess.run(args, text=True, stdout=subprocess.PIPE,
                          stderr=subprocess.STDOUT, **kw)


class CellPatchScenarios(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(dir=ROOT / 'tmp')
        self.addCleanup(self.tmp.cleanup)
        self.base = Path(self.tmp.name)
        repo = self.repo = self.base / 'upstream'
        repo.mkdir()
        run('git', 'init', '-q', str(repo)).check_returncode()
        (repo / 'cell').mkdir()
        (repo / 'cell/example.asd').write_text('(defsystem "example")\n')
        (repo / 'cell/code.lisp').write_text('(original)\n')
        run('git', '-C', str(repo), 'add', '.').check_returncode()
        run('git', '-C', str(repo), '-c', 'user.name=Test', '-c',
            'user.email=test@example.invalid', 'commit', '-qm', 'fixture').check_returncode()
        self.pin = run('git', '-C', str(repo), 'rev-parse', 'HEAD').stdout.strip()
        self.cells = self.base / 'cells'
        self.cells.mkdir()
        patch = self.cells / 'formatting.patch'
        patch.write_text('--- a/example/code.lisp\n+++ b/example/code.lisp\n'
                         '@@ -1 +1 @@\n-(original)\n+(formatted)\n')
        self.manifest = {'git': 'https://cells-test.invalid/repo',
                         'commit': self.pin, 'cells': {'example': 'cell'},
                         'patches': [{'file': 'formatting.patch', 'commit': self.pin}]}
        self.save()
        self.env = dict(os.environ, GIT_CONFIG_COUNT='1',
                        GIT_CONFIG_KEY_0=f'url.{repo.as_uri()}.insteadOf',
                        GIT_CONFIG_VALUE_0='https://cells-test.invalid/repo',
                        GIT_TERMINAL_PROMPT='0')

    def save(self):
        (self.cells / 'bundle.json').write_text(json.dumps(self.manifest))

    def pull(self):
        return run('bash', str(PULL), str(self.cells), env=self.env)

    def test_fresh_pull_applies_patch(self):
        result = self.pull()
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertEqual((self.cells / 'example/code.lisp').read_text(), '(formatted)\n')

    def test_repeat_pull_is_byte_identical(self):
        self.assertEqual(self.pull().returncode, 0)
        source = self.cells / 'example/code.lisp'
        before = hashlib.sha256(source.read_bytes()).hexdigest()
        result = self.pull()
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertEqual(source.read_text(), '(formatted)\n')
        self.assertEqual(hashlib.sha256(source.read_bytes()).hexdigest(), before)

    def test_pin_mismatch_fails(self):
        self.manifest['patches'][0]['commit'] = '0' * 40
        self.save()
        result = self.pull()
        self.assertNotEqual(result.returncode, 0, result.stdout)
        self.assertIn('patch revision', result.stdout)

    def test_conflict_is_preserved(self):
        self.assertEqual(self.pull().returncode, 0)
        source = self.cells / 'example/code.lisp'
        source.write_text('(local-edit)\n')
        result = self.pull()
        self.assertNotEqual(result.returncode, 0, result.stdout)
        self.assertIn('patch conflict', result.stdout)
        self.assertEqual(source.read_text(), '(local-edit)\n')


    def apply(self, mode='apply'):
        return run('python3', str(PATCH), mode, str(self.cells / 'bundle.json'))

    def set_patch(self, old='a/example/code.lisp', new='b/example/code.lisp'):
        (self.cells / 'formatting.patch').write_text(
            f'--- {old}\n+++ {new}\n@@ -1 +1 @@\n-(original)\n+(formatted)\n')

    def test_partial_refresh_of_two_cells(self):
        (self.repo / 'other').mkdir()
        (self.repo / 'other/other.asd').write_text('(defsystem "other")\n')
        (self.repo / 'other/code.lisp').write_text('(original)\n')
        run('git', '-C', str(self.repo), 'add', '.').check_returncode()
        run('git', '-C', str(self.repo), '-c', 'user.name=Test', '-c',
            'user.email=test@example.invalid', 'commit', '-qm', 'second cell').check_returncode()
        pin = run('git', '-C', str(self.repo), 'rev-parse', 'HEAD').stdout.strip()
        self.manifest['commit'] = self.manifest['patches'][0]['commit'] = pin
        self.manifest['cells']['other'] = 'other'
        self.save()
        patch = self.cells / 'formatting.patch'
        patch.write_text(patch.read_text() + '--- a/other/code.lisp\n+++ b/other/code.lisp\n'
                         '@@ -1 +1 @@\n-(original)\n+(formatted)\n')
        self.assertEqual(self.pull().returncode, 0)
        # Missing .asd forces pull-cells to refresh only this cell.
        (self.cells / 'example/example.asd').unlink()
        result = self.pull()
        self.assertEqual(result.returncode, 0, result.stdout)
        for cell in ('example', 'other'):
            self.assertEqual((self.cells / cell / 'code.lisp').read_text(), '(formatted)\n')
        self.assertEqual(self.pull().returncode, 0)

    def test_all_files_preflight_before_conflict(self):
        self.assertEqual(self.pull().returncode, 0)
        first = self.cells / 'example/code.lisp'
        first.write_text('(original)\n')
        second = self.cells / 'example/second.lisp'
        second.write_text('(local-edit)\n')
        patch = self.cells / 'formatting.patch'
        patch.write_text(patch.read_text() + '--- a/example/second.lisp\n+++ b/example/second.lisp\n'
                         '@@ -1 +1 @@\n-(original)\n+(formatted)\n')
        self.assertNotEqual(self.apply().returncode, 0)
        self.assertEqual(first.read_text(), '(original)\n')
        self.assertEqual(second.read_text(), '(local-edit)\n')

    def test_unsafe_old_and_new_targets_rejected_in_both_modes(self):
        self.assertEqual(self.pull().returncode, 0)
        outside = self.base / 'outside.txt'
        outside.write_text('(original)\n')
        bad = ('a/../outside.txt', 'a/example/../../outside.txt',
               'a/undeclared/code.lisp', 'a//absolute.txt',
               str(outside), 'a/example/../code.lisp')
        for value in bad:
            for side in ('old', 'new'):
                old = value if side == 'old' else 'a/example/code.lisp'
                new = value.replace('a/', 'b/', 1) if side == 'new' else 'b/example/code.lisp'
                self.set_patch(old, new)
                for mode in ('check', 'apply'):
                    with self.subTest(value=value, side=side, mode=mode):
                        result = self.apply(mode)
                        self.assertNotEqual(result.returncode, 0, result.stdout)
                        self.assertEqual(outside.read_text(), '(original)\n')
                        self.assertEqual((self.cells / 'example/code.lisp').read_text(), '(formatted)\n')

    def test_symlink_file_and_directory_escape_rejected(self):
        self.assertEqual(self.pull().returncode, 0)
        outside = self.base / 'outside'
        outside.mkdir()
        source = outside / 'code.lisp'
        source.write_text('(original)\n')
        target = self.cells / 'example/code.lisp'
        target.unlink()
        target.symlink_to(source)
        for mode in ('check', 'apply'):
            self.assertNotEqual(self.apply(mode).returncode, 0)
        target.unlink()
        (self.cells / 'example/link').symlink_to(outside, target_is_directory=True)
        self.set_patch('a/example/link/code.lisp', 'b/example/link/code.lisp')
        for mode in ('check', 'apply'):
            self.assertNotEqual(self.apply(mode).returncode, 0)
        self.assertEqual(source.read_text(), '(original)\n')

    def test_symlink_cell_root_escape_rejected(self):
        self.assertEqual(self.pull().returncode, 0)
        (self.cells / 'example').rename(self.base / 'outside')
        (self.cells / 'example').symlink_to(self.base / 'outside', target_is_directory=True)
        for mode in ('check', 'apply'):
            self.assertNotEqual(self.apply(mode).returncode, 0)
        self.assertEqual((self.base / 'outside/code.lisp').read_text(), '(formatted)\n')

    def test_rename_or_git_metadata_is_not_a_formatting_patch(self):
        self.assertEqual(self.pull().returncode, 0)
        self.set_patch(new='b/example/renamed.lisp')
        self.assertNotEqual(self.apply('check').returncode, 0)
        patch = self.cells / 'formatting.patch'
        patch.write_text('diff --git a/example/code.lisp b/../outside.txt\n'
                         'similarity index 100%\nrename from example/code.lisp\n'
                         'rename to ../outside.txt\n')
        self.assertNotEqual(self.apply('check').returncode, 0)

    def test_later_unsafe_patch_prevents_any_write(self):
        self.assertEqual(self.pull().returncode, 0)
        source = self.cells / 'example/code.lisp'
        source.write_text('(original)\n')
        (self.cells / 'bad.patch').write_text('--- a/../outside.txt\n+++ b/../outside.txt\n'
                                              '@@ -1 +1 @@\n-(original)\n+(formatted)\n')
        self.manifest['patches'].append({'file': 'bad.patch', 'commit': self.pin})
        self.save()
        self.assertNotEqual(self.apply().returncode, 0)
        self.assertEqual(source.read_text(), '(original)\n')


    def test_unsupported_or_malformed_patches_fail_closed(self):
        self.assertEqual(self.pull().returncode, 0)
        valid = (self.cells / 'formatting.patch').read_text()
        cases = ('', valid + valid,
                 valid.replace('@@ -1 +1 @@', '@@ -2,4 +2,4 @@'),
                 valid + 'old mode 100644\nnew mode 100755\n',
                 valid.replace('--- a/example/code.lisp', '--- /dev/null'),
                 valid.replace('+++ b/example/code.lisp', '+++ /dev/null'))
        for text in cases:
            (self.cells / 'formatting.patch').write_text(text)
            for mode in ('check', 'apply'):
                with self.subTest(text=text, mode=mode):
                    self.assertNotEqual(self.apply(mode).returncode, 0)
                    self.assertEqual((self.cells / 'example/code.lisp').read_text(), '(formatted)\n')

    def test_missing_patch_and_patch_outside_bundle_fail_before_pull(self):
        for file in ('missing.patch', '../outside.patch'):
            self.manifest['patches'][0]['file'] = file
            self.save()
            result = self.pull()
            self.assertNotEqual(result.returncode, 0)
            self.assertIn('patch must be a file beside bundle.json', result.stdout)
            self.assertFalse((self.cells / 'example').exists())


if __name__ == '__main__':
    (ROOT / 'tmp').mkdir(exist_ok=True)
    unittest.main(verbosity=2)
