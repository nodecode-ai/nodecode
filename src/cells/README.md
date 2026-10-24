# Shipped cells and downstream patches

`bundle.json` pins the upstream cells revision. `just cells-pull` materializes
ignored dependency folders and applies the explicit `patches` list. Packaging
uses the same materializer. No manual formatting overlay is required.

`formatting.patch` is a formatting-only correction for four files at
`780fbc5eeb65ca4c85decc6f2c3f7ff3dc324d13`: assertion descriptions use the
layout required by the core structural linter, and two auxiliary bindings
are aligned. Assertions, descriptions, and Lisp expressions are preserved.
The patch is tracked here because its upstream repository is separately
pinned; no upstream change or publication is implied.

Each patch names the exact upstream commit it was reviewed against. On a pin
change, review and update or remove it explicitly. Missing patches, mismatched
revisions, and conflicts fail rather than silently skipping formatting fixes.
Each file is recognized independently as pristine or already applied, so a
selective cell refresh works even while other cells remain patched. All files
are preflighted before a single application of the pristine subset; local
conflicting edits are not overwritten on a repeat pull. A changed upstream pin still replaces cell
folders, as the materializer has always done.

Run `just cell-patches-test` for offline regression scenarios, and `just lint`
for the resulting source layout. Both are part of `just check`.

## Bounded bridge and removal condition

Prefer fixing these four files in the upstream cells repository. This downstream
bridge exists only until that correction can be consumed at a reviewed pin.
When advancing to a corrected upstream commit, remove `formatting.patch` and its
manifest entry; if no other downstream bridge is needed, remove the applicator,
its pull hooks and its dedicated tests as well. A future unrelated pin must not
silently inherit this correction.

Only ordinary unified text diffs of existing files at identical old/new paths
are supported, not Git rename, mode, binary, creation or deletion metadata. Every
old and new target must be beneath a declared cell, without absolute paths,
parent traversal or symlink components. `check` validates targets before a pull
can fetch or replace sources; `apply` repeats validation after materialization.
This is a build helper for a stable staging tree, not a sandbox against another
process concurrently replacing paths while application is in progress.

The repository's LOC ceiling and its enforcement are unchanged by this fix.
