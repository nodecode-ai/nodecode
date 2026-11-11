#!/usr/bin/env bash
# compile-cells.sh --- compile the shipped cell folders beside their source.
#
# SPDX-License-Identifier: MIT
#
# usage: compile-cells.sh BINARY CELLS_DIR FASL_DIR
#
# FASL_DIR is laid down first: its existence is what routes ASDF's output
# there (SHIPPED-FASL-DIRECTORY, cells.lisp). Every shipped folder but
# template is linked into a scratch home, and BINARY's `trial' verb runs
# there: the trial's load IS the compile, so a launch afterwards loads what
# is compiled and never pays the compiler at its first paint. The release's
# tarball (pack.sh) and a checkout's `just release' both call this, so a
# fresh cache meets compiled folders. The gate is the trial's score: a
# folder that fails to load is named on stderr and the exit is 1; what
# compiled stays.
#
# The scratch home is exactly what the trial must judge — NODECODE_HOME is
# set and not merely left to HOME, since LOCATE-HOME reads it first and the
# operator's shell carries one. Without this, the gate would judge the
# operator's own layer, which no release contains.
#
# Exit 0 when every folder loaded, 1 when a folder failed (named on stderr),
# 3 when the trial itself died before it gave a score -- the runtime crashed
# or the image would not boot -- so no cell was judged and none is to blame.
set -euo pipefail

binary="$1"
cells="$2"
fasl="$3"

test -x "$binary" || { echo "compile-cells: $binary is not executable" >&2; exit 2; }
test -d "$cells" || { echo "compile-cells: $cells is not a directory" >&2; exit 2; }
mkdir -p "$fasl"

home="$(mktemp -d)"
diagnostics="$home/diagnostics.txt"
trap 'rm -rf "$home"' EXIT
mkdir -p "$home/.nodecode/cells"
find "$cells" -name '*.asd' -not -path '*/template/*' -not -path '*/test/*' | while read -r asd; do
  ln -s "$(cd "$(dirname "$asd")" && pwd)" "$home/.nodecode/cells/$(basename "$asd" .asd)"
done

# The form answers the score and writes WHICH folder failed beside it: the
# trial silences a form's own output, so a bare `score 0' would leave the
# failing cell to be guessed at.
# The trial's own exit is kept, not left to errexit: a trial that dies leaves
# no score, and that is this build failing, never a shipped cell.
trial_status=0
score="$(printf '%s' '(progn
  (let ((o (open (uiop:getenv "NODECODE_PACK_DIAGNOSTICS") :direction :output
                 :if-exists :supersede :if-does-not-exist :create)))
    (unwind-protect
         (dolist (cell (nodecode.kernel::cells))
           (unless (eq (nodecode.kernel::cell-state cell) :loaded)
             (format o "~a is ~a~%~{~a~%~}" (nodecode.kernel::cell-name cell)
                     (nodecode.kernel::cell-state cell)
                     (mapcar (lambda (failure)
                               (format nil "  ~a: ~a" (car failure) (cdr failure)))
                             (nodecode.kernel::cell-failures cell)))))
      (close o)))
  (if (find :failed (nodecode.kernel::cells) :key (function nodecode.kernel::cell-state)) 0 1))' \
  | HOME="$home" NODECODE_HOME="$home/.nodecode" \
    XDG_CACHE_HOME="$home/.cache" XDG_DATA_HOME="$home/.local/share" \
    XDG_CONFIG_HOME="$home/.config" \
    NODECODE_PACK_DIAGNOSTICS="$diagnostics" \
    "$binary" trial | sed -n 's/^nodecode-trial-score //p')" || trial_status=$?

if [ -z "$score" ]; then
  echo "compile-cells: the trial died before it judged any cell (exit $trial_status): $binary itself failed, not a shipped cell; its own error is above" >&2
  exit 3
fi

compiled="$(find "$fasl" -name '*.fasl' | wc -l | tr -d ' ')"
if [ "$score" != 1 ]; then
  echo "compile-cells: a shipped cell does not load against this build (score '${score:-none}')" >&2
  if [ -s "$diagnostics" ]; then
    sed 's/^/compile-cells: /' "$diagnostics" >&2
  else
    echo "compile-cells: (the trial scored ${score} but no cell named its failure)" >&2
  fi
  exit 1
fi
if [ "$compiled" -eq 0 ]; then
  echo "compile-cells: nothing compiled under $fasl" >&2
  exit 1
fi
echo "compile-cells: $compiled fasls under $fasl"
