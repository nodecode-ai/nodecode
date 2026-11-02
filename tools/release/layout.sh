# layout.sh --- where a nodecode tree keeps its Lisp system, in either layout.
#
# SPDX-License-Identifier: MIT
#
# Sourced by the release scripts. The repository kept its system under src/
# (nodecode.asd, cells/, tools/, the code in src/src/) until the move put it at
# the root (nodecode.asd, cells/, tools/ there, the code in src/). A release
# keeps the old shape for good -- <release>/src/src/, src/cells/, src/fasl/,
# src/nodecode.asd -- because the image that takes an update runs its OWN
# update.lisp, and every one ever shipped reads those paths (RELEASE-PLANS-AT,
# CURRENT-RELEASE-DIRECTORY, RELEASE-NAME, *SOURCE-ROOT*). So a packer reads a
# tree either way and lays the release out one way, and packing a revision
# from before the move still works.
#
#   layout_system DIR    the folder of a tree on disk holding nodecode.asd,
#                        cells/ and tools/: DIR/src before the move, DIR after.
#                        Its src/ is the code either way.
#   layout_asd_at REV    nodecode.asd's path in a commit, from the root.
#   layout_ships_at REV  the pathspecs of what a release built from REV ships,
#                        one a line (resources/ is the same in both).

layout_system() {
  if [ -f "${1%/}/src/nodecode.asd" ]; then printf '%s\n' "${1%/}/src"; else printf '%s\n' "${1%/}"; fi
}

layout_asd_at() {
  if git cat-file -e "$1:src/nodecode.asd" 2>/dev/null; then printf 'src/nodecode.asd\n'; else printf 'nodecode.asd\n'; fi
}

layout_ships_at() {
  if [ "$(layout_asd_at "$1")" = src/nodecode.asd ]; then
    # The installers lived at src/tools/install.sh and install.ps1 before they
    # moved into src/tools/install/: both count.
    printf '%s\n' src/src src/cells src/nodecode.asd resources \
      src/tools/install/install.sh src/tools/install/install.ps1 src/tools/install.sh src/tools/install.ps1 \
      ':(exclude,glob)src/cells/**/test/**'
  else
    printf '%s\n' src cells nodecode.asd resources tools/install/install.sh tools/install/install.ps1 \
      ':(exclude,glob)cells/**/test/**'
  fi
}
