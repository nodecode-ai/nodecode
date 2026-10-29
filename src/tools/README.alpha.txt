nodecode — alpha build (Linux x86_64, macOS Apple Silicon)

WHAT THIS IS
  A self-contained build. No git clone, no SBCL, no Quicklisp, no compile
  step. The executable carries the whole organism; the src/ directory beside
  it is the source the organism reads its own definitions from, and the
  cell folders it installs from. Keep the two together.

CHECK YOUR BOX FIRST — LINUX
  Paste this. It answers in one line per item.

    ldd --version | head -1
    for l in libsqlite3.so.0 libssl.so.3 libcrypto.so.3; do
      printf '%-18s ' "$l"
      ldconfig -p | grep -q "$l" && echo present || echo MISSING
    done
    printf '%-18s ' ca-certificates
    test -e /etc/ssl/certs/ca-certificates.crt && echo present || echo MISSING
    printf '%-18s ' git; command -v git >/dev/null && echo present || echo MISSING

  glibc must be 2.34 or newer. Ubuntu 22.04+, Debian 12+, RHEL/Alma/Rocky
  9+, Amazon Linux 2023, Fedora 35+ and Arch are fine. Ubuntu 20.04, Debian
  11 and RHEL 8 will NOT run this build at all, and there is no workaround
  short of a newer distro.

  ca-certificates is not optional and its absence is quiet: without it the
  first-run provider list comes up EMPTY with no error on screen, because
  the catalog is fetched over HTTPS. If that list is empty, install it and
  relaunch before assuming anything else is wrong.

  git is not needed to start, and cells load without it. It is needed for
  what the organism commits: its own layers, its memory, and the file
  history behind checkpoints. Install it.

  On a bare Debian, all four of those come from:
    sudo apt install -y ca-certificates git libsqlite3-0 openssl

CHECK YOUR BOX FIRST — MACOS
  Apple Silicon, macOS 15 (Sequoia) or newer. Paste this:

    sw_vers -productVersion
    printf '%-18s ' openssl@3
    test -e /opt/homebrew/opt/openssl/lib/libssl.dylib && echo present || echo MISSING
    printf '%-18s ' zstd
    test -e /opt/homebrew/opt/zstd/lib/libzstd.1.dylib && echo present || echo MISSING
    printf '%-18s ' git; command -v git >/dev/null && echo present || echo MISSING

  OpenSSL 3 and zstd from Homebrew are both required: the build loads them
  by those paths, and without either the executable does not start at all.
  The CA bundle comes with OpenSSL, so the provider list is not empty on a
  Mac for that reason.
    brew install openssl@3 zstd
  git comes with the Xcode command line tools (xcode-select --install).
  sqlite3 is the system's own; nothing to install.

RUN IT
  tar -xzf nodecode-*.tar.gz
  cd nodecode-*-*-*/
  ./nodecode

  First launch opens a setup wizard: it offers to import from other coding
  agents it finds, then you pick a provider, paste its key, pick a model and
  its reasoning effort. Cells are turned on afterwards, in /cells. State
  lives under ~/.nodecode.

  Put it on your PATH if you like — link the binary, do not move it away
  from its src/ directory:
    ln -s "$PWD/nodecode" ~/.local/bin/nodecode

TRY IT WITHOUT TOUCHING ANYTHING
  ./nodecode --ephemeral
  A throwaway store in /tmp, its own gateway, channels stood down. Only the
  store is thrown away: config and layer edits still land in ~/.nodecode.

KNOWN, EXPECTED
  This is a pre-release build. The one-liner installs and updates it:
    curl -fsSL https://nodecode.ai/install.sh | sh
  and a running organism updates itself (`nodecode update', src/UPGRADE.md).
