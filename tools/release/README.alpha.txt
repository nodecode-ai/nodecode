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

  glibc must be 2.38 or newer. Ubuntu 24.04+, Fedora 39+, Debian 13, Arch
  are fine. Ubuntu 22.04 (2.35) and Debian 12 (2.36) will NOT run this
  build at all, and there is no workaround short of a newer distro.

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
  Apple Silicon, macOS 14 or newer. Paste this:

    sw_vers -productVersion
    printf '%-18s ' openssl@3
    test -e /opt/homebrew/opt/openssl/lib/libssl.dylib && echo present || echo MISSING
    printf '%-18s ' git; command -v git >/dev/null && echo present || echo MISSING

  OpenSSL 3 from Homebrew is required: the build loads it by that path, and
  without it the executable does not start at all. Its CA bundle comes with
  it, so the provider list is not empty on a Mac for that reason.
    brew install openssl@3
  git comes with the Xcode command line tools (xcode-select --install).
  sqlite3 is the system's own; nothing to install.

RUN IT
  tar -xzf nodecode-*.tar.gz
  cd nodecode-*-*-*/
  ./nodecode

  First launch opens a setup wizard: pick a provider, paste its key, pick a
  model, choose which cells to install. State lives under ~/.nodecode.

  Put it on your PATH if you like — link the binary, do not move it away
  from its src/ directory:
    ln -s "$PWD/nodecode" ~/.local/bin/nodecode

TRY IT WITHOUT TOUCHING ANYTHING
  ./nodecode --ephemeral
  A throwaway store in /tmp, its own gateway, channels stood down. Nothing
  it does in that session survives the exit.

KNOWN, EXPECTED
  This is a pre-release build off an internal commit. There is no installer
  and no update path yet: a new build arrives as a new tarball.
