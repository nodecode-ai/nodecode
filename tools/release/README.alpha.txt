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
  9+, Amazon Linux 2023, Fedora 35+ and Arch are fine. Ubuntu 20.04,
  Debian 11 and RHEL 8 will NOT run this build at all, and there is no
  workaround short of a newer distro.

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
  Apple Silicon, macOS 15 or newer. Paste this:

    sw_vers -productVersion
    printf '%-18s ' openssl@3; [ -e /opt/homebrew/opt/openssl@3/lib/libssl.3.dylib ] && echo present || echo MISSING
    printf '%-18s ' zstd; [ -e /opt/homebrew/opt/zstd/lib/libzstd.1.dylib ] && echo present || echo MISSING
    printf '%-18s ' git; command -v git >/dev/null && echo present || echo MISSING

  Homebrew's openssl@3 and zstd are required: the build opens both by their
  Homebrew paths, and without either the executable does not start at all.
  The CA bundle comes with openssl@3, so the provider list is not empty on a
  Mac for that reason.
    brew install openssl@3 zstd

  It is openssl@3 by name: the build opens that formula's own path. Homebrew's
  unversioned `openssl' is OpenSSL 4 now, which removed a function this build
  calls. Having it installed is fine; the build never opens it.
  git comes with the Xcode command line tools (xcode-select --install).
  sqlite3 is the system's own; nothing to install.

RUN IT
  tar -xzf nodecode-*.tar.gz
  cd nodecode-*-*-*/
  ./nodecode

  First launch opens a setup wizard: pick a provider, paste its key, pick a
  model, choose which cells to install. State lives under ~/.nodecode.

  Run from this folder, nodecode stays the build you unpacked: it does not
  update itself. Keep the binary beside its src/ directory. To put it on
  your PATH and keep it current, install it instead.

INSTALL AND UPDATE
  The installer travels in this folder:
    sh install.sh

  It fetches the newest stable release from nodecode.ai, checks its sha256,
  lays it out under ~/.local/share/nodecode and links ~/.local/bin/nodecode
  to it, the same as `curl -fsSL https://nodecode.ai/install.sh | sh'. An
  installed build updates itself by default, and `nodecode update' moves it
  to the newest release at any time.

TRY IT WITHOUT TOUCHING ANYTHING
  ./nodecode --ephemeral
  A throwaway store in /tmp, its own gateway, channels stood down. Nothing
  it does in that session survives the exit.

KNOWN, EXPECTED
  This is a pre-release build off an internal commit.
