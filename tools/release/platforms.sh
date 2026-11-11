# platforms.sh --- the platforms a release ships, and the ones it holds back.
#
# SPDX-License-Identifier: MIT
# shellcheck shell=bash
#
# Sourced by publish.sh and release-check.sh. Every platform rides every
# release; leaving one out is said, never assumed, by the setting that names
# it:
#
#   NODECODE_PUBLISH_SKIP_MACOS    no Mac arm. A Mac that is asleep or away is
#                                  a red arm, not a quiet skip: this says so.
#   NODECODE_PUBLISH_SKIP_WINDOWS  Windows is held on the release it runs, for
#                                  as long as the setting is there: until its
#                                  builds are signed, an update is an unsigned
#                                  binary replacing itself, which antivirus
#                                  (Bitdefender) reads as the malware it looks
#                                  like.
#
# A platform held back ships no archive, and every pointer of it -- stable and
# nightly alike -- keeps naming the label it names. Its installs, `nodecode
# update', the poller and the release feed read that platform's pointer alone
# (update.lisp, install.ps1), so all of them stay on that release. On stable
# its own installer stays the one that release came with: install.ps1 is
# Windows' alone, install.sh is Linux's as much as the Mac's.
#
#   release_platforms          the platforms a publish ships, in pack order
#   release_held               the platforms it holds back, or nothing
#   release_holds PLATFORM     whether PLATFORM is held back
#   release_platform_name P    the name a person knows P by
#   release_hold_setting P     the setting that holds P back

release_holds() {
  case "$1" in
    arm64-darwin) [ -n "${NODECODE_PUBLISH_SKIP_MACOS:-}" ] ;;
    x86_64-windows) [ -n "${NODECODE_PUBLISH_SKIP_WINDOWS:-}" ] ;;
    *) return 1 ;;
  esac
}

release_platforms() {
  local p out=''
  for p in x86_64-linux arm64-darwin x86_64-windows; do
    release_holds "$p" || out="$out $p"
  done
  printf '%s\n' "${out# }"
}

release_held() {
  local p out=''
  for p in x86_64-linux arm64-darwin x86_64-windows; do
    ! release_holds "$p" || out="$out $p"
  done
  printf '%s\n' "${out# }"
}

release_platform_name() {
  case "$1" in *-darwin) echo macOS ;; *-windows) echo Windows ;; *) echo Linux ;; esac
}

release_hold_setting() {
  case "$1" in
    arm64-darwin) echo NODECODE_PUBLISH_SKIP_MACOS ;;
    x86_64-windows) echo NODECODE_PUBLISH_SKIP_WINDOWS ;;
  esac
}
