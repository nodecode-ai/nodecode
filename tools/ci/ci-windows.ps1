<#
.SYNOPSIS
  The Windows CI job: the release's own guest half, run on the checked-out
  commit.
.DESCRIPTION
  Run from a checkout's root on the Windows runner (.github/workflows/ci.yml).
  It lays the commit down the way tools/release/pack-windows.sh lays a release
  down for the guest -- a git archive of HEAD with the shipped cells that
  cells/bundle.json pins, as nodecode-src.tar beside release-rev.txt --
  in a share of its own, and runs tools/release/release-windows.ps1 against it
  unchanged: the build, the platform seams through the trial verb, a headless
  serve, the build-home gate and the zip. So CI proves the path a release
  takes, and nothing touches the publish share (\\host.lan\Data).
  The archive is taken from git's objects with line endings left alone, as
  the release's Linux-made tar has them: Git for Windows converts the working
  tree to CRLF, and the build must never see that.
  Its last line is `windows-ci-ok <label>'.
#>
[CmdletBinding()]
param(
  [string]$Root = $(if ($env:RUNNER_TEMP) { Join-Path $env:RUNNER_TEMP 'nodecode-ci' } else { 'C:\gha-ci' })
)
$ErrorActionPreference = 'Stop'

. (Join-Path $PSScriptRoot 'windows-tree.ps1')

$repo = (git rev-parse --show-toplevel).Trim()
$label = (git -C $repo rev-parse --short=9 HEAD).Trim()
if ($label -notmatch '^[0-9a-f]{9}$') { throw "HEAD has no nine-digit label: '$label'" }
Write-Host "ci-windows: $label from $repo"

if (Test-Path $Root) { Remove-Item -Recurse -Force $Root }
$share = Join-Path $Root 'share'
$stage = Join-Path $Root 'tree'
$tree = Join-Path $stage 'nodecode'
New-Item -ItemType Directory -Force $share, $tree | Out-Null

# --- the commit and its shipped cells, as pull-cells.sh lays them down --------
New-CommitTree -Repo $repo -Root $Root -Tree $tree

# --- the share release-windows.ps1 reads -------------------------------------
Run tar -cf (Join-Path $share 'nodecode-src.tar') -C $stage nodecode
[System.IO.File]::WriteAllText((Join-Path $share 'release-rev.txt'), "$label`n")
Remove-Item -Recurse -Force $stage

# --- the release's guest half, unchanged, in a console of its own -------------
# The runner service starts a job's PowerShell with no console, and from there
# `nodecode.exe trial' wrote nothing at all, while this same script under the
# same account (NETWORK SERVICE, from a scheduled task, which has a console)
# passed every seam (2026-10-27). A release runs over ssh, in a console too.
# So the half runs in a fresh console, its output streamed into the job log.
$out = Join-Path $Root 'release-windows.out'
$err = Join-Path $Root 'release-windows.err'
$argv = @('-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', "`"$(Join-Path $repo 'tools\release\release-windows.ps1')`"",
          '-Share', "`"$share`"", '-Work', "`"$(Join-Path $Root 'work')`"")
$p = Start-Process -FilePath powershell.exe -ArgumentList $argv -RedirectStandardOutput $out -RedirectStandardError $err -PassThru
$null = $p.Handle  # keeps the exit code readable after the process ends
$script:pos = 0
function Show-New([string]$path) {
  if (-not (Test-Path $path)) { return }
  $fs = [System.IO.File]::Open($path, 'Open', 'Read', 'ReadWrite')
  try {
    [void]$fs.Seek($script:pos, 'Begin')
    $text = (New-Object System.IO.StreamReader($fs)).ReadToEnd()
    $script:pos = $fs.Position
  } finally { $fs.Close() }
  if ($text) { Write-Host -NoNewline $text }
}
while (-not $p.HasExited) { Start-Sleep -Seconds 2; Show-New $out }
$p.WaitForExit()
Show-New $out
Get-Content $err -ErrorAction SilentlyContinue | ForEach-Object { Write-Host $_ }
if ($p.ExitCode -ne 0) { throw "release-windows.ps1 failed ($($p.ExitCode))" }
$zip = Join-Path $share "nodecode-$label-x86_64-windows.zip"
if (-not (Test-Path $zip)) { throw "release-windows.ps1 staged no $zip" }
Write-Host "windows-ci-ok $label"
