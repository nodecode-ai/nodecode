<#
.SYNOPSIS
  The Windows CI job: the release's own guest half, run on the checked-out
  commit.
.DESCRIPTION
  Run from a checkout's root on the Windows runner (.github/workflows/ci.yml).
  It lays the commit down the way src/tools/pack-windows.sh lays a release
  down for the guest -- a git archive of HEAD with the shipped cells that
  src/cells/bundle.json pins, as nodecode-src.tar beside release-rev.txt --
  in a share of its own, and runs src/tools/release-windows.ps1 against it
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

# A native command's exit code, checked; its stderr is progress, not an error.
function Run([string]$exe) {
  $ErrorActionPreference = 'Continue'
  & $exe @args
  if ($LASTEXITCODE -ne 0) { throw "$exe $($args -join ' ') failed ($LASTEXITCODE)" }
}

$repo = (git rev-parse --show-toplevel).Trim()
$label = (git -C $repo rev-parse --short=9 HEAD).Trim()
if ($label -notmatch '^[0-9a-f]{9}$') { throw "HEAD has no nine-digit label: '$label'" }
Write-Host "ci-windows: $label from $repo"

if (Test-Path $Root) { Remove-Item -Recurse -Force $Root }
$share = Join-Path $Root 'share'
$stage = Join-Path $Root 'tree'
$tree = Join-Path $stage 'nodecode'
New-Item -ItemType Directory -Force $share, $tree | Out-Null

# --- the commit, from git's objects ------------------------------------------
$headTar = Join-Path $Root 'head.tar'
Run git -C $repo -c core.autocrlf=false -c core.eol=lf archive --format=tar -o $headTar HEAD
Run tar -xf $headTar -C $tree

# --- the shipped cells, as pull-cells.sh lays them down ----------------------
$cells = Join-Path $tree 'src\cells'
$bundle = Get-Content (Join-Path $cells 'bundle.json') -Raw | ConvertFrom-Json
if ($bundle.git -notmatch '^https://') { throw "bundle.json: git is not https: $($bundle.git)" }
if ($bundle.commit -notmatch '^[0-9a-f]{40}$') { throw "bundle.json: commit is not 40 hex: $($bundle.commit)" }
$part = '[A-Za-z0-9_][A-Za-z0-9._-]*'
$pathRe = "^$part(/$part)*$"
$fetch = Join-Path $Root 'cells-repo'
Run git init -q $fetch
Run git -C $fetch fetch -q --depth 1 $bundle.git $bundle.commit
$cellTar = Join-Path $Root 'cell.tar'
$n = 0
foreach ($p in $bundle.cells.PSObject.Properties) {
  $local = $p.Name; $hub = [string]$p.Value
  if ($local -notmatch $pathRe -or $hub -notmatch $pathRe) { throw "bundle.json: bad cell path $local -> $hub" }
  $dest = Join-Path $cells ($local -replace '/', '\')
  New-Item -ItemType Directory -Force $dest | Out-Null
  Run git -C $fetch -c core.autocrlf=false -c core.eol=lf archive --format=tar -o $cellTar FETCH_HEAD $hub
  Run tar -xf $cellTar --strip-components=$(($hub -split '/').Count) -C $dest
  if (-not (Get-ChildItem $dest -Filter *.asd)) { throw "$hub at $($bundle.commit.Substring(0, 9)) has no .asd" }
  [System.IO.File]::WriteAllText((Join-Path $dest '.bundle'), "$($bundle.git)`n$($bundle.commit)`n$hub`n")
  $n++
}
Write-Host "ci-windows: $n shipped cells at $($bundle.commit.Substring(0, 9))"

# --- the share release-windows.ps1 reads -------------------------------------
Run tar -cf (Join-Path $share 'nodecode-src.tar') -C $stage nodecode
[System.IO.File]::WriteAllText((Join-Path $share 'release-rev.txt'), "$label`n")
Remove-Item -Recurse -Force $stage, $fetch, $headTar, $cellTar

# --- the release's guest half, unchanged, in a console of its own -------------
# The runner service starts a job's PowerShell with no console, and from there
# `nodecode.exe trial' wrote nothing at all, while this same script under the
# same account (NETWORK SERVICE, from a scheduled task, which has a console)
# passed every seam (2026-10-27). A release runs over ssh, in a console too.
# So the half runs in a fresh console, its output streamed into the job log.
$out = Join-Path $Root 'release-windows.out'
$err = Join-Path $Root 'release-windows.err'
$argv = @('-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', "`"$(Join-Path $repo 'src\tools\release-windows.ps1')`"",
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
