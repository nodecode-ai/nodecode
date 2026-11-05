<#
.SYNOPSIS
  The Windows suite job: every test of the core suite on Windows, held to the
  reds tools/ci/windows-known-red.txt lists.
.DESCRIPTION
  Run from a checkout's root on the Windows runner (.github/workflows/ci.yml),
  or by hand on the same machine. It lays the commit down as ci-windows.ps1
  does, in a stranger's home (quicklisp joined from C:\nodecode-build, which
  the Windows job's release half lays down), compiles the system and every
  test file once (tools/ci/windows-suite.lisp), then runs the suite as -Shards
  processes at once, each in a console of its own, shard I taking every test
  whose place in the suite is I modulo the count.

  A test that ends its process is recorded as KILLED and its shard resumes
  after it. A test in the list marked `skip' is not run (one that hangs costs
  its whole timeout every run).

  The run is red when a test the list does not name goes red, a test file
  loads worse than the list says, fewer tests load than its `tests' floor, or
  a shard does not finish. A listed test that passes is named as now green:
  take it off the list in the change that fixed it. Results and load logs go
  to $Root\out, which the workflow keeps.
  Its last line is `windows-suite-ok <label>' or `windows-suite-red <label>'.
.PARAMETER Only
  Run only these tests: a file of names, one a line, or names joined by
  commas. A fix proves itself this way on the runner by hand; the floor and
  the list's other names are then not checked.
.PARAMETER Shard
  Internal: run one shard (I/N) in the current process, resuming past a test
  that kills it. The job starts one such process per shard.
#>
[CmdletBinding()]
param(
  [string]$Root = $(if ($env:RUNNER_TEMP) { Join-Path $env:RUNNER_TEMP 'nodecode-suite' } else { 'C:\gha-suite' }),
  [int]$Shards = 2,
  [int]$Timeout = 180,
  [string]$Only = '',
  [string]$Shard = ''
)
$ErrorActionPreference = 'Stop'
. (Join-Path $PSScriptRoot 'windows-tree.ps1')

$tree = Join-Path $Root 'nodecode'
$out = Join-Path $Root 'out'

function Find-Sbcl {
  $s = Get-ChildItem "$env:ProgramFiles\Steel Bank Common Lisp" -Recurse -Filter sbcl.exe -ErrorAction SilentlyContinue |
    Select-Object -First 1
  if (-not $s) { throw 'SBCL is not installed on this runner' }
  return $s.FullName
}

# One SBCL from the tree's root, its output to LOG (and LOG.err), waited on
# by itself: a test's child left running must not hold the shard.
function Start-Sbcl([string]$sbcl, [string]$log) {
  $argv = @('--dynamic-space-size', '3072', '--control-stack-size', '64MB', '--noinform', '--non-interactive',
            '--load', "`"$(Join-Path $tree 'tools\ci\windows-suite.lisp')`"")
  $p = Start-Process -FilePath $sbcl -ArgumentList $argv -WorkingDirectory $tree -NoNewWindow -PassThru `
    -RedirectStandardOutput $log -RedirectStandardError "$log.err"
  $null = $p.Handle  # keeps the exit code readable after the process ends
  $p.WaitForExit()
  return $p.ExitCode
}

# --- one shard, in its own console --------------------------------------------
if ($Shard) {
  $tag = $Shard.Replace('/', '-')
  $sbcl = Find-Sbcl
  $env:WINSUITE_PHASE = 'run'
  $env:WINSUITE_SHARD = $Shard
  $res = Join-Path $out "results-$tag.tsv"
  $start = ''
  Set-Location $tree
  for ($round = 0; $round -lt 40; $round++) {
    if ($start) { $env:WINSUITE_START = $start } else { Remove-Item Env:WINSUITE_START -ErrorAction SilentlyContinue }
    $rc = Start-Sbcl $sbcl (Join-Path $out "sbcl-$tag-$round.txt")
    if ((Test-Path $res) -and ((Get-Content $res -Tail 1) -eq 'SUITE-DONE')) { exit 0 }
    $curFile = Join-Path $out "current-$tag.txt"
    $cur = if (Test-Path $curFile) { (Get-Content $curFile -Raw).Trim() } else { '' }
    if (-not $cur -or $cur -eq $start) { "shard $Shard died before its next test (exit $rc)" | Add-Content (Join-Path $out "driver-$tag.txt"); exit 1 }
    "$cur`tKILLED`t0`tthe process exited $rc while this test ran" | Add-Content -Encoding utf8 $res
    "round ${round}: $cur killed the process (exit $rc); resuming after it" | Add-Content (Join-Path $out "driver-$tag.txt")
    $start = $cur
  }
  exit 1
}

# --- the known reds -------------------------------------------------------------
# NAME [skip|flaky] [# why]: a test expected red here (skip: not run at all;
# flaky: red or green, never reported). file:PATH partial: a test file that
# loads only in part. tests N: the floor on how many tests load.
$repo = (git rev-parse --show-toplevel).Trim()
$label = (git -C $repo rev-parse --short=9 HEAD).Trim()
$known = @{}; $files = @{}; $floor = 0
foreach ($line in Get-Content (Join-Path $repo 'tools\ci\windows-known-red.txt')) {
  $t = ($line -replace '#.*$', '').Trim()
  if (-not $t) { continue }
  $w = $t -split '\s+'
  if ($w[0] -eq 'tests') { $floor = [int]$w[1] }
  elseif ($w[0].StartsWith('file:')) { $files[$w[0].Substring(5)] = $w[1] }
  else { $known[$w[0].ToUpper()] = $(if ($w.Count -gt 1) { $w[1] } else { 'red' }) }
}
Write-Host "windows-suite: $label, $($known.Count) known reds, $($files.Count) partial files, floor $floor tests"

# --- the tree, the home, the DLLs -------------------------------------------------
if (Test-Path $Root) { Remove-Item -Recurse -Force $Root }
New-Item -ItemType Directory -Force $Root, $out | Out-Null
New-CommitTree -Repo $repo -Root $Root -Tree $tree

$h = Join-Path $Root 'home'
# The dependencies' compiled output is kept between runs, under the build
# root the Windows job keeps; the tree's own never is. The guest clock runs
# ahead of the host and tar restores commit-time mtimes, so a fasl of the last
# run looks newer than the source that replaced it (see release-windows.ps1).
$cache = 'C:\nodecode-build\suite-cache'
$own = (Split-Path -Qualifier $tree).TrimEnd(':') + $tree.Substring(2)
Get-ChildItem (Join-Path $cache 'common-lisp') -Directory -ErrorAction SilentlyContinue |
  ForEach-Object { Remove-Item -Recurse -Force (Join-Path $_.FullName $own) -ErrorAction SilentlyContinue }
foreach ($d in $h, $cache, (Join-Path $Root 'temp'), (Join-Path $Root 'fasl')) {
  New-Item -ItemType Directory -Force $d | Out-Null
}
$ql = 'C:\nodecode-build\quicklisp'
if (-not (Test-Path (Join-Path $ql 'setup.lisp'))) { throw "no quicklisp at ${ql}: the Windows job's release half lays it down" }
New-Item -ItemType Junction -Path (Join-Path $h 'quicklisp') -Target $ql | Out-Null
'{"update":{"mode":"off"}}' | Set-Content (Join-Path $h 'config.jsonc') -Encoding utf8

# The DLLs the system loads by name, beside the tree as build-windows.ps1 lays
# them: the sqlite.org zip that script names, and Git for Windows' OpenSSL pair.
$ast = [System.Management.Automation.Language.Parser]::ParseFile((Join-Path $tree 'tools\release\build-windows.ps1'), [ref]$null, [ref]$null)
$zipUrl = ($ast.ParamBlock.Parameters | Where-Object { $_.Name.VariablePath.UserPath -eq 'SqliteZip' }).DefaultValue.Value
$zip = Join-Path $Root 'sqlite-dll.zip'
Invoke-WebRequest -UseBasicParsing -Uri $zipUrl -OutFile $zip
Expand-Archive -Path $zip -DestinationPath (Join-Path $Root 'sqlite-dll')
Copy-Item (Join-Path $Root 'sqlite-dll\sqlite3.dll') (Join-Path $tree 'libsqlite3.dll')
$git = (Get-Command git.exe).Source
$ssl = @((Join-Path (Split-Path -Parent (Split-Path -Parent $git)) 'mingw64\bin'), "$env:ProgramFiles\Git\mingw64\bin") |
  Where-Object { Test-Path (Join-Path $_ 'libssl-3-x64.dll') } | Select-Object -First 1
if (-not $ssl) { throw 'libssl-3-x64.dll not found: install Git for Windows' }
Copy-Item (Join-Path $ssl 'libssl-3-x64.dll'), (Join-Path $ssl 'libcrypto-3-x64.dll') $tree

$sbcl = Find-Sbcl
$sbclDir = Split-Path -Parent $sbcl
$env:USERPROFILE = $h; $env:HOME = $h
$env:HOMEDRIVE = Split-Path -Qualifier $h; $env:HOMEPATH = $h.Substring(2)
$env:NODECODE_HOME = Join-Path $h '.nodecode'
$env:XDG_CACHE_HOME = $cache
$env:TEMP = Join-Path $Root 'temp'; $env:TMP = $env:TEMP
$env:SBCL_HOME = $sbclDir
$env:PATH = "$tree;$sbclDir;$env:PATH"
# The tree is a git archive with no .git to ask: the build names its commit by
# the stamp, as release-windows.ps1 stamps a release (nodecode.asd's rule).
$env:NODECODE_BUILD_COMMIT = $label
$env:WINSUITE_OUT = $out
$env:WINSUITE_FASL = Join-Path $Root 'fasl'
$env:WINSUITE_TIMEOUT = "$Timeout"
$skipFile = Join-Path $Root 'skip.txt'
@($known.Keys | Where-Object { $known[$_] -eq 'skip' }) | Set-Content $skipFile -Encoding utf8
$env:WINSUITE_SKIP = $skipFile
if ($Only) {
  $names = if (Test-Path $Only) { Get-Content $Only } else { $Only -split ',' }
  $onlyFile = Join-Path $Root 'only.txt'
  @($names | ForEach-Object { $_.Trim() } | Where-Object { $_ }) | Set-Content $onlyFile -Encoding utf8
  $env:WINSUITE_ONLY = $onlyFile
}

# --- compile once -------------------------------------------------------------------
$t0 = Get-Date
Set-Location $tree
$env:WINSUITE_PHASE = 'compile'
$rc = Start-Sbcl $sbcl (Join-Path $out 'sbcl-compile.txt')
$loadLog = Get-Content (Join-Path $out 'load-compile.txt')
if ($loadLog -notcontains 'COMPILE-DONE') {
  Get-Content (Join-Path $out 'sbcl-compile.txt') -Tail 40 | ForEach-Object { Write-Host $_ }
  throw "the compile phase did not finish (exit $rc)"
}
Write-Host ("windows-suite: compiled in {0:n0} s" -f ((Get-Date) - $t0).TotalSeconds)

# --- the shards, at once, each in a console of its own -------------------------------
$self = Join-Path $repo 'tools\ci\ci-windows-suite.ps1'
$procs = foreach ($i in 0..($Shards - 1)) {
  $argv = @('-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', "`"$self`"", '-Root', "`"$Root`"", '-Shard', "$i/$Shards")
  $p = Start-Process -FilePath powershell.exe -ArgumentList $argv -PassThru -WindowStyle Hidden
  $null = $p.Handle  # keeps the exit code readable after the process ends
  $p
}
while ($procs | Where-Object { -not $_.HasExited }) {
  Start-Sleep -Seconds 60
  $done = 0
  foreach ($i in 0..($Shards - 1)) {
    $r = Join-Path $out "results-$i-$Shards.tsv"
    if (Test-Path $r) { $done += @(Get-Content $r).Count }
  }
  Write-Host ("windows-suite: {0} tests done after {1:n0} s" -f $done, ((Get-Date) - $t0).TotalSeconds)
}

# --- held to the list ---------------------------------------------------------------
$results = @{}; $unfinished = @()
foreach ($i in 0..($Shards - 1)) {
  $r = Join-Path $out "results-$i-$Shards.tsv"
  $lines = if (Test-Path $r) { @(Get-Content $r -Encoding utf8) } else { @() }
  if ($lines.Count -eq 0 -or $lines[-1] -ne 'SUITE-DONE') { $unfinished += "$i/$Shards" }
  foreach ($l in $lines) {
    $c = $l -split "`t"
    if ($c.Count -ge 2) { $results[$c[0].ToUpper()] = @($c[1], $(if ($c.Count -gt 3) { $c[3] } else { '' })) }
  }
}
$loaded = 0
foreach ($l in $loadLog) { if ($l -match '^TESTS (\d+)') { $loaded = [int]$Matches[1] } }
$runLog = Get-Content (Join-Path $out "load-0-$Shards.txt") -ErrorAction SilentlyContinue
foreach ($l in $runLog) { if ($l -match '^TESTS (\d+)') { $loaded = [int]$Matches[1] } }

$new = @(); $green = @(); $worseFiles = @(); $betterFiles = @()
foreach ($n in $results.Keys) {
  $status, $why = $results[$n]
  $k = $known[$n]
  if ($status -ne 'pass' -and $status -ne 'SKIP' -and -not $k) { $new += "$n`t$status`t$why" }
  if ($status -eq 'pass' -and $k -eq 'red') { $green += $n }
}
foreach ($l in $loadLog) {
  if ($l -match '^FILE-(PARTIAL|FAILED) (\S+)') {
    $f = $Matches[2]; $how = $Matches[1].ToLower()
    if (-not $files.ContainsKey($f) -or ($how -eq 'failed' -and $files[$f] -ne 'failed')) { $worseFiles += "$f $how" }
  }
  if ($l -match '^FILE-OK (\S+)' -and $files.ContainsKey($Matches[1])) { $betterFiles += $Matches[1] }
}
$red = @($results.Values | Where-Object { $_[0] -ne 'pass' -and $_[0] -ne 'SKIP' }).Count
$skipped = @($results.Values | Where-Object { $_[0] -eq 'SKIP' }).Count
$short = (-not $Only) -and ($loaded -lt $floor)
$summary = @(
  "### Windows suite $label",
  '',
  "$($results.Count) of $loaded tests: $($results.Count - $red - $skipped) pass, $red red ($($red - $new.Count) known), $skipped skipped.",
  ''
)
if ($new) {
  $summary += "**New reds ($($new.Count)):**", ''
  $summary += ($new | Sort-Object | ForEach-Object { $c = $_ -split "`t", 3; "- ``$($c[0])`` $($c[1]): $($c[2])" })
  $summary += ''
}
if ($worseFiles) { $summary += "**Test files that load worse than listed:** $($worseFiles -join ', ')", '' }
if ($short) { $summary += "**Only $loaded tests loaded; the floor is $floor.**", '' }
if ($unfinished) { $summary += "**Shards that did not finish:** $($unfinished -join ', ')", '' }
if ($green) { $summary += "**Now green, take off the list ($($green.Count)):** $(($green | Sort-Object) -join ', ')", '' }
$stale = if ($Only) { @() } else { @($known.Keys | Where-Object { -not $results.ContainsKey($_) } | Sort-Object) }
if ($stale) { $summary += "**On the list but not in the suite ($($stale.Count)):** $($stale -join ', ')", '' }
if ($betterFiles) { $summary += "**Test files that now load whole:** $($betterFiles -join ', ')", '' }
$summary | ForEach-Object { Write-Host $_ }
if ($env:GITHUB_STEP_SUMMARY) { $summary | Add-Content $env:GITHUB_STEP_SUMMARY -Encoding utf8 }

if ($new -or $worseFiles -or $unfinished -or $short) {
  Write-Host "windows-suite-red $label"
  exit 1
}
Write-Host "windows-suite-ok $label"
