<#
.SYNOPSIS
  Read-ShardResults, the Windows suite's read of one shard's results file,
  held to every shape a shard leaves.
.DESCRIPTION
  Loads the function from ci-windows-suite.ps1 by its syntax tree, so the
  suite itself does not run, then reads results files written as the suite's
  runner writes them (UTF-8, no BOM). A -Only shard that runs none of the
  named tests leaves a file of just SUITE-DONE: that shard finished.
  Its last line is `ci-windows-suite-test: ok' or it exits 1 naming each
  case that failed.
#>
$ErrorActionPreference = 'Stop'
$suite = Join-Path $PSScriptRoot 'ci-windows-suite.ps1'
$ast = [System.Management.Automation.Language.Parser]::ParseFile($suite, [ref]$null, [ref]$null)
$fn = $ast.Find({ param($n)
    $n -is [System.Management.Automation.Language.FunctionDefinitionAst] -and $n.Name -eq 'Read-ShardResults' }, $true)
if (-not $fn) { Write-Host 'ci-windows-suite-test: ci-windows-suite.ps1 defines no Read-ShardResults'; exit 1 }
. ([scriptblock]::Create($fn.Extent.Text))

$dir = Join-Path ([IO.Path]::GetTempPath()) ('winsuite-test-' + [guid]::NewGuid())
$null = New-Item -ItemType Directory $dir
$failed = @()
try {
  $cases = @(
    @{ Name = 'ran none of the named tests'; Write = @('SUITE-DONE'); Done = $true; Count = 1 },
    @{ Name = 'ran one test'; Write = @("A-TEST`tpass`t0.1`t", 'SUITE-DONE'); Done = $true; Count = 2 },
    @{ Name = 'died after one test'; Write = @("A-TEST`tpass`t0.1`t"); Done = $false; Count = 1 },
    @{ Name = 'wrote nothing'; Write = @(); Done = $false; Count = 0 },
    @{ Name = 'left no file'; Write = $null; Done = $false; Count = 0 }
  )
  $k = 0
  foreach ($c in $cases) {
    $path = Join-Path $dir "results-$k.tsv"; $k++
    if ($null -ne $c.Write) { [IO.File]::WriteAllLines($path, [string[]]$c.Write) }
    $got = Read-ShardResults $path
    if ($got.Done -ne $c.Done -or @($got.Lines).Count -ne $c.Count) {
      $failed += "$($c.Name): Done $($got.Done), $(@($got.Lines).Count) lines; wanted Done $($c.Done), $($c.Count) lines"
    }
  }
} finally {
  Remove-Item -Recurse -Force $dir
}
if ($failed) {
  $failed | ForEach-Object { Write-Host "ci-windows-suite-test: FAILED $_" }
  exit 1
}
Write-Host "ci-windows-suite-test: ok ($($cases.Count) shapes of a shard's results)"
