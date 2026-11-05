<#
.SYNOPSIS
  The runtime to run the installed SBCL's core on.

.DESCRIPTION
  Answers sbcl-2.6.8-coreviews.exe beside this script (build-runtime.sh: the
  SBCL 2.6.8 runtime that maps its core where the stock one reads it whole)
  when it runs the core and contribs of the SBCL at -Sbcl, and -Sbcl itself
  otherwise: a runtime built for another SBCL refuses the core. Run the answer
  with --core <the installed SBCL's folder>\sbcl.core and SBCL_HOME at that
  folder. build-windows.ps1 dumps nodecode.exe on it, so every nodecode
  process starts on it; the Windows suite runs its tests on it.

.PARAMETER Sbcl
  The installed sbcl.exe.
#>
param([Parameter(Mandatory = $true)][string]$Sbcl)

# A refusal is a line on stderr, which 'Stop' would raise
$ErrorActionPreference = 'Continue'
$runtime = Join-Path $PSScriptRoot 'sbcl-2.6.8-coreviews.exe'
$dir = Split-Path -Parent $Sbcl
$core = Join-Path $dir 'sbcl.core'
if (-not (Test-Path $runtime) -or -not (Test-Path $core)) { return $Sbcl }
$home0 = $env:SBCL_HOME
$env:SBCL_HOME = $dir
& $runtime --core $core --noinform --non-interactive --no-sysinit --no-userinit `
  --eval '(progn (require :sb-posix) (sb-ext:exit :code 0))' *> $null
$ok = $LASTEXITCODE -eq 0
$env:SBCL_HOME = $home0
if ($ok) { return $runtime }
Write-Host "sbcl: stock runtime ($(Split-Path -Leaf $runtime) does not run $core)"
return $Sbcl
