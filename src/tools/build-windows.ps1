<#
.SYNOPSIS
  Build nodecode.exe on Windows: the Windows arm of `just release`.

.DESCRIPTION
  Run from anywhere on a Windows machine with SBCL (x86-64) on PATH and Git
  for Windows installed. Installs quicklisp for the current user when it is
  missing, fetches libsqlite3.dll beside the tree when it is missing, loads
  src/tools/windows-release.lisp in a fresh SBCL with the release heap
  settings, then lays the three DLLs the image loads at run time beside the
  executable: libsqlite3.dll and the OpenSSL pair Git for Windows carries.
  The result is <repo>\nodecode.exe plus its DLLs — the same files
  install.ps1 unpacks from a release zip.

.PARAMETER SqliteZip
  The sqlite.org DLL zip to fetch when no libsqlite3.dll sits beside src/.
#>
[CmdletBinding()]
param(
  [string]$SqliteZip = "https://www.sqlite.org/2026/sqlite-dll-win-x64-3530400.zip"
)

$ErrorActionPreference = 'Stop'
$src = Split-Path -Parent $PSScriptRoot
$repo = Split-Path -Parent $src

function Find-Sbcl {
  $onPath = Get-Command sbcl.exe -ErrorAction SilentlyContinue
  if ($onPath) { return $onPath.Source }
  foreach ($root in @("$env:ProgramFiles\Steel Bank Common Lisp", "$env:LOCALAPPDATA\Programs\Steel Bank Common Lisp")) {
    if (Test-Path $root) {
      $found = Get-ChildItem -Path $root -Recurse -Filter sbcl.exe -ErrorAction SilentlyContinue | Select-Object -First 1
      if ($found) { return $found.FullName }
    }
  }
  throw "sbcl.exe not found: install SBCL x86-64 for Windows (https://github.com/roswell/sbcl_bin/releases) and put it on PATH"
}

function Ensure-Quicklisp {
  # SBCL's (user-homedir-pathname) reads $env:HOME before the profile folder,
  # and PowerShell's $HOME is always the profile: look where SBCL will load.
  $lispHome = if ($env:HOME) { $env:HOME } else { $HOME }
  $setup = Join-Path $lispHome 'quicklisp\setup.lisp'
  if (Test-Path $setup) { return }
  Write-Host "installing quicklisp for $env:USERNAME"
  $bootstrap = Join-Path $env:TEMP 'quicklisp.lisp'
  Invoke-WebRequest -Uri 'https://beta.quicklisp.org/quicklisp.lisp' -OutFile $bootstrap
  & $script:sbcl --noinform --non-interactive --load $bootstrap --eval '(quicklisp-quickstart:install)'
  if ($LASTEXITCODE -ne 0) { throw "quicklisp install failed ($LASTEXITCODE)" }
}

# Named libsqlite3.dll, the first name cl-sqlite asks for on Windows, so the
# build loads this file and the image records the name the zip ships: the
# dumped image reopens exactly that name at every start. Named sqlite3.dll it
# would be the runner's own libsqlite3.dll the build loaded, and a released
# nodecode.exe would die at boot on a machine without one.
function Ensure-Sqlite {
  $dll = Join-Path $src 'libsqlite3.dll'
  if (Test-Path $dll) { return $dll }
  Write-Host "fetching libsqlite3.dll"
  $zip = Join-Path $env:TEMP 'sqlite-dll.zip'
  $dir = Join-Path $env:TEMP 'sqlite-dll'
  Invoke-WebRequest -Uri $SqliteZip -OutFile $zip
  if (Test-Path $dir) { Remove-Item -Recurse -Force $dir }
  Expand-Archive -Path $zip -DestinationPath $dir
  Copy-Item (Join-Path $dir 'sqlite3.dll') $dll
  return $dll
}

function Find-OpenSsl {
  # Git for Windows carries the OpenSSL 3 pair cl+ssl loads by name.
  $candidates = @()
  $git = Get-Command git.exe -ErrorAction SilentlyContinue
  if ($git) { $candidates += (Join-Path (Split-Path -Parent (Split-Path -Parent $git.Source)) 'mingw64\bin') }
  $candidates += "$env:ProgramFiles\Git\mingw64\bin"
  $candidates += "$env:ProgramFiles\OpenSSL\bin"
  $candidates += "$env:ProgramFiles\OpenSSL-Win64\bin"
  foreach ($dir in $candidates) {
    if ((Test-Path (Join-Path $dir 'libssl-3-x64.dll')) -and (Test-Path (Join-Path $dir 'libcrypto-3-x64.dll'))) {
      return $dir
    }
  }
  throw "libssl-3-x64.dll / libcrypto-3-x64.dll not found: install Git for Windows (its mingw64\bin carries them)"
}

$script:sbcl = Find-Sbcl
Write-Host "sbcl: $script:sbcl"
Ensure-Quicklisp
$sqlite = Ensure-Sqlite
$openssl = Find-OpenSsl
# The DLLs the build itself loads (cl-sqlite and cl+ssl load at compile time)
# must be findable now: beside the working directory and on PATH.
Copy-Item (Join-Path $openssl 'libssl-3-x64.dll') (Join-Path $src 'libssl-3-x64.dll') -Force
Copy-Item (Join-Path $openssl 'libcrypto-3-x64.dll') (Join-Path $src 'libcrypto-3-x64.dll') -Force
$env:PATH = "$src;$env:PATH"

Push-Location $src
try {
  & $script:sbcl --dynamic-space-size 8192 --control-stack-size 8MB --noinform --non-interactive `
    --load (Join-Path $src 'tools\windows-release.lisp')
  if ($LASTEXITCODE -ne 0) { throw "sbcl build failed ($LASTEXITCODE)" }
} finally {
  Pop-Location
}

$exe = Join-Path $repo 'nodecode.exe'
$bare = Join-Path $repo 'nodecode'
if (-not (Test-Path $exe) -and (Test-Path $bare)) { Move-Item $bare $exe -Force }
if (-not (Test-Path $exe)) { throw "no executable at $exe after the build" }
foreach ($dll in @('libsqlite3.dll', 'libssl-3-x64.dll', 'libcrypto-3-x64.dll')) {
  Copy-Item (Join-Path $src $dll) (Join-Path $repo $dll) -Force
}
Write-Host "build-windows complete: $exe ($([math]::Round((Get-Item $exe).Length / 1MB)) MB)"
