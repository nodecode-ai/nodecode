<#
.SYNOPSIS
  Pack the Windows release inside the local Windows sandbox.

.DESCRIPTION
  The guest half of `just windows'. src/tools/pack-windows.sh drives it through
  ~/nodecode-winbox/win: the host puts a git archive of the release revision --
  its shipped cells pulled in -- at \\host.lan\Data\nodecode-src.tar and the
  nine-digit label in release-rev.txt. This script builds nodecode.exe from
  that tree with src\tools\build-windows.ps1, smokes the platform seams through
  the trial verb and a headless serve, and stages
  nodecode-<label>-x86_64-windows.zip plus its sidecar beside the tar. Its last
  line is `winbox-release-ok <label>'; the host takes that line as the arm's
  verdict and then proves the zip's bytes against the tree it sent.

  Everything runs under a scratch home. This guest is a machine with its own
  nodecode home -- a layer, a store, a config from earlier work -- and a
  release proof belongs to a stranger's machine; pack.sh makes its gate's home
  its own for the same reason.
#>
[CmdletBinding()]
param(
  [string]$Share = '\\host.lan\Data',
  [string]$Work = 'C:\work'
)

$ErrorActionPreference = 'Stop'

$label = (Get-Content (Join-Path $Share 'release-rev.txt') -Raw).Trim()
if ($label -notmatch '^[0-9a-f]{9}$') {
  throw "release-rev.txt must carry a nine-digit label, not '$label'"
}
$name = "nodecode-$label-x86_64-windows"
Write-Host "release-windows: packing $name"

# --- a stranger's home --------------------------------------------------------
if (Test-Path $Work) { Remove-Item -Recurse -Force $Work }
$scratchHome = Join-Path $Work 'home'
$scratchCache = Join-Path $Work 'cache'
$scratchTemp = Join-Path $Work 'temp'
New-Item -ItemType Directory -Force $scratchHome, $scratchCache, $scratchTemp | Out-Null
# windows-release.lisp finds quicklisp under (user-homedir-pathname), which SBCL
# reads off HOME (set below): the scratch home gets the dependency tree as a
# junction, never a copy -- the same quicklisp dist the Linux build compiles
# against -- and nothing else of this user's.
# The tree itself sits at a path that names nobody, C:\nodecode-build\quicklisp,
# copied once from this user's: SBCL writes the true path of every library it
# compiles into the image, and a junction into C:\Users\<account> put that
# account's name in every release. The gate before the zip checks it.
$accountHome = $env:USERPROFILE
$neutralQl = 'C:\nodecode-build\quicklisp'
if (-not (Test-Path (Join-Path $neutralQl 'setup.lisp'))) {
  New-Item -ItemType Directory -Force (Split-Path -Parent $neutralQl) | Out-Null
  robocopy (Join-Path $accountHome 'quicklisp') $neutralQl /E /NFL /NDL /NJH /NJS /NP | Out-Null
  if ($LASTEXITCODE -ge 8) { throw "copying quicklisp to $neutralQl failed ($LASTEXITCODE)" }
  $global:LASTEXITCODE = 0
}
New-Item -ItemType Junction -Path (Join-Path $scratchHome 'quicklisp') -Target $neutralQl | Out-Null
$env:USERPROFILE = $scratchHome
# SBCL's (user-homedir-pathname) reads HOME first and then the profile folder
# from the Windows API, never USERPROFILE: without HOME the build loaded the
# account's own quicklisp and wrote C:/Users/<account>/ into the image.
$env:HOME = $scratchHome
$env:HOMEDRIVE = Split-Path -Qualifier $scratchHome
$env:HOMEPATH = $scratchHome.Substring(2)
$env:NODECODE_HOME = Join-Path $scratchHome '.nodecode'
$env:XDG_CACHE_HOME = $scratchCache
$env:TEMP = $scratchTemp
$env:TMP = $scratchTemp
# A build never looks for a release of its own.
'{"update":{"mode":"off"}}' | Set-Content (Join-Path $scratchHome 'config.jsonc') -Encoding utf8

# --- SBCL: on this session's PATH, not the machine's -------------------------
# The ssh session is up before the machine PATH is applied, so the installer's
# directory is not on it.
$sbcl = Get-ChildItem "$env:ProgramFiles\Steel Bank Common Lisp" -Recurse -Filter sbcl.exe | Select-Object -First 1
if (-not $sbcl) { throw 'SBCL is not installed in the guest' }
$sbclDir = Split-Path -Parent $sbcl.FullName
$env:PATH = "$sbclDir;$env:PATH"
$env:SBCL_HOME = $sbclDir
& $sbcl.FullName --version

# --- no compiled output from an earlier run ----------------------------------
# The guest clock runs hours ahead of the host and tar restores the archive's
# commit-time mtimes, so a fasl from the last build looks newer than the source
# that replaced it and ASDF skips the recompile: the 8b9a8dc51 zip carried
# f172368ed's terminal.fasl, its fix missing. The dependencies' fasls stay
# cached; the tree's own and the cells' do not.
Get-ChildItem (Join-Path $env:LOCALAPPDATA 'cache\common-lisp') -Directory -ErrorAction SilentlyContinue |
  ForEach-Object { Remove-Item -Recurse -Force (Join-Path $_.FullName 'C\work') -ErrorAction SilentlyContinue }
Remove-Item -Recurse -Force (Join-Path $env:LOCALAPPDATA 'nodecode\peripherals') -ErrorAction SilentlyContinue

# --- the tree ----------------------------------------------------------------
$tree = Join-Path $Work 'nodecode'
tar -xf (Join-Path $Share 'nodecode-src.tar') -C $Work
if (-not (Test-Path (Join-Path $tree 'src\nodecode.asd'))) { throw "no nodecode tree under $Work after the extract" }
$cells = @(Get-ChildItem (Join-Path $tree 'src\cells') -Directory -ErrorAction SilentlyContinue)
if ($cells.Count -lt 2 -or -not (Test-Path (Join-Path $tree 'src\cells\bundle.json'))) {
  throw 'the archive carries no shipped cells: an installed organism could offer none'
}
Write-Host "release-windows: $($cells.Count) shipped cells under $tree"

$env:NODECODE_BUILD_COMMIT = $label
Push-Location $tree
try {
  Write-Host 'release-windows: === build ==='
  & (Join-Path $tree 'src\tools\build-windows.ps1')
  if ($LASTEXITCODE -ne 0) { throw "build-windows.ps1 failed ($LASTEXITCODE)" }
} finally { Pop-Location }

# --- the platform seams, through the trial verb ------------------------------
# The image answers, the bare launch path survives, and what it reopens at
# start is the files beside it, not this machine's PATH: a user's Windows has
# none of the guest's DLLs.
Write-Host 'release-windows: === smoke: the platform seams ==='
Set-Location $tree
& {
  $ErrorActionPreference = 'Stop'
  function Trial([string]$form) {
    $out = ($form | .\nodecode.exe trial 2>&1 | Out-String)
    Write-Host "trial: $form"
    Write-Host $out
    if ($out -notmatch 'nodecode-trial-score (\S+)') { throw "no score line for: $form" }
    return $Matches[1]
  }
  if ((Trial '(+ 1 2)') -ne '3') { throw 'arithmetic' }
  # The bare launch's first step: a build that keeps no launch images follows no
  # pointer. nothing here runs the shell.
  if ((Trial '(progn (nlk::hand-over-to-published-image) 1)') -ne '1') { throw 'the bare launch path' }
  $path = $env:PATH
  try {
    $env:PATH = "$env:SystemRoot\System32;$env:SystemRoot"
    if ((Trial '(+ 2 3)') -ne '5') { throw 'the executable needs a DLL it does not ship' }
  } finally { $env:PATH = $path }
  if ((Trial '(if (= 2 (length (multiple-value-list (nlt:terminal-size)))) 1 0)') -ne '1') { throw 'terminal-size without a console' }
  if ((Trial '(if (and nlk:*shell* (search "sh" (namestring nlk:*shell*))) 1 0)') -ne '1') { throw 'no shell resolved' }
  if ((Trial '(multiple-value-bind (out err status) (nlk:run-bounded (nlk:shell-argv "echo hi; echo bad 1>&2; exit 4") :seconds 20) (if (and (search "hi" out) (search "bad" err) (eql status 4)) 1 0))') -ne '1') { throw 'run-bounded' }
  if ((Trial '(if (eq :timeout (nth-value 2 (nlk:run-bounded (nlk:shell-argv "sleep 30") :seconds 1))) 1 0)') -ne '1') { throw 'run-bounded timeout' }
  if ((Trial '(if (nlk:pid-live-p (sb-posix:getpid)) 1 0)') -ne '1') { throw 'pid-live-p self' }
  # The idle release on Windows is a working-set trim: the working set is read,
  # and the policy's release shrinks it.
  if ((Trial '(let ((before (nlk:resident-bytes))) (if (and (integerp before) (eq t (nlk::release-image-pages)) (< (nlk:resident-bytes) before)) 1 0))') -ne '1') { throw 'working-set trim' }
  if ((Trial '(if (nlk:pid-live-p 4000000) 0 1)') -ne '1') { throw 'pid-live-p dead' }
  $lock = (Join-Path $scratchTemp 'probe.lock') -replace '\\', '/'
  if ((Trial "(let ((lock (nlk:lock-file `"$lock`"))) (if (and lock (nlk:file-locked-p `"$lock`") (progn (nlk:unlock-file lock) (not (nlk:file-locked-p `"$lock`")))) 1 0))") -ne '1') { throw 'lock-file' }
  $log = (Join-Path $scratchTemp 'detached.log') -replace '\\', '/'
  if ((Trial "(progn (nlk:spawn-detached (list `"cmd`" `"/c`" `"echo detached-marker`") `"$log`") (sleep 3) (if (search `"detached-marker`" (uiop:read-file-string `"$log`")) 1 0))") -ne '1') { throw 'spawn-detached' }
  $from = (Join-Path $scratchTemp 'from.txt') -replace '\\', '/'
  $to = (Join-Path $scratchTemp 'to.txt') -replace '\\', '/'
  Set-Content -Path $from -Value 'new'
  Set-Content -Path $to -Value 'old'
  if ((Trial "(progn (nlk:replace-file `"$from`" `"$to`") (if (and (search `"new`" (uiop:read-file-string `"$to`")) (not (probe-file `"$from`"))) 1 0))") -ne '1') { throw 'replace-file' }
}

# --- the launcher ------------------------------------------------------------
# What the user PATH names: bin\nodecode.exe runs the release `current` names,
# the arguments as typed, stdin and the exit code passed through, and the next
# launch follows `current` when it moves - a window opened before an update
# runs the new release.
Write-Host 'release-windows: === smoke: the launcher ==='
& {
  $ErrorActionPreference = 'Stop'
  $root = Join-Path $scratchTemp 'launcher'
  $bin = Join-Path $root 'bin'
  $current = Join-Path $root 'current'
  New-Item -ItemType Directory -Force $bin | Out-Null
  Copy-Item (Join-Path $tree 'nodecode-launcher.exe') (Join-Path $bin 'nodecode.exe')
  $launch = Join-Path $bin 'nodecode.exe'
  $none = (& { $ErrorActionPreference = 'Continue'; & $launch --version 2>&1 | Out-String })
  if ($LASTEXITCODE -ne 127 -or $none -notmatch 'no release is current') { throw "the launcher with no current: $LASTEXITCODE $none" }
  New-Item -ItemType Junction -Path $current -Target $tree | Out-Null
  $out = ('(+ 4 5)' | & $launch trial 2>&1 | Out-String)
  if ($out -notmatch 'nodecode-trial-score 9') { throw "the launcher's trial: $out" }
  & { $ErrorActionPreference = 'Continue'; & $launch --no-such-flag 2>&1 | Out-Null }
  if ($LASTEXITCODE -ne 2) { throw "the launcher passed exit $LASTEXITCODE for usage's 2" }
  $other = Join-Path $root 'other'
  New-Item -ItemType Directory -Force $other | Out-Null
  (Get-Item $current).Delete()
  New-Item -ItemType Junction -Path $current -Target $other | Out-Null
  $moved = (& { $ErrorActionPreference = 'Continue'; & $launch --version 2>&1 | Out-String })
  if ($LASTEXITCODE -ne 127 -or $moved -notmatch 'holds no nodecode.exe') { throw "the launcher did not follow current: $moved" }
  (Get-Item $current).Delete()
  Write-Host 'launcher ok'
}

# --- a headless serve, the store lock and the release of it -------------------
# The other half of the port: a Windows box with no console still boots, serves
# and publishes the port a client attaches to, and its store lock is held while
# it lives and free after it.
Write-Host 'release-windows: === smoke: a headless serve ==='
& {
  $ErrorActionPreference = 'Stop'
  $store = Join-Path $scratchTemp 'serve\history.db'
  New-Item -ItemType Directory -Force (Split-Path -Parent $store) | Out-Null
  $log = Join-Path $scratchTemp 'serve.log'
  $p = Start-Process -FilePath (Join-Path $tree 'nodecode.exe') -ArgumentList @('serve', '--store', $store) -RedirectStandardOutput $log -RedirectStandardError "$log.err" -PassThru -NoNewWindow
  $owner = "$store.owner"
  $port = $null
  for ($i = 0; $i -lt 120; $i++) {
    Start-Sleep -Milliseconds 500
    if ($p.HasExited) { break }
    if (Test-Path $owner) {
      $json = Get-Content $owner -Raw | ConvertFrom-Json
      if ($json.port) { $port = $json.port; break }
    }
  }
  Get-Content $log, "$log.err" -ErrorAction SilentlyContinue | Select-Object -Last 40
  if ($p.HasExited) { throw "serve exited $($p.ExitCode) before publishing a port" }
  if (-not $port) { throw 'serve never published its port' }
  Write-Host "serve published port $port"
  $lockHeld = ($(try { [System.IO.File]::Open("$store.lock", 'Open', 'ReadWrite', 'None').Close(); $false } catch { $true }))
  if (-not $lockHeld) { throw 'the owner does not hold the store lock' }
  Stop-Process -Id $p.Id -Force
  Start-Sleep -Seconds 1
  $lockFree = ($(try { [System.IO.File]::Open("$store.lock", 'Open', 'ReadWrite', 'None').Close(); $true } catch { $false }))
  if (-not $lockFree) { throw 'the lock outlived its owner' }
}

# --- stage what a user unpacks ----------------------------------------------
# The same five things beside the binary that install.ps1 lays down from a
# release: the exe, the three DLLs the image reopens by name, the installer (a
# running organism stages its own update through it), the source it reads its
# definitions from, the shipped cells found as <source>\..\cells\ -- without
# them an installed organism can offer no cell at all -- and the system
# definition those folders declare a depend-on. resources/ does not travel.
Write-Host 'release-windows: === stage ==='
$stage = Join-Path $tree 'dist\nodecode'
New-Item -ItemType Directory -Force $stage | Out-Null
Copy-Item (Join-Path $tree 'nodecode.exe'), (Join-Path $tree 'libsqlite3.dll'),
          (Join-Path $tree 'libssl-3-x64.dll'), (Join-Path $tree 'libcrypto-3-x64.dll'),
          (Join-Path $tree 'nodecode-launcher.exe') $stage
Copy-Item (Join-Path $tree 'src\tools\install.ps1') $stage
New-Item -ItemType Directory -Force (Join-Path $stage 'src') | Out-Null
Copy-Item -Recurse (Join-Path $tree 'src\src') (Join-Path $stage 'src\src')
Copy-Item -Recurse (Join-Path $tree 'src\cells') (Join-Path $stage 'src\cells')
# A cell's gates are the maintainer's, not the user's, and the folder loads
# without them (pack.sh prunes the same directories).
Get-ChildItem (Join-Path $stage 'src\cells') -Recurse -Directory -Filter test | Remove-Item -Recurse -Force
Copy-Item (Join-Path $tree 'src\nodecode.asd') (Join-Path $stage 'src\nodecode.asd')

# The gate: the build account's home is written nowhere in the program a
# stranger runs (SBCL keeps every build path in the image).
foreach ($shipped in @('nodecode.exe', 'nodecode-launcher.exe')) {
  $exeText = [System.Text.Encoding]::GetEncoding(28591).GetString([System.IO.File]::ReadAllBytes((Join-Path $stage $shipped)))
  foreach ($form in @("$accountHome\", (($accountHome -replace '\\', '/') + '/'))) {
    if ($exeText.IndexOf($form, [System.StringComparison]::OrdinalIgnoreCase) -ge 0) {
      throw "refusing: the build account's home ($form) is written in $shipped"
    }
  }
  Remove-Variable exeText
}
$zip = Join-Path $Share "$name.zip"
Remove-Item -Force $zip, "$zip.sha256" -ErrorAction SilentlyContinue
Compress-Archive -Path (Join-Path $stage '*') -DestinationPath $zip
$hash = (Get-FileHash $zip -Algorithm SHA256).Hash.ToLower()
"$hash  $name.zip" | Out-File "$zip.sha256" -Encoding ascii
Write-Host "release-windows: $name.zip ($([math]::Round((Get-Item $zip).Length / 1MB)) MB) sha256 $hash"
Write-Host "winbox-release-ok $label"
