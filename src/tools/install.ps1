<#
.SYNOPSIS
  Install nodecode on Windows in one line.

.DESCRIPTION
  The Windows arm of the one-line installer, the twin of install.sh:

    irm https://nodecode.ai/install.ps1 | iex

  Downloads the release the channel pointer names (or unpacks -FromZip),
  checks its sha256
  against the sidecar published beside it, lays it out under
  %LOCALAPPDATA%\Programs\nodecode\<version>\ - the executable, its three
  DLLs, and src\, the source the organism reads itself from and the cell
  folders it ships - then points a stable `current` junction at it, puts
  the launcher folder bin\ on the user PATH, and clears mark-of-the-web so
  the executable runs without a SmartScreen stop. Re-running installs the
  newer version beside the old and moves the junction; a running organism
  keeps its own files, since nothing is overwritten in place.

  bin\nodecode.exe is the launcher a release carries (nodecode-launcher.exe,
  src\tools\launcher.cs): it runs the nodecode.exe of the release `current`
  names, read at every launch. A window keeps the PATH it opened with, and
  when the PATH named each version's own folder, a window that ran
  `nodecode update` went on running the release it updated from. The PATH
  cannot name `current` itself: Windows 11 refuses an elevated process
  passage through a junction a non-elevated one made (RedirectionGuard, error
  448, "an untrusted mount point"), and PowerShell skips such a PATH entry
  without a word. The organism's own update moves `current` unelevated, and
  an SSH login to an administrator is elevated, so after the first update
  `nodecode` was "not recognized" in every SSH shell (winpc, build 26200,
  2026-10-04). The launcher reads where the junction points without passing
  through it. A release that carries no launcher, or one that does not
  answer, puts the version's own folder on the PATH instead.

  Git for Windows is required at run time (layers commit through git, and
  its sh.exe is the shell commands run through); the installer says so when
  it is missing and prints the winget line.

  A running organism updates itself through the copy of this file its
  release carries (update.lisp), in one of two narrower modes named by
  NODECODE_STAGE, as install.sh's:

    stage   lay the version out beside the others and probe it where it
            lies; `current` does not move, the PATH is not touched
    link    point `current` at a version laid out earlier, lay its launcher
            in bin\, and move the user PATH's entry under the install
            folder to bin\; no download

  NODECODE_INSTALL_DIR names where the versions lie.

  This file stays ASCII. Windows PowerShell decodes a script fetched with
  irm as Latin-1 when the server names no charset, and a UTF-8 dash comes
  out as a curly quote, which its parser reads as a real one.

.PARAMETER Version
  The release to install: the channel pointer's unless -Version or
  NODECODE_VERSION names one.
.PARAMETER FromZip
  A local release zip to install instead of downloading - the CI artifact.
.PARAMETER NoModifyPath
  Leave the user PATH alone and print what to add.
.PARAMETER Force
  Download and lay the release out again even when it is already laid out
  and its executable answers (NODECODE_FORCE=1 does the same).
#>
[CmdletBinding()]
param(
  [string]$Version = '',
  [string]$FromZip = '',
  [switch]$NoModifyPath,
  [switch]$Force
)

$baseUrl = if ($env:NODECODE_BASE_URL) { $env:NODECODE_BASE_URL } else { 'https://nodecode.ai/dl' }
# The release host is https:// or a folder (file://), and nothing else: the
# rule the organism's own update check holds too (update.lisp,
# RELEASE-HOST-P). Over plain http anyone on the path could hand over another
# pointer, another archive and the sha256 sidecar that matches it.
if ($baseUrl -notmatch '^(https|file)://') {
  throw "NODECODE_BASE_URL is https:// or file:// (got $baseUrl): over plain http anyone on the path could swap the release"
}
$channel = if ($env:NODECODE_CHANNEL) { $env:NODECODE_CHANNEL } else { 'stable' }
$root = if ($env:NODECODE_INSTALL_DIR) { $env:NODECODE_INSTALL_DIR } else { Join-Path $env:LOCALAPPDATA 'Programs\nodecode' }

function Get-ChannelVersion {
  # The channel pointer is the one copy of which release is current: the same
  # file a running organism's own update check reads, so an install and a
  # self-update can never name different builds. A label is a short commit id;
  # without that check a host answering with an error page would be read as a
  # release name.
  # Read past any HTTP cache: the release host's edge answers /dl from cache
  # for hours and rewrites the cache-control it was given, so a plain read can
  # be an answer from before this file existed - or a label that has moved.
  $pointer = "$baseUrl/$channel-x86_64-windows"
  $read = $pointer
  if ($baseUrl -match '^https:') { $read = "$pointer`?cb=$([DateTimeOffset]::UtcNow.ToUnixTimeSeconds())" }
  # The host serves the pointer as application/octet-stream, and for that type
  # Invoke-WebRequest hands back the body as bytes, not text: .Trim() on it
  # threw, and every Windows install read "no release is published".
  try { $body = (Invoke-WebRequest -Uri $read -UseBasicParsing).Content }
  catch { throw "no $channel release is published for x86_64-windows yet - $pointer did not answer ($($_.Exception.Message))" }
  if ($body -is [byte[]]) { $body = [Text.Encoding]::ASCII.GetString($body) }
  $label = "$body".Trim()
  if ($label -notmatch '^[0-9a-f]{7,40}$') { throw "$pointer did not name a release: $label" }
  return $label
}

$stage = if ($env:NODECODE_STAGE) { $env:NODECODE_STAGE } else { 'full' }
if (@('full', 'stage', 'link') -notcontains $stage) { throw "NODECODE_STAGE is full, stage or link (got $stage)" }

# -FromZip is a local artifact: it names its own release, so no pointer is read.
# A link names a version already laid out, so it never asks the channel.
if (-not $Version -and -not $FromZip) {
  if ($env:NODECODE_VERSION) { $Version = $env:NODECODE_VERSION }
  elseif ($stage -eq 'link') { throw 'a link needs the version to point at: set NODECODE_VERSION' }
  else { $Version = Get-ChannelVersion }
}
# A label becomes a folder under $root that a reinstall replaces whole, so
# '.' or '..' would name the install folder or its parent: a label is 1 to 63
# of letters, digits and ._+-, never starting with a dot - update.lisp's
# RELEASE-LABEL-P, the rule a label the organism stages already passed.
if ($Version -and $Version -cnotmatch '^[A-Za-z0-9_+-][A-Za-z0-9._+-]{0,62}$') {
  throw "NODECODE_VERSION is not a release label: $Version"
}
# Two containers are published under one label: tar.zst, which bsdtar unpacks
# in a fifth of the time over a quarter less wire, and the zip every release
# before that one carries - and that an installer older than this one reads.
# Get-Release tries them in that order and says which it took.
function Get-ArchiveNames([string]$label) {
  return @("nodecode-$label-x86_64-windows.tar.zst", "nodecode-$label-x86_64-windows.zip")
}

function Say([string]$text) { Write-Host "nodecode: $text" }

function Assert-Platform {
  if (-not [Environment]::Is64BitOperatingSystem) { throw 'nodecode needs 64-bit Windows' }
  $arch = $env:PROCESSOR_ARCHITECTURE
  if ($arch -ne 'AMD64') { throw "nodecode ships for x86-64 Windows only today (this machine is $arch)" }
}

function Save-Download([string]$url, [string]$path) {
  # The archive is ~30 MB. Invoke-WebRequest with its progress hidden printed
  # nothing until it was done, so a slow link read as a hang (a Windows PC on
  # a 54 Mbps Wi-Fi link, 2026-10-22: over a minute under a bare
  # "downloading" line, where the same PC fetches it in 8 s); with its progress
  # shown, Windows PowerShell 5.1 redraws on every chunk and the download
  # itself crawls. This copies the stream itself and redraws one line at most
  # twice a second, then says the size, the time and the rate it got - only
  # in a one-liner's window: a staged update's output is read, not watched.
  $live = $stage -eq 'full'
  $response = [Net.WebRequest]::Create($url).GetResponse()
  try {
    $total = [long]$response.ContentLength
    $in = $response.GetResponseStream()
    $out = [IO.File]::Create($path)
    try {
      $buffer = New-Object byte[] 262144
      [long]$done = 0
      $clock = [Diagnostics.Stopwatch]::StartNew()
      $drawn = [long]-1000
      while (($n = $in.Read($buffer, 0, $buffer.Length)) -gt 0) {
        $out.Write($buffer, 0, $n)
        $done += $n
        if ($live -and $clock.ElapsedMilliseconds - $drawn -ge 500) {
          $drawn = $clock.ElapsedMilliseconds
          $rate = $done / 1MB / [Math]::Max($clock.Elapsed.TotalSeconds, 0.001)
          $of = if ($total -gt 0) { ' of {0:N1}' -f ($total / 1MB) } else { '' }
          Write-Host -NoNewline ("`rnodecode: {0:N1}{1} MB, {2:N1} MB/s" -f ($done / 1MB), $of, $rate).PadRight(60)
        }
      }
    } finally { $out.Dispose(); $in.Dispose() }
  } finally { $response.Close() }
  $seconds = [Math]::Max($clock.Elapsed.TotalSeconds, 0.001)
  $line = 'nodecode: downloaded {0:N1} MB in {1:N1} s, {2:N1} MB/s' -f ($done / 1MB), $seconds, ($done / 1MB / $seconds)
  if ($live) { Write-Host ("`r" + $line.PadRight(60)) } else { Write-Host $line }
}

function Get-HttpStatus($errorRecord) {
  # A failure in Save-Download reaches the caller wrapped: .GetResponse()
  # throws a MethodInvocationException whose inner exception is the
  # WebException that holds the answer. => the status, or 0 for a failure
  # that never reached the server.
  $e = $errorRecord.Exception
  while ($e) {
    if ($e -is [Net.WebException] -and $e.Response) { return [int]$e.Response.StatusCode }
    $e = $e.InnerException
  }
  return 0
}

function Get-Release {
  param([string]$work)
  if ($FromZip) {
    $local = Join-Path $work (Split-Path -Leaf $FromZip)
    Copy-Item $FromZip $local
    $sidecar = "$FromZip.sha256"
    if (Test-Path $sidecar) { Copy-Item $sidecar "$local.sha256" }
    return $local
  }
  $names = @(Get-ArchiveNames $Version)
  for ($i = 0; $i -lt $names.Count; $i++) {
    $name = $names[$i]
    $path = Join-Path $work $name
    $url = "$baseUrl/$name"
    Say "downloading $url"
    # Windows PowerShell 5.1 reports this host's 404 as "The connection was
    # closed unexpectedly", which says nothing about a release that is not
    # there: name the URL instead.
    try { Save-Download $url $path }
    catch {
      # The one miss that moves on is a release published before tar.zst was:
      # it carries the zip alone. Said, never silent; any other failure stops.
      $status = Get-HttpStatus $_
      if ($status -eq 404 -and $i -lt $names.Count - 1) {
        Say "$name is not published for this release"
        continue
      }
      throw "download failed: $url ($($_.Exception.Message))"
    }
    try { Invoke-WebRequest -Uri "$url.sha256" -OutFile "$path.sha256" -UseBasicParsing }
    catch { throw "no sha256 is published for ${url}: a release nothing vouches for is not installed" }
    return $path
  }
  throw "download failed: no archive of $Version at $baseUrl"
}

function Assert-Checksum([string]$zip) {
  # A download is installed only against the sha256 its host published:
  # Get-Release refuses one that has none. A -FromZip archive is the
  # operator's own file, checked when a sidecar lies beside it.
  $sidecar = "$zip.sha256"
  if (-not (Test-Path $sidecar)) {
    if ($FromZip) { Say "no sha256 beside $FromZip; the operator's own file is installed as it is"; return }
    throw "no sha256 for ${zip}: a release nothing vouches for is not installed"
  }
  $expected = ((Get-Content $sidecar -Raw) -split '\s+')[0].ToLower()
  $actual = (Get-FileHash $zip -Algorithm SHA256).Hash.ToLower()
  if ($expected -ne $actual) { throw "sha256 mismatch for ${zip}: expected $expected, got $actual" }
  Say 'sha256 verified'
}

function Test-Executable([string]$exe) {
  # The probe is an argument the CLI rejects: it answers any unknown one with
  # its usage, which proves the executable and its DLLs load. It writes that
  # on STDERR and exits 2 - and the caller runs under
  # $ErrorActionPreference = 'Stop', where merging native stderr with 2>&1
  # makes ErrorRecords that THROW. Left alone, the install lands every file
  # and then dies on its own verification line, which reads to the user as a
  # failed install. Scope the preference to the probe: everything around it
  # should still stop on the first error.
  $probe = & {
    $ErrorActionPreference = 'Continue'
    try { & $exe --help 2>&1 | Out-String } catch { $_.Exception.Message }
  }
  # One line of it: an executable that will not start answers with a whole
  # PowerShell error record, whose last line is a row of tildes.
  $first = @("$probe" -split "`r?`n" | Where-Object { $_.Trim() }) | Select-Object -First 1
  $first = "$first" -replace '\s*At \S+:\d+ char:\d+.*$', ''
  return @{ Runs = ($probe -match 'usage: nodecode'); Answer = $first.Trim() }
}

function Set-Current([string]$target) {
  $current = Join-Path $root 'current'
  if (Test-Path $current) { (Get-Item $current).Delete() }
  New-Item -ItemType Junction -Path $current -Target $target | Out-Null
}

function Invoke-Retried([scriptblock]$step) {
  # Installs into one folder at once share its few files - the `current`
  # junction, base-url - and Windows refuses a file another process holds:
  # the one that finds it held tries again rather than failing an install
  # whose release is already laid out.
  for ($try = 0; ; $try++) {
    try { & $step; return }
    catch { if ($try -ge 20) { throw }; Start-Sleep -Milliseconds 150 }
  }
}

function Test-UnderRoot([string]$entry) {
  $base = [IO.Path]::GetFullPath($root).TrimEnd('\') + '\'
  $full = [Environment]::ExpandEnvironmentVariables($entry).TrimEnd('\') + '\'
  return $full.StartsWith($base, [StringComparison]::OrdinalIgnoreCase)
}

function Set-UserPath([string]$dir, [switch]$Add) {
  # The user PATH's entry under the install folder names $dir: one naming
  # another version, or `current`, gives way in its place. None is added
  # without -Add - a link moves the entry an install put there, and an
  # install that left the PATH alone stays so. => whether the PATH changed.
  # Read and written raw, of the kind it is: GetEnvironmentVariable expands
  # %VARS% and SetEnvironmentVariable writes a plain string, which froze each
  # of the operator's %VAR% entries - and a link rewrites the PATH at every
  # update.
  if ($Add) {
    # This session gets the directory whether or not the user PATH already
    # has it: a rerun in a window opened before the first install would
    # otherwise find nodecode on the user PATH and not in its own.
    $env:Path = (@($dir) + @($env:Path -split ';' | Where-Object { $_ -and -not (Test-UnderRoot $_) })) -join ';'
  }
  $key = [Microsoft.Win32.Registry]::CurrentUser.CreateSubKey('Environment')
  try {
    $raw = "$($key.GetValue('Path', '', 'DoNotExpandEnvironmentNames'))"
    $kind = if ($key.GetValueNames() -contains 'Path') { $key.GetValueKind('Path') } else { 'ExpandString' }
    $parts = New-Object 'Collections.Generic.List[string]'
    $at = -1
    foreach ($entry in ($raw -split ';')) {
      if (-not $entry) { continue }
      if (Test-UnderRoot $entry) { if ($at -lt 0) { $at = $parts.Count } }
      else { $parts.Add($entry) }
    }
    if ($at -lt 0) {
      if (-not $Add) { return $false }
      $at = $parts.Count
    }
    $parts.Insert($at, $dir)
    $value = $parts -join ';'
    if ($value -ceq $raw) { return $false }
    $key.SetValue('Path', $value, $kind)
  } finally { $key.Close() }
  # Explorer hands a new terminal the environment it read at login unless
  # told it changed, as SetEnvironmentVariable tells it.
  try {
    if (-not ('Nodecode.Environment' -as [type])) {
      Add-Type -Namespace Nodecode -Name Environment -MemberDefinition '[DllImport("user32.dll", CharSet = CharSet.Unicode)] public static extern IntPtr SendMessageTimeout(IntPtr window, uint message, UIntPtr wide, string text, uint flags, uint timeout, out UIntPtr result);'
    }
    $result = [UIntPtr]::Zero
    [Nodecode.Environment]::SendMessageTimeout([IntPtr]0xffff, 0x1a, [UIntPtr]::Zero, 'Environment', 2, 5000, [ref]$result) | Out-Null
  } catch { Say "the user PATH changed and Windows was not told ($($_.Exception.Message)); sign out and in to pick it up" }
  return $true
}

function Test-SameFile([string]$a, [string]$b) {
  if (-not (Test-Path $a) -or -not (Test-Path $b)) { return $false }
  $x = [IO.File]::ReadAllBytes($a)
  $y = [IO.File]::ReadAllBytes($b)
  if ($x.Length -ne $y.Length) { return $false }
  for ($i = 0; $i -lt $x.Length; $i++) { if ($x[$i] -ne $y[$i]) { return $false } }
  return $true
}

function Set-Launcher([string]$release) {
  # bin\nodecode.exe becomes the launcher $release carries, which runs whatever
  # `current` names. => bin\, the folder the user PATH names, or nothing when
  # neither $release nor an earlier install left a launcher that answers - the
  # PATH then names the release's own folder, as it did before launchers.
  $bin = Join-Path $root 'bin'
  $exe = Join-Path $bin 'nodecode.exe'
  $from = Join-Path $release 'nodecode-launcher.exe'
  if ((Test-Path $from) -and -not (Test-SameFile $from $exe)) {
    New-Item -ItemType Directory -Force $bin | Out-Null
    $new = "$exe.new-" + [guid]::NewGuid().ToString('n')
    Copy-Item $from $new
    Invoke-Retried {
      # A launcher a window runs holds its file, and Windows lets it be moved
      # but not replaced: it goes aside, runs on, and a later install removes it.
      if (Test-Path $exe) { [IO.File]::Move($exe, "$exe.old-" + [guid]::NewGuid().ToString('n')) }
      [IO.File]::Move($new, $exe)
    }
  }
  foreach ($old in @(Get-ChildItem -Path $bin -Filter 'nodecode.exe.*-*' -ErrorAction SilentlyContinue)) {
    if ($old.Name -match '\.old-' -or $old.LastWriteTime -lt (Get-Date).AddHours(-1)) {
      Remove-Item -LiteralPath $old.FullName -Force -ErrorAction SilentlyContinue
    }
  }
  if (-not (Test-Path $exe)) { return $null }
  $probe = Test-Executable $exe
  if ($probe.Runs) { return $bin }
  Say "the launcher in $bin did not answer ($($probe.Answer)); the PATH names the release's own folder"
  return $null
}

function Test-LaidOut([string]$dir) {
  # A folder holding a release: its executable answering beside its src\.
  $exe = Join-Path $dir 'nodecode.exe'
  if (-not (Test-Path $exe) -or -not (Test-Path (Join-Path $dir 'src'))) { return $false }
  return [bool](Test-Executable $exe).Runs
}

function Find-LaidOut([string]$label) {
  # The release's own folder, else the newest timestamped copy of it an
  # earlier install made beside a folder something ran from. => its path, or
  # nothing when no copy answers.
  $names = @($label) + @(Get-ChildItem -Path $root -Directory -ErrorAction SilentlyContinue |
    Where-Object { $_.Name -match ('^' + [regex]::Escape($label) + '-\d{14}$') } |
    Sort-Object Name -Descending | ForEach-Object { $_.Name })
  foreach ($name in $names) {
    $dir = Join-Path $root $name
    if (Test-LaidOut $dir) { return $dir }
  }
  return $null
}

function Test-InUse([string]$dir) {
  # Something runs from the folder: a process whose image lies under it, or an
  # executable or DLL of it that will not open for writing (Windows holds a
  # running image's file, whichever path - `current` too - started it).
  $base = [IO.Path]::GetFullPath($dir).TrimEnd('\') + '\'
  foreach ($process in @(Get-Process -ErrorAction SilentlyContinue)) {
    $image = $null
    try { $image = $process.Path } catch { }
    if ($image -and $image.StartsWith($base, [StringComparison]::OrdinalIgnoreCase)) { return $true }
  }
  foreach ($file in @(Get-ChildItem -Path $dir -File -ErrorAction SilentlyContinue | Where-Object { $_.Extension -eq '.exe' -or $_.Extension -eq '.dll' })) {
    try { $stream = [IO.File]::Open($file.FullName, 'Open', 'ReadWrite', 'None'); $stream.Close() }
    catch { return $true }
  }
  return $false
}

function Remove-Release([string]$dir) {
  # Moved aside under a name no release has, then removed: a removal that
  # stops partway leaves no half release under a release's name. A folder
  # something runs from stays. => whether it is gone from its name.
  if (Test-InUse $dir) { return $false }
  $aside = $dir.TrimEnd('\') + '.prune-' + [guid]::NewGuid().ToString('n')
  try { [IO.Directory]::Move($dir, $aside) } catch { return $false }
  Remove-Item -Recurse -Force $aside -ErrorAction SilentlyContinue
  return $true
}

function Get-CurrentTarget {
  # The folder `current` names now, before an install or a link moves it.
  $current = Join-Path $root 'current'
  if (-not (Test-Path $current)) { return $null }
  $named = @((Get-Item $current -Force).Target) | Select-Object -First 1
  if (-not $named) { return $null }
  return ("$named" -replace '^\\\\\?\\', '')
}

function Remove-OldReleases([string]$keep, [string]$previous) {
  # Every release folder but the one `current` names and the one it named
  # before is removed, so reinstalls and updates stop piling up 100 MB copies
  # (three installs at once left three). One something runs from is skipped,
  # as is another install's lay-out still in progress; nothing here is an
  # error - the install has already succeeded.
  try {
    $kept = @(@($keep, $previous) | Where-Object { $_ } | ForEach-Object { [IO.Path]::GetFullPath($_).TrimEnd('\') })
    foreach ($dir in @(Get-ChildItem -Path $root -Directory -ErrorAction SilentlyContinue)) {
      if ($dir.Attributes -band [IO.FileAttributes]::ReparsePoint) { continue }
      if ($dir.Name -notmatch '^([0-9a-f]{7,40}(-\d{14})?|\d{14})(\.(new|prune)-[0-9a-f]{32})?$') { continue }
      if ($kept -contains [IO.Path]::GetFullPath($dir.FullName).TrimEnd('\')) { continue }
      if ($dir.Name -match '\.new-' -and $dir.LastWriteTime -gt (Get-Date).AddHours(-1)) { continue }
      if (Remove-Release $dir.FullName) { Say "removed $($dir.Name), an older copy" }
      else { Say "kept $($dir.Name): something runs from it" }
    }
  } catch { Say "left older copies in place: $($_.Exception.Message)" }
}

function Add-Release([bool]$force) {
  # Downloads (or unpacks -FromZip) the release and lays it out under $root.
  # => the folder it lies in.
  $work = Join-Path $env:TEMP ("nodecode-install-" + [guid]::NewGuid().ToString('n'))
  New-Item -ItemType Directory -Force $work | Out-Null
  try {
    $archive = Get-Release $work
    Assert-Checksum $archive
    $payload = $null
    $tarred = [IO.Path]::GetExtension($archive) -eq '.zst'
    if (-not $tarred) {
      $unpacked = Join-Path $work 'unpacked'
      # Not Expand-Archive: it draws its progress box whatever this function's
      # preference says (a module reads the global one), and it is slow.
      Add-Type -AssemblyName System.IO.Compression.FileSystem
      [IO.Compression.ZipFile]::ExtractToDirectory($archive, $unpacked)
      $exe = Get-ChildItem -Path $unpacked -Recurse -Filter nodecode.exe | Select-Object -First 1
      if (-not $exe) { throw "the archive carries no nodecode.exe" }
      $payload = Split-Path -Parent $exe.FullName
    }
    # The directory is the release, as install.sh names it; a local zip has
    # no release name, so it gets the time it landed.
    $stamp = if ($FromZip) { Get-Date -Format 'yyyyMMddHHmmss' } else { $Version }
    $target = Join-Path $root $stamp
    # Laid out under a name of its own and moved into place whole, so neither
    # a concurrent install nor anything reading the folder sees half a release
    # under the release's name.
    New-Item -ItemType Directory -Force $root | Out-Null
    $staging = Join-Path $root ("$stamp.new-" + [guid]::NewGuid().ToString('n'))
    New-Item -ItemType Directory -Force $staging | Out-Null
    try {
      if ($tarred) {
        # bsdtar ships with Windows 10 1803+, and unpacks the tree straight
        # into the folder that becomes the release: 101 MB in about 0.2 s,
        # where the zip is read and then copied into the same place.
        & tar.exe -xf $archive -C $staging
        if ($LASTEXITCODE -ne 0 -or -not (Test-Path (Join-Path $staging 'nodecode.exe'))) {
          throw "the archive did not unpack: tar.exe answered $LASTEXITCODE"
        }
      } else {
        Copy-Item -Recurse -Force (Join-Path $payload '*') $staging
      }
      # Only a file that carries the Mark of the Web is unblocked. tar.exe and
      # .NET's unzip write none, so on a download this touches nothing.
      # Unblock-File over every file - 300-odd - in a PowerShell that an
      # unsigned nodecode.exe started, and then running the new executable,
      # is what Bitdefender's Advanced Threat Defense blocks as
      # SuspiciousBehavior during a self-update, quarantining the running
      # nodecode.exe with it (reproduced on Windows 11 with Antivirus Free).
      Get-ChildItem -Path $staging -Recurse -File |
        Where-Object { Get-Item -LiteralPath $_.FullName -Stream Zone.Identifier -ErrorAction SilentlyContinue } |
        Unblock-File -ErrorAction SilentlyContinue
      $keepable = ($stage -ne 'stage') -and -not $force -and -not $FromZip
      if (Test-Path $target) {
        if ($keepable -and (Test-LaidOut $target)) {
          Say "$Version was laid out at $target meanwhile; keeping it"
          return $target
        }
        if ($stage -eq 'stage') {
          # A staged version is named exactly: `link` and the organism's own
          # label read the folder's name. An earlier staging of it gives way; a
          # copy something still runs from cannot, and says so.
          try { Remove-Item -Recurse -Force $target }
          catch { throw "cannot replace $target, something runs from it: $($_.Exception.Message)" }
        } elseif (-not (Remove-Release $target)) {
          # A running organism keeps every file it started with: a copy it
          # runs from cannot be replaced, so this one lands beside it.
          $stamp = "$stamp-" + (Get-Date -Format 'yyyyMMddHHmmss')
          $target = Join-Path $root $stamp
        }
      }
      try { [IO.Directory]::Move($staging, $target) }
      catch {
        if ($keepable -and (Test-LaidOut $target)) {
          Say "$Version was laid out at $target meanwhile; keeping it"
          return $target
        }
        throw "could not lay $stamp out at ${target}: $($_.Exception.Message)"
      }
      return $target
    } finally {
      if (Test-Path $staging) { Remove-Item -Recurse -Force $staging -ErrorAction SilentlyContinue }
    }
  } finally {
    Remove-Item -Recurse -Force $work -ErrorAction SilentlyContinue
  }
}

function Install-Nodecode {
  # The preferences live here, not at the top of the file: `irm | iex` runs
  # this file in the caller's own session, and a top-level Stop would stay
  # behind in it after the install.
  $ErrorActionPreference = 'Stop'
  $ProgressPreference = 'SilentlyContinue'
  Assert-Platform
  $previous = Get-CurrentTarget
  if ($stage -eq 'link') {
    $target = Join-Path $root $Version
    if (-not (Test-Path (Join-Path $target 'nodecode.exe'))) {
      throw "$Version is not laid out at $target; stage it first"
    }
    Invoke-Retried { Set-Current $target }
    Say "current is $Version"
    $onPath = Set-Launcher $target
    if (-not $onPath) { $onPath = $target }
    if (-not ($NoModifyPath -or $env:NODECODE_NO_MODIFY_PATH) -and (Set-UserPath $onPath)) {
      Say "the user PATH names $onPath"
    }
    Remove-OldReleases $target $previous
    return
  }
  # A label names one build, so a release already laid out here - its
  # executable answering beside its src\ - is that build: installing it again
  # downloaded 100 MB and laid a second copy out beside the first. It is kept
  # and only `current` and the PATH move; -Force (or NODECODE_FORCE=1) lays it
  # out again regardless.
  $force = [bool]$Force -or ($env:NODECODE_FORCE -and $env:NODECODE_FORCE -ne '0')
  $target = $null
  if (-not $FromZip -and -not $force) {
    if ($stage -eq 'stage') {
      $own = Join-Path $root $Version
      if (Test-LaidOut $own) { $target = $own }
    } else { $target = Find-LaidOut $Version }
    if ($target) { Say "$Version is already laid out at $target; keeping it (-Force downloads it again)" }
  }
  if ($stage -eq 'stage') {
    if (-not $target) {
      $target = Add-Release $force
      $probe = Test-Executable (Join-Path $target 'nodecode.exe')
      if (-not $probe.Runs) {
        # Nothing may link a version that cannot run, so none is left lying there.
        Remove-Item -Recurse -Force $target -ErrorAction SilentlyContinue
        throw "the staged executable did not answer: $($probe.Answer)"
      }
    }
    Say "staged $Version at $target"
    return
  }
  if (-not $target) { $target = Add-Release $force }

  Invoke-Retried { Set-Current $target }
  Say "installed $(Split-Path -Leaf $target) at $target"
  # The host a whole install came from is the one its updates read, as
  # install.sh keeps it; the organism's own stage and link never reach here.
  $kept = Join-Path $root 'base-url'
  Invoke-Retried {
    if ($env:NODECODE_BASE_URL) { [IO.File]::WriteAllText($kept, $baseUrl) }
    elseif (Test-Path $kept) { Remove-Item $kept }
  }

  $onPath = Set-Launcher $target
  if (-not $onPath) { $onPath = $target }
  if ($NoModifyPath -or $env:NODECODE_NO_MODIFY_PATH) {
    Say "add $onPath to your PATH to run nodecode"
  } elseif (Set-UserPath $onPath -Add) {
    # Set-UserPath gave this session the folder as well. Run as the one-liner
    # (`irm | iex`, no script path), this session is the operator's own
    # window; run with -File, it is a child whose PATH dies with it.
    if ($PSCommandPath) { Say "your user PATH names $onPath; open a new terminal to pick it up" }
    else { Say "your user PATH names $onPath; nodecode runs in this window now, and in new ones" }
  }

  if (-not (Get-Command git.exe -ErrorAction SilentlyContinue)) {
    # A window opened before Git was installed keeps the PATH it started with.
    if (Test-Path (Join-Path $env:ProgramFiles 'Git\cmd\git.exe')) {
      Say 'Git for Windows is installed but not on this window''s PATH; open a new terminal'
    } else {
      Say 'git is not on PATH; nodecode needs Git for Windows at run time: winget install --id Git.Git -e --source winget'
    }
  }
  $probe = Test-Executable (Join-Path $target 'nodecode.exe')
  if ($probe.Runs) { Say 'the executable runs' } else { Say "the executable answered: $($probe.Answer)" }
  Remove-OldReleases $target $previous
}

# The organism runs `stage` and `link` with -File and says the last line of
# what they print as the reason an update failed: that line is the error's own
# sentence, not the tail of PowerShell's error record. A one-liner runs in the
# caller's own session, where an `exit` would close the window, so it throws.
if ($stage -eq 'full') { Install-Nodecode }
else {
  try { Install-Nodecode }
  catch { [Console]::Error.WriteLine($_.Exception.Message); exit 1 }
}
