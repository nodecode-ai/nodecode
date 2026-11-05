<#
.SYNOPSIS
  Lay a commit down for the Windows CI jobs: HEAD from git's objects, with the
  shipped cells cells/bundle.json pins, as tools/release/pack-windows.sh lays
  a release down for the guest.
.DESCRIPTION
  Dot-sourced by ci-windows.ps1 and ci-windows-suite.ps1. The archive is taken
  from git's objects with line endings left alone, as the release's Linux-made
  tar has them: Git for Windows converts the working tree to CRLF, and the
  build must never see that.
#>

# A native command's exit code, checked; its stderr is progress, not an error.
function Run([string]$exe) {
  $ErrorActionPreference = 'Continue'
  & $exe @args
  if ($LASTEXITCODE -ne 0) { throw "$exe $($args -join ' ') failed ($LASTEXITCODE)" }
}

# HEAD of $Repo and its shipped cells, into $Tree; scratch files under $Root.
function New-CommitTree([string]$Repo, [string]$Root, [string]$Tree) {
  New-Item -ItemType Directory -Force $Tree | Out-Null
  $headTar = Join-Path $Root 'head.tar'
  Run git -C $Repo -c core.autocrlf=false -c core.eol=lf archive --format=tar -o $headTar HEAD
  Run tar -xf $headTar -C $Tree

  $cells = Join-Path $Tree 'cells'
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
  Remove-Item -Recurse -Force $fetch, $headTar, $cellTar
  Write-Host "tree: $n shipped cells at $($bundle.commit.Substring(0, 9))"
}
