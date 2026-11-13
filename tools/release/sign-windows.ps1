<#
.SYNOPSIS
  Sign nodecode.exe and nodecode-launcher.exe with Foresight Labs'
  certificate, or prove that a signed nodecode.exe still starts.

.DESCRIPTION
  Run by release-windows.ps1 right after the build, so every smoke after it
  runs the bytes a user gets.

  With NODECODE_SIGN=azure (release.yml's Windows job, in the `release'
  environment, which only main and a v* tag reach) both programs are signed
  through Azure Artifact Signing: account foresightlabs, certificate profile
  NODECODE, North Europe, timestamped by Microsoft's RFC 3161 server. The job
  holds no key and no password: GitHub's OIDC token for the job is traded at
  Microsoft Entra for a short-lived access token of the CI identity
  (AZURE_TENANT_ID, AZURE_CLIENT_ID), which may only sign with that profile.
  The signer is jsign (https://github.com/ebourg/jsign) on a Temurin JRE, both
  fetched once into -Tools and checked against pinned SHA-256 sums. Each
  program must then read Valid, signed by CN=Foresight Labs and timestamped,
  and nodecode.exe must answer --version as nodecode, or the release stops.

  Any other run (a pull request's CI, `just windows') signs a copy of
  nodecode.exe with a throwaway self-signed certificate and runs it: the
  signature is appended where SBCL used to look for the image, so this proves
  on every build that the runtime (sbcl-win\signed-core.patch) still finds it.

.PARAMETER Tree
  The folder holding nodecode.exe, nodecode-launcher.exe and the DLLs.

.PARAMETER Tools
  Where the signer is cached between runs.
#>
[CmdletBinding()]
param(
  [Parameter(Mandatory = $true)][string]$Tree,
  [string]$Tools = 'C:\nodecode-build\sign-tools'
)

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'  # Windows PowerShell's bar slows a download tenfold

$exe = Join-Path $Tree 'nodecode.exe'
$launcher = Join-Path $Tree 'nodecode-launcher.exe'

function Assert-Starts([string]$path) {
  $out = (& { $ErrorActionPreference = 'Continue'; & $path --version 2>&1 | Out-String }).Trim()
  if ($LASTEXITCODE -ne 0 -or $out -notmatch '^nodecode \d') {
    throw "signed, $(Split-Path -Leaf $path) does not start as nodecode (exit $LASTEXITCODE): $out"
  }
  Write-Host "sign: signed $(Split-Path -Leaf $path) --version: $out"
}

# --- any other run: a throwaway signature on a copy ---------------------------
if ($env:NODECODE_SIGN -ne 'azure') {
  $copy = Join-Path $Tree 'nodecode-signed-test.exe'
  Copy-Item $exe $copy -Force
  $cert = New-SelfSignedCertificate -Type CodeSigningCert -Subject 'CN=nodecode signing test' `
    -CertStoreLocation Cert:\CurrentUser\My -NotAfter (Get-Date).AddDays(1)
  try {
    $null = Set-AuthenticodeSignature -FilePath $copy -Certificate $cert -HashAlgorithm SHA256
    $sig = Get-AuthenticodeSignature $copy
    if (-not $sig.SignerCertificate -or $sig.SignerCertificate.Thumbprint -ne $cert.Thumbprint) {
      throw "the test signature was not applied: $($sig.Status) $($sig.StatusMessage)"
    }
    # Untrusted by design (a self-signed root), but never HashMismatch
    if ($sig.Status -eq 'HashMismatch') { throw 'the test signature does not match the file' }
    Assert-Starts $copy
  } finally {
    Remove-Item (Join-Path 'Cert:\CurrentUser\My' $cert.Thumbprint) -ErrorAction SilentlyContinue
    Remove-Item $copy -Force -ErrorAction SilentlyContinue
  }
  Write-Host 'sign: unsigned build; a signed copy starts as nodecode'
  return
}

# --- a release: Foresight Labs' signature ------------------------------------
foreach ($name in 'AZURE_TENANT_ID', 'AZURE_CLIENT_ID', 'ACTIONS_ID_TOKEN_REQUEST_URL', 'ACTIONS_ID_TOKEN_REQUEST_TOKEN') {
  if (-not [Environment]::GetEnvironmentVariable($name)) {
    throw "NODECODE_SIGN=azure needs $($name): release.yml's Windows job sets it, in the release environment with id-token: write"
  }
}
$endpoint = 'neu.codesigning.azure.net'
$alias = 'foresightlabs/NODECODE'
$tsa = 'http://timestamp.acs.microsoft.com'

function Get-Pinned([string]$url, [string]$sha256, [string]$file) {
  $path = Join-Path $Tools $file
  if ((Test-Path $path) -and (Get-FileHash $path -Algorithm SHA256).Hash -eq $sha256) { return $path }
  New-Item -ItemType Directory -Force $Tools | Out-Null
  $part = "$path.part"
  Invoke-WebRequest -UseBasicParsing -Uri $url -OutFile $part
  $got = (Get-FileHash $part -Algorithm SHA256).Hash
  if ($got -ne $sha256) {
    Remove-Item $part -Force
    throw "$file has sha256 $got, not the pinned $sha256"
  }
  Move-Item -Force $part $path
  return $path
}

[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
$jsign = Get-Pinned 'https://github.com/ebourg/jsign/releases/download/7.6/jsign-7.6.jar' `
  '6cf58aa080e7ff82656f320cff8d4a3bff30408df8766806dcba3740dd839fa5' 'jsign-7.6.jar'
$jreZip = Get-Pinned 'https://github.com/adoptium/temurin21-binaries/releases/download/jdk-21.0.13%2B9/OpenJDK21U-jre_x64_windows_hotspot_21.0.13_9.zip' `
  '2332c0b4cff6bed722da3567c91d5e47568bcba863fa4945d0a8d6fc76158628' 'OpenJDK21U-jre_x64_windows_hotspot_21.0.13_9.zip'
$java = Join-Path $Tools 'jdk-21.0.13+9-jre\bin\java.exe'
if (-not (Test-Path $java)) {
  Remove-Item -Recurse -Force (Join-Path $Tools 'jdk-21.0.13+9-jre') -ErrorAction SilentlyContinue
  Expand-Archive -Path $jreZip -DestinationPath $Tools
  if (-not (Test-Path $java)) { throw "the JRE zip holds no $java" }
}

# GitHub's token for this job, traded for the CI identity's signing token
$gh = Invoke-RestMethod -Uri "$($env:ACTIONS_ID_TOKEN_REQUEST_URL)&audience=api://AzureADTokenExchange" `
  -Headers @{ Authorization = "bearer $($env:ACTIONS_ID_TOKEN_REQUEST_TOKEN)" }
$entra = Invoke-RestMethod -Method Post -Uri "https://login.microsoftonline.com/$($env:AZURE_TENANT_ID)/oauth2/v2.0/token" -Body @{
  client_id             = $env:AZURE_CLIENT_ID
  scope                 = 'https://codesigning.azure.net/.default'
  grant_type            = 'client_credentials'
  client_assertion_type = 'urn:ietf:params:oauth:client-assertion-type:jwt-bearer'
  client_assertion      = $gh.value
}

# The token reaches jsign through its environment, never its command line
$env:NODECODE_SIGN_TOKEN = $entra.access_token
try {
  & $java -jar $jsign --storetype TRUSTEDSIGNING --keystore $endpoint --storepass env:NODECODE_SIGN_TOKEN `
    --alias $alias --tsaurl $tsa --tsmode RFC3161 --tsretries 3 --tsretrywait 10 $exe $launcher
  if ($LASTEXITCODE -ne 0) { throw "jsign failed ($LASTEXITCODE)" }
} finally {
  Remove-Item Env:\NODECODE_SIGN_TOKEN -ErrorAction SilentlyContinue
}

foreach ($path in $exe, $launcher) {
  $sig = Get-AuthenticodeSignature $path
  $subject = if ($sig.SignerCertificate) { $sig.SignerCertificate.Subject } else { '' }
  if ($sig.Status -ne 'Valid' -or $subject -notmatch '^CN=Foresight Labs,' -or -not $sig.TimeStamperCertificate) {
    throw "$(Split-Path -Leaf $path): signature $($sig.Status), signer '$subject', timestamped $([bool]$sig.TimeStamperCertificate)"
  }
  Write-Host "sign: $(Split-Path -Leaf $path) Valid, $subject, timestamped by $($sig.TimeStamperCertificate.Subject)"
}
Assert-Starts $exe
