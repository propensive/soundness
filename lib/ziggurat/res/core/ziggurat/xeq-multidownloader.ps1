$os = if ($IsWindows -or $env:OS) {
    "windows"
} elseif ($IsMacOS) { "macos"
} else { "linux" }
$raw = if ($env:PROCESSOR_ARCHITECTURE) {
    $env:PROCESSOR_ARCHITECTURE
} else {
    [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture.ToString()
}
$arch = switch ($raw) {
    "AMD64"  { "x64" }
    "X64"    { "x64" }
    "x86"    { "x86" }
    "ARM64"  { "arm64" }
    default  { $raw.ToLower() }
}
$label = "${os}-${arch}"
$s = $MyInvocation.MyCommand.Definition
$lines = Get-Content -Path $s
$assets = ($lines -match '^assets:' | Select-Object -First 1) -replace '^assets:'
if (-not $assets) { [Console]::Error.WriteLine("No asset table"); exit 1 }
$row = $assets.Split(',') | Where-Object { $_.StartsWith("$label=") } | Select-Object -First 1
if (-not $row) { [Console]::Error.WriteLine("No binary for $label"); exit 1 }
$value = $row.Substring($label.Length + 1)
$parts = $value.Split('|')
$url = $parts[0]
$hash = $parts[1]
$t = "$s.tmp"
xeq_msg 33 '████████' 0 'Downloading…'
try { Invoke-WebRequest -Uri $url -OutFile $t -UseBasicParsing } catch {
    [Console]::Error.WriteLine("Download failed"); exit 1
}
$size = (Get-Item $t).Length
xeq_msg 32 '████████' 1 "Downloaded $size bytes"
xeq_msg 33 '████████' 0 'Verifying SHA-256…'
$g = (Get-FileHash -Path $t -Algorithm SHA256).Hash
if ($g -ine $hash) {
    [Console]::Error.WriteLine("Hash mismatch"); Remove-Item $t; exit 1
}
xeq_msg 32 '████████' 1 'Verified SHA-256'
Move-Item -Force $t $s
& $s @args
