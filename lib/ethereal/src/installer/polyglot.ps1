$os = if ($IsWindows -or $env:OS) {
    "windows"
} elseif ($IsMacOS) { "macos"
} else { "linux" }
$raw = if ($env:PROCESSOR_ARCHITECTURE) {
    $env:PROCESSOR_ARCHITECTURE
} else { "Unknown" }
$arch = switch ($raw) {
    "AMD64"  { "x64" }
    "x86"    { "x86" }
    "ARM64"  { "arm64" }
    default  { $raw.ToLower() }
}
Write-Host "Operating system: $os"
Write-Host "Architecture: $arch"
$scriptPath = $MyInvocation.MyCommand.Definition
$ext = if ($os -eq "windows") { ".exe" } else { "" }
$outputPath = [IO.Path]::ChangeExtension(
    $scriptPath, $ext
)
$lines = [IO.File]::ReadAllLines($scriptPath)
$indexIdx = -1
for ($i = 0; $i -lt $lines.Length; $i++) {
    if ($lines[$i] -like 'index:*') {
        $indexIdx = $i; break
    }
}
if ($indexIdx -lt 0) {
    Write-Host "No index found" -ForegroundColor Red
    exit 1
}
$offsets = @{}
foreach ($entry in $lines[$indexIdx].Substring(6).Split(',')) {
    $k, $v = $entry.Split('=')
    $offsets[$k] = [int]$v
}

function Extract-Payload($name) {
    if (-not $offsets.ContainsKey($name)) { return $null }
    $beginLine = $indexIdx + $offsets[$name]
    $startIdx = $beginLine + 1
    $endIdx = $startIdx
    while ($endIdx -lt $lines.Length -and
           $lines[$endIdx].Trim() -notmatch '^-----END') {
        $endIdx++
    }
    $b64 = (
        $lines[$startIdx..($endIdx - 1)] |
        ForEach-Object { $_.Trim() } |
        Where-Object { $_ -match '^[A-Za-z0-9+/=]+$' }
    ) -join ''
    if (-not $b64) { return $null }
    return [Convert]::FromBase64String($b64)
}

$label = "${os}-${arch}"
$binBytes = Extract-Payload $label
if ($binBytes -eq $null) {
    Write-Host "No payload for $label" -ForegroundColor Red
    exit 1
}
$dataBytes = Extract-Payload "data"
$stream = [IO.File]::Create($outputPath)
$stream.Write($binBytes, 0, $binBytes.Length)
if ($dataBytes -ne $null) {
    $stream.Write($dataBytes, 0, $dataBytes.Length)
}
$stream.Close()
Write-Host "Extracted to $outputPath"
