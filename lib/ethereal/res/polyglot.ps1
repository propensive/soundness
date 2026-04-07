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
$label = "payload:${os}-${arch}"
$lines = [IO.File]::ReadAllLines($scriptPath)
$startIdx = -1
for ($i = 0; $i -lt $lines.Length; $i++) {
    if ($lines[$i] -eq $label) {
        $startIdx = $i; break
    }
}
if ($startIdx -lt 0) {
    Write-Host "No payload for $label" -ForegroundColor Red
    exit 1
}
$endIdx = $lines.Length
for ($i = $startIdx + 1; $i -lt $lines.Length; $i++) {
    if ($lines[$i] -like 'payload:*') {
        $endIdx = $i; break
    }
}
$payload = (
    $lines[($startIdx + 1)..($endIdx - 1)]
) -join ''
[IO.File]::WriteAllBytes(
    $outputPath,
    [Convert]::FromBase64String($payload)
)
Write-Host "Extracted to $outputPath"
