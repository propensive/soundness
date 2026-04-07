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

$reader = [IO.StreamReader]::new($scriptPath)
$indexLineNum = 0
$indexContent = $null
while ($null -ne ($line = $reader.ReadLine())) {
    $indexLineNum++
    if ($line.StartsWith('index:')) {
        $indexContent = $line.Substring(6)
        break
    }
}
$reader.Close()

if ($null -eq $indexContent) {
    Write-Host "No index found" -ForegroundColor Red
    exit 1
}
$offsets = @{}
foreach ($entry in $indexContent.Split(',')) {
    $k, $v = $entry.Split('=')
    $offsets[$k] = [int]$v
}

$label = "${os}-${arch}"
if (-not $offsets.ContainsKey($label)) {
    Write-Host "No payload for $label" -ForegroundColor Red
    exit 1
}

$tmp = Join-Path $env:TEMP "~ethereal_$PID"

if ($os -eq "windows") {
    $skip = $indexLineNum + $offsets[$label] - 1
    & cmd /c "more +$skip `"$scriptPath`" > `"${tmp}.pem`""
    & certutil -decode "${tmp}.pem" "${tmp}.bin" > $null
    Remove-Item "${tmp}.pem"
    if ($offsets.ContainsKey("data")) {
        $dskip = $indexLineNum + $offsets["data"] - 1
        & cmd /c "more +$dskip `"$scriptPath`" > `"${tmp}.pem`""
        & certutil -decode "${tmp}.pem" "${tmp}.dat" > $null
        Remove-Item "${tmp}.pem"
        & cmd /c "copy /b `"${tmp}.bin`"+`"${tmp}.dat`" `"$outputPath`" > nul"
        Remove-Item "${tmp}.bin", "${tmp}.dat"
    } else {
        Move-Item "${tmp}.bin" $outputPath -Force
    }
} else {
    $skip = $indexLineNum + $offsets[$label]
    $decode = "{ base64 -d 2>/dev/null || base64 -D; }"
    & bash -c "tail -n +$($skip + 1) '$scriptPath' | sed -n '/^-----END/q; /^-----BEGIN/d; p' | eval $decode > '$outputPath'"
    if ($offsets.ContainsKey("data")) {
        $dskip = $indexLineNum + $offsets["data"]
        & bash -c "tail -n +$($dskip + 1) '$scriptPath' | sed -n '/^-----END/q; /^-----BEGIN/d; p' | eval $decode >> '$outputPath'"
    }
    & chmod +x $outputPath
}
Write-Host "Extracted to $outputPath"
