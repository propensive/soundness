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
Write-Host "Operating system: $os"
Write-Host "Architecture: $arch"
$scriptPath = $MyInvocation.MyCommand.Definition
$dir = [IO.Path]::GetDirectoryName($scriptPath)
$name = [IO.Path]::GetFileNameWithoutExtension($scriptPath)
if ($os -eq "windows") {
    $outputPath = [IO.Path]::Combine($dir, "$name.exe")
} else {
    $outputPath = [IO.Path]::Combine($dir, $name)
}

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

$tmpDir = if ($env:TEMP) { $env:TEMP } else { "/tmp" }
$tmp = Join-Path $tmpDir "~ethereal_$PID"

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
    $decode = '{ base64 -d 2>/dev/null || base64 -D; }'
    & bash -c "tail -n +$($skip + 1) '$scriptPath' | sed -n '/^-----END/q; /^-----BEGIN/d; p' | $decode > '$outputPath'" 2>/dev/null
    if ($offsets.ContainsKey("data")) {
        $dskip = $indexLineNum + $offsets["data"]
        & bash -c "tail -n +$($dskip + 1) '$scriptPath' | sed -n '/^-----END/q; /^-----BEGIN/d; p' | $decode >> '$outputPath'" 2>/dev/null
    }
    & chmod +x $outputPath
}
Remove-Item $scriptPath
Write-Host "Extracted to $outputPath"

$exeName = [IO.Path]::GetFileNameWithoutExtension($outputPath)
$marker = "# $exeName tab-completions"
$completer = @"
$marker
Register-ArgumentCompleter -Native -CommandName '$exeName' -ScriptBlock {
    param(`$wordToComplete, `$commandAst, `$cursorPosition)
    & '$outputPath' --completions "`$commandAst" `$cursorPosition |
    ForEach-Object {
        `$parts = `$_ -split "`t", 2
        `$name = `$parts[0]
        `$desc = if (`$parts.Length -gt 1) { `$parts[1] } else { `$name }
        [System.Management.Automation.CompletionResult]::new(
            `$name, `$name, 'ParameterValue', `$desc
        )
    }
}
"@

$profilePath = $PROFILE
$profileDir = Split-Path $profilePath
if (-not (Test-Path $profileDir)) {
    New-Item -ItemType Directory -Path $profileDir -Force | Out-Null
    Write-Host "Created profile directory: $profileDir"
}
if (Test-Path $profilePath) {
    $existing = Get-Content $profilePath -Raw
} else {
    $existing = ""
}
if ($existing -match [regex]::Escape($marker)) {
    $pattern = "(?ms)$([regex]::Escape($marker)).*?(?=\r?\n# |\z)"
    $updated = $existing -replace $pattern, $completer
    Set-Content $profilePath $updated -NoNewline
    Write-Host "Updated tab completions for '$exeName' in $profilePath"
} else {
    Add-Content $profilePath "`n$completer"
    Write-Host "Added tab completions for '$exeName' to $profilePath"
}

& $outputPath $args
