$s = $MyInvocation.MyCommand.Definition
$arch = if ($env:PROCESSOR_ARCHITECTURE -eq 'ARM64') { 'arm64' } else { 'x64' }
$label = "windows-$arch"
$lines = Get-Content -Path $s
$assets = ($lines -match '^assets:' | Select-Object -First 1) -replace '^assets:'
$row = ($assets -split ',' | Where-Object { $_.StartsWith("$label=") } | Select-Object -First 1)
if (-not $row) { [Console]::Error.WriteLine("No executable for $label"); exit 1 }
$value = $row.Substring($label.Length + 1)
$url, $hash = $value -split '\|', 2
$exe = [IO.Path]::ChangeExtension($s, 'exe')
$t = "$exe.tmp"
xeq_msg 33 '████████' 0 'Downloading…'
try { Invoke-WebRequest -Uri $url -OutFile $t -UseBasicParsing } catch {
    [Console]::Error.WriteLine("Download failed"); exit 1
}
$size = (Get-Item $t).Length
xeq_msg 32 '████████' 1 "Downloaded $size bytes"
xeq_msg 33 '████████' 0 'Verifying SHA-256…'
$g = (Get-FileHash -Path $t -Algorithm SHA256).Hash
if ($g -ne $hash) {
    [Console]::Error.WriteLine("Hash mismatch"); Remove-Item $t; exit 1
}
xeq_msg 32 '████████' 1 'Verified SHA-256'
Move-Item -Force $t $exe
& $exe @args
$code = $LASTEXITCODE
Remove-Item -Force $s
exit $code
