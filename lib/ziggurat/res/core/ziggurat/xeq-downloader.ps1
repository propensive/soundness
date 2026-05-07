$s = $MyInvocation.MyCommand.Definition
$lines = Get-Content -Path $s
$url = ($lines -match '^# URL=' | Select-Object -First 1) -replace '^# URL='
$hash = ($lines -match '^# HASH=' | Select-Object -First 1) -replace '^# HASH='
$t = "$s.tmp"
xeq_msg 33 '▅▅' 0 'Downloading…'
try { Invoke-WebRequest -Uri $url -OutFile $t -UseBasicParsing } catch {
    [Console]::Error.WriteLine("Download failed"); exit 1
}
$size = (Get-Item $t).Length
xeq_msg 32 '██' 1 "Downloaded $size bytes"
xeq_msg 33 '▅▅' 0 'Verifying SHA-256…'
$g = (Get-FileHash -Path $t -Algorithm SHA256).Hash
if ($g -ne $hash) {
    [Console]::Error.WriteLine("Hash mismatch"); Remove-Item $t; exit 1
}
xeq_msg 32 '██' 1 'Verified SHA-256'
Move-Item -Force $t $s
& $s @args
