$s = $MyInvocation.MyCommand.Definition
$lines = Get-Content -Path $s
$url = ($lines -match '^# URL=' | Select-Object -First 1) -replace '^# URL='
$hash = ($lines -match '^# HASH=' | Select-Object -First 1) -replace '^# HASH='
$t = "$s.tmp"
try { Invoke-WebRequest -Uri $url -OutFile $t -UseBasicParsing } catch {
    [Console]::Error.WriteLine("Download failed"); exit 1
}
$g = (Get-FileHash -Path $t -Algorithm SHA256).Hash
if ($g -ne $hash) {
    [Console]::Error.WriteLine("Hash mismatch"); Remove-Item $t; exit 1
}
Move-Item -Force $t $s
& $s @args
