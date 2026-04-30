#requires -Version 5.1
<#
.SYNOPSIS
  Cross-shell test harness for the Ethereal Windows client.
  Compatible with both Windows PowerShell 5.1 and PowerShell 7.x.

.DESCRIPTION
  Mirrors the test scenarios in lib/ethereal/src/test/ethereal_test.scala
  against a Windows build of an Ethereal-launched test fixture binary. The
  fixture is a standalone @main that responds to the same commands the Linux
  test sandbox implements: echo, args, lines, exit, stderr, sleep, env, pid,
  pwd, cat, and (for the upgrade tests) version.

  Signal-related tests from the Linux suite (SIGUSR1/2/HUP/WINCH and the
  trap-defer / trap-undefined / trap-slow scenarios) do not apply on Windows,
  which only delivers Ctrl+C / Ctrl+Break / console-close events. They are
  intentionally omitted.

.PARAMETER Tool
  Path to the Ethereal-built test fixture executable (.exe). The fixture
  must use the daemon name supplied via -Name (default: abcde).

.PARAMETER ToolV2
  Optional. Path to a second fixture binary built with a higher build id and
  the same daemon name as -Tool. When supplied, the daemon-upgrade and
  self-update suites run; when omitted they are skipped.

.PARAMETER Name
  Daemon name, must match the value used by the fixture's cli call.
  Default: abcde.

.PARAMETER StateRoot
  Directory used as XDG_STATE_HOME during the run. Default:
  $env:TEMP\ethereal-tests-state. The directory is wiped before each run.

.PARAMETER DataRoot
  Directory used as XDG_DATA_HOME during the run (used by self-update).
  Default: $env:TEMP\ethereal-tests-data. Wiped before each run.

.PARAMETER Only
  Optional list of suite names to run (others are skipped).

.PARAMETER Skip
  Optional list of suite or "Suite :: Test" identifiers to skip.

.PARAMETER DefaultTimeoutSec
  Per-test timeout for tool invocations. Default: 30.

.NOTES
  - First invocation may be slow because the launcher downloads its bundled
    JDK into XDG_DATA_HOME. Pre-warm by running ``& $Tool`` once before the
    suite if you want a faster cold start.
  - On Windows the AF_UNIX socket file appears as a regular file / reparse
    point, so the Linux ``test -S`` check is replaced with a simple existence
    probe plus a successful round-trip via the launcher.
#>
[CmdletBinding()]
param(
  [Parameter(Mandatory = $true)] [string] $Tool,
  [string] $ToolV2,
  [string] $Name = 'abcde',
  [string] $StateRoot,
  [string] $DataRoot,
  [string[]] $Only,
  [string[]] $Skip,
  [int] $DefaultTimeoutSec = 30
)

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version 3.0

# ----- preflight -----------------------------------------------------------

if (-not (Test-Path -LiteralPath $Tool)) { throw "Tool not found: $Tool" }
$script:Tool = (Resolve-Path -LiteralPath $Tool).Path
if ($ToolV2) {
  if (-not (Test-Path -LiteralPath $ToolV2)) { throw "ToolV2 not found: $ToolV2" }
  $script:ToolV2 = (Resolve-Path -LiteralPath $ToolV2).Path
} else {
  $script:ToolV2 = $null
}

if (-not $StateRoot) { $StateRoot = Join-Path $env:TEMP 'ethereal-tests-state' }
if (-not $DataRoot)  { $DataRoot  = Join-Path $env:TEMP 'ethereal-tests-data' }

foreach ($d in @($StateRoot, $DataRoot)) {
  if (Test-Path -LiteralPath $d) {
    Remove-Item -LiteralPath $d -Recurse -Force -ErrorAction SilentlyContinue
  }
  New-Item -ItemType Directory -Force -Path $d | Out-Null
}

# state.rs picks the daemon's base dir from XDG_RUNTIME_DIR > XDG_STATE_HOME >
# $HOME/.local/state. Pin our temp dirs and clear XDG_RUNTIME_DIR so a value
# inherited from MSYS / WSL doesn't divert the daemon out of the test sandbox.
$env:XDG_RUNTIME_DIR = ''
$env:XDG_STATE_HOME  = $StateRoot
$env:XDG_DATA_HOME   = $DataRoot

$script:StateDir   = Join-Path $StateRoot $Name
$script:PidFile    = Join-Path $script:StateDir 'pid'
$script:BuildFile  = Join-Path $script:StateDir 'build'
$script:SocketFile = Join-Path $script:StateDir 'socket'
$script:FailFile   = Join-Path $script:StateDir 'fail'

# ----- helpers -------------------------------------------------------------

# Format a single argument per the CreateProcess + msvcrt argv parsing rules.
# Used because PS 5.1's ProcessStartInfo doesn't expose ArgumentList; we
# build the Arguments string ourselves to avoid PowerShell's own argv
# transformations.
function Format-NativeArgument([string] $value) {
  if ($value -eq '') { return '""' }
  if ($value -notmatch '[\s"]') { return $value }
  $sb = New-Object System.Text.StringBuilder
  [void]$sb.Append('"')
  $i = 0
  while ($i -lt $value.Length) {
    $bs = 0
    while ($i -lt $value.Length -and $value[$i] -eq '\') { $bs++; $i++ }
    if ($i -eq $value.Length) {
      [void]$sb.Append('\' * ($bs * 2))
    } elseif ($value[$i] -eq '"') {
      [void]$sb.Append('\' * ($bs * 2 + 1))
      [void]$sb.Append('"')
      $i++
    } else {
      [void]$sb.Append('\' * $bs)
      [void]$sb.Append($value[$i])
      $i++
    }
  }
  [void]$sb.Append('"')
  $sb.ToString()
}

function New-ToolStartInfo {
  param(
    [string] $Exe,
    [string[]] $ToolArgs = @(),
    [hashtable] $EnvOverride,
    [bool] $RedirectStdin = $false
  )
  $psi = New-Object System.Diagnostics.ProcessStartInfo
  $psi.FileName = $Exe
  $psi.UseShellExecute        = $false
  $psi.CreateNoWindow         = $true
  $psi.RedirectStandardOutput = $true
  $psi.RedirectStandardError  = $true
  $psi.RedirectStandardInput  = $RedirectStdin
  # Force UTF-8 reads so daemon output round-trips correctly regardless of
  # the parent console code page (CP437 / 1252 on most Windows installs).
  $psi.StandardOutputEncoding = [System.Text.Encoding]::UTF8
  $psi.StandardErrorEncoding  = [System.Text.Encoding]::UTF8
  if ($ToolArgs.Count -gt 0) {
    $psi.Arguments = (($ToolArgs | ForEach-Object { Format-NativeArgument $_ }) -join ' ')
  }
  if ($EnvOverride) {
    foreach ($k in $EnvOverride.Keys) {
      if ($null -eq $EnvOverride[$k]) {
        [void]$psi.EnvironmentVariables.Remove($k)
      } else {
        $psi.EnvironmentVariables[$k] = [string]$EnvOverride[$k]
      }
    }
  }
  $psi
}

function Invoke-Tool {
  param(
    [string[]] $ToolArgs = @(),
    [string]   $Stdin,
    [hashtable] $EnvOverride,
    [int]      $TimeoutSec = $DefaultTimeoutSec,
    [string]   $Exe        = $script:Tool
  )
  $psi  = New-ToolStartInfo -Exe $Exe -ToolArgs $ToolArgs `
            -EnvOverride $EnvOverride -RedirectStdin:($PSBoundParameters.ContainsKey('Stdin'))
  $proc = New-Object System.Diagnostics.Process
  $proc.StartInfo = $psi
  [void]$proc.Start()

  if ($PSBoundParameters.ContainsKey('Stdin')) {
    # Write bytes directly so we can keep encoding under control on PS 5.1
    # (where StandardInputEncoding is unavailable).
    $bytes = [System.Text.Encoding]::UTF8.GetBytes($Stdin)
    $proc.StandardInput.BaseStream.Write($bytes, 0, $bytes.Length)
    $proc.StandardInput.BaseStream.Flush()
    $proc.StandardInput.Close()
  }

  $stdoutTask = $proc.StandardOutput.ReadToEndAsync()
  $stderrTask = $proc.StandardError.ReadToEndAsync()
  if (-not $proc.WaitForExit($TimeoutSec * 1000)) {
    try { $proc.Kill() } catch { }
    throw "Tool timed out after ${TimeoutSec}s: $Exe $($ToolArgs -join ' ')"
  }
  [pscustomobject]@{
    Stdout   = $stdoutTask.Result
    Stderr   = $stderrTask.Result
    ExitCode = $proc.ExitCode
    Pid      = $proc.Id
  }
}

function Start-Tool {
  param(
    [string[]] $ToolArgs = @(),
    [hashtable] $EnvOverride,
    [string]   $Exe = $script:Tool
  )
  $psi  = New-ToolStartInfo -Exe $Exe -ToolArgs $ToolArgs -EnvOverride $EnvOverride
  $proc = New-Object System.Diagnostics.Process
  $proc.StartInfo = $psi
  [void]$proc.Start()
  $proc
}

function Wait-Tool {
  param(
    [System.Diagnostics.Process] $Process,
    [int] $TimeoutSec = $DefaultTimeoutSec
  )
  $stdoutTask = $Process.StandardOutput.ReadToEndAsync()
  $stderrTask = $Process.StandardError.ReadToEndAsync()
  if (-not $Process.WaitForExit($TimeoutSec * 1000)) {
    try { $Process.Kill() } catch { }
    throw "Process $($Process.Id) timed out"
  }
  [pscustomobject]@{
    Stdout   = $stdoutTask.Result
    Stderr   = $stderrTask.Result
    ExitCode = $Process.ExitCode
    Pid      = $Process.Id
  }
}

function Get-DaemonPid {
  if (-not (Test-Path -LiteralPath $script:PidFile)) { return $null }
  $raw = Get-Content -LiteralPath $script:PidFile -Raw -ErrorAction SilentlyContinue
  if ($null -eq $raw) { return $null }
  $trimmed = $raw.Trim()
  if ($trimmed -match '^\d+$') { return [int]$trimmed }
  $null
}

function Test-ProcessAlive([int] $id) {
  if (-not $id) { return $false }
  try {
    $p = Get-Process -Id $id -ErrorAction Stop
    -not $p.HasExited
  } catch {
    $false
  }
}

function Wait-ProcessGone([int] $id, [int] $TimeoutSec = 10) {
  $deadline = [DateTime]::UtcNow.AddSeconds($TimeoutSec)
  while ((Test-ProcessAlive $id) -and [DateTime]::UtcNow -lt $deadline) {
    Start-Sleep -Milliseconds 100
  }
}

function Stop-DaemonHard {
  $id = Get-DaemonPid
  if ($id) {
    try { Stop-Process -Id $id -Force -ErrorAction SilentlyContinue } catch { }
  }
  Get-Process -Name $Name -ErrorAction SilentlyContinue |
    Stop-Process -Force -ErrorAction SilentlyContinue
  if (Test-Path -LiteralPath $script:StateDir) {
    Get-ChildItem -LiteralPath $script:StateDir -Force -ErrorAction SilentlyContinue |
      Remove-Item -Force -ErrorAction SilentlyContinue
  }
}

# ----- test runner ---------------------------------------------------------

$script:CurrentSuite = ''
$script:Pass         = 0
$script:Fail         = 0
$script:Skipped      = 0
$script:Failures     = New-Object System.Collections.Generic.List[object]

function Suite([string] $name, [scriptblock] $body) {
  $script:CurrentSuite = $name
  if ($Only -and ($Only -notcontains $name)) {
    Write-Host ""
    Write-Host "[$name] (skipped)" -ForegroundColor DarkGray
    return
  }
  Write-Host ""
  Write-Host "[$name]" -ForegroundColor Cyan
  & $body
}

function It {
  param(
    [Parameter(Mandatory)] [string] $Name,
    [Parameter(Mandatory)] [scriptblock] $Body
  )
  $key = "$($script:CurrentSuite) :: $Name"
  if ($Skip -and (($Skip -contains $script:CurrentSuite) -or ($Skip -contains $key))) {
    $script:Skipped++
    Write-Host "  - skip  $Name" -ForegroundColor DarkGray
    return
  }
  try {
    & $Body
    $script:Pass++
    Write-Host "  + pass  $Name" -ForegroundColor Green
  } catch {
    $script:Fail++
    [void]$script:Failures.Add([pscustomobject]@{
      Suite = $script:CurrentSuite
      Name  = $Name
      Error = $_.Exception.Message
    })
    Write-Host "  ! FAIL  $Name" -ForegroundColor Red
    Write-Host "         $($_.Exception.Message)" -ForegroundColor Red
  }
}

function Should-Equal($actual, $expected, [string] $label = 'value') {
  if ($actual -cne $expected) {
    $a = ConvertTo-Json -InputObject $actual   -Compress -Depth 4
    $e = ConvertTo-Json -InputObject $expected -Compress -Depth 4
    throw "expected $label = $e, got $a"
  }
}

function Should-Match($actual, [string] $regex, [string] $label = 'value') {
  if ($actual -notmatch $regex) {
    $a = ConvertTo-Json -InputObject $actual -Compress -Depth 4
    throw "expected $label to match /$regex/, got $a"
  }
}

function Should-Be-True($cond, [string] $msg) {
  if (-not $cond) { throw $msg }
}

# ----- pre-suite cleanup ---------------------------------------------------

Stop-DaemonHard
Start-Sleep -Milliseconds 200

Write-Host "Tool:      $($script:Tool)"
if ($script:ToolV2) { Write-Host "ToolV2:    $($script:ToolV2)" }
Write-Host "StateRoot: $StateRoot"
Write-Host "DataRoot:  $DataRoot"
Write-Host "Daemon:    $Name"

# ===========================================================================
# Test suites (mirroring lib/ethereal/src/test/ethereal_test.scala)
# ===========================================================================

Suite 'Basic invocation' {
  It 'prints expected output on first invocation' {
    $r = Invoke-Tool -ToolArgs 'echo','hello'
    Should-Equal $r.Stdout   'hello' 'stdout'
    Should-Equal $r.ExitCode 0       'exit code'
  }
  It 'reuses the daemon on second invocation' {
    Invoke-Tool -ToolArgs 'echo','hello' | Out-Null
    $pidBefore = Get-DaemonPid
    $r2 = Invoke-Tool -ToolArgs 'echo','world'
    $pidAfter  = Get-DaemonPid
    Should-Equal $r2.Stdout 'world' 'stdout'
    Should-Equal $pidAfter  $pidBefore 'daemon pid'
  }
  It 'returns exit 0 on success (no args)' {
    $r = Invoke-Tool
    Should-Equal $r.ExitCode 0 'exit code'
  }
  It 'forwards exit 42' {
    $r = Invoke-Tool -ToolArgs 'exit','42'
    Should-Equal $r.ExitCode 42 'exit code'
  }
  It 'forwards exit 1' {
    $r = Invoke-Tool -ToolArgs 'exit','1'
    Should-Equal $r.ExitCode 1 'exit code'
  }
}

Suite 'Argument passing' {
  It 'single argument is passed through' {
    Should-Equal (Invoke-Tool -ToolArgs 'args','one').Stdout 'one' 'stdout'
  }
  It 'multiple arguments are passed through' {
    Should-Equal (Invoke-Tool -ToolArgs 'args','one','two','three').Stdout "one`ntwo`nthree" 'stdout'
  }
  It 'argument with spaces is preserved' {
    Should-Equal (Invoke-Tool -ToolArgs 'args','hello world').Stdout 'hello world' 'stdout'
  }
  It 'empty argument is preserved' {
    Should-Equal (Invoke-Tool -ToolArgs 'args','','something').Stdout "`nsomething" 'stdout'
  }
  It 'trailing newline in output is preserved' {
    Should-Equal (Invoke-Tool -ToolArgs 'lines','one','two','three').Stdout "one`ntwo`nthree`n" 'stdout'
  }
}

Suite 'Environment forwarding' {
  It 'environment variable is forwarded' {
    $r = Invoke-Tool -ToolArgs 'env','TEST_ETHEREAL_VAR' `
                     -EnvOverride @{ TEST_ETHEREAL_VAR = 'hello_ethereal' }
    Should-Equal $r.Stdout 'hello_ethereal' 'stdout'
  }
}

Suite 'Working directory' {
  It 'working directory is forwarded' {
    $r = Invoke-Tool -ToolArgs 'pwd'
    Should-Be-True ($r.Stdout.Length -gt 0) 'stdout should not be empty'
  }
}

Suite 'Stderr forwarding' {
  It 'stderr output is forwarded' {
    $r = Invoke-Tool -ToolArgs 'stderr','error message'
    Should-Equal $r.Stderr.Trim() 'error message' 'stderr'
  }
}

Suite 'State file monitoring' {
  It 'daemon restarts after pid file is deleted' {
    Invoke-Tool -ToolArgs 'echo','probe' | Out-Null
    $oldPid = Get-DaemonPid
    Remove-Item -LiteralPath $script:PidFile -Force -ErrorAction SilentlyContinue
    Start-Sleep -Milliseconds 200
    Invoke-Tool -ToolArgs 'echo','probe' | Out-Null
    $newPid = Get-DaemonPid
    Should-Be-True ($newPid -and $newPid -ne $oldPid) "expected new daemon pid (was $oldPid, now $newPid)"
  }
  It 'daemon restarts after socket file is deleted' {
    Invoke-Tool -ToolArgs 'echo','probe' | Out-Null
    $oldPid = Get-DaemonPid
    Remove-Item -LiteralPath $script:SocketFile -Force -ErrorAction SilentlyContinue
    if ($oldPid) { Wait-ProcessGone -id $oldPid -TimeoutSec 10 }
    Remove-Item -LiteralPath $script:FailFile -Force -ErrorAction SilentlyContinue
    Invoke-Tool -ToolArgs 'echo','probe' | Out-Null
    $newPid = Get-DaemonPid
    Should-Be-True ($newPid -and $newPid -ne $oldPid) "expected new daemon pid (was $oldPid, now $newPid)"
  }
}

Suite 'Daemon lifecycle' {
  It 'pid file is present while daemon is running' {
    Invoke-Tool | Out-Null
    Should-Be-True (Test-Path -LiteralPath $script:PidFile) 'pid file should exist'
  }
  It 'recovery after daemon is killed forcefully' {
    Remove-Item -LiteralPath $script:FailFile -Force -ErrorAction SilentlyContinue
    Invoke-Tool -ToolArgs 'echo','probe' | Out-Null
    $oldPid = Get-DaemonPid
    if ($oldPid) {
      Stop-Process -Id $oldPid -Force -ErrorAction SilentlyContinue
      Wait-ProcessGone -id $oldPid -TimeoutSec 5
    }
    Remove-Item -LiteralPath $script:FailFile -Force -ErrorAction SilentlyContinue
    $r = Invoke-Tool -ToolArgs 'echo','recovered'
    Should-Equal $r.Stdout 'recovered' 'stdout'
  }
  It 'fail file is removed after 2 seconds' {
    if (-not (Test-Path -LiteralPath $script:StateDir)) {
      New-Item -ItemType Directory -Force -Path $script:StateDir | Out-Null
    }
    New-Item -ItemType File -Force -Path $script:FailFile | Out-Null
    Remove-Item -LiteralPath $script:PidFile, $script:BuildFile, $script:SocketFile `
                -Force -ErrorAction SilentlyContinue
    Start-Sleep -Milliseconds 2500
    $r = Invoke-Tool -ToolArgs 'echo','after-fail'
    Should-Equal $r.Stdout 'after-fail' 'stdout'
  }
}

Suite 'Concurrent invocations' {
  It 'parallel invocations share the same daemon' {
    Invoke-Tool -ToolArgs 'echo','warmup' | Out-Null   # ensure daemon is up
    $p1 = Start-Tool -ToolArgs 'pid'
    $p2 = Start-Tool -ToolArgs 'pid'
    $p3 = Start-Tool -ToolArgs 'pid'
    $r1 = Wait-Tool $p1
    $r2 = Wait-Tool $p2
    $r3 = Wait-Tool $p3
    $unique = @($r1.Stdout.Trim(), $r2.Stdout.Trim(), $r3.Stdout.Trim()) | Sort-Object -Unique
    Should-Equal $unique.Count 1 'unique daemon-pid count'
  }
  It 'rapid sequential invocations succeed' {
    $results = 1..5 | ForEach-Object { (Invoke-Tool -ToolArgs 'echo',[string]$_).Stdout.Trim() }
    Should-Equal ($results -join ',') ((1..5) -join ',') 'echoed values'
  }
}

Suite 'Forced kill and cleanup' {
  It 'daemon survives launcher being killed' {
    $proc = Start-Tool -ToolArgs 'sleep','30'
    Start-Sleep -Milliseconds 300
    Stop-Process -Id $proc.Id -Force -ErrorAction SilentlyContinue
    Start-Sleep -Milliseconds 300
    $r = Invoke-Tool -ToolArgs 'echo','still-alive'
    Should-Equal $r.Stdout 'still-alive' 'stdout'
  }
  It 'launcher exits when daemon is killed' {
    $proc = Start-Tool -ToolArgs 'sleep','30'
    Start-Sleep -Milliseconds 400
    $daemon = Get-DaemonPid
    if ($daemon) { Stop-Process -Id $daemon -Force -ErrorAction SilentlyContinue }
    if (-not $proc.WaitForExit(5000)) {
      try { Stop-Process -Id $proc.Id -Force -ErrorAction SilentlyContinue } catch { }
      throw 'launcher did not exit after daemon was killed'
    }
    Should-Be-True ($proc.ExitCode -ne 0) "expected nonzero launcher exit, got $($proc.ExitCode)"
  }
  It 'new daemon starts after previous was killed' {
    $r = Invoke-Tool -ToolArgs 'echo','restarted'
    Should-Equal $r.Stdout 'restarted' 'stdout'
  }
}

Suite 'State file integrity' {
  It 'build file contains a single integer build id' {
    Invoke-Tool -ToolArgs 'echo','probe' | Out-Null
    $content = (Get-Content -LiteralPath $script:BuildFile -Raw).Trim()
    Should-Match $content '^\d+$' 'build file contents'
  }
  It 'socket file is present (Windows AF_UNIX shows as a reparse point)' {
    Invoke-Tool -ToolArgs 'echo','probe' | Out-Null
    Should-Be-True (Test-Path -LiteralPath $script:SocketFile) 'socket file should exist'
  }
  It 'pid file contains a valid running PID' {
    Invoke-Tool | Out-Null
    $id = Get-DaemonPid
    Should-Be-True (Test-ProcessAlive $id) "pid in pid file ($id) should be alive"
  }
}

Suite 'Pipe mode' {
  It 'pipe input is forwarded to the application' {
    $r = Invoke-Tool -ToolArgs 'cat' -Stdin "piped input`n"
    Should-Equal $r.Stdout 'piped input' 'stdout'
  }
}

if ($script:ToolV2) {
  Suite 'Daemon upgrade' {
    It 'higher build-id launcher replaces the running daemon' {
      Invoke-Tool -ToolArgs 'version' | Out-Null
      $oldPid = Get-DaemonPid
      $r = Invoke-Tool -Exe $script:ToolV2 -ToolArgs 'version'
      Should-Equal $r.Stdout 'v2' 'stdout from v2'
      $newPid = Get-DaemonPid
      Should-Be-True ($newPid -and $newPid -ne $oldPid) `
        "expected daemon pid to change after upgrade (was $oldPid, now $newPid)"
    }
  }

  Suite 'Self-update' {
    It 'launcher swaps in pending binary before next invocation' {
      Stop-DaemonHard
      Start-Sleep -Milliseconds 200
      $dataNameDir = Join-Path $DataRoot $Name
      New-Item -ItemType Directory -Force -Path $dataNameDir | Out-Null
      # update.rs reads {data_home}/{name}/.pending and renames the running
      # script to {data_home}/{name}.old before re-exec.
      Copy-Item -LiteralPath $script:ToolV2 -Destination (Join-Path $dataNameDir '.pending') -Force
      $r = Invoke-Tool -ToolArgs 'version'
      Should-Equal $r.Stdout 'v2' 'stdout after self-update'
    }
    It 'old binary is preserved after upgrade' {
      $oldPath = Join-Path $DataRoot ($Name + '.old')
      Should-Be-True (Test-Path -LiteralPath $oldPath) "$oldPath should exist"
    }
  }
}

# ===========================================================================
# Summary
# ===========================================================================

Write-Host ""
Write-Host "----------------------------------------"
Write-Host ("Passed: {0}   Failed: {1}   Skipped: {2}" -f $script:Pass, $script:Fail, $script:Skipped)
if ($script:Fail -gt 0) {
  Write-Host ""
  foreach ($f in $script:Failures) {
    Write-Host (" - [{0}] {1}" -f $f.Suite, $f.Name) -ForegroundColor Red
    Write-Host ("   {0}" -f $f.Error) -ForegroundColor Red
  }
  exit 1
}
exit 0
