@echo off
rem ---------------------------------------------------------------------------
rem  cmd.exe smoke test for the Ethereal Windows client.
rem
rem  Verifies that an Ethereal-built fixture binary launches correctly from a
rem  plain cmd.exe shell. This is intentionally narrow: the heavy testing lives
rem  in Run-EtherealTests.ps1. This script just confirms that the launcher,
rem  daemon, and basic stdio plumbing work when invoked from cmd.exe rather
rem  than PowerShell.
rem
rem  Usage:
rem      cmd-smoke.cmd <path-to-tool.exe>
rem ---------------------------------------------------------------------------

setlocal enabledelayedexpansion

if "%~1"=="" (
  echo Usage: %~nx0 ^<path-to-tool.exe^>
  exit /b 1
)

set "TOOL=%~1"
if not exist "%TOOL%" (
  echo Tool not found: %TOOL%
  exit /b 1
)

rem Pin XDG dirs so the daemon's state lives in the test sandbox, mirroring
rem the PowerShell harness. Clear XDG_RUNTIME_DIR so an inherited MSYS/WSL
rem value doesn't divert the daemon out of our temp dir.
set "XDG_RUNTIME_DIR="
set "XDG_STATE_HOME=%TEMP%\ethereal-cmd-state"
set "XDG_DATA_HOME=%TEMP%\ethereal-cmd-data"

if exist "%XDG_STATE_HOME%" rmdir /s /q "%XDG_STATE_HOME%"
if exist "%XDG_DATA_HOME%"  rmdir /s /q "%XDG_DATA_HOME%"
mkdir "%XDG_STATE_HOME%" >nul 2>&1
mkdir "%XDG_DATA_HOME%"  >nul 2>&1

set "FAIL=0"

echo.
echo === cmd.exe smoke tests ===
echo Tool:      %TOOL%
echo StateRoot: %XDG_STATE_HOME%
echo DataRoot:  %XDG_DATA_HOME%
echo.

call :check "launches and exits 0"          0  "hello"      echo hello
call :check "second invocation reuses tool" 0  "again"      echo again
call :check "forwards exit 7"               7  ""           exit 7
call :check "single arg passed through"     0  "one"        args one
call :check "trailing newline preserved"    0  "one"        lines one

set "TEST_ETHEREAL_VAR=value-x"
call :check "env var is forwarded"          0  "value-x"    env TEST_ETHEREAL_VAR
set "TEST_ETHEREAL_VAR="

echo.
if not "%FAIL%"=="0" (
  echo cmd.exe smoke: FAILED
  exit /b 1
)
echo cmd.exe smoke: ok
exit /b 0


rem ---------------------------------------------------------------------------
rem  :check  <label>  <expected-exit>  <expected-stdout>  <tool-args...>
rem
rem  Runs the tool with the given args, captures stdout to a temp file,
rem  compares stdout (line 1) and exit code against expected values.
rem  Pass an empty expected-stdout ("") to skip stdout comparison.
rem ---------------------------------------------------------------------------
:check
set "LABEL=%~1"
set "EXP_EXIT=%~2"
set "EXP_OUT=%~3"
shift & shift & shift

set "TMPOUT=%TEMP%\ethereal-cmd-out-%RANDOM%-%RANDOM%.txt"
"%TOOL%" %1 %2 %3 %4 %5 %6 %7 %8 %9 > "%TMPOUT%" 2>nul
set "GOT_EXIT=%ERRORLEVEL%"

set "GOT_OUT="
if exist "%TMPOUT%" (
  set /p GOT_OUT=<"%TMPOUT%"
)

set "OK=1"
if not "!GOT_EXIT!"=="%EXP_EXIT%" set "OK=0"
if not "%EXP_OUT%"=="" if not "!GOT_OUT!"=="%EXP_OUT%" set "OK=0"

if "!OK!"=="1" (
  echo + pass  !LABEL!
) else (
  echo ! FAIL  !LABEL! ^(exit=!GOT_EXIT! expected=%EXP_EXIT%; stdout="!GOT_OUT!" expected="%EXP_OUT%"^)
  set "FAIL=1"
)

del /q "%TMPOUT%" >nul 2>&1
goto :eof
