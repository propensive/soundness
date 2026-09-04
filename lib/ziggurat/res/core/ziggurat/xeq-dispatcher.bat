set "label=windows-x64"
if /i "%PROCESSOR_ARCHITECTURE%"=="ARM64" set "label=windows-arm64"
set "assets="
for /f "delims=" %%a in ('findstr /b /c:"assets:" "%~f0"') do if not defined assets set "assets=%%a"
set "assets=%assets:assets:=%"
set "row="
for %%r in (%assets%) do echo(%%r|findstr /b /c:"%label%=" >nul 2>&1 && set "row=%%r"
if not defined row (echo No executable for %label%>&2 & exit /b 1)
set "row=%row:*==%"
for /f "tokens=1,2 delims=|" %%u in ("%row%") do (set "url=%%u" & set "hash=%%v")
set "exe=%~dpn0.exe"
set "t=%TEMP%\~zigdp%RANDOM%.tmp"
call :xeq_msg 33 ████████ 0 "Downloading…"
where curl >nul 2>&1
if %errorlevel% equ 0 (curl -fsSL "%url%" -o "%t%") else (powershell -NoProfile -Command "Invoke-WebRequest -Uri '%url%' -OutFile '%t%'")
if not exist "%t%" exit /b 1
for %%S in ("%t%") do set "size=%%~zS"
call :xeq_msg 32 ████████ 1 "Downloaded %size% bytes"
call :xeq_msg 33 ████████ 0 "Verifying SHA-256…"
set "g="
for /f "skip=1 tokens=*" %%H in ('certutil -hashfile "%t%" SHA256') do if not defined g set "g=%%H"
set "g=%g: =%"
if /i "%g%" neq "%hash%" (echo Hash mismatch>&2 & del "%t%" & exit /b 1)
call :xeq_msg 32 ████████ 1 "Verified SHA-256"
move /y "%t%" "%exe%" >nul
"%exe%" %*
set "code=%errorlevel%"
del "%~f0" 2>nul
exit /b %code%
