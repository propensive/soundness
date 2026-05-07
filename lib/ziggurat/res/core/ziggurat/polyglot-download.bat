for /f "tokens=1* delims==" %%a in ('findstr /b /c:"# URL=" "%~f0"') do set "url=%%b"
for /f "tokens=1* delims==" %%a in ('findstr /b /c:"# HASH=" "%~f0"') do set "hash=%%b"
set "t=%TEMP%\~zigdl%RANDOM%.tmp"
where curl >nul 2>&1
if %errorlevel% equ 0 (curl -fsSL "%url%" -o "%t%") else (powershell -NoProfile -Command "Invoke-WebRequest -Uri '%url%' -OutFile '%t%'")
if not exist "%t%" exit /b 1
set "g="
for /f "skip=1 tokens=*" %%H in ('certutil -hashfile "%t%" SHA256') do if not defined g set "g=%%H"
set "g=%g: =%"
if /i "%g%" neq "%hash%" (echo Hash mismatch>&2 & del "%t%" & exit /b 1)
move /y "%t%" "%~f0" >nul
call "%~f0" %*
