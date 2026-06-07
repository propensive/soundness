setlocal enabledelayedexpansion
set "arch=x64"
if /i "%PROCESSOR_ARCHITECTURE%"=="ARM64" set "arch=arm64"
if /i "%PROCESSOR_ARCHITECTURE%"=="x86" set "arch=x86"
set "label=windows-!arch!"
set "assets="
for /f "tokens=1* delims=:" %%a in ('findstr /b /c:"assets:" "%~f0"') do if not defined assets set "assets=%%b"
if not defined assets (echo No asset table>&2 & exit /b 1)
set "value="
for %%E in ("!assets:,=" "!") do (
    for /f "tokens=1* delims==" %%K in (%%E) do (
        if "%%K"=="!label!" set "value=%%L"
    )
)
if not defined value (echo No binary for !label!>&2 & exit /b 1)
for /f "tokens=1,2 delims=|" %%U in ("!value!") do (set "url=%%U" & set "hash=%%V")
set "t=%TEMP%\~zigdl%RANDOM%.tmp"
call :xeq_msg 33 ████████ 0 "Downloading…"
where curl >nul 2>&1
if %errorlevel% equ 0 (curl -fsSL "!url!" -o "!t!") else (powershell -NoProfile -Command "Invoke-WebRequest -Uri '!url!' -OutFile '!t!'")
if not exist "!t!" exit /b 1
for %%S in ("!t!") do set "size=%%~zS"
call :xeq_msg 32 ████████ 1 "Downloaded !size! bytes"
call :xeq_msg 33 ████████ 0 "Verifying SHA-256…"
set "g="
for /f "skip=1 tokens=*" %%H in ('certutil -hashfile "!t!" SHA256') do if not defined g set "g=%%H"
set "g=!g: =!"
if /i "!g!" neq "!hash!" (echo Hash mismatch>&2 & del "!t!" & exit /b 1)
call :xeq_msg 32 ████████ 1 "Verified SHA-256"
endlocal & move /y "%t%" "%~f0" >nul & call "%~f0" %*
