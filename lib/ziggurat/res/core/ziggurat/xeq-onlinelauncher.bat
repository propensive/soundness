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
if not defined value (echo No runner for !label!>&2 & exit /b 1)
for /f "tokens=1,2 delims=|" %%U in ("!value!") do (set "url=%%U" & set "hash=%%V")
set "t=%TEMP%\~zigdl%RANDOM%.tmp"
call :xeq_msg 33 ████████ 0 "Downloading runner…"
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
call :xeq_msg 33 ████████ 0 "Assembling…"
set "indexnum=0"
set "indexrest="
for /f "usebackq tokens=1* delims=:" %%a in (`findstr /n /b "index:" "%~f0"`) do if !indexnum! equ 0 (set "indexnum=%%a" & set "indexrest=%%b")
set "indexcontent=!indexrest:~6!"
set "data_offset="
for %%E in ("!indexcontent:,=" "!") do (
    for /f "tokens=1,2 delims==" %%K in (%%E) do (if "%%K"=="data" set "data_offset=%%L")
)
if not defined data_offset (echo No embedded data payload>&2 & del "!t!" & exit /b 1)
set "out=%TEMP%\~zigout%RANDOM%.exe"
set "tmp_pem=%TEMP%\~zigpem%RANDOM%.pem"
set "tmp_data=%TEMP%\~zigdat%RANDOM%"
set /a "dskip=indexnum + data_offset - 1"
more +!dskip! "%~f0" > "!tmp_pem!"
certutil -decode "!tmp_pem!" "!tmp_data!" > nul
del "!tmp_pem!"
copy /b "!t!"+"!tmp_data!" "!out!" > nul
del "!t!" "!tmp_data!"
call :xeq_msg 32 ████████ 1 "Assembled"
endlocal & move /y "%out%" "%~f0" >nul & call "%~f0" %*
