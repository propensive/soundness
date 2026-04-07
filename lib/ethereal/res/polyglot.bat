setlocal enabledelayedexpansion
echo Operating system: %OS%
echo Architecture: %PROCESSOR_ARCHITECTURE%
set "output=%~dpn0.exe"
set "arch=x64"
if /i "%PROCESSOR_ARCHITECTURE%"=="ARM64" set "arch=arm64"
if /i "%PROCESSOR_ARCHITECTURE%"=="x86" set "arch=x86"
set "label=payload:windows-!arch!"
set "script=%~f0"
set "start=0"
set "end=0"
set "found=0"
for /f "usebackq tokens=1* delims=:" %%a in (
    `findstr /n /b "payload:" "!script!"`
) do (
    if !found! equ 1 if !end! equ 0 set "end=%%a"
    if "%%b"=="!label!" if !found! equ 0 (
        set "start=%%a"
        set "found=1"
    )
)
if !start! equ 0 (
    echo No payload for windows-!arch!
    exit /b 1
)
if !end! gtr 0 (
    set /a "count=end - start - 1"
) else (
    set "count=999999"
)
set "b64=%TEMP%\~ethereal_%RANDOM%.b64"
set "n=0"
(echo -----BEGIN CERTIFICATE-----
for /f "usebackq delims=" %%L in (
    `more +!start! "!script!"`
) do (
    if !n! lss !count! (
        echo %%L
        set /a "n+=1"
    )
)
echo -----END CERTIFICATE-----)> "!b64!"
certutil -decode "!b64!" "!output!" > nul
del "!b64!"
if not exist "!output!" (
    echo Extraction failed for windows-!arch!
    exit /b 1
)
echo Extracted to !output!
endlocal
