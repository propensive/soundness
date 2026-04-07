setlocal enabledelayedexpansion
echo Operating system: %OS%
echo Architecture: %PROCESSOR_ARCHITECTURE%
set "output=%~dpn0.exe"
set "arch=x64"
if /i "%PROCESSOR_ARCHITECTURE%"=="ARM64" set "arch=arm64"
if /i "%PROCESSOR_ARCHITECTURE%"=="x86" set "arch=x86"
set "script=%~f0"
set "indexnum=0"
set "indexrest="
for /f "usebackq tokens=1* delims=:" %%a in (
    `findstr /n /b "index:" "!script!"`
) do if !indexnum! equ 0 (
    set "indexnum=%%a"
    set "indexrest=%%b"
)
if !indexnum! equ 0 (
    echo No index found
    exit /b 1
)
set "indexcontent=!indexrest:~6!"
set "offset="
set "data_offset="
for %%E in ("!indexcontent:,=" "!") do (
    for /f "tokens=1,2 delims==" %%K in (%%E) do (
        if "%%K"=="windows-!arch!" set "offset=%%L"
        if "%%K"=="data" set "data_offset=%%L"
    )
)
if not defined offset (
    echo No payload for windows-!arch!
    exit /b 1
)
set "tmp_bin=%TEMP%\~ethereal_%RANDOM%_bin"
set "tmp_pem=%TEMP%\~ethereal_%RANDOM%.pem"
set /a "skip=indexnum + offset - 1"
more +!skip! "!script!" > "!tmp_pem!"
certutil -decode "!tmp_pem!" "!tmp_bin!" > nul
del "!tmp_pem!"
if not exist "!tmp_bin!" (
    echo Extraction failed for windows-!arch!
    exit /b 1
)
if defined data_offset (
    set "tmp_data=%TEMP%\~ethereal_%RANDOM%_data"
    set /a "dskip=indexnum + data_offset - 1"
    more +!dskip! "!script!" > "!tmp_pem!"
    certutil -decode "!tmp_pem!" "!tmp_data!" > nul
    del "!tmp_pem!"
    if exist "!tmp_data!" (
        copy /b "!tmp_bin!"+"!tmp_data!" "!output!" > nul
        del "!tmp_bin!" "!tmp_data!"
    ) else (
        move /y "!tmp_bin!" "!output!" > nul
    )
) else (
    move /y "!tmp_bin!" "!output!" > nul
)
echo Extracted to !output!
endlocal
