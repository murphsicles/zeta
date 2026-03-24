@echo off
REM get_compiler.cmd - Download Zeta v0.4.1 compiler
echo Downloading Zeta v0.4.1 compiler...
echo URL: https://github.com/murphsicles/zeta/releases/download/v0.4.1/zetac.exe

REM Try multiple methods
echo.
echo Method 1: Using curl...
curl -L -o zetac_v0.4.1.exe https://github.com/murphsicles/zeta/releases/download/v0.4.1/zetac.exe --silent

if exist zetac_v0.4.1.exe (
    echo ✅ Download successful!
    for %%F in (zetac_v0.4.1.exe) do echo File size: %%~zF bytes
    echo.
    echo Testing compiler...
    zetac_v0.4.1.exe --version
    echo.
    copy zetac_v0.4.1.exe zetac.exe /Y
    echo ✅ Copied as zetac.exe
    exit /b 0
) else (
    echo ❌ curl failed
)

echo.
echo Method 2: Using bitsadmin...
bitsadmin /transfer zetaDownload /download /priority normal "https://github.com/murphsicles/zeta/releases/download/v0.4.1/zetac.exe" "zetac_v0.4.1.exe"

if exist zetac_v0.4.1.exe (
    echo ✅ Download successful!
    for %%F in (zetac_v0.4.1.exe) do echo File size: %%~zF bytes
    echo.
    echo Testing compiler...
    zetac_v0.4.1.exe --version
    echo.
    copy zetac_v0.4.1.exe zetac.exe /Y
    echo ✅ Copied as zetac.exe
    exit /b 0
) else (
    echo ❌ All download methods failed
    echo Please manually download from:
    echo https://github.com/murphsicles/zeta/releases/download/v0.4.1/zetac.exe
    exit /b 1
)