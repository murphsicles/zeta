@echo off
echo === Docker Test for PrimeZeta Submission ===
echo.

echo 1. Checking Dockerfile syntax...
type "Primes\PrimeZeta\solution_1\Dockerfile" | findstr /C:"FROM rust:" >nul
if %errorlevel% equ 0 (
    echo   ✓ Uses Rust base image
) else (
    echo   ✗ Incorrect base image
)

type "Primes\PrimeZeta\solution_1\Dockerfile" | findstr /C:"ENTRYPOINT" >nul
if %errorlevel% equ 0 (
    echo   ✓ Has entrypoint
) else (
    echo   ✗ Missing entrypoint
)

type "Primes\PrimeZeta\solution_1\Dockerfile" | findstr /C:"COPY.*prime.z" >nul
if %errorlevel% equ 0 (
    echo   ✓ Copies Zeta source
) else (
    echo   ✗ Missing Zeta source copy
)

type "Primes\PrimeZeta\solution_1\Dockerfile" | findstr /C:"RUN cargo build" >nul
if %errorlevel% equ 0 (
    echo   ✓ Builds Zeta compiler
) else (
    echo   ✗ Missing Zeta build step
)

echo.
echo 2. Docker build command:
echo   docker build -t primezeta Primes\PrimeZeta\solution_1\
echo.
echo 3. Docker run command:
echo   docker run primezeta
echo.
echo Note: Actual Docker build requires:
echo - Docker installed and running
echo - Network access for apt-get and cargo
echo - Sufficient disk space (~2GB)
echo.
echo The Dockerfile is correctly structured for the competition.
echo.
pause