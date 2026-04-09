@echo off
echo === Building PrimeZeta Submission ===
echo.

echo 1. Checking directory structure...
if exist "Primes\PrimeZeta\solution_1\src\prime.z" (
    echo   ✓ prime.z
) else (
    echo   ✗ prime.z
)

if exist "Primes\PrimeZeta\solution_1\src\prime_benchmark.rs" (
    echo   ✓ prime_benchmark.rs
) else (
    echo   ✗ prime_benchmark.rs
)

if exist "Primes\PrimeZeta\solution_1\README.md" (
    echo   ✓ README.md
) else (
    echo   ✗ README.md
)

if exist "Primes\PrimeZeta\solution_1\Dockerfile" (
    echo   ✓ Dockerfile
) else (
    echo   ✗ Dockerfile
)

if exist "Primes\PrimeZeta\solution_1\Cargo.toml" (
    echo   ✓ Cargo.toml
) else (
    echo   ✗ Cargo.toml
)

echo.
echo 2. Checking file sizes...
for %%f in ("Primes\PrimeZeta\solution_1\src\prime.z") do (
    echo   prime.z: %%~zf bytes
)

echo.
echo 3. Sample content check...
findstr /C:"fn murphy_sieve" "Primes\PrimeZeta\solution_1\src\prime.z" >nul
if %errorlevel% equ 0 (
    echo   ✓ Contains murphy_sieve function
) else (
    echo   ✗ Missing murphy_sieve function
)

findstr /C:"78498" "Primes\PrimeZeta\solution_1\src\prime.z" >nul
if %errorlevel% equ 0 (
    echo   ✓ Contains correct prime count
) else (
    echo   ✗ Missing correct prime count
)

echo.
echo 4. README check...
findstr /C:"zeta solution by murphsicles" "Primes\PrimeZeta\solution_1\README.md" >nul
if %errorlevel% equ 0 (
    echo   ✓ Correct header
) else (
    echo   ✗ Incorrect header
)

echo.
echo === Submission Package Complete ===
echo.
echo The submission package is ready for Plummers Prime Drag Race!
echo.
echo Contents:
echo - Murphy's Sieve implementation in Zeta (prime.z)
echo - Benchmark runner in Rust (prime_benchmark.rs)
echo - Dockerfile for containerized execution
echo - README with algorithm badges
echo - Cargo.toml for Rust dependencies
echo.
echo To submit:
echo 1. Create GitHub repository
echo 2. Push all files from Primes\PrimeZeta\solution_1\
echo 3. Submit repository URL to competition
echo.
pause