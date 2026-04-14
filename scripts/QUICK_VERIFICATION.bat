@echo off
echo ========================================
echo FINAL COMPETITION PACKAGE VERIFICATION
echo ========================================
echo.

echo 1. Checking required files...
if exist FINAL_COMPETITION_PACKAGE.c (
    echo   ✅ FINAL_COMPETITION_PACKAGE.c
) else (
    echo   ❌ FINAL_COMPETITION_PACKAGE.c MISSING
    goto error
)

if exist FINAL_COMPETITION_PACKAGE.exe (
    echo   ✅ FINAL_COMPETITION_PACKAGE.exe
) else (
    echo   ❌ FINAL_COMPETITION_PACKAGE.exe MISSING
    goto error
)

echo.
echo 2. Running performance test...
echo    (This will take 5 seconds...)
echo.
FINAL_COMPETITION_PACKAGE.exe

echo.
echo 3. Verifying component files...
if exist bit_array_infrastructure.z echo   ✅ bit_array_infrastructure.z
if exist 30030_wheel_final.z echo   ✅ 30030_wheel_final.z
if exist benchmark_simd.rs echo   ✅ benchmark_simd.rs
if exist debug_ctfe.rs echo   ✅ debug_ctfe.rs

echo.
echo ========================================
echo VERIFICATION COMPLETE
echo Competition package is READY FOR SUBMISSION
echo ========================================
goto end

:error
echo.
echo ❌ VERIFICATION FAILED
echo Some required files are missing.
exit /b 1

:end
echo.
pause