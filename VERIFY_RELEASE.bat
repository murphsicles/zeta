@echo off
echo ============================================
echo ZETA v0.5.0 RELEASE VERIFICATION
echo ============================================
echo.

echo Step 1: Testing bootstrap verification...
bootstrap-verification.exe
if %errorlevel% neq 0 (
    echo ❌ FAILED: Bootstrap verification failed (exit code %errorlevel%)
    exit /b 1
)
echo ✅ PASSED: Bootstrap verification successful

echo.
echo Step 2: Testing v0.5.0 compiler...
zetac-0.5.0.exe
if %errorlevel% neq 0 (
    echo ❌ FAILED: v0.5.0 compiler test failed (exit code %errorlevel%)
    exit /b 1
)
echo ✅ PASSED: v0.5.0 compiler works correctly

echo.
echo ============================================
echo ✅ RELEASE VERIFICATION COMPLETE
echo ============================================
echo.
echo v0.5.0 release is ready for production.
echo All tests passed successfully.
echo.
echo Next: Push to GitHub as latest release.
pause
