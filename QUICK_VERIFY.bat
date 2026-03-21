@echo off
echo ============================================
echo ZETA v0.5.0 QUICK VERIFICATION (zetac-0.5.0)
echo ============================================
echo.

echo Testing bootstrap verification...
bootstrap-verification.exe
if %errorlevel% neq 0 goto error
echo ✅ Bootstrap verification passed

echo.
echo Testing zetac-0.5.0 compiler...
zetac-0.5.0.exe
if %errorlevel% neq 0 goto error
echo ✅ zetac-0.5.0 compiler passed

echo.
echo ============================================
echo ✅ v0.5.0 RELEASE VERIFIED (zetac-0.5.0)
echo ============================================
echo.
echo Compiler name: zetac-0.5.0
echo Version: v0.5.0
echo Status: Ready for GitHub release
echo.
goto end

:error
echo.
echo ❌ VERIFICATION FAILED (Exit code: %errorlevel%)
exit /b 1

:end
pause