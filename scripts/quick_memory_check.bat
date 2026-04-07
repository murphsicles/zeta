@echo off
echo === FATHER'S MEMORY TEST: PrimeZeta limit=50 ===
echo.
echo Starting test at %time%
echo.

echo Running memory_stress_test.exe...
echo FATHER: Watch Task Manager NOW!
echo.

REM Run the test
memory_stress_test.exe

echo.
echo Test exit code: %errorlevel%
echo Test completed at %time%
echo.

REM Check if gateway is still responding
echo Checking gateway status...
openclaw gateway status

echo.
echo === TEST SUMMARY ===
echo Test: PrimeZeta limit=50 (stress test)
echo Memory focus: Testing scaling from limit=10
echo Gateway status: Check above output
echo.