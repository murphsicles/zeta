@echo off
echo Benchmarking optimized algorithm...
echo.

set passes=0
set start_ms=%time:~9,2%%time:~12,2%

:loop
call competition_optimized.exe >nul
set /a passes+=1

set now_ms=%time:~9,2%%time:~12,2%
set /a elapsed=now_ms - start_ms

if %elapsed% lss 20 goto loop  REM 20 centiseconds = 0.2 seconds

echo Results (0.2 second test):
echo   Passes: %passes%
set /a passes_per_sec=passes*100/elapsed
echo   Passes/sec: %passes_per_sec%
set /a estimated_5s=passes_per_sec*5
echo   Estimated passes in 5s: %estimated_5s%
echo.