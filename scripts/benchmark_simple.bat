@echo off
echo Benchmarking competition entry...
echo.

set start=%time%
set passes=0

:loop
REM Run for about 2 seconds
for /f "tokens=1-4 delims=:." %%a in ("%start%") do set /a start_sec=((((%%a*60)+%%b)*60)+%%c)*100+%%d

:inner
call competition_final.exe >nul
set /a passes+=1

for /f "tokens=1-4 delims=:." %%a in ("%time%") do set /a now_sec=((((%%a*60)+%%b)*60)+%%c)*100+%%d
set /a elapsed=(now_sec - start_sec)

if %elapsed% lss 200 goto inner  REM 200 centiseconds = 2 seconds

echo Results:
echo   Time: 2.00 seconds
echo   Passes: %passes%
set /a passes_per_sec=passes*100/elapsed
echo   Passes/sec: %passes_per_sec%
set /a estimated_5s=passes_per_sec*5
echo   Estimated passes in 5s: %estimated_5s%
echo.