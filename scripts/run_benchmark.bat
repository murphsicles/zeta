@echo off
echo Murphy's Sieve Enhanced Benchmark
echo =================================
echo.
echo Testing with Father's 50/50 chance assessment
echo.

set total=0
set passed=0

REM Test each executable
for %%f in (murphy_fixed.exe murphy_safe.exe murphy_sieve_fixed.exe murphy_small.exe simple_murphy.exe) do (
    if exist %%f (
        echo Testing %%f...
        call %%f
        set exit_code=%errorlevel%
        echo   Exit code: !exit_code!
        
        REM Check if exit code is a valid positive number
        if !exit_code! geq 0 (
            set /a total+=1
            
            REM Check against known prime counts
            if !exit_code!==4 (
                echo   ^✓ Valid: matches limit=10 test (4 primes)
                set /a passed+=1
            ) else if !exit_code!==25 (
                echo   ^✓ Valid: matches limit=100 test (25 primes)
                set /a passed+=1
            ) else if !exit_code!==168 (
                echo   ^✓ Valid: matches limit=1000 test (168 primes)
                set /a passed+=1
            ) else if !exit_code!==1229 (
                echo   ^✓ Valid: matches limit=10000 test (1229 primes)
                set /a passed+=1
            ) else (
                echo   ? Unknown value: !exit_code!
            )
        ) else (
            echo   ^✗ Invalid result
        )
        echo.
    ) else (
        echo Skipping %%f - not found
    )
)

echo Benchmark Summary
echo ================
echo Total executables tested: %total%
echo Executables with valid prime counts: %passed%

if %total% gtr 0 (
    set /a success_rate=passed*100/total
    echo Success rate: %success_rate%%%
    echo.
    echo Father's Assessment: 50/50 chance for success
    echo.
    
    if %success_rate% geq 50 (
        echo ✅ EXCEEDS expectations!
        echo Outcome: Complete success (25%% probability)
    ) else if %success_rate% geq 25 (
        echo ⚠️  Meets partial success criteria
        echo Outcome: Partial success (15%% probability)
    ) else if %passed% gtr 0 (
        echo ⚠️  Algorithm works but issues present
        echo Outcome: Algorithm works but heap arrays may fail (10%% probability)
    ) else (
        echo ❌ Below expectations
        echo Outcome: Failure (50%% probability)
    )
)

echo.
echo Enhanced Testing Notes:
echo - Tested existing compiled executables
echo - For full enhanced testing (heap arrays, performance timing,
echo   scale testing to 10000), compilation of new test cases is needed
echo - Workaround for while loop condition bug appears to be implemented
echo   in some versions (if i*i >= limit break pattern)