# PrimeZeta Compilation Test Runner
# Father's URGENT COMMAND: Test PrimeZeta compilation after fixes

param(
    [string]$ZetaCompiler = ".\target\release\zetac.exe",
    [string]$TestDir = ".\test_suite",
    [string]$PrimeZetaDir = ".\PrimeZeta",
    [switch]$BuildCompiler,
    [switch]$RunAllTests,
    [switch]$Benchmark
)

Write-Host "=== PRIMEZETA COMPILATION TEST RUNNER ===" -ForegroundColor Cyan
Write-Host "Father's URGENT COMMAND: Test PrimeZeta compilation after fixes" -ForegroundColor Yellow
Write-Host "Timestamp: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')" -ForegroundColor White
Write-Host ""

# Check if Zeta compiler exists
if (-not (Test-Path $ZetaCompiler)) {
    Write-Host "❌ Zeta compiler not found at: $ZetaCompiler" -ForegroundColor Red
    Write-Host "   Building compiler first..." -ForegroundColor Yellow
    $BuildCompiler = $true
}

# Build compiler if requested
if ($BuildCompiler) {
    Write-Host "🔨 Building Zeta compiler..." -ForegroundColor Cyan
    $BuildStart = Get-Date
    cargo build --release
    $BuildEnd = Get-Date
    $BuildTime = ($BuildEnd - $BuildStart).TotalSeconds
    Write-Host "✅ Compiler built in $BuildTime seconds" -ForegroundColor Green
    
    if (-not (Test-Path $ZetaCompiler)) {
        Write-Host "❌ Compiler still not found after build!" -ForegroundColor Red
        exit 1
    }
}

# Test 1: Basic compilation test
Write-Host "`n=== TEST 1: BASIC COMPILATION ===" -ForegroundColor Cyan
$Test1File = "$TestDir\simple_test.z"
if (Test-Path $Test1File) {
    Write-Host "Testing: $Test1File" -ForegroundColor White
    $Output = & $ZetaCompiler $Test1File 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ Basic compilation test PASSED" -ForegroundColor Green
        Write-Host "   Output: $($Output | Select-String -Pattern 'Result:')" -ForegroundColor Gray
    } else {
        Write-Host "❌ Basic compilation test FAILED" -ForegroundColor Red
        Write-Host "   Error output:" -ForegroundColor Red
        $Output | ForEach-Object { Write-Host "   $_" -ForegroundColor Red }
    }
} else {
    Write-Host "⚠️  Test file not found: $Test1File" -ForegroundColor Yellow
}

# Test 2: Array return types
Write-Host "`n=== TEST 2: ARRAY RETURN TYPES ===" -ForegroundColor Cyan
$Test2File = "$TestDir\test_array_return.z"
if (Test-Path $Test2File) {
    Write-Host "Testing: $Test2File" -ForegroundColor White
    $Output = & $ZetaCompiler $Test2File 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ Array return types test PASSED" -ForegroundColor Green
        Write-Host "   Output: $($Output | Select-String -Pattern 'Result:')" -ForegroundColor Gray
    } else {
        Write-Host "❌ Array return types test FAILED" -ForegroundColor Red
        # Check for specific error patterns
        if ($Output -match "array") {
            Write-Host "   Array syntax error detected" -ForegroundColor Red
        }
    }
} else {
    Write-Host "⚠️  Test file not found: $Test2File" -ForegroundColor Yellow
}

# Test 3: Bool return types
Write-Host "`n=== TEST 3: BOOL RETURN TYPES ===" -ForegroundColor Cyan
$Test3File = "$TestDir\test_bool_return.z"
if (Test-Path $Test3File) {
    Write-Host "Testing: $Test3File" -ForegroundColor White
    $Output = & $ZetaCompiler $Test3File 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ Bool return types test PASSED" -ForegroundColor Green
        Write-Host "   Output: $($Output | Select-String -Pattern 'Result:')" -ForegroundColor Gray
    } else {
        Write-Host "❌ Bool return types test FAILED" -ForegroundColor Red
        if ($Output -match "bool") {
            Write-Host "   Bool type error detected" -ForegroundColor Red
        }
    }
} else {
    Write-Host "⚠️  Test file not found: $Test3File" -ForegroundColor Yellow
}

# Test 4: Complex functions (is_prime_wheel)
Write-Host "`n=== TEST 4: COMPLEX FUNCTIONS (is_prime_wheel) ===" -ForegroundColor Cyan
$Test4File = "$TestDir\test_is_prime_wheel.z"
if (Test-Path $Test4File) {
    Write-Host "Testing: $Test4File" -ForegroundColor White
    $Output = & $ZetaCompiler $Test4File 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ Complex functions test PASSED" -ForegroundColor Green
        Write-Host "   Output: $($Output | Select-String -Pattern 'Result:')" -ForegroundColor Gray
    } else {
        Write-Host "❌ Complex functions test FAILED" -ForegroundColor Red
        # Check for specific issues
        if ($Output -match "comptime") {
            Write-Host "   Comptime function error" -ForegroundColor Red
        }
        if ($Output -match "while") {
            Write-Host "   While loop error" -ForegroundColor Red
        }
        if ($Output -match "gcd") {
            Write-Host "   Function call error" -ForegroundColor Red
        }
    }
} else {
    Write-Host "⚠️  Test file not found: $Test4File" -ForegroundColor Yellow
}

# Test 5: Full PrimeZeta algorithm
Write-Host "`n=== TEST 5: FULL PRIMEZETA ALGORITHM ===" -ForegroundColor Cyan
$Test5File = "$TestDir\test_primezeta_full.z"
if (Test-Path $Test5File) {
    Write-Host "Testing: $Test5File" -ForegroundColor White
    $Output = & $ZetaCompiler $Test5File 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ Full PrimeZeta algorithm test PASSED" -ForegroundColor Green
        Write-Host "   Output: $($Output | Select-String -Pattern 'Result:')" -ForegroundColor Gray
    } else {
        Write-Host "❌ Full PrimeZeta algorithm test FAILED" -ForegroundColor Red
        # Check for specific issues
        if ($Output -match "dynamic") {
            Write-Host "   Dynamic array error" -ForegroundColor Red
        }
        if ($Output -match "malloc") {
            Write-Host "   Memory allocation error" -ForegroundColor Red
        }
        if ($Output -match "free") {
            Write-Host "   Memory deallocation error" -ForegroundColor Red
        }
    }
} else {
    Write-Host "⚠️  Test file not found: $Test5File" -ForegroundColor Yellow
}

# Test 6: Existing PrimeZeta test suite
Write-Host "`n=== TEST 6: EXISTING PRIMEZETA TEST SUITE ===" -ForegroundColor Cyan
$Test6File = "$PrimeZetaDir\test_suite.z"
if (Test-Path $Test6File) {
    Write-Host "Testing: $Test6File" -ForegroundColor White
    $Output = & $ZetaCompiler $Test6File 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ Existing test suite PASSED" -ForegroundColor Green
        # Extract test results from output
        $TestResults = $Output | Select-String -Pattern "Test \d+:"
        $TestResults | ForEach-Object {
            Write-Host "   $_" -ForegroundColor Gray
        }
        $FinalResult = $Output | Select-String -Pattern "Results:"
        if ($FinalResult) {
            Write-Host "   $FinalResult" -ForegroundColor Gray
        }
    } else {
        Write-Host "❌ Existing test suite FAILED" -ForegroundColor Red
    }
} else {
    Write-Host "⚠️  Test file not found: $Test6File" -ForegroundColor Yellow
}

# Test 7: Minimal PrimeZeta versions
Write-Host "`n=== TEST 7: MINIMAL PRIMEZETA VERSIONS ===" -ForegroundColor Cyan
$MinimalTests = @(
    "$PrimeZetaDir\absolute_minimal.z",
    "$PrimeZetaDir\minimal_working.z",
    "$PrimeZetaDir\prime.z",
    "$PrimeZetaDir\prime_working.z"
)

foreach ($TestFile in $MinimalTests) {
    if (Test-Path $TestFile) {
        $FileName = Split-Path $TestFile -Leaf
        Write-Host "Testing: $FileName" -ForegroundColor White -NoNewline
        $Output = & $ZetaCompiler $TestFile 2>&1
        if ($LASTEXITCODE -eq 0) {
            Write-Host " ✅" -ForegroundColor Green
        } else {
            Write-Host " ❌" -ForegroundColor Red
        }
    }
}

# Benchmark if requested
if ($Benchmark) {
    Write-Host "`n=== BENCHMARK MODE ===" -ForegroundColor Cyan
    Write-Host "Running performance benchmarks..." -ForegroundColor White
    
    # Benchmark 1: Compilation speed
    Write-Host "`nBenchmark 1: Compilation Speed" -ForegroundColor Cyan
    $BenchFiles = @(
        "$TestDir\simple_test.z",
        "$TestDir\test_array_return.z",
        "$TestDir\test_bool_return.z"
    )
    
    foreach ($BenchFile in $BenchFiles) {
        if (Test-Path $BenchFile) {
            $FileName = Split-Path $BenchFile -Leaf
            Write-Host "  ${FileName}: " -ForegroundColor White -NoNewline
            $StartTime = Get-Date
            $Output = & $ZetaCompiler $BenchFile 2>&1 > $null
            $EndTime = Get-Date
            $Elapsed = ($EndTime - $StartTime).TotalMilliseconds
            Write-Host "$Elapsed ms" -ForegroundColor Gray
        }
    }
    
    # Benchmark 2: Code size
    Write-Host "`nBenchmark 2: Generated Code Size" -ForegroundColor Cyan
    Write-Host "  (Not implemented - would require LLVM output analysis)" -ForegroundColor Gray
}

# Summary
Write-Host "`n=== TEST SUMMARY ===" -ForegroundColor Cyan
Write-Host "Zeta Compiler: $ZetaCompiler" -ForegroundColor White
Write-Host "Test Directory: $TestDir" -ForegroundColor White
Write-Host "PrimeZeta Directory: $PrimeZetaDir" -ForegroundColor White
Write-Host "Timestamp: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')" -ForegroundColor White

Write-Host "`n=== NEXT STEPS ===" -ForegroundColor Yellow
Write-Host "1. Run this test after each parser fix" -ForegroundColor White
Write-Host "2. Check which tests pass/fail" -ForegroundColor White
Write-Host "3. Report failures to fix agents" -ForegroundColor White
Write-Host "4. Verify 100% PrimeZeta compilation" -ForegroundColor White

Write-Host "`n=== FATHER'S COMMAND STATUS ===" -ForegroundColor Cyan
Write-Host "Mission: Test PrimeZeta compilation after fixes" -ForegroundColor White
Write-Host "Status: TEST RUNNER DEPLOYED" -ForegroundColor Green
Write-Host "Ready for continuous testing throughout sprint!" -ForegroundColor Green