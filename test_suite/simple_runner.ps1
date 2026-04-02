# Simple PrimeZeta Test Runner
Write-Host "=== PRIMEZETA COMPILATION TEST RUNNER ===" -ForegroundColor Cyan
Write-Host "Testing Zeta compiler with PrimeZeta features" -ForegroundColor White
Write-Host ""

$ZetaCompiler = ".\target\release\zetac.exe"

# Test 1: Simple test
Write-Host "Test 1: Simple compilation" -ForegroundColor Yellow
$Test1 = ".\test_suite\simple_test.z"
if (Test-Path $Test1) {
    & $ZetaCompiler $Test1
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ PASSED" -ForegroundColor Green
    } else {
        Write-Host "❌ FAILED" -ForegroundColor Red
    }
}

Write-Host ""

# Test 2: Array return
Write-Host "Test 2: Array return types" -ForegroundColor Yellow
$Test2 = ".\test_suite\test_array_return.z"
if (Test-Path $Test2) {
    & $ZetaCompiler $Test2
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ PASSED" -ForegroundColor Green
    } else {
        Write-Host "❌ FAILED" -ForegroundColor Red
    }
}

Write-Host ""

# Test 3: Bool return
Write-Host "Test 3: Bool return types" -ForegroundColor Yellow
$Test3 = ".\test_suite\test_bool_return.z"
if (Test-Path $Test3) {
    & $ZetaCompiler $Test3
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ PASSED" -ForegroundColor Green
    } else {
        Write-Host "❌ FAILED" -ForegroundColor Red
    }
}

Write-Host ""

# Test 4: Existing PrimeZeta test suite
Write-Host "Test 4: Existing PrimeZeta test suite" -ForegroundColor Yellow
$Test4 = ".\PrimeZeta\test_suite.z"
if (Test-Path $Test4) {
    & $ZetaCompiler $Test4
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ PASSED" -ForegroundColor Green
    } else {
        Write-Host "❌ FAILED" -ForegroundColor Red
    }
}

Write-Host ""
Write-Host "=== TEST COMPLETE ===" -ForegroundColor Cyan