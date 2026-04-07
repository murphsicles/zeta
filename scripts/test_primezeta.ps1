# PrimeZeta Compilation Test - Main Entry Point
# Father's URGENT COMMAND: Test PrimeZeta compilation after fixes

param(
    [switch]$Quick,
    [switch]$Full,
    [switch]$Benchmark,
    [string]$TestFile
)

Write-Host "=== PRIMEZETA COMPILATION TEST ===" -ForegroundColor Cyan
Write-Host "Father's Command: 'Test PrimeZeta compilation after fixes'" -ForegroundColor Yellow
Write-Host "Timestamp: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')" -ForegroundColor White
Write-Host ""

# Default to quick test if no options specified
if (-not ($Quick -or $Full -or $Benchmark -or $TestFile)) {
    $Quick = $true
}

if ($TestFile) {
    Write-Host "Testing specific file: $TestFile" -ForegroundColor Cyan
    if (Test-Path $TestFile) {
        & .\target\release\zetac.exe $TestFile
    } else {
        Write-Host "❌ File not found: $TestFile" -ForegroundColor Red
    }
    exit
}

if ($Quick) {
    Write-Host "Running QUICK test suite..." -ForegroundColor Cyan
    & powershell -ExecutionPolicy Bypass -File .\test_suite\simple_runner.ps1
}

if ($Full) {
    Write-Host "Running FULL test suite..." -ForegroundColor Cyan
    & powershell -ExecutionPolicy Bypass -File .\test_suite\test_current.ps1
}

if ($Benchmark) {
    Write-Host "Running BENCHMARK tests..." -ForegroundColor Cyan
    Write-Host "Benchmark mode not fully implemented yet" -ForegroundColor Yellow
    Write-Host "Use -Quick or -Full for now" -ForegroundColor White
}

Write-Host ""
Write-Host "=== TEST COMPLETE ===" -ForegroundColor Cyan
Write-Host "Next: Run tests after each parser fix delivery" -ForegroundColor White
Write-Host "Command: .\test_primezeta.ps1 -Quick" -ForegroundColor Gray
Write-Host "Full status: .\test_primezeta.ps1 -Full" -ForegroundColor Gray