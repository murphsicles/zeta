# CI Benchmarking Script
# Runs benchmarks and checks for performance regressions

Write-Host "=== ZETA CI BENCHMARKING ===" -ForegroundColor Cyan

# 1. Check if benchmarks compile
Write-Host "1. Compiling benchmarks..." -ForegroundColor Yellow
cargo bench --no-run
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Benchmark compilation failed" -ForegroundColor Red
    exit 1
}
Write-Host "✅ Benchmarks compiled successfully" -ForegroundColor Green

# 2. Run regression tests
Write-Host "2. Running regression tests..." -ForegroundColor Yellow
cargo run --bin regression_test
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Performance regression detected" -ForegroundColor Red
    Write-Host "To update baseline: cargo run --bin regression_test -- update" -ForegroundColor Yellow
    exit 1
}
Write-Host "✅ No performance regressions" -ForegroundColor Green

# 3. Run quick benchmarks (CI-friendly)
Write-Host "3. Running quick benchmarks..." -ForegroundColor Yellow
$benchOutput = cargo bench -- --warm-up-time 50ms --measurement-time 200ms --sample-size 10 2>&1
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Benchmark execution failed" -ForegroundColor Red
    Write-Host $benchOutput
    exit 1
}

# Extract benchmark results
$results = $benchOutput | Select-String -Pattern "time:" | Select-Object -First 5
if ($results) {
    Write-Host "`nBenchmark Results:" -ForegroundColor Cyan
    foreach ($result in $results) {
        Write-Host "  $($result.Line)" -ForegroundColor White
    }
}

Write-Host "`n=== CI BENCHMARKING COMPLETE ===" -ForegroundColor Cyan
Write-Host "All benchmarks passed!" -ForegroundColor Green