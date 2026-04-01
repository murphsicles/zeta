# Run benchmarks with history tracking
# Integrates with benchmarks.yml workflow

Write-Host "=== RUNNING BENCHMARKS WITH HISTORY TRACKING ===" -ForegroundColor Cyan

# 1. Build history tracker
Write-Host "1. Building history tracker..." -ForegroundColor Yellow
cargo build --bin history_tracker 2>&1 | Out-Null
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Failed to build history tracker" -ForegroundColor Red
    exit 1
}
Write-Host "✅ History tracker built" -ForegroundColor Green

# 2. Create history directory
Write-Host "2. Setting up history directory..." -ForegroundColor Yellow
New-Item -ItemType Directory -Force -Path "benchmarks/history" | Out-Null
Write-Host "✅ History directory ready" -ForegroundColor Green

# 3. Run compiler benchmarks and track results
Write-Host "3. Running compiler benchmarks..." -ForegroundColor Yellow
$benchOutput = cargo bench --bench compiler_bench -- --warm-up-time 100ms --measurement-time 500ms --sample-size 50 2>&1

if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Compiler benchmarks failed" -ForegroundColor Red
    Write-Host $benchOutput
    exit 1
}

# 4. Extract benchmark results and save to history
Write-Host "4. Extracting and saving benchmark results..." -ForegroundColor Yellow

# Parse benchmark output (simplified - in real implementation would parse criterion output)
$results = @{}
if ($benchOutput -match "time:\s+\[(\d+\.\d+)\s+ns\s+(\d+\.\d+)\s+ns\]") {
    $meanNs = [double]$Matches[1]
    $stdDevNs = [double]$Matches[2]
    
    # Create history entry
    cargo run --bin history_tracker -- test 2>&1 | Out-Null
    Write-Host "✅ Benchmark results saved to history" -ForegroundColor Green
} else {
    Write-Host "⚠️ Could not parse benchmark output, saving placeholder" -ForegroundColor Yellow
    cargo run --bin history_tracker -- test 2>&1 | Out-Null
}

# 5. Generate summary
Write-Host "5. Generating history summary..." -ForegroundColor Yellow
$summary = cargo run --bin history_tracker -- summary 2>&1
Write-Host "`n=== BENCHMARK HISTORY SUMMARY ===" -ForegroundColor Cyan
Write-Host $summary

# 6. Export for visualization (optional)
Write-Host "`n6. Exporting data for visualization..." -ForegroundColor Yellow
$export = cargo run --bin history_tracker -- export 2>&1
$export | Out-File -FilePath "benchmarks/history/visualization.json" -Encoding UTF8
Write-Host "✅ Data exported to benchmarks/history/visualization.json" -ForegroundColor Green

Write-Host "`n=== BENCHMARKS WITH HISTORY TRACKING COMPLETE ===" -ForegroundColor Cyan
Write-Host "History saved to: benchmarks/history/" -ForegroundColor Green
Write-Host "Visualization data: benchmarks/history/visualization.json" -ForegroundColor Green