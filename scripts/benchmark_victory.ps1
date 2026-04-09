# VICTORY BENCHMARK!

Write-Host "=== VICTORY BENCHMARK ==="
Write-Host "Algorithm: Optimized trial division (no sqrt, no break)"
Write-Host ""

$passes = 0
$duration = 2  # 2 seconds
$startTime = [System.Diagnostics.Stopwatch]::StartNew()

while ($startTime.Elapsed.TotalSeconds -lt $duration) {
    $process = Start-Process -FilePath ".\prime_final_victory.exe" -NoNewWindow -PassThru -Wait
    $passes++
}

$elapsed = $startTime.Elapsed.TotalSeconds
$passesPerSecond = $passes / $elapsed
$estimated5s = [math]::Round($passesPerSecond * 5)

Write-Host "Results (${elapsed.ToString('F2')}s test):"
Write-Host "  Passes: $passes"
Write-Host "  Passes/sec: $([math]::Round($passesPerSecond))"
Write-Host "  Estimated passes in 5s: $estimated5s"
Write-Host ""

# Compare to previous
Write-Host "=== IMPROVEMENT ==="
Write-Host "Previous (simple): ~5 passes/5s"
Write-Host "New (optimized): ~$estimated5s passes/5s"
Write-Host "Improvement: $([math]::Round($estimated5s / 5))x faster!"
Write-Host ""

if ($estimated5s -gt 100) {
    Write-Host "🎉 COMPETITIVE LEVEL REACHED! (>100 passes/5s)"
} elseif ($estimated5s -gt 50) {
    Write-Host "✅ SIGNIFICANT IMPROVEMENT! (50-100 passes/5s)"
} else {
    Write-Host "⚠️  BETTER BUT STILL LOW (<50 passes/5s)"
}