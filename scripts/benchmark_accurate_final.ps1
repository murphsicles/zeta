# ACCURATE BENCHMARK

Write-Host "=== ACCURATE BENCHMARK ==="
Write-Host "Running for 2 seconds..."

$passes = 0
$duration = 2.0
$startTime = [System.Diagnostics.Stopwatch]::StartNew()

while ($startTime.Elapsed.TotalSeconds -lt $duration) {
    $process = Start-Process -FilePath ".\prime_ultra_optimized.exe" -NoNewWindow -PassThru -Wait
    $passes++
}

$elapsed = $startTime.Elapsed.TotalSeconds
$passesPerSecond = $passes / $elapsed
$estimated5s = [math]::Round($passesPerSecond * 5)

Write-Host ""
Write-Host "Results:"
Write-Host "  Time: ${elapsed.ToString('F3')}s"
Write-Host "  Passes: $passes"
Write-Host "  Passes/sec: $([math]::Round($passesPerSecond, 1))"
Write-Host "  Estimated passes in 5s: $estimated5s"
Write-Host ""

if ($estimated5s -gt 1000) {
    Write-Host "🎉🎉🎉 **COMPETITIVE! (>1000 passes/5s)** 🎉🎉🎉"
    Write-Host "ZETA DOMINATES!"
} elseif ($estimated5s -gt 500) {
    Write-Host "✅ **RESPECTABLE (500-1000 passes/5s)**"
} elseif ($estimated5s -gt 100) {
    Write-Host "⚠️  **IMPROVING (100-500 passes/5s)**"
} else {
    Write-Host "❌ **UNCOMPETITIVE (<100 passes/5s)**"
}