# ULTRA-FAST BENCHMARK (1 second)

Write-Host "=== ULTRA-FAST BENCHMARK ==="
Write-Host ""

$passes = 0
$duration = 1.0
$startTime = [System.Diagnostics.Stopwatch]::StartNew()

while ($startTime.Elapsed.TotalSeconds -lt $duration) {
    $process = Start-Process -FilePath ".\prime_simplest_no_break.exe" -NoNewWindow -PassThru -Wait
    $passes++
}

$elapsed = $startTime.Elapsed.TotalSeconds
$passesPerSecond = $passes / $elapsed
$estimated5s = [math]::Round($passesPerSecond * 5)

Write-Host "Results:"
Write-Host "  Time: ${elapsed.ToString('F3')}s"
Write-Host "  Passes: $passes"
Write-Host "  Passes/sec: $([math]::Round($passesPerSecond, 1))"
Write-Host "  Estimated passes in 5s: $estimated5s"
Write-Host ""

if ($estimated5s -gt 100) {
    Write-Host "🎉 COMPETITIVE! (>100 passes/5s)"
} elseif ($estimated5s -gt 50) {
    Write-Host "✅ IMPROVING! (50-100 passes/5s)"
} elseif ($estimated5s -gt 20) {
    Write-Host "⚠️  BETTER (20-50 passes/5s)"
} else {
    Write-Host "❌ UNCOMPETITIVE (<20 passes/5s)"
}