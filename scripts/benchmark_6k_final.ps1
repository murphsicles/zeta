# FINAL 6k±1 WHEEL BENCHMARK

Write-Host "=== 6k±1 WHEEL FINAL BENCHMARK ==="
Write-Host ""

$passes = 0
$duration = 2
$startTime = [System.Diagnostics.Stopwatch]::StartNew()

while ($startTime.Elapsed.TotalSeconds -lt $duration) {
    $process = Start-Process -FilePath ".\prime_6k_wheel_final.exe" -NoNewWindow -PassThru -Wait
    $passes++
}

$elapsed = $startTime.Elapsed.TotalSeconds
$passesPerSecond = $passes / $elapsed
$estimated5s = [math]::Round($passesPerSecond * 5)

Write-Host "Results:"
Write-Host "  Time: ${elapsed.ToString('F2')}s"
Write-Host "  Passes: $passes"
Write-Host "  Passes/sec: $([math]::Round($passesPerSecond))"
Write-Host "  Estimated passes in 5s: $estimated5s"
Write-Host ""

if ($estimated5s -gt 20) {
    Write-Host "🎉 4x IMPROVEMENT! (~20+ passes/5s)"
} elseif ($estimated5s -gt 10) {
    Write-Host "✅ 2x IMPROVEMENT! (~10-20 passes/5s)"
} else {
    Write-Host "⚠️  MINIMAL IMPROVEMENT (<10 passes/5s)"
}