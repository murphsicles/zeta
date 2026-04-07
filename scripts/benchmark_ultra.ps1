# Benchmark ultra simple algorithm

Write-Host "Benchmarking prime_ultra_simple.exe..."
Write-Host "Algorithm: Basic trial division (odd numbers only)"
Write-Host ""

$passes = 0
$duration = 2  # 2 seconds
$startTime = [System.Diagnostics.Stopwatch]::StartNew()

while ($startTime.Elapsed.TotalSeconds -lt $duration) {
    $process = Start-Process -FilePath ".\prime_ultra_simple.exe" -NoNewWindow -PassThru -Wait
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

if ($estimated5s -gt 1000) {
    Write-Host "✅ COMPETITIVE (>1000 passes/5s)"
} elseif ($estimated5s -gt 100) {
    Write-Host "⚠️  MODERATE (100-1000 passes/5s)"
} else {
    Write-Host "❌ UNACCEPTABLE (<100 passes/5s)"
}