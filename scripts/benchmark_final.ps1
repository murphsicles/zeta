# Benchmark the working competition_final.exe

Write-Host "Benchmarking competition_final.exe (simple trial division)..."
Write-Host ""

$passes = 0
$duration = 2  # Test for 2 seconds
$startTime = [System.Diagnostics.Stopwatch]::StartNew()

while ($startTime.Elapsed.TotalSeconds -lt $duration) {
    $process = Start-Process -FilePath ".\competition_final.exe" -NoNewWindow -PassThru -Wait
    $passes++
}

$elapsed = $startTime.Elapsed.TotalSeconds
$passesPerSecond = $passes / $elapsed
$estimated5s = $passesPerSecond * 5

Write-Host "Results:"
Write-Host "  Time: $elapsed.ToString('F2') seconds"
Write-Host "  Passes: $passes"
Write-Host "  Passes/sec: $passesPerSecond.ToString('F0')"
Write-Host "  Estimated passes in 5s: $estimated5s.ToString('F0')"
Write-Host ""

# Check correctness
$process = Start-Process -FilePath ".\competition_final.exe" -NoNewWindow -PassThru -Wait
$exitCode = $process.ExitCode
Write-Host "Correctness check: Exit code = $exitCode (should be 78498)"
if ($exitCode -eq 78498) {
    Write-Host "  ✅ CORRECT!"
} else {
    Write-Host "  ❌ WRONG!"
}