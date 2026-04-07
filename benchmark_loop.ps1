# Quick benchmark to estimate passes per 5 seconds
$startTime = Get-Date
$passes = 0
$timeout = 2  # Test for 2 seconds instead of 5

Write-Host "Benchmarking competition entry for $timeout seconds..."

while ((Get-Date) - $startTime).TotalSeconds -lt $timeout {
    # Run our compiled program
    $null = .\competition_final.exe
    $passes++
}

$elapsed = (Get-Date) - $startTime
$passesPerSecond = $passes / $elapsed.TotalSeconds
$estimatedPasses5s = $passesPerSecond * 5

Write-Host "Results:"
Write-Host "  Time: $($elapsed.TotalSeconds.ToString('F2')) seconds"
Write-Host "  Passes: $passes"
Write-Host "  Passes/sec: $($passesPerSecond.ToString('F0'))"
Write-Host "  Estimated passes in 5s: $($estimatedPasses5s.ToString('F0'))"