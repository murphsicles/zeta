# Run memory test for PrimeZeta limit=50
# Father's Command: Monitor memory usage in real-time

Write-Host "=== REAL-TIME MEMORY TEST: PrimeZeta limit=50 ===" -ForegroundColor Cyan
Write-Host "FATHER: Please monitor Task Manager NOW!" -ForegroundColor Red -BackgroundColor White
Write-Host "Previous limit=10: CPU 98%, Memory 74%" -ForegroundColor White
Write-Host "Testing limit=50 - Memory scaling expected" -ForegroundColor Yellow
Write-Host "Critical threshold: 90% memory - will stop if exceeded" -ForegroundColor Red
Write-Host "Timestamp: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')" -ForegroundColor White
Write-Host ""

# Check gateway status before test
Write-Host "=== PRE-TEST GATEWAY CHECK ===" -ForegroundColor Cyan
$gatewayBefore = Get-Process -Name "openclaw-gateway" -ErrorAction SilentlyContinue
if ($gatewayBefore) {
    $gatewayMemBefore = [math]::Round($gatewayBefore.WorkingSet64 / 1MB, 2)
    Write-Host "✅ Gateway running - Memory: ${gatewayMemBefore}MB" -ForegroundColor Green
} else {
    Write-Host "❌ Gateway not found before test!" -ForegroundColor Red
}

Write-Host ""
Write-Host "=== STARTING TEST EXECUTION ===" -ForegroundColor Cyan
Write-Host "Running: .\prime_limit_50.exe" -ForegroundColor White

$testStart = Get-Date
$process = Start-Process -FilePath ".\prime_limit_50.exe" -NoNewWindow -PassThru -RedirectStandardOutput "test_output.txt"

# Monitor memory and CPU in real-time
$maxMemoryMB = 0
$maxCPU = 0
$samples = 0
$memoryReadings = @()

Write-Host "Monitoring process PID: $($process.Id)" -ForegroundColor White
Write-Host "Sampling every 100ms..." -ForegroundColor Gray

while (-not $process.HasExited -and $samples -lt 300) { # Max 30 seconds
    try {
        $procStats = Get-Process -Id $process.Id -ErrorAction SilentlyContinue
        if ($procStats) {
            $memoryMB = [math]::Round($procStats.WorkingSet64 / 1MB, 2)
            $cpu = [math]::Round($procStats.CPU, 1)
            
            $memoryReadings += $memoryMB
            
            if ($memoryMB -gt $maxMemoryMB) {
                $maxMemoryMB = $memoryMB
            }
            if ($cpu -gt $maxCPU) {
                $maxCPU = $cpu
            }
            
            # Display progress
            if ($samples % 10 -eq 0) {
                Write-Host ("Sample {0}: Memory={1}MB, CPU={2}%" -f $samples, $memoryMB, $cpu) -ForegroundColor Gray
            }
            
            # Check for memory threshold
            if ($memoryMB -gt 90) {
                Write-Host "⚠️  CRITICAL: Memory exceeded 90% threshold: ${memoryMB}MB" -ForegroundColor Red -BackgroundColor White
                Write-Host "Stopping test as per Father's command..." -ForegroundColor Red
                Stop-Process -Id $process.Id -Force
                $stoppedEarly = $true
                break
            }
        }
    } catch {
        # Process may have exited
    }
    
    $samples++
    Start-Sleep -Milliseconds 100
}

$testEnd = Get-Date
$testTime = ($testEnd - $testStart).TotalSeconds

# Get exit code
$exitCode = $process.ExitCode

# Read output
$output = ""
if (Test-Path "test_output.txt") {
    $output = Get-Content "test_output.txt" -Raw
    Remove-Item "test_output.txt" -Force
}

Write-Host ""
Write-Host "=== TEST EXECUTION COMPLETE ===" -ForegroundColor Cyan
Write-Host "Execution time: $testTime seconds" -ForegroundColor White
Write-Host "Exit code: $exitCode" -ForegroundColor White
Write-Host "Output: $output" -ForegroundColor Gray

# Memory analysis
if ($memoryReadings.Count -gt 0) {
    $avgMemory = [math]::Round(($memoryReadings | Measure-Object -Average).Average, 2)
    $minMemory = [math]::Round(($memoryReadings | Measure-Object -Minimum).Minimum, 2)
    
    Write-Host ""
    Write-Host "=== MEMORY ANALYSIS ===" -ForegroundColor Cyan
    Write-Host "Maximum memory: ${maxMemoryMB}MB" -ForegroundColor $(if ($maxMemoryMB -gt 90) { "Red" } else { "Green" })
    Write-Host "Average memory: ${avgMemory}MB" -ForegroundColor White
    Write-Host "Minimum memory: ${minMemory}MB" -ForegroundColor White
    Write-Host "CPU peak: ${maxCPU}%" -ForegroundColor White
    Write-Host "Samples collected: $($memoryReadings.Count)" -ForegroundColor Gray
}

# Check gateway status after test
Write-Host ""
Write-Host "=== POST-TEST GATEWAY CHECK ===" -ForegroundColor Cyan
$gatewayAfter = Get-Process -Name "openclaw-gateway" -ErrorAction SilentlyContinue
if ($gatewayAfter) {
    $gatewayMemAfter = [math]::Round($gatewayAfter.WorkingSet64 / 1MB, 2)
    Write-Host "✅ Gateway still running - Memory: ${gatewayMemAfter}MB" -ForegroundColor Green
    
    # Check for memory change
    if ($gatewayBefore) {
        $memChange = $gatewayMemAfter - $gatewayMemBefore
        Write-Host "Gateway memory change: ${memChange}MB" -ForegroundColor $(if ($memChange -gt 50) { "Yellow" } else { "White" })
    }
} else {
    Write-Host "❌ GATEWAY CRASHED DURING TEST!" -ForegroundColor Red -BackgroundColor White
}

Write-Host ""
Write-Host "=== FINAL REPORT FOR FATHER ===" -ForegroundColor Cyan -BackgroundColor DarkBlue
Write-Host "Test: PrimeZeta with limit=50" -ForegroundColor White
Write-Host "Memory peak: ${maxMemoryMB}MB $(if ($maxMemoryMB -gt 90) { '[CRITICAL]' } else { '[SAFE]' })" -ForegroundColor $(if ($maxMemoryMB -gt 90) { "Red" } else { "Green" })
Write-Host "CPU peak: ${maxCPU}%" -ForegroundColor White
Write-Host "Gateway status: $(if ($gatewayAfter) { 'STABLE' } else { 'CRASHED' })" -ForegroundColor $(if ($gatewayAfter) { "Green" } else { "Red" })
Write-Host "Test result: $(if ($exitCode -eq 15) { 'PASS (15 primes)' } else { "FAIL (got $exitCode)" })" -ForegroundColor $(if ($exitCode -eq 15) { "Green" } else { "Red" })
Write-Host "Execution time: ${testTime}s" -ForegroundColor White

if ($stoppedEarly) {
    Write-Host "⚠️  TEST STOPPED EARLY: Memory exceeded 90% threshold" -ForegroundColor Red -BackgroundColor White
}

Write-Host ""
Write-Host "=== FATHER'S COMMAND COMPLETE ===" -ForegroundColor Cyan
Write-Host "Memory test with limit=50 executed and monitored" -ForegroundColor White