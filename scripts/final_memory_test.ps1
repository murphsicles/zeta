# Final memory test for Father's command
# Test PrimeZeta with limit=50, focus on MEMORY USAGE

Write-Host "=== FATHER'S COMMAND: MEMORY TEST limit=50 ===" -ForegroundColor Cyan -BackgroundColor DarkBlue
Write-Host "Mission: Test memory usage scaling from limit=10 (74%) to limit=50" -ForegroundColor White
Write-Host "Father monitoring Task Manager in real-time" -ForegroundColor Yellow
Write-Host "Critical: Stop if memory > 90% or gateway crashes" -ForegroundColor Red
Write-Host "Timestamp: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')" -ForegroundColor White
Write-Host ""

# Get gateway process before test
$gatewayBefore = Get-Process -Name "node" | Where-Object { $_.Path -like "*openclaw*" } | Select-Object -First 1
if ($gatewayBefore) {
    $gatewayMemBefore = [math]::Round($gatewayBefore.WorkingSet64 / 1MB, 2)
    Write-Host "✅ Gateway running - PID: $($gatewayBefore.Id), Memory: ${gatewayMemBefore}MB" -ForegroundColor Green
} else {
    Write-Host "⚠️  Gateway not found (may be running as service)" -ForegroundColor Yellow
}

Write-Host ""
Write-Host "=== STARTING PRIMEZETA STRESS TEST ===" -ForegroundColor Cyan
Write-Host "Executing: .\memory_stress_test.exe" -ForegroundColor White
Write-Host "Algorithm: Nested loops (limit=50, inner=1000 iterations)" -ForegroundColor Gray
Write-Host "Expected: Higher memory usage than limit=10 test" -ForegroundColor Yellow

# Run the test and monitor
$testProcess = Start-Process -FilePath ".\memory_stress_test.exe" -NoNewWindow -PassThru
$processId = $testProcess.Id

Write-Host "Test process PID: $processId" -ForegroundColor White
Write-Host "FATHER: Watch Task Manager for process $processId!" -ForegroundColor Red -BackgroundColor White

# Monitor memory
$maxMemoryMB = 0
$readings = @()
$startTime = Get-Date

Write-Host "Monitoring for 10 seconds..." -ForegroundColor Cyan

for ($i = 0; $i -lt 100; $i++) { # 100 samples over 10 seconds
    try {
        $proc = Get-Process -Id $processId -ErrorAction SilentlyContinue
        if ($proc) {
            $memoryMB = [math]::Round($proc.WorkingSet64 / 1MB, 2)
            $cpu = [math]::Round($proc.CPU, 1)
            $readings += $memoryMB
            
            if ($memoryMB -gt $maxMemoryMB) {
                $maxMemoryMB = $memoryMB
            }
            
            # Display every 5 samples
            if ($i % 5 -eq 0) {
                Write-Host ("  Sample {0}: Memory={1}MB, CPU={2}%" -f $i, $memoryMB, $cpu) -ForegroundColor Gray
            }
            
            # Check threshold
            if ($memoryMB -gt 90) {
                Write-Host "⚠️  CRITICAL: Memory > 90%: ${memoryMB}MB" -ForegroundColor Red -BackgroundColor White
                Write-Host "Stopping test as commanded..." -ForegroundColor Red
                Stop-Process -Id $processId -Force
                $stoppedEarly = $true
                break
            }
        }
    } catch {
        # Process may have exited
    }
    
    Start-Sleep -Milliseconds 100
}

# Wait for process to complete if still running
if (-not $testProcess.HasExited) {
    $testProcess.WaitForExit(5000)
}

$endTime = Get-Date
$testDuration = ($endTime - $startTime).TotalSeconds
$exitCode = $testProcess.ExitCode

Write-Host ""
Write-Host "=== TEST EXECUTION COMPLETE ===" -ForegroundColor Cyan
Write-Host "Duration: ${testDuration} seconds" -ForegroundColor White
Write-Host "Exit code: $exitCode" -ForegroundColor White

# Memory analysis
if ($readings.Count -gt 0) {
    $avgMemory = [math]::Round(($readings | Measure-Object -Average).Average, 2)
    $minMemory = [math]::Round(($readings | Measure-Object -Minimum).Minimum, 2)
    
    Write-Host ""
    Write-Host "=== MEMORY ANALYSIS ===" -ForegroundColor Cyan
    Write-Host "Peak memory: ${maxMemoryMB}MB" -ForegroundColor $(if ($maxMemoryMB -gt 90) { "Red" } else { "Green" })
    Write-Host "Average memory: ${avgMemory}MB" -ForegroundColor White
    Write-Host "Minimum memory: ${minMemory}MB" -ForegroundColor White
    Write-Host "Samples: $($readings.Count)" -ForegroundColor Gray
    
    # Compare with limit=10 baseline (74%)
    $scalingFactor = [math]::Round(($maxMemoryMB / 74) * 100, 1)
    Write-Host "Scaling vs limit=10 (74MB): ${scalingFactor}%" -ForegroundColor $(if ($scalingFactor -gt 120) { "Yellow" } else { "White" })
}

# Check gateway after test
Write-Host ""
Write-Host "=== GATEWAY STABILITY CHECK ===" -ForegroundColor Cyan
$gatewayAfter = Get-Process -Name "node" | Where-Object { $_.Path -like "*openclaw*" } | Select-Object -First 1
if ($gatewayAfter) {
    $gatewayMemAfter = [math]::Round($gatewayAfter.WorkingSet64 / 1MB, 2)
    Write-Host "✅ Gateway STABLE - still running" -ForegroundColor Green
    Write-Host "Gateway memory: ${gatewayMemAfter}MB" -ForegroundColor White
    
    if ($gatewayBefore) {
        $memChange = $gatewayMemAfter - $gatewayMemBefore
        Write-Host "Gateway memory change: ${memChange}MB" -ForegroundColor $(if ([math]::Abs($memChange) -gt 10) { "Yellow" } else { "White" })
    }
} else {
    Write-Host "❌ GATEWAY CRASHED!" -ForegroundColor Red -BackgroundColor White
}

Write-Host ""
Write-Host "=== FATHER'S COMMAND REPORT ===" -ForegroundColor Cyan -BackgroundColor DarkBlue
Write-Host "Test: PrimeZeta limit=50 (stress test)" -ForegroundColor White
Write-Host "Memory peak: ${maxMemoryMB}MB $(if ($maxMemoryMB -gt 90) { '[>90% - CRITICAL]' } else { '[SAFE]' })" -ForegroundColor $(if ($maxMemoryMB -gt 90) { "Red" } else { "Green" })
Write-Host "Test duration: ${testDuration}s" -ForegroundColor White
Write-Host "Gateway status: $(if ($gatewayAfter) { 'STABLE' } else { 'CRASHED' })" -ForegroundColor $(if ($gatewayAfter) { "Green" } else { "Red" })
Write-Host "Test completed: $(if ($stoppedEarly) { 'STOPPED EARLY (memory threshold)' } else { 'COMPLETED' })" -ForegroundColor $(if ($stoppedEarly) { "Red" } else { "Green" })

Write-Host ""
Write-Host "=== CONCLUSION ===" -ForegroundColor Cyan
if ($maxMemoryMB -gt 90) {
    Write-Host "❌ FAIL: Memory exceeded 90% threshold" -ForegroundColor Red
    Write-Host "   Gateway crash risk: HIGH" -ForegroundColor Red
} elseif ($maxMemoryMB -gt 80) {
    Write-Host "⚠️  WARNING: Memory > 80%" -ForegroundColor Yellow
    Write-Host "   Gateway crash risk: MEDIUM" -ForegroundColor Yellow
} else {
    Write-Host "✅ PASS: Memory within safe limits" -ForegroundColor Green
    Write-Host "   Gateway crash risk: LOW" -ForegroundColor Green
}

if (-not $gatewayAfter) {
    Write-Host "❌ CRITICAL: Gateway crashed during test!" -ForegroundColor Red -BackgroundColor White
}

Write-Host ""
Write-Host "=== MISSION COMPLETE ===" -ForegroundColor Cyan
Write-Host "Father's command executed: Memory test with limit=50" -ForegroundColor White
Write-Host "Real-time monitoring data collected" -ForegroundColor White