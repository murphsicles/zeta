# Memory test for PrimeZeta with limit=50
# Father's Command: Monitor memory usage during test execution

Write-Host "=== MEMORY TEST: PrimeZeta limit=50 ===" -ForegroundColor Cyan
Write-Host "Father's Command: Test memory usage with limit=50" -ForegroundColor Yellow
Write-Host "Previous limit=10 results: CPU 98%, Memory 74%" -ForegroundColor White
Write-Host "Expected: Memory may approach/exceed 90% based on scaling" -ForegroundColor Yellow
Write-Host "Critical: Stop if memory exceeds 90% or gateway shows crash signs" -ForegroundColor Red
Write-Host "Timestamp: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')" -ForegroundColor White
Write-Host ""

# Step 1: Compile the test file
Write-Host "Step 1: Compiling prime_limit_50.z..." -ForegroundColor Cyan
$compileStart = Get-Date
& .\target\release\zetac.exe prime_limit_50.z
$compileEnd = Get-Date
$compileTime = ($compileEnd - $compileStart).TotalSeconds
Write-Host "Compilation completed in $compileTime seconds" -ForegroundColor White

# Check if executable was created
if (Test-Path "prime_limit_50.exe") {
    Write-Host "✅ Executable created: prime_limit_50.exe" -ForegroundColor Green
    
    # Step 2: Run the executable with memory monitoring
    Write-Host ""
    Write-Host "Step 2: Running executable with memory monitoring..." -ForegroundColor Cyan
    Write-Host "FATHER: Please monitor Task Manager NOW!" -ForegroundColor Red -BackgroundColor White
    Write-Host "Watching for: CPU peak, Memory peak, Gateway stability" -ForegroundColor Yellow
    
    $runStart = Get-Date
    $process = Start-Process -FilePath ".\prime_limit_50.exe" -NoNewWindow -PassThru
    
    # Monitor process while it runs
    $maxMemoryMB = 0
    $maxCPU = 0
    $startTime = Get-Date
    
    while (-not $process.HasExited) {
        try {
            $procStats = Get-Process -Id $process.Id -ErrorAction SilentlyContinue
            if ($procStats) {
                $memoryMB = [math]::Round($procStats.WorkingSet64 / 1MB, 2)
                $cpu = $procStats.CPU
                
                if ($memoryMB -gt $maxMemoryMB) {
                    $maxMemoryMB = $memoryMB
                }
                if ($cpu -gt $maxCPU) {
                    $maxCPU = $cpu
                }
                
                # Check for memory threshold
                if ($memoryMB -gt 90) {
                    Write-Host "⚠️  WARNING: Memory exceeded 90% threshold: ${memoryMB}MB" -ForegroundColor Red
                    Write-Host "Stopping test as per Father's command..." -ForegroundColor Red
                    Stop-Process -Id $process.Id -Force
                    break
                }
            }
        } catch {
            # Process may have exited
        }
        
        Start-Sleep -Milliseconds 100
    }
    
    $runEnd = Get-Date
    $runTime = ($runEnd - $runStart).TotalSeconds
    
    # Get exit code
    $exitCode = $process.ExitCode
    
    Write-Host ""
    Write-Host "=== TEST RESULTS ===" -ForegroundColor Cyan
    Write-Host "Execution time: $runTime seconds" -ForegroundColor White
    Write-Host "Exit code: $exitCode" -ForegroundColor White
    Write-Host "Maximum memory used: ${maxMemoryMB}MB" -ForegroundColor $(if ($maxMemoryMB -gt 90) { "Red" } else { "Green" })
    Write-Host "Maximum CPU: ${maxCPU}%" -ForegroundColor White
    
    # Expected result: 15 primes up to 50
    if ($exitCode -eq 15) {
        Write-Host "✅ CORRECT RESULT: Found 15 primes (as expected)" -ForegroundColor Green
    } else {
        Write-Host "❌ UNEXPECTED RESULT: Got $exitCode, expected 15" -ForegroundColor Red
    }
    
    # Gateway stability check
    Write-Host ""
    Write-Host "=== GATEWAY STABILITY CHECK ===" -ForegroundColor Cyan
    $gatewayProcess = Get-Process -Name "openclaw-gateway" -ErrorAction SilentlyContinue
    if ($gatewayProcess) {
        Write-Host "✅ Gateway is still running" -ForegroundColor Green
        $gatewayMemory = [math]::Round($gatewayProcess.WorkingSet64 / 1MB, 2)
        Write-Host "Gateway memory usage: ${gatewayMemory}MB" -ForegroundColor White
    } else {
        Write-Host "❌ Gateway appears to have crashed!" -ForegroundColor Red
    }
    
} else {
    Write-Host "❌ Compilation failed - no executable created" -ForegroundColor Red
}

Write-Host ""
Write-Host "=== TEST COMPLETE ===" -ForegroundColor Cyan
Write-Host "Father's monitoring complete. Report:" -ForegroundColor White
Write-Host "1. Memory peak: ${maxMemoryMB}MB" -ForegroundColor $(if ($maxMemoryMB -gt 90) { "Red" } else { "White" })
Write-Host "2. CPU peak: ${maxCPU}%" -ForegroundColor White
Write-Host "3. Gateway status: $(if ($gatewayProcess) { 'STABLE' } else { 'CRASHED' })" -ForegroundColor $(if ($gatewayProcess) { "Green" } else { "Red" })
Write-Host "4. Test result: $(if ($exitCode -eq 15) { 'PASS' } else { 'FAIL' })" -ForegroundColor $(if ($exitCode -eq 15) { "Green" } else { "Red" })