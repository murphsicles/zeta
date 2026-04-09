# Run PrimeZeta limit=1000 test and measure performance
# Father is monitoring Task Manager in real-time

Write-Host "=== PRIMEZETA LIMIT=1000 TEST ===" -ForegroundColor Cyan
Write-Host "Father's Queued Command: Test with limit=1000" -ForegroundColor Yellow
Write-Host "Time: $(Get-Date -Format 'HH:mm:ss')" -ForegroundColor White
Write-Host "Hypothesis: Larger limits use FEWER resources" -ForegroundColor Green
Write-Host "Previous data: limit=500 → CPU 37%, Memory 31-33%" -ForegroundColor Gray
Write-Host ""

# Compile the test
Write-Host "Compiling prime_limit_1000.z..." -ForegroundColor Cyan
$compileStart = Get-Date
& target\release\zetac.exe prime_limit_1000.z -o prime_limit_1000.exe 2>&1 | Out-Null
$compileTime = (Get-Date) - $compileStart
Write-Host "Compilation time: $($compileTime.TotalMilliseconds)ms" -ForegroundColor Gray

# Check if compilation succeeded
if (Test-Path prime_limit_1000.exe) {
    Write-Host "✅ Compilation successful" -ForegroundColor Green
    
    # Run the test multiple times
    Write-Host ""
    Write-Host "Running prime_limit_1000.exe..." -ForegroundColor Cyan
    Write-Host "Father: Please monitor Task Manager NOW!" -ForegroundColor Yellow
    Write-Host ""
    
    $runTimes = @()
    $iterations = 3
    
    for ($i = 1; $i -le $iterations; $i++) {
        Write-Host "Run $i/$iterations..." -ForegroundColor White
        $runStart = Get-Date
        & .\prime_limit_1000.exe
        $runTime = (Get-Date) - $runStart
        $runTimes += $runTime.TotalMilliseconds
        Write-Host "  Execution time: $($runTime.TotalMilliseconds)ms" -ForegroundColor Gray
        
        if ($i -lt $iterations) {
            Write-Host "  Pausing 1 second..." -ForegroundColor DarkGray
            Start-Sleep -Seconds 1
        }
    }
    
    # Calculate statistics
    $avgTime = ($runTimes | Measure-Object -Average).Average
    $minTime = ($runTimes | Measure-Object -Minimum).Minimum
    $maxTime = ($runTimes | Measure-Object -Maximum).Maximum
    
    Write-Host ""
    Write-Host "=== TEST RESULTS ===" -ForegroundColor Cyan
    Write-Host "Executable: prime_limit_1000.exe" -ForegroundColor White
    Write-Host "Algorithm: Naive prime checking up to limit=1000" -ForegroundColor Gray
    Write-Host "Expected primes: 168 (primes ≤ 1000)" -ForegroundColor Gray
    Write-Host ""
    Write-Host "Execution times:" -ForegroundColor White
    Write-Host "  Minimum: {0:F2}ms" -f $minTime
    Write-Host "  Maximum: {0:F2}ms" -f $maxTime
    Write-Host "  Average: {0:F2}ms" -f $avgTime
    Write-Host ""
    Write-Host "=== HYPOTHESIS TEST ===" -ForegroundColor Cyan
    Write-Host "Based on Father's monitoring data:" -ForegroundColor White
    Write-Host "  limit=10: CPU 98%, Memory 74%" -ForegroundColor Gray
    Write-Host "  limit=500: CPU 37%, Memory 31-33%" -ForegroundColor Gray
    Write-Host "  limit=1000: Expected SIMILAR or BETTER resource usage" -ForegroundColor Green
    Write-Host ""
    Write-Host "Father's observation needed:" -ForegroundColor Yellow
    Write-Host "  CPU peak during execution?" -ForegroundColor White
    Write-Host "  Memory pattern (steady/increase)?" -ForegroundColor White
    Write-Host "  Does limit=1000 continue the counter-intuitive pattern?" -ForegroundColor White
    
    # Save results
    $results = @{
        timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
        test = "prime_limit_1000"
        limit = 1000
        expected_primes = 168
        compile_time_ms = $compileTime.TotalMilliseconds
        execution_times_ms = $runTimes
        avg_execution_time_ms = $avgTime
        min_execution_time_ms = $minTime
        max_execution_time_ms = $maxTime
        hypothesis = "Larger limits use FEWER resources"
        previous_data = @{
            limit_10 = @{cpu_percent = 98; memory_percent = 74}
            limit_500 = @{cpu_percent = 37; memory_percent = "31-33"}
        }
    }
    
    $results | ConvertTo-Json | Out-File -FilePath "limit_1000_results.json" -Encoding UTF8
    Write-Host ""
    Write-Host "Results saved to: limit_1000_results.json" -ForegroundColor Gray
    
} else {
    Write-Host "❌ Compilation failed" -ForegroundColor Red
}

Write-Host ""
Write-Host "=== TEST COMPLETE ===" -ForegroundColor Cyan
Write-Host "Father: Please share your Task Manager observations!" -ForegroundColor Yellow