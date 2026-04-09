# Simulate a long-running process to see if gateway crashes

$gatewayPid = 12956
Write-Host "Starting long-running test..."
Write-Host "Gateway PID: $gatewayPid"
Write-Host ""

# Function to check gateway
function Check-Gateway {
    param($iteration)
    $process = Get-Process -Id $gatewayPid -ErrorAction SilentlyContinue
    if ($process) {
        $workingSetMB = [math]::Round($process.WorkingSet64 / 1MB, 2)
        $privateMB = [math]::Round($process.PrivateMemorySize64 / 1MB, 2)
        Write-Host "[Iteration $iteration] Gateway ALIVE - Memory: ${workingSetMB}MB (WS), ${privateMB}MB (PM)"
        return $true
    } else {
        Write-Host "[Iteration $iteration] Gateway CRASHED!" -ForegroundColor Red
        return $false
    }
}

# Run for 2 minutes (120 seconds) with checks every 10 seconds
$maxIterations = 12
$iteration = 0

while ($iteration -lt $maxIterations) {
    $iteration++
    Write-Host ""
    Write-Host "=== Iteration $iteration of $maxIterations ==="
    
    if (-not (Check-Gateway $iteration)) {
        break
    }
    
    # Simulate some work (like PrimeZeta might do)
    Write-Host "   Simulating work..."
    
    # Do some CPU-intensive work
    $sum = 0
    1..100000 | ForEach-Object { $sum += $_ }
    
    # Allocate some memory
    $largeArray = @()
    1..10000 | ForEach-Object { $largeArray += "x" * 100 }
    
    # Sleep for 10 seconds
    Write-Host "   Sleeping for 10 seconds..."
    Start-Sleep -Seconds 10
}

Write-Host ""
Write-Host "Test completed."
if (Get-Process -Id $gatewayPid -ErrorAction SilentlyContinue) {
    Write-Host "Gateway is STILL RUNNING after $iteration iterations." -ForegroundColor Green
} else {
    Write-Host "Gateway CRASHED during test." -ForegroundColor Red
}