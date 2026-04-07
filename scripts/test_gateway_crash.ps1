# Test script to monitor gateway during PrimeZeta execution

Write-Host "Starting gateway crash test..."
Write-Host "Current time: $(Get-Date)"
Write-Host ""

# Check gateway status
Write-Host "1. Checking gateway status..."
$gatewayStatus = openclaw gateway status
Write-Host "Gateway status:"
$gatewayStatus
Write-Host ""

# Get gateway PID
$gatewayPid = 12956
Write-Host "2. Gateway PID: $gatewayPid"

# Monitor memory before test
Write-Host "3. Memory before test:"
$beforeMem = Get-Process -Id $gatewayPid -ErrorAction SilentlyContinue | Select-Object WorkingSet64, PrivateMemorySize64
$beforeMem
Write-Host ""

# Try to run a simple test (not full PrimeZeta)
Write-Host "4. Running simple test..."
Write-Host "   This is a placeholder for PrimeZeta test"
Write-Host "   Actual test would run: cargo test test_primezeta_simple"
Write-Host ""

# Simulate some work
Start-Sleep -Seconds 5

# Monitor memory during test
Write-Host "5. Memory during test:"
$duringMem = Get-Process -Id $gatewayPid -ErrorAction SilentlyContinue | Select-Object WorkingSet64, PrivateMemorySize64
$duringMem
Write-Host ""

# Check if gateway is still running
Write-Host "6. Checking if gateway is still alive..."
$gatewayProcess = Get-Process -Id $gatewayPid -ErrorAction SilentlyContinue
if ($gatewayProcess) {
    Write-Host "   Gateway is STILL RUNNING (PID: $gatewayPid)"
} else {
    Write-Host "   GATEWAY CRASHED!"
}

Write-Host ""
Write-Host "Test completed at: $(Get-Date)"