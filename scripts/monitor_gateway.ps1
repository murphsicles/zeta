$gatewayPid = 12956
$monitorDuration = 30  # seconds
$interval = 1  # seconds

Write-Host "Monitoring gateway PID $gatewayPid for $monitorDuration seconds..."
Write-Host "Time,WorkingSet(MB),PrivateMemory(MB),CPU(%)"

for ($i = 0; $i -lt $monitorDuration; $i++) {
    $process = Get-Process -Id $gatewayPid -ErrorAction SilentlyContinue
    if ($process) {
        $workingSetMB = [math]::Round($process.WorkingSet64 / 1MB, 2)
        $privateMB = [math]::Round($process.PrivateMemorySize64 / 1MB, 2)
        $cpu = $process.CPU
        Write-Host "$i,$workingSetMB,$privateMB,$cpu"
    } else {
        Write-Host "$i,GATEWAY CRASHED"
        break
    }
    Start-Sleep -Seconds $interval
}

Write-Host "Monitoring complete."