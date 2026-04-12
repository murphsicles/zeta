# Benchmark competition entry
param(
    [string]$ExePath = ".\final_submission.exe",
    [int]$DurationSeconds = 5
)

Write-Host "=== COMPETITION BENCHMARK ==="
Write-Host "Duration: ${DurationSeconds}s"
Write-Host ""

# Start process with output redirection
$processInfo = New-Object System.Diagnostics.ProcessStartInfo
$processInfo.FileName = $ExePath
$processInfo.RedirectStandardOutput = $true
$processInfo.UseShellExecute = $false
$processInfo.CreateNoWindow = $true

$process = New-Object System.Diagnostics.Process
$process.StartInfo = $processInfo

# Start timer
$startTime = Get-Date
$lineCount = 0
$outputLines = New-Object System.Collections.ArrayList

# Event handler for output
$outputHandler = {
    $line = $event.SourceEventArgs.Data
    if ($line) {
        $outputLines.Add($line) | Out-Null
        $lineCount++
        Write-Host "Line $lineCount : $line" -ForegroundColor Cyan
    }
}

# Register event
Register-ObjectEvent -InputObject $process -EventName OutputDataReceived -Action $outputHandler | Out-Null

# Start process
$process.Start() | Out-Null
$process.BeginOutputReadLine()

Write-Host "Benchmark running..." -ForegroundColor Yellow

# Wait for duration
Start-Sleep -Seconds $DurationSeconds

# Kill process
Write-Host "Stopping benchmark..." -ForegroundColor Yellow
$process.Kill($true)
$process.WaitForExit()

# Unregister event
Get-EventSubscriber | Where-Object {$_.SourceObject -eq $process} | Unregister-Event

$endTime = Get-Date
$elapsed = $endTime - $startTime

Write-Host ""
Write-Host "=== BENCHMARK RESULTS ==="
Write-Host "Total lines: $lineCount"
Write-Host "Elapsed time: $($elapsed.TotalSeconds.ToString('F2'))s"
Write-Host "Passes per second: $(($lineCount / $elapsed.TotalSeconds).ToString('F2'))"
Write-Host "Estimated competition passes (5s): $(($lineCount / $elapsed.TotalSeconds * 5).ToString('F0'))"

# Verify output correctness
if ($outputLines.Count -gt 0) {
    $firstValue = $outputLines[0]
    Write-Host "First output value: $firstValue"
    
    # Check if all outputs are 78498
    $allCorrect = $true
    foreach ($line in $outputLines) {
        if ($line -ne "78498") {
            Write-Host "ERROR: Wrong output '$line'" -ForegroundColor Red
            $allCorrect = $false
            break
        }
    }
    
    if ($allCorrect) {
        Write-Host "✓ All outputs correct (78498)" -ForegroundColor Green
    }
}

# Cleanup
Get-Process -Name ($ExePath -replace '.*\\', '' -replace '\..*', '') -ErrorAction SilentlyContinue | 
    Stop-Process -Force -ErrorAction SilentlyContinue

Write-Host "=== BENCHMARK COMPLETE ==="