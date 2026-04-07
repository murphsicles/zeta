# Setup Heartbeat Monitor Scheduled Task
# Creates a Windows Task Scheduler task to run heartbeat monitor every 15 minutes

$TaskName = "OpenClaw-Heartbeat-Monitor"
$ScriptPath = Join-Path $PSScriptRoot "heartbeat_monitor.ps1"
$WorkingDir = Split-Path $ScriptPath -Parent
$PowerShellPath = "C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe"
$Arguments = "-ExecutionPolicy Bypass -File `"$ScriptPath`" update"

# Check if task already exists
$ExistingTask = Get-ScheduledTask -TaskName $TaskName -ErrorAction SilentlyContinue

if ($ExistingTask) {
    Write-Host "Task '$TaskName' already exists. Updating..." -ForegroundColor Yellow
    
    # Unregister existing task
    Unregister-ScheduledTask -TaskName $TaskName -Confirm:$false
    Write-Host "Existing task unregistered." -ForegroundColor Green
}

# Create task action
$Action = New-ScheduledTaskAction -Execute $PowerShellPath -Argument $Arguments -WorkingDirectory $WorkingDir

# Create task trigger (every 15 minutes, starting now)
$Trigger = New-ScheduledTaskTrigger -Once -At (Get-Date) -RepetitionInterval (New-TimeSpan -Minutes 15)

# Create task settings
$Settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries -StartWhenAvailable -RestartInterval (New-TimeSpan -Minutes 1) -RestartCount 3

# Register the task
$Task = Register-ScheduledTask -TaskName $TaskName -Action $Action -Trigger $Trigger -Settings $Settings -Description "OpenClaw Factory Heartbeat Monitor - Runs every 15 minutes to ensure factory is operational"

Write-Host "✅ Scheduled task '$TaskName' created successfully!" -ForegroundColor Green
Write-Host "   - Runs: Every 15 minutes"
Write-Host "   - Action: $PowerShellPath $Arguments"
Write-Host "   - Working Directory: $WorkingDir"

# Test the task
Write-Host "`nTesting heartbeat monitor..." -ForegroundColor Cyan
& $PowerShellPath -ExecutionPolicy Bypass -File $ScriptPath check

Write-Host "`n✅ Heartbeat monitor setup complete!" -ForegroundColor Green
Write-Host "The factory will now have automatic heartbeat monitoring every 15 minutes."
Write-Host "If the factory stalls for 60+ minutes, the status will be updated to STALLED."