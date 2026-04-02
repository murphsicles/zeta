# setup_heartbeat_monitor.ps1
# Sets up scheduled task to run agent heartbeat monitoring every 15 minutes

param(
    [switch]$Install,
    [switch]$Uninstall,
    [switch]$Test
)

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$CheckScript = Join-Path $ScriptDir "check_agents.ps1"
$TaskName = "ZetaFactoryAgentMonitor"
$TaskDescription = "Monitors Zeta Factory agent status every 15 minutes to prevent stalls"

function Test-Monitor {
    Write-Host "Testing agent monitor..." -ForegroundColor Cyan
    Write-Host "Running check_agents.ps1..." -ForegroundColor Yellow
    
    & $CheckScript -Action check
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✓ Monitor test successful" -ForegroundColor Green
    }
    else {
        Write-Host "✗ Monitor test failed" -ForegroundColor Red
    }
    
    Write-Host "`nCurrent status:" -ForegroundColor Cyan
    if (Test-Path ".github/automation/agent_status.json") {
        $status = Get-Content ".github/automation/agent_status.json" -Raw | ConvertFrom-Json
        $status | Format-List | Out-Host
    }
    else {
        Write-Host "Status file not found" -ForegroundColor Yellow
    }
}

function Install-Monitor {
    Write-Host "Installing agent heartbeat monitor..." -ForegroundColor Cyan
    
    # First, initialize the status system
    Write-Host "Initializing status system..." -ForegroundColor Yellow
    & $CheckScript -Action init
    if ($LASTEXITCODE -ne 0) {
        Write-Host "Failed to initialize status system" -ForegroundColor Red
        return $false
    }
    
    # Create PowerShell script for scheduled task
    $TaskScript = @"
# Agent Monitor Task Script
Set-Location "$(Get-Location)"
& "$CheckScript" -Action check
"@
    
    $TaskScriptPath = Join-Path $ScriptDir "run_monitor.ps1"
    Set-Content -Path $TaskScriptPath -Value $TaskScript
    
    # Create scheduled task
    Write-Host "Creating scheduled task..." -ForegroundColor Yellow
    
    $Action = New-ScheduledTaskAction -Execute "powershell.exe" -Argument "-NoProfile -ExecutionPolicy Bypass -File `"$TaskScriptPath`""
    $Trigger = New-ScheduledTaskTrigger -Once -At (Get-Date) -RepetitionInterval (New-TimeSpan -Minutes 15) -RepetitionDuration (New-TimeSpan -Days 365)
    $Principal = New-ScheduledTaskPrincipal -UserId "SYSTEM" -LogonType ServiceAccount -RunLevel Highest
    $Settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries -StartWhenAvailable -RestartInterval (New-TimeSpan -Minutes 5) -RestartCount 3
    
    try {
        Register-ScheduledTask -TaskName $TaskName -Description $TaskDescription `
            -Action $Action -Trigger $Trigger -Principal $Principal -Settings $Settings -Force
        
        Write-Host "✓ Scheduled task created: $TaskName" -ForegroundColor Green
        Write-Host "  - Runs every 15 minutes" -ForegroundColor Gray
        Write-Host "  - Runs as SYSTEM account" -ForegroundColor Gray
        Write-Host "  - Auto-restarts on failure" -ForegroundColor Gray
        
        # Start the task immediately
        Start-ScheduledTask -TaskName $TaskName
        Write-Host "✓ Task started successfully" -ForegroundColor Green
        
        # Test the installation
        Write-Host "`nTesting installation..." -ForegroundColor Cyan
        Start-Sleep -Seconds 2
        Test-Monitor
        
        return $true
    }
    catch {
        Write-Host "✗ Failed to create scheduled task: $_" -ForegroundColor Red
        return $false
    }
}

function Uninstall-Monitor {
    Write-Host "Uninstalling agent heartbeat monitor..." -ForegroundColor Cyan
    
    try {
        # Stop and remove scheduled task
        $task = Get-ScheduledTask -TaskName $TaskName -ErrorAction SilentlyContinue
        if ($task) {
            Stop-ScheduledTask -TaskName $TaskName -ErrorAction SilentlyContinue
            Unregister-ScheduledTask -TaskName $TaskName -Confirm:$false
            Write-Host "✓ Scheduled task removed: $TaskName" -ForegroundColor Green
        }
        else {
            Write-Host "Task '$TaskName' not found" -ForegroundColor Yellow
        }
        
        # Clean up generated files
        $filesToRemove = @(
            "run_monitor.ps1",
            "agent_monitor.log"
        )
        
        foreach ($file in $filesToRemove) {
            $filePath = Join-Path $ScriptDir $file
            if (Test-Path $filePath) {
                Remove-Item $filePath -Force -ErrorAction SilentlyContinue
                Write-Host "✓ Removed: $file" -ForegroundColor Green
            }
        }
        
        Write-Host "✓ Monitor uninstalled successfully" -ForegroundColor Green
        return $true
    }
    catch {
        Write-Host "✗ Failed to uninstall: $_" -ForegroundColor Red
        return $false
    }
}

# Main execution
if ($Test) {
    Test-Monitor
}
elseif ($Uninstall) {
    Uninstall-Monitor
}
elseif ($Install) {
    Install-Monitor
}
else {
    Write-Host "Usage:" -ForegroundColor Cyan
    Write-Host "  .\setup_heartbeat_monitor.ps1 -Install    # Install the monitor" -ForegroundColor Gray
    Write-Host "  .\setup_heartbeat_monitor.ps1 -Uninstall  # Remove the monitor" -ForegroundColor Gray
    Write-Host "  .\setup_heartbeat_monitor.ps1 -Test       # Test the monitor" -ForegroundColor Gray
    Write-Host ""
    Write-Host "The monitor will check agent status every 15 minutes and alert if any agent" -ForegroundColor Yellow
    Write-Host "is idle for more than 60 minutes (alert) or 120 minutes (auto-respawn)." -ForegroundColor Yellow
}