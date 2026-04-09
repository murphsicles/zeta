# SIMPLE HEARTBEAT MONITOR - Works within OpenClaw constraints
# This runs manually or can be triggered by OpenClaw cron

param(
    [switch]$CheckOnly,
    [switch]$ForceRespawn
)

$workspace = $PWD
$statusFile = Join-Path $workspace ".github\automation\agent_status.json"
$failureLog = Join-Path $workspace ".github\automation\failure_log.json"

# Initialize status if missing
if (-not (Test-Path $statusFile)) {
    $initialStatus = @{
        factory_status = "ACTIVE"
        last_heartbeat = Get-Date -Format "yyyy-MM-ddTHH:mm:ssZ"
        agents = @()
        alerts = @("Initialized at $(Get-Date -Format 'HH:mm:ss')")
        release_pipeline = @{
            current = "v0.3.29"
            queue = @("v0.3.30", "v0.3.31", "v0.3.32", "v0.3.33", "v0.3.34")
            status = "ACTIVE"
        }
    }
    $initialStatus | ConvertTo-Json | Out-File $statusFile -Encoding UTF8
}

# Load current status
$status = Get-Content $statusFile -Raw | ConvertFrom-Json
$lastHeartbeat = [DateTime]::Parse($status.last_heartbeat)
$minutesIdle = [Math]::Round(((Get-Date) - $lastHeartbeat).TotalMinutes, 2)

# Update heartbeat timestamp
$status.last_heartbeat = Get-Date -Format "yyyy-MM-ddTHH:mm:ssZ"

Write-Host "[$(Get-Date -Format 'HH:mm:ss')] 🔍 Factory heartbeat check" -ForegroundColor Cyan
Write-Host "   Last heartbeat: $($lastHeartbeat.ToString('HH:mm:ss'))" -ForegroundColor Gray
Write-Host "   Minutes idle: $minutesIdle" -ForegroundColor Gray
Write-Host "   Factory status: $($status.factory_status)" -ForegroundColor Gray

# Check for stall conditions
if ($minutesIdle -gt 30) {
    Write-Host "   ⚠️ Warning: Factory idle for $minutesIdle minutes" -ForegroundColor Yellow
    $status.alerts += "WARNING: Factory idle for $minutesIdle minutes at $(Get-Date -Format 'HH:mm:ss')"
    $status.factory_status = "IDLE_WARNING"
}

if ($minutesIdle -gt 60 -or $ForceRespawn) {
    Write-Host "   🚨 CRITICAL: Factory stalled for $minutesIdle minutes" -ForegroundColor Red
    Write-Host "   Triggering emergency recovery..." -ForegroundColor Red
    
    # Log failure
    $failureEntry = @{
        timestamp = Get-Date -Format "yyyy-MM-ddTHH:mm:ssZ"
        idle_minutes = $minutesIdle
        action = "EMERGENCY_RECOVERY_TRIGGERED"
        status_before = $status.factory_status
    }
    
    if (Test-Path $failureLog) {
        $log = Get-Content $failureLog -Raw | ConvertFrom-Json
        $log += $failureEntry
    } else {
        $log = @($failureEntry)
    }
    $log | ConvertTo-Json | Out-File $failureLog -Encoding UTF8
    
    # Update status for recovery
    $status.factory_status = "RECOVERING_FROM_STALL"
    $status.alerts += "EMERGENCY: Factory stalled for $minutesIdle minutes, recovery triggered at $(Get-Date -Format 'HH:mm:ss')"
    
    # In a real implementation, this would spawn emergency agents
    # For now, we just log and update status
    Write-Host "   ✅ Emergency recovery would spawn agents for: $($status.release_pipeline.current)" -ForegroundColor Green
}

# Save updated status
$status | ConvertTo-Json | Out-File $statusFile -Encoding UTF8

if (-not $CheckOnly) {
    Write-Host "   ✅ Heartbeat updated at $(Get-Date -Format 'HH:mm:ss')" -ForegroundColor Green
}

Write-Host ""