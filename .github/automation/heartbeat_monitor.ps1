# Heartbeat Monitor Script
# Runs every 15 minutes to ensure factory is operational
# Updates agent_status.json with current heartbeat timestamp

param(
    [string]$Action = "check"  # check, update, or test
)

$StatusFile = ".github/automation/agent_status.json"
$Now = [DateTime]::UtcNow.ToString("yyyy-MM-ddTHH:mm:ssZ")

function Update-Heartbeat {
    $status = Get-Content $StatusFile -Raw | ConvertFrom-Json
    $status.last_heartbeat = $Now
    
    # Check if factory is stalled (no heartbeat for 60 minutes)
    $lastHeartbeat = [DateTime]::Parse($status.last_heartbeat)
    $stallThreshold = [DateTime]::UtcNow.AddMinutes(-$status.monitoring.stall_threshold_minutes)
    
    if ($lastHeartbeat -lt $stallThreshold) {
        $status.factory_status = "STALLED"
        $status.alerts += "STALL_DETECTED: No heartbeat for $($status.monitoring.stall_threshold_minutes) minutes"
        Write-Host "❌ FACTORY STALLED! Last heartbeat: $($status.last_heartbeat)" -ForegroundColor Red
    } else {
        Write-Host "✅ Heartbeat updated: $Now" -ForegroundColor Green
    }
    
    $status | ConvertTo-Json -Depth 10 | Set-Content $StatusFile
}

function Check-Status {
    $status = Get-Content $StatusFile -Raw | ConvertFrom-Json
    $lastHeartbeat = [DateTime]::Parse($status.last_heartbeat)
    $minutesSince = [Math]::Round(([DateTime]::UtcNow - $lastHeartbeat).TotalMinutes, 1)
    
    Write-Host "=== FACTORY STATUS CHECK ==="
    Write-Host "Factory Status: $($status.factory_status)"
    Write-Host "Last Heartbeat: $($status.last_heartbeat) ($minutesSince minutes ago)"
    Write-Host "Stall Threshold: $($status.monitoring.stall_threshold_minutes) minutes"
    Write-Host "Heartbeat Interval: $($status.monitoring.heartbeat_interval_minutes) minutes"
    
    if ($minutesSince -gt $status.monitoring.stall_threshold_minutes) {
        Write-Host "❌ CRITICAL: Factory appears stalled!" -ForegroundColor Red
        return 1
    } elseif ($minutesSince -gt $status.monitoring.heartbeat_interval_minutes) {
        Write-Host "⚠️  WARNING: Heartbeat overdue by $($minutesSince - $status.monitoring.heartbeat_interval_minutes) minutes" -ForegroundColor Yellow
        return 2
    } else {
        Write-Host "✅ Factory operational" -ForegroundColor Green
        return 0
    }
}

switch ($Action) {
    "update" {
        Update-Heartbeat
    }
    "check" {
        exit (Check-Status)
    }
    "test" {
        Write-Host "Testing heartbeat monitor..."
        Check-Status
        Update-Heartbeat
        Check-Status
    }
    default {
        Write-Host "Usage: .\heartbeat_monitor.ps1 [check|update|test]"
        exit 1
    }
}