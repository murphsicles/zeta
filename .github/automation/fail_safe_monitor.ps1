# Fail-safe monitor for Zeta Factory
param()

$workspace = Split-Path $MyInvocation.MyCommand.Path -Parent
$statusFile = Join-Path $workspace ".github\automation\agent_status.json"

try {
    if (Test-Path $statusFile) {
        $status = Get-Content $statusFile -Raw | ConvertFrom-Json
        $lastHeartbeat = [DateTime]::Parse($status.last_heartbeat)
        $minutesIdle = [Math]::Round(((Get-Date) - $lastHeartbeat).TotalMinutes, 2)
        
        if ($minutesIdle -gt 30) {
            # Warning: Factory showing signs of slowing
            Write-Host "[$(Get-Date -Format 'HH:mm:ss')] âš ï¸ Factory idle for $minutesIdle minutes" -ForegroundColor Yellow
            
            # Update status
            $status.alerts += "WARNING: Factory idle for $minutesIdle minutes at $(Get-Date -Format 'HH:mm:ss')"
            $status | ConvertTo-Json | Out-File $statusFile -Encoding UTF8
        }
        
        if ($minutesIdle -gt 60) {
            # Critical: Factory stalled
            Write-Host "[$(Get-Date -Format 'HH:mm:ss')] ðŸš¨ FACTORY STALLED FOR $minutesIdle MINUTES" -ForegroundColor Red
            
            # Log failure
            $failureLog = @{
                timestamp = Get-Date -Format "yyyy-MM-ddTHH:mm:ssZ"
                idle_minutes = $minutesIdle
                action = "STALL_DETECTED"
                status = $status.factory_status
            }
            
            $logFile = Join-Path $workspace ".github\automation\failure_log.json"
            if (Test-Path $logFile) {
                $log = Get-Content $logFile -Raw | ConvertFrom-Json
                $log += $failureLog
            } else {
                $log = @($failureLog)
            }
            $log | ConvertTo-Json | Out-File $logFile -Encoding UTF8
            
            # Could trigger emergency recovery here
            Write-Host "   EMERGENCY RECOVERY WOULD BE TRIGGERED" -ForegroundColor Red
        }
    }
} catch {
    Write-Host "[$(Get-Date -Format 'HH:mm:ss')] âŒ Fail-safe monitor error: $_" -ForegroundColor Red
}
