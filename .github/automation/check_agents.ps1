# check_agents.ps1 - Agent Heartbeat Monitoring System
# Monitors agent status and triggers alerts/respawns

param(
    [string]$Action = "check",
    [string]$AgentName = "",
    [string]$Status = "active"
)

# Configuration
$StatusFile = ".github/automation/agent_status.json"
$LogFile = ".github/automation/agent_monitor.log"
$AlertThresholdMinutes = 60  # Alert after 1 hour idle
$RespawnThresholdMinutes = 120  # Auto-respawn after 2 hours idle

# Ensure automation directory exists
if (-not (Test-Path ".github/automation")) {
    New-Item -ItemType Directory -Path ".github/automation" -Force | Out-Null
}

function Write-Log {
    param([string]$Message)
    $Timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $LogMessage = "[$Timestamp] $Message"
    Add-Content -Path $LogFile -Value $LogMessage
    Write-Host $LogMessage
}

function Get-CurrentTimestamp {
    return (Get-Date).ToUniversalTime().ToString("yyyy-MM-ddTHH:mm:ssZ")
}

function Load-Status {
    if (Test-Path $StatusFile) {
        try {
            $content = Get-Content $StatusFile -Raw
            return $content | ConvertFrom-Json
        }
        catch {
            Write-Log "ERROR: Failed to parse status file: $_"
            return $null
        }
    }
    return $null
}

function Save-Status {
    param($StatusObj)
    try {
        $json = $StatusObj | ConvertTo-Json -Depth 10
        Set-Content -Path $StatusFile -Value $json
        return $true
    }
    catch {
        Write-Log "ERROR: Failed to save status file: $_"
        return $false
    }
}

function Update-AgentHeartbeat {
    param(
        [string]$AgentName,
        [string]$Activity = "heartbeat"
    )
    
    $status = Load-Status
    if (-not $status) {
        Write-Log "ERROR: Cannot update heartbeat - status file not loaded"
        return $false
    }
    
    $timestamp = Get-CurrentTimestamp
    
    # Initialize agent if not exists
    if (-not $status.agents.$AgentName) {
        $status.agents | Add-Member -MemberType NoteProperty -Name $AgentName -Value @{
            status = "active"
            last_heartbeat = $timestamp
            idle_time_minutes = 0
            current_release = $status.factory.current_release_target
            last_activity = $Activity
            spawn_count = 1
        }
    }
    else {
        # Update existing agent
        $agent = $status.agents.$AgentName
        $agent.last_heartbeat = $timestamp
        $agent.last_activity = $Activity
        $agent.status = "active"
        
        # Calculate idle time
        if ($agent.last_heartbeat) {
            $lastHeartbeat = [DateTime]::Parse($agent.last_heartbeat)
            $idleTime = (Get-Date).ToUniversalTime() - $lastHeartbeat
            $agent.idle_time_minutes = [math]::Round($idleTime.TotalMinutes, 2)
        }
    }
    
    # Update factory status
    $status.last_updated = $timestamp
    
    # Check for idle agents
    $idleAgents = @()
    foreach ($agentName in $status.agents.PSObject.Properties.Name) {
        $agent = $status.agents.$agentName
        if ($agent.idle_time_minutes -gt $AlertThresholdMinutes) {
            $idleAgents += @{
                name = $agentName
                idle_minutes = $agent.idle_time_minutes
            }
        }
    }
    
    # Update alerts
    $status.factory.alerts = @()
    if ($idleAgents.Count -gt 0) {
        foreach ($idleAgent in $idleAgents) {
            $alertMsg = "WARNING: Agent '$($idleAgent.name)' idle for $($idleAgent.idle_minutes) minutes"
            $status.factory.alerts += $alertMsg
            Write-Log $alertMsg
        }
    }
    
    # Check if factory is operational
    $activeAgents = 0
    foreach ($agentName in $status.agents.PSObject.Properties.Name) {
        $agent = $status.agents.$agentName
        if ($agent.status -eq "active" -and $agent.idle_time_minutes -lt $AlertThresholdMinutes) {
            $activeAgents++
        }
    }
    
    $status.factory.status = if ($activeAgents -gt 0) { "operational" } else { "degraded" }
    
    Save-Status $status | Out-Null
    Write-Log "Heartbeat updated for agent: $AgentName"
    return $true
}

function Check-Agents {
    $status = Load-Status
    if (-not $status) {
        Write-Log "ERROR: Cannot check agents - status file not loaded"
        return $false
    }
    
    $timestamp = Get-CurrentTimestamp
    Write-Log "=== Agent Status Check at $timestamp ==="
    
    $idleAgents = @()
    $stuckAgents = @()
    
    foreach ($agentName in $status.agents.PSObject.Properties.Name) {
        $agent = $status.agents.$agentName
        
        # Calculate idle time
        if ($agent.last_heartbeat) {
            $lastHeartbeat = [DateTime]::Parse($agent.last_heartbeat)
            $idleTime = (Get-Date).ToUniversalTime() - $lastHeartbeat
            $agent.idle_time_minutes = [math]::Round($idleTime.TotalMinutes, 2)
            
            # Check for alerts
            if ($agent.idle_time_minutes -gt $AlertThresholdMinutes) {
                $idleAgents += $agentName
                Write-Log "ALERT: Agent '$agentName' idle for $($agent.idle_time_minutes) minutes"
            }
            
            # Check for auto-respawn
            if ($agent.idle_time_minutes -gt $RespawnThresholdMinutes) {
                $stuckAgents += $agentName
                Write-Log "CRITICAL: Agent '$agentName' stuck for $($agent.idle_time_minutes) minutes - requires respawn"
            }
        }
    }
    
    # Update factory status
    $activeCount = ($status.agents.PSObject.Properties.Name | Where-Object {
        $agent = $status.agents.$_
        $agent.status -eq "active" -and $agent.idle_time_minutes -lt $AlertThresholdMinutes
    }).Count
    
    $status.factory.status = if ($activeCount -gt 0) { "operational" } else { "degraded" }
    $status.last_updated = $timestamp
    
    # Generate report
    Write-Log "=== Status Summary ==="
    Write-Log "Factory Status: $($status.factory.status)"
    Write-Log "Active Agents: $activeCount"
    Write-Log "Idle Agents (>${AlertThresholdMinutes}m): $($idleAgents.Count)"
    Write-Log "Stuck Agents (>${RespawnThresholdMinutes}m): $($stuckAgents.Count)"
    
    if ($idleAgents.Count -gt 0) {
        Write-Log "Idle Agents: $($idleAgents -join ', ')"
    }
    
    if ($stuckAgents.Count -gt 0) {
        Write-Log "Stuck Agents (needs respawn): $($stuckAgents -join ', ')"
        
        # Trigger respawn logic
        foreach ($stuckAgent in $stuckAgents) {
            Write-Log "Would trigger respawn for agent: $stuckAgent"
            # In a real implementation, this would call the respawn function
        }
    }
    
    Save-Status $status | Out-Null
    Write-Log "=== Check Complete ==="
    
    return @{
        idle_agents = $idleAgents
        stuck_agents = $stuckAgents
        factory_status = $status.factory.status
    }
}

function Initialize-Status {
    Write-Log "Initializing agent status system..."
    
    $initialStatus = @{
        version = "1.0.0"
        last_updated = Get-CurrentTimestamp
        agents = @{
            release_agent = @{
                status = "unknown"
                last_heartbeat = $null
                idle_time_minutes = $null
                current_release = $null
                last_activity = $null
                spawn_count = 0
            }
        }
        factory = @{
            status = "degraded"
            last_successful_release = $null
            current_release_target = "v0.3.28"
            release_behind_schedule_hours = 10
            uptime_24h_percentage = 0
            alerts = @("CRITICAL: Factory stalled for 10+ hours")
        }
        heartbeat_config = @{
            check_interval_minutes = 15
            idle_alert_threshold_minutes = $AlertThresholdMinutes
            auto_respawn_threshold_minutes = $RespawnThresholdMinutes
            max_spawn_attempts = 3
        }
    }
    
    $json = $initialStatus | ConvertTo-Json -Depth 10
    Set-Content -Path $StatusFile -Value $json
    Write-Log "Status system initialized"
    return $true
}

# Main execution
switch ($Action.ToLower()) {
    "check" {
        $result = Check-Agents
        if ($result) {
            exit 0
        }
        else {
            exit 1
        }
    }
    "heartbeat" {
        if (-not $AgentName) {
            Write-Log "ERROR: AgentName parameter required for heartbeat action"
            exit 1
        }
        $result = Update-AgentHeartbeat -AgentName $AgentName -Activity $Status
        if ($result) {
            exit 0
        }
        else {
            exit 1
        }
    }
    "init" {
        $result = Initialize-Status
        if ($result) {
            exit 0
        }
        else {
            exit 1
        }
    }
    default {
        Write-Log "ERROR: Unknown action: $Action. Use 'check', 'heartbeat', or 'init'"
        exit 1
    }
}