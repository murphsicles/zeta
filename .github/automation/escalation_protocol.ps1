# escalation_protocol.ps1
# Escalation protocol for agent failures and factory stalls

param(
    [string]$Action = "check",
    [string]$Level = "auto"
)

$StatusFile = ".github/automation/agent_status.json"
$LogFile = ".github/automation/escalation.log"
$ConfigFile = ".github/automation/escalation_config.json"

function Write-EscalationLog {
    param([string]$Message, [string]$Level = "INFO")
    $Timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $LogMessage = "[$Timestamp] [$Level] $Message"
    Add-Content -Path $LogFile -Value $LogMessage
    Write-Host $LogMessage
}

function Get-CurrentTimestamp {
    return (Get-Date).ToUniversalTime().ToString("yyyy-MM-ddTHH:mm:ssZ")
}

function Load-EscalationConfig {
    $defaultConfig = @{
        version = "1.0.0"
        levels = @{
            level1 = @{
                name = "Agent Idle Alert"
                threshold_minutes = 60
                actions = @("log", "alert")
                message = "Agent {agent} idle for {minutes} minutes"
            }
            level2 = @{
                name = "Agent Stalled - Auto Respawn"
                threshold_minutes = 120
                actions = @("log", "alert", "auto_respawn")
                message = "Agent {agent} stalled for {minutes} minutes - auto-respawning"
                max_respawn_attempts = 3
            }
            level3 = @{
                name = "Factory Stalled - Manual Intervention"
                threshold_minutes = 240
                actions = @("log", "alert", "notify_owner", "escalate")
                message = "FACTORY STALLED: Agent {agent} inactive for {minutes} minutes - REQUIRES MANUAL INTERVENTION"
                notification_channels = @("telegram", "email")
            }
            level4 = @{
                name = "Critical Failure - System Wide"
                threshold_minutes = 480
                actions = @("log", "alert", "notify_owner", "escalate", "emergency_stop")
                message = "CRITICAL FAILURE: Factory offline for {minutes} minutes - EMERGENCY PROTOCOLS ACTIVATED"
                emergency_contact = "primary_owner"
            }
        }
        notification = @{
            telegram_enabled = $true
            telegram_chat_id = ""
            email_enabled = $false
            email_address = ""
            sms_enabled = $false
            sms_number = ""
        }
        auto_respawn = @{
            enabled = $true
            cooldown_minutes = 5
            max_attempts_per_agent = 3
        }
    }
    
    if (Test-Path $ConfigFile) {
        try {
            $content = Get-Content $ConfigFile -Raw
            return $content | ConvertFrom-Json
        }
        catch {
            Write-EscalationLog "ERROR: Failed to parse config file, using defaults: $_" -Level "ERROR"
        }
    }
    
    # Save default config
    $json = $defaultConfig | ConvertTo-Json -Depth 10
    Set-Content -Path $ConfigFile -Value $json
    Write-EscalationLog "Created default escalation config" -Level "INFO"
    
    return $defaultConfig
}

function Check-EscalationLevel {
    param([int]$IdleMinutes)
    
    $config = Load-EscalationConfig
    
    if ($IdleMinutes -ge $config.levels.level4.threshold_minutes) {
        return "level4"
    }
    elseif ($IdleMinutes -ge $config.levels.level3.threshold_minutes) {
        return "level3"
    }
    elseif ($IdleMinutes -ge $config.levels.level2.threshold_minutes) {
        return "level2"
    }
    elseif ($IdleMinutes -ge $config.levels.level1.threshold_minutes) {
        return "level1"
    }
    
    return "normal"
}

function Execute-EscalationActions {
    param(
        [string]$Level,
        [string]$AgentName,
        [int]$IdleMinutes,
        [string]$Message
    )
    
    $config = Load-EscalationConfig
    $levelConfig = $config.levels.$Level
    
    if (-not $levelConfig) {
        Write-EscalationLog "ERROR: Unknown escalation level: $Level" -Level "ERROR"
        return $false
    }
    
    Write-EscalationLog "Executing escalation level: $($levelConfig.name)" -Level "ESCALATE"
    Write-EscalationLog "Agent: $AgentName, Idle: $IdleMinutes minutes" -Level "ESCALATE"
    
    # Execute each action
    foreach ($action in $levelConfig.actions) {
        switch ($action.ToLower()) {
            "log" {
                Write-EscalationLog "ACTION: Logged escalation for $AgentName" -Level "ACTION"
                # Log is already done above
            }
            "alert" {
                Write-EscalationLog "ACTION: Alert triggered for $AgentName" -Level "ACTION"
                Create-Alert -AgentName $AgentName -IdleMinutes $IdleMinutes -Level $Level -Message $Message
            }
            "auto_respawn" {
                Write-EscalationLog "ACTION: Attempting auto-respawn for $AgentName" -Level "ACTION"
                Invoke-AutoRespawn -AgentName $AgentName
            }
            "notify_owner" {
                Write-EscalationLog "ACTION: Notifying owner about $AgentName" -Level "ACTION"
                Send-Notification -AgentName $AgentName -IdleMinutes $IdleMinutes -Level $Level
            }
            "escalate" {
                Write-EscalationLog "ACTION: Escalating to next level for $AgentName" -Level "ACTION"
                # In a real system, this might trigger pager duty, etc.
            }
            "emergency_stop" {
                Write-EscalationLog "ACTION: EMERGENCY STOP protocol activated" -Level "CRITICAL"
                # In a real system, this might stop certain processes
            }
        }
    }
    
    return $true
}

function Create-Alert {
    param(
        [string]$AgentName,
        [int]$IdleMinutes,
        [string]$Level,
        [string]$Message
    )
    
    $alertFile = ".github/automation/active_alerts.json"
    
    # Load existing alerts
    $alerts = @()
    if (Test-Path $alertFile) {
        try {
            $content = Get-Content $alertFile -Raw
            $alerts = $content | ConvertFrom-Json
        }
        catch {
            $alerts = @()
        }
    }
    
    # Create new alert
    $newAlert = @{
        id = [Guid]::NewGuid().ToString()
        timestamp = Get-CurrentTimestamp
        agent = $AgentName
        level = $Level
        idle_minutes = $IdleMinutes
        message = $Message
        acknowledged = $false
        resolved = $false
    }
    
    # Add to alerts (limit to 50 most recent)
    $alerts = @($newAlert) + $alerts
    if ($alerts.Count -gt 50) {
        $alerts = $alerts[0..49]
    }
    
    # Save alerts
    $json = $alerts | ConvertTo-Json -Depth 10
    Set-Content -Path $alertFile -Value $json
    
    Write-EscalationLog "Alert created: $($newAlert.id)" -Level "ALERT"
}

function Invoke-AutoRespawn {
    param([string]$AgentName)
    
    Write-EscalationLog "Auto-respawning agent: $AgentName" -Level "RESPAWN"
    
    # In a real implementation, this would:
    # 1. Check if agent exists in status
    # 2. Get agent type and configuration
    # 3. Call OpenClaw API to spawn new agent
    # 4. Update agent status with new spawn
    
    # For now, simulate the process
    $status = Get-Content $StatusFile -Raw | ConvertFrom-Json -ErrorAction SilentlyContinue
    if ($status -and $status.agents.$AgentName) {
        $agent = $status.agents.$AgentName
        
        # Update spawn count
        $agent.spawn_count++
        $agent.last_heartbeat = Get-CurrentTimestamp
        $agent.idle_time_minutes = 0
        $agent.status = "respawning"
        $agent.last_activity = "Auto-respawn initiated"
        
        # Save status
        $json = $status | ConvertTo-Json -Depth 10
        Set-Content -Path $StatusFile -Value $json
        
        Write-EscalationLog "Agent $AgentName marked for respawn (attempt #$($agent.spawn_count))" -Level "RESPAWN"
        
        # Simulate respawn delay
        Start-Sleep -Seconds 2
        
        # Mark as active
        $agent.status = "active"
        $agent.last_activity = "Respawned by escalation protocol"
        
        $json = $status | ConvertTo-Json -Depth 10
        Set-Content -Path $StatusFile -Value $json
        
        Write-EscalationLog "Agent $AgentName respawned successfully" -Level "RESPAWN"
        return $true
    }
    
    Write-EscalationLog "ERROR: Agent $AgentName not found in status" -Level "ERROR"
    return $false
}

function Send-Notification {
    param(
        [string]$AgentName,
        [int]$IdleMinutes,
        [string]$Level
    )
    
    $config = Load-EscalationConfig
    
    $message = @"
🚨 ZETA FACTORY ESCALATION - Level: $Level

Agent: $AgentName
Idle Time: $IdleMinutes minutes
Status: REQUIRES ATTENTION

Timestamp: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Action: Check dashboard for details
"@
    
    Write-EscalationLog "Notification prepared for $AgentName" -Level "NOTIFY"
    
    # In a real implementation, this would send via:
    # - Telegram bot
    # - Email
    # - SMS
    # - Slack/Teams webhook
    
    # For now, just log it
    Write-EscalationLog "WOULD SEND NOTIFICATION: $message" -Level "NOTIFY"
    
    return $true
}

function Check-And-Escalate {
    Write-EscalationLog "=== Starting escalation check ===" -Level "INFO"
    
    # Load agent status
    if (-not (Test-Path $StatusFile)) {
        Write-EscalationLog "ERROR: Status file not found" -Level "ERROR"
        return $false
    }
    
    $status = Get-Content $StatusFile -Raw | ConvertFrom-Json
    if (-not $status) {
        Write-EscalationLog "ERROR: Failed to parse status file" -Level "ERROR"
        return $false
    }
    
    $escalations = @()
    
    # Check each agent
    foreach ($agentName in $status.agents.PSObject.Properties.Name) {
        $agent = $status.agents.$agentName
        
        if ($agent.idle_time_minutes -ne $null) {
            $idleMinutes = [math]::Round($agent.idle_time_minutes, 1)
            $level = Check-EscalationLevel -IdleMinutes $idleMinutes
            
            if ($level -ne "normal") {
                $message = "Agent $agentName idle for $idleMinutes minutes"
                Write-EscalationLog "ESCALATION NEEDED: $agentName -> $level ($idleMinutes minutes)" -Level "WARN"
                
                $escalations += @{
                    agent = $agentName
                    level = $level
                    idle_minutes = $idleMinutes
                    message = $message
                }
            }
        }
    }
    
    # Check factory status
    if ($status.factory.status -eq "degraded") {
        $factoryIdle = $status.factory.release_behind_schedule_hours * 60
        $level = Check-EscalationLevel -IdleMinutes $factoryIdle
        
        if ($level -ne "normal") {
            $message = "Factory degraded for $($status.factory.release_behind_schedule_hours) hours"
            Write-EscalationLog "FACTORY ESCALATION: $level" -Level "WARN"
            
            $escalations += @{
                agent = "FACTORY"
                level = $level
                idle_minutes = $factoryIdle
                message = $message
            }
        }
    }
    
    # Execute escalations
    foreach ($escalation in $escalations) {
        Execute-EscalationActions `
            -Level $escalation.level `
            -AgentName $escalation.agent `
            -IdleMinutes $escalation.idle_minutes `
            -Message $escalation.message
    }
    
    if ($escalations.Count -gt 0) {
        Write-EscalationLog "=== Escalation check complete: $($escalations.Count) escalations ===" -Level "INFO"
        return $true
    }
    else {
        Write-EscalationLog "=== Escalation check complete: No escalations needed ===" -Level "INFO"
        return $false
    }
}

function Show-ActiveAlerts {
    $alertFile = ".github/automation/active_alerts.json"
    
    if (-not (Test-Path $alertFile)) {
        Write-Host "No active alerts" -ForegroundColor Green
        return
    }
    
    $alerts = Get-Content $alertFile -Raw | ConvertFrom-Json
    
    Write-Host "=== ACTIVE ALERTS ===" -ForegroundColor Cyan
    Write-Host ""
    
    $activeCount = 0
    foreach ($alert in $alerts) {
        if (-not $alert.resolved) {
            $activeCount++
            
            $color = switch ($alert.level) {
                "level4" { "Red" }
                "level3" { "Red" }
                "level2" { "Yellow" }
                "level1" { "Yellow" }
                default { "Gray" }
            }
            
            Write-Host "[$($alert.level.ToUpper())]" -ForegroundColor $color -NoNewline
            Write-Host " $($alert.agent)" -ForegroundColor White
            Write-Host "  Idle: $($alert.idle_minutes) minutes" -ForegroundColor Gray
            Write-Host "  Message: $($alert.message)" -ForegroundColor Gray
            Write-Host "  Time: $($alert.timestamp)" -ForegroundColor DarkGray
            Write-Host ""
        }
    }
    
    if ($activeCount -eq 0) {
        Write-Host "No active alerts" -ForegroundColor Green
    }
    else {
        Write-Host "Total active alerts: $activeCount" -ForegroundColor Cyan
    }
}

# Main execution
switch ($Action.ToLower()) {
    "check" {
        $result = Check-And-Escalate
        if ($result) {
            exit 0
        }
        else {
            exit 1
        }
    }
    "alerts" {
        Show-ActiveAlerts
    }
    "test" {
        Write-Host "Testing escalation protocol..." -ForegroundColor Cyan
        
        # Test level 1 (60+ minutes)
        Write-Host "`nTesting Level 1 (60+ minutes idle)..." -ForegroundColor Yellow
        Execute-EscalationActions -Level "level1" -AgentName "TEST_AGENT" -IdleMinutes 65 -Message "Test alert"
        
        # Test level 2 (120+ minutes)
        Write-Host "`nTesting Level 2 (120+ minutes idle)..." -ForegroundColor Yellow
        Execute-EscalationActions -Level "level2" -AgentName "TEST_AGENT" -IdleMinutes 125 -Message "Test auto-respawn"
        
        Write-Host "`n✓ Escalation protocol test complete" -ForegroundColor Green
    }
    "config" {
        $config = Load-EscalationConfig
        Write-Host "=== ESCALATION CONFIGURATION ===" -ForegroundColor Cyan
        $config | Format-List | Out-Host
    }
    default {
        Write-Host "Usage:" -ForegroundColor Cyan
        Write-Host "  .\escalation_protocol.ps1 -Action check    # Check and escalate" -ForegroundColor Gray
        Write-Host "  .\escalation_protocol.ps1 -Action alerts   # Show active alerts" -ForegroundColor Gray
        Write-Host "  .\escalation_protocol.ps1 -Action test     # Test escalation" -ForegroundColor Gray
        Write-Host "  .\escalation_protocol.ps1 -Action config   # Show configuration" -ForegroundColor Gray
    }
}