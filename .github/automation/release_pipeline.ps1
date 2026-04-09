# release_pipeline.ps1
# Automated release pipeline for Zeta Factory
# Spawns release agents, detects completion, triggers next releases

param(
    [string]$Action = "status",
    [string]$ReleaseVersion = "",
    [switch]$Force
)

$StatusFile = ".github/automation/agent_status.json"
$PipelineFile = ".github/automation/release_pipeline.json"
$LogFile = ".github/automation/release_pipeline.log"

function Write-PipelineLog {
    param([string]$Message)
    $Timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $LogMessage = "[$Timestamp] $Message"
    Add-Content -Path $LogFile -Value $LogMessage
    Write-Host $LogMessage
}

function Get-CurrentTimestamp {
    return (Get-Date).ToUniversalTime().ToString("yyyy-MM-ddTHH:mm:ssZ")
}

function Load-PipelineState {
    if (Test-Path $PipelineFile) {
        try {
            $content = Get-Content $PipelineFile -Raw
            return $content | ConvertFrom-Json
        }
        catch {
            Write-PipelineLog "ERROR: Failed to parse pipeline file: $_"
            return $null
        }
    }
    return $null
}

function Save-PipelineState {
    param($PipelineState)
    try {
        $json = $PipelineState | ConvertTo-Json -Depth 10
        Set-Content -Path $PipelineFile -Value $json
        return $true
    }
    catch {
        Write-PipelineLog "ERROR: Failed to save pipeline file: $_"
        return $false
    }
}

function Initialize-Pipeline {
    Write-PipelineLog "Initializing release pipeline..."
    
    $initialState = @{
        version = "1.0.0"
        last_updated = Get-CurrentTimestamp
        current_release = "v0.3.28"
        release_queue = @(
            "v0.3.28",
            "v0.3.29",
            "v0.3.30",
            "v0.4.0"
        )
        release_history = @()
        pipeline_status = "idle"
        last_spawn_attempt = $null
        spawn_attempts = 0
        max_spawn_attempts = 3
        config = @{
            spawn_cooldown_minutes = 5
            release_timeout_hours = 6
            auto_advance = $true
            notify_on_stall = $true
        }
    }
    
    Save-PipelineState $initialState
    Write-PipelineLog "Pipeline initialized"
    return $initialState
}

function Check-ReleaseCompletion {
    Write-PipelineLog "Checking for release completion..."
    
    $pipeline = Load-PipelineState
    if (-not $pipeline) {
        Write-PipelineLog "ERROR: Pipeline state not loaded"
        return $null
    }
    
    # In a real implementation, this would check:
    # 1. GitHub releases for the version
    # 2. Build status
    # 3. Test results
    # 4. Deployment status
    
    # For now, we'll simulate based on time
    $currentRelease = $pipeline.current_release
    
    # Check if release agent is active in status
    $status = Get-Content $StatusFile -Raw | ConvertFrom-Json -ErrorAction SilentlyContinue
    if ($status) {
        $releaseAgent = $status.agents.release_agent
        if ($releaseAgent -and $releaseAgent.status -eq "active") {
            # Agent is active, check if it's working on current release
            if ($releaseAgent.current_release -eq $currentRelease) {
                # Check idle time
                if ($releaseAgent.idle_time_minutes -gt 30) {
                    Write-PipelineLog "WARNING: Release agent idle for $($releaseAgent.idle_time_minutes) minutes on $currentRelease"
                    return @{
                        completed = $false
                        status = "stalled"
                        reason = "Agent idle for $($releaseAgent.idle_time_minutes) minutes"
                    }
                }
                else {
                    return @{
                        completed = $false
                        status = "in_progress"
                        reason = "Agent actively working"
                    }
                }
            }
        }
    }
    
    # No active release agent - check if we should spawn one
    return @{
        completed = $false
        status = "needs_agent"
        reason = "No active release agent"
    }
}

function Spawn-ReleaseAgent {
    param([string]$ReleaseVersion)
    
    Write-PipelineLog "Spawning release agent for $ReleaseVersion..."
    
    $pipeline = Load-PipelineState
    if (-not $pipeline) {
        Write-PipelineLog "ERROR: Cannot spawn agent - pipeline not loaded"
        return $false
    }
    
    # Check cooldown
    if ($pipeline.last_spawn_attempt) {
        $lastAttempt = [DateTime]::Parse($pipeline.last_spawn_attempt)
        $cooldown = (Get-Date).ToUniversalTime() - $lastAttempt
        $cooldownMinutes = $cooldown.TotalMinutes
        
        if ($cooldownMinutes -lt $pipeline.config.spawn_cooldown_minutes) {
            Write-PipelineLog "ERROR: Still in cooldown period ($cooldownMinutes minutes since last attempt)"
            return $false
        }
    }
    
    # Check max attempts
    if ($pipeline.spawn_attempts -ge $pipeline.max_spawn_attempts) {
        Write-PipelineLog "ERROR: Maximum spawn attempts ($($pipeline.max_spawn_attempts)) reached"
        return $false
    }
    
    # Update pipeline state
    $pipeline.last_spawn_attempt = Get-CurrentTimestamp
    $pipeline.spawn_attempts++
    $pipeline.pipeline_status = "spawning"
    Save-PipelineState $pipeline | Out-Null
    
    # Update agent status
    $status = Get-Content $StatusFile -Raw | ConvertFrom-Json
    if (-not $status.agents.release_agent) {
        $status.agents | Add-Member -MemberType NoteProperty -Name "release_agent" -Value @{}
    }
    
    $status.agents.release_agent.status = "active"
    $status.agents.release_agent.last_heartbeat = Get-CurrentTimestamp
    $status.agents.release_agent.idle_time_minutes = 0
    $status.agents.release_agent.current_release = $ReleaseVersion
    $status.agents.release_agent.last_activity = "Starting release $ReleaseVersion"
    $status.agents.release_agent.spawn_count = ($status.agents.release_agent.spawn_count + 1)
    
    $status.last_updated = Get-CurrentTimestamp
    $status.factory.current_release_target = $ReleaseVersion
    
    $json = $status | ConvertTo-Json -Depth 10
    Set-Content -Path $StatusFile -Value $json
    
    Write-PipelineLog "✓ Release agent spawned for $ReleaseVersion"
    Write-PipelineLog "  Spawn attempt #$($pipeline.spawn_attempts)"
    
    # In a real implementation, this would:
    # 1. Call OpenClaw API to spawn agent
    # 2. Pass release version as parameter
    # 3. Set up monitoring
    
    # For now, we'll simulate successful spawn
    $pipeline.pipeline_status = "active"
    Save-PipelineState $pipeline | Out-Null
    
    return $true
}

function Advance-Release {
    Write-PipelineLog "Advancing to next release..."
    
    $pipeline = Load-PipelineState
    if (-not $pipeline) {
        Write-PipelineLog "ERROR: Cannot advance - pipeline not loaded"
        return $false
    }
    
    # Move current release to history
    if ($pipeline.current_release) {
        $pipeline.release_history += @{
            version = $pipeline.current_release
            completed_at = Get-CurrentTimestamp
            status = "completed"
        }
    }
    
    # Get next release from queue
    if ($pipeline.release_queue.Count -gt 0) {
        $pipeline.current_release = $pipeline.release_queue[0]
        $pipeline.release_queue = $pipeline.release_queue[1..($pipeline.release_queue.Count - 1)]
        
        Write-PipelineLog "✓ Advanced to $($pipeline.current_release)"
        Write-PipelineLog "  Remaining in queue: $($pipeline.release_queue.Count)"
        
        # Reset spawn attempts for new release
        $pipeline.spawn_attempts = 0
        $pipeline.last_spawn_attempt = $null
        
        Save-PipelineState $pipeline | Out-Null
        
        # Spawn agent for new release
        Spawn-ReleaseAgent -ReleaseVersion $pipeline.current_release
        
        return $true
    }
    else {
        Write-PipelineLog "✓ Release queue empty - pipeline complete!"
        $pipeline.pipeline_status = "complete"
        Save-PipelineState $pipeline | Out-Null
        return $true
    }
}

function Monitor-Pipeline {
    Write-PipelineLog "=== Pipeline Monitor Run ==="
    
    $pipeline = Load-PipelineState
    if (-not $pipeline) {
        Write-PipelineLog "ERROR: Pipeline not initialized"
        return $false
    }
    
    Write-PipelineLog "Current Release: $($pipeline.current_release)"
    Write-PipelineLog "Pipeline Status: $($pipeline.pipeline_status)"
    Write-PipelineLog "Queue Length: $($pipeline.release_queue.Count)"
    Write-PipelineLog "Spawn Attempts: $($pipeline.spawn_attempts)/$($pipeline.max_spawn_attempts)"
    
    # Check current release status
    $releaseStatus = Check-ReleaseCompletion
    
    if ($releaseStatus) {
        Write-PipelineLog "Release Status: $($releaseStatus.status)"
        Write-PipelineLog "Reason: $($releaseStatus.reason)"
        
        # Handle different statuses
        if ($releaseStatus.status -eq "needs_agent") {
            Write-PipelineLog "Spawning agent for $($pipeline.current_release)..."
            Spawn-ReleaseAgent -ReleaseVersion $pipeline.current_release
        }
        elseif ($releaseStatus.status -eq "stalled") {
            Write-PipelineLog "WARNING: Release stalled - $($releaseStatus.reason)"
            
            # Check if we should retry spawn
            if ($pipeline.spawn_attempts -lt $pipeline.max_spawn_attempts) {
                Write-PipelineLog "Attempting to respawn agent..."
                Spawn-ReleaseAgent -ReleaseVersion $pipeline.current_release
            }
            else {
                Write-PipelineLog "ERROR: Max spawn attempts reached for $($pipeline.current_release)"
                Write-PipelineLog "Consider manual intervention or skipping this release"
            }
        }
        elseif ($releaseStatus.status -eq "in_progress") {
            Write-PipelineLog "Release in progress - monitoring continues"
        }
        elseif ($releaseStatus.status -eq "completed") {
            Write-PipelineLog "✓ Release completed! Advancing pipeline..."
            Advance-Release
        }
    }
    
    Write-PipelineLog "=== Monitor Complete ==="
    return $true
}

function Show-PipelineStatus {
    $pipeline = Load-PipelineState
    if (-not $pipeline) {
        Write-Host "Pipeline not initialized" -ForegroundColor Red
        return
    }
    
    Write-Host "=== ZETA RELEASE PIPELINE ===" -ForegroundColor Cyan
    Write-Host ""
    
    Write-Host "Current Release:" -ForegroundColor Yellow -NoNewline
    Write-Host " $($pipeline.current_release)" -ForegroundColor White
    
    Write-Host "Pipeline Status:" -ForegroundColor Yellow -NoNewline
    $statusColor = "Gray"
    if ($pipeline.pipeline_status -eq "active") { $statusColor = "Green" }
    elseif ($pipeline.pipeline_status -eq "idle") { $statusColor = "Yellow" }
    elseif ($pipeline.pipeline_status -eq "stalled") { $statusColor = "Red" }
    elseif ($pipeline.pipeline_status -eq "complete") { $statusColor = "Cyan" }
    Write-Host " $($pipeline.pipeline_status)" -ForegroundColor $statusColor
    
    Write-Host ""
    Write-Host "Release Queue:" -ForegroundColor Cyan
    if ($pipeline.release_queue.Count -gt 0) {
        for ($i = 0; $i -lt $pipeline.release_queue.Count; $i++) {
            Write-Host "  $($i+1). $($pipeline.release_queue[$i])" -ForegroundColor Gray
        }
    }
    else {
        Write-Host "  (empty)" -ForegroundColor DarkGray
    }
    
    Write-Host ""
    Write-Host "Release History:" -ForegroundColor Cyan
    if ($pipeline.release_history.Count -gt 0) {
        $recentHistory = $pipeline.release_history[-3..-1] | Where-Object { $_ }
        foreach ($release in $recentHistory) {
            Write-Host "  ✓ $($release.version) - $($release.status)" -ForegroundColor DarkGreen
        }
        if ($pipeline.release_history.Count -gt 3) {
            Write-Host "  ... and $($pipeline.release_history.Count - 3) more" -ForegroundColor DarkGray
        }
    }
    else {
        Write-Host "  (no releases completed)" -ForegroundColor DarkGray
    }
    
    Write-Host ""
    Write-Host "Statistics:" -ForegroundColor Cyan
    Write-Host "  Spawn Attempts:" -ForegroundColor Yellow -NoNewline
    Write-Host " $($pipeline.spawn_attempts)/$($pipeline.max_spawn_attempts)" -ForegroundColor White
    Write-Host "  Auto-advance:" -ForegroundColor Yellow -NoNewline
    Write-Host " $($pipeline.config.auto_advance)" -ForegroundColor White
    Write-Host "  Last Updated:" -ForegroundColor Yellow -NoNewline
    Write-Host " $($pipeline.last_updated)" -ForegroundColor Gray
}

# Main execution
switch ($Action.ToLower()) {
    "status" {
        Show-PipelineStatus
    }
    "monitor" {
        Monitor-Pipeline
    }
    "spawn" {
        if (-not $ReleaseVersion) {
            $pipeline = Load-PipelineState
            if ($pipeline) {
                $ReleaseVersion = $pipeline.current_release
            }
            else {
                Write-PipelineLog "ERROR: No release version specified and pipeline not initialized"
                exit 1
            }
        }
        Spawn-ReleaseAgent -ReleaseVersion $ReleaseVersion
    }
    "advance" {
        Advance-Release
    }
    "init" {
        Initialize-Pipeline
    }
    "check" {
        $result = Check-ReleaseCompletion
        if ($result) {
            Write-Host "Release Status: $($result.status)" -ForegroundColor Cyan
            Write-Host "Reason: $($result.reason)" -ForegroundColor Gray
        }
    }
    default {
        Write-Host "Usage:" -ForegroundColor Cyan
        Write-Host "  .\release_pipeline.ps1 -Action status    # Show pipeline status" -ForegroundColor Gray
        Write-Host "  .\release_pipeline.ps1 -Action monitor   # Run pipeline monitor" -ForegroundColor Gray
        Write-Host "  .\release_pipeline.ps1 -Action spawn     # Spawn release agent" -ForegroundColor Gray
        Write-Host "  .\release_pipeline.ps1 -Action advance   # Advance to next release" -ForegroundColor Gray
        Write-Host "  .\release_pipeline.ps1 -Action init      # Initialize pipeline" -ForegroundColor Gray
        Write-Host "  .\release_pipeline.ps1 -Action check     # Check release completion" -ForegroundColor Gray
        Write-Host ""
        Write-Host "Options:" -ForegroundColor Yellow
        Write-Host "  -ReleaseVersion <version>  # Specify release version (for spawn)" -ForegroundColor Gray
        Write-Host "  -Force                     # Force action despite warnings" -ForegroundColor Gray
    }
}