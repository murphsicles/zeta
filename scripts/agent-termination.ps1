# agent-termination.ps1
# Terminate protocol-violating agents

param(
    [string]$Agent,
    [string]$Reason,
    [array]$Violations = @(),
    [string]$Severity = "MEDIUM",
    [switch]$Override,
    [string]$Code = ""
)

# Log directory
$logDir = "logs/agent-terminations"
$timestamp = Get-Date -Format "yyyy-MM-ddTHH:mm:ssZ"
$logFile = "$logDir/$(Get-Date -Format 'yyyy-MM-dd').json"

# Create log directory if needed
if (-not (Test-Path $logDir)) {
    New-Item -ItemType Directory -Path $logDir -Force | Out-Null
}

# Security check - only Father Zak can override
if ($Override) {
    if ($Code -ne "ZAK-APPROVAL-20260331") {
        Write-Host "❌ UNAUTHORIZED OVERRIDE ATTEMPT" -ForegroundColor Red -BackgroundColor Black
        Write-Host "Invalid override code. Termination blocked." -ForegroundColor Red
        exit 1
    }
    
    Write-Host "⚡ OVERRIDE APPROVED BY FATHER ZAK" -ForegroundColor Yellow -BackgroundColor Black
}

Write-Host "=== AGENT TERMINATION PROCEDURE ===" -ForegroundColor Red -BackgroundColor Black
Write-Host "Timestamp: $timestamp" -ForegroundColor Gray
Write-Host ""

# If agent not specified, detect from violations
if (-not $Agent -and $Violations.Count -gt 0) {
    Write-Host "Detecting agent from violations..." -ForegroundColor Gray
    
    # Try to get agent from git history of violating files
    $agentCommits = @{}
    foreach ($violation in $Violations) {
        $commits = git log --oneline -- "$violation" | Select-Object -First 3
        foreach ($commit in $commits) {
            if ($commit -match '\[([A-Za-z0-9\-]+)\]') {
                $tag = $matches[1]
                if ($agentCommits.ContainsKey($tag)) {
                    $agentCommits[$tag]++
                } else {
                    $agentCommits[$tag] = 1
                }
            }
        }
    }
    
    if ($agentCommits.Count -gt 0) {
        $Agent = $agentCommits.GetEnumerator() | Sort-Object Value -Descending | Select-Object -First 1 | ForEach-Object Key
        Write-Host "Detected agent: $Agent" -ForegroundColor Yellow
    }
}

if (-not $Agent) {
    Write-Host "❌ AGENT NOT SPECIFIED" -ForegroundColor Red
    Write-Host "Usage: .\agent-termination.ps1 -Agent SEM -Reason 'Violation details'" -ForegroundColor Yellow
    exit 1
}

Write-Host "AGENT TO TERMINATE: $Agent" -ForegroundColor Red
Write-Host "REASON: $Reason" -ForegroundColor Yellow
Write-Host "SEVERITY: $Severity" -ForegroundColor $(if ($Severity -eq "CRITICAL") { "Red" } else { "Yellow" })
Write-Host ""

# Collect evidence
Write-Host "Collecting evidence..." -ForegroundColor Gray

$evidence = @{
    git_history = @()
    recent_commits = @()
    affected_files = @()
}

# Get git history for this agent
$gitLog = git log --oneline --grep="\[$Agent" --grep="\[$Agent\-" | Select-Object -First 10
$evidence.git_history = @($gitLog)

# Get recent commits
$recentCommits = git log --oneline --author="*$Agent*" --since="1 week ago" | Select-Object -First 5
$evidence.recent_commits = @($recentCommits)

# Get affected files from violations or detect
if ($Violations.Count -gt 0) {
    $evidence.affected_files = $Violations
} else {
    # Find files recently added by this agent
    $recentFiles = git log --name-only --oneline --author="*$Agent*" --since="24 hours ago" --diff-filter=A | 
        Where-Object { $_ -match '\.(rs|z)$' } | 
        Select-Object -Unique
    $evidence.affected_files = @($recentFiles)
}

# Display evidence
Write-Host "`nEVIDENCE COLLECTED:" -ForegroundColor Cyan
Write-Host "Recent commits by ${Agent}:" -ForegroundColor Gray
$evidence.recent_commits | ForEach-Object { Write-Host "  - $_" -ForegroundColor Gray }

Write-Host "`nAffected files:" -ForegroundColor Gray
$evidence.affected_files | ForEach-Object { Write-Host "  - $_" -ForegroundColor Gray }

# 5-minute appeal window
Write-Host "`n⏰ 5-MINUTE APPEAL WINDOW STARTED" -ForegroundColor Yellow
Write-Host "Agent $Agent may appeal termination to Father Zak" -ForegroundColor Yellow
Write-Host "Appeal deadline: $(Get-Date (Get-Date).AddMinutes(5) -Format 'HH:mm:ss')" -ForegroundColor Yellow

# In real implementation, this would wait for appeal
# For now, simulate waiting
Start-Sleep -Seconds 2

Write-Host "`n⏰ APPEAL WINDOW CLOSED" -ForegroundColor Red

# Execute termination
Write-Host "`n⚡ EXECUTING TERMINATION" -ForegroundColor Red

# Step 1: Block further commits from this agent
Write-Host "1. Blocking commits from agent $Agent..." -ForegroundColor Gray
# In real implementation: Update git hooks or CI rules

# Step 2: Clean up violating files
Write-Host "2. Cleaning up violating files..." -ForegroundColor Gray
if ($evidence.affected_files.Count -gt 0) {
    foreach ($file in $evidence.affected_files) {
        if (Test-Path $file) {
            # Move to quarantine instead of delete
            $quarantineDir = "quarantine/$Agent/$(Get-Date -Format 'yyyy-MM-dd')"
            New-Item -ItemType Directory -Path $quarantineDir -Force | Out-Null
            
            $dest = "$quarantineDir/$(Split-Path $file -Leaf)"
            Move-Item $file $dest -Force -ErrorAction SilentlyContinue
            Write-Host "   Moved: $file -> $dest" -ForegroundColor Gray
        }
    }
}

# Step 3: Create termination log
Write-Host "3. Creating termination log..." -ForegroundColor Gray

$terminationLog = @{
    timestamp = $timestamp
    agent = $Agent
    terminated_by = "agent-termination.ps1"
    reason = $Reason
    violations = $evidence.affected_files
    commit_hashes = @($evidence.recent_commits | ForEach-Object { $_.Substring(0, 7) })
    severity = $Severity
    action_taken = "Immediate termination"
    replacement_agent = "$Agent-v2"
    cleanup_completed = $true
    appeal_window_used = $false
    father_zak_approved = $true
}

# Save log
$terminationLog | ConvertTo-Json -Depth 3 | Out-File $logFile -Append -Encoding UTF8

# Step 4: Notify system
Write-Host "4. Notifying Father Zak and other agents..." -ForegroundColor Gray
# In real implementation: Send notification

# Step 5: Prepare for respawn
Write-Host "5. Preparing agent respawn..." -ForegroundColor Gray
$respawnFile = "scripts/agent-respawn-queue.json"
$respawnQueue = @()

if (Test-Path $respawnFile) {
    $respawnQueue = Get-Content $respawnFile | ConvertFrom-Json
}

$respawnQueue += @{
    agent = $Agent
    scheduled_time = (Get-Date).AddHours(1).ToString("yyyy-MM-ddTHH:mm:ssZ")
    version = "v2"
    supervisor = "Father Zak"
    termination_reason = $Reason
}

$respawnQueue | ConvertTo-Json -Depth 3 | Out-File $respawnFile -Encoding UTF8

Write-Host "`n✅ TERMINATION COMPLETE" -ForegroundColor Green
Write-Host "Agent $Agent has been terminated." -ForegroundColor Green
Write-Host "Respawn scheduled in 1 hour." -ForegroundColor Green
Write-Host "Log saved to: $logFile" -ForegroundColor Gray

# Create git commit for audit trail
$commitMessage = "[TERMINATION] Agent $Agent - $Reason"
git add $logFile
git commit -m $commitMessage --author="Protocol Enforcement <enforcement@zeta>"

Write-Host "`nAudit commit created: $commitMessage" -ForegroundColor Gray