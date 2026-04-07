# Simple factory status check
# Run this to prevent stalls

$statusFile = ".github/automation/agent_status.json"

# Create status if missing
if (-not (Test-Path $statusFile)) {
    @"
{
  "factory_status": "ACTIVE",
  "last_heartbeat": "$(Get-Date -Format 'yyyy-MM-ddTHH:mm:ssZ')",
  "agents": [],
  "alerts": ["Initialized at $(Get-Date -Format 'HH:mm:ss')"],
  "release_pipeline": {
    "current": "v0.3.29",
    "queue": ["v0.3.30", "v0.3.31", "v0.3.32", "v0.3.33", "v0.3.34"],
    "status": "ACTIVE"
  }
}
"@ | Out-File $statusFile -Encoding UTF8
}

# Load and update
$status = Get-Content $statusFile -Raw | ConvertFrom-Json
$last = [DateTime]::Parse($status.last_heartbeat)
$idle = [Math]::Round(((Get-Date) - $last).TotalMinutes, 2)

Write-Host "Factory check: $idle minutes idle, status: $($status.factory_status)"

# Update heartbeat
$status.last_heartbeat = (Get-Date).ToString("yyyy-MM-ddTHH:mm:ssZ")
$status | ConvertTo-Json | Out-File $statusFile -Encoding UTF8

Write-Host "Heartbeat updated"
Write-Host ""