# release_pipeline_simple.ps1
# Simplified automated release pipeline

param(
    [string]$Action = "status"
)

$PipelineFile = ".github/automation/release_pipeline.json"

function Get-CurrentTimestamp {
    return (Get-Date).ToUniversalTime().ToString("yyyy-MM-ddTHH:mm:ssZ")
}

function Initialize-Pipeline {
    Write-Host "Initializing release pipeline..." -ForegroundColor Cyan
    
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
    }
    
    $json = $initialState | ConvertTo-Json -Depth 10
    Set-Content -Path $PipelineFile -Value $json
    Write-Host "✓ Pipeline initialized" -ForegroundColor Green
}

function Show-Status {
    if (-not (Test-Path $PipelineFile)) {
        Write-Host "Pipeline not initialized. Run with -Action init first." -ForegroundColor Red
        return
    }
    
    $pipeline = Get-Content $PipelineFile -Raw | ConvertFrom-Json
    
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
    Write-Host "Statistics:" -ForegroundColor Cyan
    Write-Host "  Spawn Attempts:" -ForegroundColor Yellow -NoNewline
    Write-Host " $($pipeline.spawn_attempts)/$($pipeline.max_spawn_attempts)" -ForegroundColor White
    Write-Host "  Last Updated:" -ForegroundColor Yellow -NoNewline
    Write-Host " $($pipeline.last_updated)" -ForegroundColor Gray
}

# Main execution
switch ($Action.ToLower()) {
    "status" {
        Show-Status
    }
    "init" {
        Initialize-Pipeline
    }
    default {
        Write-Host "Usage:" -ForegroundColor Cyan
        Write-Host "  .\release_pipeline_simple.ps1 -Action status    # Show pipeline status" -ForegroundColor Gray
        Write-Host "  .\release_pipeline_simple.ps1 -Action init      # Initialize pipeline" -ForegroundColor Gray
    }
}