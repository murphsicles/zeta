# pipeline_status.ps1
# Shows release pipeline status

$PipelineFile = ".github/automation/release_pipeline.json"

if (-not (Test-Path $PipelineFile)) {
    Write-Host "Pipeline not initialized." -ForegroundColor Red
    exit 1
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