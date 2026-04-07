Write-Host "Testing failing files with detailed error output..."
Write-Host "================================================"

$failing_files = @(
    "zeta_src/main.z",
    "zeta_src/plan.z", 
    "zeta_src/frontend/parser/stmt.z",
    "zeta_src/runtime/xai.z",
    "zeta_src/runtime/actor/channel.z",
    "zeta_src/runtime/actor/result.z",
    "zeta_src/runtime/actor/scheduler.z"
)

foreach ($file in $failing_files) {
    Write-Host "`n=== Testing: $file ==="
    $output = cargo run --bin debug_parser $file 2>&1
    Write-Host "Exit code: $LASTEXITCODE"
    
    # Look for error messages
    $errorFound = $false
    foreach ($line in $output) {
        if ($line -match "error" -or $line -match "Error" -or $line -match "failed" -or $line -match "Failed") {
            Write-Host "ERROR: $line"
            $errorFound = $true
        }
    }
    
    if (-not $errorFound) {
        Write-Host "No obvious error messages found in output"
        Write-Host "Last 5 lines of output:"
        $output | Select-Object -Last 5 | ForEach-Object { Write-Host "  $_" }
    }
}