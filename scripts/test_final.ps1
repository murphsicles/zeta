Write-Host "Final compilation testing..."
Write-Host "============================"

$files = Get-ChildItem -Path .\zeta_src -File -Recurse
$total = $files.Count
$compiles = 0
$errors = 0

foreach ($file in $files) {
    Write-Host "Testing: $($file.Name)"
    
    $output = cargo run --bin debug_parser $file.FullName 2>&1
    $exitCode = $LASTEXITCODE
    
    if ($exitCode -eq 0) {
        $outputStr = $output -join "`n"
        if ($outputStr -match "Result: Ok\(") {
            Write-Host "  COMPILES"
            $compiles++
        } else {
            Write-Host "  PARSING ERROR"
            $errors++
        }
    } else {
        Write-Host "  PARSING ERROR (exit code $exitCode)"
        $errors++
    }
}

Write-Host "`n=========================================="
Write-Host "SUMMARY:"
Write-Host "Total files: $total"
Write-Host "COMPILES: $compiles"
Write-Host "PARSING ERRORS: $errors"

$percentage = [math]::Round(($compiles / $total) * 100, 2)
Write-Host "`nCompilation percentage: $percentage%"

$baselinePercentage = 83.00
$improvement = $percentage - $baselinePercentage

Write-Host "`nCOMPARISON TO v0.3.21:"
Write-Host "v0.3.21 baseline: $baselinePercentage%"
Write-Host "Current: $percentage%"
Write-Host "Change: $improvement percentage points"

if ($improvement -gt 0) {
    Write-Host "STATUS: IMPROVEMENT! ✓"
} elseif ($improvement -eq 0) {
    Write-Host "STATUS: NO CHANGE"
} else {
    Write-Host "STATUS: REGRESSION! ✗"
}