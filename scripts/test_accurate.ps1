Write-Host "Accurate compilation testing..."
Write-Host "================================"

$files = Get-ChildItem -Path .\zeta_src -File -Recurse
$total = $files.Count
$compiles = 0
$errors = 0

$results = @()

foreach ($file in $files) {
    Write-Host "Testing: $($file.Name)"
    
    # Try to parse the file
    $output = cargo run --bin debug_parser $file.FullName 2>&1
    $exitCode = $LASTEXITCODE
    
    if ($exitCode -eq 0) {
        # Check if the output contains actual parse errors (not just the word "error" in comments)
        $outputStr = $output -join "`n"
        
        # Look for actual error patterns in the parser output
        # The debug_parser outputs "Result: Ok" for successful parses
        if ($outputStr -match "Result: Ok\(") {
            $status = "COMPILES"
            $compiles++
        } else {
            $status = "PARSING ERROR"
            $errors++
        }
    } else {
        $status = "PARSING ERROR (exit code $exitCode)"
        $errors++
    }
    
    $result = [PSCustomObject]@{
        File = $file.Name
        Status = $status
        Path = $file.FullName
    }
    
    $results += $result
    Write-Host "  $status"
}

Write-Host "`n=========================================="
Write-Host "SUMMARY:"
Write-Host "Total files: $total"
Write-Host "COMPILES: $compiles"
Write-Host "PARSING ERRORS: $errors"

$percentage = [math]::Round(($compiles / $total) * 100, 2)
Write-Host "`nCompilation percentage: $percentage%"

# Compare to v0.3.21 baseline
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

# Save detailed results
$results | Export-Csv -Path "accurate_compilation_results.csv" -NoTypeInformation
Write-Host "`nDetailed results saved to accurate_compilation_results.csv"