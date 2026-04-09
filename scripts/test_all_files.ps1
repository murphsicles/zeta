Write-Host "Testing compilation of all 41 Zeta files..."
Write-Host "=========================================="

$files = Get-ChildItem -Path .\zeta_src -File -Recurse
$total = $files.Count
$compiles = 0
$parsing_errors = 0
$codegen_errors = 0
$other_errors = 0

$results = @()

foreach ($file in $files) {
    Write-Host "Testing: $($file.Name)"
    
    # Try to parse the file
    $output = cargo run --bin debug_parser $file.FullName 2>&1
    
    # Check if parsing succeeded
    if ($LASTEXITCODE -eq 0) {
        # Check for specific error messages in output
        $outputStr = $output -join "`n"
        
        if ($outputStr -match "clone_i64" -or $outputStr -match "is_null_i64" -or $outputStr -match "to_string_str") {
            $status = "CODEGEN ERRORS"
            $codegen_errors++
        } elseif ($outputStr -match "error" -or $outputStr -match "Error" -or $outputStr -match "failed" -or $outputStr -match "Failed") {
            $status = "OTHER ERRORS"
            $other_errors++
        } else {
            $status = "COMPILES"
            $compiles++
        }
    } else {
        $status = "PARSING ERRORS"
        $parsing_errors++
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
Write-Host "PARSING ERRORS: $parsing_errors"
Write-Host "CODEGEN ERRORS: $codegen_errors"
Write-Host "OTHER ERRORS: $other_errors"

$percentage = [math]::Round(($compiles / $total) * 100, 2)
Write-Host "`nCompilation percentage: $percentage%"

Write-Host "`nDETAILED RESULTS:"
foreach ($result in $results) {
    Write-Host "$($result.Status) - $($result.File)"
}

# Save results to file
$results | ConvertTo-Json | Out-File -FilePath "compilation_results.json" -Encoding UTF8
Write-Host "`nResults saved to compilation_results.json"