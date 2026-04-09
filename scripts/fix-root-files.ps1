# Fix root files - simple version
Write-Host "Fixing files in root directory..." -ForegroundColor Cyan

# Move test_generic_arity.rs to tests/
$file = "test_generic_arity.rs"
if (Test-Path $file) {
    Write-Host "Moving $file to tests/" -ForegroundColor Gray
    if (-not (Test-Path "tests")) {
        New-Item -ItemType Directory -Path "tests" -Force | Out-Null
    }
    Move-Item $file "tests/" -Force
    Write-Host "✅ Moved $file to tests/" -ForegroundColor Green
}

# Check for other files
$otherFiles = Get-ChildItem -Filter "*.rs" | Where-Object { 
    $_.Name -notin @('Cargo.toml', 'README.md') -and
    $_.Name -ne 'test_generic_arity.rs'
}

foreach ($f in $otherFiles) {
    Write-Host "Found: $($f.Name) - should be in src/ or tests/" -ForegroundColor Yellow
}

Write-Host "`nCleanup complete." -ForegroundColor Green
Write-Host "Run validation to check: .\validate-agent-files.ps1" -ForegroundColor Gray