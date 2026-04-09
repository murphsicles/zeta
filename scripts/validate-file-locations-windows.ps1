# Windows-compatible file validation script
param()

Write-Host "=== Zeta Project File Location Validation ===" -ForegroundColor Cyan
Write-Host "Checking file organization compliance..." -ForegroundColor Yellow

$errors = @()
$currentDir = Get-Location

# Check 1: No .rs/.z files in root directory
Write-Host "`n[1/3] Checking for .rs/.z files in root directory..." -ForegroundColor Gray
$rootRsFiles = Get-ChildItem -Path . -Filter "*.rs" -File
$rootZFiles = Get-ChildItem -Path . -Filter "*.z" -File
$rootZetaFiles = Get-ChildItem -Path . -Filter "*.zeta" -File

if ($rootRsFiles.Count -gt 0) {
    $errors += "Found $($rootRsFiles.Count) .rs file(s) in root directory:"
    foreach ($file in $rootRsFiles) {
        $errors += "  - $($file.Name)"
    }
}

if ($rootZFiles.Count -gt 0) {
    $errors += "Found $($rootZFiles.Count) .z file(s) in root directory:"
    foreach ($file in $rootZFiles) {
        $errors += "  - $($file.Name)"
    }
}

if ($rootZetaFiles.Count -gt 0) {
    $errors += "Found $($rootZetaFiles.Count) .zeta file(s) in root directory:"
    foreach ($file in $rootZetaFiles) {
        $errors += "  - $($file.Name)"
    }
}

# Summary
Write-Host "`n=== Validation Results ===" -ForegroundColor Cyan

if ($errors.Count -eq 0) {
    Write-Host "✓ All checks passed! File organization is compliant." -ForegroundColor Green
    exit 0
} else {
    Write-Host "✗ Found $($errors.Count) error(s):" -ForegroundColor Red
    foreach ($error in $errors) {
        Write-Host "  $error" -ForegroundColor Red
    }
    
    Write-Host "`n=== Recommendations ===" -ForegroundColor Cyan
    Write-Host "1. Move .rs/.z/.zeta files from root to appropriate directories"
    Write-Host "2. Ensure all test files are in tests/ directory"
    Write-Host "3. Ensure all source files are in src/ or zeta_src/ directories"
    
    exit 1
}