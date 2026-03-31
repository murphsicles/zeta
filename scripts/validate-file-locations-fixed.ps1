# validate-file-locations-fixed.ps1
# Validation script for Zeta project file locations

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

# Check 2: All tests in tests/ directory
Write-Host "`n[2/3] Checking test file locations..." -ForegroundColor Gray
$allTestFiles = Get-ChildItem -Path . -Recurse -Filter "*test*.rs" -File
$testFilesOutsideTests = @()

foreach ($file in $allTestFiles) {
    $dir = $file.DirectoryName
    if ($dir -notlike "*\tests*" -and $dir -notlike "*\target*" -and $dir -ne $currentDir.Path) {
        $testFilesOutsideTests += $file
    }
}

if ($testFilesOutsideTests.Count -gt 0) {
    $errors += "Found $($testFilesOutsideTests.Count) test file(s) outside tests/ directory:"
    foreach ($file in $testFilesOutsideTests) {
        $relativePath = $file.FullName.Replace("$currentDir\", "")
        $errors += "  - $relativePath"
    }
}

# Check 3: All source in src/ or zeta_src/
Write-Host "`n[3/3] Checking source file locations..." -ForegroundColor Gray
$allSourceFiles = Get-ChildItem -Path . -Recurse -Filter "*.rs" -File
$misplacedSourceFiles = @()

foreach ($file in $allSourceFiles) {
    $dir = $file.DirectoryName
    # Skip certain directories
    if ($dir -like "*\target*" -or $dir -like "*\tests*" -or $dir -like "*\examples*" -or $dir -like "*\benches*" -or $dir -like "*\benchmarks*" -or $dir -like "*\scripts*" -or $dir -like "*\.*") {
        continue
    }
    
    # Check if in allowed source directories
    if ($dir -notlike "*\src*" -and $dir -notlike "*\zeta_src*") {
        $misplacedSourceFiles += $file
    }
}

if ($misplacedSourceFiles.Count -gt 0) {
    $errors += "Found $($misplacedSourceFiles.Count) source file(s) outside src/ or zeta_src/ directories:"
    foreach ($file in $misplacedSourceFiles) {
        $relativePath = $file.FullName.Replace("$currentDir\", "")
        $errors += "  - $relativePath"
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