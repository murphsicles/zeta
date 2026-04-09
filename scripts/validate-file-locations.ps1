# validate-file-locations.ps1
# Validation script for Zeta project file locations
# Checks protocol compliance for file organization

Write-Host "=== Zeta Project File Location Validation ===" -ForegroundColor Cyan
Write-Host "Checking file organization compliance..." -ForegroundColor Yellow

$errors = @()
$warnings = @()

# Get current directory
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
$testFilesOutsideTests = Get-ChildItem -Path . -Recurse -Filter "*test*.rs" -File | 
    Where-Object { $_.DirectoryName -notlike "*\tests*" -and $_.DirectoryName -notlike "*\target*" -and $_.DirectoryName -ne $currentDir.Path }

if ($testFilesOutsideTests.Count -gt 0) {
    $errors += "Found $($testFilesOutsideTests.Count) test file(s) outside tests/ directory:"
    foreach ($file in $testFilesOutsideTests) {
        $relativePath = $file.FullName.Substring($currentDir.Path.Length + 1)
        $errors += "  - $relativePath"
    }
}

# Check 3: All source in src/ or zeta_src/
Write-Host "`n[3/3] Checking source file locations..." -ForegroundColor Gray
$sourceFiles = Get-ChildItem -Path . -Recurse -Filter "*.rs" -File | 
    Where-Object { $_.DirectoryName -notlike "*\target*" -and $_.DirectoryName -notlike "*\tests*" -and $_.DirectoryName -notlike "*\examples*" -and $_.DirectoryName -notlike "*\benches*" -and $_.DirectoryName -notlike "*\benchmarks*" -and $_.DirectoryName -notlike "*\scripts*" -and $_.DirectoryName -notlike "*\.*" }

$misplacedSourceFiles = @()
foreach ($file in $sourceFiles) {
    $dir = $file.DirectoryName
    if ($dir -notlike "*\src*" -and $dir -notlike "*\zeta_src*" -and $dir -ne "$currentDir\scripts") {
        $misplacedSourceFiles += $file
    }
}

if ($misplacedSourceFiles.Count -gt 0) {
    $errors += "Found $($misplacedSourceFiles.Count) source file(s) outside src/ or zeta_src/ directories:"
    foreach ($file in $misplacedSourceFiles) {
        $relativePath = $file.FullName.Substring($currentDir.Path.Length + 1)
        $errors += "  - $relativePath"
    }
}

# Summary
Write-Host "`n=== Validation Results ===" -ForegroundColor Cyan

if ($errors.Count -eq 0 -and $warnings.Count -eq 0) {
    Write-Host "✓ All checks passed! File organization is compliant." -ForegroundColor Green
    exit 0
} else {
    if ($errors.Count -gt 0) {
        Write-Host "✗ Found $($errors.Count) error(s):" -ForegroundColor Red
        foreach ($error in $errors) {
            Write-Host "  $error" -ForegroundColor Red
        }
    }
    
    if ($warnings.Count -gt 0) {
        Write-Host "`n⚠ Found $($warnings.Count) warning(s):" -ForegroundColor Yellow
        foreach ($warning in $warnings) {
            Write-Host "  $warning" -ForegroundColor Yellow
        }
    }
    
    Write-Host "`n=== Recommendations ===" -ForegroundColor Cyan
    Write-Host "1. Move .rs/.z/.zeta files from root to appropriate directories"
    Write-Host "2. Ensure all test files are in tests/ directory"
    Write-Host "3. Ensure all source files are in src/ or zeta_src/ directories"
    
    exit 1
}