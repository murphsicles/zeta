# ZETA PROJECT CLEANUP SCRIPT
# Run this in PowerShell as Administrator
# WARNING: This will reorganize and delete files. Review before running.

Write-Host "=========================================" -ForegroundColor Red
Write-Host "ZETA PROJECT CLEANUP - AUTONOMOUS EXECUTION" -ForegroundColor Red
Write-Host "=========================================" -ForegroundColor Red
Write-Host "Current directory: $(Get-Location)" -ForegroundColor Yellow
Write-Host "Files in root: $( (Get-ChildItem -File | Measure-Object).Count )" -ForegroundColor Yellow
Write-Host "" -ForegroundColor White

# Safety check
$confirm = Read-Host "This will reorganize and delete files. Type 'CLEANUP' to proceed"
if ($confirm -ne "CLEANUP") {
    Write-Host "Cleanup cancelled." -ForegroundColor Yellow
    exit 1
}

Write-Host "Starting cleanup..." -ForegroundColor Green

# =========================================
# PHASE 1: CREATE DIRECTORY STRUCTURE
# =========================================
Write-Host "`n=== CREATING DIRECTORY STRUCTURE ===" -ForegroundColor Green

$directories = @(
    "build",
    "build\executables",
    "build\objects", 
    "build\intermediates",
    "tests",
    "tests\unit",
    "tests\integration",
    "tests\artifacts",
    "docs",
    "docs\planning",
    "docs\architecture",
    "docs\thinking",
    "scripts",
    "tools"
)

foreach ($dir in $directories) {
    if (-not (Test-Path $dir)) {
        New-Item -ItemType Directory -Path $dir -Force | Out-Null
        Write-Host "  Created: $dir" -ForegroundColor Green
    } else {
        Write-Host "  Exists: $dir" -ForegroundColor Gray
    }
}

# =========================================
# PHASE 2: MOVE BUILD ARTIFACTS
# =========================================
Write-Host "`n=== MOVING BUILD ARTIFACTS ===" -ForegroundColor Green

# Move executables
$exeFiles = Get-ChildItem -Filter "*.exe" -File
foreach ($file in $exeFiles) {
    $destination = "build\executables\$($file.Name)"
    Move-Item $file.FullName $destination -Force
    Write-Host "  Moved: $($file.Name) → build\executables\" -ForegroundColor Cyan
}

# Move object files
$objectFiles = Get-ChildItem -Filter "*.exe.o", "*.o" -File
foreach ($file in $objectFiles) {
    $destination = "build\objects\$($file.Name)"
    Move-Item $file.FullName $destination -Force
    Write-Host "  Moved: $($file.Name) → build\objects\" -ForegroundColor Cyan
}

# =========================================
# PHASE 3: MOVE DOCUMENTATION
# =========================================
Write-Host "`n=== ORGANIZING DOCUMENTATION ===" -ForegroundColor Green

# Move markdown files to docs/
$mdFiles = Get-ChildItem -Filter "*.md" -File | Where-Object { $_.Name -notmatch "README" }
foreach ($file in $mdFiles) {
    $destination = "docs\$($file.Name)"
    Move-Item $file.FullName $destination -Force
    Write-Host "  Moved: $($file.Name) → docs\" -ForegroundColor Cyan
}

# Move planning documents
$planningFiles = Get-ChildItem -Filter "*SUMMARY*", "*PLAN*", "*LOG*" -File
foreach ($file in $planningFiles) {
    $destination = "docs\planning\$($file.Name)"
    Move-Item $file.FullName $destination -Force
    Write-Host "  Moved: $($file.Name) → docs\planning\" -ForegroundColor Cyan
}

# =========================================
# PHASE 4: MOVE TEST FILES
# =========================================
Write-Host "`n=== ORGANIZING TEST FILES ===" -ForegroundColor Green

# Move test source files
$testFiles = Get-ChildItem -Filter "*test*.z" -File
foreach ($file in $testFiles) {
    $destination = "tests\unit\$($file.Name)"
    Move-Item $file.FullName $destination -Force
    Write-Host "  Moved: $($file.Name) → tests\unit\" -ForegroundColor Cyan
}

# Move test output files
$testOutputs = Get-ChildItem -Filter "*test*.txt", "*output*.txt" -File
foreach ($file in $testOutputs) {
    $destination = "tests\artifacts\$($file.Name)"
    Move-Item $file.FullName $destination -Force
    Write-Host "  Moved: $($file.Name) → tests\artifacts\" -ForegroundColor Cyan
}

# =========================================
# PHASE 5: MOVE SCRIPTS
# =========================================
Write-Host "`n=== ORGANIZING SCRIPTS ===" -ForegroundColor Green

$scriptFiles = Get-ChildItem -Filter "*.ps1", "*.sh", "*.bat" -File
foreach ($file in $scriptFiles) {
    $destination = "scripts\$($file.Name)"
    Move-Item $file.FullName $destination -Force
    Write-Host "  Moved: $($file.Name) → scripts\" -ForegroundColor Cyan
}

# =========================================
# PHASE 6: CLEAN TEMPORARY FILES
# =========================================
Write-Host "`n=== CLEANING TEMPORARY FILES ===" -ForegroundColor Green

# Delete backup files
$backupFiles = Get-ChildItem -Filter "*.bak" -File
foreach ($file in $backupFiles) {
    Remove-Item $file.FullName -Force
    Write-Host "  Deleted: $($file.Name)" -ForegroundColor Red
}

# Delete temporary test files (not in tests/ directory)
$tempFiles = Get-ChildItem -Filter "*_temp*", "*_debug*", "*_tmp*" -File
foreach ($file in $tempFiles) {
    Remove-Item $file.FullName -Force
    Write-Host "  Deleted: $($file.Name)" -ForegroundColor Red
}

# =========================================
# PHASE 7: MOVE SOURCE FILES TO SRC/ STRUCTURE
# =========================================
Write-Host "`n=== ORGANIZING SOURCE FILES ===" -ForegroundColor Green

# First, check what .z files remain in root
$sourceFiles = Get-ChildItem -Filter "*.z" -File
Write-Host "  Found $($sourceFiles.Count) .z files in root" -ForegroundColor Yellow

# These should be moved to appropriate src/ subdirectories
# For now, move them to src/ (manual organization needed later)
foreach ($file in $sourceFiles) {
    $destination = "src\$($file.Name)"
    Move-Item $file.FullName $destination -Force
    Write-Host "  Moved: $($file.Name) → src\" -ForegroundColor Cyan
}

# =========================================
# PHASE 8: FINAL VERIFICATION
# =========================================
Write-Host "`n=== FINAL VERIFICATION ===" -ForegroundColor Green

Write-Host "`nFiles remaining in root:" -ForegroundColor White
Get-ChildItem -File | Select-Object Name, Length | Format-Table -AutoSize

Write-Host "`nDirectory structure:" -ForegroundColor White
Get-ChildItem -Directory | Select-Object Name | Format-Table -AutoSize

Write-Host "`nFile counts by directory:" -ForegroundColor White
$dirs = @(".", "src", "build", "tests", "docs", "scripts")
foreach ($dir in $dirs) {
    $count = (Get-ChildItem -Path $dir -File -Recurse -ErrorAction SilentlyContinue | Measure-Object).Count
    Write-Host "  $dir : $count files" -ForegroundColor Cyan
}

# =========================================
# SUMMARY
# =========================================
Write-Host "`n=========================================" -ForegroundColor Green
Write-Host "CLEANUP COMPLETE" -ForegroundColor Green
Write-Host "=========================================" -ForegroundColor Green

Write-Host "`nNext steps:" -ForegroundColor Yellow
Write-Host "1. Review src/ directory - organize .z files into subdirectories" -ForegroundColor White
Write-Host "2. Check build/ directory - verify no source files moved there" -ForegroundColor White
Write-Host "3. Update .gitignore for new structure" -ForegroundColor White
Write-Host "4. Commit cleaned structure to git" -ForegroundColor White

Write-Host "`nCleanup script completed at $(Get-Date)" -ForegroundColor Green