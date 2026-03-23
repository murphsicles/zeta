# ZETA PROJECT CLEANUP SCRIPT
# Run this in PowerShell as Administrator

Write-Host "=========================================" -ForegroundColor Red
Write-Host "ZETA PROJECT CLEANUP" -ForegroundColor Red
Write-Host "=========================================" -ForegroundColor Red

$startTime = Get-Date
Write-Host "Start time: $startTime" -ForegroundColor Yellow
Write-Host "Current directory: $(Get-Location)" -ForegroundColor Yellow
Write-Host "" -ForegroundColor White

# =========================================
# STEP 1: CREATE DIRECTORY STRUCTURE
# =========================================
Write-Host "=== CREATING DIRECTORY STRUCTURE ===" -ForegroundColor Green

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
    }
}

# =========================================
# STEP 2: MOVE BUILD ARTIFACTS
# =========================================
Write-Host "`n=== MOVING BUILD ARTIFACTS ===" -ForegroundColor Green

# Move executables
$exeFiles = Get-ChildItem -Filter "*.exe" -File
Write-Host "  Moving $($exeFiles.Count) .exe files..." -ForegroundColor White
foreach ($file in $exeFiles) {
    Move-Item $file.FullName "build\executables\" -Force
}

# Move object files
$objectFiles = Get-ChildItem -File | Where-Object { $_.Extension -in @(".o", ".exe.o") }
Write-Host "  Moving $($objectFiles.Count) .o files..." -ForegroundColor White
foreach ($file in $objectFiles) {
    Move-Item $file.FullName "build\objects\" -Force
}

# =========================================
# STEP 3: DELETE BACKUP FILES
# =========================================
Write-Host "`n=== DELETING BACKUP FILES ===" -ForegroundColor Green

$backupFiles = Get-ChildItem -Filter "*.bak" -File
Write-Host "  Deleting $($backupFiles.Count) .bak files..." -ForegroundColor White
foreach ($file in $backupFiles) {
    Remove-Item $file.FullName -Force
}

# =========================================
# STEP 4: MOVE DOCUMENTATION
# =========================================
Write-Host "`n=== ORGANIZING DOCUMENTATION ===" -ForegroundColor Green

$docFiles = Get-ChildItem -Filter "*.md" -File | Where-Object { $_.Name -notmatch "README" }
Write-Host "  Moving $($docFiles.Count) .md files..." -ForegroundColor White
foreach ($file in $docFiles) {
    Move-Item $file.FullName "docs\" -Force
}

# =========================================
# STEP 5: MOVE SCRIPTS
# =========================================
Write-Host "`n=== ORGANIZING SCRIPTS ===" -ForegroundColor Green

$scriptFiles = Get-ChildItem -File | Where-Object { $_.Extension -in @(".ps1", ".bat", ".sh", ".cmd") }
Write-Host "  Moving $($scriptFiles.Count) script files..." -ForegroundColor White
foreach ($file in $scriptFiles) {
    Move-Item $file.FullName "scripts\" -Force
}

# =========================================
# STEP 6: VERIFICATION
# =========================================
Write-Host "`n=== VERIFICATION ===" -ForegroundColor Green

Write-Host "`nFiles remaining in root:" -ForegroundColor White
Get-ChildItem -File | Select-Object Name, Length | Format-Table -AutoSize

Write-Host "`nFile counts by directory:" -ForegroundColor White
$dirs = @(".", "build", "docs", "scripts", "src")
foreach ($dir in $dirs) {
    $count = (Get-ChildItem -Path $dir -File -Recurse -ErrorAction SilentlyContinue | Measure-Object).Count
    Write-Host "  $dir : $count files" -ForegroundColor Cyan
}

# =========================================
# SUMMARY
# =========================================
$endTime = Get-Date
$duration = $endTime - $startTime

Write-Host "`n=========================================" -ForegroundColor Green
Write-Host "CLEANUP COMPLETE" -ForegroundColor Green
Write-Host "=========================================" -ForegroundColor Green

Write-Host "`nDuration: $($duration.TotalSeconds) seconds" -ForegroundColor Yellow
Write-Host "Cleanup completed at: $endTime" -ForegroundColor Yellow

Write-Host "`nNext steps:" -ForegroundColor White
Write-Host "1. Organize .z files into src/ subdirectories" -ForegroundColor Cyan
Write-Host "2. Update .gitignore for new structure" -ForegroundColor Cyan
Write-Host "3. Commit cleaned structure to git" -ForegroundColor Cyan