# PRE-COMMIT VALIDATION SCRIPT
# Runs automatically before any git commit
# Prevents protocol violations from being committed

Write-Host "=== PRE-COMMIT PROTOCOL VALIDATION ===" -ForegroundColor Cyan
Write-Host "Time: $(Get-Date -Format 'HH:mm:ss')"
Write-Host ""

$violations = @()
$errors = 0
$warnings = 0

# 1. CHECK FOR FILES IN ROOT DIRECTORY
Write-Host "1. Checking for files in root directory..." -ForegroundColor Yellow
$rootFiles = Get-ChildItem -File -Path "." -Exclude @("Cargo.toml", "Cargo.lock", ".gitignore", "README.md", "LICENSE", "deny.toml", ".zeta_*", "AGENTS.md", "IDENTITY.md", "SOUL.md", "TOOLS.md", "USER.md", "HEARTBEAT.md", "MEMORY.md", "WORK_QUEUE.md")

foreach ($file in $rootFiles) {
    $violations += "❌ File in root: $($file.Name)"
    $errors++
}

# 2. CHECK FOR WORKSPACE FILES
Write-Host "2. Checking for workspace files..." -ForegroundColor Yellow
$workspaceFiles = @("AGENTS.md", "IDENTITY.md", "SOUL.md", "TOOLS.md", "USER.md", "HEARTBEAT.md", "MEMORY.md", "WORK_QUEUE.md")
foreach ($file in $workspaceFiles) {
    if (Test-Path $file) {
        $violations += "❌ SECURITY: Workspace file in repository: $file"
        $errors++
    }
}

# 3. CHECK FOR TEST FILES IN WRONG LOCATIONS
Write-Host "3. Checking test file locations..." -ForegroundColor Yellow
$testFilesInWrongPlace = Get-ChildItem -File -Path "." -Filter "*.z" -ErrorAction SilentlyContinue
foreach ($file in $testFilesInWrongPlace) {
    $violations += "❌ Test file in root: $($file.Name) (should be in tests/ directory)"
    $errors++
}

# 4. CHECK FOR RELEASE NOTES IN ROOT
Write-Host "4. Checking release note locations..." -ForegroundColor Yellow
$releaseNotesInRoot = Get-ChildItem -File -Path "." -Filter "RELEASE_*.md" -ErrorAction SilentlyContinue
foreach ($file in $releaseNotesInRoot) {
    $violations += "⚠️ Release note in root: $($file.Name) (should be in docs/releases/)"
    $warnings++
}

# 5. VALIDATION SUMMARY
Write-Host ""
Write-Host "=== VALIDATION RESULTS ===" -ForegroundColor Cyan
Write-Host "Errors: $errors" -ForegroundColor $(if ($errors -gt 0) { "Red" } else { "Green" })
Write-Host "Warnings: $warnings" -ForegroundColor $(if ($warnings -gt 0) { "Yellow" } else { "Green" })

if ($violations.Count -gt 0) {
    Write-Host ""
    Write-Host "=== PROTOCOL VIOLATIONS FOUND ===" -ForegroundColor Red
    foreach ($violation in $violations) {
        Write-Host $violation
    }
    
    Write-Host ""
    Write-Host "❌ COMMIT BLOCKED: Fix violations before committing" -ForegroundColor Red
    Write-Host "Suggested fixes:" -ForegroundColor Yellow
    Write-Host "  • Move .z files to tests/ directory"
    Write-Host "  • Remove workspace files (AGENTS.md, IDENTITY.md, etc.)"
    Write-Host "  • Move release notes to docs/releases/"
    Write-Host "  • Keep root directory clean"
    
    exit 1
} else {
    Write-Host ""
    Write-Host "✅ All protocols validated successfully!" -ForegroundColor Green
    Write-Host "Ready to commit." -ForegroundColor Green
    exit 0
}