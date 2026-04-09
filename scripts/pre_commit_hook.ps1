# pre_commit_hook.ps1
# Git pre-commit hook for GitHub protocol compliance

param(
    [string]$GitDir = ".git"
)

$ErrorActionPreference = "Stop"

Write-Host "🔍 Running GitHub Protocol Pre-Commit Check..." -ForegroundColor Cyan

# Run validation script
$validationScript = Join-Path $PSScriptRoot "validate_repository_structure.ps1"
if (-not (Test-Path $validationScript)) {
    Write-Host "❌ Validation script not found: $validationScript" -ForegroundColor Red
    exit 1
}

# Run validation in report-only mode first
$result = & $validationScript -Strict
$validationExitCode = $LASTEXITCODE

if ($validationExitCode -eq 0) {
    Write-Host "✅ Repository structure is compliant with GitHub protocol." -ForegroundColor Green
    exit 0
} elseif ($validationExitCode -eq 1) {
    Write-Host "❌ Repository structure violations detected!" -ForegroundColor Red
    Write-Host "`n💡 Violations must be fixed before committing." -ForegroundColor Yellow
    Write-Host "   Run: powershell -File scripts/validate_repository_structure.ps1 -Fix" -ForegroundColor Yellow
    Write-Host "   Then commit again." -ForegroundColor Yellow
    exit 1
} elseif ($validationExitCode -eq 2) {
    Write-Host "❌ Error running validation script." -ForegroundColor Red
    Write-Host "   Check the script and try again." -ForegroundColor Yellow
    exit 1
} else {
    Write-Host "❌ Unknown validation error (exit code: $validationExitCode)" -ForegroundColor Red
    exit 1
}