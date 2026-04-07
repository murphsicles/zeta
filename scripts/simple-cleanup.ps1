# Simple cleanup script
Write-Host "=== SIMPLE ROOT DIRECTORY CLEANUP ===" -ForegroundColor Cyan
Write-Host ""

# Get files in root that should be moved
$filesToMove = Get-ChildItem -File | Where-Object {
    $_.Extension -in '.rs', '.z' -and
    $_.Name -notin @('Cargo.toml', 'README.md', 'LICENSE', '.gitignore', 'deny.toml')
}

if ($filesToMove.Count -eq 0) {
    Write-Host "✅ No files to move from root directory." -ForegroundColor Green
    exit 0
}

Write-Host "Found $($filesToMove.Count) files in root directory:" -ForegroundColor Yellow
$filesToMove | ForEach-Object { Write-Host "  - $($_.Name)" }

Write-Host "`nWhere should these files go?" -ForegroundColor Yellow
Write-Host "1. test_*.rs, test_*.z -> tests/" -ForegroundColor Gray
Write-Host "2. debug_*.rs, debug_*.z -> tests/debug/" -ForegroundColor Gray
Write-Host "3. Other .rs files -> src/" -ForegroundColor Gray
Write-Host "4. Other .z files -> zeta_src/" -ForegroundColor Gray

Write-Host "`nRun this command to move them:" -ForegroundColor Green
Write-Host "  .\scripts\cleanup-root-files.ps1" -ForegroundColor Green

exit 0