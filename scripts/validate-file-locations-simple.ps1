# Simple validation script
Write-Host "Testing file validation..."

# Check for .rs files in root
$rootFiles = Get-ChildItem -Path . -Filter "*.rs" -File
if ($rootFiles.Count -gt 0) {
    Write-Host "Found $($rootFiles.Count) .rs files in root" -ForegroundColor Red
    exit 1
} else {
    Write-Host "No .rs files in root - OK" -ForegroundColor Green
    exit 0
}