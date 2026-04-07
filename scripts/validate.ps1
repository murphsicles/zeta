Write-Host "File validation check"
$files = Get-ChildItem -Path . -Filter "*.rs" -File
if ($files.Count -gt 0) {
    Write-Host "Found $($files.Count) .rs files in root"
    foreach ($f in $files) { Write-Host "  - $($f.Name)" }
    exit 1
}
Write-Host "OK - No .rs files in root"
exit 0