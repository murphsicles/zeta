# Cleanup script for build artifacts
Write-Host "Cleaning up build artifacts..." -ForegroundColor Yellow

# Remove .exe files
Get-ChildItem -Path . -Filter *.exe -File | Where-Object { $_.Name -ne "zetac.exe" } | ForEach-Object {
    Write-Host "Removing: $($_.Name)" -ForegroundColor Gray
    Remove-Item $_.FullName -Force
}

# Remove .pdb files
Get-ChildItem -Path . -Filter *.pdb -File | ForEach-Object {
    Write-Host "Removing: $($_.Name)" -ForegroundColor Gray
    Remove-Item $_.FullName -Force
}

# Remove .o files
Get-ChildItem -Path . -Filter *.o -File | ForEach-Object {
    Write-Host "Removing: $($_.Name)" -ForegroundColor Gray
    Remove-Item $_.FullName -Force
}

# Remove .ll files
Get-ChildItem -Path . -Filter *.ll -File | ForEach-Object {
    Write-Host "Removing: $($_.Name)" -ForegroundColor Gray
    Remove-Item $_.FullName -Force
}

# Remove test output directories
$testDirs = @("test_benchmark", "nour", "Primes", "PrimeZeta")
foreach ($dir in $testDirs) {
    if (Test-Path $dir) {
        Write-Host "Removing directory: $dir" -ForegroundColor Gray
        Remove-Item $dir -Recurse -Force
    }
}

# Remove temporary files
$tempFiles = @("error.txt", "output.txt")
foreach ($file in $tempFiles) {
    if (Test-Path $file) {
        Write-Host "Removing: $file" -ForegroundColor Gray
        Remove-Item $file -Force
    }
}

Write-Host "Cleanup completed!" -ForegroundColor Green