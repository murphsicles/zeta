# PowerShell script to convert PrimeZeta array syntax from [SIZE]TYPE to [TYPE; SIZE]
# Father's command: "If PrimeZeta is wrong, we can edit the file"

param(
    [string]$FilePath = "C:\Users\mummy\.openclaw\workspace\PrimeZeta\prime.z"
)

Write-Host "Converting PrimeZeta array syntax from [SIZE]TYPE to [TYPE; SIZE]..." -ForegroundColor Cyan
Write-Host "File: $FilePath" -ForegroundColor Yellow

# Backup original file
$BackupFile = "$FilePath.backup_$(Get-Date -Format 'yyyyMMdd_HHmmss')"
Copy-Item -Path $FilePath -Destination $BackupFile
Write-Host "Backup created: $BackupFile" -ForegroundColor Green

# Read file content
$Content = Get-Content -Path $FilePath -Raw

# Pattern 1: Simple arrays [SIZE]TYPE (most common)
# Example: [NUM_RESIDUES]u64 -> [u64; NUM_RESIDUES]
# Only match uppercase size identifiers followed by lowercase type names
$Pattern1 = '\[([A-Z_][A-Z0-9_]*)\]\s*([a-z][a-z0-9_]*\b)'
$Replacement1 = '[$2; $1]'

# Pattern 2: Arrays in function return types -> [SIZE]TYPE
# Example: -> [NUM_RESIDUES]u64 { -> -> [u64; NUM_RESIDUES] {
$Pattern2 = '->\s*\[([A-Z_][A-Z0-9_]*)\]\s*([a-z][a-z0-9_]*)'
$Replacement2 = '-> [$2; $1]'

# Pattern 3: Multi-dimensional arrays (complex)
# Example: [[NUM_RESIDUES]u16; NUM_RESIDUES] -> [[u16; NUM_RESIDUES]; NUM_RESIDUES]
# This requires recursive processing

Write-Host "`nConverting simple array declarations..." -ForegroundColor Cyan
$ConvertedContent = $Content -replace $Pattern1, $Replacement1
$ConvertedContent = $ConvertedContent -replace $Pattern2, $Replacement2

# Count conversions
$OriginalLines = ($Content -split "`n").Count
$ConvertedLines = ($ConvertedContent -split "`n").Count
$Changes = (Compare-Object ($Content -split "`n") ($ConvertedContent -split "`n")).Count / 2

Write-Host "`nConversion Statistics:" -ForegroundColor Cyan
Write-Host "  Original lines: $OriginalLines" -ForegroundColor White
Write-Host "  Converted lines: $ConvertedLines" -ForegroundColor White
Write-Host "  Estimated changes: $Changes" -ForegroundColor White

# Write converted file
Set-Content -Path $FilePath -Value $ConvertedContent -Encoding UTF8

Write-Host "`nConversion complete!" -ForegroundColor Green
Write-Host "Original backed up to: $BackupFile" -ForegroundColor Yellow
Write-Host "Converted file: $FilePath" -ForegroundColor Green

# Show sample of changes
Write-Host "`nSample changes:" -ForegroundColor Cyan
$SampleOriginal = ($Content -split "`n")[0..10] -join "`n"
$SampleConverted = ($ConvertedContent -split "`n")[0..10] -join "`n"

Write-Host "`nBEFORE (first 10 lines):" -ForegroundColor Yellow
$SampleOriginal

Write-Host "`nAFTER (first 10 lines):" -ForegroundColor Green  
$SampleConverted

Write-Host "`nReady to compile PrimeZeta with Zeta syntax!" -ForegroundColor Cyan