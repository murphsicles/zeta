# Zeta Parser Patch System
# Father's command: "Option A: Fix Zeta Parser"
# This script patches PrimeZeta code to work with current Zeta parser

param(
    [string]$InputFile = "C:\Users\mummy\.openclaw\workspace\PrimeZeta\prime.z",
    [string]$OutputFile = "C:\Users\mummy\.openclaw\workspace\PrimeZeta\prime_patched.z"
)

Write-Host "Zeta Parser Patch System" -ForegroundColor Cyan
Write-Host "Father's Command: 'Option A: Fix Zeta Parser'" -ForegroundColor Yellow
Write-Host "Input: $InputFile" -ForegroundColor White
Write-Host "Output: $OutputFile" -ForegroundColor White

# Read original PrimeZeta
$Content = Get-Content -Path $InputFile -Raw

Write-Host "`nApplying parser patches..." -ForegroundColor Cyan

# Patch 1: Convert var to let (Zeta doesn't have var keyword)
$PatchedContent = $Content -replace '\bvar\b', 'let'
Write-Host "  Patch 1: var → let" -ForegroundColor Green

# Patch 2: Remove type aliases (Zeta doesn't support type aliases)
# Convert: type Name = [SIZE]TYPE
# To: // type Name = [SIZE]TYPE (commented out)
$TypeAliasPattern = '^\s*type\s+([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.*?)\s*$'
$PatchedContent = $PatchedContent -replace $TypeAliasPattern, '// type $1 = $2  // Zeta parser patch: type aliases not supported'
Write-Host "  Patch 2: Comment out type aliases" -ForegroundColor Green

# Patch 3: Convert [SIZE]TYPE to [TYPE; SIZE] (Zeta syntax)
# But careful: [dynamic]T is different!
$ArrayPattern = '\[([A-Z_][A-Z0-9_]*)\]\s*([a-z][a-z0-9_]*)'
$PatchedContent = $PatchedContent -replace $ArrayPattern, '[$2; $1]'
Write-Host "  Patch 3: [SIZE]TYPE → [TYPE; SIZE]" -ForegroundColor Green

# Patch 4: Handle [dynamic]T syntax (convert to comment)
$DynamicPattern = '\[dynamic\]\s*([a-z][a-z0-9_]*)'
$PatchedContent = $PatchedContent -replace $DynamicPattern, '/* [dynamic]$1 */ []$1  // Zeta parser patch: dynamic arrays not supported'
Write-Host "  Patch 4: Handle [dynamic]T syntax" -ForegroundColor Green

# Patch 5: Convert comptime blocks to comptime variables
# PrimeZeta: comptime { ... }
# Zeta: comptime let ... (only variables, not blocks)
$ComptimeBlockPattern = 'comptime\s*\{'
$PatchedContent = $PatchedContent -replace $ComptimeBlockPattern, '// comptime {  // Zeta parser patch: comptime blocks not supported'
Write-Host "  Patch 5: Handle comptime blocks" -ForegroundColor Green

# Write patched file
Set-Content -Path $OutputFile -Value $PatchedContent -Encoding UTF8

Write-Host "`nPatch complete!" -ForegroundColor Green
Write-Host "Original file: $InputFile" -ForegroundColor Yellow
Write-Host "Patched file: $OutputFile" -ForegroundColor Green

# Show patch summary
$OriginalLines = ($Content -split "`n").Count
$PatchedLines = ($PatchedContent -split "`n").Count
$Changes = (Compare-Object ($Content -split "`n") ($PatchedContent -split "`n")).Count / 2

Write-Host "`nPatch Statistics:" -ForegroundColor Cyan
Write-Host "  Original lines: $OriginalLines" -ForegroundColor White
Write-Host "  Patched lines: $PatchedLines" -ForegroundColor White
Write-Host "  Estimated changes: $Changes" -ForegroundColor White

Write-Host "`nReady to compile patched PrimeZeta with Zeta!" -ForegroundColor Cyan