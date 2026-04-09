# Test current PrimeZeta compilation status
Write-Host "=== CURRENT PRIMEZETA COMPILATION STATUS ===" -ForegroundColor Cyan
Write-Host "Testing all PrimeZeta .z files" -ForegroundColor White
Write-Host ""

$ZetaCompiler = ".\target\release\zetac.exe"
$PrimeZetaDir = ".\PrimeZeta"

# Get all .z files
$Files = Get-ChildItem -Path $PrimeZetaDir -Filter *.z | Sort-Object Name

$Total = $Files.Count
$Passed = 0
$Failed = 0
$Partial = 0

foreach ($File in $Files) {
    Write-Host "Testing: $($File.Name)" -ForegroundColor White -NoNewline
    Write-Host " (" -NoNewline
    Write-Host "$($File.Length) bytes" -ForegroundColor Gray -NoNewline
    Write-Host ") " -NoNewline
    
    $Output = & $ZetaCompiler $File.FullName 2>&1
    $ExitCode = $LASTEXITCODE
    
    if ($ExitCode -eq 0) {
        # Check if it actually has a main function
        if ($Output -match "No main function") {
            Write-Host "⚠️ PARTIAL (no main)" -ForegroundColor Yellow
            $Partial++
        } else {
            Write-Host "✅ PASSED" -ForegroundColor Green
            $Passed++
        }
    } else {
        Write-Host "❌ FAILED" -ForegroundColor Red
        $Failed++
        
        # Show error type
        if ($Output -match "Incomplete parse") {
            Write-Host "   Error: Incomplete parse" -ForegroundColor Red
        } elseif ($Output -match "Typecheck failed") {
            Write-Host "   Error: Type check failed" -ForegroundColor Red
        } elseif ($Output -match "Type error") {
            Write-Host "   Error: Type error" -ForegroundColor Red
        }
    }
}

Write-Host ""
Write-Host "=== SUMMARY ===" -ForegroundColor Cyan
Write-Host "Total files: $Total" -ForegroundColor White
Write-Host "Passed: $Passed" -ForegroundColor Green
Write-Host "Partial: $Partial" -ForegroundColor Yellow
Write-Host "Failed: $Failed" -ForegroundColor Red

$SuccessRate = [math]::Round(($Passed / $Total) * 100, 1)
Write-Host "Success rate: ${SuccessRate}%" -ForegroundColor Cyan

Write-Host ""
Write-Host "=== FILES THAT COMPILE ===" -ForegroundColor Cyan
foreach ($File in $Files) {
    $Output = & $ZetaCompiler $File.FullName 2>&1 > $null
    if ($LASTEXITCODE -eq 0) {
        Write-Host "  $($File.Name)" -ForegroundColor Green
    }
}

Write-Host ""
Write-Host "=== FILES THAT FAIL ===" -ForegroundColor Cyan
foreach ($File in $Files) {
    $Output = & $ZetaCompiler $File.FullName 2>&1 > $null
    if ($LASTEXITCODE -ne 0) {
        Write-Host "  $($File.Name)" -ForegroundColor Red
    }
}