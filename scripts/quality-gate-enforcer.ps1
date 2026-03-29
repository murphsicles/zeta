# Zeta Quality Gate Enforcer
# Run this script to validate all quality gates before any push

param(
    [switch]$Fix = $false,
    [switch]$Strict = $true,
    [switch]$Test = $true
)

Write-Host "ZETA QUALITY GATE ENFORCER" -ForegroundColor Cyan
Write-Host "==============================" -ForegroundColor Cyan
Write-Host "Running at: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')" -ForegroundColor Gray
Write-Host ""

# Configuration
$RUSTFLAGS = if ($Strict) { "-Dwarnings" } else { "" }
$env:RUSTFLAGS = $RUSTFLAGS

# Check if we're in a Rust project
if (-not (Test-Path "Cargo.toml")) {
    Write-Host "ERROR: Not a Rust project. Exiting." -ForegroundColor Red
    exit 1
}

# Function to run command and check result
function Invoke-QualityCheck {
    param(
        [string]$Name,
        [string]$Command,
        [string]$FixCommand = $null,
        [switch]$AllowFailure = $false
    )
    
    Write-Host "`nCHECK: $Name..." -ForegroundColor Cyan
    Write-Host "   Command: $Command" -ForegroundColor Gray
    
    $result = Invoke-Expression $Command 2>&1
    $exitCode = $LASTEXITCODE
    
    if ($exitCode -eq 0) {
        Write-Host "   PASSED" -ForegroundColor Green
        return $true
    } else {
        Write-Host "   FAILED (exit code: $exitCode)" -ForegroundColor Red
        
        if ($result) {
            $errorLines = $result | Select-Object -First 5
            foreach ($line in $errorLines) {
                Write-Host "      $line" -ForegroundColor DarkRed
            }
            if ($result.Count -gt 5) {
                Write-Host "      ... and $($result.Count - 5) more lines" -ForegroundColor DarkGray
            }
        }
        
        if ($Fix -and $FixCommand) {
            Write-Host "   Attempting automatic fix..." -ForegroundColor Yellow
            Write-Host "      Fix command: $FixCommand" -ForegroundColor Gray
            $fixResult = Invoke-Expression $FixCommand 2>&1
            $fixExitCode = $LASTEXITCODE
            
            if ($fixExitCode -eq 0) {
                Write-Host "      Fix applied successfully" -ForegroundColor Green
                
                # Re-run the original check
                Write-Host "      Re-running check..." -ForegroundColor Gray
                $retryResult = Invoke-Expression $Command 2>&1
                $retryExitCode = $LASTEXITCODE
                
                if ($retryExitCode -eq 0) {
                    Write-Host "      Check now passes after fix" -ForegroundColor Green
                    return $true
                } else {
                    Write-Host "      Still failing after fix" -ForegroundColor Red
                }
            } else {
                Write-Host "      Fix failed" -ForegroundColor Red
            }
        }
        
        if (-not $AllowFailure) {
            return $false
        }
        return $false
    }
}

# Track results
$allPassed = $true
$results = @{}

# 1. Code Formatting Check
$results.Formatting = Invoke-QualityCheck -Name "Code Formatting (rustfmt)" `
    -Command "cargo fmt --all -- --check" `
    -FixCommand "cargo fmt --all"

# 2. Linting Check
$clippyCommand = "cargo clippy --workspace --all-features --all-targets"
if ($Strict) {
    $clippyCommand += " -- -D warnings"
}

$results.Linting = Invoke-QualityCheck -Name "Linting (clippy)" `
    -Command $clippyCommand `
    -FixCommand "cargo clippy --fix --workspace --all-features --all-targets"

# 3. Compilation Check
$results.Compilation = Invoke-QualityCheck -Name "Compilation Check" `
    -Command "cargo check --workspace --all-features --all-targets"

# 4. Test Suite (optional)
if ($Test) {
    $results.Tests = Invoke-QualityCheck -Name "Test Suite" `
        -Command "cargo test --workspace --all-features" `
        -AllowFailure:$false
}

# 5. Documentation Check (basic)
$results.Docs = Invoke-QualityCheck -Name "Documentation Check" `
    -Command "cargo doc --no-deps --workspace --all-features" `
    -AllowFailure:$true

# Summary
Write-Host "`nQUALITY GATE SUMMARY" -ForegroundColor Cyan
Write-Host "=====================" -ForegroundColor Cyan

$passCount = 0
$failCount = 0

foreach ($key in $results.Keys) {
    if ($results[$key]) {
        Write-Host "   PASS: $key" -ForegroundColor Green
        $passCount++
    } else {
        Write-Host "   FAIL: $key" -ForegroundColor Red
        $failCount++
        $allPassed = $false
    }
}

Write-Host "`nRESULTS: $passCount passed, $failCount failed" -ForegroundColor $(if ($allPassed) { "Green" } else { "Red" })

if ($allPassed) {
    Write-Host "`nALL QUALITY GATES PASSED!" -ForegroundColor Green
    Write-Host "   Ready for commit and push." -ForegroundColor Green
    
    # Check git status
    Write-Host "`nGit Status:" -ForegroundColor Cyan
    git status --short
    
    exit 0
} else {
    Write-Host "`nQUALITY GATES FAILED" -ForegroundColor Red
    Write-Host "   Do not commit or push until all issues are resolved." -ForegroundColor Red
    Write-Host "`nRecommended actions:" -ForegroundColor Yellow
    
    if (-not $results.Formatting) {
        Write-Host "   - Run: cargo fmt --all" -ForegroundColor Yellow
    }
    if (-not $results.Linting) {
        Write-Host "   - Run: cargo clippy --fix --workspace --all-features --all-targets" -ForegroundColor Yellow
        Write-Host "   - Then: cargo clippy --workspace --all-features --all-targets -- -D warnings" -ForegroundColor Yellow
    }
    if (-not $results.Compilation) {
        Write-Host "   - Fix compilation errors shown above" -ForegroundColor Yellow
    }
    if ($Test -and (-not $results.Tests)) {
        Write-Host "   - Fix failing tests" -ForegroundColor Yellow
    }
    
    Write-Host "`nRe-run with -Fix parameter to attempt automatic fixes:" -ForegroundColor Gray
    Write-Host "   .\scripts\quality-gate-enforcer.ps1 -Fix" -ForegroundColor White
    
    exit 1
}