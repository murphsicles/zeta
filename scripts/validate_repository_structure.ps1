# validate_repository_structure.ps1
# GitHub Protocol Validation Script

param([switch]$Fix = $false)

Write-Host "GitHub Protocol Validation" -ForegroundColor Cyan
Write-Host "==========================" -ForegroundColor Cyan

$violations = 0
$fixed = 0

# Check 1: Root directory contamination
Write-Host "`n1. Checking root directory..." -ForegroundColor Yellow

# Test files in root
$testFiles = Get-ChildItem -Filter "*.z" -File | Where-Object { $_.Name -match "^test_|^debug_" }
if ($testFiles) {
    Write-Host "   VIOLATION: Found $($testFiles.Count) test files in root" -ForegroundColor Red
    $violations += $testFiles.Count
    if ($Fix) {
        foreach ($file in $testFiles) {
            Move-Item $file.FullName "tests\debug" -Force -ErrorAction SilentlyContinue
            Write-Host "   FIXED: Moved $($file.Name) to tests/debug" -ForegroundColor Green
            $fixed++
        }
    }
}

# Build artifacts in root
$artifacts = Get-ChildItem -File | Where-Object { $_.Extension -in ".exe", ".exe.o", ".pdb", ".obj", ".rlib" }
if ($artifacts) {
    Write-Host "   VIOLATION: Found $($artifacts.Count) build artifacts in root" -ForegroundColor Red
    $violations += $artifacts.Count
    if ($Fix) {
        foreach ($file in $artifacts) {
            Move-Item $file.FullName "target" -Force -ErrorAction SilentlyContinue
            Write-Host "   FIXED: Moved $($file.Name) to target" -ForegroundColor Green
            $fixed++
        }
    }
}

# Check 2: Test organization
Write-Host "`n2. Checking test organization..." -ForegroundColor Yellow

# Files in tests root (should be minimal)
$testRootFiles = Get-ChildItem "tests" -File
if ($testRootFiles.Count -gt 10) {
    Write-Host "   VIOLATION: Too many files ($($testRootFiles.Count)) in tests root" -ForegroundColor Red
    $violations++
    if ($Fix) {
        # Create directories if they don't exist
        @("unit", "integration", "performance", "competition", "debug") | ForEach-Object {
            if (-not (Test-Path "tests\$_")) { New-Item -ItemType Directory -Path "tests\$_" -Force | Out-Null }
        }
        
        # Move files based on name patterns
        foreach ($file in $testRootFiles) {
            $dest = "tests\unit"
            if ($file.Name -match "integration") { $dest = "tests\integration" }
            elseif ($file.Name -match "performance|benchmark|sieve") { $dest = "tests\performance" }
            elseif ($file.Name -match "competition|murphy") { $dest = "tests\competition" }
            elseif ($file.Name -match "debug") { $dest = "tests\debug" }
            
            Move-Item $file.FullName $dest -Force -ErrorAction SilentlyContinue
            Write-Host "   FIXED: Moved $($file.Name) to $dest" -ForegroundColor Green
            $fixed++
        }
    }
}

# Check 3: Release documentation
Write-Host "`n3. Checking release documentation..." -ForegroundColor Yellow

$tags = git tag --list | Where-Object { $_ -match "^v\d+\.\d+\.\d+$" }
$changelog = Get-Content "CHANGELOG.md" -Raw -ErrorAction SilentlyContinue

if ($changelog) {
    foreach ($tag in $tags) {
        if ($changelog -notmatch "\[$tag\]") {
            Write-Host "   VIOLATION: Tag $tag missing from CHANGELOG.md" -ForegroundColor Red
            $violations++
            # Can't auto-fix this
        }
    }
} else {
    Write-Host "   VIOLATION: CHANGELOG.md not found or empty" -ForegroundColor Red
    $violations++
}

# Check 4: README security
Write-Host "`n4. Checking README security..." -ForegroundColor Yellow

$readme = Get-Content "README.md" -Raw -ErrorAction SilentlyContinue
if ($readme) {
    # Check for private email patterns
    if ($readme -match "@(gmail\.com|hotmail\.com|yahoo\.com|outlook\.com|live\.com|aol\.com)") {
        Write-Host "   VIOLATION: Possible private email in README.md" -ForegroundColor Red
        $violations++
    }
    
    # Check for credentials
    if ($readme -match "(?i)(password|secret|key|token)\s*[:=]\s*\S+" -and $readme -notmatch "example" -and $readme -notmatch "test") {
        Write-Host "   VIOLATION: Possible credentials in README.md" -ForegroundColor Red
        $violations++
    }
}

# Summary
Write-Host "`n" + ("="*50) -ForegroundColor Cyan
Write-Host "VALIDATION SUMMARY" -ForegroundColor Cyan
Write-Host "="*50 -ForegroundColor Cyan

if ($violations -eq 0) {
    Write-Host "✅ NO VIOLATIONS FOUND" -ForegroundColor Green
    Write-Host "Repository is compliant with GitHub protocol." -ForegroundColor Green
    exit 0
} else {
    Write-Host "❌ FOUND $violations VIOLATION(S)" -ForegroundColor Red
    if ($Fix) {
        Write-Host "🔧 FIXED $fixed VIOLATION(S)" -ForegroundColor Green
        if ($fixed -eq $violations) {
            Write-Host "All violations have been fixed!" -ForegroundColor Green
            exit 0
        } else {
            Write-Host "Some violations require manual fixing." -ForegroundColor Yellow
            exit 1
        }
    } else {
        Write-Host "TIP: Run with -Fix parameter to automatically fix violations" -ForegroundColor Yellow
        exit 1
    }
}