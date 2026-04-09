# Simple validation script for GitHub protocol

Write-Host "GitHub Protocol Validation" -ForegroundColor Cyan
Write-Host "==========================" -ForegroundColor Cyan

$issues = @()

# Check 1: No test files in root
Write-Host "`n1. Checking for test files in root..." -ForegroundColor Yellow
$testFiles = Get-ChildItem -Filter "*.z" -File | Where-Object { $_.Name -match "^test_|^debug_" }
if ($testFiles) {
    $issues += "Found $($testFiles.Count) test files in root directory"
    Write-Host "   FAIL: $($testFiles.Count) test files in root" -ForegroundColor Red
} else {
    Write-Host "   PASS: No test files in root" -ForegroundColor Green
}

# Check 2: No build artifacts in root
Write-Host "`n2. Checking for build artifacts in root..." -ForegroundColor Yellow
$artifacts = Get-ChildItem -File | Where-Object { $_.Extension -in ".exe", ".exe.o", ".pdb", ".obj", ".rlib" }
if ($artifacts) {
    $issues += "Found $($artifacts.Count) build artifacts in root directory"
    Write-Host "   FAIL: $($artifacts.Count) build artifacts in root" -ForegroundColor Red
} else {
    Write-Host "   PASS: No build artifacts in root" -ForegroundColor Green
}

# Check 3: Test organization
Write-Host "`n3. Checking test organization..." -ForegroundColor Yellow
$testRootFiles = Get-ChildItem "tests" -File
if ($testRootFiles.Count -gt 10) {
    $issues += "Too many files ($($testRootFiles.Count)) in tests root directory"
    Write-Host "   FAIL: $($testRootFiles.Count) files in tests root (should be < 10)" -ForegroundColor Red
} else {
    Write-Host "   PASS: Tests are organized" -ForegroundColor Green
}

# Check 4: Recent releases have documentation (v0.4.0+)
Write-Host "`n4. Checking recent release documentation..." -ForegroundColor Yellow
if (Test-Path "CHANGELOG.md") {
    # Check for v0.4.0, v0.4.1, v0.5.0 specifically
    $recentTags = @("v0.4.0", "v0.4.1", "v0.5.0")
    $changelog = Get-Content "CHANGELOG.md" -Raw
    $missing = @()
    foreach ($tag in $recentTags) {
        if ($changelog -notmatch "\[$tag\]") {
            $missing += $tag
        }
    }
    if ($missing) {
        $issues += "Missing release notes for recent releases: $($missing -join ', ')"
        Write-Host "   FAIL: Missing release notes for $($missing.Count) recent releases" -ForegroundColor Red
    } else {
        Write-Host "   PASS: Recent releases have documentation" -ForegroundColor Green
    }
} else {
    $issues += "CHANGELOG.md not found"
    Write-Host "   FAIL: CHANGELOG.md not found" -ForegroundColor Red
}

# Check 5: Protocol documentation exists
Write-Host "`n5. Checking protocol documentation..." -ForegroundColor Yellow
if (-not (Test-Path "AGENT_GITHUB_PROTOCOL.md")) {
    $issues += "AGENT_GITHUB_PROTOCOL.md not found"
    Write-Host "   FAIL: Protocol documentation missing" -ForegroundColor Red
} else {
    Write-Host "   PASS: Protocol documentation exists" -ForegroundColor Green
}

# Check 6: Clean root directory (no .z files except maybe README, etc.)
Write-Host "`n6. Checking root directory cleanliness..." -ForegroundColor Yellow
$zFilesInRoot = Get-ChildItem -Filter "*.z" -File
if ($zFilesInRoot.Count -gt 0) {
    $issues += "Found $($zFilesInRoot.Count) .z files in root directory"
    Write-Host "   FAIL: $($zFilesInRoot.Count) .z files in root" -ForegroundColor Red
} else {
    Write-Host "   PASS: No .z files in root" -ForegroundColor Green
}

# Summary
Write-Host "`n" + ("="*50) -ForegroundColor Cyan
Write-Host "VALIDATION SUMMARY" -ForegroundColor Cyan
Write-Host "="*50 -ForegroundColor Cyan

if ($issues.Count -eq 0) {
    Write-Host "✅ SUCCESS: Repository is compliant with GitHub protocol" -ForegroundColor Green
    exit 0
} else {
    Write-Host "❌ FAILED: Found $($issues.Count) issue(s)" -ForegroundColor Red
    foreach ($issue in $issues) {
        Write-Host "   - $issue" -ForegroundColor Yellow
    }
    Write-Host "`nSee AGENT_GITHUB_PROTOCOL.md for compliance requirements." -ForegroundColor Cyan
    exit 1
}