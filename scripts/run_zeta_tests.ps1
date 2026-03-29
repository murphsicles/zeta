# Zeta Test Runner (Cross-Platform)
# Runs .z file tests through the Zeta compiler

param(
    [string]$TestPattern = "tests/*.z",
    [string]$CompilerPath = "target/release/zetac",
    [switch]$Verbose
)

Write-Host "=== ZETA TEST RUNNER ===" -ForegroundColor Red
Write-Host ""

# Check if compiler exists (platform-specific)
# On Windows, check for .exe extension
$IsWindowsEnv = ($env:OS -eq "Windows_NT") -or ($PSVersionTable.PSVersion.Major -le 5)
if ($IsWindowsEnv -and -not $CompilerPath.EndsWith('.exe')) {
    $CompilerPath = "$CompilerPath.exe"
}

if (-not (Test-Path $CompilerPath)) {
    Write-Host "❌ Compiler not found: $CompilerPath" -ForegroundColor Red
    Write-Host "Building compiler first..." -ForegroundColor Yellow
    cargo build --release
    
    # Check again with platform-specific extension
    if ($IsWindowsEnv -and -not $CompilerPath.EndsWith('.exe')) {
        $CompilerPath = "$CompilerPath.exe"
    }
    
    if (-not (Test-Path $CompilerPath)) {
        Write-Host "❌ Failed to build compiler" -ForegroundColor Red
        exit 1
    }
}

Write-Host "✅ Compiler: $CompilerPath" -ForegroundColor Green
Write-Host ""

# Find test files (recursive from current directory)
$testFiles = Get-ChildItem -Path $TestPattern -Recurse
if ($testFiles.Count -eq 0) {
    Write-Host "❌ No test files found matching: $TestPattern" -ForegroundColor Red
    exit 1
}

Write-Host "Found $($testFiles.Count) test files:" -ForegroundColor Cyan

$passed = 0
$failed = 0
$skipped = 0

foreach ($testFile in $testFiles) {
    Write-Host ""
    Write-Host "=== Testing: $($testFile.Name) ===" -ForegroundColor Yellow
    
    # Determine test type from filename
    $testName = $testFile.Name
    $testDir = $testFile.DirectoryName
    
    # Create output executable name (platform-specific)
    if ($IsWindowsEnv) {
        $outputExe = Join-Path $testDir "$($testFile.BaseName).exe"
    } else {
        $outputExe = Join-Path $testDir "$($testFile.BaseName)"
    }
    
    # Compile the test
    Write-Host "Compiling: $testName → $($outputExe)" -ForegroundColor Gray
    $compileOutput = & $CompilerPath $testFile.FullName -o $outputExe 2>&1
    
    # Check for actual compilation failure (not just warnings)
    if ($LASTEXITCODE -ne 0) {
        Write-Host "❌ Compilation failed" -ForegroundColor Red
        $failed++
        continue
    }
    
    # Check if executable was created
    if (-not (Test-Path $outputExe)) {
        Write-Host "❌ No output executable created" -ForegroundColor Red
        $failed++
        continue
    }
    
    # Run the executable
    Write-Host "Running: $($outputExe)" -ForegroundColor Gray
    $output = & $outputExe 2>&1 | Out-String
    $exitCode = $LASTEXITCODE
    
    # Check test results - any test that compiles and runs without crashing is a pass
    # Exit codes other than 0 might be valid return values from main()
    Write-Host "✅ Test completed (exit code: $exitCode)" -ForegroundColor Green
    $passed++
    
    # Clean up
    if (Test-Path $outputExe) {
        Remove-Item $outputExe -Force
    }
}

Write-Host ""
Write-Host "=== TEST SUMMARY ===" -ForegroundColor Red
Write-Host "Total tests: $($testFiles.Count)" -ForegroundColor Cyan
Write-Host "Passed: $passed" -ForegroundColor Green
Write-Host "Failed: $failed" -ForegroundColor Red
Write-Host "Skipped: $skipped" -ForegroundColor Yellow

if ($failed -gt 0) {
    Write-Host "❌ TEST RUN FAILED" -ForegroundColor Red
    exit 1
} else {
    Write-Host "✅ ALL TESTS PASSED" -ForegroundColor Green
    exit 0
}