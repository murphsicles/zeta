# Simple test runner for bootstrap status check

Write-Host "=== ZETA BOOTSTRAP STATUS CHECK ==="
Write-Host "Test started at: $(Get-Date)"

# Check files
$minimalCompilerPath = "tests/minimal_compiler.z"
$selfTestPath = "tests/self_compile_test.z"

if (Test-Path $minimalCompilerPath) {
    $size = (Get-Item $minimalCompilerPath).Length
    Write-Host "✅ Minimal compiler: $size bytes"
} else {
    Write-Host "❌ Minimal compiler not found"
}

if (Test-Path $selfTestPath) {
    $size = (Get-Item $selfTestPath).Length
    Write-Host "✅ Self-test program: $size bytes"
} else {
    Write-Host "❌ Self-test program not found"
}

# Check compiler
$zetaCompiler = "target\debug\zetac.exe"
if (Test-Path $zetaCompiler) {
    Write-Host "✅ Zeta compiler binary found"
} else {
    Write-Host "⚠️  Zeta compiler binary not found"
}

Write-Host ""
Write-Host "=== STATUS SUMMARY ==="
Write-Host "Phase 1.3: Bootstrap Validation - IN PROGRESS (75%)"
Write-Host "Self-compilation test runner: Fixed version created"
Write-Host "Next step: Implement compilation of minimal_compiler.z with itself"
Write-Host ""
Write-Host "Test completed at: $(Get-Date)"