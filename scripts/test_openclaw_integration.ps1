# Test OpenClaw integration with Zeta compiler
$env:LLVM_SYS_210_PREFIX="C:\LLVM-21"

Write-Host "Testing OpenClaw integration with Zeta compiler..."
Write-Host "Current directory: $(Get-Location)"
Write-Host "LLVM_SYS_210_PREFIX: $env:LLVM_SYS_210_PREFIX"

# Test 1: Simple compilation
$testCode = @'
fn main() -> i64 {
    let result = 100;
    result
}
'@
$testCode | Out-File -FilePath "openclaw_test.z" -Encoding UTF8

Write-Host "`nTest 1: Compiling simple program..."
& ".\target\debug\zetac.exe" "openclaw_test.z" -o "openclaw_test.exe" 2>&1

if ($LASTEXITCODE -eq 0) {
    Write-Host "Compilation successful!"
    
    Write-Host "`nTest 2: Running compiled program..."
    & ".\openclaw_test.exe"
    Write-Host "Exit code: $LASTEXITCODE (expected: 100)"
    
    if ($LASTEXITCODE -eq 100) {
        Write-Host "SUCCESS: OpenClaw integration test passed!"
    } else {
        Write-Host "WARNING: Program ran but returned unexpected exit code"
    }
} else {
    Write-Host "ERROR: Compilation failed"
}

# Clean up
Remove-Item -Path "openclaw_test.z", "openclaw_test.exe", "openclaw_test.exe.o" -ErrorAction SilentlyContinue