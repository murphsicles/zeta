# Simple test runner for bootstrap validation
Write-Host "=== ZETA BOOTSTRAP TEST RUNNER ==="
Write-Host "Started: $(Get-Date)"

# Check files
$minimalCompiler = "tests/minimal_compiler.z"
$selfTest = "tests/self_compile_test.z"

if (-not (Test-Path $minimalCompiler)) {
    Write-Host "ERROR: Missing $minimalCompiler"
    exit 1
}

if (-not (Test-Path $selfTest)) {
    Write-Host "ERROR: Missing $selfTest"
    exit 1
}

Write-Host "OK: Found required files"

# Check compiler
$compiler = "target\debug\zetac.exe"
if (-not (Test-Path $compiler)) {
    Write-Host "WARNING: Compiler not found, trying to build..."
    cargo build --bin zetac 2>&1 | Out-Null
    if (-not (Test-Path $compiler)) {
        Write-Host "ERROR: Failed to build compiler"
        exit 1
    }
}

Write-Host "OK: Compiler available"

# Try to compile the self-test
Write-Host "Testing compilation of self-test program..."
$output = "bootstrap/test_output.ll"

try {
    & $compiler $selfTest -o $output 2>&1
    if (Test-Path $output) {
        Write-Host "SUCCESS: Compiled self-test to $output"
        $size = (Get-Item $output).Length
        Write-Host "Output size: $size bytes"
    } else {
        Write-Host "FAILED: No output file created"
    }
} catch {
    Write-Host "ERROR: Compilation failed: $_"
}

# Create a simple verification test
$testCode = @'
fn test_bootstrap() -> i64 {
    return 42;
}
'@

$testFile = "bootstrap/simple_test.z"
Set-Content -Path $testFile -Value $testCode
Write-Host "Created test file: $testFile"

# Summary
Write-Host ""
Write-Host "=== SUMMARY ==="
Write-Host "Status: Bootstrap validation infrastructure is ready"
Write-Host "Next: Implement actual self-compilation in minimal_compiler.z"
Write-Host "Completed: $(Get-Date)"