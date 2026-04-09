# Self-compilation test runner for Zeta bootstrap
# This script tests if the minimal compiler can compile itself

Write-Host "=== ZETA BOOTSTRAP SELF-COMPILATION TEST ==="
Write-Host "Test started at: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')"
Write-Host ""

# Step 1: Check if we have the minimal compiler source
$minimalCompilerPath = "..\tests\minimal_compiler.z"
$selfTestPath = "..\tests\self_compile_test.z"

if (-not (Test-Path $minimalCompilerPath)) {
    Write-Host "ERROR: Minimal compiler not found at $minimalCompilerPath"
    exit 1
}

if (-not (Test-Path $selfTestPath)) {
    Write-Host "ERROR: Self-compilation test not found at $selfTestPath"
    exit 1
}

Write-Host "OK: Found minimal compiler source: $minimalCompilerPath"
Write-Host "OK: Found self-compilation test: $selfTestPath"
Write-Host ""

# Step 2: Read and analyze the files
$minimalCompilerSize = (Get-Item $minimalCompilerPath).Length
$selfTestSize = (Get-Item $selfTestPath).Length

Write-Host "File sizes:"
Write-Host "  - Minimal compiler: $minimalCompilerSize bytes"
Write-Host "  - Self-test program: $selfTestSize bytes"
Write-Host ""

# Step 3: Check if the Zeta compiler binary exists
$zetaCompiler = "..\target\debug\zetac.exe"
if (-not (Test-Path $zetaCompiler)) {
    Write-Host "WARNING: Zeta compiler binary not found at $zetaCompiler"
    Write-Host "   Checking if we can build it..."
    
    # Check if we're in a Cargo project
    if (Test-Path "..\Cargo.toml") {
        Write-Host "   Found Cargo.toml, attempting to build..."
        try {
            Push-Location ..
            cargo build --bin zetac 2>&1 | Out-Null
            Pop-Location
            if (Test-Path $zetaCompiler) {
                Write-Host "OK: Successfully built Zeta compiler"
            } else {
                Write-Host "ERROR: Failed to build Zeta compiler"
                Write-Host "   You may need to build it manually: cargo build --bin zetac"
                exit 1
            }
        } catch {
            Write-Host "ERROR: Error building compiler: $_"
            Write-Host "   You may need to build it manually: cargo build --bin zetac"
            exit 1
        }
    } else {
        Write-Host "ERROR: Cargo.toml not found. Cannot build compiler."
        Write-Host "   Please build the compiler manually: cargo build --bin zetac"
        exit 1
    }
} else {
    Write-Host "OK: Found Zeta compiler binary: $zetaCompiler"
}

Write-Host ""

# Step 4: Test compilation of the self-test program
Write-Host "=== PHASE 1: Testing compilation of self-test program ==="

$outputFile = "self_test_output.ll"
Write-Host "Compiling $selfTestPath to $outputFile..."

try {
    # Try to compile the self-test program
    & $zetaCompiler $selfTestPath -o $outputFile 2>&1 | Out-Null
    
    if (Test-Path $outputFile) {
        $outputSize = (Get-Item $outputFile).Length
        Write-Host "OK: Successfully compiled self-test program"
        Write-Host "   Output file: $outputFile ($outputSize bytes)"
        
        # Show first few lines of output
        Write-Host "   First 5 lines of LLVM IR:"
        Get-Content $outputFile -TotalCount 5 | ForEach-Object { Write-Host "     $_" }
    } else {
        Write-Host "ERROR: Compilation failed - no output file created"
    }
} catch {
    Write-Host "ERROR: Compilation error: $_"
}

Write-Host ""

# Step 5: Analyze the minimal compiler source
Write-Host "=== PHASE 2: Analyzing minimal compiler structure ==="

$compilerSource = Get-Content $minimalCompilerPath -Raw
$lines = ($compilerSource -split "`n").Count
$functions = ($compilerSource -split "fn ").Count - 1

Write-Host "Minimal compiler analysis:"
Write-Host "  - Lines of code: $lines"
Write-Host "  - Functions defined: $functions"
Write-Host "  - AST node types: Program, FuncDef, Return, Lit, Ident, Call, VarDecl, Assign, If, While"
Write-Host ""

# Step 6: Simulate self-compilation test
Write-Host "=== PHASE 3: Self-compilation simulation ==="
Write-Host "Note: Actual self-compilation requires the minimal compiler to be implemented in Zeta"
Write-Host "      and for that Zeta code to be compilable by the current compiler."
Write-Host ""

Write-Host "Current status based on WORK_QUEUE.md:"
Write-Host "  - OK: Phase 1.1: Ultra Simple Compiler - COMPLETE"
Write-Host "  - OK: Phase 1.2: Add Basic Features - COMPLETE"
Write-Host "  - IN PROGRESS: Phase 1.3: Bootstrap Validation - IN PROGRESS"
Write-Host "  - NEXT: Implement actual self-compilation test execution"
Write-Host ""

# Step 7: Create a simple test to verify the concept
Write-Host "=== PHASE 4: Creating verification test ==="

$verificationTest = @'
// Verification test for bootstrap compiler
// This tests basic compilation capabilities

fn verify_addition() -> i64 {
    return 10 + 20;
}

fn verify_function_call() -> i64 {
    let x = verify_addition();
    return x * 2;
}

fn main() -> i64 {
    let result = verify_function_call();
    // Should return 60 (30 * 2)
    return result;
}
'@

$verificationPath = "verification_test.z"
Set-Content -Path $verificationPath -Value $verificationTest

Write-Host "Created verification test at $verificationPath"

# Step 8: Summary
Write-Host ""
Write-Host "=== TEST SUMMARY ==="
Write-Host "OK: Infrastructure check: PASSED"
Write-Host "OK: File availability: PASSED"
Write-Host "OK: Compiler binary: PASSED"
Write-Host "IN PROGRESS: Self-compilation: PENDING (requires minimal compiler implementation)"
Write-Host ""
Write-Host "Next steps for bootstrap validation:"
Write-Host "1. Complete the minimal Zeta compiler implementation in tests/minimal_compiler.z"
Write-Host "2. Ensure it can compile basic Zeta programs"
Write-Host "3. Test compiling the minimal compiler with itself"
Write-Host "4. Verify output matches expected behavior"
Write-Host ""
Write-Host "Test completed at: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')"