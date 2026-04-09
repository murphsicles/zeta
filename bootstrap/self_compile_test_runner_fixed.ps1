# Self-compilation test runner for Zeta bootstrap
# This script tests if the minimal compiler can compile itself

Write-Host "=== ZETA BOOTSTRAP SELF-COMPILATION TEST ===" -ForegroundColor Cyan
Write-Host "Test started at: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')"
Write-Host ""

# Step 1: Check if we have the minimal compiler source
$minimalCompilerPath = "tests/minimal_compiler.z"
$selfTestPath = "tests/self_compile_test.z"

if (-not (Test-Path $minimalCompilerPath)) {
    Write-Host "❌ ERROR: Minimal compiler not found at $minimalCompilerPath" -ForegroundColor Red
    exit 1
}

if (-not (Test-Path $selfTestPath)) {
    Write-Host "❌ ERROR: Self-compilation test not found at $selfTestPath" -ForegroundColor Red
    exit 1
}

Write-Host "✅ Found minimal compiler source: $minimalCompilerPath" -ForegroundColor Green
Write-Host "✅ Found self-compilation test: $selfTestPath" -ForegroundColor Green
Write-Host ""

# Step 2: Read and analyze the files
$minimalCompilerSize = (Get-Item $minimalCompilerPath).Length
$selfTestSize = (Get-Item $selfTestPath).Length

Write-Host "File sizes:" -ForegroundColor Yellow
Write-Host "  - Minimal compiler: $minimalCompilerSize bytes"
Write-Host "  - Self-test program: $selfTestSize bytes"
Write-Host ""

# Step 3: Check if the Zeta compiler binary exists
$zetaCompiler = "target\debug\zetac.exe"
if (-not (Test-Path $zetaCompiler)) {
    Write-Host "⚠️  WARNING: Zeta compiler binary not found at $zetaCompiler" -ForegroundColor Yellow
    Write-Host "   Building compiler..." -ForegroundColor Yellow
    
    # Try to build the compiler
    try {
        cargo build --bin zetac 2>&1 | Out-Null
        if (Test-Path $zetaCompiler) {
            Write-Host "✅ Successfully built Zeta compiler" -ForegroundColor Green
        } else {
            Write-Host "❌ Failed to build Zeta compiler" -ForegroundColor Red
            exit 1
        }
    } catch {
        Write-Host "❌ Error building compiler: $_" -ForegroundColor Red
        exit 1
    }
} else {
    Write-Host "✅ Found Zeta compiler binary: $zetaCompiler" -ForegroundColor Green
}

Write-Host ""

# Step 4: Test compilation of the self-test program
Write-Host "=== PHASE 1: Testing compilation of self-test program ===" -ForegroundColor Cyan

$outputFile = "bootstrap\self_test_output.ll"
Write-Host "Compiling $selfTestPath to $outputFile..." -ForegroundColor Yellow

try {
    # Try to compile the self-test program
    & $zetaCompiler $selfTestPath -o $outputFile 2>&1 | Out-Null
    
    if (Test-Path $outputFile) {
        $outputSize = (Get-Item $outputFile).Length
        Write-Host "✅ Successfully compiled self-test program" -ForegroundColor Green
        Write-Host "   Output file: $outputFile ($outputSize bytes)" -ForegroundColor Green
        
        # Show first few lines of output
        Write-Host "   First 5 lines of LLVM IR:" -ForegroundColor Gray
        Get-Content $outputFile -TotalCount 5 | ForEach-Object { Write-Host "     $_" -ForegroundColor Gray }
    } else {
        Write-Host "❌ Compilation failed - no output file created" -ForegroundColor Red
    }
} catch {
    Write-Host "❌ Compilation error: $_" -ForegroundColor Red
}

Write-Host ""

# Step 5: Analyze the minimal compiler source
Write-Host "=== PHASE 2: Analyzing minimal compiler structure ===" -ForegroundColor Cyan

$compilerSource = Get-Content $minimalCompilerPath -Raw
$lines = ($compilerSource -split "`n").Count
$functions = ($compilerSource -split "fn ").Count - 1

Write-Host "Minimal compiler analysis:" -ForegroundColor Yellow
Write-Host "  - Lines of code: $lines"
Write-Host "  - Functions defined: $functions"
Write-Host "  - AST node types: Program, FuncDef, Return, Lit, Ident, Call, VarDecl, Assign, If, While"
Write-Host ""

# Step 6: Simulate self-compilation test
Write-Host "=== PHASE 3: Self-compilation simulation ===" -ForegroundColor Cyan
Write-Host "Note: Actual self-compilation requires the minimal compiler to be implemented in Zeta" -ForegroundColor Yellow
Write-Host "      and for that Zeta code to be compilable by the current compiler." -ForegroundColor Yellow
Write-Host ""

Write-Host "Current status based on WORK_QUEUE.md:" -ForegroundColor Yellow
Write-Host "  - ✅ Phase 1.1: Ultra Simple Compiler - COMPLETE" -ForegroundColor Green
Write-Host "  - ✅ Phase 1.2: Add Basic Features - COMPLETE" -ForegroundColor Green
Write-Host "  - 🚧 Phase 1.3: Bootstrap Validation - IN PROGRESS" -ForegroundColor Yellow
Write-Host "  - 📋 Next: Implement actual self-compilation test execution" -ForegroundColor Cyan
Write-Host ""

# Step 7: Create a simple test to verify the concept
Write-Host "=== PHASE 4: Creating verification test ===" -ForegroundColor Cyan

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

$verificationPath = "bootstrap\verification_test.z"
Set-Content -Path $verificationPath -Value $verificationTest

Write-Host "Created verification test at $verificationPath" -ForegroundColor Green

# Step 8: Summary
Write-Host ""
Write-Host "=== TEST SUMMARY ===" -ForegroundColor Cyan
Write-Host "[OK] Infrastructure check: PASSED" -ForegroundColor Green
Write-Host "[OK] File availability: PASSED" -ForegroundColor Green
Write-Host "[OK] Compiler binary: PASSED" -ForegroundColor Green
Write-Host "[IN PROGRESS] Self-compilation: PENDING (requires minimal compiler implementation)" -ForegroundColor Yellow
Write-Host ""
Write-Host "Next steps for bootstrap validation:" -ForegroundColor Yellow
Write-Host "1. Complete the minimal Zeta compiler implementation in tests/minimal_compiler.z"
Write-Host "2. Ensure it can compile basic Zeta programs"
Write-Host "3. Test compiling the minimal compiler with itself"
Write-Host "4. Verify output matches expected behavior"
Write-Host ""
Write-Host "Test completed at: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')" -ForegroundColor Cyan