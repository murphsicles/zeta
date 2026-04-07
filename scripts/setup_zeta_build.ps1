# Zeta Compiler Build Setup Script
# Use this script to set up the correct environment for building the Zeta compiler

Write-Host "========================================="
Write-Host "Zeta Compiler Build Setup"
Write-Host "========================================="

# 1. Set LLVM environment variable
$env:LLVM_SYS_210_PREFIX="C:\LLVM-21"
Write-Host "[1/4] Set LLVM_SYS_210_PREFIX to: $env:LLVM_SYS_210_PREFIX"

# 2. Verify LLVM installation
Write-Host "[2/4] Verifying LLVM installation..."
if (Test-Path "C:\LLVM-21\bin\llvm-config.exe") {
    $llvmVersion = & "C:\LLVM-21\bin\llvm-config.exe" --version
    Write-Host "  ✓ LLVM $llvmVersion found at C:\LLVM-21"
} else {
    Write-Error "  ✗ LLVM not found at C:\LLVM-21"
    Write-Host "  Please install LLVM 21 to C:\LLVM-21"
    exit 1
}

# 3. Clean previous build
Write-Host "[3/4] Cleaning previous build..."
cargo clean
if ($LASTEXITCODE -eq 0) {
    Write-Host "  ✓ Build cleaned"
} else {
    Write-Warning "  ⚠ Clean command returned non-zero exit code"
}

# 4. Build
Write-Host "[4/4] Building Zeta compiler..."
cargo build
if ($LASTEXITCODE -eq 0) {
    Write-Host "  ✓ Build successful!"
    
    # 5. Copy required DLLs
    Write-Host "[5/5] Copying required DLLs..."
    $dlls = @("LLVM-C.dll", "LTO.dll", "Remarks.dll")
    foreach ($dll in $dlls) {
        $source = "C:\LLVM-21\bin\$dll"
        $dest = "target\debug\$dll"
        if (Test-Path $source) {
            Copy-Item $source $dest -Force
            Write-Host "  ✓ Copied $dll"
        } else {
            Write-Warning "  ⚠ $dll not found at $source"
        }
    }
    
    Write-Host "`n========================================="
    Write-Host "BUILD COMPLETE!"
    Write-Host "========================================="
    Write-Host "Compiler: target\debug\zetac.exe"
    Write-Host "Environment: LLVM_SYS_210_PREFIX=C:\LLVM-21"
    Write-Host "`nTest command:"
    Write-Host '  $env:LLVM_SYS_210_PREFIX="C:\LLVM-21"; .\target\debug\zetac.exe test.z -o test.exe'
    Write-Host "========================================="
} else {
    Write-Error "  ✗ Build failed"
    exit 1
}