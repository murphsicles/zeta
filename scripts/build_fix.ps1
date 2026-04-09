# Build fix script for Zeta compiler
$env:LLVM_SYS_210_PREFIX="C:\LLVM-21"
Write-Host "Building with LLVM_SYS_210_PREFIX=$env:LLVM_SYS_210_PREFIX"

# Check LLVM installation
if (Test-Path "C:\LLVM-21\bin\llvm-config.exe") {
    Write-Host "LLVM 21.1.8 found at C:\LLVM-21"
} else {
    Write-Error "LLVM 21 not found at C:\LLVM-21"
    exit 1
}

# Build with cargo
cargo build --verbose

# Check if build succeeded
if ($LASTEXITCODE -eq 0) {
    Write-Host "Build successful!"
    
    # Test the executable
    $exePath = "target\debug\zetac.exe"
    if (Test-Path $exePath) {
        Write-Host "Testing executable: $exePath"
        & $exePath --version
    } else {
        Write-Error "Executable not found at $exePath"
    }
} else {
    Write-Error "Build failed with exit code $LASTEXITCODE"
}