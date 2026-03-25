# Directory creation script for Zeta project
# Run this in PowerShell (as Administrator if needed)

Write-Host "Creating Zeta directory structure..." -ForegroundColor Green

# Navigate to zeta src directory
$zetaSrc = "C:\Users\mummy\Documents\DarkFactory\zeta\src"
cd $zetaSrc

# Create frontend directories
Write-Host "Creating frontend directories..." -ForegroundColor White
New-Item -ItemType Directory -Path "frontend\lexer" -Force
New-Item -ItemType Directory -Path "frontend\ast" -Force
New-Item -ItemType Directory -Path "frontend\diagnostics" -Force

# Create middle directories (with agreed names)
Write-Host "Creating middle directories..." -ForegroundColor White
New-Item -ItemType Directory -Path "middle\types" -Force
New-Item -ItemType Directory -Path "middle\ownership" -Force

# Create driver directory
Write-Host "Creating driver directory..." -ForegroundColor White
New-Item -ItemType Directory -Path "driver" -Force

# Verify creation
Write-Host "`nVerifying directory structure..." -ForegroundColor Green
$createdDirs = @(
    "frontend\lexer",
    "frontend\ast", 
    "frontend\diagnostics",
    "middle\types",
    "middle\ownership",
    "driver"
)

foreach ($dir in $createdDirs) {
    if (Test-Path $dir) {
        Write-Host "  ✅ $dir" -ForegroundColor Green
    } else {
        Write-Host "  ❌ $dir (failed)" -ForegroundColor Red
    }
}

Write-Host "`nDirectory creation complete." -ForegroundColor Green
Write-Host "Run: dir /s C:\Users\mummy\Documents\DarkFactory\zeta\src" -ForegroundColor Yellow