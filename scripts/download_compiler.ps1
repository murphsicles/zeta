# download_compiler.ps1
# Download Zeta v0.4.1 compiler for testing

param(
    [string]$Url = "https://github.com/murphsicles/zeta/releases/download/v0.4.1/zetac.exe",
    [string]$Output = "zetac_v0.4.1.exe"
)

Write-Host "=== DOWNLOADING ZETA v0.4.1 COMPILER ===" -ForegroundColor Cyan
Write-Host "URL: $Url" -ForegroundColor Gray
Write-Host "Output: $Output" -ForegroundColor Gray
Write-Host ""

try {
    # Try using .NET WebClient
    Write-Host "Attempting download with WebClient..." -ForegroundColor White
    
    $webClient = New-Object System.Net.WebClient
    $webClient.DownloadFile($Url, $Output)
    
    if (Test-Path $Output) {
        $fileInfo = Get-Item $Output
        Write-Host "✅ DOWNLOAD SUCCESSFUL!" -ForegroundColor Green
        Write-Host "   File: $($fileInfo.Name)" -ForegroundColor Gray
        Write-Host "   Size: $($fileInfo.Length) bytes" -ForegroundColor Gray
        Write-Host "   Modified: $($fileInfo.LastWriteTime)" -ForegroundColor Gray
        
        # Also copy as zetac.exe for testing
        Copy-Item $Output "zetac.exe" -Force
        Write-Host "✅ Copied as zetac.exe for testing" -ForegroundColor Green
        
        # Test the compiler
        Write-Host ""
        Write-Host "=== TESTING COMPILER ===" -ForegroundColor Cyan
        Write-Host "Running: .\zetac.exe --version" -ForegroundColor White
        
        $versionOutput = .\zetac.exe --version 2>&1
        Write-Host "Output: $versionOutput" -ForegroundColor Gray
        
        return $true
    } else {
        Write-Host "❌ Download failed - file not created" -ForegroundColor Red
        return $false
    }
} catch {
    Write-Host "❌ Download error: $_" -ForegroundColor Red
    return $false
}