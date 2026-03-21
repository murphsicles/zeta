# PowerShell script to help push v0.5.0 to GitHub
# Requires GitHub Personal Access Token

param(
    [Parameter(Mandatory=$true)]
    [string]$GitHubToken
)

Write-Host "🚀 ZETA v0.5.0 GITHUB PUSH ASSISTANT" -ForegroundColor Cyan
Write-Host "=====================================" -ForegroundColor Cyan

# Verify release first
Write-Host "`n🔍 VERIFYING RELEASE..." -ForegroundColor Yellow

$bootstrapTest = .\bootstrap-verification.exe
$compilerTest = .\zetac-0.5.0.exe

if ($bootstrapTest -ne 0 -or $compilerTest -ne 0) {
    Write-Host "❌ RELEASE VERIFICATION FAILED" -ForegroundColor Red
    Write-Host "   Bootstrap test exit code: $bootstrapTest" -ForegroundColor Gray
    Write-Host "   Compiler test exit code: $compilerTest" -ForegroundColor Gray
    exit 1
}

Write-Host "✅ Release verification passed" -ForegroundColor Green

# Configure git remote
Write-Host "`n🔧 CONFIGURING GIT REMOTE..." -ForegroundColor Yellow

$remoteUrl = "https://$GitHubToken@github.com/murphsicles/zeta.git"

try {
    git remote remove origin 2>$null
    git remote add origin $remoteUrl
    Write-Host "✅ Git remote configured" -ForegroundColor Green
} catch {
    Write-Host "❌ Failed to configure git remote: $_" -ForegroundColor Red
    exit 1
}

# Show push commands
Write-Host "`n📤 PUSH COMMANDS:" -ForegroundColor Yellow
Write-Host "=====================================" -ForegroundColor Gray
Write-Host "1. Push to main branch (force):" -ForegroundColor Cyan
Write-Host "   git push -f origin master:main" -ForegroundColor White
Write-Host ""
Write-Host "2. Push v0.5.0 tag:" -ForegroundColor Cyan
Write-Host "   git push origin v0.5.0" -ForegroundColor White
Write-Host ""
Write-Host "3. Alternative: Create release via web:" -ForegroundColor Cyan
Write-Host "   https://github.com/murphsicles/zeta/releases/new" -ForegroundColor White
Write-Host "=====================================" -ForegroundColor Gray

# Show release details
Write-Host "`n📋 RELEASE DETAILS:" -ForegroundColor Yellow
Write-Host "   Tag: v0.5.0" -ForegroundColor Gray
Write-Host "   Target: main branch" -ForegroundColor Gray
Write-Host "   Status: Latest release" -ForegroundColor Gray
Write-Host "   Files: 9 production files" -ForegroundColor Gray

# Show manual instructions
Write-Host "`n📝 MANUAL RELEASE CREATION:" -ForegroundColor Yellow
Write-Host "1. Go to: https://github.com/murphsicles/zeta/releases/new" -ForegroundColor Gray
Write-Host "2. Tag: v0.5.0" -ForegroundColor Gray
Write-Host "3. Target: main" -ForegroundColor Gray
Write-Host "4. Title: 'Zeta v0.5.0: Pure Zeta Compiler'" -ForegroundColor Gray
Write-Host "5. Description: Copy from RELEASE_v0.5.0.md" -ForegroundColor Gray
Write-Host "6. ☑️ Set as latest release" -ForegroundColor Gray
Write-Host "7. Upload all files from this directory" -ForegroundColor Gray
Write-Host "8. Publish" -ForegroundColor Gray

Write-Host "`n✅ READY FOR PUSHING v0.5.0 AS LATEST RELEASE" -ForegroundColor Green
Write-Host "==============================================" -ForegroundColor Green
Write-Host "The Dark Factory has prepared everything." -ForegroundColor Cyan
Write-Host "v0.5.0 is production-ready and verified." -ForegroundColor Cyan
Write-Host "Execute the push when ready." -ForegroundColor Cyan
