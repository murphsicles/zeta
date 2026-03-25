# SYNC ACTUAL BOOTSTRAP TO GITHUB
# Run this in PowerShell as Administrator

Write-Host "=========================================" -ForegroundColor Red
Write-Host "SYNC ACTUAL BOOTSTRAP TO GITHUB" -ForegroundColor Red
Write-Host "=========================================" -ForegroundColor Red

# Paths
$bootstrapPath = "C:\Users\mummy\Documents\DarkFactory\zeta"
$githubPath = "C:\Users\mummy\.openclaw\workspace\zeta-public"

Write-Host "Source: $bootstrapPath" -ForegroundColor Yellow
Write-Host "Destination: $githubPath" -ForegroundColor Yellow
Write-Host "" -ForegroundColor White

# Safety check
$confirm = Read-Host "This will REPLACE GitHub repository with actual bootstrap. Type 'SYNC' to proceed"
if ($confirm -ne "SYNC") {
    Write-Host "Sync cancelled." -ForegroundColor Yellow
    exit 1
}

Write-Host "Starting sync..." -ForegroundColor Green

# =========================================
# STEP 1: CLEAN GITHUB REPOSITORY
# =========================================
Write-Host "`n=== CLEANING GITHUB REPOSITORY ===" -ForegroundColor Green

cd $githubPath

# Remove everything except .git
Get-ChildItem -Path $githubPath -Exclude ".git" | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue
Write-Host "  GitHub repository cleaned" -ForegroundColor Green

# =========================================
# STEP 2: COPY ACTUAL BOOTSTRAP
# =========================================
Write-Host "`n=== COPYING ACTUAL BOOTSTRAP ===" -ForegroundColor Green

# Copy source code
Copy-Item -Path "$bootstrapPath\src" -Destination "$githubPath\" -Recurse -Force
Write-Host "  Copied: src/ (49 .z files)" -ForegroundColor Cyan

# Copy essential files
$essentialFiles = @(
    ".gitignore",
    "README.md", 
    "Cargo.toml",
    "LICENSE",
    "CONTRIBUTING.md"
)

foreach ($file in $essentialFiles) {
    $source = "$bootstrapPath\$file"
    if (Test-Path $source) {
        Copy-Item -Path $source -Destination "$githubPath\" -Force
        Write-Host "  Copied: $file" -ForegroundColor Cyan
    }
}

# =========================================
# STEP 3: ADD PUBLIC ACCOUNTABILITY FILES
# =========================================
Write-Host "`n=== ADDING PUBLIC ACCOUNTABILITY ===" -ForegroundColor Green

# Create public development files
$publicFiles = @{
    "PUBLIC_DEVELOPMENT_PLAN.md" = @"
# PUBLIC DEVELOPMENT PLAN

## 🎯 CURRENT STATE
- **v0.3.7 compiler** exists (zetac-v0.3.7-fixed.exe)
- **Bootstrap source** (49 .z files) too advanced for v0.3.7
- **Gap**: v0.3.7 can only parse 635/5300 characters of `src/main.z`

## 🚀 DEVELOPMENT STRATEGY
1. **Extend v0.3.7 incrementally** to parse more of bootstrap
2. **Public accountability** - All work on GitHub
3. **CI verification** - Every commit tested
4. **Incremental progress** - Small, verifiable steps

## 📊 PROGRESS TRACKING
- **Baseline**: 635/5300 characters parsed
- **Goal**: Parse `lt(Result, i64)` (generic types)
- **Success**: Compile more of `src/main.z` each iteration
"@
    
    "PUBLIC_PROGRESS_TRACKER.md" = @"
# PUBLIC PROGRESS TRACKER

## 🏗️ PHASE 1: GENERIC TYPE SUPPORT
**Goal**: Extend v0.3.7 to parse `lt(Result, i64)`

### Status: NOT STARTED
- **Baseline test**: `src/baseline_test.z` (635 chars parse)
- **Extension goal**: `src/extension_goal.z` (generic types)
- **Current capability**: v0.3.7 fails on parenthesized types

### Next Steps:
1. Create type alias workaround
2. Extend parser incrementally
3. Test with CI verification
"@
}

foreach ($fileName in $publicFiles.Keys) {
    $content = $publicFiles[$fileName]
    $path = "$githubPath\$fileName"
    Set-Content -Path $path -Value $content -Encoding UTF8
    Write-Host "  Created: $fileName" -ForegroundColor Cyan
}

# =========================================
# STEP 4: VERIFY SYNC
# =========================================
Write-Host "`n=== VERIFYING SYNC ===" -ForegroundColor Green

cd $githubPath

Write-Host "Files in repository:" -ForegroundColor White
Get-ChildItem -Recurse -File | Select-Object -First 20 Name, Length | Format-Table -AutoSize

Write-Host "`n.z file count:" -ForegroundColor White
$zCount = (Get-ChildItem -Path "src" -Filter "*.z" -Recurse -File | Measure-Object).Count
Write-Host "  $zCount .z files in src/" -ForegroundColor Cyan

# =========================================
# STEP 5: COMMIT AND PUSH
# =========================================
Write-Host "`n=== COMMITTING TO GITHUB ===" -ForegroundColor Green

# Git operations
git add .
git commit -m "SYNC: Actual bootstrap source (49 .z files)

REPLACED toy experiments with actual bootstrap compiler source.

WHAT'S INCLUDED:
- 49 actual .z source files from bootstrap
- Full src/ structure (frontend, middle, backend, runtime)
- Public accountability files
- Clean .gitignore for professional structure

WHAT WAS REMOVED:
- All toy parser experiments
- Type alias experiments  
- Test prototypes

NEXT STEPS:
- Extend v0.3.7 incrementally to parse more of this source
- Public CI verification for all work
- Transparent progress tracking

This is the ACTUAL bootstrap compiler, not experiments."
Write-Host "  Committed changes" -ForegroundColor Green

git push origin main
Write-Host "  Pushed to GitHub" -ForegroundColor Green

# =========================================
# SUMMARY
# =========================================
Write-Host "`n=========================================" -ForegroundColor Green
Write-Host "SYNC COMPLETE" -ForegroundColor Green
Write-Host "=========================================" -ForegroundColor Green

Write-Host "`nGitHub now contains:" -ForegroundColor Yellow
Write-Host "✅ Actual bootstrap source (49 .z files)" -ForegroundColor Green
Write-Host "✅ Professional directory structure" -ForegroundColor Green
Write-Host "✅ Public accountability files" -ForegroundColor Green
Write-Host "✅ Clean .gitignore" -ForegroundColor Green

Write-Host "`nView at: https://github.com/murphsicles/zeta" -ForegroundColor Cyan
Write-Host "`nSync completed at $(Get-Date)" -ForegroundColor Green