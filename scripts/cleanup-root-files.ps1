# cleanup-root-files.ps1
# Move files from root directory to proper locations

param(
    [switch]$DryRun,
    [switch]$Force,
    [string]$TargetDir = "tests"
)

Write-Host "=== ROOT DIRECTORY CLEANUP ===" -ForegroundColor Cyan
Write-Host "Mode: $(if ($DryRun) { 'Dry Run' } else { 'Live' })" -ForegroundColor Gray
Write-Host ""

# Mapping of file patterns to target directories
$fileMappings = @{
    'test_*.rs' = 'tests'
    'test_*.z' = 'tests'
    'debug_*.rs' = 'tests/debug'
    'debug_*.z' = 'tests/debug'
    '*.rs' = 'src'  # Default for other .rs files
    '*.z' = 'zeta_src'  # Default for other .z files
}

# Get all files in root directory
$rootFiles = Get-ChildItem -File | Where-Object { 
    $_.Extension -in '.rs', '.z', '.ps1', '.md' -and
    $_.Name -notin @('Cargo.toml', 'README.md', 'LICENSE', '.gitignore', 'deny.toml')
}

if ($rootFiles.Count -eq 0) {
    Write-Host "✅ No files found in root directory that need moving." -ForegroundColor Green
    exit 0
}

Write-Host "Found $($rootFiles.Count) files in root directory:" -ForegroundColor Yellow
$rootFiles | ForEach-Object { Write-Host "  - $($_.Name)" -ForegroundColor Gray }

Write-Host ""

$movedCount = 0
$errors = @()

foreach ($file in $rootFiles) {
    $filename = $file.Name
    $extension = $file.Extension
    
    # Determine target directory
    $targetDir = $null
    
    foreach ($pattern in $fileMappings.Keys) {
        if ($filename -like $pattern) {
            $targetDir = $fileMappings[$pattern]
            break
        }
    }
    
    if (-not $targetDir) {
        # Default based on extension
        switch ($extension) {
            '.rs' { $targetDir = 'src' }
            '.z' { $targetDir = 'zeta_src' }
            '.ps1' { $targetDir = 'scripts' }
            '.md' { $targetDir = 'docs' }
            default { $targetDir = 'misc' }
        }
    }
    
    # Create target directory if needed
    $fullTargetDir = $targetDir
    if (-not (Test-Path $fullTargetDir)) {
        Write-Host "Creating directory: $fullTargetDir" -ForegroundColor Gray
        if (-not $DryRun) {
            New-Item -ItemType Directory -Path $fullTargetDir -Force | Out-Null
        }
    }
    
    $targetPath = Join-Path $fullTargetDir $filename
    
    # Check if target already exists
    if (Test-Path $targetPath) {
        $errorMsg = "Target already exists: $targetPath"
        Write-Host "❌ $errorMsg" -ForegroundColor Red
        
        if ($Force) {
            # Generate unique name
            $baseName = [System.IO.Path]::GetFileNameWithoutExtension($filename)
            $counter = 1
            do {
                $newName = "${baseName}_${counter}$extension"
                $targetPath = Join-Path $fullTargetDir $newName
                $counter++
            } while (Test-Path $targetPath)
            
            Write-Host "  Using alternative name: $newName" -ForegroundColor Yellow
        } else {
            $errors += $errorMsg
            continue
        }
    }
    
    # Move the file
    Write-Host "Moving: $filename -> $targetPath" -ForegroundColor Gray
    
    if (-not $DryRun) {
        try {
            Move-Item $file.FullName $targetPath -Force
            $movedCount++
            Write-Host "  ✅ Moved successfully" -ForegroundColor Green
        } catch {
            $errorMsg = "Failed to move ${filename}: $_"
            Write-Host "  ❌ $errorMsg" -ForegroundColor Red
            $errors += $errorMsg
        }
    } else {
        Write-Host "  📝 (Dry run - would move)" -ForegroundColor Blue
        $movedCount++
    }
}

Write-Host "`n=== CLEANUP SUMMARY ===" -ForegroundColor Cyan

if ($DryRun) {
    Write-Host "DRY RUN COMPLETE" -ForegroundColor Blue
    Write-Host "Would move $movedCount files" -ForegroundColor Blue
} else {
    Write-Host "CLEANUP COMPLETE" -ForegroundColor Green
    Write-Host "Moved $movedCount files" -ForegroundColor Green
}

if ($errors.Count -gt 0) {
    Write-Host "`nERRORS ENCOUNTERED:" -ForegroundColor Red
    Write-Host "Total errors: $($errors.Count)" -ForegroundColor Red
    $errors | ForEach-Object { Write-Host "  - $_" -ForegroundColor Red }
    
    Write-Host "`nTo fix errors, run with -Force flag:" -ForegroundColor Yellow
    Write-Host "  .\cleanup-root-files.ps1 -Force" -ForegroundColor Yellow
}

# Update git if files were moved
if ($movedCount -gt 0 -and -not $DryRun) {
    Write-Host "`nUpdating git..." -ForegroundColor Gray
    
    # Add all moved files
    $movedFiles = $rootFiles | Where-Object { $movedCount -gt 0 } | ForEach-Object { $_.Name }
    
    foreach ($file in $movedFiles) {
        # Find new location
        foreach ($pattern in $fileMappings.Keys) {
            if ($file -like $pattern) {
                $targetDir = $fileMappings[$pattern]
                $targetPath = Join-Path $targetDir $file
                
                if (Test-Path $targetPath) {
                    git add $targetPath 2>$null
                    Write-Host "  Added to git: $targetPath" -ForegroundColor Gray
                }
                break
            }
        }
    }
    
    # Remove from root (they've been moved)
    foreach ($file in $movedFiles) {
        if (Test-Path $file) {
            git rm $file 2>$null
        }
    }
    
    # Commit the cleanup
    $commitMessage = "[CLEANUP] Moved $movedCount files from root to proper directories"
    git commit -m $commitMessage --author="Cleanup Bot cleanup@zeta"
    
    Write-Host "`n✅ Git updated with cleanup commit" -ForegroundColor Green
    Write-Host "Commit: $commitMessage" -ForegroundColor Gray
}

Write-Host "`n=== NEXT STEPS ===" -ForegroundColor Yellow
Write-Host "1. Run validation to confirm cleanup:" -ForegroundColor Gray
Write-Host "   .\validate-agent-files.ps1" -ForegroundColor Gray
Write-Host "2. Check for any remaining violations:" -ForegroundColor Gray
Write-Host "   Get-ChildItem -Filter *.rs,*.z | Where-Object { \$_.Directory -eq \$PWD }" -ForegroundColor Gray
Write-Host "3. Update any references to moved files" -ForegroundColor Gray