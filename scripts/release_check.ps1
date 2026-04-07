# release_check.ps1
# Validates release creation protocol

param(
    [string]$Version,
    [switch]$CheckOnly = $false
)

$ErrorActionPreference = "Stop"

function Write-Error {
    param($message)
    Write-Host "[ERROR] $message" -ForegroundColor Red
}

function Write-Warning {
    param($message)
    Write-Host "[WARNING] $message" -ForegroundColor Yellow
}

function Write-Info {
    param($message)
    Write-Host "[INFO] $message" -ForegroundColor Cyan
}

function Write-Success {
    param($message)
    Write-Host "[SUCCESS] $message" -ForegroundColor Green
}

function Test-VersionFormat {
    param($version)
    
    if ($version -notmatch "^v\d+\.\d+\.\d+$") {
        Write-Error "Version must match format vX.Y.Z (e.g., v0.3.54)"
        return $false
    }
    
    # Check if version is higher than current tags
    $currentTags = git tag --list | Where-Object { $_ -match "^v\d+\.\d+\.\d+$" } | Sort-Object { [version]($_ -replace '^v', '') }
    if ($currentTags.Count -gt 0) {
        $latestTag = $currentTags[-1]
        $latestVersion = [version]($latestTag -replace '^v', '')
        $newVersion = [version]($version -replace '^v', '')
        
        if ($newVersion -le $latestVersion) {
            Write-Error "New version $version must be greater than latest tag $latestTag"
            return $false
        }
    }
    
    return $true
}

function Test-ReleaseNotes {
    param($version)
    
    $changelogPath = "CHANGELOG.md"
    if (-not (Test-Path $changelogPath)) {
        Write-Error "CHANGELOG.md not found"
        return $false
    }
    
    $changelogContent = Get-Content $changelogPath -Raw
    
    # Check if version has release notes
    if ($changelogContent -notmatch "\[$version\]") {
        Write-Error "No release notes found for $version in CHANGELOG.md"
        Write-Info "Add release notes to CHANGELOG.md before creating tag"
        return $false
    }
    
    # Check release notes format
    $pattern = "## \[$version\].*?\n\n(### Added|### Changed|### Fixed|### Security|### Removed|### Deprecated)"
    if ($changelogContent -notmatch $pattern) {
        Write-Warning "Release notes for $version may not follow standard format"
        Write-Info "Expected sections: Added, Changed, Fixed, Security, Removed, or Deprecated"
    }
    
    return $true
}

function Test-RepositoryState {
    # Check for uncommitted changes
    $status = git status --porcelain
    if ($status) {
        Write-Warning "There are uncommitted changes:"
        $status | ForEach-Object { Write-Host "  $_" -ForegroundColor Gray }
        Write-Info "Commit or stash changes before creating release"
        return $false
    }
    
    return $true
}

function Create-Release {
    param($version)
    
    Write-Info "Creating release $version..."
    
    # Create tag
    Write-Info "Creating git tag: $version"
    git tag $version
    if ($LASTEXITCODE -ne 0) {
        Write-Error "Failed to create tag"
        return $false
    }
    
    # Push tag
    Write-Info "Pushing tag to remote..."
    git push origin $version
    if ($LASTEXITCODE -ne 0) {
        Write-Error "Failed to push tag"
        return $false
    }
    
    return $true
}

# Main execution
Write-Host "=========================================" -ForegroundColor Magenta
Write-Host "GitHub Release Protocol Check" -ForegroundColor Magenta
Write-Host "=========================================" -ForegroundColor Magenta

if (-not $Version) {
    Write-Error "Version parameter is required"
    Write-Host "Usage: .\release_check.ps1 -Version vX.Y.Z [-CheckOnly]" -ForegroundColor Yellow
    exit 1
}

# Validate version format
if (-not (Test-VersionFormat $Version)) {
    exit 1
}

# Check repository state
if (-not (Test-RepositoryState)) {
    if (-not $CheckOnly) {
        Write-Error "Cannot create release with uncommitted changes"
        exit 1
    }
}

# Check release notes
if (-not (Test-ReleaseNotes $Version)) {
    if (-not $CheckOnly) {
        Write-Error "Cannot create release without proper release notes"
        exit 1
    }
}

if ($CheckOnly) {
    Write-Success "Release $version passes all checks"
    Write-Info "Ready to create release with: git tag $version && git push origin $version"
    exit 0
} else {
    # Create release
    if (Create-Release $Version) {
        Write-Success "Release $version created successfully!"
        Write-Info "GitHub Actions should automatically create the release on GitHub"
        exit 0
    } else {
        exit 1
    }
}