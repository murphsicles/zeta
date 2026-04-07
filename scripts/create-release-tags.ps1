# Create release tags for v0.3.29 through v0.3.50

# Get current commit hash
$commitHash = git rev-parse HEAD

# Create tags for each version
for ($i = 29; $i -le 50; $i++) {
    $version = "v0.3.$i"
    
    # Check if tag already exists
    $tagExists = git tag --list $version
    if ($tagExists) {
        Write-Host "Tag $version already exists, skipping..."
        continue
    }
    
    # Create annotated tag
    git tag -a $version -m "Zeta Compiler $version
    
    Major release with significant improvements and new features.
    See docs/releases/$version.md for detailed release notes."
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "Created tag $version"
    } else {
        Write-Host "Failed to create tag $version"
    }
}

# Show all tags
Write-Host "`nAll tags:"
git tag --list | Select-String -Pattern "v0\.3\.[0-9]+" | Sort-Object