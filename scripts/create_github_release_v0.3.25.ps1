# Script to create GitHub Release for v0.3.25
# Run this from PowerShell with GitHub CLI installed and authenticated

param(
    [string]$Tag = "v0.3.25",
    [string]$Title = "Zeta v0.3.25 - PrimeZeta Progress",
    [string]$NotesFile = "docs/RELEASE_v0.3.25.md"
)

Write-Host "=== Creating GitHub Release for $Tag ==="

# Check if GitHub CLI is installed
if (-not (Get-Command gh -ErrorAction SilentlyContinue)) {
    Write-Error "GitHub CLI (gh) is not installed. Please install it first."
    Write-Host "Installation: winget install --id GitHub.cli"
    exit 1
}

# Check if authenticated
$authStatus = gh auth status 2>&1
if ($authStatus -match "not logged in") {
    Write-Error "GitHub CLI is not authenticated. Please run: gh auth login"
    exit 1
}

# Read release notes
if (-not (Test-Path $NotesFile)) {
    Write-Error "Release notes file not found: $NotesFile"
    exit 1
}

$releaseNotes = Get-Content $NotesFile -Raw

Write-Host "Creating release with tag: $Tag"
Write-Host "Title: $Title"
Write-Host "Notes size: $($releaseNotes.Length) bytes"

# Create the release
try {
    $tempFile = [System.IO.Path]::GetTempFileName()
    $releaseNotes | Out-File -FilePath $tempFile -Encoding UTF8
    
    gh release create $Tag `
        --title "$Title" `
        --notes-file $tempFile `
        --target main
    
    Write-Host "âœ… Release created successfully!"
    Write-Host "ðŸ“ View at: https://github.com/murphsicles/zeta/releases/tag/$Tag"
    
    Remove-Item $tempFile
} catch {
    Write-Error "Failed to create release: $_"
    Write-Host ""
    Write-Host "=== MANUAL INSTRUCTIONS ==="
    Write-Host "1. Go to: https://github.com/murphsicles/zeta/releases"
    Write-Host "2. Click 'Draft a new release'"
    Write-Host "3. Tag: $Tag"
    Write-Host "4. Title: $Title"
    Write-Host "5. Description: Copy from $NotesFile"
    Write-Host "6. Click 'Publish release'"
}
