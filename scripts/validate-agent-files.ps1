# validate-agent-files.ps1
# Pre-commit validation for agent file locations
# Violation = Immediate termination

param(
    [string]$CommitMessage = "",
    [switch]$PreCommit,
    [switch]$Audit
)

Write-Host "=== AGENT FILE VALIDATION ===" -ForegroundColor Cyan
Write-Host "Mode: $(if ($PreCommit) { 'Pre-Commit' } elseif ($Audit) { 'Audit' } else { 'Standard' })"
Write-Host ""

# Allowed directories for file types
$allowedDirs = @{
    '.rs' = @('src', 'tests', 'examples', 'benches', 'verification', 'scripts')
    '.z' = @('tests', 'examples', 'zeta_src', 'zorb', 'zeta_src/tests')
    '.ps1' = @('scripts')
    '.md' = @('.', 'design-docs', 'docs', 'memory')  # Root allowed for documentation
}

# Valid agent tags
$validAgentTags = @(
    'ZAK', 'SEM', 'SYN', 'GEN', 'LEX', 'VER', 'CRON', 
    'BOOTSTRAP', 'PERF-AUDIT', 'UPDATE'
)

$specialTags = @('FIX', 'FEAT', 'TEST', 'CI', 'QUALITY', 'CLIPPY', 'SECURITY', 'EMERGENCY')

# Check 1: Agent tag in commit message
if ($PreCommit) {
    Write-Host "Checking commit message for agent tag..." -ForegroundColor Gray
    
    # Get commit message from parameter
    if ([string]::IsNullOrEmpty($CommitMessage)) {
        # In pre-commit hook, message should be passed as parameter
        Write-Host "⚠️  Warning: No commit message provided" -ForegroundColor Yellow
        Write-Host "   Agent tag validation skipped (will fail in production)" -ForegroundColor Gray
        return
    }
    
    Write-Host "Commit message: $CommitMessage" -ForegroundColor Gray
    
    $agentTag = $null
    if ($CommitMessage -match '\[([A-Za-z0-9\-]+)\]') {
        $agentTag = $matches[1]
    }
    
    if (-not $agentTag) {
        Write-Host "❌ VIOLATION: No agent tag in commit message" -ForegroundColor Red
        Write-Host "   Commit must include [AGENT] tag (e.g., [SEM], [GEN-FIX])" -ForegroundColor Yellow
        Write-Host "   Blocking commit..." -ForegroundColor Red
        exit 1
    }
    
    # Validate agent tag format
    $isValid = $false
    if ($validAgentTags -contains $agentTag) {
        $isValid = $true
    }
    elseif ($agentTag -match "^([A-Z]+)\-([A-Z]+)$") {
        $agent = $matches[1]
        $special = $matches[2]
        if ($validAgentTags -contains $agent -and $specialTags -contains $special) {
            $isValid = $true
        }
    }
    
    if (-not $isValid) {
        Write-Host "❌ VIOLATION: Invalid agent tag: [$agentTag]" -ForegroundColor Red
        Write-Host "   Valid tags: $($validAgentTags -join ', ')" -ForegroundColor Yellow
        Write-Host "   Special tags: $($specialTags -join ', ') (must combine: [AGENT-SPECIAL])" -ForegroundColor Yellow
        Write-Host "   Blocking commit..." -ForegroundColor Red
        exit 1
    }
    
    Write-Host "✅ Agent tag valid: [$agentTag]" -ForegroundColor Green
}

# Check 2: Files in root directory
Write-Host "`nChecking for files in root directory..." -ForegroundColor Gray

$violations = @()
$filesToCheck = @()

if ($PreCommit) {
    # Get staged files for pre-commit
    $filesToCheck = git diff --cached --name-only --diff-filter=A
} else {
    # Get all files for audit
    $filesToCheck = Get-ChildItem -Recurse -File | Where-Object { $_.Extension -in '.rs', '.z', '.ps1' } | ForEach-Object { $_.FullName }
}

foreach ($file in $filesToCheck) {
    $filename = Split-Path $file -Leaf
    $extension = [System.IO.Path]::GetExtension($filename)
    
    if ($extension -in '.rs', '.z', '.ps1') {
        # Check if file is in root (no directory in path)
        $dir = Split-Path $file -Parent
        $isInRoot = ($dir -eq '.' -or $dir -eq $PWD.Path)
        
        if ($isInRoot) {
            $violations += $filename
            Write-Host "  ❌ $filename - IN ROOT DIRECTORY" -ForegroundColor Red
        } else {
            # Check if in allowed directory
            $parentDir = (Split-Path $dir -Leaf).ToLower()
            $allowed = $false
            
            if ($allowedDirs.ContainsKey($extension)) {
                foreach ($allowedDir in $allowedDirs[$extension]) {
                    if ($dir -like "*$allowedDir*") {
                        $allowed = $true
                        break
                    }
                }
            }
            
            if (-not $allowed) {
                $violations += "$filename (in $dir)"
                Write-Host "  ❌ $filename - IN WRONG DIRECTORY: $dir" -ForegroundColor Red
            } else {
                Write-Host "  ✅ $filename" -ForegroundColor Green
            }
        }
    }
}

# Report results
if ($violations.Count -gt 0) {
    Write-Host "`n❌❌❌ PROTOCOL VIOLATIONS DETECTED ❌❌❌" -ForegroundColor Red -BackgroundColor Black
    Write-Host "`nViolating files ($($violations.Count) total):" -ForegroundColor Yellow
    $violations | ForEach-Object { Write-Host "  - $_" -ForegroundColor Red }
    
    if ($PreCommit) {
        Write-Host "`n🚫 COMMIT BLOCKED" -ForegroundColor Red
        Write-Host "Run cleanup script: scripts/cleanup-root-files.ps1" -ForegroundColor Yellow
        Write-Host "Then stage files in correct locations and try again." -ForegroundColor Yellow
    } else {
        Write-Host "`n🚨 AGENT TERMINATION TRIGGERED" -ForegroundColor Red
        Write-Host "Running agent termination procedure..." -ForegroundColor Red
        
        # Trigger termination
        if (Test-Path "scripts/agent-termination.ps1") {
            & "scripts/agent-termination.ps1" -Violations $violations
        } else {
            Write-Host "Termination script not found. Manual intervention required." -ForegroundColor Red
        }
    }
    
    exit 1
} else {
    Write-Host "`n✅ ALL FILES COMPLY WITH PROTOCOL" -ForegroundColor Green
    
    if ($PreCommit) {
        Write-Host "Commit allowed to proceed." -ForegroundColor Green
    }
    
    exit 0
}