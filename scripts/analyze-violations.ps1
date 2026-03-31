# Analyze agent protocol violations
Write-Host "=== AGENT PROTOCOL VIOLATION ANALYSIS ===" -ForegroundColor Red
Write-Host ""

# Get all commits that added files to root
$commits = git log --oneline --name-only --diff-filter=A -- "*.rs" "*.z"

$currentCommit = ""
$violations = @{}
$agentPattern = "\[(ZAK|SEM|SYN|GEN|LEX|VER|CRON|BOOTSTRAP|PERF-AUDIT|UPDATE|FEAT|TEST|FIX|QUALITY|CI|CLIPPY|FIRST-PRINCIPLES|SECURITY|EMERGENCY|PRIVACY|CHORE|MERGE|v\d+\.\d+\.\d+)\]"

foreach ($line in $commits) {
    if ($line -match "^([a-f0-9]{7}) ") {
        $currentCommit = $matches[1]
        $commitMessage = $line.Substring(8)
        
        # Check if commit has agent tag
        if ($commitMessage -match $agentPattern) {
            $agent = $matches[1]
        } else {
            $agent = "UNKNOWN"
        }
    }
    elseif ($line -match '\.(rs|z)$' -and $line -notmatch '/' -and $currentCommit -ne "") {
        # File added to root directory
        if (-not $violations.ContainsKey($agent)) {
            $violations[$agent] = @{
                Count = 0
                Files = @()
                Commits = @()
            }
        }
        
        $violations[$agent].Count++
        $violations[$agent].Files += $line
        if (-not $violations[$agent].Commits.Contains($currentCommit)) {
            $violations[$agent].Commits += $currentCommit
        }
    }
}

Write-Host "VIOLATING AGENTS SUMMARY:" -ForegroundColor Yellow
Write-Host ""

foreach ($agent in $violations.Keys | Sort-Object) {
    $data = $violations[$agent]
    Write-Host "AGENT: $agent" -ForegroundColor Cyan
    Write-Host "  Violations: $($data.Count) files in root directory"
    Write-Host "  Commits: $($data.Commits -join ', ')"
    Write-Host "  Sample files:" -ForegroundColor Gray
    $data.Files | Select-Object -First 5 | ForEach-Object {
        Write-Host "    - $_"
    }
    if ($data.Files.Count -gt 5) {
        Write-Host "    ... and $($data.Files.Count - 5) more"
    }
    Write-Host ""
}

Write-Host "=== RECOMMENDED ACTIONS ===" -ForegroundColor Green
Write-Host "1. Terminate agents with most violations"
Write-Host "2. Move all root files to proper directories"
Write-Host "3. Update protocol with strict file location rules"
Write-Host "4. Implement pre-commit validation"