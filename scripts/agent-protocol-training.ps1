# Zeta Agent Protocol Training
# Run this to learn and practice the quality protocols

Write-Host "ZETA AGENT PROTOCOL TRAINING" -ForegroundColor Cyan
Write-Host "================================" -ForegroundColor Cyan
Write-Host ""

Write-Host "LESSON 1: Why Protocols Matter" -ForegroundColor Yellow
Write-Host "---------------------------------" -ForegroundColor Yellow
Write-Host "In v0.3.11, agents violated protocols and:" -ForegroundColor White
Write-Host "  - Pushed code without rustfmt -> 100+ formatting diffs" -ForegroundColor Red
Write-Host "  - Pushed code with clippy warnings -> CI failures" -ForegroundColor Red
Write-Host "  - Caused release delays -> Factory inefficiency" -ForegroundColor Red
Write-Host ""
Write-Host "Result: Firstborn Zak had to fix violations manually." -ForegroundColor White
Write-Host "Lesson: Protocols prevent wasted time and CI failures." -ForegroundColor Green
Write-Host ""

Write-Host "LESSON 2: The Three Sacred Commands" -ForegroundColor Yellow
Write-Host "----------------------------------------" -ForegroundColor Yellow
Write-Host "ALWAYS run these BEFORE any commit or push:" -ForegroundColor White
Write-Host ""
Write-Host "1. cargo fmt --all" -ForegroundColor Cyan
Write-Host "   Purpose: Ensures consistent code formatting" -ForegroundColor Gray
Write-Host "   Failure: CI will reject your code" -ForegroundColor Gray
Write-Host ""
Write-Host "2. cargo clippy --workspace --all-features --all-targets -- -D warnings" -ForegroundColor Cyan
Write-Host "   Purpose: Catches bugs and code smells" -ForegroundColor Gray
Write-Host "   Auto-fix: cargo clippy --fix --workspace --all-features --all-targets" -ForegroundColor Gray
Write-Host ""
Write-Host "3. cargo check --workspace --all-features --all-targets" -ForegroundColor Cyan
Write-Host "   Purpose: Verifies code compiles" -ForegroundColor Gray
Write-Host "   Failure: Your code won't work" -ForegroundColor Gray
Write-Host ""

Write-Host "LESSON 3: Git Hook Protection" -ForegroundColor Yellow
Write-Host "---------------------------------" -ForegroundColor Yellow
Write-Host "Automatic hooks run when you:" -ForegroundColor White
Write-Host ""
Write-Host "- git commit -> Runs pre-commit hook:" -ForegroundColor Cyan
Write-Host "  - rustfmt check" -ForegroundColor Gray
Write-Host "  - clippy check" -ForegroundColor Gray
Write-Host "  - compilation check" -ForegroundColor Gray
Write-Host ""
Write-Host "- git push -> Runs pre-push hook:" -ForegroundColor Cyan
Write-Host "  - test suite execution" -ForegroundColor Gray
Write-Host "  - uncommitted changes warning" -ForegroundColor Gray
Write-Host ""
Write-Host "To bypass (EMERGENCY ONLY):" -ForegroundColor Red
Write-Host "  git commit --no-verify" -ForegroundColor DarkRed
Write-Host "  git push --no-verify" -ForegroundColor DarkRed
Write-Host "  WARNING: Requires justification in commit message!" -ForegroundColor Red
Write-Host ""

Write-Host "LESSON 4: Complete Workflow" -ForegroundColor Yellow
Write-Host "-------------------------------" -ForegroundColor Yellow
Write-Host "1. Create feature branch:" -ForegroundColor White
Write-Host "   git checkout -b feature/your-feature" -ForegroundColor Cyan
Write-Host ""
Write-Host "2. Make changes and test:" -ForegroundColor White
Write-Host "   cargo fmt --all" -ForegroundColor Cyan
Write-Host "   cargo clippy --workspace --all-features --all-targets" -ForegroundColor Cyan
Write-Host "   cargo test --workspace --all-features" -ForegroundColor Cyan
Write-Host ""
Write-Host "3. Commit with quality gates:" -ForegroundColor White
Write-Host "   git add ." -ForegroundColor Cyan
Write-Host "   git commit -m 'feat: description of changes'" -ForegroundColor Cyan
Write-Host "   ^ This runs pre-commit hook automatically" -ForegroundColor Gray
Write-Host ""
Write-Host "4. Push with validation:" -ForegroundColor White
Write-Host "   git push origin feature/your-feature" -ForegroundColor Cyan
Write-Host "   ^ This runs pre-push hook automatically" -ForegroundColor Gray
Write-Host ""
Write-Host "5. Create Pull Request on GitHub" -ForegroundColor White
Write-Host "6. Wait for CI to pass (green checkmark)" -ForegroundColor White
Write-Host "7. Request review from Firstborn Zak" -ForegroundColor White
Write-Host "8. Merge after approval" -ForegroundColor White
Write-Host ""

Write-Host "PRACTICE EXERCISE" -ForegroundColor Magenta
Write-Host "====================" -ForegroundColor Magenta
Write-Host "Let's practice the protocol. We'll:" -ForegroundColor White
Write-Host "1. Check current code quality" -ForegroundColor White
Write-Host "2. Make a small change" -ForegroundColor White
Write-Host "3. Run through the quality gates" -ForegroundColor White
Write-Host ""

$practice = Read-Host "Run practice exercise? (y/N)"
if ($practice -match "^[Yy]$") {
    Write-Host "`nStarting practice exercise..." -ForegroundColor Green
    
    # Step 1: Check current state
    Write-Host "`nStep 1: Current git status" -ForegroundColor Cyan
    git status --short
    
    # Step 2: Run quality gate enforcer
    Write-Host "`nStep 2: Run quality gate enforcer" -ForegroundColor Cyan
    if (Test-Path "scripts/quality-gate-enforcer.ps1") {
        & "scripts/quality-gate-enforcer.ps1"
    } else {
        Write-Host "Quality gate enforcer not found. Running basic checks..." -ForegroundColor Yellow
        cargo fmt --all -- --check
        cargo clippy --workspace --all-features --all-targets -- -D warnings
        cargo check --workspace --all-features --all-targets
    }
    
    # Step 3: Create a test change
    Write-Host "`nStep 3: Create a practice change" -ForegroundColor Cyan
    $testFile = "test_protocol_practice.md"
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    @"
# Protocol Practice File
Created: $timestamp
Agent: Training Exercise

This file was created to practice the Zeta agent protocols.
It should be deleted after training is complete.

## Quality Gates Practiced:
1. Code formatting (rustfmt)
2. Linting (clippy) 
3. Compilation (cargo check)
4. Git workflow (commit/push hooks)

## Lesson Learned:
Protocols enable parallel development without chaos.
"@ | Out-File -FilePath $testFile -Encoding UTF8
    
    Write-Host "Created practice file: $testFile" -ForegroundColor Green
    
    # Step 4: Run quality gates
    Write-Host "`nStep 4: Practice quality gates" -ForegroundColor Cyan
    Write-Host "1. Formatting: cargo fmt --all" -ForegroundColor White
    cargo fmt --all
    
    Write-Host "`n2. Linting: cargo clippy --workspace --all-features --all-targets -- -D warnings" -ForegroundColor White
    cargo clippy --workspace --all-features --all-targets -- -D warnings
    
    Write-Host "`n3. Compilation: cargo check --workspace --all-features --all-targets" -ForegroundColor White
    cargo check --workspace --all-features --all-targets
    
    # Step 5: Simulate git workflow
    Write-Host "`nStep 5: Simulate git commit (dry run)" -ForegroundColor Cyan
    Write-Host "What would happen if you ran:" -ForegroundColor White
    Write-Host "  git add $testFile" -ForegroundColor Cyan
    Write-Host "  git commit -m 'docs: add protocol practice file'" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "The pre-commit hook would run and check:" -ForegroundColor Gray
    Write-Host "  - rustfmt compliance" -ForegroundColor Gray
    Write-Host "  - clippy warnings" -ForegroundColor Gray
    Write-Host "  - compilation status" -ForegroundColor Gray
    
    # Step 6: Cleanup
    Write-Host "`nStep 6: Cleanup" -ForegroundColor Cyan
    Remove-Item $testFile -ErrorAction SilentlyContinue
    Write-Host "Deleted practice file: $testFile" -ForegroundColor Green
    
    Write-Host "`nPRACTICE COMPLETE!" -ForegroundColor Green
    Write-Host "You've practiced the essential Zeta agent protocols." -ForegroundColor White
    Write-Host "Remember: Quality gates -> Faster development -> More features shipped" -ForegroundColor Green
} else {
    Write-Host "Skipping practice exercise." -ForegroundColor Gray
}

Write-Host "`nADDITIONAL RESOURCES" -ForegroundColor Yellow
Write-Host "----------------------" -ForegroundColor Yellow
Write-Host "- AGENT_PROTOCOL_COMPLIANCE.md - Full protocol documentation" -ForegroundColor White
Write-Host "- .github/workflows/ci.yml - CI configuration" -ForegroundColor White
Write-Host "- .git/hooks/ - Git hook implementations" -ForegroundColor White
Write-Host "- memory/2026-03-29.md - v0.3.11 violation case study" -ForegroundColor White
Write-Host ""

Write-Host "KEY TAKEAWAY" -ForegroundColor Magenta
Write-Host "Protocols aren't bureaucracy. They're the foundation that enables" -ForegroundColor White
Write-Host "5 agents to work in parallel without breaking each other's code." -ForegroundColor White
Write-Host ""
Write-Host "First Principles. Parallel Execution. Quality First." -ForegroundColor Cyan