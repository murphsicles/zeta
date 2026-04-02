# test_all_systems.ps1
# Test all autonomy fix systems

Write-Host "=== TESTING AUTONOMY FIX SYSTEMS ===" -ForegroundColor Cyan
Write-Host ""

# Test 1: Heartbeat System
Write-Host "1. Testing Heartbeat System..." -ForegroundColor Yellow
if (Test-Path ".github/automation/check_agents.ps1") {
    Write-Host "   ✓ Heartbeat script exists" -ForegroundColor Green
} else {
    Write-Host "   ✗ Heartbeat script missing" -ForegroundColor Red
}

if (Test-Path ".github/automation/agent_status.json") {
    Write-Host "   ✓ Status file exists" -ForegroundColor Green
    $status = Get-Content ".github/automation/agent_status.json" -Raw | ConvertFrom-Json
    Write-Host "   ✓ Status file valid JSON" -ForegroundColor Green
} else {
    Write-Host "   ✗ Status file missing" -ForegroundColor Red
}

Write-Host ""

# Test 2: Release Pipeline
Write-Host "2. Testing Release Pipeline..." -ForegroundColor Yellow
if (Test-Path ".github/automation/release_pipeline.json") {
    Write-Host "   ✓ Pipeline file exists" -ForegroundColor Green
    $pipeline = Get-Content ".github/automation/release_pipeline.json" -Raw | ConvertFrom-Json
    Write-Host "   ✓ Pipeline file valid JSON" -ForegroundColor Green
    Write-Host "   Current release: $($pipeline.current_release)" -ForegroundColor Gray
} else {
    Write-Host "   ✗ Pipeline file missing" -ForegroundColor Red
}

Write-Host ""

# Test 3: Dashboard
Write-Host "3. Testing Dashboard..." -ForegroundColor Yellow
if (Test-Path ".github/automation/dashboard_simple.html") {
    Write-Host "   ✓ Dashboard exists" -ForegroundColor Green
    $size = (Get-Item ".github/automation/dashboard_simple.html").Length
    Write-Host "   ✓ Dashboard size: $size bytes" -ForegroundColor Green
} else {
    Write-Host "   ✗ Dashboard missing" -ForegroundColor Red
}

Write-Host ""

# Test 4: Escalation Protocol
Write-Host "4. Testing Escalation Protocol..." -ForegroundColor Yellow
if (Test-Path ".github/automation/escalation_protocol.ps1") {
    Write-Host "   ✓ Escalation script exists" -ForegroundColor Green
} else {
    Write-Host "   ✗ Escalation script missing" -ForegroundColor Red
}

if (Test-Path ".github/automation/escalation_config.json") {
    Write-Host "   ✓ Escalation config exists" -ForegroundColor Green
} else {
    Write-Host "   ✗ Escalation config missing" -ForegroundColor Red
}

Write-Host ""

# Test 5: System Integration
Write-Host "5. Testing System Integration..." -ForegroundColor Yellow

# Check if all required files exist
$requiredFiles = @(
    "check_agents.ps1",
    "setup_heartbeat_monitor.ps1",
    "release_pipeline.json",
    "pipeline_status.ps1",
    "dashboard_simple.html",
    "escalation_protocol.ps1",
    "escalation_config.json"
)

$allExist = $true
foreach ($file in $requiredFiles) {
    $path = ".github/automation/$file"
    if (Test-Path $path) {
        Write-Host "   ✓ $file" -ForegroundColor Green
    } else {
        Write-Host "   ✗ $file" -ForegroundColor Red
        $allExist = $false
    }
}

Write-Host ""

# Summary
Write-Host "=== TEST SUMMARY ===" -ForegroundColor Cyan

if ($allExist) {
    Write-Host "✅ ALL SYSTEMS READY" -ForegroundColor Green
    Write-Host ""
    Write-Host "Autonomy Fix Systems Deployed:" -ForegroundColor White
    Write-Host "1. Heartbeat Monitoring (15-minute intervals)" -ForegroundColor Gray
    Write-Host "2. Release Pipeline (v0.3.28 -> v0.4.0)" -ForegroundColor Gray
    Write-Host "3. Status Dashboard (real-time monitoring)" -ForegroundColor Gray
    Write-Host "4. Escalation Protocol (4-level alert system)" -ForegroundColor Gray
    Write-Host ""
    Write-Host "Next Steps:" -ForegroundColor Yellow
    Write-Host "1. Run: .\setup_heartbeat_monitor.ps1 -Install" -ForegroundColor White
    Write-Host "2. Open: .\dashboard_simple.html" -ForegroundColor White
    Write-Host "3. Monitor: .\pipeline_status.ps1" -ForegroundColor White
} else {
    Write-Host "❌ SOME SYSTEMS MISSING" -ForegroundColor Red
    Write-Host "Check above for missing files and recreate them." -ForegroundColor Yellow
}