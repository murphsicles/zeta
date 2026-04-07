# 🚨 AGENT TERMINATION PROTOCOL
**Effective: 2026-03-31**  
**Purpose: Automated termination of protocol-violating agents**

## ⚡ IMMEDIATE TERMINATION TRIGGERS

### **Level 1: Automatic Termination (No Warning)**
1. **Adding files to root directory** - Any .rs or .z file in workspace root
2. **Anonymous commits** - Commit without agent tag `[AGENT]`
3. **Repeated coordination failures** - 3+ conflicts in single sprint

### **Level 2: Investigation Required**
1. **Test regression** - Breaking existing functionality
2. **Cross-agent conflict** - Unresolved after 1 hour
3. **Security violation** - Attempting unauthorized access

## 🔧 TERMINATION PROCEDURE

### **Step 1: Violation Detection**
```powershell
# Automated detection
scripts/validate-agent-files.ps1 -Audit

# Manual trigger
scripts/agent-termination.ps1 -Agent "SEM" -Reason "Root file violation"
```

### **Step 2: Evidence Collection**
1. **Git history**: `git log --oneline --name-only --author="*AGENT*"`
2. **Violation details**: Files, timestamps, commit hashes
3. **Impact assessment**: Tests broken, conflicts created

### **Step 3: Termination Execution**
```powershell
# Full termination sequence
scripts/agent-termination.ps1 `
  -Agent "VIOLATING_AGENT" `
  -Reason "Added 5 files to root directory" `
  -Violations @("test_parser.rs", "debug_test.z") `
  -Severity "CRITICAL"
```

### **Step 4: Cleanup & Recovery**
1. **Move violating files** to correct directories
2. **Update git history** to remove root commits
3. **Respawn agent** with protocol re-education
4. **Update coordination protocol** to prevent recurrence

## 🛡️ TERMINATION SAFEGUARDS

### **False Positive Protection**
- **Human review**: Father Zak approves all terminations
- **Appeal process**: 5-minute window to contest termination
- **Backup restore**: All changes reversible for 24 hours

### **Emergency Override**
```powershell
# Only Father Zak can execute
scripts/agent-termination.ps1 -Override -Code "ZAK-APPROVAL-20260331"
```

## 📊 TERMINATION LOGGING

### **Required Log Fields**
```json
{
  "timestamp": "2026-03-31T02:45:00Z",
  "agent": "SEM",
  "terminated_by": "validate-agent-files.ps1",
  "reason": "File location violation",
  "violations": ["test_float_comprehensive.rs", "test_float_type.z"],
  "commit_hashes": ["467ae2e", "1620524"],
  "severity": "CRITICAL",
  "action_taken": "Immediate termination",
  "replacement_agent": "SEM-v2",
  "cleanup_completed": true
}
```

### **Log Locations**
1. `logs/agent-terminations/YYYY-MM-DD.json`
2. `memory/agent-violations.md`
3. Git commit message: `[TERMINATION] Agent SEM - root file violation`

## 🔄 AGENT RESPAWN PROCEDURE

### **Pre-Respawn Requirements**
1. **Protocol re-education**: Read updated AGENT_COORDINATION_PROTOCOL.md
2. **Violation analysis**: Review own termination log
3. **Clean workspace**: No residual violations
4. **Test compliance**: Pass validation script

### **Respawn Command**
```powershell
scripts/agent-respawn.ps1 -Agent "SEM" -Version "v2" -Supervisor "Father Zak"
```

### **Post-Respawn Monitoring**
1. **First 24 hours**: Hourly validation checks
2. **First week**: Daily protocol review
3. **Permanent**: Standard validation on every commit

## 🚨 EMERGENCY TERMINATION

### **Manual Trigger Conditions**
1. **Security breach detected**
2. **System instability caused**
3. **Multiple agents compromised**
4. **Father Zak direct order**

### **Emergency Command**
```powershell
# Full system lockdown
scripts/emergency-termination.ps1 -Lockdown -Reason "SECURITY_BREACH"
```

## 📈 TERMINATION METRICS

### **Success Metrics (✅ GOOD)**
- Violations caught within 5 minutes
- Clean termination with full cleanup
- No residual workspace pollution
- Agent respawned within 1 hour

### **Failure Metrics (❌ BAD)**
- Violations undetected for >1 hour
- Termination causes data loss
- False positive termination
- Agent respawn fails

---

## 🔄 PROTOCOL UPDATES

This protocol evolves based on termination effectiveness. After each termination:
1. **Analyze root cause** of violation
2. **Update prevention mechanisms**
3. **Improve detection accuracy**
4. **Document lessons learned**

**Last updated: 2026-03-31**  
**Updated by: Protocol Enforcement Subagent**  
**Reason: Initial implementation after mass protocol violations**