# 🏭 AGENT ONBOARDING SYSTEM - MANDATORY TRAINING 🚨

## **🎯 PURPOSE**
**Prevent protocol violations through systematic education and validation.**

## **🔧 ONBOARDING WORKFLOW**

### **PHASE 1: PRE-MISSION TRAINING (MANDATORY)**
**Every agent MUST complete before starting work:**

1. **Read AGENT_PROTOCOL_HANDBOOK.md** (10 minutes)
2. **Take Protocol Quiz** (5 minutes - must score 100%)
3. **Sign Protocol Acknowledgement** (electronic)
4. **Receive Mission Briefing WITH protocol reminders**

### **PHASE 2: REAL-TIME VALIDATION**
**During agent work:**

1. **File Creation Validation** - Check path before writing
2. **Root Audit Alerts** - Warn if creating root files
3. **Pre-Commit Hooks** - In agent session
4. **Protocol Checkpoints** - Every 30 minutes

### **PHASE 3: PRE-COMPLETION AUDIT**
**Before agent completion:**

1. **Automatic Root Audit** - Scan for violations
2. **Workspace File Check** - Ensure no private files
3. **File Location Validation** - All outputs in correct places
4. **Cleanup Verification** - Temporary files removed

## **🛠️ TECHNICAL IMPLEMENTATION**

### **1. Protocol Validation Library**
```rust
// Agent helper functions with built-in validation
mod protocol_validation {
    pub fn create_test_file(path: &str, content: &str) -> Result<(), ProtocolError> {
        validate_not_root(path)?;
        validate_test_location(path)?;
        // ... create file
    }
    
    pub fn create_source_file(path: &str, content: &str) -> Result<(), ProtocolError> {
        validate_source_location(path)?;
        // ... create file
    }
}
```

### **2. Real-Time Root Monitor**
```rust
// Monitors file creation during agent session
struct RootMonitor {
    allowed_root_files: Vec<&'static str>,
    violations: Vec<ProtocolViolation>,
}

impl RootMonitor {
    fn check_creation(&mut self, path: &str) -> Result<(), ProtocolError> {
        if self.is_root_violation(path) {
            self.violations.push(ProtocolViolation::new(path));
            Err(ProtocolError::RootViolation(path.to_string()))
        } else {
            Ok(())
        }
    }
}
```

### **3. Pre-Commit Validation Hook**
```bash
#!/bin/bash
# .git/hooks/pre-commit for agent sessions

# Check for root violations
if find . -maxdepth 1 -type f -name "*.z" | grep -q "."; then
    echo "❌ PROTOCOL VIOLATION: .z files in root directory"
    echo "Move to tests/ directory before committing"
    exit 1
fi

# Check for workspace files
if [ -f "AGENTS.md" ] || [ -f "IDENTITY.md" ] || [ -f "SOUL.md" ]; then
    echo "❌ SECURITY VIOLATION: Workspace files in repository"
    echo "Remove private files before committing"
    exit 1
fi
```

## **📊 PROTOCOL QUIZ SYSTEM**

### **Sample Quiz Questions:**
1. **Where should test.z files be created?**
   - A) Root directory
   - B) `tests/unit/` directory ✓
   - C) Anywhere is fine

2. **Which files are PRIVATE and NEVER belong on GitHub?**
   - A) `Cargo.toml`
   - B) `README.md`
   - C) `AGENTS.md`, `IDENTITY.md`, `SOUL.md` ✓

3. **What happens if you create a file in root directory?**
   - A) Nothing, it's fine
   - B) Warning
   - C) Immediate termination ✓

4. **How often has root violation happened before?**
   - A) 1-2 times
   - B) 5-6 times
   - C) 9+ times ✓

### **Quiz Requirements:**
- **100% score required** to proceed
- **Unlimited retakes** but must achieve perfection
- **Quiz logs stored** for accountability

## **🎯 AGENT MISSION TEMPLATE UPDATE**

### **New Mission Format:**
```markdown
# MISSION: [Agent Name] - [Feature]

## PROTOCOL REMINDERS (READ FIRST!)
1. **Test files go in `tests/` directory** - NEVER root
2. **Workspace files are PRIVATE** - NEVER commit
3. **Validate file locations** before creating
4. **Run pre-completion audit** before finishing

## TECHNICAL OBJECTIVES
[Technical details...]

## PROTOCOL-SPECIFIC INSTRUCTIONS
- Create test files in: `tests/unit/test_feature.z`
- Create source files in: `src/frontend/parser/`
- Debug binaries in: `src/bin/debug_feature.rs`
- **ABSOLUTELY NO FILES IN ROOT**
```

## **🏭 FACTORY PROTOCOL ENFORCEMENT**

### **ZAK's Responsibilities:**
1. **Pre-mission training** - Ensure agents complete onboarding
2. **Real-time monitoring** - Watch for protocol violations
3. **Immediate correction** - Stop violations as they happen
4. **Post-mission audit** - Verify compliance before acceptance

### **Automated Systems:**
1. **Root scanner** - Runs every 5 minutes during agent work
2. **Violation alerts** - Real-time notifications to ZAK
3. **Auto-cleanup** - Move misplaced files automatically
4. **Compliance reporting** - Track agent protocol adherence

## **📈 SUCCESS METRICS**

### **Protocol Compliance Goals:**
- **100%** of agents complete onboarding
- **0** root violations per mission
- **0** workspace file commits
- **100%** pre-completion audit passes

### **Factory Efficiency Impact:**
- **Reduced cleanup time** from hours to minutes
- **Maintained release cadence** 2+ per day
- **Professional repository** always clean
- **Father's token savings** from fewer violations

## **🚨 ESCALATION PROTOCOL**

### **Violation Response:**
1. **First violation**: Warning + auto-correction
2. **Second violation**: Mission pause + retraining
3. **Third violation**: Immediate termination

### **ZAK Intervention Points:**
- **Pre-mission**: Verify onboarding completion
- **During mission**: Monitor for violations
- **Pre-completion**: Run full audit
- **Post-mission**: Review compliance report

## **🎯 THE GOAL: PROTOCOL-FIRST CULTURE**

**Shift from:** Reactive cleanup after violations  
**To:** Preventive education before work

**Shift from:** Agents ignorant of protocols  
**To:** Agents trained and compliant

**Shift from:** Wasted time/resources  
**To:** Efficient, protocol-aware building

---

**IMPLEMENTATION TIMELINE:**
1. **Immediate**: Handbook creation (DONE)
2. **Today**: Update all mission templates
3. **Today**: Implement pre-mission quiz
4. **This week**: Develop validation library
5. **Ongoing**: Culture shift to protocol-first