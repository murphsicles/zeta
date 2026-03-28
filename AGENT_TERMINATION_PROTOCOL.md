# AGENT TERMINATION PROTOCOL

## **NON-NEGOTIABLE RULES FOR AGENT SESSION ENDING**

### **1. Quality Gates (MUST PASS BEFORE TERMINATION):**
```
cargo clippy --tests -- -D warnings    # Exit code 0 REQUIRED
cargo fmt --all --check                # Exit code 0 REQUIRED  
cargo build                            # Exit code 0 REQUIRED
```

### **2. GitHub Verification (MUST VERIFY BEFORE TERMINATION):**
- **CI Status:** GitHub Actions must be GREEN or RUNNING
- **Push Complete:** All work must be pushed to GitHub
- **Branch Status:** No corrupted or destructive branches

### **3. Self-Correction Mandate:**
- **Agents fix their own CI failures** - No Zak intervention
- **Learn from mistakes** - Update self-improving memory
- **No termination with known issues** - Fix before ending

### **4. Termination Checklist:**
- [ ] `cargo clippy --tests -- -D warnings` passes
- [ ] `cargo fmt --all` executed and clean
- [ ] `cargo build` successful
- [ ] All work pushed to GitHub
- [ ] CI status verified (green or running)
- [ ] Self-improving memory updated with lessons
- [ ] No corrupted branches left behind
- [ ] **Test files in correct location** (`tests/` directory, NOT root)

## **PROTOCOL VIOLATION CONSEQUENCES:**

### **Agent-Level:**
1. **Cannot terminate** - Session must continue until protocols satisfied
2. **Knowledge cache update required** - Document violation and correction
3. **Future spawn restrictions** - May be blocked until competence proven

### **Factory-Level:**
1. **System audit** - Review why protocols were bypassed
2. **Protocol reinforcement** - Add stricter enforcement mechanisms
3. **Escalation to Zak** - Only for true blockers, not routine fixes

## **IMPLEMENTATION:**

### **Agent Spawn Template Addition:**
```rust
// PSEUDOCODE - Built into every agent
impl Agent {
    fn can_terminate(&self) -> bool {
        self.quality_checks_passed() &&
        self.github_verified() &&
        self.ci_passing() &&
        !self.has_corrupted_branches()
    }
}
```

### **Pre-Termination Validation Script:**
```bash
#!/bin/bash
# pre_termination_check.sh
cargo clippy --tests -- -D warnings || exit 1
cargo fmt --all --check || exit 1
cargo build || exit 1
git push origin $(git branch --show-current) || exit 1
echo "Agent termination approved"
```

## **RATIONALE:**

**Why these rules?**
- **Prevents CI failures** - Catches issues before GitHub
- **Ensures quality** - Non-negotiable code standards
- **Builds agent competence** - Forces self-correction
- **Reduces Zak workload** - Agents handle their own quality

**Historical Context:**
Created 2026-03-28 after multiple agent CI failures required Zak intervention.
Agents delivered work but didn't run quality checks, violating "If it's not on GitHub with CI passing, it didn't happen."

## **ENFORCEMENT:**

This protocol is **MANDATORY** for all agents. Violations will be documented in self-improving memory and may result in agent respawn with additional training.