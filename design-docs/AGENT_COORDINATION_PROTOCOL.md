# 🏭 AGENT COORDINATION PROTOCOL
**Effective: 2026-03-30**  
**Purpose: Prevent cross-agent conflicts and ensure feature compatibility**

## 🚨 CRITICAL RULES

### 1. **Feature vs Optimization Coordination**
- **Performance agents** MUST test that optimizations don't break existing features
- **Feature agents** MUST verify their features work after performance optimizations
- **Integration testing** required before any agent completes work

### 2. **Agent Dependency Mapping**
Before starting work, agents MUST:
1. **Identify dependencies**: What other agent work might conflict?
2. **Check active agents**: Who else is working on related code?
3. **Coordinate schedules**: Stagger conflicting work if possible

### 3. **Integration Checkpoints**
- **Hourly check-ins**: Agents report progress and test integration
- **Cross-testing**: Feature agents test against performance changes
- **Father Zak oversight**: All conflicts reported immediately

## 🔧 TECHNICAL PROTOCOLS

### A. **Pre-Work Validation**
```rust
// Before starting optimization work:
1. Run ALL existing tests (cargo test --workspace)
2. Record baseline performance (cargo bench)
3. Verify no regressions in feature tests
```

### B. **Post-Work Validation**
```rust
// After completing work:
1. Run ALL tests again (must pass)
2. Test integration with other agent features
3. Update documentation with compatibility notes
```

### C. **Conflict Resolution**
1. **Immediate reporting** of any test failures
2. **Rollback first** if optimization breaks features
3. **Coordinate fix** between affected agents

## 🎯 AGENT RESPONSIBILITIES

### **Performance Agents (PERF-AUDIT, etc.)**
- ✅ Must preserve ALL existing functionality
- ✅ Must test with feature agent test suites
- ✅ Must document any API changes
- ❌ Cannot break working features for speed

### **Feature Agents (SEM, LEX, GEN, etc.)**
- ✅ Must test against latest performance optimizations
- ✅ Must adapt to any API changes from optimizations
- ✅ Must report optimization conflicts immediately
- ❌ Cannot assume stable APIs during optimization sprints

### **Father Zak (Oversight)**
- ✅ Enforces coordination protocols
- ✅ Resolves agent conflicts
- ✅ Approves integration checkpoints
- ✅ Manages agent scheduling to avoid conflicts

## 📊 COORDINATION WORKFLOW

```
[START SPRINT]
    │
    ├──► Agent A: Identify dependencies
    │     │
    │     └──► Check Agent B's work area
    │
    ├──► Agent B: Coordinate schedule
    │     │
    │     └──► Stagger if conflict detected
    │
    ├──► [WORK PHASE]
    │     │
    │     ├──► Hourly: Run integration tests
    │     │
    │     └──► Report conflicts immediately
    │
    └──► [COMPLETION]
          │
          ├──► Final integration testing
          │
          ├──► Cross-agent validation
          │
          └──► Father Zak approval
```

## 🚨 CONFLICT RESOLUTION PROCEDURE

### **Step 1: Immediate Detection**
```bash
# Agent detects conflict
git status
cargo test --workspace  # Tests fail
```

### **Step 2: Conflict Reporting**
```bash
# Report to Father Zak
echo "CONFLICT: Performance optimization broke static method parsing"
echo "Affected tests: tests/static_method_tests.rs"
echo "Agent A: PERF-AUDIT (optimized MIR generation)"
echo "Agent B: SEM (implementing static methods)"
```

### **Step 3: Resolution**
1. **Temporary rollback** of conflicting changes
2. **Coordinated fix** with both agents
3. **Integration testing** before re-application
4. **Protocol update** to prevent recurrence

## 📈 SUCCESS METRICS

### **Green Metrics (✅ GOOD)**
- All tests pass after agent work
- No regression in feature functionality  
- Performance improvements verified
- Cross-agent integration successful

### **Red Metrics (❌ BAD)**
- Any test failure after agent work
- Feature regression for optimization gain
- Lack of coordination between agents
- Unreported conflicts

## 🏭 FACTORY ENFORCEMENT

**Violations result in:**
1. **First offense**: Agent re-education on protocols
2. **Second offense**: Temporary agent suspension
3. **Third offense**: Agent termination and respawn

**Success rewards:**
1. **Coordinated work**: Faster approval and integration
2. **Conflict-free sprints**: Higher productivity
3. **Factory harmony**: All agents working together

---

## 🔄 PROTOCOL UPDATES

This protocol is living documentation. Agents should:
1. **Follow** the current protocol
2. **Suggest** improvements based on experience
3. **Update** when better coordination methods are found

**Last updated: 2026-03-30**  
**Updated by: Father Zak**  
**Reason: Critical coordination failure between performance and feature agents**