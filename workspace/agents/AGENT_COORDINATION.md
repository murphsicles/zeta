# AGENT COORDINATION PROTOCOL

## Purpose
Ensure agents stay synced, aware of each other's work, and safely merge changes into v0.3.9.

## Core Principle
**No agent works in isolation.** All changes must integrate with v0.3.9 and other agents' work.

## Workflow Stages

### Stage 1: Feature Development (on agent branch)
```
1. Create branch: feat/[agent]-[feature]
2. Develop feature
3. Test locally
4. Push to GitHub (triggers CI)
5. Mark READY when complete
```

### Stage 2: Sync with v0.3.9
```
1. Fetch latest v0.3.9 changes
2. Merge v0.3.9 into feature branch
3. Resolve any conflicts
4. Run tests to ensure still works
5. Push updated feature branch
```

### Stage 3: Merge to v0.3.9
```
1. Agent marks branch READY ([READY] tag)
2. Sync workflow runs automatically
3. Father Zak reviews
4. Merge to v0.3.9 (triggers CI)
5. All agents fetch updated v0.3.9
```

## Detailed Protocols

### 1. Daily Sync Schedule
- **09:00 GMT**: Morning sync - all agents merge v0.3.9 into their branches
- **13:00 GMT**: Midday sync - resolve conflicts, test integration
- **17:00 GMT**: Evening sync - prepare for merges
- **Before any merge**: Always sync first

### 2. Conflict Resolution
```
When merge conflict occurs:
1. Agent stops new work
2. Agent fetches latest v0.3.9
3. Agent resolves conflicts locally
4. Agent tests resolution
5. Agent pushes resolved branch
6. Sync workflow re-runs
```

### 3. READY Tag System
Agents mark branches ready for merge by adding `[READY]` to commit message:
```
[SYN] Fix const parsing bug [READY]

- Move parse_const before parse_func
- Add comprehensive tests
- All tests pass after sync with v0.3.9
```

### 4. Merge Approval Process
1. **Agent**: Marks branch READY
2. **Sync workflow**: Runs automatically, reports status
3. **Father Zak**: Reviews sync report, tests results
4. **CI**: Must pass on synced branch
5. **Merge**: Father approves merge to v0.3.9

## GitHub Actions Integration

### Workflows:
1. **agent-sync.yml**: Runs on push to feat/ branches
   - Checks for [READY] tag
   - Attempts sync with v0.3.9
   - Reports conflicts or success
   - Notifies agent on conflict

2. **agent-branches.yml**: CI for individual branches
   - Runs tests on feature branches
   - Checks commit tagging
   - Provides quality gates

3. **agent-dashboard.yml**: Monitoring
   - Shows sync status
   - Tracks ready branches
   - Identifies blockers

## Agent Responsibilities

### Before Starting Work (Daily)
```bash
# 1. Fetch all changes
git fetch origin

# 2. Update local v0.3.9
git checkout v0.3.9
git pull origin v0.3.9

# 3. Update feature branch
git checkout feat/[agent]-[feature]
git merge v0.3.9

# 4. Resolve any conflicts
# 5. Test after merge
cargo test --workspace --all-features

# 6. Push updates
git push origin feat/[agent]-[feature]
```

### When Feature Complete
```bash
# 1. Final sync with v0.3.9
git checkout feat/[agent]-[feature]
git fetch origin
git merge origin/v0.3.9

# 2. Test thoroughly
cargo test --workspace --all-features
cargo clippy --workspace --all-features --all-targets -- -D warnings

# 3. Commit with READY tag
git commit --amend -m "[AGENT] Feature description [READY]"
git push origin feat/[agent]-[feature] --force-with-lease
```

### After Merge to v0.3.9
```bash
# 1. All agents update
git checkout v0.3.9
git pull origin v0.3.9

# 2. Update feature branches
git checkout feat/[agent]-[feature]
git merge v0.3.9

# 3. Continue development
```

## Conflict Resolution Guide

### Common Conflict Scenarios:

1. **Same file modified by multiple agents**
   - Communicate via Father Zak
   - Decide whose changes take priority
   - Manually merge compatible changes

2. **API changes in v0.3.9**
   - Update feature to use new API
   - Test thoroughly
   - Document changes needed

3. **Test file conflicts**
   - Merge test cases from both agents
   - Ensure all tests still pass
   - Update test expectations if needed

### Resolution Commands:
```bash
# See conflicts
git status

# Abort merge (if stuck)
git merge --abort

# Accept incoming changes (theirs)
git checkout --theirs path/to/file

# Accept your changes (ours)
git checkout --ours path/to/file

# Edit conflicted file manually
# Then mark resolved
git add path/to/file
git merge --continue
```

## Communication Protocol

### Agent ↔ Agent (via Father Zak)
```
Agent A: "Working on parser changes in file X"
Agent B: "Also modifying file X for type system"
Father Zak: "Coordinate. Agent A finishes by 14:00, then Agent B updates."
```

### Status Reporting
```
Hourly check-in includes:
- Current file modifications
- Planned changes to shared files
- Any anticipated conflicts
- Sync status with v0.3.9
```

### Blockers
```
When blocked by another agent's work:
1. Report to Father Zak immediately
2. Provide specific file/line conflicts
3. Suggest resolution approach
4. Father coordinates resolution
```

## Quality Gates

### Before Marking READY:
1. ✅ All tests pass on feature branch
2. ✅ Synced with latest v0.3.9 (no conflicts)
3. ✅ Clippy clean with `-D warnings`
4. ✅ Rustfmt applied
5. ✅ Documentation updated
6. ✅ Integration tested with other features

### After Merge to v0.3.9:
1. ✅ v0.3.9 CI passes
2. ✅ All agents can sync without conflict
3. ✅ Bootstrap tests still pass
4. ✅ No regression in existing functionality

## Emergency Procedures

### Broken v0.3.9 (CI failing after merge)
1. **Immediate**: Father Zak notified
2. **Rollback**: Revert problematic merge
3. **Fix**: Agent fixes issue on feature branch
4. **Retest**: Sync and test again
5. **Remerge**: When fixed

### Agent Offline During Conflict
1. Father Zak resolves conflict
2. Father commits resolution
3. Agent notified when back online
4. Agent reviews Father's resolution

### Critical Path Blockage
If SYN's const parsing blocks all other work:
1. SYN gets highest priority
2. Other agents work on non-conflicting areas
3. Daily standup to coordinate
4. Father ensures SYN unblocked

## Success Metrics

### Individual Agent:
- ✅ Daily syncs completed
- ✅ Merge conflicts resolved within 4 hours
- ✅ READY branches merge within 24 hours
- ✅ No broken v0.3.9 merges

### Factory Overall:
- ✅ v0.3.9 always compiles and tests pass
- ✅ No merge conflicts > 8 hours unresolved
- ✅ All agents synced at least daily
- ✅ Steady progress toward v0.5.0

## Tools & Automation

### Sync Helper Script
```bash
#!/bin/bash
# scripts/sync-with-main.sh

# Run this daily
git fetch origin
git checkout v0.3.9
git pull origin v0.3.9
git checkout feat/[agent]-[feature]
git merge v0.3.9

if [ $? -ne 0 ]; then
    echo "🚨 Merge conflict detected"
    echo "Please resolve conflicts and run:"
    echo "  git add . && git merge --continue"
    exit 1
fi

cargo test --workspace --all-features
if [ $? -ne 0 ]; then
    echo "🚨 Tests failing after merge"
    exit 1
fi

git push origin feat/[agent]-[feature]
echo "✅ Synced successfully"
```

### Conflict Detection
GitHub Actions will:
- Auto-detect merge conflicts
- Notify agent via workflow failure
- Create issue for tracking
- Update dashboard with conflict status

## Starting Now

### Immediate Actions:
1. **All agents**: Run initial sync with v0.3.9
2. **Father Zak**: Monitor first sync attempts
3. **Establish**: Daily sync schedule
4. **Implement**: READY tag system

### First 24 Hours:
1. All agents synced at least once
2. First [READY] branches identified
3. Initial merges to v0.3.9
4. CI monitoring established

---

**Coordination prevents integration nightmares. Sync early, sync often.**

**The Dark Factory works as a coordinated team, not isolated individuals.**

**This is the way.**