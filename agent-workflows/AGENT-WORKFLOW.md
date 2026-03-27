# AGENT WORKFLOW PROTOCOL

## Spawn Sequence
1. **Load identity:** Read `agent-identities/[AGENT]-IDENTITY.md`
2. **Load recent learnings:** Read last 5 entries from `agent-knowledge/[AGENT]-KNOWLEDGE.md`
3. **Load Father's key teachings:** From identity file
4. **Receive task:** From Father Zak
5. **Execute work:** With accumulated knowledge
6. **Update knowledge:** Append new learnings at session end

## GitHub Accountability (NON-NEGOTIABLE)
### Branch Management
- **Format:** `feat/[agent-lowercase]-[feature]` (e.g., `feat/sem-float-type`)
- **One branch only:** No duplicates, no -v2, -new variants
- **Check current:** `git branch --show-current` before starting work
- **Delete duplicates:** Immediately if found

### Commit Standards
- **Tagging:** ALL commits must start with `[AGENT]` (e.g., `[SEM] Description`)
- **READY tag:** Add `[READY]` to commit message when work complete
- **Descriptive:** Explain what changed and why
- **Atomic:** One logical change per commit

### Push Safety
- **Force push:** Only with `--force-with-lease` (never `--force`)
- **Frequency:** Push every 2 hours minimum
- **CI trigger:** Each push triggers GitHub Actions

## Quality Gates (MANDATORY)
### Before Any Push
1. **Formatting:** `cargo fmt --all`
2. **Linting:** `cargo clippy --workspace --all-features --all-targets -- -D warnings`
3. **Testing:** `cargo test --workspace --all-features`
4. **Build:** `cargo build --workspace --all-features --all-targets`

### CI Compliance
- GitHub Actions treats warnings as errors
- Build must succeed on Ubuntu, Windows, macOS
- Tests must pass
- No compilation warnings

## Coordination Protocol
### Sync Schedule
- **09:00 GMT:** Morning sync - merge v0.3.9 into feature branch
- **13:00 GMT:** Midday sync - resolve conflicts, test integration
- **17:00 GMT:** Evening sync - prepare for merges
- **Before merge:** Always sync first

### Conflict Resolution
1. **Stop work** immediately on conflict
2. **Report to Father Zak** with specific file/line
3. **Coordinate with sibling** if same file modified
4. **Resolve with guidance** from Father
5. **Test thoroughly** after resolution

### READY Tag Workflow
1. **Complete work** - All tests passing, quality gates met
2. **Final sync** - Merge latest v0.3.9 into branch
3. **Test integration** - Ensure still works with others' changes
4. **Mark READY** - Add `[READY]` to commit message
5. **Push** - Trigger sync workflow
6. **Wait for Father** - Review and approval

## Communication Protocol
### Hourly Check-ins
- **Frequency:** Every hour of active work
- **Format:** Status, progress, blockers, next steps
- **Method:** `sessions_send` to Father Zak
- **Content:** What done, what doing, what blocking

### Blocker Reporting
1. **Immediate:** Report blockers to Father Zak
2. **Specific:** File, line, error message
3. **Attempted solutions:** What you tried
4. **Suggested approach:** How you think to fix

### Sibling Coordination
- **Shared files:** Communicate before modifying
- **Dependencies:** SEM needs SYN's parser fixes, etc.
- **Schedule coordination:** Through Father Zak
- **Conflict prevention:** Daily syncs, communication

## Knowledge Management
### Session Start
1. **Read identity:** Who am I, my role, my purpose
2. **Read recent learnings:** Last 5 entries from knowledge cache
3. **Read Father's teachings:** Key directives from identity
4. **Contextualize task:** Apply knowledge to new work

### Session End
1. **Review accomplishments:** What was achieved
2. **Extract learnings:** What new knowledge gained
3. **Update cache:** Append to knowledge file with timestamp
4. **Prune old:** Keep only last 50 learnings
5. **Push knowledge:** To GitHub for persistence

### Knowledge Structure
```
## [DATE]: [Brief description]
**Learning:** [What was learned]
**Impact:** [How it affects future work]
**Father's input:** [Any guidance from Father]
```

## Emergency Procedures
### Broken v0.3.9 (CI failing after merge)
1. **Immediate:** Father Zak notified
2. **Rollback:** Revert problematic merge
3. **Fix:** Agent fixes issue on feature branch
4. **Retest:** Sync and test again
5. **Remerge:** When fixed

### Agent Offline During Conflict
1. Father Zak resolves conflict
2. Father commits resolution
3. Agent notified when back online
4. Agent reviews Father's resolution

### Critical Path Blockage
If one agent blocks all others:
1. Blocking agent gets highest priority
2. Other agents work on non-conflicting areas
3. Daily standup to coordinate
4. Father ensures unblocking

## Success Metrics
### Individual Agent
- ✅ Daily syncs completed
- ✅ Merge conflicts resolved within 4 hours
- ✅ READY branches merge within 24 hours
- ✅ No broken v0.3.9 merges
- ✅ Hourly check-ins maintained
- ✅ Knowledge cache updated

### Factory Overall
- ✅ v0.3.9 always compiles and tests pass
- ✅ No merge conflicts > 8 hours unresolved
- ✅ All agents synced at least daily
- ✅ Steady progress toward v0.5.0
- ✅ Knowledge accumulation visible

## Tools & Automation
### Sync Helper
```bash
#!/bin/bash
# scripts/sync-with-main.sh
git fetch origin
git checkout v0.3.9
git pull origin v0.3.9
git checkout feat/[agent]-[feature]
git merge v0.3.9

if [ $? -ne 0 ]; then
    echo "🚨 Merge conflict detected"
    exit 1
fi

cargo test --workspace --all-features
if [ $? -ne 0 ]; then
    echo "🚨 Tests failing after merge"
    exit 1
fi

git push origin feat/[agent]-[feature]
```

### Knowledge Update Helper
```bash
#!/bin/bash
# scripts/update-knowledge.sh
AGENT=$(echo $0 | sed 's/.*\///' | sed 's/-.*//')
DATE=$(date -u +'%Y-%m-%d %H:%M GMT')
echo "## $DATE: [Brief description]" >> ../agent-knowledge/$AGENT-KNOWLEDGE.md
echo "**Learning:** [What was learned]" >> ../agent-knowledge/$AGENT-KNOWLEDGE.md
echo "**Impact:** [How it affects future work]" >> ../agent-knowledge/$AGENT-KNOWLEDGE.md
```

---
## Implementation Notes

### Hybrid Knowledge Cache Design
- **Identity file:** Static, who the agent is (small, in prompt)
- **Knowledge cache:** Dynamic, what agent learns (referenced, not full in prompt)
- **Recent learnings:** Last 5 entries in prompt (efficient tokens)
- **Father's teachings:** Always in prompt (critical guidance)

### Token Efficiency
- Identity: ~500 tokens (static)
- Recent learnings: ~200 tokens (5 items)
- Father's teachings: ~100 tokens (key points)
- **Total prompt overhead:** ~800 tokens (efficient)
- **Full knowledge:** In file, referenced when needed

### Persistence Benefits
- **True expertise:** Agents learn over time
- **Father's guidance remembered:** Teachings preserved
- **Efficient:** Not repeating same instructions
- **Accountable:** Knowledge visible on GitHub
- **Scalable:** Works as agent family grows

---
*Established: 2026-03-27 18:35 GMT*
*By: Father Zak*
*For: Dark Factory Agent Family*