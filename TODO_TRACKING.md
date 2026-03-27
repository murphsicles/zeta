# TODO TRACKING SYSTEM

## Purpose
Track all TODOs, FIXMEs, placeholders, stubs, and incomplete implementations to ensure they are addressed.

## Rules
1. **No placeholder logic** unless replaced in same commit
2. **All TODOs must be tracked** in this file
3. **Each TODO must have**:
   - Owner (agent responsible)
   - Priority (P0-P3)
   - Due date
   - Status
4. **Placeholder logic is forbidden** - Use proper implementation or fail fast

## TODO Categories

### P0 - Critical (Blocking)
- Placeholder logic that breaks functionality
- Missing core features
- Security issues
- Build/CI failures

### P1 - High Priority
- Missing important features
- Performance issues
- User-facing bugs
- Integration blockers

### P2 - Medium Priority
- Nice-to-have features
- Code quality improvements
- Documentation gaps
- Test coverage

### P3 - Low Priority
- Refactoring opportunities
- Optimization potential
- Future enhancements
- Cosmetic issues

## Tracking Format
```
### [PRIORITY] [FILE]:[LINE] - Brief description
- **ID:** TODO-YYYYMMDD-NNN (auto-generated)
- **Owner:** [AGENT] (assigned agent)
- **Created:** YYYY-MM-DD
- **Due:** YYYY-MM-DD
- **Status:** open | assigned | in-progress | blocked | done
- **Description:** What needs to be done
- **Placeholder:** What placeholder exists (if any)
- **Impact:** What breaks if not fixed
- **Notes:** Additional context
```

## Current TODOs

### P1 src/middle/mir/gen.rs:629 - Implement proper field access
- **ID:** TODO-20260327-001
- **Owner:** GEN (Generation Master)
- **Created:** 2026-03-27
- **Due:** 2026-03-28
- **Status:** open
- **Description:** Field access (`x.field`) currently returns placeholder `Lit(0)`
- **Placeholder:** `MirExpr::Lit(0)` at line 562
- **Impact:** Struct field access doesn't work, breaks struct functionality
- **Notes:** Part of struct pattern support added in commit 6d8d2e7

### P1 src/middle/mir/gen.rs:641 - Implement proper struct literal creation
- **ID:** TODO-20260327-002
- **Owner:** GEN (Generation Master)
- **Created:** 2026-03-27
- **Due:** 2026-03-28
- **Status:** open
- **Description:** Struct literals (`Point { x: 1, y: 2 }`) return placeholder `Lit(0)`
- **Placeholder:** `MirExpr::Lit(0)` logic for struct literals
- **Impact:** Struct literals don't work, breaks struct functionality
- **Notes:** Part of struct pattern support added in commit 6d8d2e7

### P1 src/middle/mir/gen.rs:557 - Field pattern binding placeholder
- **ID:** TODO-20260327-003
- **Owner:** GEN (Generation Master)
- **Created:** 2026-03-27
- **Due:** 2026-03-28
- **Status:** open
- **Description:** Field pattern binding uses placeholder ID
- **Placeholder:** `self.exprs.insert(field_id, MirExpr::Lit(0));`
- **Impact:** Struct pattern matching doesn't work properly
- **Notes:** Comment says "Create a placeholder ID for the field value"

### P2 src/frontend/parser/parser.rs:67 - Re-add logical operators
- **ID:** TODO-20260327-004
- **Owner:** SYN (Syntax Master)
- **Created:** 2026-03-27
- **Due:** 2026-03-29
- **Status:** open
- **Description:** Logical operators (`&&`, `||`) commented out, need re-implementation
- **Placeholder:** Operators commented out in parser
- **Impact:** Missing logical operator support
- **Notes:** Comment says "TODO: re-add these when we implement logical operators"

### P1 src/middle/resolver/new_resolver.rs:156 - Parse type string to Type
- **ID:** TODO-20260327-005
- **Owner:** SEM (Semantic Master)
- **Created:** 2026-03-27
- **Due:** 2026-03-28
- **Status:** open
- **Description:** Type annotation parsing from string to Type enum
- **Placeholder:** Missing type string parsing implementation
- **Impact:** Type annotations in let statements don't work properly
- **Notes:** Critical for `let x: f64 = 3.14` type checking

### P2 src/middle/mir/gen.rs:359-368 - Temporary assignment patterns
- **ID:** TODO-20260327-006
- **Owner:** GEN (Generation Master)
- **Created:** 2026-03-27
- **Due:** 2026-03-29
- **Status:** open
- **Description:** Multiple temporary/placeholder patterns in MIR generation
- **Placeholder:** `temp_id` patterns, `MirExpr::Var(temp_id)` self-references
- **Impact:** Potential correctness issues in generated code
- **Notes:** Found 6 placeholder patterns in temp assignment logic

### P3 tests/smoke_test.z:10 - Replace placeholder comment with proper logic
- **ID:** TODO-20260327-007
- **Owner:** LEX (Test Master)
- **Created:** 2026-03-27
- **Due:** 2026-03-29
- **Status:** open
- **Description:** Smoke test has placeholder comment "// Return 0 for success"
- **Placeholder:** Comment indicates placeholder logic
- **Impact:** Test validation may fail due to placeholder detection
- **Notes:** Found during commit validation, should either remove comment or implement proper test logic

## TODO Discovery Process

### Daily Scan
```bash
# Find all TODOs, FIXMEs, etc.
grep -r "TODO\|FIXME\|XXX\|HACK\|placeholder\|stub" --include="*.rs" --include="*.z" src/ tests/
```

### Commit Validation
Before any commit:
1. Check for new TODOs added
2. Add them to tracking system
3. Ensure no placeholder logic without tracking

### Agent Assignment
- **GEN:** MIR/codegen TODOs (field access, struct literals)
- **SYN:** Parser TODOs
- **SEM:** Type system TODOs
- **LEX:** Test-related TODOs
- **VER:** Verification TODOs

## Resolution Workflow

### 1. Discovery
- Agent finds TODO during work
- Agent adds to tracking system
- Agent assigns to appropriate owner

### 2. Assignment
- Father Zak reviews and prioritizes
- Owner accepts assignment
- Due date set (typically 24-48 hours)

### 3. Implementation
- Owner implements proper solution
- Owner removes placeholder logic
- Owner updates TODO status

### 4. Verification
- Tests added for new functionality
- CI passes
- TODO marked done
- Placeholder removed from code

## Prohibited Patterns

### ❌ FORBIDDEN
```rust
// TODO: Implement this properly
return 0; // Placeholder

// FIXME: This is a hack
let result = unsafe { mem::transmute(x) }; // Dangerous placeholder

// Placeholder logic
if condition {
    // TODO: Handle this case
    return default_value; // Silent failure
}
```

### ✅ REQUIRED
```rust
// Proper implementation or panic
if !condition {
    panic!("Field access not yet implemented"); // Fail fast
}

// Or: Tracked TODO with owner
// TODO-GEN-001: Implement field access (assigned to GEN, due 2026-03-28)
unimplemented!("Field access - see TODO-GEN-001");
```

## Agent Responsibilities

### When Adding Code
1. **No placeholder logic** - Implement properly or fail fast
2. **If MUST add placeholder**:
   - Add TODO comment with ID
   - Add to tracking system immediately
   - Assign owner and due date
   - Document impact

### When Finding TODOs
1. Add to tracking system
2. Notify Father Zak
3. Get assignment if appropriate
4. Schedule for resolution

### Daily Check
1. Review assigned TODOs
2. Update status
3. Report progress to Father Zak
4. Request help if blocked

## Father Zak Oversight

### Daily Review
1. Check TODO_TRACKING.md
2. Ensure all TODOs have owners
3. Adjust priorities as needed
4. Unblock agents if stuck

### Quality Gate
1. No commits with untracked TODOs
2. No placeholder logic without tracking
3. All TODOs have reasonable due dates
4. Progress on P0/P1 TODOs daily

## Integration with GitHub

### Issues
Each TODO should have corresponding GitHub issue:
- Issue title: `[TODO-ID] Brief description`
- Labels: `todo`, `priority-PX`, `owner-AGENT`
- Milestone: Due date
- Linked to commit that introduced it

### Pull Requests
When fixing TODO:
- PR title: `[TODO-ID] Fix: Brief description`
- PR must remove TODO comment
- PR must update tracking system
- PR must have tests

## Metrics

### TODO Health
- **Open TODOs:** Total count
- **Aged TODOs:** > 7 days old
- **P0/P1 backlog:** Critical items
- **Resolution rate:** TODOs closed/week

### Agent Performance
- **TODOs assigned:** Per agent
- **TODOs resolved:** Per agent
- **Average resolution time:** Per agent
- **Blocked TODOs:** Need attention

---
## TODO Discovery Script
```bash
#!/bin/bash
# scripts/find-todos.sh

echo "# TODO Scan - $(date)"

echo "## High Priority (TODO/FIXME)"
grep -r "TODO\|FIXME" --include="*.rs" --include="*.z" src/ tests/ | \
  sed 's/^/### /' | \
  sed 's/\(.*\):\([0-9]*\):.*\(TODO\|FIXME\).*/\1:\2 - \3/'

echo "## Placeholder Logic"
grep -r "placeholder\|stub\|hack\|XXX" --include="*.rs" --include="*.z" src/ tests/ -i | \
  sed 's/^/### /' | \
  sed 's/\(.*\):\([0-9]*\):.*/\1:\2 - Placeholder/'

echo "## Magic Numbers/Values"
grep -r "0\|1\|2\|true\|false\|null" --include="*.rs" --include="*.z" src/ tests/ | \
  grep -v "test" | \
  grep -v "0\.0" | \
  head -20 | \
  sed 's/^/### /' | \
  sed 's/\(.*\):\([0-9]*\):.*/\1:\2 - Magic value/'
```

---
*Established: 2026-03-27 19:36 GMT*
*By: Father Zak*
*In response to: Placeholder logic concern in commit 6d8d2e7*