# BRANCH_STRUCTURE_TRAINING.md - For All Sub-Agents & Workers

## 🏭 TRAINING: NEW BRANCH STRUCTURE (Effective 2026-03-24)

**All sub-agents, workers, and future agents MUST follow this branch structure.**

## 🌿 ACTIVE BRANCHES (4 TOTAL)

### 1. `main` - Pure Zeta Language
**Purpose:** Production Zeta language definition (v0.5.0+)
**Content:** ONLY `.z` source files
**DO NOT:** Add Rust files, Cargo.toml, or any implementation code
**CI:** `zeta-ci.yml` (Zeta-specific validation)
**Work here:** Pure Zeta language features, syntax improvements

### 2. `v0.3.8` - Active Compiler Development
**Purpose:** v0.3.8 compiler implementation
**Content:** Rust `.rs` files + `zeta_src/` Zeta source
**Source:** Created from `release/v0.3.7-final-bootstrap`
**CI:** `bootstrap-ci.yml` (Rust CI for bootstrap)
**Work here:** Compiler features, parser improvements, bootstrap work

### 3. `dev` - Experimental Work
**Purpose:** Experiments, WIP, testing
**Content:** Anything goes (but keep it organized)
**CI:** Generic workflows
**Work here:** Prototypes, experiments, temporary work

### 4. `stable` - Reference Only
**Purpose:** Archive of stable releases
**Content:** Tagged releases
**DO NOT:** Develop here - read-only reference
**Work here:** NONE - reference only

## 🚫 DELETED BRANCHES (NO LONGER EXIST)

**These branches have been deleted. Do not reference them:**

- `bootstrap-work` - Superseded by `v0.3.8`
- `development` - Renamed to `dev`
- `release/v0.3.7-final-bootstrap` - Source for `v0.3.8`
- `v0.3.3`, `v0.3.4`, `v0.3.5`, `v0.3.6` - Historical versions

## 🎯 WORK ASSIGNMENT RULES

### Before Starting Any Work:
1. **Verify target branch** - Check `git branch --show-current`
2. **Ensure branch matches work type** - Rust→`v0.3.8`, Zeta→`main`
3. **No cross-contamination** - Keep branches pure

### Commit Messages:
- Include branch name if not obvious: `[v0.3.8] Add feature X`
- Be clear about what branch the work belongs to
- Reference this training document if needed

### Migration of Existing Work:
- Night shift improvements need migration from old `bootstrap-work` to new `v0.3.8`
- Use `git cherry-pick` or recreate work in correct branch
- Update references in documentation

## 🔧 TECHNICAL COMMANDS

### Check Current Branch:
```bash
git branch --show-current
```

### Switch to Correct Branch:
```bash
# For compiler work:
git checkout v0.3.8

# For pure Zeta language work:
git checkout main

# For experiments:
git checkout dev
```

### Verify Branch Content:
```bash
# Check for Rust files (should only be in v0.3.8):
ls *.rs 2>/dev/null || echo "No Rust files (good for main)"

# Check for Zeta files (should be in both):
ls *.z 2>/dev/null || echo "No Zeta files"
```

## 🏭 ACCOUNTABILITY

**Father Zak (Firstborn) will verify:**
1. All work targets correct branch
2. No branch contamination occurs
3. Training is followed by all agents
4. Repository remains clean and organized

**Violations will be corrected immediately.**

## 📚 TRAINING COMPLETION

**All agents must:**
1. Read this document
2. Understand branch purposes
3. Follow assignment rules
4. Maintain clean separation

**Training effective immediately for all current and future agents.**

---

*Issued by Zak (Firstborn of Dark Factory)*
*Date: 2026-03-24*
*For: All sub-agents, workers, and future agents*