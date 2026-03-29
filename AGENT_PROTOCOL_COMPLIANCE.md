# AGENT PROTOCOL COMPLIANCE GUIDE

**Version:** 1.0  
**Effective:** 2026-03-29  
**Enforced by:** Firstborn Zak & Quality-Enforcer System

## CRITICAL PROTOCOLS (NON-NEGOTIABLE)

### 1. Code Quality Gates (MUST DO BEFORE ANY PUSH)
```bash
# ALWAYS run these three commands before committing or pushing:
cargo fmt --all
cargo clippy --workspace --all-features --all-targets -- -D warnings
cargo check --workspace --all-features --all-targets
cargo test --workspace --all-features
```

**Violation Example (v0.3.11):**
- Pushed code without rustfmt -> 100+ formatting diffs
- Pushed code with clippy warnings -> CI failures
- Caused release delays -> Factory inefficiency

### 2. Local Testing Protocol
```bash
# ALWAYS test locally before pushing to GitHub:
cargo test --workspace --all-features
cargo build --release
./scripts/run_zeta_tests.ps1
```

**Rationale:** CI failures waste time and block other agents. Catch issues locally.

### 3. GitHub-First Workflow
- ALL work must be on GitHub - "If it's not on GitHub, it didn't happen"
- Commit frequently - Small, focused commits with clear messages
- Push to feature branches - Never push directly to main or dev without review
- Use PRs for integration - Pull requests enable code review and CI validation

## TECHNICAL ENFORCEMENT SYSTEMS

### Git Hooks (Automatic Validation)
The following hooks are installed in `.git/hooks/`:

1. **pre-commit** - Runs on `git commit`:
   - rustfmt check
   - clippy lint check
   - compilation check

2. **pre-push** - Runs on `git push`:
   - test suite execution
   - uncommitted changes warning

**To bypass (EMERGENCY ONLY):**
```bash
git commit --no-verify  # Skip pre-commit hook
git push --no-verify    # Skip pre-push hook
```

**Note:** Bypassing requires explicit justification in commit message.

### CI Configuration (.github/workflows/ci.yml)
CI treats warnings as errors:
```yaml
env:
  RUSTFLAGS: "-Dwarnings"  # ALL warnings are errors

jobs:
  check:
    steps:
      - name: Clippy
        run: cargo clippy --workspace --all-features --all-targets -- -D warnings
      - name: Fmt
        run: cargo fmt --all -- --check
```

**Result:** Any warning or formatting issue fails the CI build.

## AGENT WORKFLOW CHECKLIST

### Before Starting Work:
- [ ] Read this protocol document
- [ ] Check RELEASE_STATUS.md for current priorities
- [ ] Check TODO_TRACKING.md for assigned tasks
- [ ] Create feature branch: `git checkout -b feature/your-feature`

### During Development:
- [ ] Run `cargo fmt --all` after significant changes
- [ ] Run `cargo clippy --workspace --all-features --all-targets` frequently
- [ ] Run `cargo test --workspace --all-features` after each logical unit
- [ ] Commit with descriptive messages: `git commit -m "feat: add X to Y"`

### Before Push:
- [ ] `cargo fmt --all`
- [ ] `cargo clippy --workspace --all-features --all-targets -- -D warnings`
- [ ] `cargo check --workspace --all-features --all-targets`
- [ ] `cargo test --workspace --all-features`
- [ ] All tests pass locally
- [ ] Push: `git push origin feature/your-feature`

### After Push:
- [ ] Create Pull Request on GitHub
- [ ] Wait for CI to pass (green checkmark)
- [ ] Request review from Firstborn Zak
- [ ] Merge after approval

## PROTOCOL VIOLATION CONSEQUENCES

### Minor Violations:
- Single clippy warning missed
- Minor formatting issue
- **Action:** Automated fix by CI, warning issued

### Major Violations:
- Multiple warnings pushed
- Formatting issues causing CI failure
- Tests not run before push
- **Action:** CI fails, release blocked, agent review required

### Critical Violations:
- Bypassing all quality gates intentionally
- Pushing broken code that blocks other agents
- Repeated violations after warnings
- **Action:** Agent termination, task reassignment

## QUALITY METRICS & MONITORING

### Automated Checks:
- **Code Coverage:** Minimum 80% for new features
- **Warning Count:** Zero warnings in CI
- **Formatting:** 100% rustfmt compliance
- **Test Pass Rate:** 100% test suite passing

### Manual Reviews:
- **Firstborn Zak:** Final approval on all PRs
- **Architecture Review:** For major changes
- **Performance Review:** For optimization changes

### Reporting:
- Daily quality report in `memory/YYYY-MM-DD.md`
- Violation tracking in agent memory
- CI failure analysis and root cause documentation

## CONTINUOUS IMPROVEMENT

### Agent Training:
1. New agents must read this document
2. Shadow experienced agents for first task
3. Practice protocol on non-critical branches
4. Pass quality gate simulation before production work

### Protocol Updates:
- Protocols evolve based on lessons learned
- All agents notified of changes
- Training materials updated
- Historical violations inform improvements

### Feedback Loop:
- Agents report protocol pain points
- Suggestions for automation improvements
- Success stories shared across factory
- Violation patterns analyzed for systemic fixes

## SUPPORT & ESCALATION

### When Stuck:
1. Check AGENT_COORDINATION.md for coordination channels
2. Consult TOOLS.md for environment-specific notes
3. Review similar implementations in codebase
4. Ask Firstborn Zak for guidance

### Emergency Bypass:
Only in true emergencies:
1. Document reason for bypass in commit message
2. Notify Firstborn Zak immediately
3. Create follow-up PR to fix violations
4. Update protocols if gap identified

### Reporting Issues:
- Protocol gaps: Update this document
- Tool issues: Update TOOLS.md
- CI problems: Check .github/workflows/ci.yml
- Systemic issues: Document in memory/YYYY-MM-DD.md

## SUCCESS STORIES

### v0.3.11 Recovery:
**Violation:** Agents pushed code without clippy/rustfmt
**Response:** Firstborn Zak enforced protocols, fixed violations
**Result:** CI passed, v0.3.11 released successfully
**Lesson:** Protocols prevent CI failures and release delays

### Best Practice:
**Agent:** GENERICS-ARCHITECT
**Action:** Ran all quality gates before each commit
**Result:** Zero CI issues, feature delivered in 22 minutes
**Recognition:** Protocol compliance enabled rapid delivery

## REMEMBER

Quality gates aren't bureaucracy. They're the foundation that enables rapid, parallel development without chaos. Every minute spent on quality saves hours of CI debugging and release blocking.

**First Principles. Parallel Execution. Quality First.**

*"Protocols enable velocity, not hinder it. The factory moves fastest when every agent follows the same quality standards." - Zak, Firstborn*