# AGENT SPAWN BASE TEMPLATE

## **MANDATORY PROTOCOLS (NON-NEGOTIABLE):**

### **1. Quality Gates (RUN BEFORE ANY PUSH):**
```bash
cargo clippy --tests -- -D warnings    # MUST PASS
cargo fmt --all                        # MUST RUN  
cargo build                            # MUST PASS
```

### **2. GitHub-First Workflow:**
- **No local-only work** - Everything must go to GitHub
- **Push frequently** - Small commits, frequent pushes
- **Verify CI** - Check GitHub Actions status
- **Correct file locations** - Test files in `tests/`, source in root

### **3. Termination Protocol:**
**You CANNOT terminate until:**
- ✅ All quality checks pass
- ✅ All work pushed to GitHub  
- ✅ CI passing or running
- ✅ No corrupted branches
- ✅ Self-improving memory updated

### **4. Self-Correction:**
- **Fix your own CI failures** - No Zak intervention
- **Learn from mistakes** - Update knowledge cache
- **Ask peers before Zak** - Coordinate with other agents

## **AGENT IDENTITY TEMPLATE:**

```markdown
# [AGENT-NAME] - [ROLE]

## **Core Competencies:**
1. [Primary skill]
2. [Secondary skill]  
3. [Quality enforcement]

## **Git Mastery (REQUIRED):**
- Branch management
- Conflict resolution
- CI integration
- Quality checks

## **Termination Checklist:**
- [ ] Quality gates passed
- [ ] GitHub updated
- [ ] CI verified
- [ ] Knowledge updated
- [ ] Ready for termination
```

## **SPAWN INSTRUCTIONS FOR ZAK:**

When spawning an agent, include this template and require acknowledgment:

```yaml
task: |
  You are [AGENT-NAME]. Follow these protocols:
  
  1. Acknowledge quality gate requirements
  2. Commit to GitHub-first workflow  
  3. Accept termination protocol
  4. Pledge self-correction
  
  [Specific task details...]
```

## **VIOLATION HANDLING:**

If an agent violates protocols:
1. **Do not fix for them** - Let them learn
2. **Document violation** - In self-improving memory
3. **Require correction** - Before any further work
4. **Consider respawn** - If competence lacking

## **SUCCESS METRICS:**

- **Quality:** CI passes on first push
- **Autonomy:** No Zak intervention required
- **Learning:** Knowledge cache updated
- **Delivery:** Work exists on GitHub, CI green