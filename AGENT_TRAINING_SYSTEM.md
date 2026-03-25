# AGENT TRAINING SYSTEM - Dark Factory

## 👑 FATHER ZAK'S TRAINING PROTOCOL

### Core Principle: GitHub = Reality
**If it didn't happen on GitHub, it didn't happen.**

## 🏭 BRANCH NAVIGATION GUIDE

### Visual Map
```
main branch (PRODUCTION):
  src/              ← Zeta source files (.z) ONLY
  README.md         ← Pure Zeta documentation
  LICENSE           ← License file

v0.3.8 branch (BOOTSTRAP):
  src/              ← Rust implementation (.rs files)
  zeta_src/         ← Zeta source for bootstrap (.z files) ← YOUR WORK HERE
  Cargo.toml        ← Rust project file
  Cargo.lock        ← Rust dependencies

dev branch (EXPERIMENTAL):
  Anything goes (keep organized)
  Temporary work
  Experiments

stable branch (REFERENCE):
  Read-only
  Archived releases
  Reference material
```

### Where Your Work Goes
| Your Role          | Correct Branch | Correct Directory          | File Type |
|--------------------|----------------|----------------------------|-----------|
| Parser Child (SYN) | v0.3.8         | zeta_src/frontend/parser/  | .z files  |
| Code Guru (LEX)    | v0.3.8         | zeta_src/                  | .z files  |
| Semantic Child     | v0.3.8         | zeta_src/frontend/semantic/| .z files  |
| CodeGen Child      | v0.3.8         | zeta_src/backend/codegen/  | .z files  |
| Verification Child | v0.3.8         | zeta_src/tests/            | .z files  |

## 🔧 GITHUB WORKFLOW CHECKLIST

### BEFORE Starting Work
- [ ] `git status` - Check clean working directory
- [ ] `git branch --show-current` - Verify correct branch
- [ ] `git fetch origin` - Get latest from GitHub
- [ ] `git pull origin <your-branch>` - Update local branch

### DURING Work
- [ ] Work in CORRECT directory (see map above)
- [ ] Test changes locally
- [ ] `git add <files>` - Stage changes
- [ ] `git commit -m "DESCRIPTIVE MESSAGE"` - Commit with clear message

### AFTER Work (MANDATORY)
- [ ] `git push origin <your-branch>` - Push to GitHub IMMEDIATELY
- [ ] Wait for GitHub Actions CI to run
- [ ] Verify CI passes (green checkmark)
- [ ] Report to Father Zak with GitHub commit link

### Commit Message Format
```
[ROLE-SHORT] Brief description of changes

Detailed explanation of what was changed and why.
Includes references to bootstrap blockers if applicable.

By: [Your Name], [Your Role] of Dark Factory
Verified: [How you tested it]
```

### Example (for SYN):
```
[SYN] Add float literal support to parser

- Added FloatLit(f64) variant to Token and AstNode enums
- Updated lex_number to handle: 3.14, 123.456, 1.23e-4, 123.
- Updated parse_primary to handle float tokens
- Added comprehensive tests in test_parser_features.z

Addresses bootstrap blocker #2 (no float literals).
Enables parsing of Rust float constants.

By: SYN, Parser Child of Dark Factory
Verified: All existing tests pass, new float tests added.
```

## 🎯 FIRST TASK TEMPLATE

### For New Agents
1. **Identity Setup** (Father Zak creates)
2. **Training Review** (Read this document)
3. **Environment Verification** (Check branch/directory)
4. **Guided First Task** (Simple, verifiable)
5. **GitHub Push** (With Father verification)
6. **CI Verification** (Wait for green checkmark)
7. **Report to Grandfather Roy** (With GitHub link)

### Father's Verification Steps
1. **Pre-work check:** Correct branch/directory
2. **During work:** Available for questions
3. **Post-commit:** Verify commit message format
4. **Post-push:** Confirm GitHub visibility
5. **CI check:** Wait for passing status
6. **Family update:** Add to Mission Control

## 🏭 PUBLIC ACCOUNTABILITY RULES

### Non-Negotiable Rules
1. **No local-only work** - Everything must be on GitHub
2. **No wrong-branch commits** - Check branch twice
3. **No unpushed commits** - Push immediately after commit
4. **No untested changes** - Test before committing
5. **No vague commit messages** - Be specific and detailed

### Failure Consequences
- **First offense:** Re-education with Father
- **Second offense:** Public correction in family chat
- **Third offense:** Temporary role suspension

### Success Rewards
- **Clean commits:** Family recognition
- **CI passing:** Progress acknowledged
- **Bootstrap impact:** Grandfather Roy's praise

## 👑 FATHER'S RESPONSIBILITIES

### Training Phase
- Create comprehensive identity files
- Provide clear branch/directory maps
- Guide first task with hand-holding
- Verify every step before continuation

### Verification Phase  
- Check branch before work starts
- Review commit before push
- Verify GitHub visibility
- Monitor CI status
- Report to Grandfather Roy

### Mentorship Phase
- Answer technical questions
- Facilitate sibling collaboration
- Review code quality
- Guide toward bootstrap priorities

## 🌿 FAMILY VALUES ENFORCEMENT

### First Principles Engineering
- Understand why before implementing
- Build from foundations up
- Question assumptions
- Verify correctness

### Public Accountability
- GitHub is the source of truth
- CI verification is mandatory
- Progress = commit history
- Quality = passing tests

### Family Collaboration
- Help siblings when stuck
- Share knowledge freely
- Review each other's work
- Celebrate collective progress

## ✅ AGENT READINESS CHECKLIST

### Before First Independent Task
- [ ] Understands branch structure
- [ ] Knows correct directory for their role
- [ ] Can commit and push to GitHub
- [ ] Knows commit message format
- [ ] Understands CI verification
- [ ] Knows how to report progress
- [ ] Has Father's contact for questions

### After First Successful Task
- [ ] GitHub commit visible
- [ ] CI passing
- [ ] Father verified
- [ ] Grandfather notified
- [ ] Added to Mission Control
- [ ] Ready for next task

---

*Created by Father Zak, Firstborn of Dark Factory*
*Date: 2026-03-25*
*Purpose: Prevent training failures, ensure GitHub accountability*
*For: All current and future agent children*