# Agent GitHub Protocol

## 🚨 CRITICAL PROTOCOL RULES

All agents interacting with this repository MUST follow these rules. Violations will trigger automatic cleanup and protocol enforcement.

## 1. Repository Structure Protocol

### Root Directory Rules
- **NO TEST FILES** in root directory (`.z`, `.zeta`, `.rs` test files)
- **NO DEBUG FILES** in root directory (`debug_*` files)
- **NO BUILD ARTIFACTS** in root directory (`.exe`, `.exe.o`, `.pdb`, `.rlib`)
- **NO TEMPORARY FILES** in root directory (temporary test outputs, logs)

### Proper Locations
- **Test files** → `tests/` with appropriate subdirectory:
  - `tests/unit/` - Basic unit tests
  - `tests/integration/` - Integration tests  
  - `tests/performance/` - Performance/benchmark tests
  - `tests/competition/` - Competition-specific tests
  - `tests/debug/` - Debug/temporary test files
- **Build artifacts** → `target/` directory only
- **Scripts** → `scripts/` directory
- **Logs** → `logs/` directory
- **Documentation** → `docs/` or root (README.md, CHANGELOG.md only)

## 2. Release Protocol

### Release Creation Requirements
1. **Tag MUST match version** (e.g., `v0.3.54`)
2. **Release notes MUST be added to CHANGELOG.md** before tagging
3. **Release notes MUST follow format**:
   ```
   ## [vX.Y.Z] - YYYY-MM-DD - "Release Name"
   
   ### Added
   - Feature 1
   - Feature 2
   
   ### Changed
   - Change 1
   
   ### Fixed
   - Bug fix 1
   
   ### Security
   - Security improvement 1
   ```

### Release Frequency
- **Major releases** (vX.0.0): Significant architectural changes
- **Minor releases** (v0.X.0): New features
- **Patch releases** (v0.0.X): Bug fixes only

## 3. Test Organization Protocol

### Test File Naming
- **Unit tests**: `test_<feature>.z` or `test_<feature>.rs`
- **Integration tests**: `integration_<feature>.z` or include "integration" in name
- **Performance tests**: Include "benchmark", "sieve", "performance" in name
- **Competition tests**: Include "competition", "murphy" in name
- **Debug tests**: `debug_<issue>.z`

### Test Categorization
- **Simple tests** → `tests/unit/`
- **Multi-component tests** → `tests/integration/`
- **Speed/benchmark tests** → `tests/performance/`
- **Competition-specific** → `tests/competition/`
- **Temporary/debug** → `tests/debug/`

## 4. Professional Appearance Protocol

### README.md Standards
- **NO PRIVATE DATA**: No personal emails, passwords, tokens
- **Professional formatting**: Proper Markdown, clear structure
- **Updated badges**: Current version, build status, etc.
- **Clear documentation**: Installation, usage, examples

### Directory Structure
- **Clean root**: Only essential files (README, LICENSE, Cargo.toml, etc.)
- **Organized subdirectories**: Clear separation of concerns
- **No clutter**: Remove temporary/duplicate files

## 5. Agent Workflow Protocol

### Before Committing
1. Run validation script: `powershell -File scripts/validate_repository_structure.ps1`
2. Fix any violations reported
3. Ensure tests are in correct directories
4. Verify no build artifacts in root

### Creating Releases
1. Update CHANGELOG.md with release notes
2. Commit CHANGELOG.md changes
3. Create git tag: `git tag vX.Y.Z`
4. Push tag: `git push origin vX.Y.Z`
5. Verify GitHub release was created automatically

### Adding Tests
1. Place in correct `tests/` subdirectory based on test type
2. Use appropriate naming convention
3. Update test documentation if needed
4. Run test to ensure it works

## 6. Validation System

### Automatic Validation
Run periodically or before commits:
```powershell
powershell -File scripts/validate_repository_structure.ps1
```

### Validation Checks
1. Root directory contamination check
2. Test file organization check  
3. Release documentation check
4. README security check
5. Build artifact location check

### Violation Consequences
1. **Warning**: First violation - notification only
2. **Auto-cleanup**: Second violation - automatic file reorganization
3. **Protocol enforcement**: Third violation - blocked commits until fixed

## 7. Prevention Scripts

### Required Scripts
1. `scripts/validate_repository_structure.ps1` - Main validation
2. `scripts/pre_commit_hook.ps1` - Git pre-commit hook
3. `scripts/release_check.ps1` - Release validation
4. `scripts/cleanup_protocol_violations.ps1` - Automatic cleanup

### Script Maintenance
- Keep scripts updated with protocol changes
- Test scripts after repository changes
- Document script usage and outputs

## 8. Emergency Protocol

### If Protocol is Violated
1. **STOP ALL COMMITS** immediately
2. Run cleanup script: `scripts/cleanup_protocol_violations.ps1`
3. Document violation in `GITHUB_PROTOCOL_VIOLATION_REPORT.md`
4. Fix root cause of violation
5. Resume normal operations after validation passes

### Reporting Violations
Add to `GITHUB_PROTOCOL_VIOLATION_REPORT.md`:
- Date and time
- Agent responsible (if known)
- Violation type
- Files affected
- Fix applied
- Prevention measures added

---
*Protocol established by GITHUB-CLEANUP-AGENT on 2026-04-07*  
*All agents MUST comply with this protocol*