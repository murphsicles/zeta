## Phase 1: Extend v0.3.7 to parse generic types (`lt(Result, i64)`)

### Current Status
v0.3.7 fails to parse generic types. The parser stops at `lt(` and cannot continue.

### Failure Demonstration
File: `src/extension_goal.z`
```
fn with_generic() -> lt(Result, i64) {
    return 0;
}
```
**Result:** 0 ASTs parsed, compilation fails

### Impact
- 4665 characters of `src/main.z` remain unparsed
- Bootstrap compilation incomplete
- Cannot compile actual Zeta compiler source

### Goal
Extend v0.3.7 parser to recognize `lt(` as start of generic type and parse `lt(Result, i64)` successfully.

### Success Criteria
1. `src/extension_goal.z` compiles with exit code 0
2. v0.3.7 parses more of `src/main.z` (reduce unparsed characters)
3. CI passes generic type tests

### Steps
- [ ] Analyze parser failure point
- [ ] Create comprehensive test suite
- [ ] Extend lexer to recognize `lt` in type context
- [ ] Extend parser to parse generic arguments
- [ ] Test incrementally with CI verification
- [ ] Verify against bootstrap source

### Verification
- All tests must pass CI
- Exit code 0 required for success
- Public GitHub commits for all changes

### Timeline
- Start: 2026-03-23
- Target completion: 2026-03-30

### Public Accountability
This issue will be updated with progress. All work is public on GitHub with CI verification.