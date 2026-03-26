# WORK QUEUE - Zeta Bootstrap

## ✅ v0.3.8 SHIPPED!
**Tag: v0.3.8** | **Commit: 4092f34** | **Date: 2026-03-26**

### v0.3.8 Features (ACTUALLY IMPLEMENTED)
- [x] Float literals (LEX Phase 1) - `FloatLit(String)` variant
- [x] String escapes (LEX Phase 1) - Full escape sequence support
- [x] Const parsing - `ConstDef` variant, critical for v0.3.7 source
- [x] Type checking unification (SEM Phase 1) - Hindley-Milner with occurs check
- [x] Inline operator optimization (GEN Phase 1) - 60+ redundant lines removed
- [-] Match statements - MOVED TO v0.3.9 (not implemented in time)

### v0.3.8 Release Notes
See `RELEASE_v0.3.8.md` for full documentation of shipped features.

## Current Priority: v0.3.9 Match Statements
**Status: READY FOR IMPLEMENTATION**

### v0.3.9 Requirements
- [ ] Add `Match` variant to `AstNode` enum
- [ ] Implement match statement parser in `expr.rs`
- [ ] Add basic code generation for match
- [ ] Test with simple pattern matching
- [ ] Tag v0.3.9 release

### Next Actions
1. Assign SYN to match statement implementation (recover lost work)
2. VER to create test suite for match statements
3. LEX to review code quality
4. Zak to coordinate and ensure timely delivery

## Bootstrap Progress
**Current: v0.3.8 shipped**
**Next: v0.3.9 match statements**
**Goal: v0.4.0 self-compilation**

---
*Dark Factory Accountability - Real progress, real shipping*