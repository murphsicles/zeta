# Phase 6: Provable Correctness

**Version:** v0.9.5
**Status:** ✅ Complete

---

## 6.1 Precondition and postcondition assertions

- `pre(condition)` — assert at function entry
- `post(condition)` — assert at function exit
- Both are checked at runtime in debug mode, elided in release

## 6.2 Loop invariants

- `invariant(condition)` — asserted at loop entry/exit
- Helps verify algorithm correctness

## 6.3 Mathematical type properties

- `commutative(Op)` — true if `a op b == b op a`
- `associative(Op)` — true if `(a op b) op c == a op (b op c)`
- `identity_element(Op)` → e such that `a op e == a`

## Files to modify

- `src/frontend/parser/parser.rs` — Parse `pre`/`post`/`invariant`
- `src/middle/mir/gen.rs` — Lower to assertion MIR
- `src/backend/codegen/codegen.rs` — Assertion codegen
