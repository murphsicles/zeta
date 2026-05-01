# Phase 1: Foundations — Regular Types & swap

**Version:** v0.9.0
**Status:** ✅ Complete

---

### 1.1 `Regular(T)` concept derivation

Automatically derive whether a type satisfies the Regular concept:
- Default-constructible
- Copy-constructible (has `clone` or copy semantics)
- Destructible
- Equality-comparable (`==` / `!=`)
- Total ordering (`<`, `>`, `<=`, `>=` when meaningful)

Add `regular` built-in concept to the type system.
Compile-time check: `static_assert(regular(T))`

### 1.2 Built-in `swap` operation

- `swap(a, b)` as a language primitive (not a library function)
- For stack values: bitwise swap via LLVM `memcpy` or XOR swap
- For heap values: pointer swap
- Implementation: add `__builtin_swap` intrinsic in codegen

### 1.3 Value semantics guarantee

- Clarify that `let b = a` copies the value (independent object)
- Add `move(a)` for explicit transfer of ownership
- Add `copy(a)` for explicit deep copy

## Files to modify

- `src/middle/types/mod.rs` — Regular concept derivation
- `src/middle/mir/gen.rs` — swap lowering
- `src/backend/codegen/codegen.rs` — `__builtin_swap` codegen
