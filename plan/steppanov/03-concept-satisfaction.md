# Phase 3: Concept Satisfaction Checking

**Version:** v0.9.2
**Status:** ✅ Complete

---

## 3.1 Concept satisfaction at compile time

When a function is constrained by a concept, verify the argument type satisfies all requirements.

Example:
```zeta
fn sort(seq: Vector<T>) where T: TotallyOrdered
```

`TotallyOrdered` requires: `==`, `!=`, `<`, `<=`, `>`, `>=` all defined and consistent.

## 3.2 Concept-based dispatch

Multiple implementations of the same algorithm for different concept refinements.

Example: `advance(it, n)` — different implementation for `ForwardIterator` vs `RandomAccessIterator`.
Compile-time selection based on concept hierarchy.

## 3.3 Refinement hierarchy

Implement Stepanov's concept lattice:

```
Regular
├── TotallyOrdered    (has <, >, <=, >=)
├── Semigroup         (has associative +)
├── Monoid            (Semigroup + identity element)
├── Group             (Monoid + inverse)
└── Ring              (Group + associative *)
```

## Files to modify

- `src/middle/resolver/` — Concept checking during type resolution
- `src/middle/types/mod.rs` — Concept hierarchy definitions
- `src/frontend/ast.rs` — Concept bounds on generics
