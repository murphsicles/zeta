# Phase 5: Algorithmic Refinement

**Version:** v0.9.4
**Status:** ✅ Complete

---

## 5.1 Concept-constrained algorithms

Implement Stepanov's fundamental algorithms with concept constraints:

```zeta
fn find<I: InputIterator>(first: I, last: I, val: value_type<I>) -> I
fn find_if<I: InputIterator>(first: I, last: I, pred: UnaryFunction<bool, value_type<I>>) -> I
fn count<I: InputIterator>(first: I, last: I, val: value_type<I>) -> difference_type<I>
fn copy<I: InputIterator, O: OutputIterator>(first: I, last: I, out: O) -> O
fn swap_ranges<I: ForwardIterator, O: ForwardIterator>(first: I, last: I, out: O) -> O
```

## 5.2 Refinement-based optimization

- `advance(it, n)` — default: loop n times (for InputIterator)
- `advance(it, n)` — optimized: `it += n` (for RandomAccessIterator)
- Compile-time dispatch based on `iterator_category<It>`

## 5.3 Structured results

Algorithms that return multiple values use tuple/pair:
- `find` returns `(iterator, found_flag)`
- `minmax` returns `(min, max)`

## Files to modify

- Standard library: `zeta_src/` directory
- `src/middle/resolver/` — Concept-based dispatch
