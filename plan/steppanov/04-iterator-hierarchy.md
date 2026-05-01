# Phase 4: Iterator Hierarchy

**Version:** v0.9.3
**Status:** ✅ Complete

---

### 4.1 Iterator categories

- `InputIterator<T>` — can read, single-pass
- `OutputIterator<T>` — can write, single-pass
- `ForwardIterator<T>` — multi-pass, can save and restore position
- `BidirectionalIterator<T>` — Forward + can go backward (`--`)
- `RandomAccessIterator<T>` — Bidirectional + can jump in O(1) (`+= n`)

### 4.2 Iterator traits

- `value_type<It>` → `T`
- `difference_type<It>` → `i64`
- `iterator_category<It>` → one of the category tags

### 4.3 Iterator operations

- `source(it)` — read value at iterator (dereference)
- `sink(it, val)` — write value at iterator
- `successor(it)` — advance by one (`++`)
- `predecessor(it)` — go back by one (`--`), for Bidirectional
- `advance(it, n)` — advance by n steps (specialized by category)

### 4.4 Range concept

- `Range<R>` — has `begin(R)` and `end(R)` returning iterators
- All containers satisfy Range
- Algorithms operate on ranges, not containers

## Files to modify

- `src/frontend/ast.rs` — Iterator expressions
- `src/middle/mir/gen.rs` — Iterator operation lowering
- `src/backend/codegen/codegen.rs` — Iterator codegen
- `src/middle/types/mod.rs` — Iterator category types
