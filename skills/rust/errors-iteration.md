# Error Handling & Pattern Matching & Iterators

## Error Handling

- **`unwrap()` panics on None/Err** — use `?` operator or `match` in production
- **`?` requires function returns Result/Option** — can't use in main without `-> Result<()>`
- **Converting errors: `map_err()` or `From` trait implementation**
- **`expect("msg")` better than `unwrap()`** — shows context on panic
- **`Option` and `Result` don't mix** — use `.ok()` or `.ok_or()` to convert

## Pattern Matching

- **Match must be exhaustive** — use `_` wildcard for remaining cases
- **`if let` for single pattern** — avoids verbose match for one case
- **Guard conditions: `match x { n if n > 0 => ... }`** — guards don't create bindings
- **`@` bindings: `Some(val @ 1..=5)`** — binds matched value to name
- **`ref` keyword in patterns to borrow** — often unnecessary with match ergonomics

## Iterator Gotchas

- **`.iter()` borrows, `.into_iter()` moves, `.iter_mut()` borrows mutably**
- **`.collect()` needs type annotation** — `collect::<Vec<_>>()` or let binding with type
- **Iterators are lazy** — nothing happens until consumed
- **`.map()` returns iterator, not collection** — chain with `.collect()`
- **Modifying while iterating impossible** — collect indices first, then modify

## Additional Traps (NEW)

### Error Handling
- **`anyhow` vs `thiserror`** — anyhow for apps, thiserror for libraries
- **`?` in closures** — closure must also return Result/Option
- **`panic!` unwinds by default** — `panic = "abort"` in Cargo.toml for smaller binary

### Iterators
- **`.filter_map()` combines filter and map** — cleaner than chaining
- **`.flatten()` for nested iterators** — `Option` and `Result` are iterators too
- **`.cloned()` vs `.copied()`** — copied for Copy types, cloned calls clone
- **`Iterator` vs `IntoIterator`** — for loops call `into_iter()` automatically
- **`.enumerate()` gives `(index, value)`** — index is usize
