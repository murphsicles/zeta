# Advanced Traps — unsafe, macros, FFI, testing, performance

## Unsafe Code

- **`unsafe` doesn't disable borrow checker** — only allows 5 specific operations
- **Raw pointers `*const T` / `*mut T`** — can be null, dangling, or aliased
- **`unsafe impl` for Send/Sync** — you guarantee invariants compiler can't check
- **`transmute` is nuclear** — reinterprets bits, can cause UB easily
- **Undefined behavior is NEVER acceptable** — even if "it works on my machine"

## Macro Pitfalls

- **`macro_rules!` hygiene** — identifiers don't leak, but paths can be tricky
- **Macro expansion order** — can cause surprising errors
- **`$crate` for paths in macros** — ensures correct crate resolution
- **Proc macros need separate crate** — `proc-macro = true` in Cargo.toml
- **Debug macros with `cargo expand`** — see what code actually generates
- **`stringify!` and `concat!`** — compile-time string operations

## FFI Issues

- **`#[repr(C)]` for C-compatible layout** — Rust default layout is unspecified
- **Null-terminated strings** — `CString` / `CStr` not `String` / `&str`
- **`extern "C"` for C ABI** — Rust ABI is unstable
- **Ownership across FFI** — who frees what? Document clearly
- **Panics across FFI boundary** — undefined behavior, use `catch_unwind`
- **`Option<&T>` is nullable pointer** — FFI-safe optimization

## Testing Traps

- **`#[cfg(test)]` module not in release** — but dependencies still compile
- **`assert_eq!` shows both values** — better than `assert!(a == b)`
- **`#[should_panic]` for panic tests** — can specify `expected = "message"`
- **`Result<(), E>` return in tests** — use `?` in test functions
- **Integration tests in `tests/`** — separate compilation, external API only
- **`cargo test -- --nocapture`** — to see println! output

## Performance Traps

- **`.clone()` is not free** — deep copy for most types
- **String allocation on every `format!`** — reuse buffers with `write!`
- **`Vec` reallocation** — use `with_capacity()` if size known
- **Iterator vs loop** — usually same perf, but check with `cargo bench`
- **`Box<dyn Trait>` indirection** — generics are faster if possible
- **`#[inline]` across crates** — needed for cross-crate inlining
- **Debug vs Release** — 10-100x difference, always bench in release
