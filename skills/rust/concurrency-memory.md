# Concurrency & Memory Patterns

## Concurrency

- **Data shared between threads needs `Send` and `Sync`** — most types are, `Rc` is not
- **Use `Arc` for shared ownership across threads** — `Rc` is single-threaded only
- **`Mutex<T>` for mutable shared state** — lock returns guard, auto-unlocks on drop
- **`RwLock` allows multiple readers or one writer** — deadlock if reader tries to write
- **Async functions return `Future`** — must be awaited or spawned

## Memory Patterns

- **`Box<T>` for heap allocation** — also needed for recursive types
- **`Rc<T>` for shared ownership (single-thread)** — `Arc<T>` for multi-thread
- **`RefCell<T>` for interior mutability** — runtime borrow checking, panics on violation
- **`Cell<T>` for Copy types interior mutability** — no borrow, just get/set
- **Avoid `Rc<RefCell<T>>` spaghetti** — rethink ownership structure

## Async Traps (NEW)

- **`.await` only in async context** — can't call from sync code directly
- **Async traits need `async-trait` crate** — or `-> impl Future` (nightly/2024+)
- **`Mutex` guard across `.await`** — use `tokio::sync::Mutex` not `std::sync::Mutex`
- **`spawn` requires `'static`** — move data in or use `Arc`
- **Executor required** — `tokio`, `async-std`, or `smol` to actually run futures
- **`select!` cancellation** — dropped future may not run cleanup

## Additional Memory Traps (NEW)

- **`Weak<T>` for breaking cycles** — `Rc`/`Arc` cycles leak memory
- **`Pin<T>` for self-referential** — async futures are often pinned
- **`MaybeUninit` for uninitialized** — safe wrapper for unsafe init patterns
- **`std::mem::drop` vs `Drop` trait** — `drop(x)` just calls `x.drop()` early
- **`ManuallyDrop` skips destructor** — useful for FFI or unions
- **Stack overflow with deep recursion** — Box recursion or increase stack
