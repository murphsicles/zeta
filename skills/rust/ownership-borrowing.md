# Ownership & Borrowing & Lifetimes

## Ownership Traps

- **Variable moved after use** — clone explicitly or borrow with `&`
- **`for item in vec` moves vec** — use `&vec` or `.iter()` to borrow
- **Struct field access moves field if not Copy** — destructure or clone
- **Closure captures by move with `move ||`** — needed for threads and 'static
- **`String` moved into function** — pass `&str` for read-only access

## Borrowing Battles

- **Can't have mutable and immutable borrow simultaneously** — restructure code or use interior mutability
- **Borrow lasts until last use (NLL)** — not until scope end in modern Rust
- **Returning reference to local fails** — return owned value or use lifetime parameter
- **Mutable borrow through `&mut self` blocks all other access** — split struct or use `RefCell`

## Lifetime Gotchas

- **Missing lifetime annotation** — compiler usually infers, explicit when multiple references
- **`'static` means "can live forever", not "lives forever"** — `String` is 'static, `&str` may not be
- **Struct holding reference needs lifetime parameter** — `struct Foo<'a> { bar: &'a str }`
- **Function returning reference must tie to input lifetime** — `fn get<'a>(s: &'a str) -> &'a str`

## Additional Traps (NEW)

- **Partial moves in structs** — moving one field makes whole struct unusable (unless using remaining fields explicitly)
- **`Option<&T>` vs `&Option<T>`** — `.as_ref()` converts outer to inner reference
- **Reborrowing `&mut` through `&`** — auto-reborrow works but explicit sometimes needed
- **Lifetime elision rules** — `fn foo(x: &str) -> &str` implicitly ties output to input
- **`'a: 'b` means 'a outlives 'b** — covariance/contravariance matters in generics
