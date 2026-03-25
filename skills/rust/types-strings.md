# Strings & Type System

## String Confusion

- **`String` is owned, `&str` is borrowed slice** — convert with `.as_str()` or `String::from()`
- **Indexing `s[0]` fails** — UTF-8 variable width, use `.chars().nth(0)` or `.bytes()`
- **Concatenation: `s1 + &s2` moves s1** — use `format!("{}{}", s1, s2)` to keep both
- **`.len()` returns bytes, not characters** — use `.chars().count()` for char count

## Type System Traps

- **Orphan rule: can't impl external trait on external type** — newtype pattern workaround
- **Trait objects `dyn Trait` have runtime cost** — generics monomorphize for performance
- **`Box<dyn Trait>` for heap-allocated trait object** — `&dyn Trait` for borrowed
- **Associated types vs generics** — use associated when one impl per type
- **`Self` vs `self`** — type vs value: `Self::new()` vs `&self`

## Additional String Traps (NEW)

- **`&String` auto-derefs to `&str`** — but prefer `&str` in function params
- **`str::from_utf8` can fail** — use `String::from_utf8_lossy` if uncertain
- **`char` is 4 bytes (Unicode scalar)** — not 1 byte like C
- **`.split()` returns iterator** — `collect()` to get `Vec<&str>`
- **`OsString` for paths** — not all paths are valid UTF-8

## Type System Advanced (NEW)

- **`impl Trait` vs `dyn Trait`** — static dispatch vs dynamic, different use cases
- **`Sized` bound implicit** — `?Sized` to accept unsized types
- **Coherence rules** — only one impl per type, foundational trait first
- **`PhantomData<T>`** — for unused type parameters (e.g., lifetime markers)
- **`Deref` coercion** — `&String` to `&str` automatic, but can be confusing
