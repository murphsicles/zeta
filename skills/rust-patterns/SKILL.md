---
name: rust-patterns
description: Production Rust patterns covering ownership, async Tokio, Axum web framework, SQLx, error handling, CLI tools, WASM, and PyO3 Python bindings
version: 1.0.0
tags:
  - rust
  - systems-programming
  - tokio
  - axum
  - midos
---

# Rust Systems Programming Patterns

## Description

A comprehensive guide to production Rust patterns for 2026. Covers ownership and borrowing mental models, error handling with thiserror/anyhow, async concurrency with Tokio, web services with Axum, database access (SQLx, Diesel, SeaORM), CLI development, WebAssembly, and Python bindings via PyO3. Targets Rust Edition 2024 (1.85.0+).

## Usage

Install this skill to get production-ready Rust patterns including:
- Ownership, borrowing, and lifetime rules with concrete mental models
- thiserror vs anyhow selection guide for libraries vs applications
- Tokio runtime patterns: spawn, select!, streams
- Axum web server: routing, extractors, middleware, shared state
- Concurrency primitives: Arc, Mutex, RwLock, channels, Rayon

When working on Rust projects, this skill provides context for:
- Deciding when to clone vs borrow vs move
- Structuring async code to avoid blocking the event loop
- Choosing between database libraries based on project needs
- Building CLI tools with Clap v4 derive API
- Creating Python extension modules with PyO3

## Key Patterns

### Ownership Mental Model

Think of ownership like a title deed:
- Move (`let y = x`): You transfer the deed. `x` is no longer valid.
- Immutable borrow (`&T`): Multiple readers simultaneously, no modifications.
- Mutable borrow (`&mut T`): One exclusive borrower who can modify. No others inside.

```rust
// Quick pattern reference
fn risky() -> Result<T, E> {
    let x = operation()?;  // ? operator: early return on error
    Ok(x)
}

// Shared mutable state across threads
let data = Arc::new(Mutex::new(vec![]));
let clone = Arc::clone(&data);
tokio::spawn(async move { clone.lock().unwrap().push(1); });
```

### Error Handling: thiserror vs anyhow

Use `thiserror` for libraries (callers need matchable error variants):

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum MyError {
    #[error("file not found: {0}")]
    FileNotFound(String),
    #[error("io error: {0}")]
    IoError(#[from] io::Error),  // auto-converts from io::Error
}
```

Use `anyhow` for applications (add context to error chains):

```rust
use anyhow::{Result, Context};

fn main() -> Result<()> {
    let data = std::fs::read_to_string("user.json")
        .context("could not read user.json")?;
    Ok(())
}
```

### Tokio Async Patterns

```rust
// Multi-threaded runtime
#[tokio::main(flavor = "multi_thread", worker_threads = 4)]
async fn main() { ... }

// Race futures: select! picks the first to complete
let winner = tokio::select! {
    result = slow_future() => result,
    result = fast_future() => result,
    _ = tokio::time::sleep(Duration::from_secs(5)) => "timeout",
};

// Blocking code inside async: use spawn_blocking
let data = tokio::task::spawn_blocking(|| {
    std::fs::read_to_string("file.txt")
}).await.unwrap();
```

Anti-pattern: blocking sync I/O inside `async fn`. Use `tokio::fs` instead of `std::fs`.

### Axum Web Server

```rust
use axum::{Router, routing::{get, post}, Json, http::StatusCode};
use std::sync::Arc;

#[derive(Clone)]
struct AppState { db_url: String }

async fn create_user(Json(user): Json<User>) -> (StatusCode, Json<User>) {
    (StatusCode::CREATED, Json(user))
}

#[tokio::main]
async fn main() {
    let state = Arc::new(AppState { db_url: "postgres://...".into() });
    let app = Router::new()
        .route("/users", post(create_user))
        .with_state(state);
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, app).await.unwrap();
}
```

### Concurrency Primitives

```rust
// Arc<Mutex<T>>: shared mutable state across threads
let counter = Arc::new(Mutex::new(0));
for _ in 0..10 {
    let c = Arc::clone(&counter);
    thread::spawn(move || { *c.lock().unwrap() += 1; });
}

// RwLock: multiple readers OR one writer
let data = Arc::new(RwLock::new(vec!["a", "b"]));
let read = data.read().unwrap();   // shared read
let write = data.write().unwrap(); // exclusive write

// Rayon: parallel iterators (automatic work-stealing)
use rayon::prelude::*;
let sum: u64 = (1..=1_000_000).collect::<Vec<_>>().par_iter().sum();
```

Prefer channels over Arc<Mutex> for message-passing patterns.

### SQLx: Compile-Time Checked SQL

```rust
use sqlx::PgPool;

let pool = PgPool::connect("postgres://user:pass@localhost/db").await?;
let users: Vec<(i32, String)> = sqlx::query_as(
    "SELECT id, name FROM users WHERE age > $1"
).bind(18).fetch_all(&pool).await?;
```

Database selection: `sqlx` for raw SQL with compile-time checks, `diesel` for type-safe query builder (sync), `sea-orm` for full async ORM.

### Clap v4 CLI (Derive API)

```rust
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "myapp", about = "My CLI tool")]
struct Cli {
    #[arg(short, long)] verbose: bool,
    #[command(subcommand)] command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Process { #[arg(short, long)] format: String },
    Download { url: String },
}
```

### PyO3: Python Bindings

```rust
use pyo3::prelude::*;

#[pymodule]
fn mymodule(py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(fast_compute, m)?)?;
    Ok(())
}

#[pyfunction]
fn fast_compute(data: Vec<f64>) -> f64 {
    data.iter().sum()
}
// Build with: maturin develop --release
// Use from Python: import mymodule; result = mymodule.fast_compute([1.0, 2.0])
```

### Rust 2024 Edition

```rust
// Async traits work natively (no async_trait crate needed)
pub trait Fetcher {
    async fn fetch(&self, url: &str) -> Result<String>;
}

// Let chains: cleaner nested conditions
if let Some(x) = get_value()
    && x > 0
    && let y = x * 2
    && y < 100
{
    println!("All conditions met: {}", y);
}
```

### Anti-Patterns to Avoid

| Anti-Pattern | Better Approach |
|---|---|
| Over-cloning to bypass borrow checker | Use references &T instead |
| Arc<Mutex<T>> everywhere | Single owner + mutable ref, or channels |
| .unwrap() in library code | Return Result<T, E> |
| Sync I/O in async functions | Use tokio::fs, spawn_blocking |

## Tools & References

- [The Rust Book](https://doc.rust-lang.org/book/)
- [Tokio Tutorial](https://tokio.rs/tokio/tutorial)
- [Axum Docs](https://docs.rs/axum/latest/axum/)
- [PyO3 Docs](https://pyo3.rs/)
- `cargo add tokio --features full` - async runtime
- `cargo add axum serde serde_json` - web + serialization
- `cargo add thiserror anyhow` - error handling

---
*Published by [MidOS](https://midos.dev) — MCP Community Library*
