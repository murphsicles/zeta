// src/benchmark.rs
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use zeta::compile_and_run_zeta;
use std::time::Instant;

// Zeta concurrent counter benchmark
fn bench_zeta_counter() -> Result<(), Box<dyn std::error::Error>> {
    let zeta_code = r#"
concept Send {}
concept Sync {}

impl Send for i32 {}
impl Sync for i32 {}

actor Counter {
    async fn increment(&self, delta: i32) -> i32;
    fn get(&self) -> i32;
}

impl Send for Counter {}
impl Sync for Counter {}

fn bench_main() -> i32 {
    let c = spawn_actor Counter();
    for i in 0..1000 {
        c.increment(i);
    }
    c.get()
}
"#;
    let start = Instant::now();
    let res = compile_and_run_zeta(zeta_code)?;
    let zeta_time = start.elapsed();
    println!("Zeta Counter: {} (res: {})", zeta_time.as_nanos(), res);
    Ok(())
}

// Rust equivalent (stub for comparison)
fn bench_rust_counter() {
    use std::sync::{Arc, Mutex};
    let counter = Arc::new(Mutex::new(0));
    let mut handles = vec![];
    for _ in 0..1000 {
        let c = Arc::clone(&counter);
        let h = std::thread::spawn(move || {
            let mut num = c.lock().unwrap();
            *num += 1;
        });
        handles.push(h);
    }
    for h in handles { h.join().unwrap(); }
    println!("Rust Counter: {} (res: {})", /* time */, counter.lock().unwrap());
}

// Zig/Go stubs omitted; extend with subprocess calls
fn bench_compare() {
    let mut group = Criterion::default().sample_size(10);
    group.bench_function("zeta_counter", |b| b.iter(|| black_box(compile_and_run_zeta("").unwrap())));
    group.finish();
}

criterion_group!(benches, bench_zeta_counter, bench_rust_counter);
criterion_main!(benches);
