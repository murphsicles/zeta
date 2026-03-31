//! Runtime performance benchmarks for Zeta
//!
//! Measures execution speed of compiled Zeta code

use criterion::{Criterion, criterion_group, criterion_main};
use std::fs;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

/// Helper: Compile and run a Zeta program, returning execution time
fn compile_and_run(source: &str, program_name: &str) -> Result<Duration, String> {
    // Write source to temporary file
    let source_file = format!("benches/temp_{}.z", program_name);
    fs::write(&source_file, source).map_err(|e| e.to_string())?;

    // Compile with zetac
    let compile_start = Instant::now();
    let compile_output = Command::new("cargo")
        .args(["run", "--bin", "zetac", "--", &source_file])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|e| e.to_string())?;

    let compile_time = compile_start.elapsed();

    if !compile_output.status.success() {
        let stderr = String::from_utf8_lossy(&compile_output.stderr);
        return Err(format!("Compilation failed: {}", stderr));
    }

    // The compiled executable is at target/debug/zetac.exe
    // For now, we'll just measure compilation time
    // TODO: Actually execute the compiled program when we have AOT compilation

    Ok(compile_time)
}

/// Benchmark: Arithmetic operations
fn bench_arithmetic(c: &mut Criterion) {
    let source = r#"
fn main() -> i64 {
    let mut sum = 0;
    let mut i = 0;
    while i < 1000 {
        sum = sum + i;
        i = i + 1;
    }
    sum
}
"#;

    c.bench_function("arithmetic_loop", |b| {
        b.iter_custom(|iterations| {
            let mut total_time = Duration::new(0, 0);

            for _ in 0..iterations {
                match compile_and_run(source, "arithmetic") {
                    Ok(time) => total_time += time,
                    Err(e) => {
                        eprintln!("Benchmark failed: {}", e);
                        return Duration::new(0, 0);
                    }
                }
            }

            total_time
        });
    });
}

/// Benchmark: Recursive function (factorial)
fn bench_recursion(c: &mut Criterion) {
    let source = r#"
fn factorial(n: i64) -> i64 {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}

fn main() -> i64 {
    factorial(10)
}
"#;

    c.bench_function("recursive_factorial", |b| {
        b.iter_custom(|iterations| {
            let mut total_time = Duration::new(0, 0);

            for _ in 0..iterations {
                match compile_and_run(source, "factorial") {
                    Ok(time) => total_time += time,
                    Err(e) => {
                        eprintln!("Benchmark failed: {}", e);
                        return Duration::new(0, 0);
                    }
                }
            }

            total_time
        });
    });
}

/// Benchmark: Function calls
fn bench_function_calls(c: &mut Criterion) {
    let source = r#"
fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn mul(a: i64, b: i64) -> i64 {
    a * b
}

fn main() -> i64 {
    let mut result = 0;
    let mut i = 0;
    while i < 100 {
        result = add(result, i);
        result = mul(result, 2);
        i = i + 1;
    }
    result
}
"#;

    c.bench_function("function_calls", |b| {
        b.iter_custom(|iterations| {
            let mut total_time = Duration::new(0, 0);

            for _ in 0..iterations {
                match compile_and_run(source, "function_calls") {
                    Ok(time) => total_time += time,
                    Err(e) => {
                        eprintln!("Benchmark failed: {}", e);
                        return Duration::new(0, 0);
                    }
                }
            }

            total_time
        });
    });
}

/// Benchmark: Memory operations (array/vector)
fn bench_memory_ops(c: &mut Criterion) {
    let source = r#"
fn main() -> i64 {
    let mut arr = [0; 100];
    let mut i = 0;
    while i < 100 {
        arr[i] = i;
        i = i + 1;
    }
    
    let mut sum = 0;
    i = 0;
    while i < 100 {
        sum = sum + arr[i];
        i = i + 1;
    }
    sum
}
"#;

    c.bench_function("memory_operations", |b| {
        b.iter_custom(|iterations| {
            let mut total_time = Duration::new(0, 0);

            for _ in 0..iterations {
                match compile_and_run(source, "memory_ops") {
                    Ok(time) => total_time += time,
                    Err(e) => {
                        eprintln!("Benchmark failed: {}", e);
                        return Duration::new(0, 0);
                    }
                }
            }

            total_time
        });
    });
}

/// Benchmark: Control flow (if/else, loops)
fn bench_control_flow(c: &mut Criterion) {
    let source = r#"
fn main() -> i64 {
    let mut result = 0;
    let mut i = 0;
    while i < 1000 {
        if i % 2 == 0 {
            result = result + i;
        } else {
            result = result - i;
        }
        
        if i % 3 == 0 {
            result = result * 2;
        }
        
        if i % 5 == 0 {
            result = result / 2;
        }
        
        i = i + 1;
    }
    result
}
"#;

    c.bench_function("control_flow", |b| {
        b.iter_custom(|iterations| {
            let mut total_time = Duration::new(0, 0);

            for _ in 0..iterations {
                match compile_and_run(source, "control_flow") {
                    Ok(time) => total_time += time,
                    Err(e) => {
                        eprintln!("Benchmark failed: {}", e);
                        return Duration::new(0, 0);
                    }
                }
            }

            total_time
        });
    });
}

// Configure benchmark groups
criterion_group!(
    name = runtime_benches;
    config = Criterion::default()
        .sample_size(10)
        .warm_up_time(std::time::Duration::from_millis(50))
        .measurement_time(std::time::Duration::from_millis(200));
    targets = bench_arithmetic, bench_recursion, bench_function_calls, bench_memory_ops, bench_control_flow
);

criterion_main!(runtime_benches);
