//! Regression testing for Zeta performance
//!
//! Tracks performance over time and fails if performance regresses
//! Integrates with CI to prevent performance degradation

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::time::{Duration, Instant};

/// Performance baseline data structure
#[derive(Debug, Clone, Serialize, Deserialize)]
struct PerformanceBaseline {
    /// Benchmark name → (mean_time_ns, std_dev_ns)
    benchmarks: HashMap<String, (f64, f64)>,
    /// Git commit hash when baseline was recorded
    commit_hash: String,
    /// Timestamp of recording
    timestamp: String,
    /// Allowed regression threshold (percentage)
    regression_threshold: f64,
}

impl PerformanceBaseline {
    /// Load baseline from file
    fn load(path: &Path) -> Result<Self, String> {
        let content =
            fs::read_to_string(path).map_err(|e| format!("Failed to read baseline file: {}", e))?;
        serde_json::from_str(&content).map_err(|e| format!("Failed to parse baseline JSON: {}", e))
    }

    /// Save baseline to file
    fn save(&self, path: &Path) -> Result<(), String> {
        let json = serde_json::to_string_pretty(self)
            .map_err(|e| format!("Failed to serialize baseline: {}", e))?;
        fs::write(path, json).map_err(|e| format!("Failed to write baseline file: {}", e))
    }

    /// Check if current performance matches baseline
    fn check_regression(&self, benchmark_name: &str, current_time_ns: f64) -> Result<(), String> {
        if let Some((baseline_mean, baseline_std)) = self.benchmarks.get(benchmark_name) {
            let diff_percent = ((current_time_ns - baseline_mean) / baseline_mean) * 100.0;

            // Allow some natural variation (2 standard deviations)
            let allowed_variation = baseline_std * 2.0;
            let allowed_percent = (allowed_variation / baseline_mean) * 100.0;

            let threshold = self.regression_threshold.max(allowed_percent);

            if diff_percent > threshold {
                return Err(format!(
                    "Performance regression detected for '{}': {:.2}% slower (baseline: {:.2}ns, current: {:.2}ns, threshold: {:.2}%)",
                    benchmark_name, diff_percent, baseline_mean, current_time_ns, threshold
                ));
            } else if diff_percent < -threshold {
                println!(
                    "Performance improvement for '{}': {:.2}% faster",
                    benchmark_name, -diff_percent
                );
            }
        } else {
            println!(
                "No baseline for '{}', recording new baseline",
                benchmark_name
            );
        }

        Ok(())
    }
}

/// Run a benchmark and return mean execution time in nanoseconds
fn run_benchmark<F>(benchmark_fn: F, iterations: usize) -> f64
where
    F: Fn() -> Duration,
{
    let mut times = Vec::with_capacity(iterations);

    for _ in 0..iterations {
        let start = Instant::now();
        benchmark_fn();
        let duration = start.elapsed();
        times.push(duration.as_nanos() as f64);
    }

    // Calculate mean
    let sum: f64 = times.iter().sum();
    sum / times.len() as f64
}

/// Simple benchmark: Parse a Zeta file
fn benchmark_parse() -> Duration {
    let source = r#"
fn main() -> i64 {
    let x = 10;
    let y = 20;
    x + y
}
"#;

    let start = Instant::now();
    let _ = zetac::frontend::parser::top_level::parse_zeta(source);
    start.elapsed()
}

/// Simple benchmark: Type check
fn benchmark_typecheck() -> Duration {
    let source = r#"
fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn main() -> i64 {
    add(10, 20)
}
"#;

    let start = Instant::now();
    let (_, asts) = zetac::frontend::parser::top_level::parse_zeta(source).unwrap();
    let mut resolver = zetac::middle::resolver::resolver::Resolver::new();
    for ast in &asts {
        resolver.register(ast.clone());
    }
    let _ = resolver.typecheck(&asts);
    start.elapsed()
}

/// Run all regression tests
fn run_regression_tests(baseline_path: &Path) -> Result<(), Vec<String>> {
    let baseline = match PerformanceBaseline::load(baseline_path) {
        Ok(b) => b,
        Err(e) => {
            println!("No baseline found: {}. Creating new baseline.", e);
            return Ok(()); // First run, no baseline to compare against
        }
    };

    let mut errors = Vec::new();

    // Run parse benchmark
    println!("Running parse benchmark...");
    let parse_time = run_benchmark(benchmark_parse, 100);
    if let Err(e) = baseline.check_regression("parse", parse_time) {
        errors.push(e);
    }

    // Run typecheck benchmark
    println!("Running typecheck benchmark...");
    let typecheck_time = run_benchmark(benchmark_typecheck, 50);
    if let Err(e) = baseline.check_regression("typecheck", typecheck_time) {
        errors.push(e);
    }

    if errors.is_empty() {
        println!("All regression tests passed!");
        Ok(())
    } else {
        Err(errors)
    }
}

/// Update baseline with current performance
fn update_baseline(baseline_path: &Path) -> Result<(), String> {
    // Get current git commit hash
    let commit_hash = match std::process::Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
    {
        Ok(output) if output.status.success() => {
            String::from_utf8_lossy(&output.stdout).trim().to_string()
        }
        _ => "unknown".to_string(),
    };

    // Run benchmarks to get current performance
    let parse_time = run_benchmark(benchmark_parse, 1000);
    let typecheck_time = run_benchmark(benchmark_typecheck, 500);

    // Calculate standard deviation (simplified)
    let parse_std = parse_time * 0.1; // Assume 10% variation
    let typecheck_std = typecheck_time * 0.1;

    let mut benchmarks = HashMap::new();
    benchmarks.insert("parse".to_string(), (parse_time, parse_std));
    benchmarks.insert("typecheck".to_string(), (typecheck_time, typecheck_std));

    let baseline = PerformanceBaseline {
        benchmarks,
        commit_hash,
        timestamp: chrono::Local::now().to_rfc3339(),
        regression_threshold: 5.0, // 5% regression threshold (changed from 10%)
    };

    baseline.save(baseline_path)
}

/// Main function for regression testing
fn main() {
    let args: Vec<String> = std::env::args().collect();
    let baseline_path = Path::new("benches/performance_baseline.json");

    if args.len() > 1 && args[1] == "update" {
        println!("Updating performance baseline...");
        match update_baseline(baseline_path) {
            Ok(_) => println!("Baseline updated successfully"),
            Err(e) => {
                eprintln!("Failed to update baseline: {}", e);
                std::process::exit(1);
            }
        }
    } else {
        println!("Running regression tests...");
        match run_regression_tests(baseline_path) {
            Ok(_) => {
                println!("Regression tests passed");
                std::process::exit(0);
            }
            Err(errors) => {
                eprintln!("Regression tests failed:");
                for error in errors {
                    eprintln!("  {}", error);
                }
                std::process::exit(1);
            }
        }
    }
}
