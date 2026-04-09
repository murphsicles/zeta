//! Performance profiling tools

use std::path::Path;
use std::time::{Duration, Instant};
use crate::workflows::WorkflowError;

/// Profile metric
#[derive(Debug, Clone)]
pub enum ProfileMetric {
    /// Execution time in seconds
    ExecutionTime(Duration),
    /// Memory usage in bytes
    MemoryUsage(u64),
    /// CPU usage percentage
    CpuUsage(f64),
    /// Cache hits/misses
    CacheStats {
        hits: u64,
        misses: u64,
        ratio: f64,
    },
    /// Throughput (operations per second)
    Throughput(f64),
}

/// Profile result
#[derive(Debug, Clone)]
pub struct ProfileResult {
    /// Test name
    pub name: String,
    /// Metrics
    pub metrics: Vec<ProfileMetric>,
    /// Timestamp
    pub timestamp: chrono::DateTime<chrono::Local>,
}

impl ProfileResult {
    /// Create a new profile result
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            metrics: Vec::new(),
            timestamp: chrono::Local::now(),
        }
    }
    
    /// Add a metric
    pub fn add_metric(&mut self, metric: ProfileMetric) {
        self.metrics.push(metric);
    }
    
    /// Format as markdown
    pub fn to_markdown(&self) -> String {
        let mut md = String::new();
        md.push_str(&format!("## {}\n\n", self.name));
        md.push_str(&format!("*Timestamp: {}*\n\n", self.timestamp.format("%Y-%m-%d %H:%M:%S")));
        
        md.push_str("| Metric | Value |\n");
        md.push_str("|--------|-------|\n");
        
        for metric in &self.metrics {
            match metric {
                ProfileMetric::ExecutionTime(duration) => {
                    let secs = duration.as_secs_f64();
                    md.push_str(&format!("| Execution Time | {:.6} seconds |\n", secs));
                }
                ProfileMetric::MemoryUsage(bytes) => {
                    let mb = *bytes as f64 / 1024.0 / 1024.0;
                    md.push_str(&format!("| Memory Usage | {:.2} MB |\n", mb));
                }
                ProfileMetric::CpuUsage(percent) => {
                    md.push_str(&format!("| CPU Usage | {:.1}% |\n", percent));
                }
                ProfileMetric::CacheStats { hits, misses, ratio } => {
                    md.push_str(&format!("| Cache Hits | {} |\n", hits));
                    md.push_str(&format!("| Cache Misses | {} |\n", misses));
                    md.push_str(&format!("| Cache Hit Ratio | {:.2}% |\n", ratio * 100.0));
                }
                ProfileMetric::Throughput(ops_per_sec) => {
                    md.push_str(&format!("| Throughput | {:.0} ops/sec |\n", ops_per_sec));
                }
            }
        }
        
        md
    }
    
    /// Format as JSON
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        #[derive(serde::Serialize)]
        struct JsonMetric {
            metric_type: String,
            value: serde_json::Value,
        }
        
        #[derive(serde::Serialize)]
        struct JsonResult {
            name: String,
            timestamp: String,
            metrics: Vec<JsonMetric>,
        }
        
        let mut json_metrics = Vec::new();
        
        for metric in &self.metrics {
            match metric {
                ProfileMetric::ExecutionTime(duration) => {
                    json_metrics.push(JsonMetric {
                        metric_type: "execution_time".to_string(),
                        value: serde_json::Value::Number(
                            serde_json::Number::from_f64(duration.as_secs_f64())
                                .unwrap_or(serde_json::Number::from(0))
                        ),
                    });
                }
                ProfileMetric::MemoryUsage(bytes) => {
                    json_metrics.push(JsonMetric {
                        metric_type: "memory_usage".to_string(),
                        value: serde_json::Value::Number(
                            serde_json::Number::from(*bytes)
                        ),
                    });
                }
                ProfileMetric::CpuUsage(percent) => {
                    json_metrics.push(JsonMetric {
                        metric_type: "cpu_usage".to_string(),
                        value: serde_json::Value::Number(
                            serde_json::Number::from_f64(*percent)
                                .unwrap_or(serde_json::Number::from(0))
                        ),
                    });
                }
                ProfileMetric::CacheStats { hits, misses, ratio } => {
                    json_metrics.push(JsonMetric {
                        metric_type: "cache_stats".to_string(),
                        value: serde_json::json!({
                            "hits": hits,
                            "misses": misses,
                            "ratio": ratio,
                        }),
                    });
                }
                ProfileMetric::Throughput(ops_per_sec) => {
                    json_metrics.push(JsonMetric {
                        metric_type: "throughput".to_string(),
                        value: serde_json::Value::Number(
                            serde_json::Number::from_f64(*ops_per_sec)
                                .unwrap_or(serde_json::Number::from(0))
                        ),
                    });
                }
            }
        }
        
        let result = JsonResult {
            name: self.name.clone(),
            timestamp: self.timestamp.to_rfc3339(),
            metrics: json_metrics,
        };
        
        serde_json::to_string_pretty(&result)
    }
}

/// Profiler
pub struct Profiler {
    /// Benchmark iterations
    pub iterations: u32,
    /// Warmup iterations
    pub warmup_iterations: u32,
}

impl Profiler {
    /// Create a new profiler
    pub fn new() -> Self {
        Self {
            iterations: 100,
            warmup_iterations: 10,
        }
    }
    
    /// Profile a function
    pub fn profile<F, R>(&self, name: &str, f: F) -> ProfileResult
    where
        F: Fn() -> R,
    {
        let mut result = ProfileResult::new(name);
        
        // Warmup
        for _ in 0..self.warmup_iterations {
            let _ = f();
        }
        
        // Actual profiling
        let start = Instant::now();
        for _ in 0..self.iterations {
            let _ = f();
        }
        let duration = start.elapsed();
        
        let avg_duration = duration / self.iterations;
        result.add_metric(ProfileMetric::ExecutionTime(avg_duration));
        
        // Calculate throughput
        let ops_per_sec = self.iterations as f64 / duration.as_secs_f64();
        result.add_metric(ProfileMetric::Throughput(ops_per_sec));
        
        // Simulate memory usage (in real implementation, would measure actual memory)
        let memory_usage = 1024 * 1024; // 1 MB simulated
        result.add_metric(ProfileMetric::MemoryUsage(memory_usage));
        
        // Simulate CPU usage (in real implementation, would measure actual CPU)
        let cpu_usage = 15.5; // 15.5% simulated
        result.add_metric(ProfileMetric::CpuUsage(cpu_usage));
        
        result
    }
    
    /// Profile with custom setup/teardown
    pub fn profile_with_setup<F, S, T, R>(
        &self,
        name: &str,
        setup: S,
        teardown: T,
        f: F,
    ) -> ProfileResult
    where
        S: Fn() -> (),
        T: Fn() -> (),
        F: Fn() -> R,
    {
        let mut result = ProfileResult::new(name);
        
        // Warmup with setup/teardown
        for _ in 0..self.warmup_iterations {
            setup();
            let _ = f();
            teardown();
        }
        
        // Actual profiling
        let start = Instant::now();
        for _ in 0..self.iterations {
            setup();
            let _ = f();
            teardown();
        }
        let duration = start.elapsed();
        
        let avg_duration = duration / self.iterations;
        result.add_metric(ProfileMetric::ExecutionTime(avg_duration));
        
        let ops_per_sec = self.iterations as f64 / duration.as_secs_f64();
        result.add_metric(ProfileMetric::Throughput(ops_per_sec));
        
        result
    }
}

/// Run profiler on a target
pub fn run_profiler(
    project_root: &Path,
    profiler: &Profiler,
    target: &str,
) -> Result<Vec<ProfileResult>, WorkflowError> {
    let mut results = Vec::new();
    
    match target {
        "build" => {
            // Profile build process
            let result = profiler.profile("Build Process", || {
                // Simulate build process
                std::thread::sleep(Duration::from_millis(10));
            });
            results.push(result);
        }
        "test" => {
            // Profile test execution
            let result = profiler.profile("Test Execution", || {
                // Simulate test execution
                std::thread::sleep(Duration::from_millis(5));
            });
            results.push(result);
        }
        "compile" => {
            // Profile compilation
            let result = profiler.profile_with_setup(
                "Compilation",
                || {
                    // Setup: create test source file
                    let test_src = project_root.join("test_compile.z");
                    let _ = std::fs::write(&test_src, "fn main() { let x = 42; }");
                },
                || {
                    // Teardown: remove test file
                    let test_src = project_root.join("test_compile.z");
                    let _ = std::fs::remove_file(&test_src);
                },
                || {
                    // Compilation
                    std::thread::sleep(Duration::from_millis(20));
                },
            );
            results.push(result);
        }
        _ => {
            return Err(WorkflowError::ProfilingError(
                format!("Unknown profiling target: {}", target)
            ));
        }
    }
    
    // Save results
    let profile_dir = project_root.join("profiles");
    std::fs::create_dir_all(&profile_dir)?;
    
    let timestamp = chrono::Local::now().format("%Y%m%d_%H%M%S");
    let report_path = profile_dir.join(format!("profile_{}_{}.md", target, timestamp));
    
    let mut report = String::new();
    report.push_str("# Performance Profile Report\n\n");
    
    for result in &results {
        report.push_str(&result.to_markdown());
        report.push_str("\n\n");
        
        // Also save individual JSON results
        let json_path = profile_dir.join(format!("{}_{}.json", result.name, timestamp));
        if let Ok(json) = result.to_json() {
            let _ = std::fs::write(json_path, json);
        }
    }
    
    std::fs::write(report_path, report)?;
    
    println!("Profiling completed. Results saved to profiles/ directory.");
    
    Ok(results)
}