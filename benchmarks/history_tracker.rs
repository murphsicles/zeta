//! Benchmark History Tracker
//! Tracks performance over time to measure improvements

use chrono::{DateTime, Local};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Single benchmark result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchmarkResult {
    pub name: String,
    pub value: f64,           // Mean time in nanoseconds
    pub unit: String,         // "ns", "ms", "ops/s"
    pub std_dev: f64,         // Standard deviation
    pub samples: usize,       // Number of samples
    pub timestamp: DateTime<Local>,
    pub git_commit: String,
    pub zeta_version: String,
}

/// Collection of benchmark results for a run
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchmarkRun {
    pub timestamp: DateTime<Local>,
    pub git_commit: String,
    pub zeta_version: String,
    pub results: HashMap<String, BenchmarkResult>, // benchmark_name -> result
    pub metadata: HashMap<String, String>,         // Additional metadata
}

/// Historical benchmark database
pub struct BenchmarkHistory {
    pub data_dir: PathBuf,
}

impl BenchmarkHistory {
    /// Create new history tracker
    pub fn new(data_dir: &Path) -> Self {
        // Create directory if it doesn't exist
        fs::create_dir_all(data_dir).unwrap_or_default();
        
        Self {
            data_dir: data_dir.to_path_buf(),
        }
    }
    
    /// Save benchmark run to history
    pub fn save_run(&self, run: &BenchmarkRun) -> Result<(), String> {
        // Create date-based directory
        let date_str = run.timestamp.format("%Y-%m-%d").to_string();
        let date_dir = self.data_dir.join(&date_str);
        fs::create_dir_all(&date_dir)
            .map_err(|e| format!("Failed to create directory: {}", e))?;
        
        // Create filename with timestamp and commit
        let filename = format!(
            "benchmarks_{}_{}.json",
            run.timestamp.format("%H%M%S"),
            &run.git_commit[..8]  // Short commit hash
        );
        
        let filepath = date_dir.join(filename);
        
        // Serialize and save
        let json = serde_json::to_string_pretty(run)
            .map_err(|e| format!("Failed to serialize benchmark run: {}", e))?;
        
        fs::write(&filepath, json)
            .map_err(|e| format!("Failed to write benchmark file: {}", e))?;
        
        // Also update latest run
        let latest_path = self.data_dir.join("latest_run.json");
        fs::write(&latest_path, serde_json::to_string_pretty(run).unwrap())
            .unwrap_or_default();
        
        Ok(())
    }
    
    /// Load all benchmark runs
    pub fn load_all_runs(&self) -> Result<Vec<BenchmarkRun>, String> {
        let mut runs = Vec::new();
        
        // Iterate through date directories
        let entries = fs::read_dir(&self.data_dir)
            .map_err(|e| format!("Failed to read data directory: {}", e))?;
        
        for entry in entries {
            let entry = entry.map_err(|e| format!("Failed to read entry: {}", e))?;
            let path = entry.path();
            
            if path.is_dir() {
                // This is a date directory (YYYY-MM-DD)
                let dir_entries = fs::read_dir(&path)
                    .map_err(|e| format!("Failed to read directory {}: {}", path.display(), e))?;
                
                for file_entry in dir_entries {
                    let file_entry = file_entry
                        .map_err(|e| format!("Failed to read file entry: {}", e))?;
                    let file_path = file_entry.path();
                    
                    if file_path.extension().map_or(false, |ext| ext == "json") {
                        match self.load_run(&file_path) {
                            Ok(run) => runs.push(run),
                            Err(e) => eprintln!("Warning: Failed to load {}: {}", file_path.display(), e),
                        }
                    }
                }
            }
        }
        
        // Sort by timestamp (oldest first)
        runs.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));
        
        Ok(runs)
    }
    
    /// Load single benchmark run
    pub fn load_run(&self, path: &Path) -> Result<BenchmarkRun, String> {
        let content = fs::read_to_string(path)
            .map_err(|e| format!("Failed to read file {}: {}", path.display(), e))?;
        
        serde_json::from_str(&content)
            .map_err(|e| format!("Failed to parse benchmark file {}: {}", path.display(), e))
    }
    
    /// Get performance trend for a specific benchmark
    pub fn get_trend(&self, benchmark_name: &str) -> Result<Vec<(DateTime<Local>, f64)>, String> {
        let runs = self.load_all_runs()?;
        
        let mut trend = Vec::new();
        
        for run in runs {
            if let Some(result) = run.results.get(benchmark_name) {
                trend.push((result.timestamp, result.value));
            }
        }
        
        // Sort by timestamp
        trend.sort_by(|a, b| a.0.cmp(&b.0));
        
        Ok(trend)
    }
    
    /// Generate summary statistics
    pub fn generate_summary(&self) -> Result<String, String> {
        let runs = self.load_all_runs()?;
        
        if runs.is_empty() {
            return Ok("No benchmark data available".to_string());
        }
        
        let mut summary = String::new();
        summary.push_str("=== BENCHMARK HISTORY SUMMARY ===\n\n");
        
        summary.push_str(&format!("Total runs: {}\n", runs.len()));
        summary.push_str(&format!("Time range: {} to {}\n", 
            runs.first().unwrap().timestamp.format("%Y-%m-%d"),
            runs.last().unwrap().timestamp.format("%Y-%m-%d")));
        
        // Group by benchmark
        let mut benchmarks: HashMap<String, Vec<f64>> = HashMap::new();
        
        for run in &runs {
            for (name, result) in &run.results {
                benchmarks.entry(name.clone())
                    .or_default()
                    .push(result.value);
            }
        }
        
        summary.push_str("\n=== BENCHMARK TRENDS ===\n");
        
        for (name, values) in benchmarks {
            if values.len() >= 2 {
                let first = values.first().unwrap();
                let last = values.last().unwrap();
                let change = (last - first) / first * 100.0;
                
                let trend = if change < -5.0 {
                    "📈 IMPROVED"
                } else if change > 5.0 {
                    "📉 REGRESSED"
                } else {
                    "📊 STABLE"
                };
                
                summary.push_str(&format!(
                    "{:30} {:8.2} → {:8.2} ({:+.1}%) {}\n",
                    name, first, last, change, trend
                ));
            }
        }
        
        // Detect regressions
        summary.push_str("\n=== RECENT REGRESSIONS (>10%) ===\n");
        
        let recent_runs: Vec<&BenchmarkRun> = runs.iter().rev().take(5).collect();
        if recent_runs.len() >= 2 {
            let current = recent_runs[0];
            let previous = recent_runs[1];
            
            for (name, current_result) in &current.results {
                if let Some(prev_result) = previous.results.get(name) {
                    let change = (current_result.value - prev_result.value) / prev_result.value * 100.0;
                    
                    if change > 10.0 {
                        summary.push_str(&format!(
                            "⚠️  {}: {:+.1}% slower ({:.2} → {:.2})\n",
                            name, change, prev_result.value, current_result.value
                        ));
                    }
                }
            }
        }
        
        Ok(summary)
    }
    
    /// Export data for visualization
    pub fn export_for_visualization(&self) -> Result<String, String> {
        let runs = self.load_all_runs()?;
        
        #[derive(Serialize)]
        struct VisualizationData {
            benchmarks: Vec<String>,
            timestamps: Vec<String>,
            data: HashMap<String, Vec<f64>>,
        }
        
        let mut vis_data = VisualizationData {
            benchmarks: Vec::new(),
            timestamps: Vec::new(),
            data: HashMap::new(),
        };
        
        // Collect all benchmark names
        let mut all_benchmarks = std::collections::HashSet::new();
        for run in &runs {
            for name in run.results.keys() {
                all_benchmarks.insert(name.clone());
            }
        }
        
        vis_data.benchmarks = all_benchmarks.into_iter().collect();
        vis_data.benchmarks.sort();
        
        // Collect data
        for run in &runs {
            vis_data.timestamps.push(run.timestamp.format("%Y-%m-%d %H:%M").to_string());
            
            for benchmark in &vis_data.benchmarks {
                if let Some(result) = run.results.get(benchmark) {
                    vis_data.data.entry(benchmark.clone())
                        .or_default()
                        .push(result.value);
                } else {
                    // Fill missing data with NaN
                    vis_data.data.entry(benchmark.clone())
                        .or_default()
                        .push(f64::NAN);
                }
            }
        }
        
        serde_json::to_string_pretty(&vis_data)
            .map_err(|e| format!("Failed to serialize visualization data: {}", e))
    }
}

/// Helper to get current git commit hash
pub fn get_git_commit() -> String {
    match std::process::Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
    {
        Ok(output) if output.status.success() => {
            String::from_utf8_lossy(&output.stdout).trim().to_string()
        }
        _ => "unknown".to_string(),
    }
}

/// Helper to get Zeta version
pub fn get_zeta_version() -> String {
    // Try to read from Cargo.toml
    match fs::read_to_string("Cargo.toml") {
        Ok(content) => {
            for line in content.lines() {
                if line.trim().starts_with("version =") {
                    if let Some(version) = line.split('=').nth(1) {
                        return version.trim().trim_matches('"').to_string();
                    }
                }
            }
            "unknown".to_string()
        }
        Err(_) => "unknown".to_string(),
    }
}

/// Create benchmark run from criterion results
pub fn create_run_from_criterion(
    benchmark_name: &str,
    mean_ns: f64,
    std_dev_ns: f64,
    samples: usize,
) -> BenchmarkRun {
    let timestamp = Local::now();
    let git_commit = get_git_commit();
    let zeta_version = get_zeta_version();
    
    let result = BenchmarkResult {
        name: benchmark_name.to_string(),
        value: mean_ns,
        unit: "ns".to_string(),
        std_dev: std_dev_ns,
        samples,
        timestamp,
        git_commit: git_commit.clone(),
        zeta_version: zeta_version.clone(),
    };
    
    let mut results = HashMap::new();
    results.insert(benchmark_name.to_string(), result);
    
    let mut metadata = HashMap::new();
    metadata.insert("source".to_string(), "criterion".to_string());
    metadata.insert("hostname".to_string(), 
        std::env::var("COMPUTERNAME").unwrap_or_else(|_| "unknown".to_string()));
    
    BenchmarkRun {
        timestamp,
        git_commit,
        zeta_version,
        results,
        metadata,
    }
}

/// Main function for testing
fn main() -> Result<(), String> {
    let args: Vec<String> = std::env::args().collect();
    
    let history = BenchmarkHistory::new(Path::new("benchmarks/history"));
    
    if args.len() > 1 {
        match args[1].as_str() {
            "summary" => {
                let summary = history.generate_summary()?;
                println!("{}", summary);
            }
            "export" => {
                let json = history.export_for_visualization()?;
                println!("{}", json);
            }
            "trend" if args.len() > 2 => {
                let trend = history.get_trend(&args[2])?;
                println!("Trend for {}:", args[2]);
                for (timestamp, value) in trend {
                    println!("  {}: {:.2}", timestamp.format("%Y-%m-%d"), value);
                }
            }
            "test" => {
                // Create a test run
                let run = create_run_from_criterion("test_benchmark", 1000.0, 50.0, 100);
                history.save_run(&run)?;
                println!("Test run saved");
            }
            _ => {
                println!("Usage:");
                println!("  history_tracker summary    - Show summary");
                println!("  history_tracker export     - Export for visualization");
                println!("  history_tracker trend <name> - Show trend for benchmark");
                println!("  history_tracker test       - Create test run");
            }
        }
    } else {
        println!("Benchmark History Tracker");
        println!("Use --help for usage information");
    }
    
    Ok(())
}