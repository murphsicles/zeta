//! SMT Solver Integration for Zeta Verification
//!
//! This module provides integration with Z3 SMT solver for
//! discharging verification conditions.

use std::process::{Command, Stdio};
use std::io::{Write, Read};

/// SMT solver result
#[derive(Debug, Clone, PartialEq)]
pub enum SolverResult {
    /// Verification condition is provable
    Proven,
    /// Verification condition is false (counterexample exists)
    Counterexample(String),
    /// Solver timeout or unknown result
    Unknown(String),
    /// Solver error
    Error(String),
}

impl SolverResult {
    /// Check if the VC was proven
    pub fn is_proven(&self) -> bool {
        matches!(self, SolverResult::Proven)
    }
}

/// SMT solver interface
pub struct Solver {
    /// Path to Z3 executable
    z3_path: String,
    /// Timeout in seconds
    timeout: u32,
}

impl Solver {
    /// Create a new solver with default settings
    pub fn new() -> Self {
        Self {
            z3_path: "z3".to_string(),
            timeout: 10,
        }
    }
    
    /// Create solver with custom Z3 path
    pub fn with_z3_path(path: &str) -> Self {
        Self {
            z3_path: path.to_string(),
            timeout: 10,
        }
    }
    
    /// Set solver timeout
    pub fn with_timeout(mut self, timeout: u32) -> Self {
        self.timeout = timeout;
        self
    }
    
    /// Solve a verification condition
    pub fn solve(&self, vc: &str) -> Result<SolverResult, String> {
        // Create SMT-LIB2 script with timeout
        let smt_script = format!(
            "(set-option :timeout {})\n{}\n(check-sat)\n(get-model)",
            self.timeout * 1000, // Convert to milliseconds
            vc
        );
        
        // Run Z3
        let mut child = Command::new(&self.z3_path)
            .arg("-smt2")
            .arg("-in")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| format!("Failed to spawn Z3 process: {}", e))?;
        
        // Write SMT script to stdin
        if let Some(mut stdin) = child.stdin.take() {
            stdin.write_all(smt_script.as_bytes())
                .map_err(|e| format!("Failed to write to Z3 stdin: {}", e))?;
        }
        
        // Wait for completion and get output
        let output = child.wait_with_output()
            .map_err(|e| format!("Failed to get Z3 output: {}", e))?;
        
        // Parse output
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        
        if !output.status.success() {
            return Ok(SolverResult::Error(format!("Z3 error: {}", stderr)));
        }
        
        // Parse result
        let lines: Vec<&str> = stdout.lines().collect();
        if lines.is_empty() {
            return Ok(SolverResult::Error("Empty output from Z3".to_string()));
        }
        
        match lines[0].trim() {
            "sat" => {
                // Counterexample found
                let model = if lines.len() > 1 {
                    lines[1..].join("\n")
                } else {
                    "No model".to_string()
                };
                Ok(SolverResult::Counterexample(model))
            }
            "unsat" => {
                // VC is provable
                Ok(SolverResult::Proven)
            }
            "unknown" => {
                // Unknown result
                let reason = if lines.len() > 1 {
                    lines[1..].join("\n")
                } else {
                    "Unknown reason".to_string()
                };
                Ok(SolverResult::Unknown(reason))
            }
            "timeout" => {
                Ok(SolverResult::Unknown("Timeout".to_string()))
            }
            other => {
                Ok(SolverResult::Error(format!("Unexpected Z3 output: {}", other)))
            }
        }
    }
    
    /// Check if Z3 is available
    pub fn check_availability(&self) -> bool {
        Command::new(&self.z3_path)
            .arg("--version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .map(|status| status.success())
            .unwrap_or(false)
    }
}

/// Simple solver for testing (doesn't require Z3)
pub struct MockSolver {
    /// Whether to return proven or counterexample
    always_proven: bool,
}

impl MockSolver {
    /// Create a mock solver that always returns proven
    pub fn always_proven() -> Self {
        Self { always_proven: true }
    }
    
    /// Create a mock solver that always returns counterexample
    pub fn always_counterexample() -> Self {
        Self { always_proven: false }
    }
}

impl SolverTrait for MockSolver {
    fn solve(&self, _vc: &str) -> Result<SolverResult, String> {
        if self.always_proven {
            Ok(SolverResult::Proven)
        } else {
            Ok(SolverResult::Counterexample("Mock counterexample".to_string()))
        }
    }
    
    fn check_availability(&self) -> bool {
        true
    }
}

/// Trait for solvers
pub trait SolverTrait {
    /// Solve a verification condition
    fn solve(&self, vc: &str) -> Result<SolverResult, String>;
    
    /// Check if solver is available
    fn check_availability(&self) -> bool;
}

impl SolverTrait for Solver {
    fn solve(&self, vc: &str) -> Result<SolverResult, String> {
        self.solve(vc)
    }
    
    fn check_availability(&self) -> bool {
        self.check_availability()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_mock_solver_proven() {
        let solver = MockSolver::always_proven();
        let result = solver.solve("(assert true)").unwrap();
        assert!(result.is_proven());
    }
    
    #[test]
    fn test_mock_solver_counterexample() {
        let solver = MockSolver::always_counterexample();
        let result = solver.solve("(assert false)").unwrap();
        assert!(!result.is_proven());
    }
}