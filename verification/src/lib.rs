//! Formal Verification System for Zeta
//!
//! This module provides compile-time formal verification using
//! refinement types and SMT solving.

pub mod refinement;
pub mod vcgen;
pub mod solver;
pub mod annotations;

/// Main verification interface
pub struct Verifier {
    solver: solver::Solver,
}

impl Verifier {
    /// Create a new verifier
    pub fn new() -> Self {
        Self {
            solver: solver::Solver::new(),
        }
    }
    
    /// Verify a Zeta program
    pub fn verify(&mut self, program: &str) -> Result<VerificationResult, VerificationError> {
        // Parse annotations
        let _annotated_ast = annotations::parse_annotations(program)?;
        
        // Generate verification conditions
        let vcs = vcgen::generate_vcs(program)?;
        
        // Solve verification conditions
        let mut results = Vec::new();
        for vc in vcs {
            // Convert VC to string for solver
            let vc_str = format!("{}", vc);
            let result = self.solver.solve(&vc_str)?;
            results.push((vc, result));
        }
        
        // Check if all VCs are proven
        let all_proven = results.iter().all(|(_, result)| result.is_proven());
        
        Ok(VerificationResult {
            vcs: results,
            all_proven,
        })
    }
}

/// Verification result
pub struct VerificationResult {
    /// Verification conditions and their results
    pub vcs: Vec<(vcgen::VerificationCondition, solver::SolverResult)>,
    /// Whether all VCs were proven
    pub all_proven: bool,
}

/// Verification error
#[derive(Debug)]
pub enum VerificationError {
    /// Parser error
    ParseError(String),
    /// VC generation error
    VCGenError(String),
    /// Solver error
    SolverError(String),
    /// Timeout
    Timeout,
}

impl std::fmt::Display for VerificationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VerificationError::ParseError(msg) => write!(f, "Parse error: {}", msg),
            VerificationError::VCGenError(msg) => write!(f, "VC generation error: {}", msg),
            VerificationError::SolverError(msg) => write!(f, "Solver error: {}", msg),
            VerificationError::Timeout => write!(f, "Verification timeout"),
        }
    }
}

impl From<String> for VerificationError {
    fn from(err: String) -> Self {
        VerificationError::ParseError(err)
    }
}

impl std::error::Error for VerificationError {}

/// Check if a Zeta program is formally verified
pub fn is_verified(program: &str) -> bool {
    match Verifier::new().verify(program) {
        Ok(result) => result.all_proven,
        Err(_) => false,
    }
}