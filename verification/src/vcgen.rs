//! Verification Condition Generation for Zeta
//!
//! This module generates verification conditions (VCs) from
//! annotated Zeta programs.

use crate::refinement::{RefinementType, Predicate, Expr};

/// Verification condition
#[derive(Debug, Clone)]
pub struct VerificationCondition {
    /// VC name/description
    pub name: String,
    /// SMT-LIB2 representation
    pub smt2: String,
    /// Source location (if available)
    pub location: Option<String>,
}

impl std::fmt::Display for VerificationCondition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.smt2)
    }
}

impl VerificationCondition {
    /// Create a new verification condition
    pub fn new(name: &str, smt2: &str) -> Self {
        Self {
            name: name.to_string(),
            smt2: smt2.to_string(),
            location: None,
        }
    }
    
    /// Create VC with location
    pub fn with_location(name: &str, smt2: &str, location: &str) -> Self {
        Self {
            name: name.to_string(),
            smt2: smt2.to_string(),
            location: Some(location.to_string()),
        }
    }
}

/// VC generator
pub struct VCGenerator {
    /// Generated VCs
    vcs: Vec<VerificationCondition>,
}

impl VCGenerator {
    /// Create a new VC generator
    pub fn new() -> Self {
        Self { vcs: Vec::new() }
    }
    
    /// Generate VCs from annotated program
    pub fn generate(&mut self, program: &str) -> Result<Vec<VerificationCondition>, String> {
        self.vcs.clear();
        
        // Parse program and extract annotations
        let lines: Vec<&str> = program.lines().collect();
        
        for (i, line) in lines.iter().enumerate() {
            let line_num = i + 1;
            
            // Check for function contracts
            if line.contains("fn ") && line.contains("->") {
                if let Some(vc) = self.generate_function_contract(line, line_num)? {
                    self.vcs.push(vc);
                }
            }
            
            // Check for loop invariants
            if line.contains("@invariant") {
                if let Some(vc) = self.generate_loop_invariant(line, line_num)? {
                    self.vcs.push(vc);
                }
            }
            
            // Check for assertions
            if line.contains("@assert") {
                if let Some(vc) = self.generate_assertion(line, line_num)? {
                    self.vcs.push(vc);
                }
            }
            
            // Check for refinement types
            if line.contains('{') && line.contains('|') && line.contains('}') {
                if let Some(vc) = self.generate_refinement_type_vc(line, line_num)? {
                    self.vcs.push(vc);
                }
            }
        }
        
        Ok(self.vcs.clone())
    }
    
    /// Generate VCs for Murphy's Sieve specifically
    pub fn generate_murphy_sieve_vcs(&mut self) -> Result<Vec<VerificationCondition>, String> {
        self.vcs.clear();
        
        // Murphy's Sieve correctness properties
        
        // 1. Soundness: No composite numbers marked as prime
        let soundness_vc = VerificationCondition::new(
            "Murphy's Sieve Soundness",
            "(assert (forall ((n Int)) (=> (and (>= n 0) (< n limit) (= (select bits n) 0)) (is_prime n))))"
        );
        self.vcs.push(soundness_vc);
        
        // 2. Completeness: All primes are marked as prime
        let completeness_vc = VerificationCondition::new(
            "Murphy's Sieve Completeness",
            "(assert (forall ((n Int)) (=> (and (>= n 0) (< n limit) (is_prime n)) (= (select bits n) 0))))"
        );
        self.vcs.push(completeness_vc);
        
        // 3. Count accuracy
        let count_vc = VerificationCondition::new(
            "Murphy's Sieve Count Accuracy",
            "(assert (= count (prime_count limit)))"
        );
        self.vcs.push(count_vc);
        
        // 4. Loop invariants for sieve
        let sieve_invariant_vc = VerificationCondition::new(
            "Sieve Loop Invariant",
            "(assert (forall ((k Int) (m Int)) (=> (and (>= k 2) (< k p) (>= m (* k k)) (< m limit) (= (select bits m) 1)) (not (is_prime m)))))"
        );
        self.vcs.push(sieve_invariant_vc);
        
        // 5. Loop invariants for counting
        let count_invariant_vc = VerificationCondition::new(
            "Count Loop Invariant",
            "(assert (= count (prime_count_up_to n)))"
        );
        self.vcs.push(count_invariant_vc);
        
        Ok(self.vcs.clone())
    }
    
    /// Generate VC for function contract
    fn generate_function_contract(&self, line: &str, line_num: usize) -> Result<Option<VerificationCondition>, String> {
        // Extract function signature
        let parts: Vec<&str> = line.split("->").collect();
        if parts.len() != 2 {
            return Ok(None);
        }
        
        let params_part = parts[0];
        let return_part = parts[1];
        
        // Check for refinement types in parameters
        if params_part.contains('{') && params_part.contains('|') {
            // Extract refinement type
            let start = params_part.find('{').unwrap();
            let end = params_part.find('}').unwrap() + 1;
            let refinement_str = &params_part[start..end];
            
            let refinement = RefinementType::parse(refinement_str)?;
            
            let vc = VerificationCondition::with_location(
                "Function Precondition",
                &refinement.to_smt(),
                &format!("line {}", line_num),
            );
            
            return Ok(Some(vc));
        }
        
        // Check for refinement type in return
        if return_part.contains('{') && return_part.contains('|') {
            // Extract refinement type
            let start = return_part.find('{').unwrap();
            let end = return_part.find('}').unwrap() + 1;
            let refinement_str = &return_part[start..end];
            
            let refinement = RefinementType::parse(refinement_str)?;
            
            let vc = VerificationCondition::with_location(
                "Function Postcondition",
                &refinement.to_smt(),
                &format!("line {}", line_num),
            );
            
            return Ok(Some(vc));
        }
        
        Ok(None)
    }
    
    /// Generate VC for loop invariant
    fn generate_loop_invariant(&self, line: &str, line_num: usize) -> Result<Option<VerificationCondition>, String> {
        // Extract invariant
        let start = line.find("@invariant").unwrap() + 10;
        let invariant_str = line[start..].trim();
        
        // Parse as predicate
        let predicate = Predicate::parse(invariant_str)?;
        
        let vc = VerificationCondition::with_location(
            "Loop Invariant",
            &format!("(assert {})", predicate.to_smt()),
            &format!("line {}", line_num),
        );
        
        Ok(Some(vc))
    }
    
    /// Generate VC for assertion
    fn generate_assertion(&self, line: &str, line_num: usize) -> Result<Option<VerificationCondition>, String> {
        // Extract assertion
        let start = line.find("@assert").unwrap() + 7;
        let assertion_str = line[start..].trim();
        
        // Parse as predicate
        let predicate = Predicate::parse(assertion_str)?;
        
        let vc = VerificationCondition::with_location(
            "Assertion",
            &format!("(assert {})", predicate.to_smt()),
            &format!("line {}", line_num),
        );
        
        Ok(Some(vc))
    }
    
    /// Generate VC for refinement type
    fn generate_refinement_type_vc(&self, line: &str, line_num: usize) -> Result<Option<VerificationCondition>, String> {
        // Find refinement type in line
        let start = line.find('{').unwrap();
        let end = line.find('}').unwrap() + 1;
        let refinement_str = &line[start..end];
        
        let refinement = RefinementType::parse(refinement_str)?;
        
        let vc = VerificationCondition::with_location(
            "Refinement Type",
            &refinement.to_smt(),
            &format!("line {}", line_num),
        );
        
        Ok(Some(vc))
    }
}

/// Generate VCs from program text
pub fn generate_vcs(program: &str) -> Result<Vec<VerificationCondition>, String> {
    let mut generator = VCGenerator::new();
    generator.generate(program)
}

/// Generate Murphy's Sieve specific VCs
pub fn generate_murphy_sieve_vcs() -> Result<Vec<VerificationCondition>, String> {
    let mut generator = VCGenerator::new();
    generator.generate_murphy_sieve_vcs()
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_generate_function_contract_vc() {
        let program = r#"
fn sqrt(x: {n: u64 | n >= 0}) -> {r: u64 | r * r <= x && (r + 1) * (r + 1) > x} {
    // implementation
}
"#;
        
        let vcs = generate_vcs(program).unwrap();
        assert!(!vcs.is_empty());
        assert!(vcs.iter().any(|vc| vc.name.contains("Precondition")));
        assert!(vcs.iter().any(|vc| vc.name.contains("Postcondition")));
    }
    
    #[test]
    fn test_generate_loop_invariant_vc() {
        let program = r#"
while i < n {
    // @invariant total == sum(arr[0..i])
    total += arr[i];
    i += 1;
}
"#;
        
        let vcs = generate_vcs(program).unwrap();
        assert!(!vcs.is_empty());
        assert!(vcs.iter().any(|vc| vc.name.contains("Loop Invariant")));
    }
    
    #[test]
    fn test_generate_assertion_vc() {
        let program = r#"
// @assert x > 0
let y = 1 / x;
"#;
        
        let vcs = generate_vcs(program).unwrap();
        assert!(!vcs.is_empty());
        assert!(vcs.iter().any(|vc| vc.name.contains("Assertion")));
    }
    
    #[test]
    fn test_generate_murphy_sieve_vcs() {
        let vcs = generate_murphy_sieve_vcs().unwrap();
        assert_eq!(vcs.len(), 5);
        
        let vc_names: Vec<&str> = vcs.iter().map(|vc| vc.name.as_str()).collect();
        assert!(vc_names.contains(&"Murphy's Sieve Soundness"));
        assert!(vc_names.contains(&"Murphy's Sieve Completeness"));
        assert!(vc_names.contains(&"Murphy's Sieve Count Accuracy"));
        assert!(vc_names.contains(&"Sieve Loop Invariant"));
        assert!(vc_names.contains(&"Count Loop Invariant"));
    }
}