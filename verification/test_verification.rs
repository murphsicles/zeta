// Test the formal verification system

use verification::*;
use verification::refinement::RefinementType;
use verification::vcgen::generate_murphy_sieve_vcs;

fn test_refinement_types() {
    println!("Testing refinement types...");
    
    // Test parsing refinement types
    let rt1 = RefinementType::parse("{n: u64 | n > 0}").unwrap();
    println!("Parsed: {}", rt1);
    
    let rt2 = RefinementType::parse("{x: i32 | x >= -10 && x <= 10}").unwrap();
    println!("Parsed: {}", rt2);
    
    let rt3 = RefinementType::parse("{arr: [u64] | len(arr) > 0}").unwrap();
    println!("Parsed: {}", rt3);
    
    println!("Refinement types test passed!");
}

fn test_vc_generation() {
    println!("\nTesting VC generation...");
    
    // Generate Murphy's Sieve VCs
    let vcs = generate_murphy_sieve_vcs().unwrap();
    
    println!("Generated {} verification conditions:", vcs.len());
    for (i, vc) in vcs.iter().enumerate() {
        println!("{}. {}:", i + 1, vc.name);
        if let Some(loc) = &vc.location {
            println!("   Location: {}", loc);
        }
        println!("   SMT2: {}", vc.smt2);
        println!();
    }
    
    println!("VC generation test passed!");
}

fn test_verification() {
    println!("\nTesting verification system...");
    
    // Create a verifier
    let mut verifier = Verifier::new();
    
    // Simple test program
    let test_program = r#"
fn sqrt(x: {n: u64 | n >= 0}) -> {r: u64 | r * r <= x && (r + 1) * (r + 1) > x} {
    // implementation would go here
    return 0;
}
"#;
    
    // Try to verify (will use mock solver since Z3 not installed)
    match verifier.verify(test_program) {
        Ok(result) => {
            println!("Verification completed!");
            println!("All VCs proven: {}", result.all_proven);
            println!("Number of VCs: {}", result.vcs.len());
        }
        Err(err) => {
            println!("Verification error: {}", err);
        }
    }
    
    println!("Verification system test completed!");
}

fn test_murphy_sieve_verification() {
    println!("\nTesting Murphy's Sieve verification...");
    
    // Read the verified Murphy's Sieve example
    let murphy_program = include_str!("examples/murphy_sieve_verified.z");
    
    println!("Murphy's Sieve program length: {} characters", murphy_program.len());
    
    // Count annotations
    let lines: Vec<&str> = murphy_program.lines().collect();
    let annotation_count = lines.iter()
        .filter(|line| line.contains("@invariant") || line.contains("@assert") || 
                       line.contains("@predicate") || line.contains("@spec"))
        .count();
    
    println!("Found {} verification annotations", annotation_count);
    
    // Check for refinement types
    let refinement_count = lines.iter()
        .filter(|line| line.contains('{') && line.contains('|') && line.contains('}'))
        .count();
    
    println!("Found {} refinement types", refinement_count);
    
    println!("Murphy's Sieve verification test completed!");
}

fn main() {
    println!("=== Zeta Formal Verification System Test ===\n");
    
    test_refinement_types();
    test_vc_generation();
    test_verification();
    test_murphy_sieve_verification();
    
    println!("\n=== All Tests Completed ===");
    println!("\nSummary:");
    println!("- Refinement types: Implemented");
    println!("- VC generation: Implemented");
    println!("- SMT solver integration: Ready (requires Z3)");
    println!("- Murphy's Sieve verification: Annotations defined");
    println!("\nTo complete the verification system:");
    println!("1. Install Z3 solver (z3 command in PATH)");
    println!("2. Integrate with Zeta compiler type checker");
    println!("3. Add verification pass to compilation pipeline");
}