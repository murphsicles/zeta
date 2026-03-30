// Progressive Test Suite - Full Test
// Tests: 1. identity::<i64>(42)
//        2. Option::<i32>::None
//        3. Vec::<i32>::new()

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::resolver::resolver::Resolver;

fn test_case(name: &str, code: &str, test_num: usize) -> bool {
    println!("\n{}", "=".repeat(60));
    println!("TEST {}: {}", test_num, name);
    println!("{}", "=".repeat(60));
    println!("Code:\n{}", code);
    println!("{}", "-".repeat(60));

    let mut all_passed = true;

    // Step 1: Parsing
    println!("Step 1: Parsing...");
    match parse_zeta(code) {
        Ok((remaining, asts)) => {
            if !remaining.trim().is_empty() {
                println!("⚠️  Warning: Not all input consumed. Remaining: '{}'", remaining);
            }
            println!("✓ Parse successful");
            println!("  AST count: {}", asts.len());
            
            // Step 2: Resolver setup
            println!("\nStep 2: Resolver setup...");
            let mut resolver = Resolver::new();
            
            for ast in &asts {
                resolver.register(ast.clone());
            }
            println!("✓ ASTs registered");

            // Step 3: Type checking
            println!("\nStep 3: Type checking...");
            let type_ok = resolver.typecheck(&asts);
            if type_ok {
                println!("✓ Type check passed");
            } else {
                println!("✗ Type check failed");
                all_passed = false;
            }

            // Step 4: MIR generation
            if type_ok {
                println!("\nStep 4: MIR generation...");
                let mirs: Vec<_> = asts
                    .iter()
                    .map(|ast| resolver.lower_to_mir(ast))
                    .collect();
                
                println!("✓ Generated {} MIRs", mirs.len());
                
                // Display MIR info
                for mir in &mirs {
                    if let Some(name) = &mir.name {
                        println!("  MIR '{}': {} statements", name, mir.stmts.len());
                        if mir.stmts.len() > 0 {
                            println!("    First statement: {:?}", mir.stmts[0]);
                        }
                    }
                }
            }
        }
        Err(e) => {
            println!("✗ Parse failed: {:?}", e);
            all_passed = false;
        }
    }

    println!("\nTest {} result: {}", test_num, if all_passed { "PASSED" } else { "FAILED" });
    all_passed
}

fn main() {
    println!("=== PROGRESSIVE TEST SUITE ===");
    println!("Testing simple → medium → complex progression\n");

    let mut test_results = Vec::new();

    // Test 1: Simple - identity::<i64>(42)
    let test1_code = r#"
    fn identity<T>(x: T) -> T { x }
    
    fn main() -> i64 {
        identity::<i64>(42)
    }
    "#;
    
    test_results.push(test_case("identity::<i64>(42)", test1_code, 1));

    // Test 2: Medium - Option::<i32>::None
    let test2_code = r#"
    enum Option<T> {
        Some(T),
        None,
    }
    
    fn main() -> Option<i32> {
        Option::<i32>::None
    }
    "#;
    
    test_results.push(test_case("Option::<i32>::None", test2_code, 2));

    // Test 3: Complex - Vec::<i32>::new()
    let test3_code = r#"
    struct Vec<T> {
        data: *mut T,
        len: usize,
        capacity: usize,
    }
    
    impl<T> Vec<T> {
        fn new() -> Vec<T> {
            Vec { data: 0 as *mut T, len: 0, capacity: 0 }
        }
    }
    
    fn main() -> Vec<i32> {
        Vec::<i32>::new()
    }
    "#;
    
    test_results.push(test_case("Vec::<i32>::new()", test3_code, 3));

    // Summary
    println!("\n{}", "=".repeat(60));
    println!("TEST SUMMARY");
    println!("{}", "=".repeat(60));
    
    for (i, passed) in test_results.iter().enumerate() {
        println!("Test {}: {}", i + 1, if *passed { "✓ PASSED" } else { "✗ FAILED" });
    }
    
    let total_passed = test_results.iter().filter(|&&p| p).count();
    println!("\nTotal: {}/{} tests passed", total_passed, test_results.len());
    
    if total_passed == test_results.len() {
        println!("\n🎉 All tests passed! Progressive testing complete.");
    } else {
        println!("\n⚠️  Some tests failed. Further investigation needed.");
    }
}