// Progressive Test Suite - Detailed Test
// Tests with more detailed error reporting

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::resolver::resolver::Resolver;

fn test_case_detailed(name: &str, code: &str, test_num: usize) -> (bool, Vec<String>) {
    println!("\n{}", "=".repeat(60));
    println!("TEST {}: {}", test_num, name);
    println!("{}", "=".repeat(60));

    let mut issues = Vec::new();
    let mut all_passed = true;

    // Step 1: Parsing
    println!("\n1. PARSING");
    println!("{}", "-".repeat(30));
    match parse_zeta(code) {
        Ok((remaining, asts)) => {
            if !remaining.trim().is_empty() {
                let issue = format!("Not all input consumed. Remaining: '{}'", remaining);
                println!("⚠️  {}", issue);
                issues.push(issue);
            }
            println!("✓ Parse successful");
            println!("  AST count: {}", asts.len());

            // Display AST details
            for (i, ast) in asts.iter().enumerate() {
                println!(
                    "  AST {}: Function '{}'",
                    i,
                    match ast {
                        zetac::frontend::ast::AstNode::FuncDef { name, .. } => name,
                        zetac::frontend::ast::AstNode::StructDef { name, .. } => name,
                        zetac::frontend::ast::AstNode::EnumDef { name, .. } => name,
                        zetac::frontend::ast::AstNode::ImplBlock { .. } => "<impl block>",
                        _ => "<unknown>",
                    }
                );
            }

            // Step 2: Resolver setup
            println!("\n2. RESOLVER SETUP");
            println!("{}", "-".repeat(30));
            let mut resolver = Resolver::new();

            for ast in &asts {
                resolver.register(ast.clone());
            }
            println!("✓ ASTs registered");

            // Step 3: Type checking
            println!("\n3. TYPE CHECKING");
            println!("{}", "-".repeat(30));
            let type_ok = resolver.typecheck(&asts);
            if type_ok {
                println!("✓ Type check passed");
            } else {
                println!("✗ Type check failed");
                all_passed = false;
                issues.push("Type check failed".to_string());
            }

            // Step 4: MIR generation
            println!("\n4. MIR GENERATION");
            println!("{}", "-".repeat(30));
            if type_ok {
                let mirs: Vec<_> = asts.iter().map(|ast| resolver.lower_to_mir(ast)).collect();

                println!("✓ Generated {} MIRs", mirs.len());

                // Analyze MIRs
                for mir in &mirs {
                    if let Some(name) = &mir.name {
                        println!("  Function '{}':", name);
                        println!("    - {} statements", mir.stmts.len());
                        println!("    - {} expressions", mir.exprs.len());

                        // Check for potential issues
                        if mir.stmts.is_empty() && name != "main" {
                            let issue = format!("Function '{}' has empty MIR statements", name);
                            println!("    ⚠️  {}", issue);
                            issues.push(issue);
                        }

                        // Show first few statements
                        if !mir.stmts.is_empty() {
                            println!("    - First statement: {:?}", mir.stmts[0]);
                        }
                    }
                }
            } else {
                println!("⏭️  Skipping MIR generation (type check failed)");
            }

            // Step 5: Check for generic instantiation
            println!("\n5. GENERIC INSTANTIATION CHECK");
            println!("{}", "-".repeat(30));

            // Look for generic functions in the code
            let has_generics = code.contains("<") && code.contains(">");
            if has_generics {
                println!("Code contains generic syntax");

                // Check if we have proper monomorphization
                let generic_funcs: Vec<&str> = asts
                    .iter()
                    .filter_map(|ast| {
                        if let zetac::frontend::ast::AstNode::FuncDef { name, generics, .. } = ast {
                            if !generics.is_empty() {
                                Some(name.as_str())
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .collect();

                if !generic_funcs.is_empty() {
                    println!("  Found generic functions: {:?}", generic_funcs);

                    // Check if they're called with type arguments
                    let mut has_generic_calls = false;
                    for ast in &asts {
                        if let zetac::frontend::ast::AstNode::FuncDef {
                            ret_expr: Some(expr),
                            ..
                        } = ast
                        {
                            // Simple check for generic calls
                            let expr_str = format!("{:?}", expr);
                            if expr_str.contains("type_args") {
                                has_generic_calls = true;
                            }
                        }
                    }

                    if has_generic_calls {
                        println!("  ✓ Generic functions are called with type arguments");
                    } else {
                        let issue =
                            "Generic functions found but no generic calls detected".to_string();
                        println!("  ⚠️  {}", issue);
                        issues.push(issue);
                    }
                }
            } else {
                println!("No generic syntax found in code");
            }
        }
        Err(e) => {
            println!("✗ Parse failed: {:?}", e);
            all_passed = false;
            issues.push(format!("Parse error: {:?}", e));
        }
    }

    println!(
        "\nTest {} result: {}",
        test_num,
        if all_passed { "PASSED" } else { "FAILED" }
    );
    if !issues.is_empty() {
        println!("Issues found: {}", issues.len());
        for issue in &issues {
            println!("  - {}", issue);
        }
    }

    (all_passed, issues)
}

fn main() {
    println!("=== PROGRESSIVE TEST SUITE - DETAILED ===");
    println!("Testing with comprehensive analysis\n");

    let mut test_results = Vec::new();
    let mut all_issues = Vec::new();

    // Test 1: Simple - identity::<i64>(42)
    println!("\n{}", "#".repeat(60));
    println!("STARTING TEST 1: SIMPLE GENERIC");
    println!("{}", "#".repeat(60));

    let test1_code = r#"
    fn identity<T>(x: T) -> T { x }
    
    fn main() -> i64 {
        identity::<i64>(42)
    }
    "#;

    let (passed1, issues1) = test_case_detailed("identity::<i64>(42)", test1_code, 1);
    test_results.push(passed1);
    all_issues.extend(issues1);

    // Test 2: Medium - Option::<i32>::None
    println!("\n{}", "#".repeat(60));
    println!("STARTING TEST 2: ENUM WITH GENERICS");
    println!("{}", "#".repeat(60));

    let test2_code = r#"
    enum Option<T> {
        Some(T),
        None,
    }
    
    fn main() -> Option<i32> {
        Option::<i32>::None
    }
    "#;

    let (passed2, issues2) = test_case_detailed("Option::<i32>::None", test2_code, 2);
    test_results.push(passed2);
    all_issues.extend(issues2);

    // Test 3: Complex - Vec::<i32>::new()
    println!("\n{}", "#".repeat(60));
    println!("STARTING TEST 3: STRUCT WITH IMPL GENERICS");
    println!("{}", "#".repeat(60));

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

    let (passed3, issues3) = test_case_detailed("Vec::<i32>::new()", test3_code, 3);
    test_results.push(passed3);
    all_issues.extend(issues3);

    // Summary
    println!("\n{}", "=".repeat(60));
    println!("FINAL SUMMARY");
    println!("{}", "=".repeat(60));

    let test_names = [
        "identity::<i64>(42)",
        "Option::<i32>::None",
        "Vec::<i32>::new()",
    ];

    for (i, (passed, name)) in test_results.iter().zip(test_names.iter()).enumerate() {
        println!(
            "Test {} ({}): {}",
            i + 1,
            name,
            if *passed { "✓ PASSED" } else { "✗ FAILED" }
        );
    }

    let total_passed = test_results.iter().filter(|&&p| p).count();
    println!(
        "\nOverall: {}/{} tests passed",
        total_passed,
        test_results.len()
    );

    if !all_issues.is_empty() {
        println!("\n{} ISSUES FOUND:", all_issues.len());
        println!("{}", "-".repeat(30));
        for (i, issue) in all_issues.iter().enumerate() {
            println!("{}. {}", i + 1, issue);
        }
    }

    if total_passed == test_results.len() && all_issues.is_empty() {
        println!("\n🎉 PERFECT! All tests passed with no issues.");
    } else if total_passed == test_results.len() {
        println!("\n⚠️  All tests passed but some issues were noted.");
    } else {
        println!("\n🔧 Some tests failed. Further investigation needed.");
    }

    // Progress report
    println!("\n{}", "=".repeat(60));
    println!("PROGRESS REPORT");
    println!("{}", "=".repeat(60));
    println!("Time: {}", chrono::Local::now().format("%H:%M:%S"));
    println!("Status: Phase 3 progressive testing completed");
    println!("Next steps: Analyze issues and fix compilation pipeline");
}
