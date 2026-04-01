//! Analyze v0.5.0 syntax issues

// use std::fs;
// use std::path::Path;
use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    println!("=== v0.5.0 Syntax Issue Analysis ===\n");

    // Test specific problematic syntax patterns
    let test_cases = vec![
        (
            "Rust-like attributes",
            r#"
        #[derive(Clone, Debug, PartialEq)]
        pub struct MatchArm {
            pattern: Box<AstNode>,
        }
        "#,
        ),
        (
            "Doc comments",
            r#"
        /// Match expression arm with pattern and body.
        pub struct MatchArm {
            /// Pattern to match against
            pattern: Box<AstNode>,
        }
        "#,
        ),
        (
            "Use statements with crate",
            r#"
        use crate::middle::mir::Mir;
        use crate::backend::codegen::ir_gen::IRGen;
        "#,
        ),
        (
            "Complex generics",
            r#"
        fn process<T: Clone + Display>(item: T) -> T {
            item.clone()
        }
        "#,
        ),
        (
            "Match with patterns",
            r#"
        fn test(x: i64) -> i64 {
            match x {
                0 => 1,
                _ => x * 2,
            }
        }
        "#,
        ),
        (
            "Impl for generic type",
            r#"
        impl<T> Option<T> {
            fn unwrap(self) -> T {
                self.value
            }
        }
        "#,
        ),
        (
            "Trait bounds in struct",
            r#"
        struct Container<T: Display> {
            value: T,
        }
        "#,
        ),
        (
            "Where clauses",
            r#"
        fn process<T>(item: T) -> T 
        where T: Clone + Display {
            item.clone()
        }
        "#,
        ),
        (
            "Async functions",
            r#"
        async fn fetch_data() -> Result<String, Error> {
            // async code
        }
        "#,
        ),
        (
            "Lifetime annotations",
            r#"
        fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
            if x.len() > y.len() { x } else { y }
        }
        "#,
        ),
    ];

    for (name, code) in test_cases {
        let result = parse_zeta(code);
        match result {
            Ok((remaining, _)) => {
                if remaining.is_empty() {
                    println!("✅ {} - Parses completely", name);
                } else {
                    println!(
                        "⚠️  {} - Partial parse ({} chars remaining)",
                        name,
                        remaining.len()
                    );
                    // Show what wasn't parsed
                    let preview = if remaining.len() > 50 {
                        &remaining[..50]
                    } else {
                        remaining
                    };
                    println!("   Unparsed: '{}'", preview.replace("\n", "\\n"));
                }
            }
            Err(e) => {
                println!("❌ {} - Parse error: {:?}", name, e);
            }
        }
    }

    // Now test what DOES work in v0.3.23
    println!("\n=== v0.3.23 Supported Syntax ===");

    let supported_cases = vec![
        (
            "Simple function",
            r#"
        fn main() -> i64 {
            42
        }
        "#,
        ),
        (
            "Let statement",
            r#"
        fn test() -> i64 {
            let x = 10;
            x
        }
        "#,
        ),
        (
            "If statement",
            r#"
        fn test(x: i64) -> i64 {
            if x > 0 {
                x
            } else {
                0
            }
        }
        "#,
        ),
        (
            "While loop",
            r#"
        fn test() -> i64 {
            let mut x = 0;
            while x < 10 {
                x = x + 1;
            }
            x
        }
        "#,
        ),
        (
            "Binary operations",
            r#"
        fn add(a: i64, b: i64) -> i64 {
            a + b
        }
        "#,
        ),
        (
            "Function calls",
            r#"
        fn main() -> i64 {
            add(1, 2)
        }
        
        fn add(a: i64, b: i64) -> i64 {
            a + b
        }
        "#,
        ),
        (
            "Struct definition (simple)",
            r#"
        struct Point {
            x: i64,
            y: i64,
        }
        "#,
        ),
        (
            "Match (simple)",
            r#"
        fn test(x: i64) -> i64 {
            match x {
                0 => 1,
                _ => 2,
            }
        }
        "#,
        ),
    ];

    let mut supported = 0;
    let mut total = 0;

    for (name, code) in supported_cases {
        total += 1;
        let result = parse_zeta(code);
        match result {
            Ok((remaining, _)) => {
                if remaining.is_empty() {
                    println!("✅ {} - Supported", name);
                    supported += 1;
                } else {
                    println!("⚠️  {} - Partial support", name);
                }
            }
            Err(e) => {
                println!("❌ {} - Not supported: {:?}", name, e);
            }
        }
    }

    println!("\n=== Summary ===");
    println!(
        "Supported features: {}/{} ({:.1}%)",
        supported,
        total,
        (supported as f32 / total as f32) * 100.0
    );

    // Analyze gap between v0.3.23 and v0.5.0
    println!("\n=== v0.3.23 → v0.5.0 Gap Analysis ===");
    println!("Major missing features for v0.5.0 compatibility:");
    println!("1. Rust-like attributes (#[derive(...)])");
    println!("2. Doc comments (///)");
    println!("3. Complex use statements (use crate::...)");
    println!("4. Trait bounds (T: Clone + Display)");
    println!("5. Generic impl blocks (impl<T> Option<T>)");
    println!("6. Where clauses");
    println!("7. Lifetime annotations");
    println!("8. Async functions");
    println!("9. Complex match patterns");
    println!("10. Advanced type system features");

    println!("\n=== Recommendations for v0.3.24 ===");
    println!("To reach 50%+ v0.5.0 compatibility, focus on:");
    println!("1. Support basic attributes (#[test], #[derive]) - HIGH IMPACT");
    println!("2. Support doc comments - MEDIUM IMPACT");
    println!("3. Support crate-relative use statements - MEDIUM IMPACT");
    println!("4. Support simple trait bounds - HIGH IMPACT");
    println!("5. Support generic impl blocks - MEDIUM IMPACT");
}
