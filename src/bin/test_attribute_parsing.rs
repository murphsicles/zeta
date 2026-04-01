//! Test attribute parsing functionality

use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    println!("=== Testing Attribute Parsing ===\n");

    let test_cases = vec![
        (
            "Simple #[test] attribute",
            r#"
        #[test]
        fn test_simple() -> i64 {
            42
        }
        "#,
        ),
        (
            "#[derive(Clone)] attribute",
            r#"
        #[derive(Clone)]
        struct Point {
            x: i64,
            y: i64,
        }
        "#,
        ),
        (
            "Multiple attributes",
            r#"
        #[test]
        #[should_panic]
        fn test_panic() -> i64 {
            panic!("expected")
        }
        "#,
        ),
        (
            "Attribute with multiple arguments",
            r#"
        #[derive(Clone, Debug, PartialEq)]
        struct Complex {
            real: i64,
            imag: i64,
        }
        "#,
        ),
        (
            "Attribute on impl block",
            r#"
        #[allow(unused)]
        impl Point {
            fn new(x: i64, y: i64) -> Point {
                Point { x, y }
            }
        }
        "#,
        ),
    ];

    let mut passed = 0;
    let total = test_cases.len();

    for (name, code) in test_cases {
        let result = parse_zeta(code);
        match result {
            Ok((remaining, _)) => {
                if remaining.is_empty() {
                    println!("✅ {} - Parses completely", name);
                    passed += 1;
                } else {
                    println!(
                        "⚠️  {} - Partial parse ({} chars remaining)",
                        name,
                        remaining.len()
                    );
                    // Show first 50 chars of unparsed content
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

    println!("\n=== Summary ===");
    println!(
        "Passed: {}/{} ({:.1}%)",
        passed,
        total,
        (passed as f32 / total as f32) * 100.0
    );

    if passed == total {
        println!("✅ All attribute tests passed!");
    } else {
        println!("⚠️  Some attribute tests failed - need to implement attribute parsing");
    }
}
